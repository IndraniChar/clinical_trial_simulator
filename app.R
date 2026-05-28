# app.R
# Clinical Trial Simulator for Rare Diseases - COMPLETELY FIXED
# Predicts trial outcomes using Monte Carlo simulation with realistic parameters

library(shiny)
library(shinydashboard)
library(ggplot2)
library(plotly)
library(DT)

# ============================================
# PK/PD MODEL - Sigmoid Emax
# ============================================

calculate_response <- function(dose, emax = 65, ed50 = 60, hill = 2, placebo_effect = 12) {
  # Realistic parameters: max 65% improvement, ED50 at 60mg
  response <- placebo_effect + (emax * dose^hill) / (ed50^hill + dose^hill)
  return(pmin(85, response))  # Cap at 85% max improvement
}

# ============================================
# REALISTIC VIRTUAL PATIENT GENERATION
# ============================================

generate_virtual_patients <- function(n_patients = 50, seed = 42) {
  set.seed(seed)
  
  # Generate realistic patient demographics
  age <- round(rnorm(n_patients, mean = 58, sd = 14), 1)
  age <- pmax(25, pmin(85, age))
  
  bmi <- round(rnorm(n_patients, mean = 28, sd = 5), 1)
  bmi <- pmax(18.5, pmin(45, bmi))
  
  sex <- sample(c("Male", "Female"), n_patients, replace = TRUE, prob = c(0.48, 0.52))
  
  baseline_severity <- round(rnorm(n_patients, mean = 72, sd = 12), 1)
  baseline_severity <- pmax(40, pmin(100, baseline_severity))
  
  # Genetic responder status (30% of population are genetic responders)
  genetic_responder <- rbinom(n_patients, 1, prob = 0.3)
  
  patients <- data.frame(
    patient_id = paste0("PT", sprintf("%03d", 1:n_patients)),
    age = age,
    bmi = bmi,
    sex = sex,
    baseline_severity = baseline_severity,
    genetic_responder = genetic_responder,
    stringsAsFactors = FALSE
  )
  
  return(patients)
}

# ============================================
# REALISTIC SIMULATION ENGINE
# ============================================

run_clinical_trial_simulation <- function(n_patients = 50, 
                                          dose = 50, 
                                          is_treatment = TRUE, 
                                          n_simulations = 30,
                                          seed_base = 42) {
  
  results <- data.frame()
  
  for(sim in 1:n_simulations) {
    set.seed(seed_base + sim * 10)
    
    # Generate virtual patients
    patients <- generate_virtual_patients(n_patients, seed = seed_base + sim)
    
    if(is_treatment) {
      # TREATMENT ARM - Realistic response based on PK/PD model
      base_response <- calculate_response(dose)
      
      # Patient-specific factors affect response
      age_factor <- (65 - patients$age) / 100  # Younger = better response
      age_factor <- pmax(-0.2, pmin(0.3, age_factor))
      
      bmi_factor <- ifelse(patients$bmi > 30, -0.15, 
                           ifelse(patients$bmi < 22, 0.1, 0))
      
      genetic_factor <- ifelse(patients$genetic_responder == 1, 0.25, 0)
      
      severity_factor <- (80 - patients$baseline_severity) / 150
      severity_factor <- pmax(-0.2, pmin(0.2, severity_factor))
      
      # Random variability (patient-to-patient variation)
      variability <- rnorm(n_patients, mean = 0, sd = 6)
      
      # Calculate final improvement for each patient
      improvements <- base_response * (1 + age_factor + bmi_factor + 
                                         genetic_factor + severity_factor) + 
        variability
      
      # Realistic bounds (5% to 75% improvement)
      improvements <- pmax(5, pmin(75, improvements))
      
    } else {
      # PLACEBO ARM - Realistic placebo response (10-25% improvement)
      # Placebo effect depends on baseline severity and patient mindset
      base_placebo <- 12  # Average 12% improvement from placebo
      severity_placebo_factor <- (80 - patients$baseline_severity) / 200
      severity_placebo_factor <- pmax(-0.1, pmin(0.15, severity_placebo_factor))
      
      variability <- rnorm(n_patients, mean = 0, sd = 4)
      
      improvements <- base_placebo * (1 + severity_placebo_factor) + variability
      improvements <- pmax(5, pmin(30, improvements))  # Placebo: 5-30% improvement
    }
    
    # Define responder threshold (typically 30% improvement is clinically meaningful)
    responders <- improvements > 30
    response_rate <- mean(responders) * 100
    
    # Calculate metrics for this simulation
    results <- rbind(results, data.frame(
      simulation = sim,
      response_rate = response_rate,
      mean_improvement = mean(improvements),
      sd_improvement = sd(improvements),
      n_responders = sum(responders),
      n_patients = n_patients
    ))
  }
  
  return(results)
}

# ============================================
# STATISTICAL ANALYSIS
# ============================================

compare_treatment_vs_placebo <- function(treatment_results, placebo_results) {
  
  # Calculate summary statistics
  treatment_mean_rate <- mean(treatment_results$response_rate)
  placebo_mean_rate <- mean(placebo_results$response_rate)
  treatment_sd_rate <- sd(treatment_results$response_rate)
  placebo_sd_rate <- sd(placebo_results$response_rate)
  
  treatment_mean_improvement <- mean(treatment_results$mean_improvement)
  placebo_mean_improvement <- mean(placebo_results$mean_improvement)
  
  # Statistical significance test (t-test)
  t_test <- t.test(treatment_results$response_rate, placebo_results$response_rate)
  
  # Calculate effect size (Cohen's d)
  pooled_sd <- sqrt((treatment_sd_rate^2 + placebo_sd_rate^2) / 2)
  effect_size <- (treatment_mean_rate - placebo_mean_rate) / pooled_sd
  
  # Interpret effect size
  effect_interpretation <- ifelse(effect_size < 0.2, "Negligible",
                                  ifelse(effect_size < 0.5, "Small",
                                         ifelse(effect_size < 0.8, "Medium", "Large")))
  
  # Calculate Number Needed to Treat (NNT)
  response_rate_diff <- (treatment_mean_rate - placebo_mean_rate) / 100
  nnt <- ifelse(response_rate_diff > 0, round(1 / response_rate_diff), NA)
  
  # Create comparison table
  comparison_table <- data.frame(
    Metric = c("Response Rate (%)", "Mean Improvement (%)", "Responders (per 100)", "Standard Deviation"),
    Treatment = c(
      round(treatment_mean_rate, 1),
      round(treatment_mean_improvement, 1),
      round(treatment_mean_rate),
      round(treatment_sd_rate, 1)
    ),
    Placebo = c(
      round(placebo_mean_rate, 1),
      round(placebo_mean_improvement, 1),
      round(placebo_mean_rate),
      round(placebo_sd_rate, 1)
    ),
    Difference = c(
      round(treatment_mean_rate - placebo_mean_rate, 1),
      round(treatment_mean_improvement - placebo_mean_improvement, 1),
      round(treatment_mean_rate - placebo_mean_rate),
      NA
    )
  )
  
  return(list(
    comparison_table = comparison_table,
    p_value = t_test$p.value,
    significant = t_test$p.value < 0.05,
    effect_size = effect_size,
    effect_interpretation = effect_interpretation,
    nnt = nnt,
    treatment_mean_rate = treatment_mean_rate,
    placebo_mean_rate = placebo_mean_rate,
    treatment_mean_improvement = treatment_mean_improvement,
    placebo_mean_improvement = placebo_mean_improvement
  ))
}

# ============================================
# SHINY UI
# ============================================

ui <- dashboardPage(
  dashboardHeader(title = "Clinical Trial Simulator"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Trial Setup", tabName = "setup", icon = icon("flask")),
      menuItem("Simulation Results", tabName = "results", icon = icon("chart-line")),
      menuItem("Patient Population", tabName = "patients", icon = icon("users")),
      menuItem("Dose-Response", tabName = "dose_response", icon = icon("chart-line"))
    )
  ),
  
  dashboardBody(
    tags$head(
      tags$style(HTML("
        .content-wrapper, .right-side { background-color: #f4f4f4; }
        .box { border-radius: 10px; }
        .small-box { border-radius: 10px; }
      "))
    ),
    
    tabItems(
      # Trial Setup Tab
      tabItem(tabName = "setup",
              fluidRow(
                box(title = "Trial Parameters", status = "primary", solidHeader = TRUE, width = 6,
                    sliderInput("n_patients", "Number of Patients per Arm",
                                min = 20, max = 200, value = 50, step = 10),
                    sliderInput("dose", "Drug Dose (mg)",
                                min = 10, max = 200, value = 50, step = 10),
                    numericInput("n_simulations", "Monte Carlo Simulations",
                                 min = 10, max = 100, value = 30, step = 10),
                    hr(),
                    actionButton("run_simulation", "RUN CLINICAL TRIAL SIMULATION",
                                 class = "btn-success btn-lg", icon = icon("play")),
                    br(), br(),
                    helpText("This simulator runs virtual clinical trials using AI-generated patient populations.")
                ),
                
                box(title = "Dose-Response Preview", status = "info", solidHeader = TRUE, width = 6,
                    plotlyOutput("dose_response_preview", height = "300px"),
                    br(),
                    valueBoxOutput("predicted_efficacy", width = 12)
                )
              ),
              
              fluidRow(
                box(title = "Simulation Status", status = "warning", width = 12,
                    verbatimTextOutput("simulation_status")
                )
              )
      ),
      
      # Simulation Results Tab
      tabItem(tabName = "results",
              fluidRow(
                valueBoxOutput("treatment_response"),
                valueBoxOutput("placebo_response"),
                valueBoxOutput("p_value")
              ),
              fluidRow(
                valueBoxOutput("effect_size_box"),
                valueBoxOutput("nnt_box")
              ),
              fluidRow(
                box(title = "Response Rate Distribution", status = "primary", width = 6,
                    plotlyOutput("response_histogram")),
                box(title = "Improvement Distribution", status = "info", width = 6,
                    plotlyOutput("improvement_boxplot"))
              ),
              fluidRow(
                box(title = "Trial Metrics Comparison", status = "success", width = 12,
                    DTOutput("comparison_table"))
              ),
              fluidRow(
                box(title = "Monte Carlo Simulation Convergence", status = "warning", width = 12,
                    plotlyOutput("simulation_convergence"))
              )
      ),
      
      # Patient Population Tab
      tabItem(tabName = "patients",
              fluidRow(
                box(title = "Virtual Patient Demographics", status = "primary", width = 12,
                    DTOutput("patient_table"))
              ),
              fluidRow(
                box(title = "Age Distribution", status = "info", width = 6,
                    plotlyOutput("age_distribution")),
                box(title = "Baseline Severity Distribution", status = "info", width = 6,
                    plotlyOutput("severity_distribution"))
              ),
              fluidRow(
                box(title = "BMI Distribution", status = "warning", width = 6,
                    plotlyOutput("bmi_distribution")),
                box(title = "Genetic Responder Status", status = "warning", width = 6,
                    plotlyOutput("genetic_pie"))
              )
      ),
      
      # Dose-Response Tab
      tabItem(tabName = "dose_response",
              fluidRow(
                box(title = "Dose-Response Curve (Sigmoid Emax Model)", status = "primary", width = 12,
                    plotlyOutput("dose_response_curve")),
                box(title = "Optimal Dose Finder", status = "warning", width = 12,
                    sliderInput("optimal_dose_range", "Dose Range (mg)",
                                min = 0, max = 200, value = c(20, 150)),
                    plotlyOutput("optimal_dose_plot"))
              )
      )
    )
  )
)

# ============================================
# SHINY SERVER - NO showNotification
# ============================================

server <- function(input, output, session) {
  
  # Reactive values
  values <- reactiveValues(
    treatment_results = NULL,
    placebo_results = NULL,
    comparison = NULL,
    is_running = FALSE
  )
  
  # Dose-response preview
  output$dose_response_preview <- renderPlotly({
    doses <- seq(0, 200, by = 5)
    responses <- calculate_response(doses)
    
    df <- data.frame(Dose = doses, Response = responses)
    
    p <- ggplot(df, aes(x = Dose, y = Response)) +
      geom_line(color = "#2E8B57", size = 1.5) +
      geom_point(data = df[df$Dose == input$dose, ], 
                 aes(x = Dose, y = Response), 
                 color = "red", size = 5) +
      geom_hline(yintercept = 30, linetype = "dashed", color = "gray50", alpha = 0.7) +
      annotate("text", x = 180, y = 32, label = "Response Threshold (30%)", size = 3) +
      theme_minimal() +
      labs(title = "Sigmoid Emax Dose-Response Model",
           y = "Predicted Improvement (%)",
           x = "Dose (mg)")
    
    ggplotly(p)
  })
  
  # Predicted efficacy at selected dose
  output$predicted_efficacy <- renderValueBox({
    predicted <- calculate_response(input$dose)
    valueBox(
      value = paste0(round(predicted, 1), "% improvement"),
      subtitle = "Predicted Efficacy at Selected Dose",
      icon = icon("chart-line"),
      color = "green"
    )
  })
  
  # Run simulation - NO showNotification
  observeEvent(input$run_simulation, {
    if (values$is_running) {
      # Just print to console instead of showing notification
      cat("Simulation already running...\n")
      return()
    }
    
    values$is_running <- TRUE
    
    # Update status
    output$simulation_status <- renderPrint({
      cat("🏃 RUNNING CLINICAL TRIAL SIMULATION...\n")
      cat("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n\n")
      cat(paste("  • Patients per arm:", input$n_patients, "\n"))
      cat(paste("  • Drug dose:", input$dose, "mg\n"))
      cat(paste("  • Monte Carlo simulations:", input$n_simulations, "\n\n"))
      cat("  ⏳ Processing... This will take 5-15 seconds.\n")
      cat("  📊 Generating virtual patients and running simulations...\n")
    })
    
    # Run treatment arm
    withProgress(message = 'Running Treatment Arm Simulation...', value = 0.3, {
      values$treatment_results <- run_clinical_trial_simulation(
        n_patients = input$n_patients,
        dose = input$dose,
        is_treatment = TRUE,
        n_simulations = input$n_simulations
      )
    })
    
    # Run placebo arm
    withProgress(message = 'Running Placebo Arm Simulation...', value = 0.6, {
      values$placebo_results <- run_clinical_trial_simulation(
        n_patients = input$n_patients,
        dose = 0,
        is_treatment = FALSE,
        n_simulations = input$n_simulations
      )
    })
    
    # Compare results
    withProgress(message = 'Analyzing statistical significance...', value = 0.9, {
      values$comparison <- compare_treatment_vs_placebo(
        values$treatment_results, 
        values$placebo_results
      )
    })
    
    values$is_running <- FALSE
    
    # Update status with results
    output$simulation_status <- renderPrint({
      cat("✅ SIMULATION COMPLETE!\n")
      cat("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n\n")
      cat(paste("  • Treatment response rate:", round(values$comparison$treatment_mean_rate, 1), "%\n"))
      cat(paste("  • Placebo response rate:", round(values$comparison$placebo_mean_rate, 1), "%\n"))
      cat(paste("  • Treatment advantage:", round(values$comparison$treatment_mean_rate - values$comparison$placebo_mean_rate, 1), "%\n"))
      cat(paste("  • P-value:", format(values$comparison$p_value, scientific = TRUE, digits = 3), "\n"))
      cat(paste("  • Statistically significant:", ifelse(values$comparison$significant, "YES ✓", "NO ✗"), "\n"))
      cat(paste("  • Effect size:", round(values$comparison$effect_size, 2), "-", values$comparison$effect_interpretation, "\n"))
      if(!is.na(values$comparison$nnt)) {
        cat(paste("  • Number Needed to Treat (NNT):", values$comparison$nnt, "\n"))
      }
    })
    
    # Print to console instead of showNotification
    cat("✅ Simulation complete! Check the Results tab.\n")
  })
  
  # Value boxes
  output$treatment_response <- renderValueBox({
    if (is.null(values$comparison)) {
      valueBox("?", "Treatment Response Rate", icon = icon("capsules"), color = "green")
    } else {
      valueBox(
        value = paste0(round(values$comparison$treatment_mean_rate, 1), "%"),
        subtitle = "Treatment Response Rate",
        icon = icon("capsules"),
        color = "green"
      )
    }
  })
  
  output$placebo_response <- renderValueBox({
    if (is.null(values$comparison)) {
      valueBox("?", "Placebo Response Rate", icon = icon("medkit"), color = "red")
    } else {
      valueBox(
        value = paste0(round(values$comparison$placebo_mean_rate, 1), "%"),
        subtitle = "Placebo Response Rate",
        icon = icon("medkit"),
        color = "red"
      )
    }
  })
  
  output$p_value <- renderValueBox({
    if (is.null(values$comparison)) {
      valueBox("?", "P-Value", icon = icon("calculator"), color = "blue")
    } else {
      p_val <- values$comparison$p_value
      color <- ifelse(values$comparison$significant, "green", "red")
      valueBox(
        value = format(p_val, scientific = TRUE, digits = 3),
        subtitle = ifelse(values$comparison$significant, "✓ STATISTICALLY SIGNIFICANT", "✗ NOT SIGNIFICANT"),
        icon = icon("chart-line"),
        color = color
      )
    }
  })
  
  output$effect_size_box <- renderValueBox({
    if (is.null(values$comparison)) {
      valueBox("?", "Effect Size", icon = icon("ruler"), color = "blue")
    } else {
      valueBox(
        value = paste0(round(values$comparison$effect_size, 2), " - ", values$comparison$effect_interpretation),
        subtitle = "Cohen's d Effect Size",
        icon = icon("ruler"),
        color = "blue"
      )
    }
  })
  
  output$nnt_box <- renderValueBox({
    if (is.null(values$comparison) || is.na(values$comparison$nnt)) {
      valueBox("?", "Number Needed to Treat", icon = icon("users"), color = "purple")
    } else {
      valueBox(
        value = values$comparison$nnt,
        subtitle = "Number Needed to Treat (NNT)",
        icon = icon("users"),
        color = "purple"
      )
    }
  })
  
  # Response histogram
  output$response_histogram <- renderPlotly({
    if (is.null(values$treatment_results) || is.null(values$placebo_results)) {
      return(plotly_empty())
    }
    
    plot_df <- rbind(
      data.frame(Response_Rate = values$treatment_results$response_rate, Arm = "Treatment"),
      data.frame(Response_Rate = values$placebo_results$response_rate, Arm = "Placebo")
    )
    
    p <- ggplot(plot_df, aes(x = Response_Rate, fill = Arm)) +
      geom_histogram(alpha = 0.6, bins = 20, position = "identity") +
      scale_fill_manual(values = c("Treatment" = "#2E8B57", "Placebo" = "#CD5C5C")) +
      theme_minimal() +
      labs(title = "Response Rate Distribution (Monte Carlo)",
           x = "Response Rate (%)",
           y = "Frequency")
    
    ggplotly(p)
  })
  
  # Improvement boxplot
  output$improvement_boxplot <- renderPlotly({
    if (is.null(values$treatment_results) || is.null(values$placebo_results)) {
      return(plotly_empty())
    }
    
    plot_df <- rbind(
      data.frame(Improvement = values$treatment_results$mean_improvement, Arm = "Treatment"),
      data.frame(Improvement = values$placebo_results$mean_improvement, Arm = "Placebo")
    )
    
    p <- ggplot(plot_df, aes(x = Arm, y = Improvement, fill = Arm)) +
      geom_boxplot(alpha = 0.7) +
      scale_fill_manual(values = c("Treatment" = "#2E8B57", "Placebo" = "#CD5C5C")) +
      theme_minimal() +
      labs(title = "Mean Improvement Distribution",
           y = "Mean Improvement (%)",
           x = "")
    
    ggplotly(p)
  })
  
  # Comparison table
  output$comparison_table <- renderDT({
    if (is.null(values$comparison)) {
      return(datatable(data.frame(Message = "Run simulation to see results")))
    }
    
    datatable(values$comparison$comparison_table,
              options = list(pageLength = 10, dom = 't'),
              rownames = FALSE) %>%
      formatRound(columns = c("Treatment", "Placebo", "Difference"), digits = 1)
  })
  
  # Simulation convergence
  output$simulation_convergence <- renderPlotly({
    if (is.null(values$treatment_results)) {
      return(plotly_empty())
    }
    
    treatment_df <- values$treatment_results
    placebo_df <- values$placebo_results
    
    treatment_df$cumulative_mean <- cumsum(treatment_df$response_rate) / 1:nrow(treatment_df)
    placebo_df$cumulative_mean <- cumsum(placebo_df$response_rate) / 1:nrow(placebo_df)
    
    plot_df <- rbind(
      data.frame(Simulation = 1:nrow(treatment_df), 
                 Response_Rate = treatment_df$cumulative_mean, 
                 Arm = "Treatment"),
      data.frame(Simulation = 1:nrow(placebo_df), 
                 Response_Rate = placebo_df$cumulative_mean, 
                 Arm = "Placebo")
    )
    
    p <- ggplot(plot_df, aes(x = Simulation, y = Response_Rate, color = Arm)) +
      geom_line(size = 1) +
      scale_color_manual(values = c("Treatment" = "#2E8B57", "Placebo" = "#CD5C5C")) +
      theme_minimal() +
      labs(title = "Monte Carlo Simulation Convergence",
           x = "Number of Simulations",
           y = "Cumulative Mean Response Rate (%)")
    
    ggplotly(p)
  })
  
  # Patient table
  output$patient_table <- renderDT({
    patients <- generate_virtual_patients(30)
    datatable(patients[, c("patient_id", "age", "bmi", "sex", "baseline_severity", "genetic_responder")],
              options = list(pageLength = 10, scrollX = TRUE))
  })
  
  # Age distribution
  output$age_distribution <- renderPlotly({
    patients <- generate_virtual_patients(500)
    
    p <- ggplot(patients, aes(x = age)) +
      geom_histogram(fill = "#2E8B57", alpha = 0.7, bins = 30) +
      theme_minimal() +
      labs(title = "Virtual Patient Age Distribution",
           x = "Age (years)",
           y = "Count")
    
    ggplotly(p)
  })
  
  # Severity distribution
  output$severity_distribution <- renderPlotly({
    patients <- generate_virtual_patients(500)
    
    p <- ggplot(patients, aes(x = baseline_severity)) +
      geom_histogram(fill = "#CD5C5C", alpha = 0.7, bins = 30) +
      theme_minimal() +
      labs(title = "Baseline Disease Severity Distribution",
           x = "Severity Score",
           y = "Count")
    
    ggplotly(p)
  })
  
  # BMI distribution
  output$bmi_distribution <- renderPlotly({
    patients <- generate_virtual_patients(500)
    
    p <- ggplot(patients, aes(x = bmi)) +
      geom_histogram(fill = "#4ECDC4", alpha = 0.7, bins = 30) +
      theme_minimal() +
      labs(title = "BMI Distribution",
           x = "BMI (kg/m²)",
           y = "Count") +
      geom_vline(xintercept = 25, linetype = "dashed", color = "gray50") +
      geom_vline(xintercept = 30, linetype = "dashed", color = "gray50")
    
    ggplotly(p)
  })
  
  # Genetic responder pie chart
  output$genetic_pie <- renderPlotly({
    patients <- generate_virtual_patients(500)
    genetic_counts <- table(patients$genetic_responder)
    
    plot_ly(labels = c("Non-Responder", "Genetic Responder"),
            values = c(genetic_counts[1], genetic_counts[2]),
            type = 'pie',
            marker = list(colors = c("#95A5A6", "#2E8B57")),
            textinfo = 'label+percent') %>%
      layout(title = "Genetic Responder Status")
  })
  
  # Dose-response curve
  output$dose_response_curve <- renderPlotly({
    doses <- seq(0, 200, by = 2)
    responses <- calculate_response(doses)
    
    df <- data.frame(Dose = doses, Response = responses)
    
    p <- ggplot(df, aes(x = Dose, y = Response)) +
      geom_line(color = "#2E8B57", size = 1.5) +
      geom_ribbon(aes(ymin = Response - 5, ymax = Response + 5), alpha = 0.2, fill = "#2E8B57") +
      geom_hline(yintercept = 30, linetype = "dashed", color = "red", alpha = 0.5) +
      annotate("text", x = 180, y = 32, label = "Clinical Response Threshold (30%)", size = 3) +
      theme_minimal() +
      labs(title = "Sigmoid Emax Dose-Response Model",
           y = "Predicted Improvement (%)",
           x = "Dose (mg)",
           subtitle = "Shaded area represents patient variability")
    
    ggplotly(p)
  })
  
  # Optimal dose plot
  output$optimal_dose_plot <- renderPlotly({
    doses <- seq(input$optimal_dose_range[1], input$optimal_dose_range[2], by = 2)
    responses <- calculate_response(doses)
    
    df <- data.frame(Dose = doses, Response = responses)
    optimal_dose <- doses[which.max(responses)]
    optimal_response <- max(responses)
    
    p <- ggplot(df, aes(x = Dose, y = Response)) +
      geom_line(color = "#2E8B57", size = 1.5) +
      geom_point(data = df[which.max(df$Response), ], 
                 aes(x = Dose, y = Response), 
                 color = "red", size = 5) +
      theme_minimal() +
      labs(title = "Optimal Dose Selection",
           y = "Predicted Improvement (%)",
           x = "Dose (mg)",
           subtitle = paste("Optimal dose:", optimal_dose, "mg | Maximum efficacy:", round(optimal_response, 1), "%"))
    
    ggplotly(p)
  })
}

# Run the app
shinyApp(ui = ui, server = server)
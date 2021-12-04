

library(shiny)


myApp  <- function() {
  
  # Extract the files names with path for the SQ from the inst folder----
  file_names <-
    list.files(path = "inst/MFTC_calculator/App_Parameters/",
               full.names = TRUE)
  
  names(file_names) <-
    basename(file_names) |> strsplit("[_.]") |> sapply(function(x)
      x[2])
  
  # UI ---------------------------------------------------------------------
  ui <- fluidPage(
    # different themes from shinythemes R package, https://rstudio.github.io/shinythemes/
    theme = shinytheme("readable"),
    # Application title
    titlePanel("Income Explorer Prototype (version 0.3.1)"),
    
    # Side menu
    sidebarLayout(
      sidebarPanel(
        width = 4,
        # Select SQ parameters file
        selectInput(inputId = 'parameters_SQ',
                    label = 'Choose a Status quo setting of Tax Year',
                    choices = file_names, 
                    selected = file_names[grep("TY22", file_names)]),
        
        downloadButton("download_template", 
                       HTML("Download Parameter Template <br> for Reform changes")),
        
        # Select reform parameters file
        fileInput('parameters_Reform', 'Reform parameters', accept = c('xlsx')),
        
        # Download the data used in the app
        downloadButton("downloadData", "Download Results"),
        
        # Input the hourly wage and hours
        fluidRow(
          column(6, numericInput(
            "wage1_hourly", "Hourly wage ($):",
            min = 18, max = 100,
            value = 20, step = .5
            # pre = "$", sep = ",",
            # animate = FALSE
          )),
          column(6, numericInput(
            "max_hours", "Max hours/week", "50"
          ))),
        
        # Input to allow the user to choose situations of people
        # stay on benefit, or on WFF or showing the maximum amount
        # between the two options
        fluidRow(
          column(4, selectInput(
            "WFFBEN_SQ", "SQ: WFF or Benefit",
            c("Max", "WFF", "Benefit"), selected = "Max"
          )),
          column(4, selectInput(
            "WFFBEN_reform", "Reform: WFF or Benefit",
            c("Max", "WFF", "Benefit"), selected = "Max"
          )),
          column(4, selectInput(
            "MFTC_WEP_scaling", "WEP:",
            c("Average week" = 1, "Winter week" = 12/5, "Summer week" = 0),
            selected = "Average week"
          ))
        ),
        # Input accomodation cost settings
        fluidRow(
          column(4, numericInput(
            "AS_Accommodation_Costs", "Weekly accomodation cost ($):",
            min = 0, max = 1000, value = 450, step = 1, 
          )),
          column(4, selectInput(
            "AS_Area", label = "AS Area:",
            choices = c(1, 2, 3, 4), selected = 2
          )),
          column(4, selectInput(
            "Acc_type", label = "Accomodation type:",
            choices = c("Renting", "Mortgage"), selected = "Renting"
          ))
        ),
        # Input the poverty threshold
        fluidRow(
          column(4, numericInput(
            "bhc_median",
            "BHC equivalised median",
            "43000"
          )),
          column(4, numericInput(
            "ahc_median",
            "AHC equivalised median",
            "33100"
          )),
          column(4, numericInput(
            "pov_thresholds",
            "Poverty thresholds (fraction of medians)",
            "0.5",
            step = 0.1
          ))
        ),
        # Input partner status
        checkboxInput("Partnered", "Partnered", value = FALSE),
        # Input parter wage details, note that this is only
        # displayed if there is a partner
        fluidRow(
          column(6, conditionalPanel(
            condition = "input.Partnered == 1",
            numericInput(
              "gross_wage2", "Partner's hourly wage ($):",
              min = 15, max = 100, value = 20, step = .5
            )
          )),
          column(6, conditionalPanel(
            condition = "input.Partnered == 1",
            numericInput(
              "hours2", "Partner's hours worked:",
              min = 0, max = 80, value = 0, step = 1
            )
          ))
        ),
        # Input the children's ages
        textInput(
          "Children_ages",
          "Age of children (e.g. '1, 4' or leave blank)",
          "0, 10"
        ),
        # Check boxes to only display certain payments
        checkboxGroupInput(
          "income_types",
          "Select income types for Income composition plot:",
          c(
            "Net Income", "Best Start", "Winter Energy", "Accomodation Supplement",
            "IWTC", "FTC", "MFTC", "IETC", 
            "Net Core Benefit", "Net Wage", "Net Wage (Partner)",
            "Tax on Core Benefit", "Tax on Wage and ACC"
          ),
          inline = TRUE,
          selected = c(
            "Best Start", "Winter Energy", "Accomodation Supplement",
            "IWTC", "FTC", "MFTC", "IETC", 
            "Net Core Benefit", "Net Wage", "Net Wage (Partner)",
            "Tax on Core Benefit", "Tax on Wage and ACC"
          )
        )
      ),
      # Main panel containing plots etc.
      mainPanel(
        tabsetPanel(
          # Plot net income and EMTR
          tabPanel(
            "EMTR",
            h2("Net Income"), plotlyOutput("plot_netincome", height = "300px"),
            h2("Effective Marginal Tax Rate"), plotlyOutput("plot_emtr", height = "300px"),
            h2("Replacement Rate"), plotlyOutput("plot_replacement_rate", height = "300px"),
            h2("Participation Tax Rate"), plotlyOutput("plot_participation_tax_rate", height = "300px")
          ),
          # Plot poverty
          tabPanel(
            "Poverty",
            h2("Equivalised Income"), plotlyOutput("plot_equivincome", height = "300px"),
            h2("BHC Poverty"), plotlyOutput("plot_bhc_depth", height = "300px"),
            h2("AHC Poverty"), plotlyOutput("plot_ahc_depth", height = "300px")
          ),
          # Plot income composition/budget constraint
          tabPanel(
            "Income composition",
            plotlyOutput("plot_incomecomposition_SQ", height = "400px"),
            plotlyOutput("plot_incomecomposition_Reform", height = "400px")
          ),
          # Table containing the parameters that changed
          tabPanel("Parameters Adjustment", rhandsontable::rHandsontableOutput("show_parameters")),
          # Table containing the parameters that changed
          tabPanel("Parameters", tableOutput("changed_parameters"))
        )
      )
    )
  )
  
  # Define server logic required to run the app ---------------------------------

  server <- function(input, output, session) {
    ##########################################
    # Pop up warning on start 
    ##########################################
    # the modal dialog where the user can enter the query details.
    warning_modal <- modalDialog(
      title = "Warning", 
      paste0(
        "All outputs must be peer-reviewed by the TAWA team before being ",
        "included in advice. The Income Explorer App is currently a prototype, ",
        "please report any issues."
      ),
      easyClose = FALSE, fade = FALSE
    )
    
    # Show the model on start up ...
    showModal(warning_modal)
    
    ##########################################
    # Loading data and calculating incomes 
    ##########################################
    
    WEEKS_IN_YEAR <- 52L
    
    load_parameters_data <- reactive({
      
      show_all_parameters(
        input$parameters_SQ,
        input$parameters_Reform$datapath
      )
    })
    
    # display changed parameters 
    output$show_parameters <- rhandsontable::renderRHandsontable({
      DF <- load_parameters_data()
      rhandsontable(DF, height = 800) %>%
        hot_cols(
          colWidths = c(500, 200, 200),
          fixedColumnsLeft = 1,
          stretchH = "all"
        ) %>%
        hot_table(highlightCol = TRUE, highlightRow = TRUE)
    })    
    
    # Download Parameter template
    output$download_template <- downloadHandler(
      filename = function() {
        paste("Reform_", 
              # Below is extracting the Tax year for the file names of reform parameter file.
              basename(input$parameters_SQ) |>
                strsplit("[_.]") |>
                sapply(function(x) x[2]), 
              ".xlsx", sep = "")
      },
      content = function(file) {
        file.copy(input$parameters_SQ, file)
      }
    )
    
    
    # Read in the parameters from files
    reload_data <- reactive({
      
      req(input$parameters_SQ, input$parameters_Reform)
      
      if(!is.null(input$show_parameters)){
        DF <- hot_to_r(input$show_parameters)
        parameters_SQ <- parameters_from_df(DF, parameters_column = 2)
        parameters_Reform <- parameters_from_df(DF, parameters_column = 3)
      } else {
        parameters_SQ  <-  parameters_from_file(input$parameters_SQ)
        
        parameters_Reform <- parameters_from_file(input$parameters_Reform$datapath)
      }
      
      return(list(
        parameters_SQ = parameters_SQ,
        parameters_Reform = parameters_Reform
      ))
    })
    
    
    # calculate incomes 
    calculate_income <- reactive({
      parameters = req(reload_data())
      
      # convert inputs
      MAX_WAGE <- input$max_hours*input$wage1_hourly
      children <- convert_ages(input$Children_ages)
      
      if (input$Acc_type == "Renting"){
        AS_Accommodation_Rent <- TRUE
      } else {
        AS_Accommodation_Rent <- FALSE
      }
      
      if (input$Partnered == 1){
        partner_wages <- input$gross_wage2*input$hours2
        partner_hours <- input$hours2
      } else {
        partner_wages <- 0
        partner_hours <- 0
      }
      
      # Create helper emtr function using current inputs
      hot_emtr <- function(params) {
        emtr_df <- emtr(
          # System parameters
          params,
          # Family parameters
          input$Partnered, input$wage1_hourly, children, partner_wages, partner_hours,
          input$AS_Accommodation_Costs, AS_Accommodation_Rent, as.numeric(input$AS_Area),
          pov_thresholds = input$pov_thresholds,bhc_median = input$bhc_median,
          ahc_median = input$ahc_median,
          # Presentation parameters
          max_wage = MAX_WAGE, steps_per_dollar = 1L, weeks_in_year = WEEKS_IN_YEAR, 
          MFTC_WEP_scaling = as.numeric(input$MFTC_WEP_scaling)
        )
        return(emtr_df)
      }
      
      X_SQ <- hot_emtr(parameters$parameters_SQ)
      X_Reform <- hot_emtr(parameters$parameters_Reform)
      
      
      
      # MFTC is meant to make families always better off being off-benefit than staying
      # on a benefit. We let the user set whether the family stays on benefit
      # or gets IWTC when they work, with the parameter input$WFFBEN_SQ. This can be:
      # "Max" - choose the option which maximises the family income.
      # "WFF" - go off the benefit while working, and get IWTC + MFTC.
      # "Benefit" - stay on the benefit while working, and never get IWTC or MFTC
      #             (benefit abates away as earned income increases).
      # Note that these are only applicable when beneficiaries are ineligible for IWTC;
      # MFTC eligibility currently depends on IWTC eligibility in the `emtr` function.
      
      if (input$WFFBEN_SQ != "WFF") {
        IWTC_in_SQ <- IWTC_check(parameters$parameters_SQ)
        
        SQ_params_with_no_IWTC <- remove_IWTC_from_params(parameters$parameters_SQ)
        X_SQ_without_IWTC <- hot_emtr(SQ_params_with_no_IWTC)
        
        if (IWTC_in_SQ && input$WFFBEN_SQ == "Max") {
          # Choose which of benefit or IWTC gives max net income
          X_SQ <- choose_IWTC_or_benefit(X_SQ, X_SQ_without_IWTC)
        } else if (input$WFFBEN_SQ == "Benefit") {
          X_SQ <- X_SQ_without_IWTC
        }
      }
      
      if (input$WFFBEN_reform != "WFF") {
        IWTC_in_Reform <- IWTC_check(parameters$parameters_Reform)
        
        Reform_params_with_no_IWTC <- remove_IWTC_from_params(parameters$parameters_Reform)
        X_Reform_without_IWTC <- hot_emtr(Reform_params_with_no_IWTC)
        
        if (IWTC_in_Reform && input$WFFBEN_reform == "Max") {
          # Choose which of benefit or IWTC gives max net income
          X_Reform <- choose_IWTC_or_benefit(X_Reform, X_Reform_without_IWTC)
        }  else if (input$WFFBEN_reform == "WFF") {
          X_Reform <- X_Reform
        } else {
          X_Reform <- X_Reform_without_IWTC
        }
      }
      
      # used to ensure that SQ and reform plots have the same axes
      max_income <- max(
        max(X_SQ$gross_wage1_annual),
        max(X_Reform$gross_wage1_annual)
      )
      
      max_net_income <- 1.1*WEEKS_IN_YEAR*max(
        max(X_SQ$Net_Income), max(X_Reform$Net_Income)
      )
      min_y_SQ <- X_SQ[, WEEKS_IN_YEAR*min(
        -(
          gross_benefit1 + gross_benefit2 - net_benefit1 - net_benefit2 +
            wage1_tax + wage2_tax + wage1_ACC_levy + wage2_ACC_levy
        )
      )]
      min_y_Reform <- X_Reform[, WEEKS_IN_YEAR*min(
        -(
          gross_benefit1 + gross_benefit2 - net_benefit1 - net_benefit2 +
            wage1_tax + wage2_tax + wage1_ACC_levy + wage2_ACC_levy
        )
      )]
      min_y <- 1.1*min(min_y_SQ, min_y_Reform) 
      
      return(list(
        X_SQ = X_SQ, 
        X_Reform = X_Reform, 
        max_income = max_income,
        max_net_income = max_net_income, 
        min_y = min_y
      ))
    })
    
    ########################################## 
    ### EMTR Tab
    ########################################## 
    
    # net income plots
    output$plot_netincome<- renderPlotly({
      X_results <- req(calculate_income())
      
      X_SQ <- X_results$X_SQ
      X_Reform <- X_results$X_Reform
      
      compare_net_income_plot(X_SQ, X_Reform, inc_limit = X_results$max_income,
                              title = "Net income comparison", policy_name1 = 'Status Quo',
                              policy_name2 = 'Reform', watermark = FALSE, weeks_in_year = WEEKS_IN_YEAR
      )
    })
    
    # emtr plots
    output$plot_emtr<- renderPlotly({
      X_results <- req(calculate_income())
      
      X_SQ <- X_results$X_SQ
      X_Reform <- X_results$X_Reform
      
      compare_plots(X_SQ, X_Reform, type = "EMTR", min_rate = 0, max_rate = 1.1,
                    inc_limit = X_results$max_income, title = "EMTR comparison",
                    policy_name1 = 'Status Quo', policy_name2 = 'Reform',
                    watermark = FALSE, weeks_in_year = WEEKS_IN_YEAR
      )
    })
    
    # replacement rate plot
    output$plot_replacement_rate <- renderPlotly({
      X_results <- req(calculate_income())
      
      X_SQ <- X_results$X_SQ
      X_Reform <- X_results$X_Reform
      
      compare_plots(X_SQ, X_Reform, type = "RR", min_rate = 0, max_rate = 1.1,
                    inc_limit = X_results$max_income, title = "Replacement Rate comparison",
                    policy_name1 = 'Status Quo', policy_name2 = 'Reform', watermark = FALSE,  
                    weeks_in_year = WEEKS_IN_YEAR
      )
    })
    
    # participation tax rate plot
    output$plot_participation_tax_rate <- renderPlotly({
      X_results <- req(calculate_income())
      
      X_SQ <- X_results$X_SQ
      X_Reform <- X_results$X_Reform
      
      compare_plots(X_SQ, X_Reform, type = "PTR", min_rate = 0, max_rate = 1.1,
                    inc_limit = X_results$max_income, title = "Participation Tax Rate comparison",
                    policy_name1 = 'Status Quo', policy_name2 = 'Reform',
                    watermark = FALSE, weeks_in_year = WEEKS_IN_YEAR
      )
    })
    
    ########################################## 
    ### Poverty impact Tab
    ########################################## 
    
    # equivalised income plots
    output$plot_equivincome<- renderPlotly({
      X_results <- req(calculate_income())
      
      X_SQ <- X_results$X_SQ
      X_Reform <- X_results$X_Reform
      
      compare_equiv_income_plot(X_SQ, X_Reform, inc_limit = X_results$max_income,
                                title = "Equivalised income comparison", policy_name1 = 'Status Quo',
                                policy_name2 = 'Reform', watermark = FALSE, weeks_in_year = WEEKS_IN_YEAR
      )
    })
    
    # BHC poverty depth plots
    output$plot_bhc_depth<- renderPlotly({
      X_results <- req(calculate_income())
      
      X_SQ <- X_results$X_SQ
      X_Reform <- X_results$X_Reform
      
      poverty_depth_plot(X_SQ, X_Reform, inc_limit = X_results$max_income,
                         policy_name1 = 'Status Quo',
                         policy_name2 = 'Reform', watermark = FALSE, weeks_in_year = WEEKS_IN_YEAR,
                         type = "BHC"
      )
    })
    
    # AHC poverty depth plots
    
    output$plot_ahc_depth<- renderPlotly({
      X_results <- req(calculate_income())
      
      X_SQ <- X_results$X_SQ
      X_Reform <- X_results$X_Reform
      
      poverty_depth_plot(X_SQ, X_Reform, inc_limit = X_results$max_income,
                         policy_name1 = 'Status Quo',
                         policy_name2 = 'Reform', watermark = FALSE, weeks_in_year = WEEKS_IN_YEAR,
                         type = "AHC"
      )
    })
    
    ########################################## 
    ### Income compostion tab
    ########################################## 
    
    # income composition plots
    output$plot_incomecomposition_SQ<- renderPlotly({
      X_results <- req(calculate_income())
      X_SQ <- X_results$X_SQ
      amounts_net_plot(
        X_SQ,
        inc_limit = X_results$max_income,
        y_min = X_results$min_y,
        y_max = X_results$max_net_income,
        display_cols = input$income_types
      )
    })
    
    output$plot_incomecomposition_Reform <- renderPlotly({
      X_results <- req(calculate_income())
      X_Reform <- X_results$X_Reform
      amounts_net_plot(
        X_Reform,
        inc_limit = X_results$max_income,
        y_min = X_results$min_y,
        y_max = X_results$max_net_income,
        display_cols = input$income_types
      )
    })
    
    ########################################## 
    ### Parameters tab
    ##########################################  
    
    # display changed parameters 
    output$changed_parameters <- renderTable({
      parameters = reload_data()
      if (is.null(input$parameters_SQ) | is.null(input$parameters_Reform)) {
        return()
      }
      
      check_for_changed_parameters(
        parameters$parameters_SQ, parameters$parameters_Reform
      )
    })
    
    ########################################## 
    ### Outputs
    ##########################################
    
    # Download everything 
    output$downloadData <- downloadHandler(
      filename = function() {
        "IncomeExplorerResults.xlsx"
      },
      content = function(file) {
        X_results <- calculate_income()
        parameters = reload_data()
        
        wb <- createWorkbook()
        
        # Details of the example family and input files
        details <- c(
          SQ_file = input$parameters_SQ$name,
          Reform_file = input$parameters_Reform$name,
          HourlyWage = input$wage1_hourl,
          Partnered = input$Partnered,
          Partner_HourlyWage = input$gross_wage2*(input$Partnered == 1),
          Partner_HoursWorked = input$hours2*(input$Partnered == 1),
          Accomodation_Costs = input$AS_Accommodation_Costs,
          Accomodation_Type = input$Acc_type,
          AS_Area = input$AS_Area,
          Children_Ages = input$Children_ages
        )
        
        addWorksheet(wb, 'Details')
        writeData(wb, 'Details', names(details), startCol=1)
        writeData(wb, 'Details', details, startCol=2)
        
        # Parameters that changed
        parameter_differences <- check_for_changed_parameters(
          parameters$parameters_SQ, parameters$parameters_Reform
        )
        addWorksheet(wb, 'Scenario Differences')
        writeData(wb, 'Scenario Differences', parameter_differences)
        
        # Full sets of results (should probably be more selective)
        addWorksheet(wb, 'SQ')
        writeData(wb, 'SQ', X_results$X_SQ)
        addWorksheet(wb, 'Reform')
        writeData(wb, 'Reform', X_results$X_Reform)
        
        saveWorkbook(wb, file, overwrite = TRUE)
      }
    )
  }
  
  shinyApp(ui, server)
  
}
library(shiny)
library(shinydashboard)
library(shinyvalidate)
library(tidyverse)
library(tidymodels)
library(glmnet)

# Load models
eipm_pre <- readRDS("eIPMPre.rds")
eipm_post <- readRDS("eIPMPost.rds")

# Create UI
ui <- dashboardPage(
  
  title = "Early ICAHT Prediction",
  
  dashboardHeader(
    title = tags$a(
      href='https://hutchdatascience.org',
      tags$img(
        src='/fhLogo.png',
        height='35px',
        width='155px'
      )
    )
  ),
  
  # Sidebar with menu items
  dashboardSidebar(
    sidebarMenu(
    menuItem("Home", tabName = "home", icon = icon("book")),
    menuItem(HTML("eIPM<sub>Pre</sub>"), tabName = "pre", icon = icon("calculator")),
    menuItem(HTML("eIPM<sub>Post</sub>"), tabName = "post", icon = icon("calculator"))
  )
  ),
  
  # Tab items
  dashboardBody(
    
    includeCSS("www/hutch_theme.css"),
    tags$head(tags$title("Early ICAHT Prediction")),
    
    tags$head(tags$style(HTML(
      '.myClass { 
        font-size: 20px;
        line-height: 50px;
        text-align: left;
        font-family: "Arial",Helvetica,Arial,sans-serif;
        padding: 0 15px;
        overflow: hidden;
        color: white;
      }
    '))),
    tags$script(HTML('
      $(document).ready(function() {
        $("header").find("nav").append(\'<span class="myClass"> Early ICAHT Prediction </span>\');
      })
     ')),
    
    tabItems(
      # Home tab
      tabItem(
        tabName = "home",
        
        fluidRow(
          box(
            width = 12, solidHeader = TRUE,
            title = "About",
            p(
              HTML('The Early ICAHT Prediction Models (eIPMs) allow you to predict 
              a patient\'s probability of developing grade 3-4 early ICAHT based 
              on pre-lymphodepletion and/or early post-infusion factors.
              <br><br>
              Additional information on the development and performance of these 
              models can be found in the manuscript: <em>Liang EC et al. Development 
              and validation of predictive models of early immune effector 
              cell-associated hematotoxicity (eIPMs). Under review</em>.
              <br><br>
              This application was developed in partnership with the Fred 
              Hutch Data Science Lab (DaSL). For more information, or to report
              an issue, please go to the
              <a href="https://github.com/FredHutch/eipm-calculator">
              GitHub repo</a>.')
            )
          ),
          box(
            width = 12, solidHeader = TRUE,
            title = HTML("eIPM<sub>Pre</sub>"),
            p(
              HTML("eIPM<sub>Pre</sub> consists of disease type (ALL vs. other), 
              pre-lymphodepletion (LD) absolute neutrophil count, pre-LD platelet
              count, pre-LD LDH, and pre-LD ferritin.")
            )
          ),
          box(
            width = 12, solidHeader = TRUE,
            title = HTML("eIPM<sub>Post</sub>"),
            p(
              HTML("eIPM<sub>Post</sub> consists of 
                disease type (ALL vs. other), pre-LD ANC, pre-LD platelet count, 
                pre-LD LDH, and day +3 ferritin.")
            )
          )
        )
        ),
    
      # eIPMPre tab
      tabItem(
        tabName = "pre",

        fluidRow(
            box(
              selectInput(
                "disease_cat1",
                label = "Disease Type",
                choices = c("ALL", "Other")
              ),
              numericInput(
                "anc_pre_ld1",
                label = HTML(paste0("Pre-lymphodepletion ANC (x10",tags$sup("9"), '/μL)')),
                value = NULL,
                min = 0.7,
                max = 8
              ),
              numericInput(
                "plt_pre_ld1",
                label = HTML(paste0("Pre-lymphodepletion platelet count (x10",tags$sup("9"), '/μL)')),
                value = NULL,
                min = 25,
                max = 290
              ),
              numericInput(
                "ldh_pre_ld1",
                label = "Pre-lymphodepletion LDH (U/L)",
                value = NULL,
                min = 115,
                max = 645
              ),
              numericInput(
                "ferritin_pre_ld1",
                label = "Pre-lymphodepletion ferritin (ng/mL)",
                value = NULL,
                min = 30,
                max = 3900
              ),
              p("Input values are limited to those within the 5th-95th percentiles of the training dataset."),
              actionButton(inputId = "calculateNowPre", label = strong("Calculate")),
              actionButton(inputId = "resetPre", label = strong("Reset"))
            ),
            box(valueBoxOutput("eipm_pre_prediction", width = 12))

        )
        

      ),
    
      # eIPMPost tab
      tabItem(
        tabName = "post",
        
        fluidRow(
          box(
            selectInput(
              "disease_cat2",
              label = "Disease Type",
              choices = c("ALL", "Other")
            ),
            numericInput(
              "anc_pre_ld2",
              label = HTML(paste0("Pre-lymphodepletion ANC (x10",tags$sup("9"), '/μL)')),
              value = NULL,
              min = 0.7,
              max = 8
            ),
            numericInput(
              "plt_pre_ld2",
              label = HTML(paste0("Pre-lymphodepletion platelet count (x10",tags$sup("9"), '/μL)')),
              value = NULL,
              min = 25,
              max = 290
            ),
            numericInput(
              "ldh_pre_ld2",
              label = "Pre-lymphodepletion LDH (U/L)",
              value = NULL,
              min = 115,
              max = 645
            ),
            numericInput(
              "ferritin_day_3",
              label = "Day +3 ferritin (ng/mL)",
              value = NULL,
              min = 70,
              max = 4930
            ),
            p("Input values are limited to those within the 5th-95th percentiles of the training dataset."),
            actionButton(inputId = "calculateNowPost", label = strong("Calculate")),
            actionButton(inputId = "resetPost", label = strong("Reset"))
          ),
          box(valueBoxOutput("eipm_post_prediction", width = 12))
        )
        
    )
  ))
)

server <- function(input, output, session) {
  
  # Create blank eIPMPre box
  output$eipm_pre_prediction <- renderValueBox({
    valueBox(
      "%", 
      subtitle = paste0("Probability of grade 3-4 early ICAHT"),
      color = "aqua"
             ) 
  })
  
  # Create blank eIPMPost box
  output$eipm_post_prediction <- renderValueBox({
    valueBox(
      "%", 
      subtitle = paste0("Probability of grade 3-4 early ICAHT"),
      color = "aqua"
    ) 
  })
  
  # Create an InputValidator object
  iv_pre <- InputValidator$new()
  iv_post <- InputValidator$new()
  
  # Create validation rules for eIPMPre
  iv_pre$add_rule("anc_pre_ld1", sv_required())
  iv_pre$add_rule("anc_pre_ld1", function(value) {
    if (value < .7 || value > 8) {
      "Must be between .7 and 8"
    }
  })
  iv_pre$add_rule("plt_pre_ld1", sv_required())
  iv_pre$add_rule("plt_pre_ld1", function(value) {
    if (value < 25 || value > 290) {
      "Must be between 25 and 290"
    }
  })
  iv_pre$add_rule("ldh_pre_ld1", sv_required())
  iv_pre$add_rule("ldh_pre_ld1", function(value) {
    if (value < 115 || value > 645) {
      "Must be between 115 and 645"
    }
  })
  iv_pre$add_rule("ferritin_pre_ld1", sv_required())
  iv_pre$add_rule("ferritin_pre_ld1", function(value) {
    if (value < 30 || value > 3900) {
      "Must be between 30 and 3900"
    }
  })
  
  # Create validation rules for eIPMPost
  iv_post$add_rule("anc_pre_ld2", sv_required())
  iv_post$add_rule("anc_pre_ld2", function(value) {
    if (value < .7 || value > 8) {
      "Must be between .7 and 8"
    }
  })
  iv_post$add_rule("plt_pre_ld2", sv_required())
  iv_post$add_rule("plt_pre_ld2", function(value) {
    if (value < 25 || value > 290) {
      "Must be between 25 and 290"
    }
  })
  iv_post$add_rule("ldh_pre_ld2", sv_required())
  iv_post$add_rule("ldh_pre_ld2", function(value) {
    if (value < 115 || value 
        > 645) {
      "Must be between 115 and 645"
    }
  })
  iv_post$add_rule("ferritin_day_3", sv_required())
  iv_post$add_rule("ferritin_day_3", function(value) {
    if (value < 70 || value > 4930) {
      "Must be between 70 and 4930"
    }
  })
  
  # Turn on InputValidator
  iv_pre$enable()
  iv_post$enable()
  
  calculate_eipm_pre <- observeEvent(
    
    input$calculateNowPre,
    {
      output$eipm_pre_prediction <- renderValueBox({
        # Require eIPMPre values to be valid
        req(iv_pre$is_valid() |> isolate())
        
        # Create data frame using input variables 
        # (and dummy/NA variables for non-predictors)
        df <- tibble(
          "disease_cat" = input$disease_cat1 |> isolate(),
          "bridging_yn" = NA,
          "LD_regimen_low_CyFlu_vs_other" = NA,
          "anc_pre_ld" = input$anc_pre_ld1 |> isolate(),
          "anc_day_3" = NA,
          "plt_pre_ld" = input$plt_pre_ld1 |> isolate(),
          "plt_day_3" = NA,
          "ldh_pre_ld" = input$ldh_pre_ld1 |> isolate(),
          "ferritin_pre_ld" = input$ferritin_pre_ld1 |> isolate(),
          "ferritin_day_0" = NA,
          "ferritin_day_3" = NA,
          "crp_day_3" = NA,
          "d_dimer_day_3" = NA,
          "crs_grade" = NA
        )
        
        df <- df |>
          mutate(
            across(anc_pre_ld:d_dimer_day_3, ~ ifelse(. == 0, 0.01, .), .names = "{.col}_log10"),
            across(anc_pre_ld_log10:d_dimer_day_3_log10, log10)
          )
        
        # Generate predictions
        prediction <- predict(eipm_pre, df, type = "prob")
        probability <- round(100 * prediction$.pred_1, 0) 
        
        valueBox(
          value = paste0(probability, "%"),
          subtitle = paste0("Probability of grade 3-4 early ICAHT"),
          color = case_when(
            probability < 33  ~ "green",
            probability %in% 33:66 ~ "yellow",
            probability > 66 ~ "red",
            TRUE ~ "aqua"
            )
          )
      })
      
    }
    
  )
  
 # Hitting the reset button will clear all values
  observeEvent(input$resetPre, {
    updateSelectInput(session,"disease_cat1", selected = "ALL")
    updateNumericInput(session, "anc_pre_ld1", value = NA)
    updateNumericInput(session, "plt_pre_ld1", value = NA)
    updateNumericInput(session, "ldh_pre_ld1", value = NA)
    updateNumericInput(session, "ferritin_pre_ld1", value = NA)
    output$eipm_pre_prediction <- renderValueBox({
      valueBox("%", subtitle = paste0("Probability of grade 3-4 early ICAHT")) 
      })
  })
  
  calculate_eipm_post <- observeEvent(
    input$calculateNowPost,
    {
      # Require valid inputs
       req(iv_post$is_valid() |> isolate())
      
      output$eipm_post_prediction <- renderValueBox({
        # Create dataframe using input variables
        df <- tibble(
          "disease_cat" = input$disease_cat2 |> isolate(),
          "bridging_yn" = NA,
          "LD_regimen_low_CyFlu_vs_other" = NA,
          "anc_pre_ld" = input$anc_pre_ld2 |> isolate(),
          "anc_day_3" = NA,
          "plt_pre_ld" = input$plt_pre_ld2 |> isolate(),
          "plt_day_3" = NA,
          "ldh_pre_ld" = input$ldh_pre_ld2 |> isolate(),
          "ferritin_pre_ld" = NA,
          "ferritin_day_0" = NA,
          "ferritin_day_3" = input$ferritin_day_3 |> isolate(),
          "crp_day_3" = NA,
          "d_dimer_day_3" = NA,
          "crs_grade" = NA
        )
        
        df <- df |>
          mutate(
            across(anc_pre_ld:d_dimer_day_3, ~ ifelse(. == 0, 0.01, .), .names = "{.col}_log10"),
            across(anc_pre_ld_log10:d_dimer_day_3_log10, log10)
          )
        
        # Generate predictions
        prediction <- predict(eipm_post, df, type = "prob")
        probability <- round(100 * prediction$.pred_1, 0)
        
        # Generate valueBox output
        valueBox(
          value = paste0(probability, "%"),
          subtitle = paste0("Probability of grade 3-4 early ICAHT"),
          color = case_when(
            probability < 33  ~ "green",
            probability %in% 33:66 ~ "yellow",
            probability > 66 ~ "red",
            TRUE ~ "aqua"
          )
        )
        
      })
  
    }
  )
  
  # Hitting the reset button will clear all values
  observeEvent(input$resetPost, {
    updateSelectInput(session,"disease_cat2", selected = "ALL")
    updateNumericInput(session, "anc_pre_ld2", value = NA)
    updateNumericInput(session, "plt_pre_ld2", value = NA)
    updateNumericInput(session, "ldh_pre_ld2", value = NA)
    updateNumericInput(session, "ferritin_day_3", value = NA)
    output$eipm_post_prediction <- renderValueBox({
      valueBox("%", subtitle = paste0("Probability of grade 3-4 early ICAHT")) 
    })
  })
  
}

shinyApp(ui, server)
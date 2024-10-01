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
    menuItem("eIPMPre", tabName = "pre", icon = icon("calculator")),
    menuItem("eIPMPost", tabName = "post", icon = icon("calculator"))
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
        h4(
          "The Early ICAHT Prediction Models (eIPMs) allow you to predict a
            patient's probability of developing grade 3-4 early ICAHT based on 
            pre-lymphodepletion (eIPMPre) and/or early post-infusion (eIPMPost) 
            factors. Based on the models described in Liang et al., 2024 (under 
            review)."
        )
      ),
    
      # eIPMPre tab
      tabItem(
        tabName = "pre",
        box(valueBoxOutput("eipm_pre_prediction", width = 12)),
        box(selectInput(
          "disease_cat",
          label = "Disease Type",
          choices = c("ALL", "Other")
        )),
        box(
          numericInput(
            "anc_pre_ld",
            label = HTML(paste0("Pre-lymphodepletion ANC (x10",tags$sup("9"), '/L)')),
            value = NULL,
            min = 0.7,
            max = 8
          )
        ),
        box(
          numericInput(
            "plt_pre_ld",
            label = HTML(paste0("Pre-lymphodepletion platelet count (x10",tags$sup("9"), '/L)')),
            value = NULL,
            min = 25,
            max = 290
          )
        ),
        box(
          numericInput(
            "ldh_pre_ld",
            label = "Pre-lymphodepletion LDH (U/L)",
            value = NULL,
            min = 115,
            max = 645
          )
        ),
        box(
          numericInput(
            "ferritin_pre_ld",
            label = "Pre-lymphodepletion ferritin (ng/mL)",
            value = NULL,
            min = 30,
            max = 3900
          )
        ),
        box(
          actionButton(inputId = "calculateNowPre", label = strong("Calculate")),
          actionButton(inputId = "resetPre", label = strong("Reset"))
        ),
        box(
          "Input values are limited to those within the 5th-95th percentiles of the training dataset."
        )
      ),
    
      # eIPMPost tab
      tabItem(
        tabName = "post",
        box(valueBoxOutput("eipm_post_prediction", width = 12)),
        box(selectInput(
          "disease_cat2",
          label = "Disease Type",
          choices = c("ALL", "Other")
        )),
        box(
          numericInput(
            "anc_pre_ld2",
            label = HTML(paste0("Pre-lymphodepletion ANC (x10",tags$sup("9"), '/L)')),
            value = NULL,
            min = 0.7,
            max = 8
          )
        ),
        box(
          numericInput(
            "plt_pre_ld2",
            label = HTML(paste0("Pre-lymphodepletion platelet count (x10",tags$sup("9"), '/L)')),
            value = NULL,
            min = 25,
            max = 290
          )
        ),
        box(
          numericInput(
            "ldh_pre_ld2",
            label = "Pre-lymphodepletion LDH (U/L)",
            value = NULL,
            min = 115,
            max = 645
          )
        ),
        box(
          numericInput(
            "ferritin_day_3",
            label = "Day +3 ferritin (ng/mL)",
            value = NULL,
            min = 70,
            max = 4930
          )
        ),
        box(
          actionButton(inputId = "calculateNowPost", label = strong("Calculate")),
          actionButton(inputId = "resetPost", label = strong("Reset"))
        ),
        box(
          "Input values are limited to those within the 5th-95th percentiles of the training dataset."
        )
    )
  ))
)

server <- function(input, output, session) {
  # validation
  iv <- InputValidator$new()
  # validation - pre
  iv$add_rule("anc_pre_ld", sv_required())
  iv$add_rule("anc_pre_ld", function(value) {
    if (value < .7 || value > 8) {
      "Must be between .7 and 8"
    }
  })
  iv$add_rule("plt_pre_ld", sv_required())
  iv$add_rule("plt_pre_ld", function(value) {
    if (value < 25 || value > 290) {
      "Must be between 25 and 290"
    }
  })
  iv$add_rule("ldh_pre_ld", sv_required())
  iv$add_rule("ldh_pre_ld", function(value) {
    if (value < 115 || value > 645) {
      "Must be between 115 and 645"
    }
  })
  iv$add_rule("ferritin_pre_ld", sv_required())
  iv$add_rule("ferritin_pre_ld", function(value) {
    if (value < 30 || value > 3900) {
      "Must be between 30 and 3900"
    }
  })
  
  # validation - post
  iv$add_rule("anc_pre_ld2", sv_required())
  iv$add_rule("anc_pre_ld2", function(value) {
    if (value < .7 || value > 8) {
      "Must be between .7 and 8"
    }
  })
  iv$add_rule("plt_pre_ld2", sv_required())
  iv$add_rule("plt_pre_ld2", function(value) {
    if (value < 25 || value > 290) {
      "Must be between 25 and 290"
    }
  })
  iv$add_rule("ldh_pre_ld2", sv_required())
  iv$add_rule("ldh_pre_ld2", function(value) {
    if (value < 115 || value > 645) {
      "Must be between 115 and 645"
    }
  })
  iv$add_rule("ferritin_day_3", sv_required())
  iv$add_rule("ferritin_day_3", function(value) {
    if (value < 70 || value > 4930) {
      "Must be between 70 and 4930"
    }
  })
  
  iv$enable()
  
  calculation_eipm_pre <- eventReactive(
    
    input$calculateNowPre,
    {
      # require valid inputs
      req(iv$is_valid())
      
      # Create dataframe using input variables 
      # (and dummy/NA variables for non-predictors)
      df <- tibble(
        "disease_cat" = input$disease_cat,
        "bridging_yn" = NA,
        "LD_regimen_low_CyFlu_vs_other" = NA,
        "anc_pre_ld" = input$anc_pre_ld,
        "anc_day_3" = NA,
        "plt_pre_ld" = input$plt_pre_ld,
        "plt_day_3" = NA,
        "ldh_pre_ld" = input$ldh_pre_ld,
        "ferritin_pre_ld" = input$ferritin_pre_ld,
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
      
      probability
    }
  )
  
  
  # eIPMPre output
  output$eipm_pre_prediction <- renderValueBox({
    
    probability_pre <- calculation_eipm_pre()
    
    # Generate valueBox output
    valueBox(
      value = paste0(probability_pre, "%"),
      subtitle = paste0("Probability of grade 3-4 early ICAHT"),
      color = case_when(
        probability_pre < 33  ~ "green",
        probability_pre %in% 33:66 ~ "yellow",
        probability_pre > 66 ~ "red"
      )
    )
  })
  
 # Hitting the reset button will clear all values
  observeEvent(input$resetPre, {
    updateSelectInput(session,"disease_cat", selected = "ALL")
    updateNumericInput(session, "anc_pre_ld", value = NA)
    updateNumericInput(session, "plt_pre_ld", value = NA)
    updateNumericInput(session, "ldh_pre_ld", value = NA)
    updateNumericInput(session, "ferritin_pre_ld", value = NA)
    output$eipm_pre_prediction <- renderValueBox({
      valueBox("%", subtitle = paste0("Probability of grade 3-4 early ICAHT")) 
      })
  })
 
  calculation_eipm_post <- eventReactive(
    input$calculateNowPost,
    {
      # require valid inputs
      req(iv$is_valid())
      
      # Create dataframe using input variables
      df <- tibble(
        "disease_cat" = input$disease_cat2,
        "bridging_yn" = NA,
        "LD_regimen_low_CyFlu_vs_other" = NA,
        "anc_pre_ld" = input$anc_pre_ld2,
        "anc_day_3" = NA,
        "plt_pre_ld" = input$plt_pre_ld2,
        "plt_day_3" = NA,
        "ldh_pre_ld" = input$ldh_pre_ld2,
        "ferritin_pre_ld" = NA,
        "ferritin_day_0" = NA,
        "ferritin_day_3" = input$ferritin_day_3,
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
      
      probability
  
    }
    
  )
  
  # eIPMPost output
  output$eipm_post_prediction <- renderValueBox({
 
    probability_post <- calculation_eipm_post()
    
    # Generate valueBox output
    valueBox(
      value = paste0(probability_post, "%"),
      subtitle = paste0("Probability of grade 3-4 early ICAHT"),
      color = case_when(
        probability_post < 33  ~ "green",
        probability_post %in% 33:66 ~ "yellow",
        probability_post > 66 ~ "red"
      )
    )
    
  })
  
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
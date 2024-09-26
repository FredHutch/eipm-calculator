library(shiny)
library(shinydashboard)
library(tidyverse)
library(tidymodels)
library(glmnet)

addResourcePath('assets', 'www')

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
        src='/assets/fhLogo.png',
        height='35px',
        width='155px'
      )
    )
  ),
  
  # Sidebar with menu items
  dashboardSidebar(
    sidebarMenu(
    menuItem("Home", tabName = "home", icon = icon("book")),
    menuItem("eIPMPre", tabName = "eipm_pre", icon = icon("calculator")),
    menuItem("eIPMPost", tabName = "eipm_post", icon = icon("calculator"))
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
        tabName = "eipm_pre",
        box(valueBoxOutput("eipm_pre_prediction", width = 12)),
        box(selectInput(
          "disease_cat",
          label = "Disease Type",
          choices = c("ALL", "Other")
        )),
        box(
          sliderInput(
            "anc_pre_ld",
            label = "Pre-lymphodepletion ANC (x 10^9/L)",
            value = 1.5,
            min = 0.7,
            max = 8,
            step = 0.1
          )
        ),
        box(
          sliderInput(
            "plt_pre_ld",
            label = "Pre-lymphodepletion platelet count (x 10^9/L)",
            value = 200,
            min = 25,
            max = 290,
            step = 5
          )
        ),
        box(
          sliderInput(
            "ldh_pre_ld",
            label = "Pre-lymphodepletion LDH (U/L)",
            value = 250,
            min = 115,
            max = 645,
            step = 5
          )
        ),
        box(
          sliderInput(
            "ferritin_pre_ld",
            label = "Pre-lymphodepletion ferritin (ng/mL)",
            value = 300,
            min = 30,
            max = 3900,
            step = 5
          )
        ),
        box(
          "Input values are limited to those within the 5th-95th percentiles of the training dataset."
        )
      ),
    
      # eIPMPost tab
      tabItem(
        tabName = "eipm_post",
        box(valueBoxOutput("eipm_post_prediction", width = 12)),
        box(selectInput(
          "disease_cat",
          label = "Disease Type",
          choices = c("ALL", "Other")
        )),
        box(
          sliderInput(
            "anc_pre_ld",
            label = "Pre-lymphodepletion ANC (x 10^9/L)",
            value = 1.5,
            min = 0.7,
            max = 8,
            step = 0.1
          )
        ),
        box(
          sliderInput(
            "plt_pre_ld",
            label = "Pre-lymphodepletion platelet count (x 10^9/L)",
            value = 200,
            min = 25,
            max = 290,
            step = 5
          )
        ),
        box(
          sliderInput(
            "ldh_pre_ld",
            label = "Pre-lymphodepletion LDH (U/L)",
            value = 250,
            min = 115,
            max = 645,
            step = 5
          )
        ),
        box(
          sliderInput(
            "ferritin_day_3",
            label = "Day +3 ferritin (ng/mL)",
            value = 300,
            min = 70,
            max = 4930,
            step = 5
          )
        ),
        box(
          "Input values are limited to those within the 5th-95th percentiles of the training dataset."
        )
    )
  ))
)

server <- function(input, output) {
  # eIPMPre output
  output$eipm_pre_prediction <- renderValueBox({
    # Create dataframe using input variables (and dummy/NA variables for non-predictors)
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
    
    df <- df %>%
      mutate(
        across(anc_pre_ld:d_dimer_day_3, ~ ifelse(. == 0, 0.01, .), .names = "{.col}_log10"),
        across(anc_pre_ld_log10:d_dimer_day_3_log10, log10)
      )
    
    # Generate predictions
    prediction <- predict(eipm_pre, df, type = "prob")
    probability <- round(100 * prediction$.pred_1, 0)
    
    # Generate valueBox output
    valueBox(
      value = paste0(probability, "%"),
      subtitle = paste0("Probability of grade 3-4 early ICAHT"),
      color = case_when(
        probability < 33  ~ "green",
        probability %in% 33:66 ~ "yellow",
        probability > 66 ~ "red"
      )
    )
    
  })
  
  # eIPMPost output
  output$eipm_post_prediction <- renderValueBox({
    # Create dataframe using input variables
    df <- tibble(
      "disease_cat" = input$disease_cat,
      "bridging_yn" = NA,
      "LD_regimen_low_CyFlu_vs_other" = NA,
      "anc_pre_ld" = input$anc_pre_ld,
      "anc_day_3" = NA,
      "plt_pre_ld" = input$plt_pre_ld,
      "plt_day_3" = NA,
      "ldh_pre_ld" = input$ldh_pre_ld,
      "ferritin_pre_ld" = NA,
      "ferritin_day_0" = NA,
      "ferritin_day_3" = input$ferritin_day_3,
      "crp_day_3" = NA,
      "d_dimer_day_3" = NA,
      "crs_grade" = NA
    )
    
    df <- df %>%
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
        probability > 66 ~ "red"
      )
    )
    
  })
  
}

shinyApp(ui, server)
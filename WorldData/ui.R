library(shiny)
library(shinyWidgets)
library(plotly)
library(here)
library(shinydashboard)
library(maps)
library(leaflet)
library(tidyverse)
library(shinycssloaders)
library(shinythemes)
#library(datadigest)
library(rio)
library(DT)
library(stargazer)
library(estimatr)
library(texreg)
my_data <- read_csv(
  here("data", "full_data.csv")
)
my_data$Region <- countrycode::countrycode(
  my_data$Country, "country.name", "continent"
)
my_data <- my_data %>%
  select(Country, Region, year, everything())
dashboardPage(
  dashboardHeader(
    title = "War, Wealth, & World Politics", 
    dropdownMenuOutput("msgOutput"),
    titleWidth = 300),
  dashboardSidebar(
    fluidPage(br(),
              box(
                pickerInput(
                  "SelectCountry",
                  label = "Select Country:",
                  choices = sort(unique(my_data$Country)),
                  multiple = TRUE,
                  options = list(`actions-box` = TRUE),
                ),
                solidHeader = TRUE,
                width = 30,
                status = "primary",
                title = "Country"
              ),
              box(
                pickerInput(
                  "SelectRegion",
                  label = "Select Region:",
                  choices = sort(unique(my_data$Region)),
                  multiple = TRUE,
                  options = list(`actions-box` = TRUE),
                ),
                solidHeader = TRUE,
                width = 30,
                status = "primary",
                title = "Region"
              ))
    
    #
    # menuItem(
    #   "Generate Report",
    #   tabName = "sectors",
    #   icon = icon("download"),
    #   radioButtons(
    #     'format',
    #     'Document format',
    #     c('HTML', 'Word'),
    #     inline = FALSE,
    #     selected = 1
    #   ),
    #   downloadButton("report", "Download Report", class = "butt"),
    #   tags$head(tags$style(".butt{color: blue !important;}"))
    # )
  ),
  dashboardBody(
    fluidPage(
        box(
            selectInput(
                "SelectVar1",
                label = "Select:",
                choices = sort(names(my_data)[-1]),
                multiple = FALSE,
                selected = "terrorist_attacks"
            ),
            solidHeader = TRUE,
            width = 5,
            status = "primary",
            title = "Varible 'X'"
        ),
        box(
            selectInput(
                "SelectVar2",
                label = "Select:",
                choices = sort(names(my_data)[-c(1, 2)]),
                multiple = FALSE,
                selected = "terrorist_attack_fatalities"
            ),
            solidHeader = TRUE,
            width = 5,
            status = "primary",
            title = "Variable 'Y'"
        )
    ),
    
    fluidPage(  
      
      tabBox(
        id = "tabset1",
        height = "1000px",
        width = 12,
        
        tabPanel("Data",
                 box(withSpinner(DTOutput(
                   "Data"
                 )), width = 12)),
        tabPanel("Description",
                 box(withSpinner(DTOutput(
                   "Description"
                 )), width = 12)),
        # tabPanel(
        #   "Data Summary",
        #   box(withSpinner(verbatimTextOutput("Summ")), width = 12)
        #   #box(withSpinner(verbatimTextOutput("Summ_old")), width = 12)
        # ),
        
        
        # tabPanel("Data Strucure",
        #          box(
        #            withSpinner(verbatimTextOutput("structure")), width = "100%"
        #          ),
        #          explorerOutput("digest")
        # ),
        # tabPanel("Correlations",
        #          box(withSpinner(plotOutput(
        #            "Corr", width = "100%"
        #          )), width = 12)),
        #box(withSpinner(verbatimTextOutput("CorrMatrix")), width = 12),
        tabPanel(
          "Relationships",
          # box(
            withSpinner(
              plotlyOutput(
                "Relationships",
                width = "100%"
              )
            )
          #   width = 12,
          #   title = "Trends in World Data"
          # )
          # box(
          #   withSpinner(verbatimTextOutput("Model_new")),
          #   width = 6,
          #   title = "Model Summary"
          # ),
          # 
        )
      )
    )
  )
)
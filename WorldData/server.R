library(shiny)
library(here)
library(shinydashboard)
library(maps)
library(tidyverse)
library(leaflet)
library(DT)
library(plotly)
#library(corrplot)
library(caret)
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
dd <- my_data
shinyServer(function(input, output, session) {
  
  InputDataset <- reactive({
    if(is.null(input$SelectCountry) & is.null(input$SelectRegion)) {
      dt <- my_data %>%
        .[ ,c("Country", "Region", "year", input$SelectVar1, input$SelectVar2)]
    } else if(!is.null(input$SelectCountry) & is.null(input$SelectRegion)){
      dt <- my_data %>%
        filter(Country %in% input$SelectCountry) %>%
        .[ ,c("Country", "Region", "year", input$SelectVar1, input$SelectVar2)]
    } else if(is.null(input$SelectCountry) & !is.null(input$SelectRegion)){
      dt <- my_data %>%
        filter(Region %in% input$SelectRegion) %>%
        .[ ,c("Country", "Region", "year", input$SelectVar1, input$SelectVar2)]
    } else {
      dt <- my_data %>%
        filter(Region %in% input$SelectRegion | Region %in% input$SelectCountry) %>%
        .[ ,c("Country", "Region", "year", input$SelectVar1, input$SelectVar2)]
    }
    dt %>% na.omit()
  })
  
  
  
  # observe({
  #   lstname <- names(InputDataset())[3]
  #   updateSelectInput(session = session,
  #                     inputId = "SelectY",
  #                     choices = lstname)
  # })
  # 
  # splitSlider <- reactive({
  #   input$Slider1 / 100
  # })
  # set.seed(100)  # setting seed to reproduce results of random sampling
  # trainingRowIndex <-
  #   reactive({
  #     sample(1:nrow(InputDataset_model()),
  #            splitSlider() * nrow(InputDataset_model()))
  #   })# row indices for training data
  # 
  # trainingData <- reactive({
  #   tmptraindt <- InputDataset_model()
  #   tmptraindt[trainingRowIndex(), ]
  # })
  
  # testData <- reactive({
  #   tmptestdt <- InputDataset_model()
  #   tmptestdt[-trainingRowIndex(),]
  # })
  
  
  
  # output$cntTrain <-
  #   renderText(paste("Train Data:", NROW(trainingData()), "records"))
  # output$cntTest <-
  #   renderText(paste("Test Data:", NROW(testData()), "records"))
  # 
  output$Data <- renderDT(InputDataset(),
                          options = list(scrollX = TRUE,
                                         scrollY = TRUE))

  # Add variable descriptions -----------------------------------------------
  Description <- tibble(
    Variable = names(my_data)[-c(1:3)],
    Description = c(
      "Number of recorded terrorist attacks.",
      "Number of injuries from terrorist attacks.",
      "Number of fatalities from terrorist attacks.",
      "Number of officially reported nuclear weapons.",
      "(%) Share of population with internet access.",
      "Annual production-based emissions of carbon dioxide (CO2), measured in million tonnes.",
      "Average life expectancy in years.",
      "Size of the immigrant population (migrant stock).",
      "Value of annaul imports in constant USD",
      "Value of annual exports in constant USD",
      "Exchange rate of currency relative to the US Dollar",
      "Gross domestic product per capita",
      "Polity 2 score. 10 is most democratic, and -10 is least democratic.",
      "Population in millions",
      "(%) Difference in value of imports relative to exports."
    ),
    Link = c(
      "https://www.rand.org/nsrd/projects/terrorism-incidents.html",
      "https://www.rand.org/nsrd/projects/terrorism-incidents.html",
      "https://www.rand.org/nsrd/projects/terrorism-incidents.html",
      "https://fas.org/issues/nuclear-weapons/status-world-nuclear-forces/",
      "https://data.worldbank.org/indicator/IT.NET.USER.ZS",
      "https://github.com/owid/co2-data",
      "https://data.worldbank.org/indicator/SP.DYN.LE00.IN",
      "https://data.worldbank.org/indicator/SM.POP.TOTL",
      "https://data.worldbank.org/indicator/NE.IMP.GNFS.KD",
      "https://data.worldbank.org/indicator/NE.EXP.GNFS.KD",
      "https://www.rug.nl/ggdc/productivity/pwt/",
      "https://www.rug.nl/ggdc/productivity/pwt/",
      "https://www.systemicpeace.org/csprandd.html",
      "https://www.rug.nl/ggdc/productivity/pwt/",
      "see links for imports and exports."
    )
  ) 
  
  output$Description <- renderDT(Description,
                                 options = list(scrollX = TRUE,
                                                scrollY = TRUE))
  
  #Code section for Linear Regression-----------------------------------------------------------------------------
  
  Linear_plot <- renderPlotly({
    dt <- InputDataset() 
    names(dt) <- c("Country", "Region", "year", "var1", "var2")
    year <- all(dt$year == dt$var1)
    cnames <- function(x) stringr::str_replace_all(
      names(x), "_", " "
    )
    p <- ggplot(dt) +
      aes(
        x = var1,
        y = var2
      ) 
      if(year) {
        p <- p + geom_line(
          aes(group = Country),
          color = "grey70"
        ) +
          stat_summary(
            fun = ~mean(.x, na.rm=T),
            geom = "line",
            size = 1.1
          ) +
          # geom_smooth(color = "black",
          #             method = "loess",
          #             se = F,
          #             size = 1.1) +
          scale_y_continuous(
            labels = scales::comma
          )
      } else {
        p <- p + geom_point(
          aes(group = Country),
          color = "grey40"
        ) +
          geom_smooth(
            method = "loess",
            se = FALSE,
            color = "black"
          ) +
          scale_x_continuous(
            labels = scales::comma
          ) +
          scale_y_continuous(
            labels = scales::comma
          )
      } 
    p <- p + 
      labs(
        x = cnames(InputDataset())[3],
        y = cnames(InputDataset())[4],
        title = paste0("Trend in ", cnames(InputDataset())[4],
                       " by ", cnames(InputDataset())[3]),
        caption = "data: various | @MDWilliamsPhD"
      ) +
      theme_bw()
     ggplotly(p) %>%
      highlight(on = 'plotly_hover')
  })
  
  output$Relationships <- #renderPlotly({
    reactive(Linear_plot())
#  })
  
  # Linear_Model <- reactive({
  #   lm_robust(foreign_aid ~ . - recipient, data = InputDataset(),
  #             se_type = "stata")
  # })
  # 
  # output$Model <- renderPrint(
  #   screenreg(
  #     Linear_Model(), 
  #     include.ci = FALSE,
  #     custom.model.names = "foreign_aid",
  #     custom.header = list("Outcome:" = 1)
  #   )
  # )
  # output$Model_new <-
  #   renderPrint(
  #     stargazer(
  #       Linear_Model(),
  #       type = "text",
  #       title = "Model Results",
  #       digits = 1,
  #       out = "table1.txt"
  #     )
  #   )
  
  # Importance <- reactive({
  #   varImp(Linear_Model(), scale = FALSE)
  # })
  
  # tmpImp <- reactive({
  #   
  #   imp <- as.data.frame(varImp(Linear_Model()))
  #   imp <- data.frame(overall = imp$Overall,
  #                     names   = rownames(imp))
  #   imp[order(imp$overall, decreasing = T),]
  #   
  # })
  
  # output$ImpVar <- renderPrint(tmpImp())
  # 
  # price_predict <- reactive({
  #   predict(Linear_Model(), testData())
  # })
  # 
  # tmp <- reactive({
  #   tmp1 <- testData()
  #   tmp1[, c(input$SelectY)]
  # })
  # 
  # 
  # actuals_preds <-
  #   reactive({
  #     data.frame(cbind(actuals = tmp(), predicted = price_predict()))
  #   })
  # 
  # Fit <-
  #   reactive({
  #     (
  #       plot(
  #         actuals_preds()$actuals,
  #         actuals_preds()$predicted,
  #         pch = 16,
  #         cex = 1.3,
  #         col = "blue",
  #         main = "Best Fit Line",
  #         xlab = "Actual",
  #         ylab = "Predicted"
  #       )
  #     )
  #   })
  # 
  # output$Prediction <- renderPlot(Fit())
  # 
  # output$residualPlots <- renderPlot({
  #   par(mfrow = c(2, 2)) # Change the panel layout to 2 x 2
  #   plot(Linear_Model())
  #   par(mfrow = c(1, 1)) # Change back to 1 x 1
  #   
  # })
  # 
  # output$digest <- renderExplorer({
  #   
  #   explorer(data = dd$data, demo = F)
  #   
  # })  
  
})
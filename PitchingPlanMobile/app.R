library(shiny)
library(shinyMobile)
library(tidyverse)
library(ggplot2)
library(mgcv)
library(REdaS)
library(gridExtra)
library(googlesheets4)
library(chron)
library(stringr)
library(gt)
library(glue)
library(plotly)

source("source.R")

ui <- f7Page(
    title =  "Individualized Pitching Plans", 
    # Application title

    f7Tabs(
        animated = TRUE,
        id = "Tabs",
        f7Tab(tabName = "Rapsodo",
              active = FALSE,
              f7Select("pitcher",
                          "Select Pitcher:",
                          sort(unique(as.character(rapsodo$Pitcher)))),
              gt_output("rapsodo")),
        # f7Tab("Location Plots", plotOutput('whiffplot')),
        f7Tab(tabName = "Pre/Post Throwing",
              active = FALSE,
              gt_output('prepost'))
        # tabPanel("Monthly Plan", gt_output("monthlyplan")),
        # tabPanel("Throwing Key", gt_output("throwingkey"))
        )
)

server <- function(input, output) {

    output$rapsodo <- render_gt({
        rapsodo_summary_table(input$pitcher)
    })
    
    output$monthlyplan <- render_gt({
        generate_monthly_plan(input$pitcher)
    })
    
    output$whiffplot <- renderPlot({
        whiff_chart(input$pitcher)
    })
    
    output$prepost <- render_gt({
        pre_post_throwing_plan(input$pitcher)
    })
    
    output$pitchType <- renderUI({
        pitches <- unique(as.character(filter(rapsodo,Pitcher == input$pitcher)$PITCH.TYPE))
        selectInput("pitchtype",
                    "Select Pitch Type:",
                    sort(pitches))
    })
    
    output$movementchart <- renderPlotly({
        movement_trend_graphs(input$pitcher,input$pitchtype)
    })
    
    output$throwingkey <- render_gt({
        throwing_key
    })
}

# Run the application 
shinyApp(ui = ui, server = server)

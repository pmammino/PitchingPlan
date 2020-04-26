library(shiny)
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

source("source.R")

ui <- fluidPage(
        tags$head(HTML("<title>Individualized Pitcher Reports</title> <link rel='icon' type='image/gif/jpg' href='wagner.jpg'>")),
        # Application title
        titlePanel(title = div("Individualized Pitcher Reports",img(src="wagner.jpg", width = 50, height = 50))),
        
        selectInput("pitcher",
                    "Select Pitcher:",
                    sort(unique(as.character(rapsodo$Pitcher)))),
        tabsetPanel(
            tabPanel("Rapsodo", gt_output("rapsodo"),
                     uiOutput("pitchType"),
                     plotlyOutput('movementchart')),
            tabPanel("Location Plots", plotOutput('whiffplot'),
                     plotOutput("breakerplot"),
                     plotOutput("offspeedplot")),
            tabPanel("Development Plan", 
                     gt_output('development')),
            tabPanel("Pre/Post Throwing", 
                     gt_output('prepost')),
            tabPanel("Monthly Plan", 
                     gt_output("monthlyplan")),
            tabPanel("Throwing Key", 
                     gt_output("throwingkey"))
        )
)

server <- function(input, output) {

    output$rapsodo <- render_gt({
        rapsodo_summary_table(input$pitcher)
    })
    
    output$monthlyplan <- render_gt({
        generate_monthly_plan(input$pitcher)
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
    
    output$development <- render_gt({
        create_development_plan(input$pitcher)
    })
    
    output$whiffplot <- renderPlot({
        whiff_chart(input$pitcher)
    })
    
    output$breakerplot <- renderPlot({
        whiff_chart_breaker(input$pitcher)
    })
    
    output$offspeedplot <- renderPlot({
        whiff_chart_offspeed(input$pitcher)
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)

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
library(plotly)

source("source.R")

ui <- fluidPage(
        tags$head(HTML("<title>Individualized Pitcher Reports</title> <link rel='icon' type='image/gif/jpg' href='wagner.jpg'>")),
        # Application title
        titlePanel(title = div("Individualized Pitcher Reports",img(src="wagner.jpg", width = 50, height = 50))),
        
        selectInput("pitcher",
                    "Select Pitcher:",
                    sort(unique(as.character(rapsodo$Pitcher)))),
        dateInput("date", "Date Since:", value = "2019-08-01",
                  format = "yyyy-mm-dd"),
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
                     gt_output("throwingkey")),
            tabPanel("Movement Links",
                     h3(a("Pivot Pick Off",href = "https://twitter.com/The_BPCsj/status/946060384531877890", target = "_blank")),
                     h3(a("Split Stance",href = "https://twitter.com/The_BPCsj/status/1161014490432966656", target = "_blank")),
                     h3(a("Roll-In",href = "https://twitter.com/The_BPCsj/status/1161369627584290817", target = "_blank")),
                     h3(a("Step back",href = "https://twitter.com/The_BPCsj/status/1049374308680835078", target = "_blank")),
                     h3(a("Walk In Wind Up",href = "https://twitter.com/The_BPCsj/status/1198355783420973057", target = "_blank")),
                     h3(a("Stretch and Stride",href = "https://twitter.com/The_BPCsj/status/1175152661450170369", target = "_blank")))
        )
)

server <- function(input, output) {

    output$rapsodo <- render_gt({
        rapsodo_summary_table(input$pitcher, input$date)
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

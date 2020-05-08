library(shiny)
library(REdaS)
library(gt)
library(glue)
library(dplyr)

all_pitch_type <- readRDS("all_results_allpitches_allseasons.rds")

all_pitches_stats <- readRDS("all_pitches_stats.rds")

all_pitches_clean <- all_pitches_stats[complete.cases(all_pitches_stats),]

all_pitches_stats_summary <- all_pitches_clean %>% 
    group_by(pitcher, player_name, pitch_type,p_throws) %>% 
    arrange(pitcher, player_name) %>%
    summarise(Count = n(),
              Velocity = round(mean(release_speed),1),
              Move_x = round(mean(pfx_x),1),
              Move_z = round(mean(pfx_z),1),
              Effective_Speed = round(mean(effective_speed),1),
              Spin_Rate = round(mean(release_spin_rate)),
              Release_Extension = round(mean(release_extension),1))

all_pitches_stats_summary <- all_pitches_stats_summary %>%
    filter(pitch_type == "CH" |
               pitch_type == "CU" |
               pitch_type == "FC" |
               pitch_type == "FF" |
               pitch_type == "KC" |
               pitch_type == "SI" |
               pitch_type == "SL" |
               pitch_type == "FS" |
               pitch_type == "FT")

all_results_pitches_withstats <- left_join(all_pitch_type,all_pitches_stats_summary, by = c("pitcher", "player_name", "pitch_type"))
all_results_pitches_withstats$BU <- round(all_results_pitches_withstats$Spin_Rate/all_results_pitches_withstats$Velocity,1)
all_results_pitches_withstats$Move_x <- all_results_pitches_withstats$Move_x * -12
all_results_pitches_withstats$Move_z <- all_results_pitches_withstats$Move_z * 12
all_results_pitches_withstats$Move_Angle <- round(rad2deg(atan2(all_results_pitches_withstats$Move_z,all_results_pitches_withstats$Move_x)),1)
all_results_pitches_withstats$Stuff <- round(all_results_pitches_withstats$Stuff,3)


FF <- all_results_pitches_withstats %>%
    filter(pitch_type == "FF") %>%
    select(pitcher,BU,Move_Angle)

FT <- all_results_pitches_withstats %>%
    filter(pitch_type == "FT") %>%
    select(pitcher,BU,Move_Angle)

FC <- all_results_pitches_withstats %>%
    filter(pitch_type == "FC") %>%
    select(pitcher,BU,Move_Angle)

SI <- all_results_pitches_withstats %>%
    filter(pitch_type == "SI") %>%
    select(pitcher,BU,Move_Angle)

SL <- all_results_pitches_withstats %>%
    filter(pitch_type == "SL") %>%
    select(pitcher,BU,Move_Angle)

CU <- all_results_pitches_withstats %>%
    filter(pitch_type == "CU") %>%
    select(pitcher,BU,Move_Angle)

KC <- all_results_pitches_withstats %>%
    filter(pitch_type == "KC") %>%
    select(pitcher,BU,Move_Angle)

CH <- all_results_pitches_withstats %>%
    filter(pitch_type == "CH") %>%
    select(pitcher,BU,Move_Angle)

FS <- all_results_pitches_withstats %>%
    filter(pitch_type == "FS") %>%
    select(pitcher,BU,Move_Angle)

all_results_pitches_withstats <- all_results_pitches_withstats[,c("pitcher",
                                                                  "player_name",
                                                                  "p_throws",
                                                                  "pitch_type",
                                                                  "Stuff",
                                                                  "Velocity",
                                                                  "Move_x",
                                                                  "Move_z",
                                                                  "Spin_Rate",
                                                                  "BU",
                                                                  "Move_Angle")]

pitch_design <- function(velo, spinRate, hBreak,vBreak,pitchType,compNum = 5)
{
    bauerUnits <- round(spinRate/velo,2)
    moveAngle <- round(rad2deg(atan2(vBreak,hBreak)),2)
    
    if(pitchType == "FF")
    {
        comps <- FF
    }
    else if(pitchType == "FT")
    {
        comps <- FT
    }
    else if(pitchType == "FC")
    {
        comps <- FC
    }
    else if(pitchType == "SI")
    {
        comps <- SI
    }
    else if(pitchType == "SL")
    {
        comps <- SL
    }
    else if(pitchType == "CU")
    {
        comps <- CU
    }
    else if(pitchType == "KC")
    {
        comps <- FF
    }
    else if(pitchType == "SH")
    {
        comps <- CH
    }
    else if(pitchType == "FS")
    {
        comps <- FS
    }
    else
    {
        comps <= ""
    }
    
    comps$dist <- sqrt(((comps$BU - bauerUnits)^2) + (((comps$Move_Angle - moveAngle)^2)))
    comps <- filter(comps, !is.na(dist))
    comps <- comps[order(comps$dist),]
    comps <- comps[1:75,]
    
    compslist <- as.list(comps$pitcher)
    

    paired_pitches <- all_results_pitches_withstats %>%
        filter(pitcher %in% compslist) %>%
        group_by(pitch_type) %>%
        mutate(Rank = order(order(Stuff, decreasing=FALSE))) %>%
        filter(Rank <= compNum) %>%
        arrange(pitch_type,Rank)
    
    paired_pitches <- paired_pitches[,c("player_name",
                                        "pitch_type",
                                        "Stuff",
                                        "Velocity",
                                        "Move_x",
                                        "Move_z",
                                        "Spin_Rate",
                                        "BU",
                                        "Move_Angle")]
    
    paired_pitches <- paired_pitches %>%
        filter(pitch_type != pitchType)
    
    gt(paired_pitches,rowname_col = "player_name",
       groupname_col = "pitch_type")  %>%
        tab_header(
            title = "Most Effective Pitches",
            subtitle = glue("Paired With Similar {pitchType}")) %>%
        cols_label(
            Move_x = "H Break",
            Move_z = "V Break"
        ) %>%
        tab_style(style = list(
            cell_fill(color = "red"),
            cell_text(weight = "bold", color = "white")
        ),
        locations = cells_row_groups(groups = unique(paired_pitches$pitch_type)))
    
}

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Pitch Design Helper"),

    sidebarLayout(
        sidebarPanel(
            selectInput("pitchType", "Select Pitch To Build Off Of:",
                        c("FF","FT","FC","SI","SL","CU","KC","CH","FS")),
            numericInput("velo","Enter Average Pitch Velocity:",
                         value = "",
                         min = 55,
                         max = 110),
            numericInput("spinRate","Enter Average Pitch Spin Rate:",
                         value = "",
                         min = 1000,
                         max = 4000),
            numericInput("vBreak","Enter Average Pitch Vertical Break:",
                         value = "",
                         min = -30,
                         max = 30),
            numericInput("hBreak","Enter Average Pitch Horizontal Break:",
                         value = "",
                         min = -30,
                         max = 30),
            numericInput("compNum","Enter Number Of Comps Wanted:",
                         value = 5,
                         min = 1,
                         max = 20),
            
        ),

        # Show a plot of the generated distribution
        mainPanel(
            gt_output('comps')
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    output$comps <- render_gt({
        pitch_design(input$velo,
                     input$spinRate,
                     input$hBreak,
                     input$vBreak,
                     input$pitchType,
                     input$compNum)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)

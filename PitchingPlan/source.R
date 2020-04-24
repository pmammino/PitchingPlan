# install.packages("dplyr")
# install.packages("ggplot2")
# install.packages("mgcv")
# install.packages("REdaS")
# install.packages("gridExtra")
# install.packages("googlesheets4")
# install.packages("gt")
# library(tidyverse)
# library(ggplot2)
# library(mgcv)
# library(REdaS)
# library(gridExtra)
# library(googlesheets4)
# library(chron)
# library(stringr)
# library(gt)
# library(glue)
# library(plotly)

ideals <- readRDS("ideals.rds")

rapsodo <- readRDS("rapsodo.rds")

handedness <- readRDS("handedness.rds")

monthlyplan <- readRDS("monthlyplan.rds")

pre_post <- readRDS("pre_post.rds")

groups <- readRDS("groups.rds")

throwingkey <- readRDS("throwingkey.rds")

development <- readRDS("development.rds")

all_pitches <- readRDS("all_pitches.rds")

rapsodo <- data.frame(rapsodo)
rapsodo <- rapsodo %>%
  filter(TOTAL.SPIN != "-")

rapsodo$SPIN.DIR. <- chron(times = paste0(rapsodo$SPIN.DIR.,":00"))
rapsodo$SPIN.AXIS <- ifelse(rapsodo$H..BREAK > 0, 180 + (chron::hours(rapsodo$SPIN.DIR.)*30) + (chron::minutes(rapsodo$SPIN.DIR.)/2),(chron::hours(rapsodo$SPIN.DIR.)*30) - 180 + (chron::minutes(rapsodo$SPIN.DIR.)/2))

rapsodo_summary_table <- function(pitcher)
{
  rapsodo <- rapsodo %>%
  filter(Pitcher == pitcher)
  
  ideals <- ideals %>%
    filter(Pitcher == pitcher)

  pitcher_stats <- rapsodo %>% 
    select(PITCH.TYPE,VELOCITY, TOTAL.SPIN,TRUE.SPIN,SPIN.AXIS,H..BREAK, V..BREAK) %>%
    group_by(PITCH.TYPE) %>%
    summarise(Velo = mean(as.numeric(VELOCITY)), 
            Total.Spin = mean(as.numeric(TOTAL.SPIN)), 
            True.Spin = mean(as.numeric(TRUE.SPIN)),
            Spin.Axis = mean(SPIN.AXIS),
            HB = mean(as.numeric(H..BREAK)), 
            VB = mean(as.numeric(V..BREAK)))
  pitcher_stats$PITCH.TYPE <- str_trim(pitcher_stats$PITCH.TYPE)
  pitcher_stats <- pitcher_stats[order(factor(pitcher_stats$PITCH.TYPE, levels = c("FB","CH","CV","SL"))), ]
  pitcher_stats$BU <- pitcher_stats$Total.Spin/pitcher_stats$Velo  
  pitcher_stats$Move_Angle <- rad2deg(atan2(pitcher_stats$VB,pitcher_stats$HB))
  pitcher_stats$SpinEff <- (pitcher_stats$True.Spin/pitcher_stats$Total.Spin)*100
  pitcher_stats <- pitcher_stats %>% mutate_if(is.numeric, ~round(., 1))
  
  summary_stats <- pitcher_stats[,c("PITCH.TYPE",
                                    "Velo",
                                    "Spin.Axis",
                                    "Total.Spin",
                                    "SpinEff",
                                    "HB",
                                    "VB")]
  summary_stats$Type = "Current"
  
  ideals <- ideals[,c("PITCH.TYPE",
                             "Velo",
                             "Spin.Axis",
                             "Total.Spin",
                             "SpinEff",
                             "HB",
                             "VB")]
  
  ideals$Type <- "Ideal"
  
  summary_stats <- rbind(summary_stats,ideals)

  gt(summary_stats,rowname_col = "Type",
     groupname_col = "PITCH.TYPE")  %>%
    tab_header(
      title = "Rapsodo Data",
      subtitle = pitcher) %>%
    cols_label(
      PITCH.TYPE = "Pitch Type",
      Total.Spin = "Total Spin",
      Spin.Axis = "Spin Axis",
      HB = "H Break",
      VB = "V Break",
      SpinEff = "Efficiency"
    ) %>%
  tab_style(style = list(
    cell_fill(color = "darkgreen"),
    cell_text(weight = "bold", color = "white")
  ),
  locations = cells_row_groups(groups = unique(summary_stats$PITCH.TYPE)))
}


whiff_chart <- function(pitcher)
{
  # define the strike zone
  topKzone <- 3.5
  botKzone <- 1.6
  inKzone <- -0.95
  outKzone <- 0.95
  kZone <- data.frame(
    x=c(inKzone, inKzone, outKzone, outKzone, inKzone),
    y=c(botKzone, topKzone, topKzone, botKzone, botKzone)
  )
  
  pitcher_fastball <- ideals %>%
    filter(Pitcher == pitcher) %>%
    filter(PITCH.TYPE == "FB")
  
  handedness <- handedness %>%
    filter(Pitcher == pitcher)
  
  # define the 1/0 response variable
  fastballs <- all_pitches %>%
    filter(pitch_group == "Fastball")
  fastballs <- mutate(fastballs, Swing=ifelse(description %in% c("ball", "blocked_ball","bunt_foul_tip", "called_strike","foul_bunt", "hit_by_pitch", "missed_bunt", "pitchout"),
                                              0, 1))
  fastballs$release_spin_rate <- as.numeric(as.character(fastballs$release_spin_rate))
  fastballs$BU <- fastballs$release_spin_rate/fastballs$release_speed
  fastballs$Move_Angle <- rad2deg(atan2(fastballs$pfx_z,-fastballs$pfx_x))
 
  pitcher_fastball$BU <- pitcher_fastball$Total.Spin/pitcher_fastball$Velo
  pitcher_fastball$Move_Angle <- rad2deg(atan2(pitcher_fastball$VB,pitcher_fastball$HB)) 
  
  ## Devleop the Same Handed Plot
  pdata <- filter(fastballs, Swing == 1)
  pdata <- filter(pdata, between(BU,pitcher_fastball$BU-.75,pitcher_fastball$BU+0.75))
  pdata <- filter(pdata, between(Move_Angle, pitcher_fastball$Move_Angle-10,pitcher_fastball$Move_Angle+10))
  if (handedness$Handedness[1] == "R") {
    pdata <- filter(pdata, p_throws == "R")
    pdata <- filter(pdata, stand == "R")
  } else {
    pdata <- filter(pdata, p_throws == "L")
    pdata <- filter(pdata, stand == "L")
  }
  fit <- gam(whiff ~ s(plate_x,plate_z), family=binomial, data=pdata)
  
  # find predicted probabilities over a 50 x 50 grid
  plate_x <- seq(-1.25, 1.25, length.out=100)
  plate_z <- seq(1.4, 3.75, length.out=100)
  data.predict <- data.frame(plate_x = c(outer(plate_x, plate_z * 0 + 1)),
                             plate_z = c(outer(plate_x * 0 + 1, plate_z)))
  lp <- predict(fit, data.predict)
  data.predict$Probability <- exp(lp) / (1 + exp(lp))
  
  # construct the plot V Same
  Same <- ggplot(kZone, aes(x, y)) +
    theme_void() +
    geom_tile(data=data.predict, 
              aes(x=plate_x, y=plate_z, fill= Probability)) +
    scale_fill_distiller(palette = "Spectral") +
    geom_path(lwd=1.5, col="black") +
    coord_fixed()+labs(title="Whiff Rates Similar Fastballs V Same")
  
  ## Devleop the Oppo Handed Plot
  pdata <- filter(fastballs, Swing == 1)
  pdata <- filter(pdata, between(BU,pitcher_fastball$BU-.75,pitcher_fastball$BU+0.75))
  pdata <- filter(pdata, between(Move_Angle, pitcher_fastball$Move_Angle-10,pitcher_fastball$Move_Angle+10))  
  if (handedness$Handedness[1] == "R") {
    pdata <- filter(pdata, p_throws == "R")
    pdata <- filter(pdata, stand == "L" | stand == "S")
  } else {
    pdata <- filter(pdata, p_throws == "L")
    pdata <- filter(pdata, stand == "R" | stand == "S")
  }
  fit <- gam(whiff ~ s(plate_x,plate_z), family=binomial, data=pdata)
  
  # find predicted probabilities over a 50 x 50 grid
  plate_x <- seq(-1.25, 1.25, length.out=100)
  plate_z <- seq(1.4, 3.75, length.out=100)
  data.predict <- data.frame(plate_x = c(outer(plate_x, plate_z * 0 + 1)),
                             plate_z = c(outer(plate_x * 0 + 1, plate_z)))
  lp <- predict(fit, data.predict)
  data.predict$Probability <- exp(lp) / (1 + exp(lp))
  
  # construct the plot V Oppo
  Oppo <- ggplot(kZone, aes(x, y)) +
    theme_void() +
    geom_tile(data=data.predict, 
              aes(x=plate_x, y=plate_z, fill= Probability)) +
    scale_fill_distiller(palette = "Spectral") +
    geom_path(lwd=1.5, col="black") +
    coord_fixed()+labs(title="Whiff Rates Similar Fastballs V Oppo")
  
  ## Generate Plots
  grid.arrange(Same, Oppo, ncol=2)
}

generate_monthly_plan <- function(pitcher)
{
  month <- months(Sys.Date())
  monthlyplan <- monthlyplan %>%
    filter(Pitcher == pitcher) %>%
    filter(Month == month)
  
  monthlyplan <- monthlyplan[,c("Week",
                                "Monday",
                                "Tuesday",
                                "Wednesday",
                                "Thursday",
                                "Friday",
                                "Saturday",
                                "Sunday")]
  
  gt(monthlyplan)  %>%
    tab_header(
      title = "Pitching Plan",
      subtitle = glue('Month Of {month}'))
}


movement_trend_graphs <- function(pitcher,pitch_type)
{
  movements <- rapsodo %>%
    filter(Pitcher == pitcher) %>%
    filter(PITCH.TYPE == pitch_type) %>%
    select(DATE,H..BREAK,V..BREAK) %>%
    group_by(DATE) %>%
    summarise(HB = round(mean(as.numeric(H..BREAK)),1),
              VB = round(mean(as.numeric(V..BREAK)),1))
  
  movements <- movements %>%
    gather(key = "variable", value = "value", -DATE)
  
  pitch_plot <- ggplot(movements, aes(x = DATE, y = value)) + 
    geom_line(aes(color = variable), lwd=1.5) + 
    geom_point(aes(color = variable), size = 4) +
    scale_color_manual(values = c("darkgreen", "black")) +
    theme_bw() +
    labs(x = "Date",
         y = "Movement",
         title = glue('Movement Over Time For {pitch_type}'),
         subtitle = glue('{pitcher}'),
         col = NULL) +
    theme(axis.title = element_text(size = 12),
          axis.text = element_text(size = 10),
          plot.title = element_text(size = 16),
          plot.subtitle = element_text(size = 14),
          plot.caption = element_text(size = 10))
  pitch_plot
  
  
  ggplotly(pitch_plot) %>%
    layout(title = list(text = paste0(glue('Movement Over Time For {pitch_type}'),
                                      '<br>',
                                      '<sup>',
                                      glue('{pitcher}'),
                                      '</sup>')), hovermode = 'compare')
}

pre_post_throwing_plan <- function(pitcher)
{
  pre_post <- data.frame(pre_post)
  
  groups <- groups %>%
    filter(Pitcher == pitcher)
  
  group <- groups$Group[1]
  
  pre_post <- pre_post %>%
    filter(Group == group)
  
  pre_post <- pre_post[,c("Type", "Movement", "Reps")]
  
  gt(pre_post,
     rowname_col = "Movement",
     groupname_col = "Type")  %>%
    tab_header(
      title = "Pre and Post Throwing Routine") %>%
    tab_options(heading.title.font.weight = "bold",) %>%
    cols_label(
      Reps = "") %>%
    tab_style(style = list(
      cell_fill(color = "darkgreen"),
      cell_text(weight = "bold", color = "white")
   ),
  locations = cells_row_groups(groups = unique(pre_post$Type)))

}

throwingkey <- data.frame(throwingkey)

throwing_key <- gt(throwingkey,
   rowname_col = "Drill",
   groupname_col = "Day")  %>%
  tab_header(
    title = "Throwing Key") %>%
  tab_options(heading.title.font.weight = "bold",) %>%
  tab_style(style = list(
    cell_fill(color = "darkgreen"),
    cell_text(weight = "bold", color = "white")
  ),locations = cells_row_groups(groups = unique(throwingkey$Day)))


create_development_plan <- function(pitcher)
{
  development <- data.frame(development)
  
  development_plan <- development %>%
    filter(Pitcher == pitcher)
  
  development_plan <- development_plan[,c("Section",'Notes')]
  
  development_plan <- gt(development_plan,
                     groupname_col = "Section")  %>%
    tab_header(
      title = "Development Plan",
      subtitle = glue('{pitcher}')) %>%
    tab_options(heading.title.font.weight = "bold",) %>%
  cols_label(
    Notes = "") %>%
    tab_style(style = list(
      cell_fill(color = "darkgreen"),
      cell_text(weight = "bold", color = "white")
    ),locations = cells_row_groups(groups = unique(development_plan$Section)))
  development_plan
}

whiff_chart_breaker <- function(pitcher)
{
  # define the strike zone
  topKzone <- 3.5
  botKzone <- 1.6
  inKzone <- -0.95
  outKzone <- 0.95
  kZone <- data.frame(
    x=c(inKzone, inKzone, outKzone, outKzone, inKzone),
    y=c(botKzone, topKzone, topKzone, botKzone, botKzone)
  )
  
  pitcher_breaker <- ideals %>%
    filter(Pitcher == pitcher) %>%
    filter(PITCH.TYPE == "CV")
  
  handedness <- handedness %>%
    filter(Pitcher == pitcher)
  
  # define the 1/0 response variable
  breakers <- all_pitches %>%
    filter(pitch_group == "Breaking")
  breakers <- mutate(breakers, Swing=ifelse(description %in% c("ball", "blocked_ball","bunt_foul_tip", "called_strike","foul_bunt", "hit_by_pitch", "missed_bunt", "pitchout"),
                                              0, 1))
  breakers$release_spin_rate <- as.numeric(as.character(breakers$release_spin_rate))
  breakers$BU <- breakers$release_spin_rate/breakers$release_speed
  breakers$Move_Angle <- rad2deg(atan2(breakers$pfx_z,-breakers$pfx_x))
  
  pitcher_breaker$BU <- pitcher_breaker$Total.Spin/pitcher_breaker$Velo
  pitcher_breaker$Move_Angle <- rad2deg(atan2(pitcher_breaker$VB,pitcher_breaker$HB)) 
  
  ## Devleop the Same Handed Plot
  pdata <- filter(breakers, Swing == 1)
  pdata <- filter(pdata, between(BU,pitcher_breaker$BU-1,pitcher_breaker$BU+1))
  pdata <- filter(pdata, between(Move_Angle, pitcher_breaker$Move_Angle-10,pitcher_breaker$Move_Angle+10))
  if (handedness$Handedness[1] == "R") {
    pdata <- filter(pdata, p_throws == "R")
    pdata <- filter(pdata, stand == "R")
  } else {
    pdata <- filter(pdata, p_throws == "L")
    pdata <- filter(pdata, stand == "L")
  }
  fit <- gam(whiff ~ s(plate_x,plate_z), family=binomial, data=pdata)
  
  # find predicted probabilities over a 50 x 50 grid
  plate_x <- seq(-1.25, 1.25, length.out=100)
  plate_z <- seq(1.4, 3.75, length.out=100)
  data.predict <- data.frame(plate_x = c(outer(plate_x, plate_z * 0 + 1)),
                             plate_z = c(outer(plate_x * 0 + 1, plate_z)))
  lp <- predict(fit, data.predict)
  data.predict$Probability <- exp(lp) / (1 + exp(lp))
  
  # construct the plot V Same
  Same <- ggplot(kZone, aes(x, y)) + 
    theme_void() +
    geom_tile(data=data.predict, 
              aes(x=plate_x, y=plate_z, fill= Probability)) +
    scale_fill_distiller(palette = "Spectral") +
    geom_path(lwd=1.5, col="black") +
    coord_fixed()+labs(title="Whiff Rates Similar Breakers V Same")
  
  ## Devleop the Oppo Handed Plot
  pdata <- filter(breakers, Swing == 1)
  pdata <- filter(pdata, between(BU,pitcher_breaker$BU-1,pitcher_breaker$BU+1))
  pdata <- filter(pdata, between(Move_Angle, pitcher_breaker$Move_Angle-10,pitcher_breaker$Move_Angle+10))  
  if (handedness$Handedness[1] == "R") {
    pdata <- filter(pdata, p_throws == "R")
    pdata <- filter(pdata, stand == "L" | stand == "S")
  } else {
    pdata <- filter(pdata, p_throws == "L")
    pdata <- filter(pdata, stand == "R" | stand == "S")
  }
  fit <- gam(whiff ~ s(plate_x,plate_z), family=binomial, data=pdata)
  
  # find predicted probabilities over a 50 x 50 grid
  plate_x <- seq(-1.25, 1.25, length.out=100)
  plate_z <- seq(1.4, 3.75, length.out=100)
  data.predict <- data.frame(plate_x = c(outer(plate_x, plate_z * 0 + 1)),
                             plate_z = c(outer(plate_x * 0 + 1, plate_z)))
  lp <- predict(fit, data.predict)
  data.predict$Probability <- exp(lp) / (1 + exp(lp))
  
  # construct the plot V Oppo
  Oppo <- ggplot(kZone, aes(x, y)) +
    theme_void() +
    geom_tile(data=data.predict, 
              aes(x=plate_x, y=plate_z, fill= Probability)) +
    scale_fill_distiller(palette = "Spectral") +
    geom_path(lwd=1.5, col="black") +
    coord_fixed()+labs(title="Whiff Rates Similar Breakers V Oppo")
  
  ## Generate Plots
  grid.arrange(Same, Oppo, ncol=2)
}

whiff_chart_offspeed <- function(pitcher)
{
  # define the strike zone
  topKzone <- 3.5
  botKzone <- 1.6
  inKzone <- -0.95
  outKzone <- 0.95
  kZone <- data.frame(
    x=c(inKzone, inKzone, outKzone, outKzone, inKzone),
    y=c(botKzone, topKzone, topKzone, botKzone, botKzone)
  )
  
  pitcher_offspeed <- ideals %>%
    filter(Pitcher == pitcher) %>%
    filter(PITCH.TYPE == "CH")
  
  handedness <- handedness %>%
    filter(Pitcher == pitcher)
  
  # define the 1/0 response variable
  offspeeds <- all_pitches %>%
    filter(pitch_group == "Breaking")
  offspeeds <- mutate(offspeeds, Swing=ifelse(description %in% c("ball", "blocked_ball","bunt_foul_tip", "called_strike","foul_bunt", "hit_by_pitch", "missed_bunt", "pitchout"),
                                            0, 1))
  offspeeds$release_spin_rate <- as.numeric(as.character(offspeeds$release_spin_rate))
  offspeeds$BU <- offspeeds$release_spin_rate/offspeeds$release_speed
  offspeeds$Move_Angle <- rad2deg(atan2(offspeeds$pfx_z,-offspeeds$pfx_x))
  
  pitcher_offspeed$BU <- pitcher_offspeed$Total.Spin/pitcher_offspeed$Velo
  pitcher_offspeed$Move_Angle <- rad2deg(atan2(pitcher_offspeed$VB,pitcher_offspeed$HB)) 
  
  ## Devleop the Same Handed Plot
  pdata <- filter(offspeeds, Swing == 1)
  pdata <- filter(pdata, between(BU,pitcher_offspeed$BU-0.75,pitcher_offspeed$BU+0.75))
  pdata <- filter(pdata, between(Move_Angle, pitcher_offspeed$Move_Angle-10,pitcher_offspeed$Move_Angle+10))
  if (handedness$Handedness[1] == "R") {
    pdata <- filter(pdata, p_throws == "R")
    pdata <- filter(pdata, stand == "R")
  } else {
    pdata <- filter(pdata, p_throws == "L")
    pdata <- filter(pdata, stand == "L")
  }
  fit <- gam(whiff ~ s(plate_x,plate_z), family=binomial, data=pdata)
  
  # find predicted probabilities over a 50 x 50 grid
  plate_x <- seq(-1.25, 1.25, length.out=100)
  plate_z <- seq(1.4, 3.75, length.out=100)
  data.predict <- data.frame(plate_x = c(outer(plate_x, plate_z * 0 + 1)),
                             plate_z = c(outer(plate_x * 0 + 1, plate_z)))
  lp <- predict(fit, data.predict)
  data.predict$Probability <- exp(lp) / (1 + exp(lp))
  
  # construct the plot V Same
  Same <- ggplot(kZone, aes(x, y)) +
    theme_void() +
    geom_tile(data=data.predict, 
              aes(x=plate_x, y=plate_z, fill= Probability)) +
    scale_fill_distiller(palette = "Spectral") +
    geom_path(lwd=1.5, col="black") +
    coord_fixed()+labs(title="Whiff Rates Similar Offspeeds V Same")
  
  ## Devleop the Oppo Handed Plot
  pdata <- filter(offspeeds, Swing == 1)
  pdata <- filter(pdata, between(BU,pitcher_offspeed$BU-0.75,pitcher_offspeed$BU+0.75))
  pdata <- filter(pdata, between(Move_Angle, pitcher_offspeed$Move_Angle-10,pitcher_offspeed$Move_Angle+10))  
  if (handedness$Handedness[1] == "R") {
    pdata <- filter(pdata, p_throws == "R")
    pdata <- filter(pdata, stand == "L" | stand == "S")
  } else {
    pdata <- filter(pdata, p_throws == "L")
    pdata <- filter(pdata, stand == "R" | stand == "S")
  }
  fit <- gam(whiff ~ s(plate_x,plate_z), family=binomial, data=pdata)
  
  # find predicted probabilities over a 50 x 50 grid
  plate_x <- seq(-1.25, 1.25, length.out=100)
  plate_z <- seq(1.4, 3.75, length.out=100)
  data.predict <- data.frame(plate_x = c(outer(plate_x, plate_z * 0 + 1)),
                             plate_z = c(outer(plate_x * 0 + 1, plate_z)))
  lp <- predict(fit, data.predict)
  data.predict$Probability <- exp(lp) / (1 + exp(lp))
  
  # construct the plot V Oppo
  Oppo <- ggplot(kZone, aes(x, y)) +
    theme_void() +
    geom_tile(data=data.predict, 
              aes(x=plate_x, y=plate_z, fill= Probability)) +
    scale_fill_distiller(palette = "Spectral") +
    geom_path(lwd=1.5, col="black") +
    coord_fixed()+labs(title="Whiff Rates Similar Offspeeds V Oppo")
  
  ## Generate Plots
  grid.arrange(Same, Oppo, ncol=2)
}

whiff_chart_offspeed("Kevin Wiseman")

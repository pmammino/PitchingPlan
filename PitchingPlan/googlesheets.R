#!/usr/bin/env Rscript
library(googlesheets4)
library(mailR)
library(lubridate)
library(glue)
library(tidyverse)
library(htmlTable)



setwd("~/GitHub/PitchingPlan/PitchingPlan")

sheets_auth(
  email = "pmammino18@gmail.com")

ideals <- read_sheet("https://docs.google.com/spreadsheets/d/1OKspMLQyR0LdI889pzpjoHqgpJDIiNlR9eTf9XNxpUo/edit#gid=0")

Sys.sleep(5)

rapsodo <- read_sheet("https://docs.google.com/spreadsheets/d/1IJwrKgo2N8SSWmOEHmaNICxG6Nx2RPQppaI2ozz7LhI/edit#gid=0")

Sys.sleep(5)

handedness <- read_sheet("https://docs.google.com/spreadsheets/d/1vttEPw3x9vLs7XY1c7YEW-VIc9uM1l4neDHY7icELVk/edit#gid=0")

Sys.sleep(5)

monthlyplan <- read_sheet("https://docs.google.com/spreadsheets/d/1ovHuTCKfAjPnNYhAA6anlm_t0vM9hShvoX5Zd_IBGho/edit#gid=0")
monthlyplan <- data.frame(monthlyplan)

Sys.sleep(5)

pre_post <- read_sheet("https://docs.google.com/spreadsheets/d/1ftejSW4anTwFeulb66EJLOuNldjLouU5twJ9eMShu6s/edit#gid=0")

Sys.sleep(5)

groups<- read_sheet("https://docs.google.com/spreadsheets/d/1lKOTTEDnQ7zLLlOZ62R5vAOmkgBxw-OYbm_J7r30spo/edit#gid=0")

Sys.sleep(5)

throwingkey <- read_sheet("https://docs.google.com/spreadsheets/d/1bCV7o-CDOEQYgCE7ditbymWbL-JrItV60vrikHy1-D4/edit#gid=0")

Sys.sleep(5)

development <- read_sheet("https://docs.google.com/spreadsheets/d/1ME7B8PnkDzXQRl9DvpVGnMHcHQWYPVeNCp6AIzCHoQA/edit#gid=0")

Sys.sleep(5)

saveRDS(ideals,"ideals.rds")

saveRDS(rapsodo,"rapsodo.rds")

saveRDS(handedness,"handedness.rds")

saveRDS(monthlyplan,"monthlyplan.rds")

saveRDS(pre_post,"pre_post.rds")

saveRDS(groups,"groups.rds")

saveRDS(throwingkey, "throwingkey.rds")

saveRDS(development,"development.rds")

pitchers <- as.list(unique(rapsodo$Pitcher))

daily_email <- function(pitcher)
{
  
  monthlyplan$Start <- as.numeric(gsub("-.*","", monthlyplan$Week))
  monthlyplan$End <- as.numeric(gsub(".*-","", monthlyplan$Week))
  
  workout <- monthlyplan %>%
    filter(Pitcher == pitcher) %>%
    filter(Start <= day(Sys.Date())) %>%
    filter(End >= day(Sys.Date()))
  
  workout <- workout[,weekdays(Sys.Date())][1]
  
  if (workout == "Light Toss/OFF")
  {
    table <- throwingkey %>%
    filter(Day == "Light Toss") %>%
    select(Drill,Ball,Sets,Reps,Intensity)
  }
  else
  {
    table <- throwingkey %>%
      filter(Day == workout) %>%
      select(Drill,Ball,Sets,Reps,Intensity)
  }
  
  if(nrow(table) > 0)
  {
  table <- htmlTable(table,rnames = FALSE)
  
  html_body <- paste0(glue("<p>Good Morning {pitcher},</p>"),
                      glue("<p>The Table below will show you your throwing program for today: {workout}</p>"),
                      table,
                      "<p>If you need a reminder of the keys for you to focus on consult the link below</p>",
                      "<p><a href='https://pmammino18.shinyapps.io/pitchingplan/'>Link To App</a></p>")
  }
  else
  {
    html_body <- paste0(glue("<p>Good Morning {pitcher},</p>"),
                        glue("<p>Today your throwing program is {workout}:</p>"),
                        "<p>If you need a reminder of the keys for you to focus on consult the link below</p>",
                        "<p><a href='https://pmammino18.shinyapps.io/pitchingplan/'>Link To App</a></p>")
  }
  
  send.mail(from = "pmammino18@gmail.com",
            to = c("pmammino18@gmail.com","Michael.adams.bpc@gmail.com"),
            subject = paste0(glue("Daily Throwing For {pitcher} "),Sys.Date()),
            body = html_body,
            html = TRUE,
            smtp = list(host.name = "smtp.gmail.com", port = 465, 
                        user.name = "pmammino18@gmail.com",            
                        passwd = "Wagner018", 
                        ssl = TRUE), 
            authenticate = TRUE,
            send = TRUE) 
}

lapply(pitchers, daily_email)

rsconnect::deployApp(forceUpdate = TRUE)

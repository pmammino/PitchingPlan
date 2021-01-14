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


rsconnect::deployApp(forceUpdate = TRUE)

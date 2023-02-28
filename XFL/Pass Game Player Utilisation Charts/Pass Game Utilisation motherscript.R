# Loading relevant libraries ====
library(googlesheets4)
library(googledrive)
library(tidyverse)
library(janitor)
library(ggimage)
library(ggtext)
library(shadowtext)
library(scales)
library(statebins)
library(gt)
library(gtExtras)
library(shadowtext)
library(fontawesome)
library(showtext)
library(sysfonts)
font_add_google('Chivo', 'Chivo')
font_add('fs', 'fonts/Font Awesome 6 Free-Solid-900.otf')
font_add('fb', 'fonts/Font Awesome 6 Brands-Regular-400.otf')
showtext_auto()
#adjust dpi in showtext -- fix issues with saving (showtext + ggtext problem)
showtext::showtext_opts(dpi = 300)
'%ni%' <- Negate("%in%")
source('XFL dataviz themes.R')
source('Final ETR dataviz themes.R')

# Setting week ====
week <- 2

# Setting main colours for plots ====
team_color_1 <- "#37af4a"
team_color_2 <- "#f88b01"

# Importing raw data ====
all_teams_wr_te_data  <- read_sheet("https://docs.google.com/spreadsheets/d/1dkHd3NN2gU982Oe4SeKcyWrFKO73lDxKIJJzyyB_jBA/edit#gid=1737114943",
                                    sheet = "WRs / TEs (Weekly)") %>% 
  clean_names()

# Light mode chart scripts
source('Team pass utilisation rate light mode chart scripts/Arlington Renegades Pass Game Facet.R')
source('Team pass utilisation rate light mode chart scripts/DC Defenders Pass Game Facet.R')
source('Team pass utilisation rate light mode chart scripts/Houston Roughnecks Pass Game Facet.R')
source('Team pass utilisation rate light mode chart scripts/Las Vegas Vipers Pass Game Facet.R')
source('Team pass utilisation rate light mode chart scripts/Orlando Guardians Pass Game Facet.R')
source('Team pass utilisation rate light mode chart scripts/San Antonio Brahmas Pass Game Facet.R')
source('Team pass utilisation rate light mode chart scripts/Seattle Sea Dragons Pass Game Facet.R')
source('Team pass utilisation rate light mode chart scripts/St Louis Battlehawks Pass Game Facet.R')

# Dark mode chart scripts ====
source('Team pass utilisation rate dark mode chart scripts/Arlington Renegades Dark Mode Pass Game Facet.R')
source('Team pass utilisation rate dark mode chart scripts/DC Defenders Dark Mode Pass Game Facet.R')
source('Team pass utilisation rate dark mode chart scripts/Houston Roughnecks Dark Mode Pass Game Facet.R')
source('Team pass utilisation rate dark mode chart scripts/Las Vegas Vipers Dark Mode Pass Game Facet.R')
source('Team pass utilisation rate dark mode chart scripts/Orlando Guardians Dark Mode Pass Game Facet.R')
source('Team pass utilisation rate dark mode chart scripts/San Antonio Brahmas Dark Mode Pass Game Facet.R')
source('Team pass utilisation rate dark mode chart scripts/Seattle Sea Dragons Dark Mode Pass Game Facet.R')
source('Team pass utilisation rate dark mode chart scripts/St Louis Battlehawks Dark Mode Pass Game Facet.R')


# welcome to my prep.R :-)
# --------------------------------------------------

library(readxl)
library(ggplot2)
library(janitor)
library(shiny)
library(styler)
library(ggrepel)
library(gapminder)
library(lubridate)
library(readxl)
library(ggplot2)
library(fs)
library(sf)
library(tidyr)
library(transformr)
library(dplyr)
library(janitor)
library(gifski)
library(gganimate)
library(ggrepel)
library(shinythemes)
library(mxmaps)
library(gapminder)
library(lubridate)
library(rsconnect)
library(tmap)
library(tmaptools)
library(rgdal)
library(mxmaps)
library(Hmisc)
library(data.table)
library(leaflet)
library(mapview)
library(plotly)
library(tidyverse)

# --------------------------------------------------

# installing a necessary package of maps of Mexico below -- 
# unsure if I'm doing this right 

if (!require(devtools)) {
  install.packages("devtools")
}
devtools::install_github('diegovalle/mxmortalitydb')
install.packages("mxmaps")

# --------------------------------------------------

# creating directory for all the data -- 

dir_create("raw-data")

# --------------------------------------------------
# DATA PREP -- downloading all data, in all its messy glory, below...

###### VIOLENCE DATA

    # [I have a spreadsheet I have been using throughout my project, but I'm
    # currently exploring other options because some of the data on it seems 
    # to just be wrong...which is strange b/c it's from the Mexican government...
    # to clarify I am doing this literally today]

    # but here is the old data, with necessary modifiers that make it
    # easier to read


######################
## VIOLENCE DATA:#####
######################


crimes <- read_xlsx("./raw-data/nm-estatal-victimas.xlsx") %>% 
  clean_names()

  # only intentional homicides
map_cartel_data_1 <- crimes %>%
  filter(subtipo == "HOMICIDIO DOLOSO") %>%
    group_by(state, date) %>%
    summarise(total = sum(count, na.rm = TRUE)) %>%
    arrange(state, date)

# make a table first (pivot wider?)
# lubridate cheat sheet group 
# mutate group by year


perception <- read_xlsx("./raw-data/INEGI perception of insecurity.xlsx")
# this data is from 2011 to 2019 in each state and in country as a whole

######################
###### BP DATA #######
######################

bp <- read_csv("./raw-data/southwest_border_apprehensions_1960_2018.csv") %>%
  # this data is from 1960 to 2018 for each different southwest border region
  # of which there are 10 per year, and in the southwest border as a whole
  # which is on every 10th row. It makes sense to me to only use the total per year
  # for the southwest border as a whole.
  clean_names()

bp_fc <- read_csv("./raw-data/family_child_total_monthly_2000_2018.csv") %>%
  # this data is specifically for families and 
  # unaccompanied children 
  clean_names()


# --------------------------------------------------


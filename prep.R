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
library(Hmisc)
library(data.table)
library(leaflet)
library(mapview)
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

# creating directories for all the data -- 

dir_create("raw-data")

dir_create("clean-data")

# --------------------------------------------------
# DATA PREP -- downloading all data, in all its messy glory, below...

# actual violent crime data:

    # [I have a spreadsheet I have been using throughout my project, but I'm
    # currently exploring other options because some of the data on it seems 
    # to just be wrong...which is strange b/c it's from the Mexican government...
    # to clarify I am doing this literally today]

    # but here is the old data, with necessary modifiers that make it
    # easier to read

# the following i'm keeping around temporarily until i figure thigns out
crimes <- read_xlsx("./raw-data/nm-estatal-victimas.xlsx") %>% 
  clean_names()

# THIS is the data i'm actually using 

download.file("http://crimenmexico.diegovalle.net/en/csv/fuero-comun-estados.csv.gz", 
              destfile = "./raw-data/fuero-comun-estados.csv.gz")


crime <- read.csv("./raw-data/fuero-comun-estados.csv.gz")

# only intentional homicides
crime <- subset(crime, crime == "HOMICIDIOS" & type == "DOLOSOS")

# Sum homicides by firearm, etc and group by state and date
hom <- crime %.%
  filter(year %in% 1997:2013) %.%
  group_by(state_code, year, type) %.%
  summarise(total = sum(count, na.rm = TRUE),
            population = mean(population) ) %.%
  mutate(rate = total / population * 10^5) %.%
  arrange(state_code, year)


# Needed for read.dbf
library("foreign")

## The dbf from the state shapefile needed to merge state_code with state
## names
codes <- read.dbf("states.dbf")
codes$NOM_ENT <- iconv(codes$NOM_ENT, "windows-1252", "utf-8")
codes$CVE_ENT <- as.numeric(codes$CVE_ENT)
codes$OID <- NULL
names(codes) <- c("state_code", "name")

## Load plyr for join(). Loading it before creates a problem with the dplyr
## call above
library("plyr")

## Names needed for the map
hom <- join(hom, codes)


# Make the map
library("rMaps")
d1 <- ichoropleth(rate ~ name, data = hom, ncuts = 9, pal = 'YlOrRd', 
                  animate = 'year',  map = 'states'
)

# then this thing 

d1$set(
  geographyConfig = list(
    dataUrl = "https://dl.dropboxusercontent.com/u/10794332/mx_states.json"
  ),
  scope = 'states',
  setProjection = '#! function( element, options ) {
   var projection, path;
   projection = d3.geo.mercator()
    .center([-89, 21]).scale(element.offsetWidth)
    .translate([element.offsetWidth / 2, element.offsetHeight / 2]);

   path = d3.geo.path().projection( projection );
   return {path: path, projection: projection};
  } !#'
)
d1$save('rMaps.html', cdn = TRUE)

# I AM CONFUSED w my own map :( 


shape <- st_read("./raw-data/GIS Mexican States/Mexican States.shp", quiet=TRUE)

# locations <- st_as_sf(homicides_rds, coords = c("lng", "lat"), crs = 4326)

# sending the above to clean-data...

write_rds(shape, path="./clean-data/mx_shape.rds")

# write_rds(locations, path = "./clean-data/mx_homicides.rds")

# perception data:

perception <- read_xlsx("./raw-data/INEGI perception of insecurity.xlsx")
  # this data is from 2011 to 2019 in each state and in country as a whole

# BP data:

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
# MAP PREP -- prepping below ...

df_mxstate$value <-  df_mxstate$indigenous / df_mxstate$pop * 100
mxstate_choropleth(df_mxstate, 
                   num_colors = 1,
                   title = "not finished",
                   legend = "%")

# --------------------------------------------------
# making the graphics' home, a new directory, below...

dir_create("graphics")

# ggplots derived from the crimes data




      # date labels fixing

labels = c("2015-01", "2016-01", "2017-01", "2018-01", "2019-01")

z + scale_x_continuous(name="Year", limits=c(2015-01, 2019-01)) + scale_y_continuous(name="Number of Crimes", limits=0, 6000)


limits=c(2015-01, 2019-01)


axis_categories <- c(2015-01, 2016-01, 2017-01, 2018-01, 2019-01) %>%
  set_names(c("2015-01", "2016-01", "2017-01", "2018-01", "2019-01"))

dates <- dates[1:90, ]

# labels and breaks for X axis text
brks <- dates$date[seq(1, length(dates$date), 12)]
lbls <- lubridate::year(brks)

a <- crimes %>%
  select(state, tipo, date) %>%
  filter(tipo == "HOMICIDIO") %>%
  summarize(count1 = nrow(crimes)) %>%
  ggplot(aes(x = state, y = count1)) +
  labs(title = "Intentional Homicides per State over the last 20 years", 
       subtitle = "Homicides have risen dramatically over time in almost every state",
       caption = "Source: INEGI") +
  geom_point() +
  scale_x_log10(labels = c("2015-01", "2016-01", "2017-01", "2018-01", "2019-01")) +
  scale_x_date(labels = lbls, 
               breaks = brks) 

a



# ------------------------------

plot1_bp <- bp_fc %>%
  filter(month == "yearly total") %>%
# getting only totals per year, for all sectors added together
  filter(sector == "southwest border") %>%
  select(fiscal_year, total_apprehensions) %>%
  ggplot(aes(x=fiscal_year, y=total_apprehensions)) + 
  geom_step() + 
  labs(title="Border Patrol Apprehensions on Southwest Border", 
       subtitle="Per Year, Total Apprehensions", 
       caption="source: US Customs and Border Patrol public data") 

plot1_bp

# this second border-patrol-related plot is only children

plot2_bp <- bp_fc %>%
  # getting only totals per year, for all sectors added together
  filter(sector == "southwest border") %>%
  select(fiscal_year, unaccompanied_child_apprehension) %>%
  ggplot(aes(x=fiscal_year, y=unaccompanied_child_apprehension)) + 
  geom_step() + 
  labs(title="Border Patrol Apprehensions on Southwest Border of Unaccompanied Children", 
       subtitle="Per Year", 
       caption="source: US Customs and Border Patrol public data") 
  
plot2_bp
  
# this third border-patrol-related plot is only families 
  
plot3_bp <- bp_fc %>%
  # getting only totals per year, for all sectors added together
  filter(sector == "southwest border") %>%
  select(fiscal_year, family_apprehensions) %>%
  ggplot(aes(x=fiscal_year, y=family_apprehensions)) + 
  geom_step() + 
  labs(title="Border Patrol Apprehensions on Southwest Border of Families", 
       subtitle="Per Year", 
       caption="source: US Customs and Border Patrol public data") 

plot3_bp



write_rds(nameofplot, "location.rds")
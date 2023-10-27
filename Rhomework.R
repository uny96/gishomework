install.packages(c("sf","tmap","tmaptools","RSQLite","tidyverse"),
                 repos = "https://www.stats.bric.ac.uk/R/")
library(sf)
shape <- st_read("E:/USS/005/week1/homework(download)/statsnz-territorial-authority-2018-generalised-SHP/territorial-authority-2018-generalised.shp")
summary(shape)
library(sf)
shape%>%
  st_geometry()%>%
  plot()
library(tidyverse)
mycsv <- read_csv("E:/USS/005/week1/homework(download)/Statistical Area 1 dataset for Census 2018-total-New Zealand_updated_4-11-21/practice.csv")
mycsv
shape <- shape%>%
  merge(.,
        mycsv,
        by.x="TA2018_V1_",
        by.y="Territorial authority code (2018 areas)")
shape%>%
  head(.,n=10)
library(tmap)
tmap_mode("plot")
shape%>%
  qtm(.,fill = "Paid employee")
shape%>%
  st_write(.,"E:/USS/005/week1/homework(download)/practiceR.gpkg",
           "Paid employee",
           delete_layer = TRUE)
library(readr)
library(RSQLite)

con <- dbConnect(RSQLite::SQLite(),dbname="E:/USS/005/week1/homework(download)/practiceR.gpkg")
con %>%
  dbListTables()
con %>%
  dbWriteTable(.,
               "original_csv",
               mycsv,
               overwrite=TRUE)

con %>% 
  dbDisconnect()
library(sf)
library(tidyverse)
library(janitor)
library(here)
install.packages('countrycode')
library(countrycode)

gender_inequality <- read_csv(here("data","HDR21-22_Composite_indices_complete_time_series.csv"),locale = locale(encoding = "latin1"),
                              na = " ", skip=0)
world_countries <- st_read(here("data","World_Countries_Generalized","World_Countries_Generalized.shp"))

gender_inequality1 <- gender_inequality%>%
  clean_names()%>%
  select(iso3,country,gii_2010,gii_2019)%>%
  mutate(difference=gii_2019-gii_2010)%>%
  mutate(iso_code=countrycode(iso3, origin ='iso3c', destination = 'iso2c'))

join <- world_countries%>%
  clean_names()%>%
  left_join(.,
          gender_inequality1,
          by=c("iso"="iso_code"))

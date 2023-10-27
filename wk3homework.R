library(sf)
library(tidyverse)
library(terra)
library(ggplot2)
library(janitor)
library(here)

spain <- sf::st_read(here("data","gadm41_ESP.gpkg"),layer = "ADM_ADM_0")
worldcities <- sf::st_read(here("data","World_cities","World_Cities.shp"))
ssp1 <- terra::rast(here("data","wc2.1_2.5m_tmax_ACCESS-CM2_ssp126_2081-2100.tif"))
ssp5 <- terra::rast(here("data","wc2.1_2.5m_tmax_ACCESS-CM2_ssp585_2081-2100.tif"))

spainishcities <- worldcities%>%
  clean_names()%>%
  filter(cntry_name=="Spain")

ssp1crop <- ssp1%>%
  terra::crop(.,spain)
ssp1mask <- ssp1crop%>%
  terra::mask(.,spain)

ssp5crop <- ssp5%>%
  terra::crop(.,spain)
ssp5mask <- ssp5crop%>%
  terra::mask(.,spain)

diff_spain_climate <-ssp5mask-ssp1mask

month <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", 
           "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
names(diff_spain_climate) <- month
spain_city_diff<- terra::extract(diff_spain_climate, spainishcities)

spainishcities_joinid <- spainishcities%>%
  dplyr::mutate(join_id=1:n())

spain_city_diff_cityname <- spain_city_diff%>%
  left_join(.,
            spainishcities_joinid,
            by=c("ID"="join_id"))

?st_drop_geometry



city_climate_diff <- spain_city_diff_cityname %>%
  dplyr::select(c(,2:13))%>%
  sf::st_drop_geometry(.)%>%
  dplyr::as_tibble()

tidy_city_diff <- city_climate_diff %>%
  tidyr::pivot_longer(everything(), 
                      names_to="Months", 
                      values_to="temp_diff")

facet_plot <- tidy_city_diff %>%
  dplyr::mutate(Months = factor(Months, levels = c("Jan","Feb","Mar",
                                                   "Apr","May","Jun",
                                                   "Jul","Aug","Sep",
                                                   "Oct","Nov","Dec")))
# Plot faceted histogram
plot<-ggplot(facet_plot, aes(x=temp_diff, na.rm=TRUE))+
  geom_histogram(color="black", binwidth = .1)+
  labs(title="Ggplot2 faceted difference in climate scenarios of max temp", 
       x="Temperature",
       y="Frequency")+
  facet_grid(Months ~ .)+
  theme(plot.title = element_text(hjust = 0.5))

plot
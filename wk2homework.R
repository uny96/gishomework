
library(sf)
library(tidyverse)
library(tmap)
library(janitor)
library(here)

WashingtonData <- read_csv(here::here("data","Report_Card_Assessment_Data_2018-19_School_Year_20231013.csv"),
                          na="NULL")

shape <- st_read(here::here("data","Washington_Counties_with_Natural_Shoreline___washsh_area","Washington_Counties_with_Natural_Shoreline___washsh_area.shp"))

DataType <- WashingtonData %>%
  summarise_all(class) %>%
  pivot_longer(everything(),
               names_to = "name",
               values_to = "class")
DataType
print(DataType, n = 31)

CountyOnly <- WashingtonData %>%
  clean_names(.) %>%
  select(organization_level,county,test_subject,count_of_students_expected_to_test,count_met_standard,grade_level)%>%
  filter(organization_level=="School")%>%
  filter(test_subject=="Science")%>%
  filter(grade_level=="All Grades")%>%
  filter(county!="Multiple")%>%
  group_by(county)%>%
  na.omit()%>%
  summarise(total_count_met_standard=sum(count_met_standard),
            total_count_of_students_expected_to_test=sum(count_of_students_expected_to_test))%>%
  mutate(percent_met_per_county=total_count_met_standard/total_count_of_students_expected_to_test*100)

state_average <- CountyOnly%>%
  mutate(tcms=sum(total_count_met_standard))%>%
  mutate(tcosett=sum(total_count_of_students_expected_to_test))%>%
  mutate(state_average=tcms/tcosett*100)%>%
  head(n=1)%>%
  pull(state_average)

county_only_above_below_state <- CountyOnly %>%
  mutate(difference_to_state=(percent_met_per_county-state_average))%>%
  mutate(across(difference_to_state , round, 0))%>%
  mutate(above_below = case_when(difference_to_state<0 ~ "below",
                                 difference_to_state>0 ~ "above",
                                 difference_to_state==0 ~ "equal"
  ))

joined_data <- shape %>% 
  clean_names(.) %>%
  left_join(., 
            county_only_above_below_state,
            by = c("countylabe" = "county"))

tm_shape(joined_data) + 
  tm_polygons("above_below", 
              # style="pretty",
              palette="Blues",
              midpoint=NA,
              #title="Number of years",
              alpha = 0.5) + 
  tm_compass(position = c("left", "bottom"),type = "arrow") + 
  tm_scale_bar(position = c("left", "bottom")) +
  tm_layout(title = "Counties above or below state avearge for science in all grades", 
            legend.position = c("right", "bottom"))  

#https://rud.is/b/2015/05/15/u-s-drought-monitoring-with-hexbin-state-maps-in-r/
# https://onlinecollegemathteacher.blogspot.com/2024/06/something-llttle-different-hexbin-maps.html
# library(rgdal)
# library(rgeos)
library(tidyverse)
library(readr)
library(tidyr)
library(dplyr)
library(grid)
library(geojsonio)
library(sf)
library(terra)

# download data:  https://team.carto.com/u/andrew/tables/andrew.us_states_hexgrid/public/map

#my_sf <- read_sf("us_states_hexgrid.geojson")
my_sf <- read_sf("us_states_hexgrid/us_states_hexgrid.shp")

my_sf <- my_sf %>%
  mutate(google_nam = gsub(" \\(United States\\)", "", google_nam))

intensity <- c(D0="Abnormally Dry", D1="Moderate Drought", D2="Severe Drought",
               D3="Extreme Drought", D4="Exceptional Drought")

drought <- read_csv("droughtmonitor2.csv") %>%
  filter(ValidEnd=="5/18/2015") %>%
  gather(drought_level, value, D0, D1, D2, D3, D4) %>%
  #filter(value != 0) %>%
  mutate(intensity=factor(intensity[drought_level],
                          levels=as.character(intensity), ordered=TRUE))

#
# join sf file with data file
my_sf_data <- my_sf %>%
  left_join(drought, by = c("iso3166_2" = "StateAbbreviation")) %>%
  mutate(lab_col=as.factor(ifelse(value<50,0,1)))

#my_sf_data <- fortify(my_sf_data,iso3166_2)
#
ggplot(my_sf_data) +
  geom_sf(aes(fill = value),col="black", linewidth = .25, alpha = 0.9) +
  geom_sf_text(aes(label = iso3166_2, color = lab_col), size = 4, alpha = 1) +
  theme_void() +
  scale_fill_gradient(low="#FDE5B4",high="#4527A0")+
  scale_color_manual(values=c("black","white"))+
  #scale_fill_viridis() +
  labs(
    title = "Drought Intensity - Proportion by State",
    fill = "Percent of State",
    ) +
  guides(color=F)+
  theme(
    legend.position="bottom",
    legend.margin = margin(t=30),
    plot.title = element_text(size=24, face="bold",hjust=.5,
                              margin=margin(b=30)),
    legend.title = element_text(size=12, face="bold"),
    legend.text = element_text(size=10, face="bold"),
    strip.text=element_text(size=14))+
  facet_wrap(.~intensity)







#https://www.datanovia.com/en/blog/how-to-create-a-map-using-ggplot2/
library(rnaturalearth)
library(rnaturalearthdata)
library(tidyverse)
library(sf)
#library(rgeos)
library(ggspatial)
library(ggrepel)

# Some EU Contries
some.eu.countries <- c(
  "Portugal", "Spain", "France", "Switzerland", "Germany",
  "Austria", "Belgium", "UK", "Netherlands",
  "Denmark", "Poland", "Italy", 
  "Croatia", "Slovenia", "Hungary", "Slovakia",
  "Czech republic"
)
# Retrieve the map data
some.eu.maps <- map_data("world", region = some.eu.countries)

# Compute the centroid as the mean longitude and lattitude
# Used as label coordinate for country's names
region.lab.data <- some.eu.maps %>%
  group_by(region) %>%
  summarise(long = mean(long), lat = mean(lat))

ggplot(some.eu.maps, aes(x = long, y = lat)) +
  geom_polygon(aes( group = group, fill = region))+
  geom_text(aes(label = region), data = region.lab.data,  size = 3, hjust = 0.5)+
  scale_fill_viridis_d()+
  theme_void()+
  theme(legend.position = "none")



#https://www.r-spatial.org/r/2018/10/25/ggplot2-sf.html



world <- ne_countries(scale = "medium", returnclass = "sf")
class(world)
ggplot(data=world) +
  geom_sf()

ggplot(data = world) +
  geom_sf() +
  xlab("Longitude") + ylab("Latitude") +
  ggtitle("World map", subtitle = paste0("(", length(unique(world$name)), " countries)"))

ggplot(data = world) + 
  geom_sf(color = "black", fill = "lightgreen")

ggplot(data = world) +
  geom_sf(aes(fill = pop_est)) +
  scale_fill_viridis_c(option = "plasma", trans = "sqrt")

ggplot(data = world) +
  geom_sf(aes(fill = pop_est)) +
  scale_fill_viridis_c(option = "plasma", trans = "sqrt")+
  coord_sf(crs = "+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +units=m +no_defs ")

#world$geometry <- world$geometry %>%
#  s2::s2_rebuild() %>%
#  sf::st_as_sfc()
sf::sf_use_s2(FALSE)
world_points<- st_centroid(world)
world_points <- cbind(world, st_coordinates(st_centroid(world$geometry)))

ggplot(data = world) + 
  geom_sf(fill= 'antiquewhite') + 
  geom_text(data= world_points,aes(x=X, y=Y, label=name), 
            color = 'darkblue', 
            fontface = 'bold', 
            check_overlap = FALSE) + 
  annotate(geom = 'text', x = -90, y = 26, 
           label = 'Gulf of Mexico', 
           fontface = 'italic', 
           color = 'grey22', size = 6) + 
  annotation_scale(location = 'bl', 
                   width_hint = 0.5) + 
  annotation_north_arrow(location = 'bl', 
                         which_north = 'true', 
                         pad_x = unit(0.75, 'in'), 
                         pad_y = unit(0.5, 'in'), 
                         style = north_arrow_fancy_orienteering) + 
  coord_sf(xlim = c(-102.15, -74.12), ylim = c(7.65, 33.97), expand = FALSE) + 
  xlab('Longitude') + 
  ylab('Latitude') + 
  ggtitle('Map of the Gulf of Mexico and the Caribbean Sea') + 
  theme(panel.grid.major = element_line(color = gray(.5), 
                                        linetype = 'dashed', 
                                        size = 0.5), 
        panel.background = element_rect(fill = 'aliceblue'))


world %>% filter(subregion=="South-Eastern Asia") %>%
  ggplot()+
  geom_sf(aes(fill=name))+
  geom_point(data=world_points %>% 
               filter(subregion=="South-Eastern Asia"),
             aes(x=X,y=Y,col=name))+
  geom_text_repel(data=world_points %>% 
               filter(subregion=="South-Eastern Asia"),
             aes(x=X,y=Y,label=name,col=name),
             alpha=.5)+
  theme(panel.grid.major = element_line(color = gray(.5), 
                                        linetype = 'dashed', 
                                        size = 0.5), 
        panel.background = element_rect(fill = 'aliceblue'))

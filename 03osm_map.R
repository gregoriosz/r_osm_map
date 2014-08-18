# térkép rajzolása
# az adatok nincsenek mellékelve

library(ggplot2)

telep_adatok <- read.csv("~/R/projects/osm/telep_adatok.csv", 
                         sep=";", dec=",", stringsAsFactors=FALSE, 
            colClasses=c("character", "character","numeric", rep("character",4)))

ggplot(telep_adatok, aes(map_id = osm_id)) + 
  geom_map(map = map_w5, color="lightgrey", aes(fill=tjogall))+
  scale_fill_discrete(name="Teletip.")+
  expand_limits(x = map_w5$long, y = map_w5$lat) + 
  coord_fixed(ratio = 3/2) + 
  geom_text( aes(x = max(map_w5$long), y=min(map_w5$lat), label="OpenStreetMap", hjust=1))

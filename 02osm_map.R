
# egyelőre így készültek =================================================

# első poligonok meghatározása

require(dplyr)
# ellenőrzés határoló vonalak száma településenként
# n>1
map_n_ways <- group_by(map_w3, osm_id, way)%>%
  summarise(node_db=n())%>%
  group_by(osm_id)%>%
  summarise(way_db=n())
# egy szakasszal körberajzolt település (Gadács, 1368020)
egy_ut <- filter(map_n_ways, way_db==1, osm_id)

# a többi település (map_w$role %in% c("admin_centre","label"))==F
osm_vector <- filter(osm_telep, (osm_id %in% egy_ut[,"osm_id"])==F )%>%
                          select(osm_id)
osm_vector <- as.vector(osm_vector[,1])

for(i in 1:length(osm_vector)){
  # települési koordináták kiválasztása
  ldnybn <- filter(map_w3, osm_id==osm_vector[i])
  # határszakaszok
  ways <- sort(unique(ldnybn[,"way"]))
  
  # kezdő szakasz
  sorba <- ldnybn[ldnybn$way==ways[1],]
  # gyűrű kezdőpontja
  kor_nyit <- sorba[sorba$way_pos==0,c("lat","long")]
  
  repeat {
    # most az utolsó eset
    i_utolso <- sorba[nrow(sorba) ,c("osm_id","lat","long")]
    # maradék
    maradek <- ldnybn[(ldnybn$way %in% unique(sorba$way))==F,]
    #következő way
    kov_way <- maradek[maradek$lat==i_utolso$lat & 
                         maradek$lon==i_utolso$long, c("way","way_pos")]
    
    if (kov_way$way_pos == 0) {
      kov_way2 <- maradek[maradek$way==kov_way[,1],]
    } else {
      kov_way2 <- maradek[maradek$way==kov_way[,1],]
      kov_way2$way_pos <- sort(kov_way2$way_pos, decreasing = T)
      kov_way2 <- kov_way2[order(kov_way2$way_pos),]
    }
    sorba <- rbind(sorba, kov_way2)
    # aktuális utolsó eset
    most_utolso <- sorba[nrow(sorba) ,c("lat","long")]
    if (most_utolso[,1]==kor_nyit[,1] && most_utolso[,2]==kor_nyit[,2]) break
  }
  
  if (i==1){
    map_poly <- sorba
  } else{
    map_poly <- rbind(map_poly, sorba)
  }
}  

map_poly <- rbind(map_poly, map_w3[(map_w3$osm_id %in% egy_ut$osm_id)==T, ])
map_poly$piece <- 1

piece_1 <- group_by(map_poly, osm_id, way)%>%
  summarise(piece = mean(piece))


# maradékok
map_w3b <- left_join(map_w3, piece_1, by = c("osm_id", "way"))%>%
  filter(is.na(piece) == T)

map_w3b$piece <- NULL

# ======== kettes poligonok ===================================================

# ellenőrzés határoló vonalak száma településenként
# n>1
map_n_ways <- group_by(map_w3b, osm_id, way)%>%
  summarise(node_db=n())%>%
  group_by(osm_id)%>%
  summarise(way_db=n())%>%
  filter(way_db>2)
# egy szakasszal körberajzolt település (Gadács, 1368020)
egy_ut <- filter(map_n_ways, way_db==1, osm_id)

# a települések dupla poligonokkal
osm_vector <- unique(map_n_ways$osm_id)

for(i in 1:length(osm_vector)){
  # települési koordináták kiválasztása
  ldnybn <- filter(map_w3b, osm_id==osm_vector[i])
  # határszakaszok
  ways <- sort(unique(ldnybn[,"way"]))
  
  # kezdő szakasz
  sorba <- ldnybn[ldnybn$way==ways[1],]
  # gyűrű kezdőpontja
  kor_nyit <- sorba[sorba$way_pos==0,c("lat","long")]
  
  repeat {
    # most az utolsó eset
    i_utolso <- sorba[nrow(sorba) ,c("osm_id","lat","long")]
    # maradék
    maradek <- ldnybn[(ldnybn$way %in% unique(sorba$way))==F,]
    #következő way
    kov_way <- maradek[maradek$lat==i_utolso$lat & 
                         maradek$lon==i_utolso$long, c("way","way_pos")]
    
    if (kov_way$way_pos == 0) {
      kov_way2 <- maradek[maradek$way==kov_way[,1],]
    } else {
      kov_way2 <- maradek[maradek$way==kov_way[,1],]
      kov_way2$way_pos <- sort(kov_way2$way_pos, decreasing = T)
      kov_way2 <- kov_way2[order(kov_way2$way_pos),]
    }
    sorba <- rbind(sorba, kov_way2)
    # aktuális utolsó eset
    most_utolso <- sorba[nrow(sorba) ,c("lat","long")]
    if (most_utolso[,1]==kor_nyit[,1] && most_utolso[,2]==kor_nyit[,2]) break
  }
  
  if (i==1){
    map_poly2 <- sorba
  } else{
    map_poly2 <- rbind(map_poly2, sorba)
  }
} 

map_poly2$piece <- 2

map_w5 <- rbind(map_poly, map_poly2)
map_w5 <- filter(map_w5, (way %in% c(57879818, 57873610))==F )

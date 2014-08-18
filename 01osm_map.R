# == OSM 'way'-k, határoló vonalak letöltése =====================

osm_ways <- function(settlement_q){
  require(XML2R)
  # web lekérdezéshez állandó részek: 
  nmt1 <- "http://api.openstreetmap.org/api/0.6/relation/"
  # keresõ kifejezés:
  search_string <- paste0(nmt1, settlement_q, collapse = "")  
  # Település koordinátái és egyebek
  dt <- XML2R(search_string)
  # település határoló vonalai és még néhány más
  dt_k <- as.data.frame(dt[[3]][,1:3], stringsAsFactors = F)
  dt_k$osm_id=settlement_q
  return(dt_k)
}

# =================
# x=lon, y=lat
# =================

# === Az adatok lekérdezése ==============================================

# bemenő adat: OSM azonosítókat tartalmazó állomány
osm_telep <- read.csv("~/R/projects/osm/osm_id_tab2.csv", sep=";", stringsAsFactors=FALSE)

# ebből készül a szakaszok állománya
library(XML)
library(XML2R)

for (i in 1:nrow(osm_telep)){
  map <- osm_ways(settlement_q = osm_id[i,2])
  if (i==1) {
    map_w <- map
  } else{
    map_w <- rbind(map_w, map)
  }
}

# map_w: az összes település 'ref' része az xml-ből: 
#  sokféle kapcsolódó adat, pl. a település adminisztratív központjának
#  azonosítója

# kiszűrjük, ami nem kell
map_w2 <- map_w[(map_w$role %in% c("admin_centre","label","inner"))==F,]
# way átalakítáa numerikussá
map_w2$way = as.numeric(map_w2$ref)
map_w2$ref <- NULL


# === Vonalakhoz tartozó pontok koordinátái ============================

# sqlite adatbázis kapcs: osm_db$con

library(dplyr)

# dplyr src kapcsolat
osm_db <- src_sqlite("hungary-latest.osm.db", create = F)
# sqlite aatbázis kapcs: osm_db$con
dbListTables(osm_db$con)
# sql a kapcsolathoz
node_sql <- "select way_id as way, node_id as node, way_pos, lat, lon as long from ways_nodes as t1 left join nodes as t2 on t1.node_id = t2.id"
# az összes pont
points <- dbGetQuery(conn = osm_db$con, statement = node_sql)

# a 'way'-hez hozzákapcsoljuk a pontjait és akoordinátáit:
# a forrás az előzőleg letöltött adatállomány
map_w3 <- left_join(map_w2, points, by = "way")%>%
  arrange(osm_id, way, way_pos)%>%
  mutate(id = osm_id)%>%
  filter(is.na(long)==F)

rm(points, map_w)

# Ezzel meg is vannak a bemenő adatok a térkép elkészítéséhez.
# De még rendezni kell.

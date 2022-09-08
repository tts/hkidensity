library(tidyverse)
library(sf)

#----------------------
# Population grid
#----------------------

# https://avoidatastr.blob.core.windows.net/avoindata/AvoinData/6_Asuminen/Vaestotietoruudukko/Vaestoruudukko.pdf
# The bigger the value of asvaljyys, the more space there is

baseurl <- "https://kartta.hsy.fi/geoserver/wfs?request=GetFeature&service=WFS&version=2.0.0"
type <- "asuminen_ja_maankaytto:Vaestotietoruudukko_2021"
request <- paste0(baseurl, "&typeName=", type, "&outputFormat=json")
pop <- st_read(request, stringsAsFactors = FALSE)
pop <- st_as_sf(pop) %>% 
  filter(asvaljyys != 999999999.0) %>% # No data
  select(id, asvaljyys) %>% 
  st_transform(crs = 4326)

#-----
# Area
#-----

url <-"https://www.hel.fi/hel2/tietokeskus/data/kartta_aineistot/PKS_Kartta_Rajat_KML2011.zip"
temp <- tempfile()
download.file(url, temp)
unzip(zipfile = temp)
unlink(temp)

areas_big <- st_read("PKS_suuralue.kml")

# Helsinki
hki_big_area <- areas_big %>%
  filter(Name %in% c("Läntinen", "Keskinen", "Pohjoinen", "Itäinen", 
                     "Koillinen", "Kaakkoinen", "Eteläinen"))

areas_small <- st_read("PKS_pienalue.kml")

# Districts within the Helsinki area
distr_in_area <- st_join(st_buffer(areas_small, dist = 0),
                         st_buffer(hki_big_area, dist = 0),
                         left = FALSE, largest = TRUE) %>% 
  st_transform(crs = 4326)

#-------------------------
# Grids in districts
#-------------------------

pop_in_distr <- st_join(pop, distr_in_area) %>% 
  filter(!is.na(Name.x)) %>% 
  rename(name = Name.x) %>% 
  select(id, name, asvaljyys) %>% 
  group_by(name) %>% 
  summarise(median_dens = mean(asvaljyys))

saveRDS(pop_in_distr, "pop_in_distr.RDS")

#------
# Map
#------

library(leaflet)

pal <- colorBin(
  palette = "viridis"  
  , domain = pop_in_distr$median_dens
  , bins = 5
  , reverse = FALSE
)

m_b <- leaflet(pop_in_distr) %>%
  addProviderTiles(
    "OpenStreetMap.Mapnik"
  ) %>%
  addTiles(attribution = " data: Population grid of Helsinki metropolitan area. Helsingin seudun ympäristöpalvelut HSY.") %>% 
  addPolygons(weight = 0.3, 
              fillColor = ~pal(median_dens), 
              fillOpacity = 1,
              opacity = 0.7,
              label = paste0(pop_in_distr$name, " ",
                             sprintf("%.2f", pop_in_distr$median_dens))) %>%
  addLegend(
    "topright"
    , pal = pal
    , values = ~median_dens
    , title = "Occupancy rate"
    , opacity = 1
  )

m_b

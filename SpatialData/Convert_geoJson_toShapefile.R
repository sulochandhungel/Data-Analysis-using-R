#install.packages("geojsonsf")
library(geojsonsf)
library(sf)

sf <- geojson_sf("C:/Users/sulochan.dhungel/Sulochan/Puunene/Building Footprints/Hawaii.geojson/Hawaii.geojson")
head(sf)

st_write(sf, "C:/Users/sulochan.dhungel/Sulochan/Puunene/Building Footprints/Hawaii.geojson/Hawaii_Bldg_Fp.shp")
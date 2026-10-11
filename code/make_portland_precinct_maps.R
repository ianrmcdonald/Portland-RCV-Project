library(sf)
library(tidyverse)
library(osmdata)
library(prismatic)
library(maptiles)
library(tidyterra)



#Multnomah Elections Precincts map:  Multnomah County
#https://multco.us/info/maps-and-data-multnomah-county-elections


d1_bounds <- c(xmin = -122.6355, ymin = 45.459, xmax = -122.4720, ymax = 45.6205)
d2_bounds <- c(xmin = -122.8, ymin = 45.52, xmax = -122.560, ymax = 45.66)
d3_bounds <- c(xmin = -122.6744, ymin = 45.4512, xmax = -122.5381, ymax = 45.5553)
d4_bounds <- c(xmin = -122.8677, ymin = 45.4327, xmax = -122.6145, ymax = 45.6255)
pdx_bounds <- c(xmin = -122.84, ymin = 45.4333, xmax = -122.45, ymax = 45.6538)


mult_precincts <- st_read(
  "data/shapefiles/Multnomah_Elections_Precinct_Split_2024/Multnomah_Elections_Precinct_Split_2024.shp") |> 
  st_transform(crs=4269) |> 
  st_make_valid()


#Oregon House Districts
#https://data.oregon.gov/dataset/House-Districts/xmn5-97j4/about_data
or_house_districts <- st_read("data/geojson/House_Districts.geojson") |> 
  st_transform(crs=4269) |> 
  st_make_valid()

#City of Portland Council Districts
#https://www.portland.gov/transition/districtcommission/map
portland_council_districts <- st_read("data/shapefiles/Portland_City_Council_Districts/Portland_City_Council_Districts.shp") |> 
  st_transform(crs=4269) |> 
  st_make_valid()

#from metro website:  precincts for Multnomah, Clackamas, and Washington Counties
#these will add the small precincts in Clackamas and Washington counties
#https://rlisdiscovery.oregonmetro.gov/datasets/drcMetro::voter-precincts/about

metro_voter_precincts <- st_read("data/shapefiles/Voter_Precincts/Voter_Precincts.shp") |> 
  st_transform(crs=4269) |> 
  st_make_valid()

neighborhoods <- st_read("data/shapefiles/Neighborhood_Boundaries/Neighborhood_Boundaries.shp") |> 
  st_transform(crs=4269) |> 
  st_make_valid()

#combines precincts and council districts
portland_dist_precincts <- 
  st_intersection(metro_voter_precincts, portland_council_districts) |> 
  st_make_valid()

portland_dist_precincts$precinct_area <- as.numeric(st_area(portland_dist_precincts))


neighborhoods_precincts <-
  st_intersection(portland_dist_precincts, neighborhoods) |> 
  st_make_valid()

#add a precinct ID that doesn't print for very small precincts
portland_dist_precincts <- portland_dist_precincts |>
  left_join(precinct_reference, by=join_by(PRECINCTID == precinct_1)) |> 
  mutate(PRECINCTID_PRINT = ifelse(precinct_area <= 4e+05,
                                   "",
                                   PRECINCTID))

portland_streets <-
  st_read("data/shapefiles/City_of_Portland_Area_Geocoding_Streets/City_of_Portland_Area_Geocoding_Streets.shp")

portland_streets |> ggplot() + geom_sf()

portland_streets <- 
  st_read("data/shapefiles/Streets/Streets.shp")


make_my_bbox <- function(boundaries) {
  sf::st_bbox(
    boundaries,
    crs = sf::st_crs(4269))
}

d1_bbox <- make_my_bbox(d1_bounds)
d2_bbox <- make_my_bbox(d2_bounds)
d3_bbox <- make_my_bbox(d3_bounds)
d4_bbox <- make_my_bbox(d4_bounds)
pdx_bbox <- make_my_bbox(pdx_bounds)

#not sure if st_transform is strictly necessary.
d1_bbox <- st_transform(st_as_sfc(d1_bbox), 4269)
d2_bbox <- st_transform(st_as_sfc(d2_bbox), 4269)
d3_bbox <- st_transform(st_as_sfc(d3_bbox), 4269)
d4_bbox <- st_transform(st_as_sfc(d4_bbox), 4269)
pdx_bbox <- st_transform(st_as_sfc(pdx_bbox), 4269)

make_my_tiles <- function(bbox_name) {
  get_tiles(bbox_name, provider = "OpenStreetMap", zoom = 14,
            forceDownload = TRUE)
}

osm_tiles_d1 <- make_my_tiles(d1_bbox)
osm_tiles_d2 <- make_my_tiles(d2_bbox)
osm_tiles_d3 <- make_my_tiles(d3_bbox)
osm_tiles_d4 <- make_my_tiles(d4_bbox)
osm_tiles_pdx <- make_my_tiles(pdx_bbox)


#map template







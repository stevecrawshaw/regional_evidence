pacman::p_load(tidyverse, glue, janitor, sf, RPostgres, DBI, lwgeom)
# utility functions ----
concat_if_2 <- function(c){
  # for concatenating list col fields c("Bristol" "City of")
  out <- if(length(c) == 2){
    paste0(c, collapse = ", ")
    
  } else {
    out = c
  }
  unlist(out)
}
sf_use_s2(TRUE)

unlist_clean_cols <- function(sf_tbl){
  sf_tbl |> 
    mutate(ladnm = map_chr(ctyua_name, ~concat_if_2(.x)),
           ctyua_name = NULL,
           across(.cols = matches("ctyua|ctry|rgn"), unlist))
}
# database connection ----
creds <- config::get(file = "../config.yml", config = "weca_gis")[c(-1)]

tryCatch({

  print("Connecting to GIS Database…")
  con <- dbConnect(RPostgres::Postgres(),
                      host = creds$hostname,
                      dbname = creds$database,
                      user = creds$uid,
                      password = creds$pwd)
  print("Database Connected! :D")
},
error=function(cond) {
  print("Unable to connect to GIS Database. Check VPN")
})

# List all of the schemas in the database
(all_schemas <- dbGetQuery(con,"SELECT schema_name FROM information_schema.schemata"))

# get data from database ----

os_tables <- dbGetQuery(con,"SELECT table_name FROM information_schema.tables WHERE table_schema='os'")

bdline_ua_weca_diss <- st_read(con, query = "SELECT * FROM os.bdline_ua_weca_diss") # weca boundary dissolved from GIS - don't use as
# the official weca boundary includes a large part of the estuary so isn't valid for comparisons with other areas (IMO)
bdline_ua_weca_diss |> st_area()

# List all of the tables from a specified schema, for example...
# list all of the tables in the natural_england schema
ne_tables <- dbGetQuery(con,"SELECT table_name FROM information_schema.tables WHERE table_schema='natural_england'")

sssi <- st_read(con, query = "SELECT * FROM natural_england.sssi")

con |> dbDisconnect()
# read data from local files ----
# boundaries from OS derived from the UA boundaries (land only)
lep_boundary <- st_read("../../qgis/gis_files/lep_boundary_ods_wgs84.geojson") |> 
  unlist_clean_cols()

weca_boundary <- st_read("../../qgis/gis_files/weca_boundary_ods_wgs84.geojson") |>
  unlist_clean_cols()

# reproject to BNG for area calculation
lep_boundary_27700 <- lep_boundary |> 
  st_transform(crs = 27700)

weca_boundary_27700 <- weca_boundary |> 
  st_transform(crs = 27700)

aonb <- st_read("../../qgis/gis_files/Areas_of_Outstanding_Natural_Beauty_England/Areas_of_Outstanding_Natural_Beauty_England_-4111463527894639920.gpkg") 

# this is for the whole of england - faster to download the whole 
# file than filter on ESRI's crap OD portal
nfi_england_all_2022 <- st_read("../../qgis/gis_files/Trees and woodland/National_Forest_Inventory_England_2022.geojson")

# get the green space data from OS 
# #https://osdatahub.os.uk/downloads/open/OpenGreenspace?_gl=1*180pnqw*_ga*ODY0OTUwNDEuMTcyMjg1MzY4Ng..*_ga_59ZBN7DVBG*MTcyMjg1MzY4Ni4xLjEuMTcyMjg1MzY5NS41MS4wLjA.
# 
os_green_space <- st_read("../../qgis/gis_files/open_green_space_os/ST_GreenspaceSite.shp")

os_green_space_weca <- os_green_space |> 
  st_intersection(weca_boundary_27700)

# spatial operations ---
# reproject
# 
# 
nfi_england_all_2022_27700 <- nfi_england_all_2022 |> 
  st_transform(crs = 27700)
#clip to LEP
nfi_lep_2022_27700 <- lep_boundary_27700 |> 
  st_intersection(nfi_england_all_2022_27700)

nfi_lep_2022_27700 |> 
  st_write("../../qgis/gis_files/Trees and woodland/nfi_2022_lep_27700.gpkg")

#clip to weca
nfi_weca_2022_27700 <- weca_boundary_27700 |> 
  st_intersection(nfi_lep_2022_27700)
# write
# 
nfi_weca_2022_path <- "../../qgis/gis_files/Trees and woodland/nfi_2022_weca_27700.gpkg"

nfi_weca_2022_27700 |> 
  st_write(nfi_weca_2022_path)

nfi_weca_2022_27700 <- st_read(nfi_weca_2022_path)

# the area of weca's land boundary
(weca_area_ua_land <- weca_boundary_27700 |> st_area() |> sum())

# the forested land of weca in 2022
(nfi_area_weca <- nfi_weca_2022_27700 |> 
  st_area() |> 
  sum())

# check the 2019 forest area
forest_2019_all <- st_read("../../qgis/gis_files/Trees and woodland/NATIONAL_FOREST_INVENTORY_ENGLAND_2019/NFI_England_IFT_Data_20210430.shp") #BNG

# basically the same as now
(forest_2019_weca <- weca_boundary_27700 |> 
  st_intersection(forest_2019_all) |> 
  st_area() |> 
    sum())

forest_2019_weca * 100 / weca_area_ua_land

sssi_weca <- weca_boundary_27700 |> 
  st_intersection(sssi) |> 
  mutate(calc_area = st_area(geometry))

aonb_weca <- weca_boundary_27700 |> 
  st_intersection(aonb) |> 
  mutate(calc_area = st_area(geometry))


st_write(aonb_weca,
         "data/aonb_weca.gpkg")

st_write(sssi_weca,
         "data/sssi_weca.gpkg")

# these files are unioned and disssolved in qgis   forest_protected_area.qgz to calculate the combined area. The next layer is where this calculation lives
# 

(protected_area <- st_read("data/sssi_aonb_union_dissolve.gpkg") |>
  st_area(geom))

# calculations ---


(os_green_space_weca_area <- os_green_space_weca |>
    st_area() |>
    sum())

# proportion of land protected by either AONB or SSSI status
protected_area / weca_area_ua_land * 100
# proportion of land included in national forest inventory
nfi_area_weca / weca_area_ua_land * 100

# proportion of open green space should be 4%!
os_green_space_weca_area / weca_area_ua_land * 100

(hectares_ogs <- os_green_space_weca_area / 10000)

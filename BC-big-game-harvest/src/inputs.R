# All user-defined inputs for BC Big Game Data app

# <!-- ===================================================================== -->
# ANALYSIS OPTIONS

# Which species to plot
#speciesToShow <- c("DEMU", "BEAB", "GOAT")#"ELK", "DEMU", "DEWT", "MOOS", "BEAB", "WOLF", "CARI", "GOAT", "SHEE", "BEAG", "COUG")
# Which WMU to plot -- as number, without dash, single digit as 0X
wmuToShow <- c(332)
# Which hunting region to plot -- as number, or as string if 7A or 7B
huntingRegionToShow <- c("7A")
# Choose to filter by province totals, hunting regions, or wmus
# Options:  "province", "huntingRegion", "wmu"
#filterBy <- "wmu"

# <!-- ===================================================================== -->
# PARAMETERS

# Plot options
map_strokeWeight <- 2
map_strokeOpacity <- 1
map_fillOpacity <- 0.4
map_hoverColour <- "red"
map_hoverStrokeWeight <- 2
map_staticLabelColour <- "grey"

# <!-- ===================================================================== -->
# INPUT FILES

# Directory containing data
data_dir = "../data"

# Historical hunting kills
data_huntingKills <- list(
  "path" = data_dir,
  "fileName" = "big-game-harvest-statistics-1976-2018.xlsx",
  "sheetName" = "BGHS",
  "columnTypes" = c("numeric",  # hunt year
                    "text",  # species
                    "text",  # CI
                    "numeric",  # prov flag
                    "text",  # WMU
                    "text",  # region
                    rep("numeric", 14)  # remaining columns
  )
)

# Historical population of BC
data_bcPopulation <- list(
  "path" = data_dir,
  "fileName" = "pop_bc_annual_estimates_clean.xlsx",
  "sheetName" = "data"
)

# Lists of WMUs and their corresponding hunting regions
data_wmuHuntingRegions <- list(
  "path" = data_dir,
  "fileName" = "conversions_hunting-region-wmus.xlsx",
  "sheetName" = "wmu_data",
  "columnTypes" = c("text",  # WMU id as string
                    "numeric",  # WMU id as int
                    "text",  # hunting region
                    "text"  # notes
  )
)

# Lists of hunting region names
data_regionNames <- list(
  "path" = data_dir,
  "fileName" = "conversions_hunting-region-wmus.xlsx",
  "sheetName" = "region_names",
  "columnTypes" = c("text",  # region id as string
                    "text"  # region name as string
  )
)

# List of species included in the hunting dataset and their attributes
data_species <- list(
  "path" = data_dir,
  "fileName" = "conversions_species.xlsx",
  "sheetName" = "data"
)

# Map of WMUs
map_wmu <- list(
  "path" = data_dir,
  "fileName" = "wmu-map/WAA_WILDLIFE_MGMT_UNITS_SVW.geojson"
)

# <!-- ===================================================================== -->
# LINKS

# Site to preview different basemaps:  http://leaflet-extras.github.io/leaflet-providers/preview/index.html
#tilesURL <- "http://server.arcgisonline.com/ArcGIS/rest/services/Canvas/World_Light_Gray_Base/MapServer/tile/{z}/{y}/{x}"
tilesURL <- "https://server.arcgisonline.com/ArcGIS/rest/services/World_Topo_Map/MapServer/tile/{z}/{y}/{x}"
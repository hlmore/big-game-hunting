#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# <!-- ===================================================================== -->
# LOAD PACKAGES

library(readxl) # read Excel spreadsheets
library(tidyverse) # data manipulation
library(janitor) # data cleaning
library(leaflet)  # mapping
library(geojsonio)  # read geojson files
library(rmapshaper)  # simplify areas on maps
library(rgeos)  # functions for handling geographic regions
library(geosphere)  # functions for handling sperical regions (i.e. lat/long coordinages)
library(RColorBrewer)  # colour schemes
library(htmltools)  # HTML formatting for map labels

# <!-- ===================================================================== -->
# SOURCE

# Path to files to source
source_dir <- "./src"
# Whether or not to source locally (TRUE or FALSE)
isSourceLocal <- TRUE
# Names of files to source
source_names <- c("inputs.R",
                  "CustomFunctions.R",
                  "GetColourHex.R"
                  )

# Source files
for (fileName in source_names) {
    source(file.path(source_dir,
                     fileName,
                     fsep = .Platform$file.sep),
           local = isSourceLocal)
}

# <!-- ===================================================================== -->
# LOAD DATA

# Hunting kills
df_huntingKills <- ReadExcel(data_huntingKills)

# BC population
df_pop <- ReadExcel(data_bcPopulation)

# Conversions between WMU numbers and hunting regions
df_wmuRegions <- ReadExcel(data_wmuHuntingRegions)
# Dictionary to convert WMU codes "XXX" to formatted "X-XX" strings.  For using
# dplyr::recode() easily later on, set the names and values as:
#   - names = codes "XXX"
#   - values = formatted strings "X-XX"
wmu_dic <- setNames(df_wmuRegions$wmu_string,  # values
                    as.character(df_wmuRegions$wmu_int))  # names

# Hunting region names
df_regionNames <- ReadExcel(data_regionNames)

# Species names and characteristics
df_species <- ReadExcel(data_species)

# Map of wildlife management units.
# https://rstudio.github.io/leaflet/json.html
# Simplify boundaries so they render faster -- size goes from 4,535,784 to 963,312 bytes
sp_wmus <- geojsonio::geojson_read(file.path(map_wmu[["path"]], map_wmu[["fileName"]], 
                                             fsep = .Platform$file.sep), what = "sp") %>% 
    rmapshaper::ms_simplify()
# Try saving reduced-size map to lower RShiny load time
# https://towardsdatascience.com/interactive-covid19-report-with-rmarkdown-plotly-leaflet-and-shiny-c6a716af7d9b
# saveRDS(world_geojson, "countries_map.rds")

# <!-- ===================================================================== -->
# FUNCTIONS
# Define here as they use some properties of the loaded data

# Look up full name of species based on code
# Input is species code as string, e.g. "BEAB"
# Output is species name as string, e.g. "Black bear"
GetSpeciesName <- function(speciesCode) {
    species_name <- df_species %>% 
        filter(code==speciesCode) %>% 
        pull(name)
    return(species_name)
}

# Find colour corresponding to species
# Input is species code as string, e.g. "BEAB"
# Output is colour as hex string, e.g. "#000000"
GetSpeciesColour <- function(speciesCode) {
    species_colour <- df_species %>% 
        filter(code==speciesCode) %>% 
        pull(colour) %>% 
        GetColourHex()
    return(species_colour)
}

# Rename entries in residency columns to "resident" or "non-resident" when they contain other information
# Input:  x is a column from a dataframe, as a list; get it by df$x
# Output:  renamed_x is a list that can be put back into a dataframe
RenameResidentEntries <- function(x) {
    x[str_detect(x, "non")] <- "non-resident"
    x[str_detect(x, "non", negate = TRUE)] <- "resident"
    return(x)
}

# Find hunting region of a WMU
# Input:  WMU number as string
# Output:  Hunting region number as string
GetRegionCode <- function(wmuID) {
    regionString <- df_wmuRegions %>% 
        filter(wmu_string==wmuID) %>% 
        pull(region)  # return column as a vector
    return(regionString)
}
# Get name of hunting region from region number
# Input:  Hunting region number as string
# Output:  Hunting region name as string
GetRegionName <- function(regionID) {
    regionName <- df_regionNames %>% 
        filter(region==regionID) %>% 
        pull(name)  # return column as a vector
    return(regionName)
}

# Filter hunting data based on criteria.
# Using a function allows all the manipulation to happen in one go, so that we only have to define the reactive dataframe once.  There must be other ways to do this but I am not sure what they are.  It's also (maybe?) nicer than having a giant if elseif statement in the middle of everything.
# df_huntingKills$prov_flag
#     9 for whole-province data
#     0 otherwise
# df_huntingKills$wmu
#     999 for whole-province data
#     X00 for all of region X
#     7A or 7B for all of region 7A or 7B
# Inputs:
# - input dataframe
# - a single string denoting how to filter the input dataframe -- currently "province", "huntingRegion", "wmu"
# Output:
# - filtered dataframe
FilterByRegion <- function(df_in, filterCriteria) {
    if (filterCriteria == "province") {
        df_out <- df_in %>%
            filter(prov_flag == 9)
    } else if (filterCriteria == "huntingRegion") {
        if (is.character(huntingRegionToShow)) {
            df_out <- df_in %>%
                filter(wmu == huntingRegionToShow)
        } else if (is.numeric(huntingRegionToShow)) {
            df_out <- df_in %>%
                filter(wmu == (100 * huntingRegionToShow + 99))
        }
    } else if (filterCriteria == "wmu") {
        df_out <- df_in %>%
            filter(wmu == wmuToShow)
    }
}

# <!-- ===================================================================== -->
# Define server logic
# All the "reactive" stuff and filtering needs to be in here (I think)
app_server <- function(input, output, session) {
    
    # <!-- =====================================================================
    # --> UPDATE UI https://shiny.rstudio.com/articles/dynamic-ui.html
    output$selectedUnitsPicker <- renderUI({
        regionOption <- input$selectedRegion
        # Get a list of options for the drop-down menu -- this will be super
        # long and many entries will be repeated.
        if (regionOption == "province") {
            listOptions <- NULL
            } else if (regionOption == "huntingRegion") {
                # Note that hunting regions 7, 7A, and 7B will all be listed.  I decided to
                # keep it this way, because often values for all of region 7 are higher than
                # 7A+7B due to reporting differences.
                listOptions <- filter(df_huntingKills, 
                                          wmu %in% c("7A", "7B", 100*seq(8) + 99)
                ) %>% 
                    pull(region)
            } else if (regionOption == "wmu") {
                listOptions <- filter(df_huntingKills, 
                                      !(wmu %in% c("7A", "7B", 
                                                   100*seq(9) + 99,
                                                   100*seq(9)
                                                   )
                                        )
                ) %>% 
                    pull(wmu) %>% 
                    unique() %>% 
                    # Name WMU codes to match how people commonly refer to them.
                    # These names will be displayed in the drop-down, and the
                    # values will be returned when they are selected.
                    # https://github.com/tidyverse/dplyr/issues/2505#issuecomment-309394137
                    recode(., !!!wmu_dic)
            }
        
        if (is.null(listOptions)) {
            # Do nothing
            # Not defining a UI element means nothing will be shown
        } else {
            # Remove duplicate entries and sort, to get entries for the menu
            listOptions <- sort(unique(paste(listOptions)))
            # Generate the dropdown menu
            pickerInput("selectedUnits",
                        label = NULL,
                        choices = listOptions,
                        selected = listOptions,
                        multiple = TRUE,
                        pickerOptions(actionsBox = TRUE,  # add select/deselect all buttons
                                      selectAllText = "Select all",  # label for select all button
                                      deselectAllText = "Deselect all",  # label for deselect all button
                                      noneSelectedText = "No region selected",  # text to display if nothing selected
                                      liveSearch = TRUE,  # add search box
                                      liveSearchStyle = "startsWith"
                        )
            )
        }
    })

    # <!-- ===================================================================== -->
    # FILTER AND ADD OTHER VARIABLES
    # Do this in one go for each dataframe, so that it's easier to make the dataframes reactive
    
    # Get selected species from checkboxes
    speciesToShow <- reactive({
        input$selectedSpecies
    })
    
    # Filter population data so years match hunting data
    df_pop_years <- df_pop %>% 
        filter(year >= min(df_huntingKills$hunt_year) 
               & year <= max(df_huntingKills$hunt_year)) %>% 
        rename(population_human = population)
    
    # Filter hunting data
    df_filtered <-  reactive({
        df_huntingKills %>%
            # Show selected species
            filter(species %in% speciesToShow()) %>%
            # Show selected region
            FilterByRegion(., input$selectedRegion) %>%
            # Add population data
            left_join(df_pop_years, by = c("hunt_year" = "year")) %>%
            # Compute new metrics
            rowwise() %>% # sum over each row:  https://stackoverflow.com/a/33806717
            mutate(
                total_kills = sum(resident_kills, non_resident_kills, na.rm = TRUE),
                total_hunters = sum(resident_hunters, non_resident_hunters, na.rm =
                                        TRUE),
                total_days = sum(resident_days, non_resident_days, na.rm = TRUE)
            )
    })
    
    # Compute totals for residents and non-residents over all species for each year
    df_year_totals <- reactive({
        df_filtered() %>% 
        # Compute totals per year
        group_by(hunt_year) %>% 
        summarise(
            across(resident_hunters, sum, na.rm=TRUE),
            across(non_resident_hunters, sum, na.rm=TRUE),
            across(resident_kills, sum, na.rm=TRUE),
            across(non_resident_kills, sum, na.rm=TRUE)
        ) %>% 
        # Add population of province in each year
        left_join(df_pop_years, by = c("hunt_year" = "year")) %>% 
        # Pivot # hunters
        pivot_longer(cols = contains("hunters"),
                     names_to = "hunter_residency", 
                     values_to = "n_hunters"
        ) %>% 
        # Pivot # kills
        pivot_longer(cols = contains("kills"),
                     names_to = "kill_residency", 
                     values_to = "n_kills"
        )  %>% 
        # Rename entries in resident columns to "resident" and "non-resident"
        mutate(hunter_residency = RenameResidentEntries(hunter_residency),
               kill_residency = RenameResidentEntries(kill_residency)
        ) %>% 
        # Remove rows where hunter residency and kill residency are different -- these are an artifact of doing two pivots
        filter(hunter_residency == kill_residency) %>% 
        # Remove redundant residency column and rename remaining column
        mutate(kill_residency = NULL) %>% 
        rename(residency = hunter_residency)
    })
    
    # <!-- ===================================================================== -->
    # ADD VARIABLES TO MAP DATA AND GENERATE MAP OF HUNTING REGIONS
    #
    # WMU codes in hunting data are e.g. 101, 115
    #   - codes ending in 00 denote values from that hunting region but unknown WMU
    #   - codes ending in 99 denote totals for that hunting region
    #   - code 999 is province-wide total
    # WMU codes in geojson are e.g. 1-1, 1-15
    
    # Check total areas add to area of BC (944,735 km^2)
    # Note that the geojson areas include a bunch of water so we expect a higher estimate
    #testarea <- sum(wmus$FEATURE_AREA_SQM/(1000^2))
    
    # ADD DATA TO WMU MAP
    
    # Add fields to separate region 7 into 7A and 7B (region code and region name)
    sp_wmus@data$HUNTING_REGION_ID <- unlist(lapply(sp_wmus$WILDLIFE_MGMT_UNIT_ID, 
                                                    GetRegionCode))
    sp_wmus@data$HUNTING_REGION_NAME <- unlist(lapply(sp_wmus$HUNTING_REGION_ID, 
                                                      GetRegionName))
    
    # GENERATE HUNTING REGION MAP
    
    # Group WMU boundaries into hunting regions
    # https://gis.stackexchange.com/a/225731
    sp_huntingRegions <- gUnaryUnion(sp_wmus, id = sp_wmus@data$HUNTING_REGION_ID)
    
    # ADD DATA TO HUNTING REGION MAP
    
    # Add dataframe of hunting region IDs to SpatialPolygons object; this turns it into a SpatialPolygonsDataFrame and allows us to add data for each region
    # - ID of each polygon is hunting region.
    # - dataframe column names will be converted to SpatialPolygons tags
    # - dataframe row names must be the same as polygon IDs
    # Resources:
    # - https://willchernoff.com/2012/10/08/add-attribute-data-to-object-of-class-spatialpolygonsdataframe-in-r/
    # - to get polygon IDs:  https://r.789695.n4.nabble.com/How-extract-the-names-of-ID-in-SpatialPolygons-object-td790614.html
    hunting_regions_data <- tibble(
        sapply(slot(sp_huntingRegions, "polygons"), function(x) slot(x, "ID"))
    )
    colnames(hunting_regions_data) <- c("HUNTING_REGION_ID")
    rownames(hunting_regions_data) <- hunting_regions_data$HUNTING_REGION_ID
    # Combind SpatialPolygons object and dataframe to generate SpatialPolygonsDataFrame
    sp_huntingRegions <- SpatialPolygonsDataFrame(sp_huntingRegions, 
                                                  hunting_regions_data
    )
    
    # NAME of each hunting region
    sp_huntingRegions@data$HUNTING_REGION_NAME <- unlist(lapply(sp_huntingRegions$HUNTING_REGION_ID, 
                                                                GetRegionName))
    # AREA of each hunting region (km^2)
    # Note that "area" in a polygon class is NOT the projected area, but rather a planar area measurement used to ensure smaller polygons are plotted after larger ones:  https://www.rdocumentation.org/packages/sp/versions/1.4-4/topics/Polygons-class
    # Instead, from the rgeos package use gArea() (for projected coordinates) or from the geosphere package use areaPolygon() (for lat-long coordinates) (check the +proj= part of the SpatialPolygon file to find out which you are using):  https://stackoverflow.com/a/8708828
    sp_huntingRegions@data$HUNTING_REGION_AREA <- areaPolygon(sp_huntingRegions)/(1000^2)
    
    # <!-- ===================================================================== -->
    # PLOTTING FUNCTIONS
    
    # Theme
    UseTheme <- function() {
        theme_light()
    }
    
    # Line and point styles
    FormatLines <- function() {
        geom_line()
    }
    FormatPoints <- function() {
        geom_point(size=2, alpha=0.7)
    }
    
    # Use species-specific colours in line plot
    UseSpeciesColours <- function() {
        scale_colour_manual(
            name = "Species",  # legend title
            values = paste(lapply(speciesToShow(), GetSpeciesColour)),  # colour hex values
            breaks = speciesToShow(),  # species codes corresponding to each hex value
            labels = paste(lapply(speciesToShow(), GetSpeciesName))  # species names for legend
        )
    }
    
    # Text identifying year range
    # YearRangeAsText <- function() {
    #     paste("from", 
    #           paste(min(df_filtered$hunt_year)), 
    #           "to", 
    #           paste(max(df_filtered$hunt_year))
    #     )
    # }
    
    # Generate colour palette for maps
    # Show possible colour schemes
    #display.brewer.all()
    mapColours <- colorFactor(
        palette = brewer.pal(length(unique(sp_wmus$HUNTING_REGION_ID)), "Set3"),
        levels = unique(sp_wmus$HUNTING_REGION_ID)
    )
    
    # Add polygons with consistent style
    # Inputs:
    # - Leaflet map (just use .)
    # - SpatialPolygonDataFrame with polygons and data field containing colour variable
    # - name of variable to colour polygons by; must be in data field of SpatialPolygonDataFrame
    # - text for labels on hover; can include HTML
    # Output:  plots polygons with popup labels on the leaflet map
    PlotPolygons <- function(leafletObject, inputData, colourVariableName, labelText) {
        leafletObject %>% 
            addPolygons(data = inputData,
                        stroke = TRUE,
                        weight = map_strokeWeight,
                        opacity = map_strokeOpacity,
                        fillOpacity = map_fillOpacity,
                        color = ~mapColours(inputData[[colourVariableName]]),
                        # Text that pops up on hover:  https://stackoverflow.com/a/43155126
                        label = lapply(labelText, htmltools::HTML),
                        labelOptions = labelOptions(direction = "top"),
                        # Polygon appearance on hover
                        highlight = highlightOptions(color = map_hoverColour,
                                                     fillColor = map_hoverColour,
                                                     weight = map_hoverStrokeWeight,
                                                     bringToFront = TRUE
                        )
            )
    }
    # Add static text to label polygons
    # Inputs:
    # - Leaflet map (just use .)
    # - Dataframe containing columns:
    #   - "long" = longitude coordinate of each label
    #   - "lat" = latitude coordinate of each label
    #   - "labelVariableName" = text to show for each label; can include HTML
    # Output:  none, just applies labels
    AddStaticLabels <- function(leafletObject, labelData, labelVariableName) {
        leafletObject %>% 
            addLabelOnlyMarkers(data = labelData,
                                lng = ~long, 
                                lat = ~lat, 
                                label = labelData[[labelVariableName]],
                                labelOptions = labelOptions(
                                    noHide = TRUE, 
                                    direction = 'auto', 
                                    textOnly = TRUE,
                                    style = list(
                                        "color" = map_staticLabelColour
                                    )
                                )
            )
    }
    
    # <!-- ===================================================================== -->
    # PLOT
    
    #browser()
        output$distPlot <- renderPlot({
            ggplot(data = df_filtered()) +
                aes(x = hunt_year, y = total_kills, color = species) +
                geom_line() +
                FormatLines() +
                FormatPoints() +
                UseTheme() +
                UseSpeciesColours() +
                labs(x = paste("Year"),
                     y = paste("Total # kills"),
                     title = paste("Total kills")#, YearRangeAsText())
                )
    
            # generate bins based on input$bins from ui.R
            # x    <- faithful[, 2]
            # bins <- seq(min(x), max(x), length.out = 12)
            # 
            # # draw the histogram with the specified number of bins
            # hist(x, breaks = bins, col = map_hoverColour, border = 'white')
    
        })
        
        output$testPlot <- renderPlot({
            # Total hunters
            # ggplot(data = df_filtered()) +
            #     aes(x = hunt_year, y = total_hunters, color = species) +
            #     # FormatLines() +
            #     # FormatPoints() +
            #     UseTheme() +
            #     # UseSpeciesColours() +
            #     labs(x = paste("Year"),
            #          y = paste("Total # hunters"),
            #          title = paste("Total hunters")#, YearRangeAsText())
            #     )
            
            # generate bins based on input$bins from ui.R
            x    <- faithful[, 2]
            bins <- seq(min(x), max(x), length.out = 12)

            # draw the histogram with the specified number of bins
            hist(x, breaks = bins, col = map_hoverColour, border = 'white')
        })
        
        output$testPlot2 <- renderPlot({
        #     # Avg kills per hunter
        #     ggplot(data = df_filtered()) +
        #         aes(x = hunt_year, y = total_kills/total_hunters, color = species) +
        #         # FormatLines() +
        #         # FormatPoints() +
        #         UseTheme() +
        #         # UseSpeciesColours() +
        #         labs(x = paste("Year"),
        #              y = paste("Avg kills per hunter"),
        #              title = paste("Average kills per hunter")#, YearRangeAsText())
        #         )
        # })
        
                # generate bins based on input$bins from ui.R
                x    <- faithful[, 2]
                bins <- seq(min(x), max(x), length.out = 12)
            
                # draw the histogram with the specified number of bins
                hist(x, breaks = bins, col = map_hoverColour, border = 'white')
        })
        
        output$testPlot3 <- renderPlot({
            # # Avg hunting days per kill
            # ggplot(data = df_filtered()) +
            #     aes(x = hunt_year, y = total_days/total_kills, color = species) +
            #     # FormatLines() +
            #     # FormatPoints() +
            #     UseTheme() +
            #     # UseSpeciesColours() +
            #     labs(x = paste("Year"),
            #          y = paste("Avg hunt days per kill"),
            #          title = paste("Average # hunting days per kill")#, YearRangeAsText())
            #     )
            
            # generate bins based on input$bins from ui.R
            x    <- faithful[, 2]
            bins <- seq(min(x), max(x), length.out = 12)
    
            # draw the histogram with the specified number of bins
            hist(x, breaks = bins, col = map_hoverColour, border = 'white')
        })
        
        }

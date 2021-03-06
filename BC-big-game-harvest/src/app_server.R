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
library(scales)  # comma separator for thousands in plot axes
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
#   - values = codes "XXX"
#   - names = formatted strings "X-XX"
wmu_dic <- setNames(as.character(df_wmuRegions$wmu_int),  # values
                    df_wmuRegions$wmu_string  # names
                    )

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

# Rename entries in residency columns to "resident" or "non-resident" when they
# contain other information
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
# Using a function allows all the manipulation to happen in one go, so that we
# only have to define the reactive dataframe once.  There must be other ways to
# do this but I am not sure what they are.  It's also (maybe?) nicer than having
# a giant if elseif statement in the middle of everything.
# df_huntingKills$prov_flag
#     9 for whole-province data
#     0 otherwise
# df_huntingKills$wmu
#     999 for whole-province data
#     X00 for all of region X
#     7A or 7B for all of region 7A or 7B
# Inputs:
# - input dataframe
# - a single string denoting at which region level to filter the input dataframe
#   -- currently "province", "huntingRegion", "wmu"
# - a list of selected subregions for the selected region level.  NOTE that
#   there may be a problem with this method if the subregion options requested
#   don't match those in the dataframe.
# Output:
# - filtered dataframe
FilterByRegion <- function(df_in, filterRegion, filterSubregion) {
    if (filterRegion == "province") {
        df_out <- df_in %>%
            filter(prov_flag == 9)
    } else {
        if (length(filterSubregion) != 0) {
            if (filterRegion == "huntingRegion") {
                df_out <- df_in %>%
                    filter(region %in% filterSubregion) %>% 
                    # Keep only rows that represent region totals, so we don't
                    # get double the values.  Note that WMU codes for region 7A
                    # and 7B are 7A and 7B rather than *99 as for other regions.
                    filter(
                        (wmu %in% c("7A", 
                                    "7B", 
                                    as.character(100*seq(8) + 99)
                        )
                          )
                        )
            } else if (filterRegion == "wmu") {
                df_out <- df_in %>%
                    filter(wmu %in% filterSubregion)
            }
        } else {
            df_out <- df_in[0,]
        }
    }
    return(df_out)
}

# <!-- ===================================================================== -->
# Find properties of dataset for later, so we don't have to filter etc. in realtime.

# Distinct hunting regions.  Note that hunting regions 7, 7A, and 7B will all be
# listed.  I decided to keep it this way, because often values for all of region
# 7 are higher than 7A+7B due to reporting differences. However during use, the
# user shouldn't be able to select region 7 at the same time as 7A or 7B, or
# else region 7 data will be doubled.
huntingRegionOptions <- filter(df_huntingKills, 
                              wmu %in% c("7A", 
                                         "7B", 
                                         as.character(100*seq(8) + 99)
                                         )
                              ) %>% 
    pull(region) %>% 
    # Remove duplicate entries.  Using {} overrides default behaviour of
    # assuming the previous result is the first argument, with . being a second
    # argument.
    {sort(unique(paste(.)))}

# Distinct wildlife management units.  Include named WMU codes to match how
# people commonly refer to them. These names will then be able to be displayed
# in the drop-down picker, and the values will be returned when they are
# selected.
wmuOptions <- filter(df_huntingKills, 
                          !(wmu %in% c("7A", 
                                       "7B", 
                                       as.character(100*seq(9) + 99),
                                       as.character(100*seq(9))
                                       )
                            )
                     ) %>% 
    pull(wmu) %>% 
    unique() %>% 
    # Name WMU codes to match how people commonly refer to them.
    # https://github.com/tidyverse/dplyr/issues/2505#issuecomment-309394137
    recode(., !!!wmu_dic) %>% 
    # Remove duplicate entries.  Using {} overrides default behaviour of
    # assuming the previous result is the first argument, with . being a second
    # argument.
    {sort(unique(paste(.)))}

# <!-- ===================================================================== -->
# Define server logic
# All the "reactive" stuff and filtering needs to be in here (I think)
app_server <- function(input, output, session) {
    
    # <!-- =====================================================================
    # --> UPDATE UI https://shiny.rstudio.com/articles/dynamic-ui.html
    output$selectedUnitsPicker <- renderUI({
        regionOption <- input$selectedRegion
        # Get a list of options for the drop-down menu.  These were computed
        # earlier to reduce lag time here.
        if (regionOption == "province") {
            listOptions <- NULL
            } else if (regionOption == "huntingRegion") {
                # List options should contain all hunting regions, including 7,
                # 7A, and 7B.
                listOptions <- huntingRegionOptions
                # The user shouldn't be able to select region 7 at the same time
                # as 7A or 7B, or else region 7 data will be doubled.
                # https://stat.ethz.ch/R-manual/R-devel/library/base/html/match.html
                if ("7" %in% listOptions) {
                    selectedOptions <- listOptions[!listOptions %in% c("7A", "7B")]
                }
            } else if (regionOption == "wmu") {
                listOptions <- wmuOptions
                selectedOptions <- wmuOptions
            }
        if (is.null(listOptions)) {
            # Do nothing
            # Not defining a UI element means nothing will be shown
        } else {
            # Generate the dropdown menu
            pickerInput("selectedUnits",
                        label = NULL,
                        choices = listOptions,
                        selected = selectedOptions,
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
    
    # Watch selected regions and disable hunting region 7 or 7A/B as needed to
    # prevent simultaneously selecting region 7 and one or both of 7A/7B.
    # https://rdrr.io/cran/shinyWidgets/man/updatePickerInput.html
    # https://stackoverflow.com/questions/52822999/disable-an-item-in-selectinput-dropdown
    observeEvent(input$selectedUnits, {
        if (input$selectedRegion == "huntingRegion") {
            if ("7" %in% input$selectedUnits) {
                cat(file=stderr(), paste('triggered'), "\n")
                # Disallow options 7A and 7B when option 7 is selected
                updatePickerInput(session = session, 
                                  inputId = "selectedUnits",
                                  choicesOpt = list(
                                      disabled = c("7A", "7B")
                                      )
                )
            } else if (any(c("7A, 7B") %in% input$selectedUnits)) {
                # Disallow option 7 when either or both of options 7A or 7B are selected
                updatePickerInput(session = session, 
                                  inputId = "selectedUnits",
                                  choicesOpt = list(
                                      disabled = c("7")
                                  )
                )
            }
        }
        }, ignoreInit = TRUE)
        
        # mtcars2 <- mtcars[mtcars$mpg >= input$up, ]
        # 
        # # Method 1
        # updatePickerInput(session = session, inputId = "selectedUnits",
        #                   choices = rownames(mtcars2))
        # 
        # # Method 2
        # disabled_choices <- !rownames(mtcars) %in% rownames(mtcars2)
        # updatePickerInput(
        #     session = session, inputId = "p2",
        #     choices = rownames(mtcars),
        #     choicesOpt = list(
        #         disabled = disabled_choices,
        #         style = ifelse(disabled_choices,
        #                        yes = "color: rgba(119, 119, 119, 0.5);",
        #                        no = "")
        #     )
        # )
        # 

    # <!-- ===================================================================== -->
    # FILTER AND ADD OTHER VARIABLES
    # Do this in one go for each dataframe, so that it's easier to make the
    # dataframes reactive

    # Get selected species from checkboxes
    speciesToShow <- reactive({
        input$selectedSpecies
    })
    
    # Filter population data so years match hunting data
    df_pop_years <- df_pop %>% 
        filter(year >= min(df_huntingKills$hunt_year) 
               & year <= max(df_huntingKills$hunt_year)) %>% 
        rename(population_human = population)
    
    # Filter hunting data and sum over each species within each year
    df_filtered <-  reactive({
        df_huntingKills %>%
            # Show selected species
            filter(species %in% speciesToShow()) %>%
            # Show selected region
            FilterByRegion(., input$selectedRegion, input$selectedUnits) %>%
            # Sum over each species within each year
            select("hunt_year",
                     "species",
                     "resident_hunters",
                     "resident_days",
                     "resident_kills",
                     "non_resident_hunters",
                     "non_resident_days",
                     "non_resident_kills") %>%
            group_by(hunt_year, species) %>%
            summarise(across(resident_hunters:non_resident_kills, sum, na.rm=TRUE)) %>%
            ungroup() %>%
            # Add population data
            left_join(df_pop_years, by = c("hunt_year" = "year")) %>%
            # Compute new metrics
            rowwise() %>% # sum over each row:  https://stackoverflow.com/a/33806717
            mutate(
                total_kills = sum(resident_kills, non_resident_kills, na.rm = TRUE),
                total_hunters = sum(resident_hunters, non_resident_hunters, na.rm =
                                        TRUE),
                total_days = sum(resident_days, non_resident_days, na.rm = TRUE)
            ) %>% 
            mutate(
                avg_days_per_kill = total_days/total_kills,
                avg_kills_per_hunter = total_kills/total_hunters
            ) %>% 
            # Change inf to zero for calculated new metrics
            mutate(
                avg_days_per_kill = ifelse(is.infinite(avg_days_per_kill), 
                                           0, 
                                           avg_days_per_kill),
                avg_kills_per_hunter = ifelse(is.infinite(avg_kills_per_hunter), 
                                              0, 
                                              avg_kills_per_hunter)
            )
                
    })
    
    # Compute totals for residents and non-residents over all species for each
    # year.  Note that some species had NA kills (as opposed to 0) in some
    # categories.  For the purposes of total kills per region or WMU, these were
    # treated as 0 (I tested this with black bears in 2012 in hunt region 6).
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
        # Remove rows where hunter residency and kill residency are different --
        # these are an artifact of doing two pivots
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
    # Note that "area" in a polygon class is NOT the projected area, but rather
    # a planar area measurement used to ensure smaller polygons are plotted
    # after larger ones:
    # https://www.rdocumentation.org/packages/sp/versions/1.4-4/topics/Polygons-class
    # Instead, from the rgeos package use gArea() (for projected coordinates) or
    # from the geosphere package use areaPolygon() (for lat-long coordinates)
    # (check the +proj= part of the SpatialPolygon file to find out which you
    # are using):  https://stackoverflow.com/a/8708828
    sp_huntingRegions@data$HUNTING_REGION_AREA <- areaPolygon(sp_huntingRegions)/(1000^2)
    
    # <!-- ===================================================================== -->
    # PLOTTING FUNCTIONS
    
    # Theme
    UseTheme <- function() {
        theme_light()
    }
    
    # Set font size.  Default is 11pt.
    SetFontSize <- function() {
        theme(text = element_text(size = 15))
    }
    
    # Adjust y-axis margin so all plots end up being the same size
    # Helpful links:
    # - https://stackoverflow.com/questions/10836843/ggplot2-plot-area-margins
    # - http://www.sthda.com/english/wiki/ggplot2-title-main-axis-and-legend-titles
    # - https://statisticsglobe.com/adjust-space-between-ggplot2-axis-labels-and-plot-area-in-r
    # - https://www.rdocumentation.org/packages/ggplot2/versions/3.3.3/topics/theme
    # - https://www.rdocumentation.org/packages/ggplot2/versions/3.3.0/topics/margin
    # - ?grid::unit gives a list of units:  https://stackoverflow.com/a/17313561
    # - https://www.rdocumentation.org/packages/graphics/versions/3.6.2/topics/strwidth
    # - https://www.tutorialspoint.com/how-to-change-plot-area-margins-using-ggplot2-in-r
    # - https://stackoverflow.com/a/29465942
    # Note that I couldn't figure out how to select a single item from a
    # reactive list, so I passed the whole list and the index of the item to use
    # for the current plot.
    # Note that this is a stupid way to do this -- it would be better to pass
    # the max y tick label, because often the max is e.g. 1,050 but the max tick
    # label is e.g. 900, meaning that basing margins on the max value does not
    # offset enough for the max y tick label.  However I can't figure out a way
    # to get the max tick label easily.
    # INPUT:
    # - longestYTickLabelList = dataframe which is basically a list of values whose string representation will be subtracted from total margin width
    # - listItem = column in dataframe to choose for the plot
    SetYMargin <- function(longestYTickLabelList, listItem) {
        maxVal <- max(longestYTickLabelList[,listItem], na.rm=TRUE)
        if(maxVal<5) {
            maxVal <- format(round(maxVal, 2), 
                             format="f",
                             nsmall=2)
        } else {
            maxVal <- format(round(maxVal, 0),
                             format="f", 
                             big.mark=",",
                             nsmall=0)
        }
        theme(plot.margin = unit(c(0,
                                   strwidth("20", 
                                            units = "inches")*72,  # make sure the end of 2020 isn't cut off
                                   0,
                                   (strwidth("60,000", 
                                            units = "inches")
                                    - strwidth(maxVal,
                                        units = "inches")
                                    )*72),  # add length to left-hand side
                                   "pt"),  # padding -- top, right, bottom, left, units
                  axis.text.y = element_text(hjust = 1)  # horizontal alignment -- 0=left, 1=right
        )
    }
    
    # Turn off legend
    RemoveLegend <- function() {
        theme(legend.position = "none")
    }
    
    # Format y axis options
    FormatY <- function() {
        scale_y_continuous(labels = comma,  # thousands separator on labels
                           limits = c(0, NA),  # set lower limit to 0
                           expand = expansion(mult = c(0, .05))  # remove padding between lower limit and axis edge
        )
    }
    
    # Format x axis options
    FormatX <- function() {
        scale_x_continuous(limits = c(1975, 2020),  # set lower limit to one year before start of data
                           expand = expansion(mult = c(0, 0))  # remove padding between lower limit and axis edge
        )
    }
    
    # Line and point styles
    FormatLines <- function() {
        geom_line(size=1.5, alpha=1)
    }
    FormatPoints <- function() {
        geom_point(size=3, alpha=1)
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
    
    # Create tooltip when hovering over plots
    # https://gitlab.com/snippets/16220
    # Inputs:
    # - hover = handle of hover object
    # - df_in = dataframe containing info for data points
    # Output:  list of two items:
    # - [1] = position and style of wellPanel
    # - [2] = dataframe of data associated with hovered point
    FormatTooltip <- function(hover, df_in) {
        point <- nearPoints(df_in, 
                            hover, 
                            threshold = 10, 
                            maxpoints = 1, 
                            addDist = TRUE)
        if (nrow(point) == 0) return(NULL)
        
        # hover contains:
        #
        # $x                1980.062
        # $y                5822.115
        # $coords_css$x     61.40341
        # $coords_css$y     308.2727
        # $coords_img$x     67.54376
        # $coords_img$y     339.1
        # $img_css_ratio$x  1.1
        # $img_css_ratio$y  1.1
        # $mapping$x        "hunt_year"
        # $mapping$y        "total_kills"
        # $mapping$colour   "species"
        # $domain$left      1975
        # $domain$right     2020
        # $domain$bottom    0
        # $domain$top       34995.45
        # $range$left       44.41094
        # $range$right      250.0522
        # $range$bottom     401.5829
        # $range$top        26.01217
        # $log$x            NULL
        # $log$y            NULL
        #
        #   hover$x, hover$y    = position of cursor ON the image
        #                           (i.e. x and y coordinates in plot)
        #   hover$domain        = values of variables at the plotting area edges
        #                           (i.e. min and max x and y values of plot)
        #   hover$range         = position of plotting area edges in pixels 
        #                           relative to whole image element (zero at top left)
        #                           (i.e. min and max pixel values of the whole
        #                           graph+axes area)
        # Additionally for ggplot used mappings are returned.
        # Because we used a div(), we can use div instead of image
        
        left_px <- hover$coords_css$x
        top_px <- hover$coords_css$y
        
        # Check if the tooltip will extend below the plot, and if so make it
        # appear above the cursor instead
        if ( (hover$range$bottom - top_px)<145) {
            top_px <- top_px - 112
        }
        # If the tooltip will extend too far to the right and be squashed, move
        # it to the left
        if ( (hover$range$right - left_px)<150) {
            left_px <- left_px - 160
        }
        
        # Create style property for tooltip
        # Set background color so it's a bit transparent
        # Set z-index so we are sure are tooltip will be on top
        style <- paste0("position:absolute; 
                        z-index:100; 
                        background-color: rgba(245, 245, 245, 0.85); ",
                        "left:", left_px + 17, "px;
                            top:", top_px + 2, "px;",
                        "padding: 10px;")
        
        return(list(style, point))
    }
    
    # <!-- ===================================================================== -->
    # LEGEND
    
    # Print names of selected species
    output$legendText <- renderText({
        speciesNames <- lapply(speciesToShow(), GetSpeciesName)
        speciesColours <- lapply(speciesToShow(), GetSpeciesColour)
        # https://stackoverflow.com/a/37736135
        # https://www.codecademy.com/articles/html-inline-styles
        # https://stackoverflow.com/questions/39150328/using-lapply-for-multiple-lists-in-a-function
        txt <- mapply(spName = speciesNames, 
                      spCol = speciesColours, 
                      FUN = function(spName, spCol) paste("<span style='color:", 
                                                 spCol, 
                                                 ";'><b> ----- </b></span>", 
                                                 spName
                      ))
        paste0(txt, collapse="<br/>")
    })
    
    # <!-- ===================================================================== -->
    # PLOT
    
    # Get lengths of longest label for each y axis.  This is a dumb way to do
    # it, but I can't figure out a way to pass the y data to max() within
    # ggplot()
    maxYLabelLengthData <- reactive({
        df_filtered() %>%
            select("total_kills", 
                   "total_hunters",
                   "avg_days_per_kill", 
                   "avg_kills_per_hunter") %>% 
            mutate(topL = total_kills,
                   topR = total_hunters,
                   botL = avg_days_per_kill,
                   botR = avg_kills_per_hunter
            ) %>% 
            select(-c("total_kills", 
                      "total_hunters", 
                      "avg_days_per_kill", 
                      "avg_kills_per_hunter"))
    })
    
    output$plotTopL <- renderPlot({
        ggplot(data = df_filtered()) +
            aes(x = hunt_year, y = total_kills, color = species) +
            FormatLines() +
            FormatPoints() +
            UseTheme() +
            SetFontSize() +
            FormatX() +
            FormatY() +
            SetYMargin(maxYLabelLengthData(), 1) +
            UseSpeciesColours() +
            RemoveLegend() +
            labs(x = paste(""),
                 y = NULL,
                 title = paste("Total kills")
            )
    },
    alt = "Total kills for each species per year"
    )
    # Add tooltip on hover
    output$plotTopL_hover_info <- renderUI({
        tooltipInfo <- FormatTooltip(input$plotTopL_hover, df_filtered())
        if (!is.null(tooltipInfo)) {
            wellPanel(
                style = tooltipInfo[1],
                p(HTML(
                    paste0("<h4>", GetSpeciesName(tooltipInfo[[2]]$species), "</h4>",
                           "<b> Year: </b>", 
                           tooltipInfo[[2]]$hunt_year, "<br/>",
                           "<b> Total kills: </b>", 
                           format(tooltipInfo[[2]]$total_kills,
                                  format="f", 
                                  big.mark=","
                                  ),
                           "<br/>")))
            )
        }
    })
    
    output$plotTopR <- renderPlot({
        # Total hunters
        ggplot(data = df_filtered()) +
            aes(x = hunt_year, y = total_hunters, color = species) +
            FormatLines() +
            FormatPoints() +
            UseTheme() +
            SetFontSize() +
            FormatX() +
            FormatY() +
            SetYMargin(maxYLabelLengthData(), 2) +
            UseSpeciesColours() +
            RemoveLegend() +
            labs(x = paste(""),
                 y = NULL,
                 title = paste("Total hunters")
            )
    })
    # Add tooltip on hover
    output$plotTopR_hover_info <- renderUI({
        tooltipInfo <- FormatTooltip(input$plotTopR_hover, df_filtered())
        if (!is.null(tooltipInfo)) {
            wellPanel(
                style = tooltipInfo[1],
                p(HTML(
                    paste0("<h4>", GetSpeciesName(tooltipInfo[[2]]$species), "</h4>",
                           "<b> Year: </b>", 
                           tooltipInfo[[2]]$hunt_year, "<br/>",
                           "<b> Total hunters: </b>", 
                           format(tooltipInfo[[2]]$total_hunters,
                                  format="f", 
                                  big.mark=","
                           ),"<br/>")))
            )
        }
    })
    
    output$plotBotL <- renderPlot({
        # Avg hunting days per kill
        ggplot(data = df_filtered()) +
            aes(x = hunt_year, y = avg_days_per_kill, color = species) +
            FormatLines() +
            FormatPoints() +
            UseTheme() +
            SetFontSize() +
            FormatX() +
            FormatY() +
            SetYMargin(maxYLabelLengthData(), 3) +
            UseSpeciesColours() +
            RemoveLegend() +
            labs(x = paste("Year"),
                 y = NULL,
                 title = paste("Average hunting days per kill")
            )
    })
    # Add tooltip on hover
    output$plotBotL_hover_info <- renderUI({
        tooltipInfo <- FormatTooltip(input$plotBotL_hover, df_filtered())
        if (!is.null(tooltipInfo)) {
            wellPanel(
                style = tooltipInfo[1],
                p(HTML(
                    paste0("<h4>", GetSpeciesName(tooltipInfo[[2]]$species), "</h4>",
                           "<b> Year: </b>", 
                           tooltipInfo[[2]]$hunt_year, "<br/>",
                           "<b> Avg days per kill: </b>", 
                           round(tooltipInfo[[2]]$avg_days_per_kill, 1), "<br/>")))
            )
        }
    })
        
    output$plotBotR <- renderPlot({
        # Avg kills per hunter
        ggplot(data = df_filtered()) +
            aes(x = hunt_year, y = avg_kills_per_hunter, color = species) +
            FormatLines() +
            FormatPoints() +
            UseTheme() +
            SetFontSize() +
            FormatX() +
            FormatY() +
            SetYMargin(maxYLabelLengthData(), 4) +
            UseSpeciesColours() +
            RemoveLegend() +
            labs(x = paste("Year"),
                 y = NULL,
                 title = paste("Average success rate (# kills per hunter)")
            )
    })
    # Add tooltip on hover
    output$plotBotR_hover_info <- renderUI({
        tooltipInfo <- FormatTooltip(input$plotBotR_hover, df_filtered())
        if (!is.null(tooltipInfo)) {
            wellPanel(
                style = tooltipInfo[1],
                p(HTML(
                    paste0("<h4>", GetSpeciesName(tooltipInfo[[2]]$species), "</h4>",
                           "<b> Year: </b>", 
                           tooltipInfo[[2]]$hunt_year, "<br/>",
                           "<b> Avg kills per hunter: </b>", 
                           round(tooltipInfo[[2]]$avg_kills_per_hunter, 2), "<br/>")))
            )
        }
    })
        
        }

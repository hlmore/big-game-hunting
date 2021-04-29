#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinyWidgets)
library(shinyjs)    # use Javascript functionality without needing .js :  https://www.rdocumentation.org/packages/shinyjs/versions/1.1


# Define UI for application that draws a histogram
app_ui <- shinyUI(fluidPage(
    
    # Set up for tooltips
    #
    # Let Shiny know you want to use JavaScript functionality
    # https://www.rdocumentation.org/packages/shinyjs/versions/1.1
    useShinyjs(),

    # Application title
    titlePanel("BC big game harvest data, 1976-2018"),

    # Sidebar with inputs
    sidebarLayout(
        sidebarPanel(
            
            # Select species to show
            # https://dreamrs.github.io/shinyWidgets/reference/pickerInput.html
            # https://dreamrs.github.io/shinyWidgets/reference/pickerOptions.html
            pickerInput("selectedSpecies",
                        label = "Select species to show",
                        choices = setNames(as.list(df_species$code), df_species$name),
                        selected = df_species$code,
                        multiple = TRUE,
                        pickerOptions(actionsBox = TRUE,  # add select/deselect all buttons
                                      selectAllText = "Select all",  # label for select all button
                                      deselectAllText = "Deselect all",  # label for deselect all button
                                      noneSelectedText = "No species selected"  # text to display if no species selected
                        )
            ),
            
            # Select whether to filter by WMU or hunting region
            prettyRadioButtons("selectedRegion",
                label = "Select region(s) to show",
                choiceNames = c("All (province)", "Hunting region", "WMU"),
                choiceValues = c("province", "huntingRegion", "wmu"),
                shape = "curve",
                icon = icon("check"),
                inline = TRUE  # align options horizontally
            ),
            
            # Select which regions to filter by
            # https://shiny.rstudio.com/articles/dynamic-ui.html
            uiOutput("selectedUnitsPicker"),
            
            # Horizontal line to separate inputs from legend
            # https://stackoverflow.com/a/43593325
            hr(style = "border-top: 3px solid #FFFFFF;"),
            
            # Show legend
            p(strong("Legend")),
            p(htmlOutput("legendText"))  # https://stackoverflow.com/a/23322262
            
        ),

        # Show plots
        mainPanel(
            # Add div for relative coordinates for tooltips
            # https://gitlab.com/snippets/16220
            div(
                
                # Include options to allow interaction with plot
                # https://shiny.rstudio.com/articles/plot-interaction.html
                fluidRow(
                    
                    column(6,
                           plotOutput("plotTopL",
                                      hover = "plotTopL_hover"),
                           uiOutput("plotTopL_hover_info")
                    ),
                    
                    column(6,
                           plotOutput("plotTopR",
                                      hover = "plotTopR_hover"),
                           uiOutput("plotTopR_hover_info")
                    )
                ),
                fluidRow(
                    
                    column(6,
                           plotOutput("plotBotL",
                                      hover = "plotBotL_hover"),
                           uiOutput("plotBotL_hover_info")
                    ),
                    
                    column(6,
                           plotOutput("plotBotR",
                                      hover = "plotBotR_hover"),
                           uiOutput("plotBotR_hover_info")
                    )
                ) 
            )
        )
    )
))

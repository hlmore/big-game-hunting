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

# Define UI for application that draws a histogram
app_ui <- shinyUI(fluidPage(

    # Application title
    titlePanel("BC big game harvest data, 1976—2018"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            
            # Select species to show
            # https://dreamrs.github.io/shinyWidgets/reference/pickerInput.html
            # https://dreamrs.github.io/shinyWidgets/reference/pickerOptions.html
            pickerInput("selectedSpecies",
                        label = "Select species to show",
                        choices = df_species$code,
                        selected = df_species$code,
                        multiple = TRUE,
                        pickerOptions(actionsBox = TRUE,  # add select/deselect all buttons
                                      selectAllText = "Select all",  # label for select all button
                                      deselectAllText = "Deselect all",  # label for deselect all button
                                      noneSelectedText = "No species selected"  # text to display if no species selected
                        )
            )
        ),

        # Show a plot of the generated distribution
        mainPanel(
            # plotOutput("distPlot")
            
            # tabsetPanel(
            #     tabPanel("Plot", plotOutput("plot")), 
            #     tabPanel("Summary", verbatimTextOutput("summary")), 
            #     tabPanel("Table", tableOutput("table"))
            # )
            
                # tabPanel(
                    fluidRow(
                        
                        column(6,
                               plotOutput("distPlot")
                        ),
                        
                        column(6,
                               plotOutput("testPlot")
                        )
                    ),
                # ),
                
                # tabPanel(
                    fluidRow(
                        
                        column(6,
                               plotOutput("testPlot2")
                        ),
                        
                        column(6,
                               plotOutput("testPlot3")
                        )
                    ) 
                # )
            # )
        )
    )
))

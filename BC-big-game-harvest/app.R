# Main file to run app
#
# Inspired by:  https://stackoverflow.com/a/43003577

library(shiny)

# Path to files to source, including UI and server files
source_dir <- "./src"

# Source UI and server files.  Do these separately from other code so that you can set the scope parameters independently 
source(file.path(source_dir,
                 "app_server.R",
                 fsep = .Platform$file.sep)
       )
source(file.path(source_dir,
                 "app_ui.R",
                 fsep = .Platform$file.sep),
       local = TRUE)

# Run app
shinyApp(
  ui = app_ui,
  server = app_server
)
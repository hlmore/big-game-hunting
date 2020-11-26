GetColour <- function(colourName) {
  
  # This function takes the name of a colour and outputs its hex code
  
  # Define colours
  colourList <- list(
    # http://www.cookbook-r.com/Graphs/Colors_(ggplot2)/
    "black" = "#000000",
    "grey" = "#999999",
    "gold" = "#E69F00",
    "light blue" = "#56B4E9",
    "green" = "#009E73",
    "yellow" = "#F0E442",
    "dark blue" = "#0072B2",
    "orange" = "#D55E00",
    "pink" = "#CC79A7",
    # https://www.datanovia.com/en/blog/ggplot-colors-best-tricks-you-will-love/
    "yellow2" = "#FFDB6D",
    "gold2" = "#C4961A",
    "cream" = "#F4EDCA",
    "rust" = "#D16103",
    "light green" = "#C3D7A4",
    "dark green" = "#52854C",
    "light blue 2" = "#4E84C4",
    "dark blue 2" = "#293352"
  )
  
  # Get colour
  colourHex <- colourList[[colourName]]
  
  return(colourHex)
  
}
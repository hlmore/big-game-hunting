# B.C. big game harvesting
Explore historical and geographical trends in big game hunting.

## Table of contents
* [Description](#description)
* [Usage](#usage)
* [To do](#to-do)
* [References](#references)
* [Author](#author)

## Description
Personal project under development.

This interactive app visualizes historical hunting data in B.C., Canada, for selected wildlife management units, selected hunting regions, or province-wide.

My main goals for the project are to learn about historical and regional trends in big game populations, and how they relate to hunting limits set by the provincial government.  A second goal is to gain experience working with government data sets.  And, I want to improve my skills in:
* data manipulation with dplyr/tidyverse
* combining data sets from different sources
* visualizing geographic data
* interactive data visualization
* RShiny app development

## Screenshots


## Usage
Run `app.R` to build the app and show interactive options.

## To do
* Don't allow selection of hunting regions 7A and/or 7B in combination with region 7
* Fix issue with dropdown menus incorrectly selecting data (possibly related to character vs. numeric selections)
* Add additional plots of timeseries data
* Use single legend, and format axes of graphs
* Add map to select regions graphically
* Add tab with regional differences for a given year
* Add tooltips to show data when you hover over a plot
* Improve aesthetics of layout
* Streamline code

## References
I used the following publicly-available data sets from the B.C. provincial government:
* Hunting kills (1976--2018): [[overview]](https://catalogue.data.gov.bc.ca/dataset/big-game-harvest-statistics-1976-2018) [[xlsx]](https://catalogue.data.gov.bc.ca/dataset/f2303645-5952-4766-bd5c-3b9b50dda1ca/resource/93daf681-ec55-4c7f-bfda-a621f67b5cea/download/big-game-harvest-statistics-1976-2018.xlsx)
  - I used the xlsx version of the dataset rather than the csv version, because the csv had half the header text cut off and does not have descriptions of field codes
  - There is a Shiny app for this data (which I didn't know about when starting mine) at https://kootenaywildlife.shinyapps.io/BCHarvestData_2018/ and an up-to-date version at https://kootenaywildlife.shinyapps.io/BCHarvestData/.  However it limits some regional analyses to the Kootenay region.
* Annual population of BC (1867--2019):  [[overview]](https://www2.gov.bc.ca/gov/content/data/statistics/people-population-community/population/population-estimates)  [[csv]](https://www2.gov.bc.ca/assets/gov/data/statistics/people-population-community/population/pop_bc_annual_estimates.csv)
* Hunting licence sales (2005--2020):  [[overview]](https://catalogue.data.gov.bc.ca/dataset/hunting-sales-statistics-2005-to-current) [[csv]](https://catalogue.data.gov.bc.ca/dataset/eeb0fd5a-36d6-41f2-be3d-568e03cbdd75/resource/2ed47a7b-1319-4efc-8ded-f88db46b2814/download/hunting-sales-statistics-05-06-to-current.csv)
  - Haven't incorporated this yet, but will use the csv version because the data was formatted in a friendlier way for analysis
* B.C. Wildlife management units:  [overview and link to custom download](https://catalogue.data.gov.bc.ca/dataset/wildlife-management-units)
  - I selected the entire province and chose geoJSON format

There is some really interesting (although a bit outdated) background information on how hunting quotas are set in the 2007--2011 [Kootenay Wildlife Harvest Advisory Committee meeting minutes](http://www.env.gov.bc.ca/kootenay/wld/kwhac.html).

## Author
Created by Heather More
#install.packages("pacman")
install.packages("rgeos", repos = "http://R-Forge.R-project.org")
pacman::p_load(
  tidyverse,
  shiny,
  shinydashboard,
  countrycode,
  rnaturalearth,
  rnaturalearthdata,
  # rgeos,
  scales,
  writexl
)


# Setup -------------------------------------------------------------------

rm=list(ls())

library(tidyverse)

iNaturalist <- read.csv("iNaturalist_data.csv",
                        na.strings = "")
gov_data <- read.csv("UK_wild_birds_datasheet_2023_condensed.csv",
                     na.strings = "na")



# Setup -------------------------------------------------------------------

rm=list(ls())

library(tidyverse)

# iNaturalist data tidying ------------------------------------------------

iNaturalist <- read.csv("iNaturalist_data.csv",
                        na.strings = "") %>% 
  mutate(id = 
           as.factor(id),
         time_zone =
           as.factor(time_zone),
         user_id =
           as.factor(user_id),
         user_name = 
           as.factor(user_name),
         quality_grade = 
           as.factor(quality_grade),
         species_guess = 
           as.factor(species_guess),
         scientific_name =
           as.factor(scientific_name),
         common_name =
           as.factor(common_name),
         iconic_taxon_name = 
           as.factor(iconic_taxon_name),
         taxon_id = 
           as.factor(taxon_id)
         )

str(iNaturalist)
head(iNaturalist)

# gov.uk data tidying -----------------------------------------------------

gov_data <- read.csv("UK_wild_birds_datasheet_2023_condensed.csv",
                     na.strings = "na") %>% 
  separate_wider_delim(species,
                       delim = "(",
                       names = c("common_name", "scientific_name")) %>% 
  mutate(scientific_name = 
           gsub(")",
                "", 
                scientific_name),
         scientific_name = 
           as.factor(scientific_name),
         common_name = 
           as.factor(common_name),
         long_term_time_period = 
           as.factor(long_term_time_period),
         long_term_trend = 
           as.factor(long_term_trend),
         short_term_trend =
           as.factor(short_term_trend),
         habitat =
           as.factor(habitat)
         )

str(gov_data)
head(gov_data)

# Combining the datasets --------------------------------------------------

unique(gov_data$scientific_name) # 161 unique species
unique(iNaturalist$scientific_name) # 112 unique species

# merging the datasets:
merged_data <- merge(gov_data,
                     iNaturalist,
                     by = 'scientific_name',
                     all = TRUE)

# Species list + what datasets its in:

species_presence <- merged_data %>% 
  select(scientific_name, common_name.x, common_name.y)

sum(is.na(species_presence$common_name.x)) # 335 species missing from iNaturalist data
sum(is.na(species_presence$common_name.y)) # 137 species missing from gov.uk data
sum(is.na(gov_data$common_name))
sum(is.na(iNaturalist$common_name)) # no blanks in original datasets, therefore issues with overlap.




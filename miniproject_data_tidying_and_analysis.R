
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
                     all = TRUE)  %>% 
  mutate(common_name_gov = 
           common_name.x,
         common_name_iNat =
           common_name.y) %>% 
  select(!c(common_name.x, common_name.y))

# Species list + what datasets its in:

species_presence <- merged_data %>% 
  select(scientific_name, common_name_gov, common_name_iNat)

sum(is.na(species_presence$common_name_gov)) # 335 species missing from iNaturalist data
sum(is.na(species_presence$common_name_iNat)) # 137 species missing from gov.uk data
sum(is.na(gov_data$common_name))
sum(is.na(iNaturalist$common_name)) # no blanks in original datasets, therefore issues with overlap.

species_presence <- species_presence %>% 
  mutate(present_gov =
           ifelse(
             !is.na(common_name_gov),
             "yes",
             "no"
           ),
         present_iNat =
           ifelse(
             !is.na(common_name_iNat),
             "yes",
             "no"
           )) %>% 
  select(scientific_name, present_gov, present_iNat) %>% 
  distinct() %>% 
  mutate(present_both = 
           ifelse(
             present_gov == "yes" & present_iNat == "yes",
             "yes",
             "no"
           ))

sum(species_presence$present_both == "yes") # 56 species in both datasets

# creating a final species list

final_species_list <- species_presence %>% 
  filter(present_both == "yes")

final_species_list <- as.vector(final_species_list$scientific_name)

# removing species from merged data not in both datasets

###(try creating new yes/no column and removing no???)
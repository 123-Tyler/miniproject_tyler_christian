
# Setup -------------------------------------------------------------------

rm=list(ls())

library(tidyverse)
library(vegan)
library(datawizard)

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

# iNaturalist data tidying ------------------------------------------------

# (data from 2000-2013)

sp_richness <- read.csv("Species_richness_2000.csv") %>% 
  select(Location, Bird) %>% 
  drop_na(Bird) %>% 
  mutate(Bird = standardise(Bird))

colnames(sp_richness) <- c("location", "richness_official")

# calculated sp richness via the Frescalo method (Sparta package in R)
# each location represents an environmental zone (1 km^2)

iNat_new <- read.csv("iNaturalist_data_final.csv",
                     na.strings = "") %>% 
  filter(iconic_taxon_name == "Aves") %>% 
  mutate(year =         # creating a year uploaded column
           substr(observed_on, 
                  1,
                  4),
         year =
           as.factor(year))

iNat_new_sp_count <- iNat_new %>% 
  filter(year %in% 2000:2013) %>% 
  mutate(scientific_name = as.factor(scientific_name),
         count = 1) %>% 
  group_by(place_county_name, scientific_name) %>% 
  reframe(place_county_name = place_county_name,
          scientific_name = scientific_name,
          count = sum(count)) %>% 
  drop_na(place_county_name) %>% 
  distinct()

county_area <- read.csv("UK_county_area.csv") %>% 
  select(ctyua19nm, Shape__Area)

colnames(county_area) <- c("place_county_name", "area")

iNat_new_sp_richness <- merge(iNat_new_sp_count, 
                              county_area, 
                              by = "place_county_name", 
                              all = TRUE) %>% 
  drop_na(scientific_name) %>% 
  drop_na(area) %>% 
  pivot_wider(names_from = scientific_name,
              values_from = count) %>% 
  replace(is.na(.), 0) %>% 
  mutate(total_species = rowSums(.[4:605])) %>% 
  mutate(richness = standardise(total_species/area)) %>%  # calculate species richness and z-standardise
  select(c(place_county_name, richness))

colnames(iNat_new_sp_richness) <- c("location", "richness_iNat")

# richness = total species/total area

# Comparing species richness ----------------------------------------------

species_richness <- merge(iNat_new_sp_richness,
                          sp_richness, 
                          all = TRUE) %>% 
  pivot_longer(cols = !location, names_to = "dataset", values_to = "richness") %>% 
  drop_na(richness) %>% 
  filter(richness<5) # removes Westminster as extremely high (therefore outlier)

hist(species_richness$richness) # normal distribution

species_richness %>% 
  ggplot(aes(x = dataset, 
             y = richness)) +
  geom_boxplot()

chisq.test(species_richness$richness,species_richness$dataset) 
# therefore no significant differences between species richness, iNaturalist is reliable.

# Observer accuracy of iNaturalist -----------------------------------------

iNat_edited <- iNat_new %>% 
  mutate(image_url_present = 
           ifelse(
             is.na(image_url),
             "No",
             "Yes"
           ),
         sound_url_present = 
           ifelse(
             is.na(sound_url),
             "No",
             "Yes"
           ),
         description_present = 
           ifelse(
             is.na(description),
             "No",
             "Yes"
           ),
         species_guess_same_common =
           ifelse(species_guess == common_name,
                  "Yes",
                  "no"),
         id = 
           as.factor(id),
         user_id =
           as.factor(user_id),
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
           as.factor(taxon_id),
         taxon_family_name = 
           as.factor(taxon_family_name)) %>% 
  select(!c(
    observed_on,
    iconic_taxon_name,
    taxon_id
  ))

iNat_sample <- iNat_edited %>% 
  sample_n(100) %>% 
  mutate(tyler_able_to_ID =
           "",
         tyler_ID =
           "",)

save(iNat_sample,file="sample_data.csv")
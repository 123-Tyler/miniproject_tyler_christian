
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

iNat_new <- read.csv("iNaturalist_data.csv",
                     na.strings = "") %>% 
  filter(iconic_taxon_name == "Aves") %>% 
  mutate(year =         # creating a year uploaded column
           substr(observed_on, 
                  1,
                  4),
         year =
           as.factor(year))

# Species richness data ---------------------------------------------------

# (data from 2000-2013)

sp_richness <- read.csv("Species_richness_2000.csv") %>% 
  select(Location, Bird) %>% 
  drop_na(Bird) %>% 
  mutate(Bird = standardise(Bird))

colnames(sp_richness) <- c("location", "richness_official")

# calculated sp richness via the Frescalo method (Sparta package in R)
# each location represents an environmental zone (1 km^2)

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
  filter(richness < 5) # removes Westminster as extremely high (therefore outlier)

hist(species_richness$richness) # normal distribution

species_richness %>% 
  ggplot(aes(x = dataset, 
             y = richness)) +
  geom_boxplot()

chisq.test(species_richness$richness,species_richness$dataset) 
# therefore no significant differences between species richness, iNaturalist is reliable.

# Observer accuracy of iNaturalist -----------------------------------------

# 0 = No, 1 = Yes

iNat_edited <- iNat_new %>% 
  mutate(image_url_present = 
           ifelse(
             is.na(image_url),
             0,
             1
           ),
         sound_url_present = 
           ifelse(
             is.na(sound_url),
             0,
             1
           ),
         description_present = 
           ifelse(
             is.na(description),
             0,
             1
           ),
         species_guess_same_common =
           ifelse(species_guess == common_name,
                  1,
                  0),
         has_coordinates = 
           ifelse(
             is.na(longitude),
                  0,
                  ifelse(
                    is.na(latitude),
                    0,
                    1
                  )
             ),
         id = 
           as.factor(id),
         user_id =
           as.factor(user_id),
         year = 
           as.factor(year),
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
           as.factor(taxon_family_name),
         place_county_name =
           as.factor(place_county_name),
         captive_cultivated =
           as.factor(captive_cultivated)) %>% 
  select(!observed_on)

user_obervations <- iNat_edited %>% 
  select(user_id) %>% 
  mutate(total_id = 
           1) %>%
  group_by(user_id) %>% 
  reframe(user_id = 
            user_id,
            total_id = 
            sum(total_id)) %>% 
  distinct()

iNat_edited <- merge(iNat_edited, user_obervations, all = TRUE)

str(iNat_edited)

# Sample data to test -----------------------------------------------------

summary(iNat_edited$quality_grade)

iNat_sample_needs_id <- iNat_edited %>% 
  filter(quality_grade == "needs_id") %>% 
  sample_n(40) # boosted slightly to provide decant sample size

iNat_sample_casual <- iNat_edited %>% 
  filter(quality_grade == "casual") %>% 
  sample_n(40)

iNat_sample_research <- iNat_edited %>% 
  filter(quality_grade == "research") %>% 
  sample_n(40)

iNat_sample <- rbind(
  iNat_sample_needs_id,
  iNat_sample_casual, 
  iNat_sample_research) %>%
  mutate(tyler_ID =
           "",
         tyler_confidence =
           "",
         tyler_comment =
           "")

# write.csv(iNat_sample, file = "sample_data.csv")

iNat_sample_commented <- read.csv("sample_data_with_comments.csv")

str(iNat_sample_commented)

# GLM ---------------------------------------------------------------------


# Plots -------------------------------------------------------------------



### to do:
# use new data
# ID random sample of 100
# create accuracy quantifier
# test quantifier against my IDs
# model change in accuracy over time (+ location???)
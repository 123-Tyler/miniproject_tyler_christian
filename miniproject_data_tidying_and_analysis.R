
# Setup -------------------------------------------------------------------

rm=list(ls())

library(tidyverse)
library(vegan)
library(datawizard)

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

# Species list + what datasets its in:

species_presence <- merge(gov_data,
                          iNaturalist,
                          by = 'scientific_name',
                          all = TRUE)  %>% 
  mutate(common_name_gov = 
           common_name.x,
         common_name_iNat =
           common_name.y) %>% 
  select(!c(common_name.x, common_name.y)) %>% 
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

# Filtering the original datasets for species present in both:

iNaturalist_both <- iNaturalist %>% 
  mutate(present_both =          # filtering for species in both
           ifelse(scientific_name %in% final_species_list,
                  "yes",
                  "no")) %>% 
  filter(present_both ==
           "yes") %>% 
  mutate(year_uploaded =         # creating a year uploaded column
           substr(observed_on, 
                  7,
                  10),
         year_uploaded =
           as.factor(year_uploaded)) %>% 
  select(                        # filtering for useful columns
    id,
    user_id,
    quality_grade,
    url,
    image_url,
    sound_url,
    tag_list,
    description,
    num_identification_agreements,
    num_identification_disagreements,
    captive_cultivated,
    place_guess,
    longitude,
    latitude,
    species_guess,
    scientific_name,
    common_name,
    iconic_taxon_name,
    taxon_id,
    year_uploaded
  )

gov_data_both <- gov_data %>% 
  mutate(present_both =          # filtering for species in both
           ifelse(scientific_name %in% final_species_list,
                  "yes",
                  "no")) %>% 
  filter(present_both == "yes") 

# calculating % change in iNaturalist -------------------------------------

unique(iNaturalist_both$year_uploaded)
plot(iNaturalist_both$year_uploaded)  # dates from 1988 to 2023
# gov_data has two categories (1970-2021 and 2016-2021), therefore only calculate short-term % change.

length(iNaturalist_both)/length(unique(iNaturalist_both$scientific_name)) # not many repeated observations of species

gov_data_both <- gov_data_both %>% 
  select(!c(long_term_time_period:long_term_trend)) # remove long term data as dates don't match iNaturalist



###########
###########
#########
##########
##########
###########
###########
############


# new data ----------------------------------------------------------------

# data from 2000-2013

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
         tag_list_present = 
           ifelse(
             is.na(tag_list),
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
                  "no")) %>% 
  select(!c(
    observed_on,
    place_guess:place_county_name,
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
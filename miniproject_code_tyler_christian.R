
# Setup -------------------------------------------------------------------

### Use if required:
# setwd() 

rm = list(ls())

# required packages:
library(tidyverse) # data tidying + ggplot
library(datawizard) # z-standardize function
library(lme4) # linear mixed models
library(lmtest) # comparing linear mixed models
library(sjPlot) # creates results table+plot for models

# Data references ---------------------------------------------------------

# Dyer, R. and Oliver, T. (2016). Estimated species richness data used in study of UK Ecological status. NERC Environmental Information Data Centre. https://doi.org/10.5285/6c535793-034d-4c4f-8a00-497315e7d689

# iNaturalist community (2024). Observations of birds from UK observed between 01/01/2000 - 01/01/2021. iNaturalist. https://www.inaturalist.org

# ONS Geography (2019). Counties and Unitary Authorities (December 2019) Boundaries UK BUC. Office For National Statistics. https://geoportal.statistics.gov.uk/datasets/bbf13c40ca8f4931afc0a46d079af58c_0/about 

# iNaturalist data tidying ------------------------------------------------

iNat_new <- read.csv("iNaturalist_data.csv",
                     na.strings = "") %>%
  filter(iconic_taxon_name == "Aves") %>%
  mutate(year =         # creating a year uploaded column (removes days and months)
           substr(observed_on,
                  7,
                  10),
         year =
           as.factor(year)) %>%  # originally numeric, this turns it into categorical data
  select(!observed_on)

# Species richness data ---------------------------------------------------

# (data from 2000-2013)

sp_richness <- read.csv("Species_richness_2000.csv") %>%
  select(Location, Bird) %>% # removes unwanted taxa
  drop_na(Bird) %>%
  mutate(Bird = standardise(Bird)) # z-standardizes species richness as a different metric is used to the iNaturalist data

colnames(sp_richness) <- c("location", 
                           "richness_official")

# calculated sp richness via the Frescalo method (Sparta package in R)
# each location represents an environmental zone (1 km^2)

iNat_new_sp_count <- iNat_new %>%
  filter(year %in% 2000:2013) %>%  # to match official dataset
  mutate(scientific_name = 
           as.factor(scientific_name),
         count = 
           1) %>%
  group_by(place_county_name, 
           scientific_name) %>%
  reframe(
    place_county_name = 
      place_county_name,
    scientific_name = 
      scientific_name,
    count = sum(count) # to get total species in each region
  ) %>%
  drop_na(place_county_name) %>%
  distinct()

county_area <- read.csv("UK_county_area.csv") %>%
  select(ctyua19nm,  # county names
         Shape__Area) # area (in miles^2 of the counties)

colnames(county_area) <- c("place_county_name",
                           "area")

iNat_new_sp_richness <- merge(iNat_new_sp_count, # similar to cbind()
                              county_area,
                              by = "place_county_name",
                              all = TRUE) %>% # prevents un-matched variables from being dropped
  drop_na(scientific_name) %>%
  drop_na(area) %>%
  pivot_wider(names_from =  # makes it easier to calculate species richness by creating a column per species
                scientific_name,
              values_from =
                count) %>%
  replace(is.na(.), 
          0) %>%  # assumes that a species richness of NA is zero
  mutate(total_species = 
           rowSums(.[4:605]),
         richness = 
           standardise(total_species / area)) %>%  # calculate species richness and z-standardize as before
  select(c(place_county_name, 
           richness))

colnames(iNat_new_sp_richness) <- c("location", 
                                    "richness_iNat")

# richness = total species/total area

# Comparing species richness ----------------------------------------------

species_richness <- merge(iNat_new_sp_richness, 
                          sp_richness,
                          all = TRUE) %>%
  pivot_longer(cols = !location, # removes location names as they are unmatched between the datasets (in essence randomising them)
               names_to = "dataset",
               values_to = "richness") %>%
  drop_na(richness) %>%
  filter(richness < 5) # removes Westminster from iNaturalist as extremely high (therefore outlier)

hist(species_richness$richness) # normal distribution

species_richness %>%
  ggplot(aes(x = dataset,
             y = richness)) +
  geom_boxplot()

summary(species_richness$richness[species_richness$dataset == "richness_iNat"])
summary(species_richness$richness[species_richness$dataset == "richness_official"])

# the official data has a larger range but means are similar

chisq.test(species_richness$richness, species_richness$dataset)

# no significant differences between species richness, and there is a decent goodness of fit.

# therefore this suggests the iNaturalist is somewhat similar to the official data but should be taken with a pinch of salt.

# Observer accuracy of iNaturalist -----------------------------------------

# 0 = No, 1 = Yes  (for binomal variables)

iNat_edited <- iNat_new %>% # (no longer filtered for 2000-1013, all years present)
  mutate(                   # creating binomial data for the key observation qualities.
    image_url_present =   # observation contains an image
      ifelse(is.na(image_url),
             0,
             1),
    sound_url_present =   # observation contains an audio recording (i.e. bird call)
      ifelse(is.na(sound_url),
             0,
             1),
    description_present =   # observation contains an description
      ifelse(is.na(description),
             0,
             1),
    species_guess_same_common =   # species guess same as iNat algorithm
      ifelse(species_guess == common_name,
             1,
             0),
    id =            # tidying the variable types for the dataset
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
      as.factor(captive_cultivated)
  ) 

user_obervations <- iNat_edited %>% # code to count the number of bird observations per user (ignores other taxa as not in dataset)
  select(user_id) %>%
  mutate(total_id =
           1) %>%
  group_by(user_id) %>%
  reframe(user_id =
            user_id,
          total_id =
            sum(total_id)) %>%
  distinct()

iNat_edited <- merge(iNat_edited,  # adds user observations to the data
                     user_obervations,
                     all = TRUE) 

str(iNat_edited)

# Sample data to test -----------------------------------------------------

### There is no need to run this code every time, its just to create the random data which I used to confirm the observation reliability

summary(iNat_edited$quality_grade)

# taking 40 random observations from each quality grade and merging them into ta single dataset:
iNat_sample_needs_id <- iNat_edited %>%
  filter(quality_grade == "needs_id") %>%
  sample_n(40)

iNat_sample_casual <- iNat_edited %>%
  filter(quality_grade == "casual") %>%
  sample_n(40)

iNat_sample_research <- iNat_edited %>%
  filter(quality_grade == "research") %>%
  sample_n(40)

iNat_sample <- rbind(iNat_sample_needs_id,
                     iNat_sample_casual,
                     iNat_sample_research) %>%
  mutate( # (I will fill these out in excel and then reload them into R)
    tyler_ID = # species which I think it is (using Collins Bird ID guide)
      "",
    tyler_comment = # any additional comments about data quality
      ""
  )

# (hash-tagged to avoid new random data from overriding the original sample)

# write.csv(iNat_sample, file = "sample_data.csv")

# Tyler observation data --------------------------------------------------


iNat_sample_commented <- 
  read.csv("sample_data_with_comments.csv") %>% # from the random sample, I identified the species in each observation to test their accuracy
  mutate(tyler_match_id =
           ifelse(tyler_ID == scientific_name, # making binomial (i.e. did my ID match the original)
                  1,
                  0)
  )

str(iNat_sample_commented)

iNat_accuracy <- iNat_sample_commented %>%
  select(quality_grade, 
         tyler_match_id) %>%
  group_by(quality_grade) %>%
  reframe(
    quality_grade = 
      quality_grade,
    percentage_correct =
      sum(tyler_match_id) / 40 # calculating the mean success of original Vs my identifications (assuming I'm 100% correct) per quality grade
  ) %>%
  distinct()

# Casual: 10.0%
# Needs ID: 22.5%
# Research: 97.5%

iNat_final <- merge(iNat_edited, # adding the success rate into the original data
                    iNat_accuracy, 
                    all = TRUE)

# Tidying final data before stats -----------------------------------------

str(iNat_final)

unique(iNat_final$species_guess_same_common) # some NA's.

iNat_final$species_guess_same_common[is.na(iNat_final$species_guess_same_common)] <- 0 # replaces NA with 0

iNat_final <- iNat_final %>% 
  mutate(agreement_rate = # combines number of ID agreements and dissagreements (thus preventing multicollinearity)
           num_identification_agreements / (num_identification_agreements+num_identification_disagreements),
         agreement_rate =
           ifelse(agreement_rate == "NaN", # Replaces NaN with 0 (not showing as NA for some reason)
                  0,
                  agreement_rate),
         image_url_present =
           as.factor(image_url_present),
         sound_url_present =
           as.factor(sound_url_present),
         description_present =
           as.factor(description_present),
         species_guess_same_common =
           as.factor(species_guess_same_common),
         percentage_correct = 
           as.numeric(percentage_correct)) %>% 
  ungroup() # removes any grouping from dataset as this messes with stats tests

str(iNat_final) 
head(iNat_final) # all looks good

# Stats ---------------------------------------------------------------------

model_1 <- lmer(
  percentage_correct ~
    agreement_rate + 
    image_url_present *
    sound_url_present * 
    description_present +
    species_guess_same_common +
    total_id +
    (1 | year) +
    (1 | place_county_name),
  data = iNat_final)

summary(model_1) # Therfore needs to be z-standardized as some variables are of different sizes

# Adds z-standardization to large-value variables:

model_2 <- lmer(
  percentage_correct ~
    standardise(agreement_rate) + 
    species_guess_same_common +
    standardise(total_id) +
    image_url_present *
    sound_url_present * 
    description_present +
    (1 | year) +
    (1 | place_county_name),
  data = iNat_final)

summary(model_2) 

# Removes interactive effects:

model_3 <- lmer(
  percentage_correct ~
    standardise(agreement_rate) + 
    image_url_present +
    sound_url_present +
    description_present +
    species_guess_same_common +
    standardise(total_id) +
    (1 | year) +
    (1 | place_county_name),
  data = iNat_final)

summary(model_3) 

lrtest(model_2, model_3) # interaction effect between image, sound and description is better

# description as additive, image and sound as interactive:

model_5 <- lmer(
  percentage_correct ~
    standardise(agreement_rate) + 
    captive_cultivated + 
    image_url_present *
    sound_url_present +
    description_present +
    species_guess_same_common +
    standardise(total_id) +
    year +
    (1 | place_county_name),
  data = iNat_final)

lrtest(model_2, model_5) # model 2 is still best

###### Use model 2 as final

tab_model(model_2)

plot_model(model_2, 
           show.values = TRUE, # Displays estimate values above lines
           show.p = TRUE) + # Displays p-values as stars
  theme_bw() 

# Plots -------------------------------------------------------------------

year_data <- iNat_final %>% 
  group_by(year,
           quality_grade,
           percentage_correct,
           agreement_rate) %>% 
  count(quality_grade) %>%    # total of each quality_grade per year
  group_by(year, quality_grade) %>% 
  reframe(year = year,
          quality_grade = quality_grade,
          percentage_correct = percentage_correct,
          agreement_rate = mean(agreement_rate),
          n = mean(n)) %>% 
  distinct()

str(year_data)

# missing needs_id for 2000 and 2001, add these manually with values of 0:

year_data[nrow(year_data) + 1,] <- list("2000",   # year is quoted because it is a factor
                                        "needs_id",
                                        0.225,
                                        0, # assumed aggreement rate and n to be 0 due to lack of data
                                        0)

year_data[nrow(year_data) + 1,] <- list("2001",
                                        "needs_id",
                                        0.225, 
                                        0,
                                        0)

total_observations <- sum(year_data$n) # total number of observations

year_data <- year_data %>% 
  mutate(standerdised_quality_per_year = # proportion of accurate observations divided by the total (per year, per quality grade)
           (n*percentage_correct*agreement_rate)/total_observations) %>% 
  mutate(quality_grade =
           as.factor(quality_grade),
         quality_grade =  # nicer-looking variable names for plot
           gsub("casual", 
                "Casual", 
                quality_grade),
         quality_grade = 
           gsub("needs_id", 
                "Needs ID", 
                quality_grade),
         quality_grade = 
           gsub("research", 
                "Research", 
                quality_grade)
  )

colour_scheme <- c("forestgreen", # setting a colour scheme for the graph
                   "firebrick1",
                   "royalblue3")

year_plot <- year_data %>%
  group_by(quality_grade) %>% 
  ggplot(aes(x = year,
             y = standerdised_quality_per_year,
             group = quality_grade,
             color = quality_grade)) +
  geom_point() +
  geom_smooth(linewidth = 0.1,
              method = "gam",  # provides the best fit (practically exponential)
              fill = "lightgrey") +
  geom_vline(xintercept = 9, 
             linetype = "dashed", 
             color = "black",
             linewidth = 0.5) +
  geom_vline(xintercept = 17, 
             linetype = "dashed", 
             color = "black",
             linewidth = 0.5) +
  geom_label(  # Writes the equation for standardized quality on the graph
    label = "                                                 Total Observations x Accuracy x Proportion Identification Agreements
Standardised Anual Quality = --------------------------------------------------------------------------------------------
                                                    Total Observations",
    x = 7.5, # position of the label
    y = 0.125,
    label.padding = unit(0.55, 
                         "lines"), # Rectangle size around label
    label.size = 0.35,
    size = 2,
    color = "black",
    fill = "white"
  ) +
  ylab("Standardised Anual Quality") +
  xlab("Year") +
  guides(color =   # changes legend title
           guide_legend(title = 
                          "Quality Grade:")) + 
  scale_color_manual(values =   # changes line and point colours
                       colour_scheme) +
  theme_bw() + 
  theme(legend.position = "bottom",
        axis.text.x = element_text(angle = 90,
                                   vjust = 0.5, 
                                   hjust = 1))
year_plot # final plot

ggsave("final_plot.jpeg", # saves the final plot as .jpeg
       width = 15.92, # setting the plot size to fit into Microsoft Word without editing
       height = 11,
       units = "cm")


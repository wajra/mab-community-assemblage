# In this file we'll perform the full analysis on the
# geofiltered values for the Northern Atlantic stocks of
# black sea bass, summer flounder, and scup
# Written for R Version 3.6.1


library(tidyverse)
library(MASS)
library(lattice)

# Read in the data
sp_obs_data <- read.csv("data/all_species_mid_atlantic_bight.csv")

# Re-check to see if the data is structured as required
# How many unique species are in the dataset?
print(unique(sp_obs_data$sppocean))
# The answer is 3

# Assigning seasons to the observations based on the month
sp_obs_data[, "season"] <- NA
# February to June is Spring
sp_obs_data$season[sp_obs_data$month %in% c(2,3,4,5)] <- 'Spring'
# June to September is Summer
sp_obs_data$season[sp_obs_data$month %in% c(6,7,8)] <- 'Summer'
# September to January is Fall
sp_obs_data$season[sp_obs_data$month %in% c(9,10,11,12)] <- 'Fall'

# Assigning common names to the species
# First an empty column
sp_obs_data[, "spp_name"] <- NA
sp_obs_data$spp_name[sp_obs_data$sppocean == 'centropristis striata_Atl'] <- 'black sea bass'
sp_obs_data$spp_name[sp_obs_data$sppocean == 'paralichthys dentatus_Atl'] <- 'summer flounder'
sp_obs_data$spp_name[sp_obs_data$sppocean == 'stenotomus chrysops_Atl'] <- 'scup'
sp_obs_data$spp_name[sp_obs_data$sppocean == 'urophycis chuss_Atl'] <- 'red hake'
sp_obs_data$spp_name[sp_obs_data$sppocean == 'urophycis regia_Atl'] <- 'spotted hake'

# We must first run a Bartlett's test for this

# First let's begin discriminator analysis for species between seasons
# LDA formula for seasons
seasons_lda_formula <- formula(season ~ SBT.seasonal + SST.seasonal.mean + SBT.min + SBT.max + SST.max + rugosity + GRAINSIZE)

# It seems that the discriminant functions are doing a good job of discriminating between seasons
# for individual species

# Now let's see if they can discrminate between the species in a single season
# My guess is probably not, but we'll see
# Formula for species
species_lda_formula <- formula(spp_name ~ SBT.seasonal + SST.seasonal.mean + SBT.min + SBT.max + SST.max + rugosity + GRAINSIZE)
species_lda_formula_2 <- formula(spp_name ~ rugosity + GRAINSIZE)

# First let's go for fall
# Filter the season
sp_obs_fall <- sp_obs_data %>% filter(season=='Fall')
# Derive the discriminant functions
sp_lda_fall <- lda(species_lda_formula, data = sp_obs_fall)
# Apply the discriminant functions to the data to get DF scores
lda_values_fall <- predict(sp_lda_fall)

lda_df_fall <- data.frame(lda_values_fall$x[,1], lda_values_fall$x[,2], sp_obs_fall$spp_name)
# Rename the columns
columns <- c("LD1", "LD2","spp_name")
colnames(lda_df_fall) <- columns

# Plot the discriminant functions
ggplot(lda_df_fall, aes(x=LD1, y=LD2)) +
  geom_point(aes(color = factor(spp_name)), show.legend=TRUE) +
  xlab("DF1 (81.06%)") + ylab("DF2 (18.52%)") +
  ggtitle("Fall") + theme(plot.title = element_text(hjust = 0.5))

ggsave("output/fall_lda.png")

# Correlation between df scores and variables
# cor(sp_obs_bsb[c("SST.seasonal.mean","SBT.seasonal","SBT.min","SBT.max","rugosity", "GRAINSIZE")], lda_values_bsb$x)

# Next let's go for spring
sp_obs_spring <- sp_obs_data %>% filter(season=='Spring')
# Derive the discriminant functions
sp_lda_spring <- lda(species_lda_formula, data = sp_obs_spring)
# Apply the discriminant functions to the data to get DF scores
lda_values_spring <- predict(sp_lda_spring)

lda_df_spring <- data.frame(lda_values_spring$x[,1], lda_values_spring$x[,2], sp_obs_spring$spp_name)
# Rename the columns
columns <- c("LD1", "LD2","spp_name")
colnames(lda_df_spring) <- columns

# Plot the discriminant functions
ggplot(lda_df_spring, aes(x=LD1, y=LD2)) +
  geom_point(aes(color = factor(spp_name)), show.legend=FALSE) +
  xlab("DF1 (76.14%)") + ylab("DF2 (16.43%)") +
  ggtitle("Spring") + theme(plot.title = element_text(hjust = 0.5), legend.position = 'right')

ggplot(lda_df_spring, aes(x=LD1, y=LD2)) +
  geom_point(aes(color = factor(spp_name)), show.legend=TRUE) +
  xlab("DF1 (76.14%)") + ylab("DF2 (16.43%)") +
  ggtitle("Spring") + 
  theme(plot.title = element_text(hjust = 0.5), legend.position = 'right')

ggsave("output/spring_lda.png")

# As can be seen from the plots, the fact that these species share similar habitats throughout the year
# is well reflected graphically through discrminant analysis

# Write out the CSV
write_csv(sp_obs_data, path="data/all_species_mid_atlantic_bight_output_common_names.csv")

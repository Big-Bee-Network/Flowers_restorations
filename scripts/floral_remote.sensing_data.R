# Aggregating Data 
setwd("C:/Users/navap/Downloads")
library(tidyverse)
library(dplyr)
library(sf)
library(r)
install.packages("r")
library(reader)
#read in CSV
res_ = read.csv("restorations_flowerRichness.csv")
sens_ = read.csv ("Remote-Sensing Data_C.csv")
# see columns
names(res_)
names(sens_)
# Select and rename columns for Remote-Sensing Data
sen <- sens_ %>%
  dplyr::select(
    site = Site,
    date = samplingdate,
    ndvi_mean, ndvi_min, ndvi_max,
    ndwi_mean, ndwi_min, ndwi_max,
    green_mean, green_min, green_max,
    blue_mean, blue_min, blue_max, red_min,
    red_mean, red_max,
    nir_mean, nir_min, nir_max
  )
#Select and rename columns for flower richness data
res <- res_ %>%
  dplyr::select(site, date, floral_abundance, floral_richness, floral_simpson)
#fix rows
print(res[,1])
print(sen[,1])
sen[,1]<-sub("greenhouse", "Greenhouse", sen[,1])
sen <- sen[sen$site != "Lagoon", ]
sen[,1]<-sub("New Lagoon Site", "Lagoon", sen[,1])
print(sen[,1])
#fix columns 
print(sen[,2])
print(res[,2])
# fix column dates
sen[,2] <- format(as.Date(sen[,2], format = "%Y-%m-%dT%H:%M:%SZ"), "%m/%d/%Y")
# Assuming res[,2] is a character vector
for (i in seq_along(res[,2])) {
  if (grepl("-", res[i,2])) {
    # Handling range format
    date_range <- unlist(strsplit(res[i,2], "/"))
    
    # Splitting the range into individual dates
    individual_dates <- seq(as.Date(date_range[1], format = "%Y-%m-%d"), 
                            as.Date(date_range[2], format = "%Y-%m-%d"), 
                            by = "days")
    
    formatted_dates <- format(individual_dates, "%m/%d/%Y")
    
    res[i,2] <- paste(formatted_dates, collapse = "/")
  } else {
    # Single date format
    res[i,2] <- format(as.Date(res[i,2], format = "%m/%d/%Y"), "%m/%d/%Y")
  }
}

# Display the updated res[,2] column
print(res[,2])



# Join the two data frames
merged_data <- merge(res, sen, by.x = c("site", "date"), by.y = c("site", "date"), all = TRUE)
# clean up NA values
for (i in 1:nrow(merged_data)) {
  if (i < nrow(merged_data)) {
    merged_data[i, is.na(merged_data[i, ])] <- merged_data[i + 1, is.na(merged_data[i, ])]
  }
}
#Cleaning up row duplicates 
rows_to_remove <- c(15, 17, 7)
merged_data <- merged_data[-rows_to_remove, ]

# save as csv for record & data frame
write.csv(merged_data, file = "merged_data.csv")
saveRDS(merged_data, file = "merged_data.RData")
loaded_data <- readRDS("merged_data.RData")


#========
#Plot variables against each other
#NDVI
library(ggplot2)
#floral abundance & NDVI for all locations
ggplot(merged_data, aes(x = floral_abundance, y = ndvi_mean)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(x = "Floral Abundance", y = "NDVI Mean")
# red
ggplot(merged_data, aes(x = floral_abundance, y = red_mean)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(x = "Floral Abundance", y = "Red Mean")
# Blue
ggplot(merged_data, aes(x = floral_abundance, y = blue_mean)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(x = "Floral Abundance", y = "Blue Mean")
# green
ggplot(merged_data, aes(x = floral_abundance, y = green_mean)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(x = "Floral Abundance", y = "Green Mean")

#greenmin & floral abundance
ggplot(merged_data, aes(x = floral_abundance, y = green_min)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(x = "Floral Abundance", y = "Green Minimum")
#greenmax & floral
ggplot(merged_data, aes(x = floral_abundance, y = green_max)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(x = "Floral Abundance", y = "Green Maximum")
#bluemax & floral
ggplot(merged_data, aes(x = floral_abundance, y = blue_max)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(x = "Floral Abundance", y = "Blue Maximum")
#bluemin & floral
ggplot(merged_data, aes(x = floral_abundance, y = blue_min)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(x = "Floral Abundance", y = "Blue Minmum")
#redmin & floral
ggplot(merged_data, aes(x = floral_abundance, y = red_min)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(x = "Floral Abundance", y = "Red Minmum")


# Create a plot for the specified site (carp)
site_data <- all_variables %>%
  filter(site == "Carp Salt Marsh")

# Create a plot for the specified site
ggplot(site_data, aes(x = floral_abundance, y = nir_mean)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(x = "Floral Abundance", y = "NIR Mean") +
  ggtitle("Scatter Plot for Carp Salt Marsh") +
  theme_minimal()
#floral abundance & NDWI for all locations
ggplot(all_variables, aes(x = floral_abundance, y = ndwi_mean)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(x = "Floral Abundance", y = "NDWI Mean")
ggplot(loaded_data, aes(x = floral_abundance, y = date)) +
  geom_point() + geom_smooth(method = "lm", se = FALSE) +
  labs(x = "Floral Abundance", y = "Date")




# random forest model
#install random forest package & caret
install.packages("randomForest")
install.packages("caret")
library(randomForest)
library(caret)
# fix data so only floralabundance column shows
columns_to_remove <- c('floral_richness', 'floral_simpson')
# date with onlay floral abundance
merged_data <- loaded_data[, !names(loaded_data) %in% columns_to_remove]
train_index <- createDataPartition(y=merged_data$floral_abundance,)
# find impratnce of variables
rf_model <- randomForest(floral_abundance ~ ., data = merged_data)
print(importance(rf_model))
#Create a training set using createDataPartition
set.seed(123)  # Set a seed for reproducibility
train_index <- createDataPartition(y = merged_data$floral_abundance, p = 0.7, list = FALSE)
train_data <- merged_data[train_index, ]



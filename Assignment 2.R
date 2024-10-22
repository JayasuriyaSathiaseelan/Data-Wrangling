# Load necessary libraries
# Install necessary packages if not already installed
if (!require("dplyr")) install.packages("dplyr", dependencies = TRUE)
if (!require("readr")) install.packages("readr", dependencies = TRUE)
library(dplyr)
library(readr)
# Read the dataset
listings <- read_csv("/Users/jayasuriyasathiaseelan/Documents/Data wrangling assignment 2 database/listings.csv")

# Select relevant columns and convert numeric columns to appropriate types
selected_columns <- listings %>%
  select(id, name, description, host_name, neighbourhood_cleansed, property_type, room_type,
         accommodates, bathrooms_text, bedrooms, beds, amenities, price, number_of_reviews,
         number_of_reviews_ltm, review_scores_rating, review_scores_accuracy, review_scores_cleanliness,
         review_scores_checkin, review_scores_communication, review_scores_location, review_scores_value, host_is_superhost)

# Convert relevant columns to numeric
selected_columns <- selected_columns %>%
  mutate(across(c(accommodates, bedrooms, beds, number_of_reviews, number_of_reviews_ltm,
                  review_scores_rating, review_scores_accuracy, review_scores_cleanliness,
                  review_scores_checkin, review_scores_communication, review_scores_location,
                  review_scores_value), as.numeric)) %>%
  mutate(price = as.numeric(gsub("[$,]", "", price)))  # Remove $ and , from price and convert to numeric

# Display summary of the dataframe
summary(selected_columns)

# Count listings with 'Airport' in their name (case insensitive)
airport_listings <- selected_columns %>%
  filter(grepl("airport", name, ignore.case = TRUE))

num_airport_listings <- nrow(airport_listings)
num_airport_listings
# Count listings with 'Airport' and 'CBD' in their name (case insensitive)
airport_cbd_listings <- selected_columns %>%
  filter(grepl("airport", name, ignore.case = TRUE) & grepl("cbd", name, ignore.case = TRUE))

num_airport_cbd_listings <- nrow(airport_cbd_listings)
num_airport_cbd_listings
# Find top-ten neighbourhoods with the highest number of reviews
top_neighbourhoods <- selected_columns %>%
  group_by(neighbourhood_cleansed) %>%
  summarize(total_reviews = sum(number_of_reviews, na.rm = TRUE),
            average_rating = mean(review_scores_rating, na.rm = TRUE)) %>%
  arrange(desc(total_reviews)) %>%
  head(10)

top_neighbourhoods
# Compare average review ratings and review counts between superhosts and non-superhosts
superhost_comparison <- selected_columns %>%
  group_by(host_is_superhost) %>%
  summarize(
    average_rating = mean(review_scores_rating, na.rm = TRUE),
    review_count = mean(number_of_reviews, na.rm = TRUE)
  )

superhost_comparison
# Filter for Wynnum and calculate average price and average review score rating for each property type
wynnum_stats <- selected_columns %>%
  filter(neighbourhood_cleansed == "Wynnum") %>%
  group_by(property_type) %>%
  summarize(
    average_price = mean(price, na.rm = TRUE),
    average_review_rating = mean(review_scores_rating, na.rm = TRUE)
  )

wynnum_stats

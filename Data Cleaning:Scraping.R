library(arrow)
library(dplyr)
library(tidyverse)
library(httr)
library(jsonlite)
library(ggplot2)
library(rvest)
library(RSelenium)

#Intitial Dataset
#swing<-read_feather("/Users/reesemullen/Desktop/Statistical Practice/ASA-2025-Challenge/OneDrive_1_10-12-2024/statcast_pitch_swing_data_20240402_20240630.arrow")

#Updated Dataset
swing<-read.csv("/Users/reesemullen/Downloads/statcast_pitch_swing_data_20240402_20241030_with_arm_angle.csv")

swing$game_date<-as.Date(swing$game_date)
summary(swing$game_date)

#Remove Deprecated Stats
swing1<-Filter(function(x)!all(is.na(x)), swing)

#Avereage Swing speed
average_swing_speed <- swing1 %>%
  group_by(batter) %>%
  summarise(avg_swing_speed = mean(bat_speed, na.rm = TRUE))

unique_player_names <- swing1 %>%
  dplyr::select(batter, player_name) %>%
  distinct()

# Join on batter
average_swing_speed <- average_swing_speed %>%
  left_join(unique_player_names, by = "batter")



#Extracting Age, Height, and Weight Data

# Unique Players
player_ids <- unique(average_swing_speed$batter)

player_info_list <- list()

# Loop each player and store info
for (id in player_ids) {
  # API URL
  url <- paste0("https://statsapi.mlb.com/api/v1/people/", id)
  
  #GET request
  response <- GET(url)
  #Checks
  if (status_code(response) == 200) {
    player_data <- fromJSON(content(response, "text", encoding = "UTF-8"))
    if (!is.null(player_data$people) && nrow(player_data$people) > 0) {
      
      player <- player_data$people[1, ]  
      
      
      # Height, weight and age
      batter <- id
      height <- if (!is.null(player$height)) player$height else NA
      weight <- if (!is.null(player$weight)) player$weight else NA
      age <- if(!is.null(player$currentAge)) player$currentAge else NA
      
      player_info_temp <- data.frame(
        batter = batter,       
        height = height,
        weight = weight,
        age = age,
        stringsAsFactors = FALSE
      )
      
      # Append info to the list
      player_info_list[[length(player_info_list) + 1]] <- player_info_temp
      
      # Debugging: Print player info to verify successful retrieval
      #print(paste("Retrieved data for batter ID:", batter, "Height:", height, "Weight:", weight))
    } else {
      # Debugging: Print if the people field is missing or empty
      #print(paste("No valid 'people' field for batter ID:", id))
    }
  } else {
    # Debugging: Print if the request failed
    #print(paste("Failed to retrieve data for batter ID:", id, "with status code:", status_code(response)))
  }
  
  # Delay
  Sys.sleep(0.1)
}

# Combine the list into a data frame
player_info <- bind_rows(player_info_list)

# Check if it worked
if (nrow(player_info) == 0) {
  print("Warning: No data was collected in player_info.")
} else {
  print("Successfully collected player data.")
}


# Join the player info with swing speed 
average_swing_speed <- average_swing_speed %>%
  left_join(player_info, by = "batter")

average_swing_speed <- average_swing_speed %>%
  distinct(batter, .keep_all = TRUE)

average_swing_speed$weight <- as.numeric(average_swing_speed$weight)
average_swing_speed$average_swing_speed <- as.numeric(average_swing_speed$avg_swing_speed)
summary(average_swing_speed$weight)


# Height in inches
convert_height_to_inches <- function(height_string) {
  
  height_parts <- str_match(height_string, "(\\d+)' (\\d+)")
  
  # Numeric Values
  feet <- as.numeric(height_parts[, 2])
  inches <- as.numeric(height_parts[, 3])
  
  # Convert height to total inches
  height_inches <- (feet * 12) + inches
  return(height_inches)
}

# Apply the height conversion function to the height column
average_swing_speed <- average_swing_speed %>%
  mutate(height_inches = sapply(height, convert_height_to_inches))

# Mean of age, height, and weight
height_mean <- mean(average_swing_speed$height_inches, na.rm = TRUE)
weight_mean <- mean(average_swing_speed$weight, na.rm = TRUE)
age_mean<- mean(average_swing_speed$age, na.rm = TRUE)

# Center age, height, and weight
average_swing_speed <- average_swing_speed %>%
  mutate(
    height_centered = height_inches - height_mean,
    weight_centered = weight - weight_mean,
    age_centered = age - age_mean
  )

summary(average_swing_speed$age)





# Individual PLayer stats
player_swing_stats <- swing1 %>%
  group_by(batter) %>%
  summarise(
    total_pitches = n(),  # Total number of pitches seen by each player
    total_swings = sum(!is.na(bat_speed))  # Count swings (where avg_swing_speed is not NA)
  )

# Join player stats 
average_swing_speed <- average_swing_speed %>%
  left_join(player_swing_stats, by = "batter")

ggplot(data = swing1, aes(x = bat_speed, color = factor(batter))) +
  geom_density() +
  labs(
    title = "Distribution of Swing Speeds by Batter",
    x = "Swing Speed (mph)",
    y = "Density"
  ) +
  guides(color = "none") +  # Remove the color legend
  theme_minimal()




#Find Player's Team
swing1 <- swing1 %>%
  mutate(
    bat_team = ifelse(inning_topbot == "top", away_team, home_team)
  )

#Add Centered stats back to dataset
swing1 <- swing1 %>%
  left_join(
    average_swing_speed %>%
      dplyr::select(batter, height_centered, weight_centered, average_swing_speed),
    by = "batter"
  )
# Subset the dataset
swing1_subset <- swing1 %>%
  dplyr::select(
    pitch_type, release_speed, release_pos_x, release_pos_z, player_name,
    zone, stand, p_throws, pfx_x, pfx_z, outs_when_up,
    release_spin_rate, release_extension, plate_x, plate_z, 
    pitch_number, bat_speed, swing_length, bat_score_diff, balls, strikes,
    age_bat, n_thruorder_pitcher, n_priorpa_thisgame_player_at_bat, 
    batter_days_since_prev_game, bat_team, height_centered, weight_centered, game_date, batter
  )

# Only pitches with swings
swing1_clean <- swing1_subset %>%
  filter(!is.na(bat_speed))

# Save as a CSV
write.csv(swing1_clean, "Bat Speed Cleaned.csv", row.names = FALSE)


ggplot(data = swing1_subset, aes(x = bat_speed, color = factor(bat_team))) +
  geom_density() +
  labs(title = "Distribution of Swing Speeds by Batter",x = "Swing Speed (mph)",y = "Density") +
  guides(color = "none") +  
  theme_minimal()

#Team Level stats
swing_summary <- swing1 %>%
  filter(!is.na(bat_speed)) %>%
  group_by(bat_team) %>%
  summarise(
    mean_swing_speed = mean(bat_speed),
    sd_swing_speed = sd(bat_speed),
    count = n()
  )

summary(average_swing_speed)

library(arrow)
library(dplyr)
library(tidyverse)
library(httr)
library(jsonlite)
library(rvest)
library(ggplot2)
library(ggcorrplot)

#Load Saved Dataset
swing1_clean1<-read.csv("/Users/reesemullen/Desktop/Statistical Practice/ASA-2025-Challenge/Bat Speed Cleaned.csv")
# Summary 
summary(swing1_clean1)
summary(swing1_clean1$bat_speed)

# Missing Values
colSums(is.na(swing1_clean1))

#Filter for Swings regarded as "competitive"
competitive_swings <- swing1_clean1 %>%
  filter(bat_speed > 60) 

# Create the pitch_style column
competitive_swings <- competitive_swings %>%
  mutate(
    pitch_style = case_when(
      pitch_type %in% c("FF", "FC", "SI") ~ "fastball",
      pitch_type %in% c("CH", "FO", "FS", "KN") ~ "offspeed",
      pitch_type %in% c("CU", "EP", "KC", "SL", "ST", "SV", "SC") ~ "breaking",
      TRUE ~ NA_character_  
    )
  )%>%
  filter(!is.na(pitch_style))  


# Histogram swing_speed
ggplot(competitive_swings , aes(x = bat_speed)) +
  geom_histogram(binwidth = 1, fill = "blue", alpha = 0.7) +
  labs(title = "Distribution of Swing Speed", x = "Swing Speed", y = "Frequency")

# Scatter plot swing_speed vs release_speed
ggplot(competitive_swings, aes(x = bat_speed, y = release_speed)) +
  geom_point(alpha = 0.6, aes(color = pitch_style)) +
  geom_smooth(method = "lm", col = "red") +
  labs(title = "Swing Speed vs Swing Length", x = "Bat Speed", y = "Pitch Speed")

# Correlation 
numeric_vars <- competitive_swings %>%
  select_if(is.numeric)

cor_matrix <- cor(numeric_vars, use = "complete.obs")
print(cor_matrix)

ggcorrplot(cor_matrix, 
           hc.order = TRUE, 
           type = "lower", 
           lab = TRUE, 
           lab_size = 3, 
           title = "Correlation Matrix Heatmap",
           colors = c("blue", "white", "red"))

# Swing speed vs batter stance
ggplot(competitive_swings, aes(x = stand, y = bat_speed)) +
  geom_violin(fill = "green", alpha = 0.6) +
  labs(title = "Swing Speed by Batter Stance", x = "Stance", y = "Swing Speed")

# Bar chart of pitch types
ggplot(competitive_swings, aes(x = pitch_name)) +
  geom_bar(fill = "purple", alpha = 0.7) +
  labs(title = "Counts of Pitch Types", x = "Pitch Type", y = "Count")

# outliers in swing_speed using boxplot
boxplot(competitive_swings$bat_speed, main = "Boxplot of Swing Speed", ylab = "Swing Speed")


# Heatmap by handedness and pitch style
ggplot(competitive_swings, aes(x = plate_x, y = plate_z, fill = bat_speed)) +
  geom_tile(size = 2, alpha=0.8) +
  facet_grid(stand ~ p_throws + pitch_style, labeller = labeller(
    pitch_style = c("fastball" = "Fastball", "offspeed" = "Offspeed", "breaking" = "Breaking"),
    stand = c("R" = "Right-Handed", "L" = "Left-Handed"),
    p_throws = c("R" = "Pitcher Right-Handed", "L" = "Pitcher Left-Handed")
  )) +
  scale_fill_gradient(low = "blue", high = "red") +
  labs(
    title = "Heatmap of Bat Speed by Plate Location and Pitch Style",
    x = "Plate X",
    y = "Plate Z",
    fill = "Bat Speed"
  ) +
  theme_minimal() +
  theme(
    axis.text = element_text(size = 8),
    axis.ticks = element_blank(),
    panel.grid =element_line(color = "grey80")  # Add grid lines back
  )

# Summary stats
summary(competitive_swings)


# Frequency tables
table(competitive_swings$pitch_type)  
table(competitive_swings$stand)
table(competitive_swings$pitch_style)
table(competitive_swings$bat_team)
table(competitive_swings$zone)
table(competitive_swings$outs_when_up)

# Summary stats numeric variables
numeric_summary <- competitive_swings %>%
  select(where(is.numeric)) %>%
  summarise(across(everything(), list(mean = mean, sd = sd, min = min, max = max), na.rm = TRUE))

# Frequency tables categorical variables
categorical_summary <- competitive_swings %>%
  select(where(is.character)) %>%
  summarise(across(everything(), ~ list(table(.))))

# View the results
numeric_summary
categorical_summary

numeric_columns <- competitive_swings %>%
  select(where(is.numeric))

# Histograms for numeric columns
for (col in names(numeric_columns)) {
  p <- ggplot(competitive_swings, aes(x = .data[[col]])) +
    geom_histogram(bins = 15, fill = "blue", color = "black", alpha = 0.7) +
    labs(
      title = paste("Distribution of", col),
      x = col,
      y = "Frequency"
    ) +
    theme_minimal()
  
  print(p)  # Print each plot to the R session
}

#Center new age variable and add fast_swing
competitive_swings <- competitive_swings %>%
  mutate(
    age_centered = age_bat - mean(age_bat),
    fast_swing = ifelse(bat_speed >= 75, 1, 0)
  )
ggplot(competitive_swings, aes(x = pitch_style)) +
  geom_bar(fill = "skyblue", color = "black") +
  labs(title = "Frequency of Pitch Styles", x = "Pitch Style", y = "Count") +
  theme_minimal()
ggplot(competitive_swings, aes(x = pitch_style, y = release_speed, fill = pitch_style)) +
  geom_boxplot(trim = TRUE, alpha = 0.8) +
  stat_summary(fun = "median", geom = "point", shape = 23, size = 3, fill = "white") +
  labs(
    title = "Box Plot of Pitch Velocity by Pitch Style",
    x = "Pitch Style",
    y = "Release Speed (mph)"
  ) +
  theme_minimal() +
  theme(legend.position = "none") 

ggplot(competitive_swings, aes(x = pitch_style, y = release_spin_rate, fill = pitch_style)) +
  geom_boxplot(trim = TRUE, alpha = 0.8) +
  stat_summary(fun = "median", geom = "point", shape = 23, size = 3, fill = "white") +
  labs(
    title = "Box Plot of Pitch Spin by Pitch Style",
    x = "Pitch Style",
    y = "Release SPin Rate (rpm)"
  ) +
  theme_minimal() +
  theme(legend.position = "none") 

ggplot(competitive_swings, aes(x = pitch_style, y = pfx_x, fill = pitch_style)) +
  geom_boxplot(trim = TRUE, alpha = 0.8) +
  stat_summary(fun = "median", geom = "point", shape = 23, size = 3, fill = "white") +
  labs(
    title = "Box Plot of X Plane Movement by Pitch Style",
    x = "Pitch Style",
    y = "Movement"
  ) +
  theme_minimal() +
  theme(legend.position = "none") 
ggplot(competitive_swings, aes(x = pitch_style, y = pfx_z, fill = pitch_style)) +
  geom_boxplot(trim = TRUE, alpha = 0.8) +
  stat_summary(fun = "median", geom = "point", shape = 23, size = 3, fill = "white") +
  labs(
    title = "Box Plot of Z Plane Movement by Pitch Style",
    x = "Pitch Style",
    y = "Movement"
  ) +
  theme_minimal() +
  theme(legend.position = "none") 

pitch_velocity_summary <- competitive_swings %>%
  group_by(pitch_style) %>%
  summarize(
    count = n(),                              # Number of observations
    mean_velocity = mean(release_speed, na.rm = TRUE), # Mean
    median_velocity = median(release_speed, na.rm = TRUE), # Median
    sd_velocity = sd(release_speed, na.rm = TRUE),       # Standard deviation
    min_velocity = min(release_speed, na.rm = TRUE),     # Minimum
    max_velocity = max(release_speed, na.rm = TRUE)      # Maximum
  ) %>%
  arrange(desc(mean_velocity))  # Sort by mean velocity (optional)

# Display the summary table
pitch_velocity_summary
summary(competitive_swings$bat_speed)
sd(competitive_swings$bat_speed)

competitive_swings <- competitive_swings %>%
  mutate(bat_speed_category = case_when(
    bat_speed < 68~ "Slow",
    bat_speed >= 68 & bat_speed < 76 ~ "Standard",
    bat_speed >= 76 ~ "Elite",
    TRUE ~ NA_character_  # Handle missing or invalid values
  ))





ggplot(competitive_swings, aes(x = bat_speed_category, fill = bat_speed_category)) +
  geom_bar() +
  labs(
    title = "Distribution of Bat Speed Categories",
    x = "Bat Speed Category",
    y = "Count"
  ) +
  theme_minimal() +
  theme(legend.position = "none")
summary(competitive_swings)
sd(competitive_swings$swing_length)

ggplot(competitive_swings, aes(x = n_thruorder_pitcher)) +
  geom_histogram(bins = 4, fill = "blue", color = "black", alpha = 0.7) +
  labs(
    title = paste("Distribution of n_thruorder_pitcher"),
    x = "n_thruorder_pitcher",
    y = "Frequency"
  ) +
  theme_minimal()
ggplot(competitive_swings, aes(x = n_priorpa_thisgame_player_at_bat)) +
  geom_histogram(bins = 7, fill = "blue", color = "black", alpha = 0.7) +
  labs(
    title = paste("Distribution of n_priorpa_thisgame_player_at_bat"),
    x = "n_priorpa_thisgame_player_at_bat",
    y = "Frequency"
  ) +
  theme_minimal()
ggplot(competitive_swings, aes(x = batter_days_since_prev_game)) +
  geom_histogram(bins = 20, fill = "blue", color = "black", alpha = 0.7) +
  labs(
    title = paste("Distribution of batter_days_since_prev_game"),
    x = "batter_days_since_prev_game",
    y = "Frequency"
  ) +
  theme_minimal()
ggplot(average_swing_speed, aes(x = height_inches)) +
  geom_histogram(bins = 14, fill = "blue", color = "black", alpha = 0.7) +
  labs(
    title = paste("Distribution of Height"),
    x = "Height",
    y = "Frequency"
  ) +
  theme_minimal()
summary(average_swing_speed$weight)
ggplot(average_swing_speed, aes(x = weight)) +
  geom_histogram(bins = 14, fill = "blue", color = "black", alpha = 0.7) +
  labs(
    title = paste("Distribution of Weight"),
    x = "Weight",
    y = "Frequency"
  ) +
  theme_minimal()
summary(average_swing_speed$weight)
competitive_swings
categorical_vars <- names(competitive_swings)[sapply(competitive_swings, is.factor)]


# Identify character and factor variables in the dataset
cat_char_vars <- names(competitive_swings)[sapply(competitive_swings, function(x) is.factor(x) || is.character(x))]

# Loop through each variable and create a bar chart
for (var in cat_char_vars) {
  # Convert character variables to factors temporarily for plotting
  competitive_swings[[var]] <- as.factor(competitive_swings[[var]])
  
  # Create a ggplot for the variable
  p <- ggplot(competitive_swings, aes_string(x = var)) +
    geom_bar(fill = "lightblue", color = "black") +
    labs(
      title = paste("Frequency of", var),
      x = var,
      y = "Count"
    ) +
    theme_minimal() +
    theme(axis.text.x = element_text(hjust = 1))  # Rotate x-axis labels
  
  # Print the plot
  print(p)
}

ggplot(competitive_swings, aes(x = balls)) +
  geom_bar(fill = "lightblue", color = "black") +
  labs(
    title = paste("Frequency of Balls"),
    x = "Balls",
    y = "Count"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(hjust = 1))

ggplot(competitive_swings, aes(x = strikes)) +
  geom_bar(fill = "lightblue", color = "black") +
  labs(
    title = paste("Frequency of Strikes"),
    x = "Strikes",
    y = "Count"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(hjust = 1))

ggplot(competitive_swings, aes(x = zone)) +
  geom_bar(fill = "lightblue", color = "black") +
  labs(
    title = paste("Frequency of Pitch Zone"),
    x = "Zone",
    y = "Count"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(hjust = 1))

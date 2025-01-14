#Data Visualization


#Remove DH's, P's, and Catchers
average_swing_speed_clean <- average_swing_speed %>%
  filter(primary_position != 'P' & primary_position != 'TWP' & primary_position != 'DH' )

average_swing_speed_clean <- average_swing_speed_clean %>%
  dplyr::mutate(
    position_basic = dplyr::case_when(
      primary_position %in% c("LF", "CF", "RF", "OF") ~ "Outfield",  # Outfield positions
      primary_position %in% c("1B", "2B", "3B", "SS") ~ "Infield",  # Infield positions
      primary_position == "C" ~ "Catcher",  # Catcher position
      TRUE ~ "Other"  # Any other position (e.g., DH, P)
    )
  )



# Replace NA with 0 for relevant columns
average_swing_speed_clean <- average_swing_speed_clean %>%
  mutate(across(
    where(is.numeric),  # Select only numeric columns
    ~ replace_na(., 0)  # Replace NA with 0
  ))


# Run ANOVA
anova_results <- aov(avg_swing_speed ~ primary_position, data = average_swing_speed_clean)

# View summary
summary(anova_results)

# Tukey's Honest Significant Difference (HSD) test
post_hoc <- TukeyHSD(anova_results)

# View post-hoc results
print(post_hoc)


# Run T Test
t.test.results <- pairwise.t.test(x = average_swing_speed_clean$avg_swing_speed, 
                                  g = average_swing_speed_clean$position_basic, 
                                  p.adjust.method = "holm")

# View summary
print(t.test.results)




#Modeling
il_model1<-lm(I(Total_Days_Out^2)~height_inches:weight+age_centered+total_swings:I(avg_swing_speed^2)+
                primary_position:rf_innings+total_baserunning_distance+hit_by_pitch, 
              data = average_swing_speed_clean)
summary(il_model1)

il_model2<-lm(Total_Days_Out~height_inches:weight+age_centered+log(total_swings):log(avg_swing_speed)+
                position_basic:rf_innings+total_baserunning_distance+hit_by_pitch, 
              data = no_catch)
summary(il_model2)

# Fit a Poisson regression model
il_model1_poisson <- glm(
  Total_Days_Out ~ 
    as.numeric(height_inches):weight + 
    age_centered + 
    log(total_swings):avg_swing_speed + 
    primary_position:rf_innings + 
    total_innings + 
    total_baserunning_distance,
  family = poisson(link = "log"),  # Specify Poisson family with log link
  data = average_swing_speed_clean
)

# Summarize the Poisson regression model
summary(il_model1_poisson)

# Calculate dispersion parameter for Poisson model
dispersion_parameter <- il_model1_poisson$deviance / il_model1_poisson$df.residual

# Print the dispersion parameter
print(paste("Dispersion Parameter:", round(dispersion_parameter, 4)))

# Check for overdispersion
if (dispersion_parameter > 1.5) {
  print("Overdispersion detected. Consider using a quasi-Poisson or negative binomial model.")
} else if (dispersion_parameter < 0.75) {
  print("Underdispersion detected. Check the model assumptions.")
} else {
  print("Dispersion parameter is within the acceptable range.")
}

# Fit a quasi-Poisson regression model
il_model1_quasipoisson <- glm(
  Total_Days_Out ~ 
    as.numeric(height_inches):weight + 
    age_centered + 
    log(total_swings):log(avg_swing_speed) + 
    primary_position :rf_innings + 
    total_innings + 
    total_baserunning_distance,
  family = quasipoisson(link = "log"),  # Quasi-Poisson family for overdispersion
  data = average_swing_speed_clean
)

# Summarize the quasi-Poisson regression model
summary(il_model1_quasipoisson)

# Load the MASS package for negative binomial regression
library(MASS)

# Refit the model with a negative binomial family
il_model1_negbin <- glm.nb(
  Total_Days_Out ~ 
    as.numeric(height_inches):weight + 
    age_centered + 
    total_swings:I(avg_swing_speed^2) + 
    primary_position : rf_innings +
    total_baserunning_distance,
  data = average_swing_speed_clean
)

# Summary of the negative binomial model
summary(il_model1_negbin)

# Compare AIC for Poisson and Negative Binomial models
AIC(il_model1_poisson, il_model1_negbin)

# Install the pscl package if not already installed
if (!requireNamespace("pscl", quietly = TRUE)) {
  install.packages("pscl")
}

library(pscl)

il_model1_zinb <- zeroinfl(
  Total_Days_Out ~ 
    as.numeric(height_inches):weight + 
    total_swings:avg_swing_speed + 
    position_basic:rf_innings |  # Negative binomial component
    age_centered + 
    rf_innings+total_baserunning_distance,  # Zero-inflation component
  data = average_swing_speed_clean,
  dist = "negbin"
)


# Summary of the ZINB model
summary(il_model1_zinb)

AIC(il_model1_zinb)

library(car)
vif_model <- lm(Total_Days_Out ~ as.numeric(height_inches):weight + total_swings:avg_swing_speed +
                  position_basic:rf_innings, data = average_swing_speed_clean)
vif(vif_model)


average_swing_speed_clean <- average_swing_speed_clean %>%
  mutate(
    height_inches_scaled = scale(as.numeric(height_inches)),
    weight_scaled = scale(weight),
    total_swings_scaled = scale(total_swings),
    avg_swing_speed_scaled = scale(avg_swing_speed),
    rf_innings_scaled = scale(rf_innings),
    total_baserunning_distance_scaled = scale(total_baserunning_distance)
  )

no_catch<- average_swing_speed_clean %>%
  filter(primary_position != 'C' )

il_model1_zinb <- zeroinfl(
  Total_Days_Out ~ 
    height_inches_scaled:weight_scaled + 
    total_swings_scaled:avg_swing_speed_scaled + 
    position_basic:rf_innings_scaled | 
    age_centered + rf_innings_scaled + total_baserunning_distance_scaled,
  data = no_catch,
  dist = "negbin"
)
summary(il_model1_zinb)

AIC(il_model1_zinb)

# Paired t-test
t_test_results1 <- t.test(
  pre_post_injury_results$pre_injury_swing_speed,
  pre_post_injury_results$post_injury_swing_speed,
  paired = TRUE,
  na.rm = TRUE
)

print(t_test_results1)

#Swing Speed Variance by Zone and Pitch Type?
unique(swing$pitch_type)

# Two-way ANOVA
swing_speed_aov <- aov(bat_speed ~ pitch_type_basic *as.factor(zone), data = swing1_swing)

# Summary of the ANOVA
summary(swing_speed_aov)
# Plot residuals
plot(swing_speed_aov)

# Tukey's HSD Test for pairwise comparisons
TukeyHSD(swing_speed_aov)
 


model3<-lm(bat_speed~age_centered+height_centered:weight_centered+ 
             pitch_type_basic:as.factor(zone)+release_speed:pitch_type_basic+ 
             release_spin_rate:pitch_type_basic, data = swing1_swing)
summary(model3)

#Predict Injuries

model4<-lm(deviation~ age+height_inches:weight+ breaking_dev +off_dev + 
             zone_dev+ pre_injury_swings+ injury_swings+ baserunning_distance+
             primary_position:plays, data = position_player_table_updated)




# Join batter from swing1 to position_player_table
position_player_table <- position_player_table %>%
  left_join(
    swing1 %>% dplyr::select(batter, Name_Last_First) %>% distinct(),  # Select and deduplicate batter and Name_Last_First
    by = "Name_Last_First"
  )

# View the updated position_player_table
print(position_player_table)


swing1_subset2 <- swing1 %>%
  filter(!is.na(events) & events != "" & events != "strikeout" & events != "walk")

# Join swing1_subset with position_player_table
joined_data <- swing1_subset2 %>%
  inner_join(
    position_player_table %>% dplyr::select(batter, `Injury / Surgery Date`),
    by = "batter"
  ) %>%
  mutate(
    two_weeks_before = `Injury / Surgery Date` - 14,
    period = case_when(
      game_date < two_weeks_before ~ "long_term",
      game_date >= two_weeks_before & game_date < `Injury / Surgery Date` ~ "short_term",
      TRUE ~ NA_character_  # Exclude other periods
    )
  ) %>%
  filter(!is.na(period))  # Remove irrelevant rows

# Count appearances of batter in fielder_2 to fielder_9 for each period
fielder_columns <- c("fielder_2", "fielder_3", "fielder_4", "fielder_5", "fielder_6", "fielder_7", "fielder_8", "fielder_9")

# Join swing1_subset2 with injuries and calculate fielder appearances
fielder_counts <- swing1_subset2 %>%
  inner_join(
    position_player_table %>% dplyr::select(batter, `Injury / Surgery Date`, Name_Last_First),
    by = "batter",
    relationship = "many-to-many"  # Handle multiple injuries per player
  ) %>%
  mutate(
    two_weeks_before = `Injury / Surgery Date` - 14,
    period = case_when(
      game_date < two_weeks_before ~ "long_term",
      game_date >= two_weeks_before & game_date < `Injury / Surgery Date` ~ "short_term",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(period)) %>%  # Keep only relevant periods
  rowwise() %>%
  mutate(
    is_fielder = sum(c_across(all_of(fielder_columns)) == batter, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  group_by(batter, `Injury / Surgery Date`, period) %>%
  summarise(
    fielder_count = sum(is_fielder),
    .groups = "drop"
  ) %>%
  pivot_wider(
    names_from = period,
    values_from = fielder_count,
    names_prefix = "fielder_count_"
  )


# Join fielder counts back to position_player_table
position_player_table_updated <- position_player_table %>%
  left_join(fielder_counts, by = "batter")

# View the updated table
print(position_player_table_updated)
# Function to calculate metrics for pre-injury periods
calculate_pre_injury_metrics <- function(swing_data, injury_data) {
  # Join swings with injuries based on player name
  joined_data <- swing_data %>%
    inner_join(injury_data %>% select(Name_Last_First, `Injury / Surgery Date`), 
               by = "Name_Last_First")
  
  # Add time periods
  joined_data <- joined_data %>%
    mutate(
      two_weeks_before = `Injury / Surgery Date` - weeks(2),
      period = case_when(
        game_date < two_weeks_before ~ "long_term",
        game_date >= two_weeks_before & game_date < `Injury / Surgery Date` ~ "short_term",
        TRUE ~ NA_character_  # Exclude other periods
      )
    ) %>%
    filter(!is.na(period))  # Filter out irrelevant rows
  
  # Calculate metrics for each period
  pre_injury_metrics <- joined_data %>%
    group_by(Name_Last_First, period) %>%
    summarise(
      avg_swing_speed = mean(bat_speed, na.rm = TRUE),
      swing_count = n(),
      total_innings = sum(innings, na.rm = TRUE),  # Replace with actual innings variable if available
      total_baserunning_distance = sum(baserunning_distance, na.rm = TRUE),  # Replace with actual variable
      .groups = "drop"
    ) %>%
    pivot_wider(
      names_from = period,
      values_from = c(avg_swing_speed, swing_count, total_innings, total_baserunning_distance),
      names_sep = "_"
    )
  
  return(pre_injury_metrics)
}

# Calculate pre-injury metrics
pre_injury_metrics <- calculate_pre_injury_metrics(swing1, position_player_table)

# Join pre-injury metrics back to the position_player_table
position_player_table_updated <- position_player_table %>%
  left_join(pre_injury_metrics, by = "Name_Last_First")

# View the updated table
print(position_player_table_updated)


#Data Visualization

#Modeling
il_model1<-lm(Total_Days_Out~as.numeric(height_inches):weight+age_centered+log(total_swings):log(avg_swing_speed)+
                primary_position*weighted_range_factor+total_innings+total_baserunning_distance, 
              data = average_swing_speed_updated)
summary(il_model1)


# Fit a Poisson regression model
il_model1_poisson <- glm(
  Total_Days_Out ~ 
    as.numeric(height_inches):weight + 
    age_centered + 
    log(total_swings):log(avg_swing_speed) + 
    primary_position * weighted_range_factor + 
    total_innings + 
    total_baserunning_distance,
  family = poisson(link = "log"),  # Specify Poisson family with log link
  data = average_swing_speed_updated
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
    primary_position :weighted_range_factor + 
    total_innings + 
    total_baserunning_distance,
  family = quasipoisson(link = "log"),  # Quasi-Poisson family for overdispersion
  data = average_swing_speed_updated
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
    log(total_swings):log(avg_swing_speed) + 
    primary_position : weighted_range_factor + 
    total_innings + 
    total_baserunning_distance,
  data = average_swing_speed_updated
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

# Fit the ZINB model using zeroinfl()
il_model1_zinb <- zeroinfl(
  Total_Days_Out ~ 
    as.numeric(height_inches):weight + 
    age_centered + 
    log(total_swings):log(avg_swing_speed) + 
    primary_position * weighted_range_factor + 
    total_innings + 
    total_baserunning_distance | # Negative binomial component
    age_centered + total_baserunning_distance,  # Zero-inflation component
  data = average_swing_speed_updated,
  dist = "negbin"
)

# Summary of the ZINB model
summary(il_model1_zinb)

AIC(il_model1_zinb)

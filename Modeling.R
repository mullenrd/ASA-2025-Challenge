library(AER)
library(MASS)
library(dplyr)
library(tidyverse)
library(arm)
library(VGAM)
library(pscl)
options(scipen = 999)
competitive_swings<-na.omit(competitive_swings)

null_lm<-lm(bat_speed~1, data = competitive_swings)
summary(null_lm)
plot(null_lm)
AIC(null_lm)

base_lm<-lm(bat_speed~pitch_style*release_speed+height_centered+weight_centered+
              age_centered+as.factor(balls)+as.factor(strikes)+stand*p_throws+as.Date(game_date), data = competitive_swings)
summary(base_lm)
plot(base_lm)

logit_mod<-glm(fast_swing~pitch_style*release_speed+height_centered+weight_centered+
             age_centered+as.factor(balls)+as.factor(strikes)+stand*p_throws+as.Date(game_date), data = competitive_swings, family = binomial(link = "logit"))
summary(logit_mod)

new_data <- expand.grid(
  pitch_style = c("fastball", "offspeed", "breaking"),
  stand = c("R", "L"),
  p_throws = c("R", "L"),
  release_speed = mean(competitive_swings$release_speed, na.rm = TRUE),
  height_centered = 0,
  weight_centered = 0,
  age_centered = 0,
  balls = 0,
  strikes = 0)

new_data$predicted_prob <- predict(logit_mod, newdata = new_data, type = "response")

ggplot(new_data, aes(x = pitch_style, y = predicted_prob, fill = stand)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~ p_throws) +
  labs(title = "Predicted Probability of Fast Swing by Pitch Style and Stance",
    x = "Pitch Style",y = "Predicted Probability",fill = "Batter Stance") +
  theme_minimal()

#Poisson
poiss_mod<-glm(fast_swing~pitch_style*release_speed+height_centered+weight_centered+
                 age_centered+as.factor(balls)+as.factor(strikes)+stand*p_throws+as.Date(game_date), data = competitive_swings, family = poisson)
summary(poiss_mod)
dispersiontest(poiss_mod)

table(competitive_swings$fast_swing)
#QP
quasi_poisson_mod<-glm(fast_swing~pitch_style*release_speed+height_centered+weight_centered+
                         age_centered+as.factor(balls)+as.factor(strikes)+stand*p_throws+as.Date(game_date), data = competitive_swings, family = quasipoisson(link = "log"))
summary(quasi_poisson_mod)
dispersion_quasi <- summary(quasi_poisson_mod)$deviance / summary(quasi_poisson_mod)$df.residual
dispersion_quasi
#Neg Binomial
nb_mod<-glm.nb(fast_swing~pitch_style*release_speed+height_centered+weight_centered+
                 age_centered+as.factor(balls)+as.factor(strikes)+stand*p_throws+ as.Date(game_date), data = competitive_swings)
summary(nb_mod)



# Zero Inflation
zip_model <- zeroinfl(fast_swing~pitch_style*release_speed+height_centered+weight_centered+
                        age_centered+as.factor(balls)+as.factor(strikes)+stand*p_throws+as.Date(game_date), data = competitive_swings, dist = "poisson")

summary(zip_model)

# ZINB Model
zinb_model <- zeroinfl(fast_swing~pitch_style*release_speed+height_centered+weight_centered+
                         age_centered+as.factor(balls)+as.factor(strikes)+stand*p_throws+as.Date(game_date) | 1, data = competitive_swings, dist = "negbin")

summary(zinb_model)

AIC(poiss_mod, nb_mod, zip_model, zinb_model)
summary(as.factor(competitive_swings$fast_swing))

#No pool
np<- lm(bat_speed~pitch_style*release_speed+height_centered+weight_centered+
          age_centered+balls+strikes+stand*p_throws + bat_team-1, data = competitive_swings
        )
summary(np)
#partial Pool 
part_pool<-lmer(bat_speed~pitch_style*release_speed+height_centered+weight_centered+
                  age_centered+balls+strikes+stand*p_throws + (1|bat_team), data = competitive_swings)
summary(part_pool)

#complete pool
comp_pool<-lm(bat_speed~pitch_style*release_speed+height_centered+weight_centered+
                age_centered+balls+strikes+stand*p_throws, data = competitive_swings)
summary(comp_pool)

predictions <- data.frame(
  bat_team = competitive_swings$bat_team,
  observed = competitive_swings$bat_speed,
  no_pool = predict(np),
  complete_pooling = predict(comp_pool),
  partial_pooling = predict(part_pool))

# Plots
ggplot(predictions, aes(x = observed)) +
  geom_point(aes(y = no_pool, color = "No Pooling", alpha = 0.8)) +
  geom_point(aes(y = complete_pooling, color = "Complete Pooling", alpha = 0.8)) +
  geom_point(aes(y = partial_pooling, color = "Partial Pooling", alpha = 0.8)) +
  facet_wrap(~ bat_team, scales = "free") +
  theme_minimal() +
  labs(title = "Comparison of Pooling Models (Subset of Teams)",
       x = "Observed Bat Speed",y = "Predicted Bat Speed",color = "Model")

ggplot(predictions, aes(x = observed)) +
  geom_point(aes(y = no_pool, color = "No Pooling"), alpha = 0.3) +
  geom_point(aes(y = complete_pooling, color = "Complete Pooling"), alpha = 0.3) +
  geom_point(aes(y = partial_pooling, color = "Partial Pooling"), alpha = 0.3) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "black") +  # Reference line
  facet_wrap(~ bat_team, scales = "free_y") +
  theme_minimal() +
  labs(title = "Observed vs Predicted Bat Speed by Model",
    x = "Observed Bat Speed", y = "Predicted Bat Speed", color = "Model")

ggplot(predictions, aes(x = observed)) +
  geom_point(aes(y = no_pool, color = "No Pooling"), alpha = 0.3) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "black") +  # Reference line
  facet_wrap(~ bat_team, scales = "free_y") +
  theme_minimal() +
  labs(title = "Observed vs Predicted Bat Speed by Model",
    x = "Observed Bat Speed", y = "Predicted Bat Speed",color = "Model" )

ggplot(predictions, aes(x = observed)) +
  geom_point(aes(y = complete_pooling, color = "Complete Pooling"), alpha = 0.3) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "black") +  # Reference line
  facet_wrap(~ bat_team, scales = "free_y") +
  theme_minimal() +
  labs(title = "Observed vs Predicted Bat Speed by Model",
    x = "Observed Bat Speed", y = "Predicted Bat Speed", color = "Model" )

ggplot(predictions, aes(x = observed)) +
  geom_point(aes(y = partial_pooling, color = "Partial Pooling", alpha = 0.8)) +
  facet_wrap(~ bat_team, scales = "free") +
  theme_minimal() +
  labs(title = "Comparison of Pooling Models (Subset of Teams)",
    x = "Observed Bat Speed",y = "Predicted Bat Speed",color = "Model")

residuals_no_pool <- residuals(np)
residuals_complete <- residuals(comp_pool)
residuals_partial <- residuals(part_pool)
residuals_poisson<-residuals(poiss_mod)

var(residuals_no_pool)
var(residuals_complete)
var(residuals_partial)
var(residuals_poisso)

AIC(np, comp_pool, part_pool)
BIC(np, comp_pool, part_pool)

logLik(no_pooling_model)
logLik(complete_pooling_model)
logLik(partial_pooling_model)

competitive_swings$bat_speed_category <- factor(
  competitive_swings$bat_speed_category,
  levels = c("Slow", "Standard", "Elite"),
  ordered = TRUE
)
cat_mod <- polr(
  bat_speed_category ~ pitch_style * release_speed + height_centered + 
    weight_centered + age_centered + as.factor(balls) + as.factor(strikes) +
    stand * p_throws + as.Date(game_date),
  data = competitive_swings,Hess = TRUE)

summary((cat_mod))

#Predictions
player_input <- data.frame(
  pitch_style = "fastball",         
  release_speed = 95,               
  height_centered = 3,            
  weight_centered = 30,             
  age_centered = -3,                
  balls = 2,                        
  strikes = 0,                      
  stand = "R",                      
  p_throws = "R",                   
  bat_team = "BOS",  
  game_date = as.Date("07-11-2024")
)

# Example: Adding models to a list
models <- list(
  "Null" = null_lm,
  "Poisson" =poiss_mod,
  "Negative Binomial" = nb_mod,
  "No Pooling" = np,                
  "Partial Pooling" = part_pool,    
  "Ordered Model" = cat_mod         
)

# Initialize a data frame to store predictions
predictions <- data.frame(
  Model = character(),
  Predicted_Value = numeric(),
  stringsAsFactors = FALSE
)

for (model_name in names(models)) {
  model <- models[[model_name]]
  
  # Generate predictions with model-specific adjustments
  if (inherits(model, "lmerMod")) {
    # Partial pooling: mixed-effects model
    pred <- predict(model, newdata = player_input, re.form = NULL)
    interpretation <- paste("Predicted bat speed:", round(pred, 2), "mph")
  } else if (inherits(model, "polr")) {
    # Ordered categorical model
    pred <- predict(model, newdata = player_input, type = "class")  # Predicted category
    interpretation <- paste("Predicted bat speed category:", pred)
  } else if (inherits(model, "glm.nb")) {
    # Negative Binomial model
    pred <- predict(model, newdata = player_input, type = "response")
    interpretation <- paste("Expected rate of fast swings:", round(pred, 5))
  } else if (inherits(model, "glm")) {
    # Poisson or logistic regression models
    pred <- predict(model, newdata = player_input, type = "response")
    interpretation <- paste("Expected rate of fast swings:", format(pred, digits =3))
  } else {
    # Null or no pooling (basic linear model)
    pred <- predict(model, newdata = player_input)
    interpretation <- paste("Predicted bat speed:", round(pred, 2), "mph")
  }
  
  # Append to predictions data frame
  predictions <- rbind(predictions, data.frame(
    Model = model_name,
    Predicted_Value = interpretation,
    stringsAsFactors = FALSE
  ))
}

print(predictions)


collisions <- dbGetQuery(db, "
SELECT
  collisions.collision_severity,
  
  collisions.alcohol_involved,
  parties.party_drug_physical,
  
  parties.cellphone_in_use,
  
  collisions.primary_collision_factor,
  
  collisions.lighting,
  collisions.road_surface,
  collisions.weather_1,
  collisions.weather_2,
  collisions.intersection,
  collisions.state_highway_indicator,
  
  parties.party_sex,
  parties.party_age
FROM 
  collisions
INNER JOIN
  parties
  ON collisions.case_id = parties.case_id
    AND at_fault = 1
    AND party_type = 'driver';")


###################### Log Entries ######################
sort(table(collisions$collision_severity), decreasing = TRUE)

sort(table(collisions$alcohol_involved), decreasing = TRUE)
sort(table(collisions$party_drug_physical), decreasing = TRUE)

sort(table(collisions$cellphone_in_use), decreasing = TRUE)

sort(table(collisions$primary_collision_factor), decreasing = TRUE)

###################### Preserve database query ######################
data <- collisions

# Create a new column 'sleepy' based on the condition
data$sleepy <- ifelse(data$party_drug_physical == "sleepy/fatigued", 1, 0)
data$sleepy[is.na(data$sleepy)] <- 0

data$asleep <- ifelse(data$primary_collision_factor == "fell asleep", 1, 0)
data$asleep[is.na(data$asleep)] <- 0

data$drug_influence <- ifelse(data$party_drug_physical == "under drug influence", 1, 0)
data$drug_influence[is.na(data$drug_influence)] <- 0  # set alcohol nas to 0 (no)

data$party_drug_physical <- NULL
data$primary_collision_factor <- NULL
data$party_race <- NULL


###################### initial correction ######################
data$alcohol_involved[is.na(data$alcohol_involved)] <- 0  # set alcohol nas to 0 (no)
data <- data[data$collision_severity != "N", ]      # remove one random entry

data$party_sex <- recode(data$party_sex,
                             "male" = 1,
                             "female" = 0,
                             "X" = 0.5)

data$lighting <- recode(data$lighting,
                            "daylight" = 0,
                            "G" = 0,
                            "dusk or dawn" = 1,
                            "dark with street lights" = 2,
                            "dark with no street lights" = 3,
                            "dark with street lights not functioning" = 3)

data$road_surface <- recode(data$road_surface,
                                "dry" = 0,
                                "I" = 0,
                                "J" = 0,
                                "H" = 0,
                                "wet" = 1,
                                "snowy" = 2,
                                "slippery" = 3)
data$road_surface <- ifelse(
  is.na(data$road_surface) & data$weather_1 == 2, 1,  # If road_surface is NA and weather_1 == 2, set to 1
  ifelse(
    is.na(data$road_surface) & data$weather_1 == 4, 2,  # If road_surface is NA and weather_1 == 4, set to 2
    ifelse(is.na(data$road_surface), 0, data$road_surface)  # Otherwise, if NA, set to 0
  )
)

data$weather_1[is.na(data$weather_1)] <- "clear"
data$weather_1 <- recode(data$weather_1,
                             "clear" = 0,
                             "wind" = 0,
                             "other" = 0,
                             "cloudy" = 1,
                             "raining" = 2,
                             "fog" = 3,
                             "snowing" = 4)
data$weather_2 <- recode(data$weather_2,
                             "clear" = 0,
                             "wind" = 0,
                             "other" = 0,
                             "cloudy" = 1,
                             "raining" = 2,
                             "fog" = 3,
                             "snowing" = 4)
data$weather_1 <- ifelse(!is.na(data$weather_2) & data$weather_2 > data$weather_1,
                             data$weather_2, 
                             data$weather_1)
data$weather_2 <- NULL


###################### Convert ordinal to factor datatypes ######################
data$collision_severity <- factor(
  data$collision_severity,
  levels = c("property damage only", "pain", "other injury", "severe injury", "fatal"),
  ordered = TRUE
)
data$collision_severity <- as.numeric(as.factor(data$collision_severity)) # for 1 to 5


###################### HEATMAP ######################
heatmap <- na.omit(data) # way smaller, but still accurate matrix
cor_matrix <- cor(heatmap, method = "spearman")
cor_matrix <- cor_matrix/5

heatmap(cor_matrix, 
        main = "Correlation Heatmap",     # Title
        col = colorRampPalette(c("blue", "white", "red"))(100),  # Color gradient
        scale = "none",                   # Do not scale the data
        margins = c(30, 30),              # Margins around the heatmap
        cexRow = 2.5,                    # Row label size
        cexCol = 2.5,                    # Column label size
        symm = TRUE,
        Rowv = NA,  # Disable automatic row clustering
        Colv = NA,
        zlim = c(-1, 1))

###################### ORDINAL REGRESSION ######################
modell <- MASS::polr(collision_severity ~ alcohol_involved + drug_influence + sleepy + asleep + cellphone_in_use, data = data, Hess = TRUE)
summary(modell)
exp(coef(modell))

predictor_coeffs <- coef(modell)[-1]  # Exclude intercepts
predictor_se <- summary(modell)$coefficients[-1, "Std. Error"]

# Calculate z-values and p-values for predictors
predictor_z <- predictor_coeffs / predictor_se
predictor_p_values <- 2 * (1 - pnorm(abs(predictor_z)))

# Create the result data frame
result <- data.frame(
  Coefficient = predictor_coeffs,
  Std_Error = predictor_se,
  Z_value = predictor_z,
  P_value = predictor_p_values
)

# Print the results
print(result)

###################### PLOT ORDINAL REGRESSION ######################
predictions <- ggpredict(modell, terms = "alcohol_involved")

# Plot with ggplot2
ggplot(predictions, aes(x = x, y = predicted, color = group)) +
  geom_line(size = 1.2) +                             # Lines for probabilities
  geom_point(size = 3) +                              # Points for probabilities
  labs(
    title = "Predicted Probabilities from polr Model",
    x = "Alcohol Involved",
    y = "Probability",
    color = "Alcohol_involved"                            # Response variable groups
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(size = 30, face = "bold"),
    axis.title = element_text(size = 25),
    axis.text = element_text(size = 20)
  )




## not suitable for modelling severity, therefore left out ##
###################### LINEAR REGRESSION ######################
model_lm <- lm(severity_numeric ~ alcohol_involved *  drug_influence * sleepy * asleep * cellphone_in_use, data = data)
summary(model_lm)

###################### POISSON REGRESSION ######################
model_poisson <- glm(severity_numeric ~ alcohol_involved + cellphone_in_use + sleepy, 
                     data = data, family = quasipoisson)
summary(model_poisson)





###################### REGRESSION POPULATION -> COLLISION ######################
population_colls <- dbGetQuery(db, "
SELECT
  city_location,
  AVG(total_collisions) AS avg_collisions,
  AVG(city_population) AS avg_population,
  AVG(total_collisions) / AVG(city_population) * 100000 AS rate
FROM (
  SELECT
    city_location,
    year,
    COUNT(*) AS total_collisions,
    city_population
  FROM 
    collisions
  WHERE 
    city_population > 10000
    AND city_population < 500000
  GROUP BY
    city_location, year
) AS subquery
GROUP BY
  city_location;")

population_colls <- na.omit(population_colls)
population_colls$avg_pop_scaled <- population_colls$avg_population/1000

###################### LINEAR REGRESSION ######################
model_lm <- lm(avg_collisions ~ avg_pop_scaled, data = population_colls) # absolute collisions
model_lm <- lm(rate ~ avg_pop_scaled, data = population_colls) # rate
summary(model_lm)

###################### PLOT REGRESSION ######################
ggplot(population_colls, aes(x = avg_pop_scaled, y = avg_collisions)) +
  geom_point(size = 2) +                               # Scatterplot points
  geom_smooth(method = "lm", color = "blue", size = 1.5) + # Regression line with custom width
  labs(
    x = "Average Population (in Thousands)",
    y = "Collisions",
    title = "Regression of Collisions on Average City Population"
  ) +
  theme_minimal(base_size = 14) +                      # Minimal theme with adjusted font size
  theme(
    plot.title = element_text(size = 30, face = "bold"),  # Title size
    axis.title = element_text(size = 25),                # Axis label size
    axis.text = element_text(size = 20)                  # Axis tick size
  )

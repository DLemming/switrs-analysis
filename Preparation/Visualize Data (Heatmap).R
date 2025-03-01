collisions <- dbGetQuery(db, "
SELECT
  collisions.*,
  parties.party_type AS party_type,
  parties.party_sex AS party_sex,
  parties.party_age AS party_age,
  parties.cellphone_in_use AS cellphone_in_use,
  parties.vehicle_year AS vehicle_year,
  parties.party_race AS party_race,
  count(victims.id) AS occupants_count
FROM 
  collisions
INNER JOIN
  parties
  ON collisions.case_id = parties.case_id
    AND at_fault = 1
    AND party_type = 'driver'
LEFT JOIN
  victims
  ON parties.case_id = victims.case_id
    AND parties.party_number = victims.party_number
GROUP BY
  collisions.case_id;")


collisions$population <- NULL
collisions$caltrans_county <- NULL
collisions$state_route <- NULL
collisions$route_suffix <- NULL
collisions$postmile_prefix <- NULL
collisions$caltrans_district <- NULL
collisions$postmile <- NULL
collisions$location_type <- NULL
collisions$ramp_intersection <- NULL
collisions$chp_road_type <- NULL

metrical <- collisions

metrical$case_id <- NULL
metrical$reporting_district <- NULL
metrical$jurisdiction <- NULL
metrical$officer_id <- NULL
metrical$chp_shift <- NULL
metrical$county_location <- NULL
metrical$beat_type <- NULL
metrical$chp_beat_type <- NULL
metrical$city_division_lapd <- NULL
metrical$chp_beat_class <- NULL
metrical$beat_number <- NULL
metrical$primary_road <- NULL
metrical$secondary_road <- NULL
metrical$direction <- NULL
metrical$side_of_highway <- NULL
metrical$primary_collision_factor <- NULL
metrical$pcf_violation_code <- NULL
metrical$pcf_violation_category <- NULL
metrical$pcf_violation_subsection <- NULL
metrical$type_of_collision <- NULL
metrical$motor_vehicle_involved_with <- NULL
metrical$pedestrian_action <- NULL
metrical$road_condition_2 <- NULL
metrical$control_device <- NULL
metrical$statewide_vehicle_type_at_fault <- NULL
metrical$chp_vehicle_type_at_fault <- NULL
metrical$primary_ramp <- NULL
metrical$secondary_ramp <- NULL
metrical$pcf_violation <- NULL

metrical$not_private_property <- NULL
metrical$process_date <- NULL
metrical$party_type <- NULL # since only drivers


# -------------------- very safe -----------------------
metrical$motorcyclist_injured_count[is.na(metrical$motorcyclist_injured_count)] <- 0 # manually evaluated: all 1 entries
metrical$distance[is.na(metrical$distance)] <- mean(metrical$distance[!is.na(metrical$distance)]) # mean: changes 2 occurences
metrical$hit_and_run[is.na(metrical$hit_and_run)] <- 0 # guess: changes 1 occurence


# ------------------- blind guess -----------------------
metrical$alcohol_involved[is.na(metrical$alcohol_involved)] <- 0 # interpret all na as 0

# -------------------- todo -------------------------
#metrical$state_highway_indicator[is.na(metrical$state_highway_indicator)] <- 0
#metrical$special_condition[is.na(metrical$special_condition)] <- 0
#metrical$intersection[is.na(metrical$intersection)] <- 0
#metrical$tow_away[is.na(metrical$tow_away)] <- 0
#metrical$injured_victims[is.na(metrical$injured_victims)] <- 0
#metrical$alcohol_involved[is.na(metrical$alcohol_involved)] <- 0
#metrical$killed_victims[is.na(metrical$killed_victims)] <- 0


# ------------------Weather ---------------------------
metrical$weather_1[is.na(metrical$weather_1)] <- "clear"
metrical$weather_1 <- recode(metrical$weather_1,
                             "clear" = 0,
                             "wind" = 0,
                             "other" = 0,
                             "cloudy" = 1,
                             "raining" = 2,
                             "fog" = 3,
                             "snowing" = 4)
metrical$weather_2 <- recode(metrical$weather_2,
                             "clear" = 0,
                             "wind" = 0,
                             "other" = 0,
                             "cloudy" = 1,
                             "raining" = 2,
                             "fog" = 3,
                             "snowing" = 4)
metrical$weather_1 <- ifelse(!is.na(metrical$weather_2) & metrical$weather_2 > metrical$weather_1,
                             metrical$weather_2, 
                             metrical$weather_1)
metrical$weather_2 <- NULL

# ----------------- Road Surface -----------------

metrical$road_surface <- recode(metrical$road_surface,
                                "dry" = 0,
                                "I" = 0,
                                "J" = 0,
                                "H" = 0,
                                "wet" = 1,
                                "snowy" = 2,
                                "slippery" = 3,
                                .default = NA)
metrical$road_surface <- ifelse(
  is.na(metrical$road_surface) & metrical$weather_1 == 2, 1,  # If road_surface is NA and weather_1 == 2, set to 1
  ifelse(
    is.na(metrical$road_surface) & metrical$weather_1 == 4, 2,  # If road_surface is NA and weather_1 == 4, set to 2
    ifelse(is.na(metrical$road_surface), 0, metrical$road_surface)  # Otherwise, if NA, set to 0
  )
)

# ----------------- Collision Severity -----------------
metrical$collision_severity <- recode(metrical$collision_severity,
                                      "property damage only" = 0,
                                      "N" = 0,
                                      "pain" = 1,
                                      "other injury" = 1,
                                      "severe injury" = 2,
                                      "fatal" = 3)

# ----------------- Road Condition -----------------
metrical$road_condition_1 <- recode(metrical$road_condition_1,
                                    "normal" = 0,
                                    "construction" = 1,
                                    "obstruction" = 1,
                                    "reduced width" = 1,
                                    "other" = 1,
                                    "holes" = 2,
                                    "loose material" = 2,
                                    "flooded" = 3)
metrical$road_condition_1[is.na(metrical$road_condition_1)] <- mean(metrical$road_condition_1[!is.na(metrical$road_condition_1)])

# ---------------- Hit and Run ---------------------
metrical$hit_and_run_felony <- ifelse(metrical$hit_and_run == "felony",
                                      1, 
                                      0)
metrical$hit_and_run <- ifelse(metrical$hit_and_run == "not hit and run",
                               0, 
                               1)

# ----------------- remap lighting -----------------
metrical$lighting <- recode(metrical$lighting,
                            "daylight" = 0,
                            "G" = 0,
                            "dusk or dawn" = 1,
                            "dark with street lights" = 2,
                            "dark with no street lights" = 3,
                            "dark with street lights not functioning" = 3)

# --------------------- Time ------------------------
time_difference <- function(time_str) {
  # If the input is NA, return NA
  if (is.na(time_str)) {
    return(NA)
  }
  
  # Convert the input time string to a time object
  time <- strptime(time_str, format = "%H:%M:%S")
  
  # Define the reference time 06:00:00
  ref_time <- strptime("06:00:00", format = "%H:%M:%S")
  
  # Calculate the difference in seconds
  time_diff <- as.numeric(difftime(time, ref_time, units = "secs"))
  
  # If the difference is negative (before 06:00:00), adjust it to the highest value
  if (time_diff < 0) {
    time_diff <- 24 * 60 * 60 + time_diff  # Add a full day in seconds
  }
  
  # Convert the time difference to minutes
  time_diff_minutes <- time_diff / 60
  
  return(time_diff_minutes)
}
metrical$collision_time <- sapply(metrical$collision_time, time_difference)

# metrical$collision_time[is.na(metrical$collision_time)] <- mean(metrical$collision_time[!is.na(metrical$collision_time)])

# ------------------ Infere Lighting / Time NAs -----------------------
avg_time_day <- mean( metrical$collision_time[!is.na(metrical$collision_time) & !is.na(metrical$lighting) & metrical$lighting == 0], na.rm = TRUE)
avg_time_night <- mean( metrical$collision_time[ !is.na(metrical$collision_time) & !is.na(metrical$lighting) & metrical$lighting == 2 | metrical$lighting == 3], na.rm = TRUE)
avg_time_duskdawn <- mean( metrical$collision_time[ !is.na(metrical$collision_time) & !is.na(metrical$lighting) & metrical$lighting == 1], na.rm = TRUE)
avg_time <- mean(metrical$collision_time[!is.na(metrical$collision_time)])

metrical <- metrical %>%
  mutate(
    collision_time = case_when(
      is.na(collision_time) & lighting == 0 ~ avg_time_day,               # If lighting is 0, use avg_time_day
      is.na(collision_time) & lighting == 1 ~ avg_time_duskdawn,          # If lighting is 1, use avg_time_duskdawn
      is.na(collision_time) & (lighting == 2 | lighting == 3) ~ avg_time_night, # If lighting is 2 or 3, use avg_time_night
      is.na(collision_time) ~ avg_time                                      # Default case for NA in collision_time
    )
  )

metrical <- metrical %>%
  mutate(
    lighting = case_when(
      collision_time < 600 ~ 0,               # Lighting 0 if collision_time < 600
      collision_time >= 600 & collision_time < 800 ~ 1,  # Lighting 1 if collision_time is between 600 and 800
      collision_time >= 800 ~ 2               # Lighting 2 if collision_time >= 800
    )
  )



# ----------------- convert dates to days since 2001-01-01 -----------------
reference_date <- as.Date("2001-01-01")
metrical$collision_date <- as.numeric(as.Date(metrical$collision_date) - reference_date)
remove(reference_date)
  
# ----------------- Party Sex -----------------
metrical$party_sex <- recode(metrical$party_sex,
                             "male" = 1,
                             "female" = 0,
                             "X" = 0.5)

# ----------------- Party Race -----------------
metrical$party_race_white <- ifelse(metrical$party_race == "white",
                                    1, 
                                    0)

metrical$party_race_hispanic <- ifelse(metrical$party_race == "hispanic",
                                       1, 
                                       0)
metrical$party_race_black <- ifelse(metrical$party_race == "black",
                                    1, 
                                    0)
metrical$party_race_asian <- ifelse(metrical$party_race == "asian",
                                    1, 
                                    0)
metrical$party_race_other <- ifelse(metrical$party_race == "other",
                                    1, 
                                    0)
metrical$party_race <- NULL



# -------------------- convert to numeric ------------------------
metrical$special_condition <- as.numeric(metrical$special_condition)
metrical$county_city_location <- as.numeric(metrical$county_city_location)


test <- na.omit(metrical) # way smaller, but still accurate matrix


cor_matrix <- cor(metrical, method = "spearman")
cor_matrix_1.5x <- round(sign(cor_matrix) * (abs(cor_matrix))^1.5, 3)
cor_matrix_2x <- round(sign(cor_matrix) * (abs(cor_matrix))^2, 3)
cor_matrix_3x <- round(sign(cor_matrix) * (abs(cor_matrix))^3, 3)

heatmap(cor_matrix_2x, 
        main = "Correlation Heatmap",     # Title
        col = colorRampPalette(c("blue", "white", "red"))(100),  # Color gradient
        scale = "none",                   # Do not scale the data
        margins = c(18, 18),              # Margins around the heatmap
        cexRow = 2,                    # Row label size
        cexCol = 2,                    # Column label size
        symm = TRUE,
        Rowv = NA,  # Disable automatic row clustering
        Colv = NA,
        zlim = c(-1, 1))
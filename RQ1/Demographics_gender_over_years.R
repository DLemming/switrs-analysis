# Get at_fault drivers from database
at_fault <- dbGetQuery(db, "
SELECT
  collisions.year AS year,
  parties.party_sex AS party_sex,
  parties.party_age AS party_age,
  parties.party_race AS party_race
FROM 
  collisions
LEFT JOIN
  parties
  ON collisions.case_id = parties.case_id
    AND parties.at_fault = 1
WHERE
  collisions.year IS NOT NULL
  AND parties.party_race IS NOT NULL
  AND parties.party_sex IS NOT NULL
  AND parties.party_sex IS NOT 'X'
  AND parties.party_age IS NOT NULL;")


################### Group by Gender & Year ###################
collisions_by_gender_year <- at_fault %>%
  group_by(party_sex, year) %>%
  summarise(collisions = n(), .groups = "drop")

################### Group by Ethnicity & Year ###################
collisions_by_ethnicity_year <- at_fault %>%
  group_by(party_race, year) %>%
  summarise(collisions = n(), .groups = "drop")

################### Group by Age & Year ###################
# Define age groups in discrete bins
at_fault <- at_fault %>%
  mutate(age_group = cut(party_age, breaks = c(10, 20, 30, 40, 50, 60, 70, 80, 90), 
                         labels = c("11-20", "21-30", "31-40", "41-50", "51-60", "61-70", "71-80", "81-90"),
                         right = FALSE))

# Sum of collisions by age group and year
collisions_by_age_year <- at_fault %>%
  group_by(age_group, year) %>%
  summarise(collisions = n(), .groups = "drop")
collisions_by_age_year <- na.omit(collisions_by_age_year)



#######################################################################
########################### Plotting ##################################
#######################################################################

################### plot gender_over_years ###################
ggplot(data = collisions_by_gender_year, aes(x = year, y = collisions/1000, color = party_sex)) +
  geom_line(size = 2) + # Line graph
  labs(title = "Collisions by Gender and Year",
       x = "Year",
       y = "Collisions (in Thousands)",
       color = "Gender") +
  scale_y_continuous() +  # Format y-axis as percentage
  theme_minimal() +
  theme(
    plot.title = element_text(size = 30, face = "bold", hjust = 0.5),
    axis.text.x = element_text(size = 20),
    axis.text.y = element_text(size = 20),
    axis.title.x = element_text(size = 25), # Increase x-axis title size
    axis.title.y = element_text(size = 25), # Increase y-axis title size
    legend.title = element_text(size = 25), # Increase legend title size
    legend.text = element_text(size = 20)
  )

################### plot race_over_years ###################
ggplot(data = collisions_by_ethnicity_year, aes(x = year, y = collisions/1000, color = party_race)) +
  geom_line(size = 2) + # Line graph
  labs(title = "Collisions by Ethnicity and Year",
       x = "Year",
       y = "Collisions (in Thousands)",
       color = "Ethnicity") +
  scale_y_continuous() +  # Format y-axis as percentage
  theme_minimal() +
  theme(
    plot.title = element_text(size = 30, face = "bold", hjust = 0.5),
    axis.text.x = element_text(size = 20),
    axis.text.y = element_text(size = 20),
    axis.title.x = element_text(size = 25), # Increase x-axis title size
    axis.title.y = element_text(size = 25), # Increase y-axis title size
    legend.title = element_text(size = 25), # Increase legend title size
    legend.text = element_text(size = 20)
  )

################### plot age_over_years ###################
ggplot(data = collisions_by_age_year, aes(x = year, y = collisions/1000, color = age_group)) +
  geom_line(size = 2) + # Line graph
  labs(title = "Collisions By Age and Year",
       x = "Year",
       y = "Collisions (in Thousands)",
       color = "Age") +
  scale_y_continuous() +  # Format y-axis as percentage
  theme_minimal() +
  theme(
    plot.title = element_text(size = 30, face = "bold", hjust = 0.5),
    axis.text.x = element_text(size = 20),
    axis.text.y = element_text(size = 20),
    axis.title.x = element_text(size = 25), # Increase x-axis title size
    axis.title.y = element_text(size = 25), # Increase y-axis title size
    legend.title = element_text(size = 25), # Increase legend title size
    legend.text = element_text(size = 20)
  )
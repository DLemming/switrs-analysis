################### Count all collisions grouped by year ###################
num_collisions <- dbGetQuery(db, "
SELECT
  count(case_id) as collisions,
  year
FROM 
  collisions
GROUP BY
  year")

################### Plot all collisions ###################
ggplot(data = num_collisions, aes(x = year, y = collisions/1000)) +
  geom_line(size = 2) + # Line graph
  labs(title = "Collisions in California by Year",
       x = "Year",
       y = "Collisions (in Thousands)") +
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



################### Gender Box Plot ###################
demographics <- dbGetQuery(db, "
SELECT
  party_sex,
  party_age,
  party_race,
  at_fault
FROM 
  parties
WHERE
  party_race IS NOT NULL
  AND party_sex IS NOT NULL
  AND party_age IS NOT NULL
  AND at_fault IS NOT NULL;")

ggplot(demographics, aes(x = factor(at_fault),
                         y = party_age,
                         fill = factor(at_fault))) +
  theme_minimal() +
  geom_boxplot() +
  labs(
    title = "Distribution of Age by Fault Status",    # Add a plot title
    x = "At Fault",                 # Rename x-axis
    y = "Age",        # Rename y-axis
    fill = "At Fault"               # Rename legend title (if applicable)
  ) +
  theme(
    plot.title = element_text(size = 30, face = "bold", hjust = 0.5),  # Title style
    axis.title = element_text(size = 25),   # Customize axis title font size
    axis.text = element_text(size = 20)    # Customize axis text size
  )


ggplot(demographics, aes(x = party_race,
                         y = party_age,
                         fill = party_race)) +
  theme_minimal() +
  geom_boxplot() +
  labs(
    title = "Distribution of Age by Ethnicity",    # Add a plot title
    x = "Ethnicity",                 # Rename x-axis
    y = "Age",        # Rename y-axis
    fill = "Ethnicity"               # Rename legend title (if applicable)
  ) +
  theme(
    plot.title = element_text(size = 30, face = "bold", hjust = 0.5),  # Title style
    axis.title = element_text(size = 25),   # Customize axis title font size
    axis.text = element_text(size = 20)    # Customize axis text size
  )

ggplot(demographics, aes(x= party_race,
                         y= party_age,
                         fill = party_race)) +
  geom_boxplot()
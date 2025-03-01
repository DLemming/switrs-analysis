at_fault <- dbGetQuery(db, "SELECT 
    COUNT(parties.case_id) AS collisions, 
        party_age, 
        party_sex
    FROM parties
    WHERE 
        parties.at_fault = 1
        AND party_type = 'driver'
        AND party_sex IN ('male', 'female')
        AND party_age IS NOT NULL
        AND party_sex IS NOT NULL
    GROUP BY 
        party_age, 
        party_sex;")

not_at_fault <- dbGetQuery(db, "SELECT 
    COUNT(parties.case_id) AS collisions, 
        party_age, 
        party_sex
    FROM parties
    WHERE 
        parties.at_fault = 0
        AND party_type = 'driver'
        AND party_sex IN ('male', 'female')
        AND party_age IS NOT NULL
        AND party_sex IS NOT NULL
    GROUP BY 
        party_age, 
        party_sex;")

not_at_fault$group_percent <- not_at_fault$collisions / sum(not_at_fault$collisions)
not_at_fault$group_population <- not_at_fault$group_percent * 39500000 # number of Californian drivers in each category
not_at_fault$group_percent <- not_at_fault$group_percent * 100

at_fault <- merge(at_fault, not_at_fault[, c("party_age", "party_sex", "group_population")],
                  by = c("party_age", "party_sex"), 
                  all.x = TRUE)
at_fault <- na.omit(at_fault)

at_fault$relative_colls <- at_fault$collisions / at_fault$group_population


age_filtered <- at_fault[at_fault$party_age >= 15 & at_fault$party_age <= 90, ] # Filter the data for ages between 15 and 90

baseline <- min(age_filtered$relative_colls, na.rm=TRUE) # Choose the baseline as the minimum
at_fault$relative_colls_factor <- at_fault$relative_colls / baseline  # Calculate the factor






# plot at_fault_parties
ggplot(data = at_fault, aes(x = party_age, y = collisions/1000, color = party_sex)) +
  geom_line(size = 2) + # Line graph
  labs(title = "Collisions by Gender and Age of At-Fault Parties",
       x = "Age",
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

# plot not_at_fault_parties
ggplot(data = not_at_fault, aes(x = party_age, y = group_percent, color = party_sex)) +
  geom_line(size = 2) + # Line graph
  labs(title = "Driver Distribution by Gender and Age",
       x = "Age",
       y = "Drivers (in Percent)",
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

# Plot proportional collisions with factors (e.g., "1x", "2x") on the y-axis
ggplot(data = at_fault, aes(x = party_age, y = relative_colls_factor, color = party_sex)) +
  geom_line(size = 2) +  # Line graph
  labs(title = "Proportional Collisions by Gender and Age",
       x = "Age",
       y = "Collisions (in Proportion)",
       color = "Gender") +
  scale_y_continuous(
    breaks = seq(1, max(at_fault$relative_colls_factor, na.rm = TRUE), by = 1),  # Breaks at 1x, 2x, etc.
    labels = function(x) paste0(x, "x")  # Add "x" to labels
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 30, face = "bold", hjust = 0.5),
    axis.text.y = element_text(size = 20),  # Format y-axis text
    axis.ticks.y = element_line(),
    axis.text.x = element_text(size = 20),
    axis.title.x = element_text(size = 25),  # Increase x-axis title size
    axis.title.y = element_text(size = 25),  # Increase y-axis title size
    legend.title = element_text(size = 25),  # Increase legend title size
    legend.text = element_text(size = 20)
  )
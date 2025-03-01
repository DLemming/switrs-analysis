################### Rank cities by collisions ###################
city_collision_counts <- dbGetQuery(db, "
  SELECT
    COUNT(*) AS collisions,
    city_location AS city
  FROM collisions
  WHERE
    city_location IS NOT NULL
    AND city_population > 0
  GROUP BY city_location
  ORDER BY collisions DESC;")

# Get the top 10 cities
top_10_cities <- head(city_collision_counts, 10)
bottom_10_cities <- tail(city_collision_counts, 10)

# Create a bar chart
ggplot(top_10_cities, aes(x = reorder(city, -collisions), y = collisions/1000)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  theme_minimal() +
  labs(title = "Top 10 Cities: Most Collisions",
       x = "City",
       y = "Collisions (in Thousands)") +
  theme(
    plot.title = element_text(size = 30, face = "bold", hjust = 0.5),
    axis.text.x = element_text(angle = 35, hjust = 1, size=15),
    axis.text.y = element_text(size = 20),
    axis.title.x = element_text(size = 25), # Increase x-axis title size
    axis.title.y = element_text(size = 25), # Increase y-axis title size
    legend.title = element_text(size = 25), # Increase legend title size
    legend.text = element_text(size = 20)
  )

# Alternative horizontal
ggplot(top_10_cities, aes(x = reorder(city, collisions), y = collisions/1000)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  theme_minimal() +
  labs(title = "Top 10 Cities with the Most Collisions",
       x = "City",
       y = "Collisions (in Thousands)")


################### Rank cities by relative collisions ###################
collision_population <- dbGetQuery(db, "
  SELECT 
    city_location AS city,
    city_population AS population,
    COUNT(*) AS collision_count,
    year
  FROM collisions
  WHERE
    city_location IS NOT NULL
    AND city_population IS NOT NULL
    AND city_population > 100000
  GROUP BY city_location, year
  ORDER BY city_location, year;
")

# Calculate collision rate for each year (collisions per 100,000 people)
collision_population$relative_rate <- (collision_population$collision_count / collision_population$population) * 100000

# Aggregate by city and calculate the average collision rate
top_relative_cities <- collision_population %>%
  group_by(city) %>%
  summarise(
    avg_relative_rate = mean(relative_rate, na.rm = TRUE)
  ) %>%
  arrange(desc(avg_relative_rate)) %>%
  slice_head(n = 10)  # Get the top 10 cities

# Create the bar plot
ggplot(top_relative_cities, aes(x = reorder(city, -avg_relative_rate), y = avg_relative_rate)) +
  geom_bar(stat = "identity", fill = "tomato") +
  theme_minimal() +
  labs(title = "Top 10 Big Cities: Highest Collision Rates",
       x = "City",
       y = "Average Collisions (per 100k per Year)") +
  theme(
    plot.title = element_text(size = 30, face = "bold", hjust = 0.5),
    axis.text.x = element_text(angle = 35, hjust = 1, size=15),
    axis.text.y = element_text(size = 20),
    axis.title.x = element_text(size = 25), # Increase x-axis title size
    axis.title.y = element_text(size = 25), # Increase y-axis title size
    legend.title = element_text(size = 25), # Increase legend title size
    legend.text = element_text(size = 20)
  )
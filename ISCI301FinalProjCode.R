install.packages("ggplot2")
install.packages("dplyr")
install.packages("devtools")

library(ggplot2)
library(dplyr)


devtools::install_github(repo = "ryurko/nflscrapR")


install.packages("teamcolors")
library(teamcolors)



data_week4 <- read.csv("nfloffenseweek4.csv")
data_week5 <- read.csv("nfloffenseweek5.csv")
data_week6 <- read.csv("nfloffenseweek6.csv")


data_combined <- rbind(data_week4, data_week5, data_week6)


ggplot(data_combined, aes(x = total_yards, y = points_scored)) +
  geom_point(color = "blue", alpha = 0.7) +  # Points
  geom_smooth(method = "lm", color = "red", se = FALSE) +  # Trendline
  labs(
    title = "Total Yards vs. Points Scored (NFL Teams)",
    x = "Total Yards",
    y = "Points Scored"
  ) +
  theme_minimal()

median_points <- median(data_combined$points_scored)
data_combined <- data_combined %>%
  mutate(
    sentiment = ifelse(points_scored > median_points, "Positive", "Negative")
  )


ggplot(data_combined, aes(x = total_yards, y = points_scored, color = sentiment)) +
  geom_point(alpha = 0.7, size = 3) +  # Points with sentiment colors
  geom_smooth(method = "lm", color = "black", se = FALSE) +  # Trendline
  labs(
    title = "Total Yards vs. Points Scored (with Sentiment)",
    x = "Total Yards",
    y = "Points Scored",
    color = "Sentiment"
  ) +
  theme_minimal()

mean_points <- mean(data_combined$points_scored, na.rm = TRUE)
mean_yards <- mean(data_combined$total_yards, na.rm = TRUE)

mean_points
mean_yards

ggplot(data_combined, aes(x = total_yards, y = points_scored, color = sentiment)) +
  geom_point(alpha = 0.7, size = 3) +  # Points with sentiment colors
  geom_smooth(method = "lm", color = "black", se = FALSE) +  # Trendline
  geom_vline(xintercept = mean_yards, linetype = "dashed", color = "blue", size = 1) +  # Mean total yards
  geom_hline(yintercept = mean_points, linetype = "dashed", color = "red", size = 1) +  # Mean points scored
  labs(
    title = "Total Yards vs. Points Scored (with Sentiment and Means)",
    x = "Total Yards",
    y = "Points Scored",
    color = "Sentiment"
  ) +
  annotate("text", x = mean_yards + 50, y = max(data_combined$points_scored), 
           label = paste("Mean Yards:", round(mean_yards, 1)), color = "blue", hjust = 0) +
  annotate("text", x = max(data_combined$total_yards), y = mean_points + 3, 
           label = paste("Mean Points:", round(mean_points, 1)), color = "red", hjust = 1) +
  theme_minimal()

mean_fumbles_week4 <- mean(data_week4$fumbles, na.rm = TRUE)
mean_fumbles_week5 <- mean(data_week5$fumbles, na.rm = TRUE)
mean_fumbles_week6 <- mean(data_week6$fumbles, na.rm = TRUE)


mean_fumbles_week4
mean_fumbles_week5
mean_fumbles_week6

top_point_scorers <- head(data_week6$team, n=3)
top_point_scorers
low_point_scorers <- tail(data_week6$team, n=3)
low_point_scorers

topturnovers <- head(data_week6$turnovers_lost, n=3)
topturnovers
bottomturnovers <- tail(data_week6$turnovers_lost, n=3)
bottomturnovers

mean_tturnovers <- mean(topturnovers)
mean_tturnovers
meanbturnovers <- mean(bottomturnovers)
meanbturnovers


nfl_data <- data.frame(
  Team = c("Chiefs", "Bills", "Eagles", "Bears", "Broncos", "Texans"),
  Fumbles = c(6,10,2,10,7,5)  # Replace with actual fumble data
)

sorted_data <- nfl_data[order(nfl_data$Fumbles), ]

#top 3 and bottom 3 teams
top_3 <- sorted_data[1:3, ]
bottom_3 <- sorted_data[(nrow(sorted_data)-2):nrow(sorted_data), ]

# Combine top and bottom teams for comparison
comparison_data <- rbind(top_3, bottom_3)
group <- c(rep("Top 3 Teams", nrow(top_3)), rep("Bottom 3 Teams", nrow(bottom_3)))
comparison_data$Group <- group

ggplot(comparison_data, aes(x = Fumbles, fill = Group)) +
  geom_histogram(binwidth = 1, position = "dodge", color = "black") +
  labs(
    title = "Fumbles: Top 3 Teams vs Bottom 3 Teams",
    x = "Number of Fumbles",
    y = "Frequency"
  ) +
  theme_minimal() +
  scale_fill_manual(values = c("Top 3 Teams" = "blue", "Bottom 3 Teams" = "red"))

median_fumbles <- median(data_combined$fumbles, na.rm = TRUE)

ggplot(data_combined, aes(x = median_fumbles)) +
  geom_histogram(binwidth = 1, fill = "lightblue", color = "black", alpha = 0.7) +  # Histogram for fumbles
  geom_vline(xintercept = median_fumbles, color = "red", linetype = "dashed", size = 1) +  # Median line
  labs(
    title = "Distribution of Fumbles Across All Weeks",
    x = "Fumbles",
    y = "Frequency"
  ) +
  annotate("text", x = median_fumbles + 1, y = 5, 
           label = paste("Median Fumbles:", round(median_fumbles, 1)), color = "red", hjust = 0) +
  theme_minimal()


summary_stats <- df %>%
  select(points_scored, total_yards) %>%
  summary()

print("Summary Statistics for points_scored and total_yards:")
print(summary_stats)

# 3. Bar Chart comparing points_scored and total_yards across teams
df_long <- df %>%
  select(team, points_scored, total_yards) %>%
  pivot_longer(cols = c(points_scored, total_yards), names_to = "metric", values_to = "value")  # Reshape data to long format

bar_chart <- ggplot(df_long, aes(x = reorder(team, -value), y = value, fill = metric)) +
  geom_bar(stat = "identity", position = "dodge") +  # Use dodge to display bars side by side
  labs(title = "Comparison of Points Scored and Total Yards Across Teams",
       x = "Team", y = "Value") +
  scale_fill_manual(values = c("points_scored" = "skyblue", "total_yards" = "orange")) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))  # Rotate team names

# Print the bar chart
print(bar_chart)

df <- df %>%
  mutate(sentiment = ifelse(points_scored > 125, "Positive", "Negative"))


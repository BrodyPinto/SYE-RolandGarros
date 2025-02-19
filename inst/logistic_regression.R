## Logistic Regression Exploratory Models
library(tidyverse)

all_matches_importance |> View()

df <- nadal_2022 |>
  filter(serverId == "Rafael Nadal") |>
  mutate(nadal_win = if_else(scorerId == "Rafael Nadal", true = 1, false = 0))

View(df)

mod1 <- glm(nadal_win ~ ballSpeed + receiverId, data = df, family = "binomial")
summary(mod1)

# Create a sequence of ball speeds for prediction
ballSpeed_seq <- data.frame(ballSpeed = seq(min(df$ballSpeed, na.rm = TRUE),
                                            max(df$ballSpeed, na.rm = TRUE),
                                            length.out = 100))

# Set a reference receiver (e.g., Casper Ruud) for predictions
ballSpeed_seq$receiverId <- "Casper Ruud"

# Get predicted probabilities
ballSpeed_seq$nadal_win_prob <- predict(mod1, newdata = ballSpeed_seq, type = "response")

# Ball Speed Plot
ggplot(df, aes(x = ballSpeed, y = nadal_win)) +
  geom_jitter(height = 0.05, alpha = 0.3) +  # Jitter to spread points
  geom_line(data = ballSpeed_seq, aes(x = ballSpeed, y = nadal_win_prob), color = "blue", size = 1) +
  labs(title = "Effect of Ball Speed on Nadal Winning the Point",
       x = "Ball Speed",
       y = "Probability of Winning the Point") +
  theme_minimal()

# Create a new data frame for predictions
receiver_df <- data.frame(ballSpeed = mean(df$ballSpeed, na.rm = TRUE),  # Use mean ball speed
                          receiverId = unique(df$receiverId))

# Get predicted probabilities
receiver_df$nadal_win_prob <- predict(mod1, newdata = receiver_df, type = "response")

# Plot
ggplot(receiver_df, aes(x = receiverId, y = nadal_win_prob, fill = receiverId)) +
  geom_col() +
  labs(title = "Effect of Receiver on Nadal Winning the Point",
       x = "Receiver",
       y = "Probability of Winning the Point") +
  theme_minimal() +
  theme(legend.position = "none")  # Remove legend since x-axis already labels opponents

## Trying out the visreg package
library(visreg)

# Effect of ball speed
visreg(mod1, "ballSpeed", scale = "response", gg = TRUE)

# Effect of receiverId
visreg(mod1, "receiverId", scale = "response", gg = TRUE)




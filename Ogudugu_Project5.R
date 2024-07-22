# Kevin Ogudugu
# Project 5
# ALY 6000

cat("\014") # clears console
rm(list = ls()) # clears global environment
try(dev.off(dev.list()["RStudioGD"]), silent = TRUE) # clears plots
try(p_unload(p_loaded(), character.only = TRUE), silent = TRUE) #
clears packages
options(scipen = 100) # disables scientific notion for entire R session

install.packages("dplyr")
install.packages("tibble")
library(dplyr)
library(tibble)
library(ggplot2)


freq_color <- ball_dataset %>% 
  group_by(color) %>% 
  summarise(counts = n()) %>% 
  as_tibble()

print(freq_color)

freq_label <- ball_dataset %>% 
  group_by(label) %>% 
  summarise(counts = n()) %>% 
  as_tibble()

print(freq_label)

ggplot(freq_color, aes(x = color, y = counts)) +
  geom_bar(stat = "identity", fill = c("blue", "green", "red", "yellow")) +
  labs(title = "Color Counts of Balls", x = "Color", y = "Count") +
  theme_minimal()

ggplot(freq_label, aes(x = label, y = counts)) +
  geom_bar(stat = "identity", fill = c("red", "#808000", "green", "blue", "purple")) +
  labs(title = "Label Counts of Balls", x = "Label", y = "Count") +
  theme_minimal()

A_count <- 75
B_count <- 275
C_count <- 50
D_count <- 325
E_count <- 125

total_balls <- A_count + B_count + C_count + D_count + E_count


prob_A_or_C <- (A_count + C_count) / total_balls


print(paste0("Probability of drawing a ball with label A or C: ", round(prob_A_or_C, 4)))


prob_yellow_D <- D_count / total_balls


print(paste0("Probability of drawing a yellow ball with a D: ", round(prob_yellow_D, 4)))


factorial_function <- function(n) {
  if (n < 0) {
    return(-1)
  } else if (n == 0) {
    return(0)
  } else {
    result <- 1
    for (i in 1:n) {
      result <- result * i
    }
    return(result)
  }
}

print(factorial_function(-10)) 

#Coins

library(tibble)

coin_outcomes <- tribble(
  ~first, ~second, ~third, ~fourth,
  "H", "H", "H", "H",
  "H", "H", "H", "T",
  "H", "H", "T", "H",
  "H", "H", "T", "T",
  "H", "T", "H", "H",
  "H", "T", "H", "T",
  "H", "T", "T", "H",
  "H", "T", "T", "T",
  "T", "H", "H", "H",
  "T", "H", "H", "T",
  "T", "H", "T", "H",
  "T", "H", "T", "T",
  "T", "T", "H", "H",
  "T", "T", "H", "T",
  "T", "T", "T", "H",
  "T", "T", "T", "T"
)

print(coin_outcomes)



coin_outcomes <- tribble(
  ~first, ~second, ~third, ~fourth,
  "H", "H", "H", "H",
  "H", "H", "H", "T",
  "H", "H", "T", "H",
  "H", "H", "T", "T",
  "H", "T", "H", "H",
  "H", "T", "H", "T",
  "H", "T", "T", "H",
  "H", "T", "T", "T",
  "T", "H", "H", "H",
  "T", "H", "H", "T",
  "T", "H", "T", "H",
  "T", "H", "T", "T",
  "T", "T", "H", "H",
  "T", "T", "H", "T",
  "T", "T", "T", "H",
  "T", "T", "T", "T"
)


row_prob <- function(row) {
  prod(ifelse(row == "H", 0.6, 0.4))
}

coin_outcomes <- coin_outcomes %>%
  rowwise() %>%
  mutate(prob = row_prob(c(first, second, third, fourth)))

coin_outcomes <- coin_outcomes %>%
  mutate(num_heads = sum(first == "H", second == "H", third == "H", fourth == "H"))

num_heads_prob <- coin_outcomes %>%
  group_by(num_heads) %>%
  summarize(total_prob = sum(prob))

print(coin_outcomes)
print(num_heads_prob)

coin_outcomes <- matrix(c("H", "H", "H", "H",
                          "H", "H", "H", "T",
                          "H", "H", "T", "H",
                          "H", "T", "H", "H",
                          "T", "H", "H", "H",
                          "H", "H", "T", "T",
                          "H", "T", "H", "T",
                          "H", "T", "T", "H",
                          "T", "H", "H", "T",
                          "T", "H", "T", "H",
                          "T", "T", "H", "H",
                          "H", "T", "T", "T",
                          "T", "H", "T", "T",
                          "T", "T", "H", "T",
                          "T", "T", "T", "H",
                          "T", "T", "T", "T"), ncol=4, byrow=TRUE)

num_heads <- rowSums(coin_outcomes == "H")

prob21_result <- sum(num_heads == 3) / nrow(coin_outcomes)
prob22_result <- sum(num_heads %in% c(2, 4)) / nrow(coin_outcomes)
prob23_result <- sum(num_heads <= 3) / nrow(coin_outcomes)

prob21_result
prob22_result
prob23_result

df <- as.data.frame(prob_distribution)
colnames(df) <- c("Number_of_Heads", "Probability")

ggplot(df, aes(x = Number_of_Heads, y = Probability)) +
  geom_bar(stat = "identity", fill = "cyan") +
  labs(title = "Probability Distribution of Heads for 4 flips",
       x = "Number of Heads", 
       y = "Probability") +
  theme_minimal()

# Soccer

home_prob <- 0.75
away_prob <- 0.50

prob25_result <- (home_prob^5) * (away_prob^5)

prob26_result <- 1 - (choose(5, 0) * (home_prob^0) * ((1-home_prob)^5) * (away_prob^5) +
                   choose(5, 0) * (home_prob^5) * (away_prob^0) * ((1-away_prob)^5) +
                   choose(5, 1) * (home_prob^1) * ((1-home_prob)^4) * (away_prob^4) +
                   choose(5, 1) * (home_prob^4) * (away_prob^1) * ((1-away_prob)^4))

prob27_result <- choose(5, 3) * choose(5, 2)


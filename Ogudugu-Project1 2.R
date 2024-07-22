# Name: Kevin Ogudugu
# Date: September 24, 2023
# Class: ALY 6000
cat("\014") # clears console
rm(list = ls()) # clears global environment
try(dev.off(dev.list()["RStudioGD"]), silent = TRUE) # clears plots
try(p_unload(p_loaded(), character.only = TRUE), silent = TRUE) #
clears packages
options(scipen = 100) # disables scientific notation for entire R
sessions

library(pacman)
p_load(testthat)
test_file("project1_tests.R")

result1 <- 123 * 453
result2 <- 5^2 * 40
result3 <- TRUE & FALSE
result4 <- TRUE | FALSE
result5 <- 75 %% 10
result6 <- 75 / 10

first_vector <- c(17, 12, -33, 5)

counting_by_fives <- c(5, 10, 15, 20, 25, 30, 35)

second_vector <- seq(from = 10, to = 30, by = 2)

counting_by_fives_with_seq <- seq(from = 5, to = 35, by = 5)

third_vector <- rep(first_vector, times = 10)

rep_vector <- rep(0, times = 20)

fourth_vector <- 10:1

counting_vector <- 5:15

grades <- c(96, 100, 85, 92, 81, 72)

bonus_points_added <- grades + 3

one_to_one_hundred <- 1:100

reverse_numbers <- seq(from = 100, to = -100, by = -3)

# Add 20 to every element in second_vector
result1 <- second_vector + 20

# Multiply every element in second_vector by 20
result2 <- second_vector * 20

# Check if each element in second_vector is greater than or equal to 20
result3 <- second_vector >= 20

# Check if each element in second_vector is not equal to 20
result4 <- second_vector != 20

total <- sum(one_to_one_hundred)

average_value <- mean(one_to_one_hundred)

median_value <- median(one_to_one_hundred)

max_value <- max(one_to_one_hundred)

min_value <- min(one_to_one_hundred)

first_value <- second_vector[1]

first_three_values <- second_vector[1:3]

vector_from_brackets <- second_vector[c(1, 5, 10, 11)]

vector_from_boolean_brackets <- first_vector[c(FALSE, TRUE, FALSE, TRUE)]

second_vector >= 20

ages_vector <- seq(from = 10, to = 30, by = 2)

ages_vector[ages_vector >= 20]

lowest_grades_removed <- grades[grades > 85]

middle_grades_removed <- grades[-c(3, 4)]

fifth_vector <- second_vector[-c(5, 10)]

set.seed(5)
random_vector <- runif(n = 10, min = 0, max = 1000)

sum_vector <- sum(random_vector)

cumsum_vector <- cumsum(random_vector)

mean_vector <- mean(random_vector)

sd_vector <- sd(random_vector)

round_vector <- round(random_vector)

sort_vector <- sort(random_vector)

set.seed(5)
random_vector <- rnorm(n = 1000, mean = 50, sd = 15)

hist(random_vector)

library(pacman)
p_load(tidyverse)

library(tidyverse)

first_dataframe <- read_csv(ds_salaries.cvs)

# Kevin Ogudugu
# Project 3
# ALY 6000

cat("\014") # clears console
rm(list = ls()) # clears global environment
try(dev.off(dev.list()["RStudioGD"]), silent = TRUE) # clears plots
try(p_unload(p_loaded(), character.only = TRUE), silent = TRUE) #
clears packages
options(scipen = 100) # disables scientific notation for entire R
session


library(janitor)

books <- clean_names(books)


library(lubridate)

books$first_publish_date <- mdy(books$first_publish_date)

books$year <- year(books$first_publish_date)

head(books)

library(dplyr)

books <- books %>%
  filter(year >= 1990 & year <= 2020)

books <- books %>%
  select(-publish_date, -edition, -characters, -price, -genres, -setting, -isbn)

library(dplyr)

books <- books %>%
  filter(pages < 1200)

glimpse(books)

summary(books)


library(ggplot2)

ggplot(books, aes(x=rating)) + 
  geom_histogram(fill="red", binwidth=0.25) +
  labs(title="Histogram of Book Ratings", x="Rating", y="Number of Books") +
  theme_bw()

}
library(ggplot2)
library(ggthemes)

# Create the boxplot
ggplot(books, aes(y=pages)) + 
  geom_boxplot(fill="magenta") +
  labs(title="Box Plot of Page Counts", x="Pages") +
  coord_flip() +
  theme_economist()

library(dplyr)
summary_df <- books %>%
  group_by(publisher) %>%
  summarise(num_books = n()) %>%
  
  filter(!is.na(publisher)) %>%
  
  filter(num_books >= 250) %>%
  
  arrange(desc(num_books)) %>%
  
  mutate(publisher = factor(publisher, levels = publisher)) %>%
  
  mutate(cumulative_books = cumsum(num_books)) %>%
  
  mutate(rel_frequency = num_books / sum(num_books)) %>%
  
  mutate(cum_rel_frequency = cumsum(rel_frequency))

summary_df

library(ggplot2)

pareto_plot <- ggplot(summary_df, aes(x = reorder(publisher, -num_books), y = num_books)) +
  
  geom_bar(stat = "identity", fill = "cyan") +
  
  geom_line(aes(y = cumulative_books, group = 1), color = "blue") +
  geom_point(aes(y = cumulative_books, group = 1), color = "blue") +
  
  labs(x = "Publisher",
       y = "Number of Books",
       title = "Pareto and Ogive of Publisher Book Counts (1990 - 2020)") +
  
  theme_clean() +
  
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 

print(pareto_plot)

library(ggplot2)

scatter_plot <- ggplot(books, aes(x = pages, y = rating, color = as.factor(year))) +
  
  geom_point(alpha = 0.6, size = 2) +
  
  labs(x = "Pages",
       y = "Rating",
       title = "Scatter Plot of Pages vs. Rating",
       color = "Year of Publication") +
  
  theme_tufte()

print(scatter_plot)

library(dplyr)

books_by_year <- books %>%
  group_by(year) %>%
  summarise(
    num_books = n(),  # Count the number of books per year
    avg_rating = mean(rating, na.rm = TRUE)  # Average rating per year
  ) %>%
  ungroup()  # Remove grouping

print(books_by_year)

library(ggplot2)
library(ggthemes)  

filtered_books_by_year <- books_by_year %>%
  filter(year >= 1990 & year <= 2020)

plot <- ggplot(filtered_books_by_year, aes(x = year, y = num_books, color = avg_rating)) +
  geom_line(aes(group = 1)) +
  geom_point(size = 3) +
  scale_color_gradient(low = "blue", high = "red") +
  labs(title = "Total Number of Books Rated Per Year", x = "Year", y = "Number of Books") +
  theme_excel_new() +
  theme(legend.title = element_text("Average Rating"))

print(plot)


average <- function(x) {
  sum(x) / length(x)
}

pop_var <- function(x) {
  mu <- average(x)
  sum((x - mu)^2) / length(x)
}

pop_sd <- function(x) {
  sqrt(pop_var(x))
}

vec <- c(1, 2, 3, 4, 5)

cat("Average:", average(vec), "\n")
cat("Population Variance:", pop_var(vec), "\n")
cat("Population Standard Deviation:", pop_sd(vec), "\n")

avg_rating <- average(books$rating)
variance_rating <- pop_var(books$rating)
std_dev_rating <- pop_sd(books$rating)

cat("Population Average Rating:", avg_rating, "\n")
cat("Population Variance of Rating:", variance_rating, "\n")
cat("Population Standard Deviation of Rating:", std_dev_rating, "\n")


sample1 <- books[sample(1:nrow(books), 100), ]
sample1_mean <- mean(sample1$rating)
sample1_var <- var(sample1$rating)
sample1_sd <- sd(sample1$rating)

sample2 <- books[sample(1:nrow(books), 100), ]
sample2_mean <- mean(sample2$rating)
sample2_var <- var(sample2$rating)
sample2_sd <- sd(sample2$rating)

sample3 <- books[sample(1:nrow(books), 100), ]
sample3_mean <- mean(sample3$rating)
sample3_var <- var(sample3$rating)
sample3_sd <- sd(sample3$rating)

cat("Sample 1 - Mean Rating:", sample1_mean, "\n")
cat("Sample 1 - Variance of Rating:", sample1_var, "\n")
cat("Sample 1 - Standard Deviation of Rating:", sample1_sd, "\n\n")

cat("Sample 2 - Mean Rating:", sample2_mean, "\n")
cat("Sample 2 - Variance of Rating:", sample2_var, "\n")
cat("Sample 2 - Standard Deviation of Rating:", sample2_sd, "\n\n")

cat("Sample 3 - Mean Rating:", sample3_mean, "\n")
cat("Sample 3 - Variance of Rating:", sample3_var, "\n")
cat("Sample 3 - Standard Deviation of Rating:", sample3_sd, "\n")


books_by_year <- books %>%
  group_by(year) %>%
  summarize(avg_rating = mean(rating, na.rm=TRUE))

ggplot(books_by_year, aes(x=year, y=avg_rating)) +
  geom_line(color="red") +
  geom_point() +
  theme_minimal() +
  labs(title="Average Rating by Year",
       x="Year", y="Average Rating")



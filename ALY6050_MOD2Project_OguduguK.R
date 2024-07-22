#part 1

n_sims <- 10000


benefits_dam1 <- data.frame(
  category = c("Improved navigation", "Hydroelectric power", "Fish and wildlife", "Recreation", "Flood control", "Commercial development"),
  min = c(1.1, 8, 1.4, 6.5, 1.7, 0),
  mode = c(2, 12, 1.4, 9.8, 2.4, 1.6),
  max = c(2.8, 14.9, 2.2, 14.6, 3.6, 2.4)
)

costs_dam1 <- data.frame(
  category = c("Annualized capital cost", "Operations & Maintenance"),
  min = c(13.2, 3.5),
  mode = c(14.2, 4.9),
  max = c(19.1, 7.4)
)


benefits_dam2 <- data.frame(
  category = c("Improved navigation", "Hydroelectric power", "Fish and wildlife", "Recreation", "Flood control", "Commercial development"),
  min = c(2.1, 8.7, 2.3, 5.9, 3, 0),
  mode = c(3, 12.2, 3, 8.7, 3.4, 1.2),
  max = c(4.8, 13.6, 3, 15, 3.4, 1.8)
)

costs_dam2 <- data.frame(
  category = c("Annualized capital cost", "Operations & Maintenance"),
  min = c(12.8, 3.8),
  mode = c(15.8, 5.7),
  max = c(20.1, 8)
)


simulate_bcr <- function(benefits, costs, n_sims) {
  total_benefits <- replicate(n_sims, {
    sum(mapply(function(min, max) {
      runif(1, min = min, max = max)
    }, benefits$min, benefits$max))
  })
  
  total_costs <- replicate(n_sims, {
    sum(mapply(function(min, max) {
      runif(1, min = min, max = max)
    }, costs$min, costs$max))
  })
  
  bcr <- total_benefits / total_costs
  return(bcr)
}


bcr_dam1 <- simulate_bcr(benefits_dam1, costs_dam1, n_sims)
bcr_dam2 <- simulate_bcr(benefits_dam2, costs_dam2, n_sims)


create_plot <- function(bcr, title) {
  df <- data.frame(bcr = bcr)
  p <- ggplot(df, aes(x = bcr)) +
    geom_histogram(binwidth = 0.1, fill = "blue", alpha = 0.7) +
    theme_minimal() +
    labs(title = title, x = "Benefit-Cost Ratio", y = "Frequency")
  return(p)
}

plot_dam1 <- create_plot(bcr_dam1, "Benefit-Cost Ratio Distribution for Dam #1")
plot_dam2 <- create_plot(bcr_dam2, "Benefit-Cost Ratio Distribution for Dam #2")


print(plot_dam1)
print(plot_dam2)


dam1_required_stats <- data.frame(
  Measure = c("Mean of the Total Benefits", "SD of the Total Benefits", 
              "Mean of the Total Cost", "SD of the Total Cost", 
              "Mean of the Benefit-cost Ratio", "SD of the Benefit-cost Ratio"),
  Observed = c(stats_dam1$Mean, stats_dam1$Std_Dev, 
               stats_dam1$Mean, stats_dam1$Std_Dev, 
               stats_dam1$Mean, stats_dam1$Std_Dev),
  Theoretical = rep(NA, 6)  # Assuming theoretical values are not provided
)

dam2_required_stats <- data.frame(
  Measure = c("Mean of the Total Benefits", "SD of the Total Benefits", 
              "Mean of the Total Cost", "SD of the Total Cost", 
              "Mean of the Benefit-cost Ratio", "SD of the Benefit-cost Ratio"),
  Observed = c(stats_dam2$Mean, stats_dam2$Std_Dev, 
               stats_dam2$Mean, stats_dam2$Std_Dev, 
               stats_dam2$Mean, stats_dam2$Std_Dev),
  Theoretical = rep(NA, 6)
)


print(dam1_required_stats)
print(dam2_required_stats)


#part 2

perform_chi_squared_test <- function(observed_bcr, num_bins) {

  hist_result <- hist(observed_bcr, breaks = num_bins, plot = FALSE)
  observed_freq <- hist_result$counts
  bin_breaks <- hist_result$breaks
  
  
  fit <- fitdistr(observed_bcr, "normal")
  fit_mean <- fit$estimate["mean"]
  fit_sd <- fit$estimate["sd"]
  
  
  expected_probs <- diff(pnorm(bin_breaks, mean = fit_mean, sd = fit_sd))
  expected_freq <- expected_probs * length(observed_bcr)
  
  
  chisq_test <- chisq.test(observed_freq, p = expected_probs, rescale.p = TRUE)
  
  return(list(
    chisq_test = chisq_test,
    fit_mean = fit_mean,
    fit_sd = fit_sd,
    observed_freq = observed_freq,
    expected_freq = expected_freq,
    bin_breaks = bin_breaks
  ))
}

chi_squared_result_dam1 <- perform_chi_squared_test(bcr_dam1, num_bins)

chi_squared_result_dam2 <- perform_chi_squared_test(bcr_dam2, num_bins)


cat("Chi-squared test results for Dam #1:\n")
cat("Test Statistic:", chi_squared_result_dam1$chisq_test$statistic, "\n")
cat("P-value:", chi_squared_result_dam1$chisq_test$p.value, "\n\n")

cat("Chi-squared test results for Dam #2:\n")
cat("Test Statistic:", chi_squared_result_dam2$chisq_test$statistic, "\n")
cat("P-value:", chi_squared_result_dam2$chisq_test$p.value, "\n\n")

df_dam1 <- data.frame(
  Bin = 1:length(chi_squared_result_dam1$observed_freq),
  Observed = chi_squared_result_dam1$observed_freq,
  Expected = chi_squared_result_dam1$expected_freq
)

p1 <- ggplot(df_dam1, aes(x = Bin)) +
  geom_bar(aes(y = Observed), stat = "identity", fill = "blue", alpha = 0.7) +
  geom_line(aes(y = Expected), color = "red", size = 1) +
  theme_minimal() +
  labs(title = "Observed vs Expected Frequencies for Dam #1", x = "Bin", y = "Frequency")

print(p1)


df_dam2 <- data.frame(
  Bin = 1:length(chi_squared_result_dam2$observed_freq),
  Observed = chi_squared_result_dam2$observed_freq,
  Expected = chi_squared_result_dam2$expected_freq
)

p2 <- ggplot(df_dam2, aes(x = Bin)) +
  geom_bar(aes(y = Observed), stat = "identity", fill = "blue", alpha = 0.7) +
  geom_line(aes(y = Expected), color = "red", size = 1) +
  theme_minimal() +
  labs(title = "Observed vs Expected Frequencies for Dam #2", x = "Bin", y = "Frequency")

print(p2)

# part 3


calculate_statistics <- function(bcr) {
  stats <- data.frame(
    Minimum = min(bcr),
    Maximum = max(bcr),
    Mean = mean(bcr),
    Median = median(bcr),
    Variance = var(bcr),
    Std_Dev = sd(bcr),
    Skewness = skewness(bcr)
  )
  
  probabilities <- data.frame(
    P_alpha_greater_2 = mean(bcr > 2),
    P_alpha_greater_1_8 = mean(bcr > 1.8),
    P_alpha_greater_1_5 = mean(bcr > 1.5),
    P_alpha_greater_1_2 = mean(bcr > 1.2),
    P_alpha_greater_1 = mean(bcr > 1)
  )
  
  return(cbind(stats, probabilities))
}

stats_dam1 <- calculate_statistics(bcr_dam1)

stats_dam2 <- calculate_statistics(bcr_dam2)


cat("Statistics for Dam #1 (α1):\n")
print(stats_dam1)
cat("\nStatistics for Dam #2 (α2):\n")
print(stats_dam2)


prob_alpha1_greater_alpha2 <- mean(bcr_dam1 > bcr_dam2)
cat("\nProbability that α1 (Dam #1) is greater than α2 (Dam #2):", prob_alpha1_greater_alpha2, "\n")


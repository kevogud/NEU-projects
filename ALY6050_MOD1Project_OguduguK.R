# part 1
p_home = 0.6        
p_away = 0.43       


win_12 = p_home * p_away
win_132 = p_home * (1 - p_away) * p_home
win_213 = (1 - p_home) * p_away * p_home


p_win_series = win_12 + win_132 + win_213


net_wins = c(1000, 480, -1040)
probabilities = c(win_12, win_132 + win_213, 1 - p_win_series)


expected_win = sum(net_wins * probabilities)
std_dev = sqrt(sum((net_wins - expected_win)^2 * probabilities))


set.seed(123)
simulated_wins = sample(net_wins, size = 10000, replace = TRUE, prob = probabilities)
mean_simulated = mean(simulated_wins)
sd_simulated = sd(simulated_wins)
conf_interval = mean_simulated + c(-1, 1) * sd_simulated / sqrt(10000) * qnorm(0.975)

table_simulated = table(simulated_wins)
chisq_test_results = chisq.test(x = table_simulated, p = probabilities)

cat("Probability Red Sox wins the series:", p_win_series, "\n")
cat("Expected net win:", expected_win, "\n")
cat("Standard deviation of net win:", std_dev, "\n")
cat("Mean of simulated wins:", mean_simulated, "\n")
cat("95% confidence interval for mean simulated wins:", conf_interval, "\n")
cat("Chi-squared test results:", chisq_test_results$statistic, "with p-value", chisq_test_results$p.value, "\n")

#part 2


p_home = 0.6  
p_away = 0.43 


win_12 = (1 - p_home) * p_home 
win_132 = (1 - p_home) * (1 - p_home) * (1 - p_home) 
win_213 = p_home * p_away * (1 - p_home) 


p_win_series = win_12 + win_132 + win_213


net_wins = c(1000, 480, -1040) 
probabilities = c(win_12, win_132 + win_213, 1 - p_win_series)



expected_win = sum(net_wins * probabilities)
std_dev = sqrt(sum((net_wins - expected_win)^2 * probabilities))


set.seed(123)
simulated_wins = sample(net_wins, size = 10000, replace = TRUE, prob = probabilities)
mean_simulated = mean(simulated_wins)
sd_simulated = sd(simulated_wins)
conf_interval = mean_simulated + c(-1, 1) * sd_simulated / sqrt(10000) * qnorm(0.975)


table_simulated = table(simulated_wins)
chisq_test_results = chisq.test(x = table_simulated, p = probabilities)


cat("Probability Red Sox wins the series:", p_win_series, "\n")
cat("Expected net win:", expected_win, "\n")
cat("Standard deviation of net win:", std_dev, "\n")
cat("Mean of simulated wins:", mean_simulated, "\n")
cat("95% confidence interval for mean simulated wins:", conf_interval, "\n")
cat("Chi-squared test results:", chisq_test_results$statistic, "with p-value", chisq_test_results$p.value, "\n")

# part 3



p_home = 0.6  
p_away = 0.43

win_123 = p_home * p_away * p_home * (1 - p_away) * (1 - p_home) + p_home * p_away * p_home * p_away * (1 - p_home) + p_home * p_away * p_home * p_away * p_home
win_124 = p_home * (1 - p_away) * p_home * p_away * (1 - p_home) + p_home * (1 - p_away) * p_home * p_away * p_home
win_134 = p_home * p_away * (1 - p_home) * p_away * (1 - p_home) + p_home * p_away * (1 - p_home) * p_away * p_home
win_135 = p_home * (1 - p_away) * (1 - p_home) * p_away * p_home

p_win_series = win_123 + win_124 + win_134 + win_135

win_net_outcomes = c(1500, 980, 460, -60, -1040) 
probabilities = c(win_123, win_124, win_134, win_135, 1 - p_win_series)  

expected_win = sum(win_net_outcomes * probabilities)
std_dev = sqrt(sum((win_net_outcomes - expected_win)^2 * probabilities))

set.seed(123)
simulated_wins = sample(win_net_outcomes, size = 10000, replace = TRUE, prob = probabilities)
mean_simulated = mean(simulated_wins)
sd_simulated = sd(simulated_wins)
conf_interval = mean_simulated + c(-1, 1) * sd_simulated / sqrt(10000) * qnorm(0.975)

table_simulated = table(simulated_wins)
chisq_test_results = chisq.test(x = table_simulated, p = probabilities)

cat("Probability Red Sox wins the series:", p_win_series, "\n")
cat("Expected net win:", expected_win, "\n")
cat("Standard deviation of net win:", std_dev, "\n")
cat("Mean of simulated wins:", mean_simulated, "\n")
cat("95% confidence interval for mean simulated wins:", conf_interval, "\n")
cat("Chi-squared test results:", chisq_test_results$statistic, "with p-value", chisq_test_results$p.value, "\n")




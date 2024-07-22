# Part I
# 3.

D <- 15000  
C <- 80     
H <- 0.18   
S <- 220    

h <- H * C

total_cost <- function(Q) {
  AOC <- (D / Q) * S
  AHC <- (Q / 2) * h
  TC <- AOC + AHC
  return(TC)
}


Q <- 677
total_cost(Q)

# 4.


Q_values <- seq(100, 1000, by=100)
TC_values <- sapply(Q_values, total_cost)

result <- data.frame(Order_Quantity = Q_values, Total_Cost = TC_values)
print(result)

# 5.

ggplot(result, aes(x = Order_Quantity, y = Total_Cost)) +
  geom_line() +
  labs(title = "Total Cost vs. Order Quantity", x = "Order Quantity", y = "Total Cost")

# Part II
# (i)

library(triangle)
library(fitdistrplus)

C <- 80       
H <- 0.18    
S <- 220     
num_simulations <- 1000

set.seed(123)  
demand_samples <- rtriangle(num_simulations, a=13000, b=17000, c=15000)

total_cost <- function(D) {
  h <- H * C  
  EOQ <- sqrt((2 * D * S) / h)  
  AOC <- (D / EOQ) * S  
  AHC <- (EOQ / 2) * h  
  TC <- AOC + AHC  
  return(TC)
}

total_costs <- sapply(demand_samples, total_cost)

mean_total_cost <- mean(total_costs)
sd_total_cost <- sd(total_costs)
error_margin <- qt(0.975, df=num_simulations-1) * sd_total_cost / sqrt(num_simulations)
confidence_interval <- c(mean_total_cost - error_margin, mean_total_cost + error_margin)

cat("Mean Total Cost:", mean_total_cost, "\n")
cat("95% Confidence Interval:", confidence_interval, "\n")

fit <- fitdist(total_costs, "norm")
summary(fit)

hist(total_costs, breaks=30, probability=TRUE, main="Histogram of Total Costs",
     xlab="Total Cost", col="lightblue")
curve(dnorm(x, mean=fit$estimate[1], sd=fit$estimate[2]), col="darkblue", lwd=2, add=TRUE)

# (ii)

C <- 80       
H <- 0.18     
S <- 220      
num_simulations <- 1000

set.seed(123)  
demand_samples <- rtriangle(num_simulations, a=13000, b=17000, c=15000)

calculate_eoq <- function(D) {
  h <- H * C  
  EOQ <- sqrt((2 * D * S) / h) 
  return(EOQ)
}

order_quantities <- sapply(demand_samples, calculate_eoq)

mean_order_quantity <- mean(order_quantities)
sd_order_quantity <- sd(order_quantities)
error_margin <- qt(0.975, df=num_simulations-1) * sd_order_quantity / sqrt(num_simulations)
confidence_interval <- c(mean_order_quantity - error_margin, mean_order_quantity + error_margin)

cat("Mean Order Quantity:", mean_order_quantity, "\n")
cat("95% Confidence Interval:", confidence_interval, "\n")

fit <- fitdist(order_quantities, "norm")
summary(fit)

hist(order_quantities, breaks=30, probability=TRUE, main="Histogram of Order Quantities",
     xlab="Order Quantity", col="lightblue")
curve(dnorm(x, mean=fit$estimate[1], sd=fit$estimate[2]), col="darkblue", lwd=2, add=TRUE)

# (iii)


C <- 80      
H <- 0.18     
S <- 220      
num_simulations <- 1000

set.seed(123)  
demand_samples <- rtriangle(num_simulations, a=13000, b=17000, c=15000)

calculate_annual_orders <- function(D) {
  h <- H * C  
  EOQ <- sqrt((2 * D * S) / h)  
  annual_orders <- D / EOQ  
  return(annual_orders)
}


annual_orders <- sapply(demand_samples, calculate_annual_orders)

mean_annual_orders <- mean(annual_orders)
sd_annual_orders <- sd(annual_orders)
error_margin <- qt(0.975, df=num_simulations-1) * sd_annual_orders / sqrt(num_simulations)
confidence_interval <- c(mean_annual_orders - error_margin, mean_annual_orders + error_margin)


cat("Mean Annual Number of Orders:", mean_annual_orders, "\n")
cat("95% Confidence Interval:", confidence_interval, "\n")

fit <- fitdist(annual_orders, "norm")  
summary(fit)

hist(annual_orders, breaks=30, probability=TRUE, main="Histogram of Annual Number of Orders",
     xlab="Annual Number of Orders", col="lightblue")
curve(dnorm(x, mean=fit$estimate[1], sd=fit$estimate[2]), col="darkblue", lwd=2, add=TRUE)


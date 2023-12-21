#----
# Modern statistics: Intuition, Math, Python, R
## Mike X Cohen (sincxpress.com)
#### https://www.amazon.com/dp/B0CQRGWGLY
#### Code for chapter 17 (Power and sample sizes)

# About this code file:
### This code file will reproduce most of the figures in this chapter 
### (some figures were made in Inkscape), and illustrate the statistical 
### concepts explained in the text. The point of providing the code is not 
### just for you to recreate the figures, but for you to modify, adapt, 
### explore, and experiment with the code.
###
### Solutions to all exercises are at the bottom of the file.


# install.packages("pwr")
library(pwr)
library(ggplot2)
library(scales)
library(readr)
library(broom)







#-------- Power for a one-sample t-test ---------#
#------------------------------------------------#
# Parameters
xBar  <- 1
h0    <- 0
std   <- 2
n     <- 42 # Sample size
alpha <- 0.05 # Significance level

# Compute the non-centrality parameter
tee <- (xBar - h0) / (std / sqrt(n))

# Critical t-values (2-tailed)
df <- n - 1 # Degrees of freedom for one-sample t-test
t_critL <- qt(alpha / 2, df) # Left critical value
t_critR <- qt(1 - alpha / 2, df) # Right critical value

# Two one-sided power areas
powerL <- pt(t_critL + tee, df) # Power on the left side
powerR <- 1 - pt(t_critR + tee, df) # Power on the right side

# Total power
totalPower <- powerL + powerR

# Reporting results
cat(sprintf("t = %.3f\n", tee))
cat(sprintf("shifted tau-left = %.3f\n", t_critL + tee))
cat(sprintf("shifted tau-right = %.3f\n\n", t_critR + tee))
cat(sprintf("Statistical power: %.4f", totalPower))













#-------- Figure 17.2: Visualization of statistical power for this example ---------#
#-----------------------------------------------------------------------------------#
# Parameters
xBar  <- 1
h0    <- 0
std   <- 2
n     <- 42 # Sample size
alpha <- 0.05 # Significance level
df    <- n - 1 # Degrees of freedom for one-sample t-test

# Compute the non-centrality parameter
tee <- (xBar - h0) / (std / sqrt(n))

# Critical t-values (2-tailed)
t_critL <- qt(alpha / 2, df)
t_critR <- qt(1 - alpha / 2, df)

# Two one-sided power areas
powerL <- pt(t_critL + tee, df)
powerR <- 1 - pt(t_critR + tee, df)
totalPower <- powerL + powerR


### visualization
# t-values for plotting
tvals <- seq(-4, 7, length.out = 401)

# Create data frames for plotting
h0_data <- data.frame(t = tvals, density = dt(tvals, df) * diff(tvals)[1])
ha_data <- data.frame(t = tvals, density = dt(tvals - tee, df) * diff(tvals)[1])

# Create the plot
p <- ggplot() +
  geom_line(data = h0_data, aes(x = t, y = density), color = "black", size = 1) +
  geom_line(data = ha_data, aes(x = t, y = density), color = "grey70", linetype = "dashed", size = 1.5) +
  geom_vline(xintercept = c(t_critL, t_critR), linetype = "dotted", color = "black") +
  annotate("text", x = t_critL - 0.2, y = dt(0, df) * 0.9 * diff(tvals)[1], label = expression(-tau/2), angle = 90, hjust = 0.5, vjust = 0.5) +
  annotate("text", x = t_critR + 0.22, y = dt(0, df) * 0.9 * diff(tvals)[1], label = expression(tau/2), angle = 90, hjust = 0.5, vjust = 0.5) +
  geom_ribbon(data = subset(ha_data, t < t_critL), aes(x = t, ymin = 0, ymax = density), fill = "grey70", alpha = 0.5) +
  geom_ribbon(data = subset(ha_data, t > t_critR), aes(x = t, ymin = 0, ymax = density), fill = "grey70", alpha = 0.5) +
  labs(x = "T-values", y = "Probability", title = sprintf("1-beta = %.3f", totalPower)) +
  theme_minimal()

# Print the plot
print(p)

# Save the plot
ggsave("power_powerExample.png", p, width = 8, height = 4)















#-------- Using the pwr library ---------#
#----------------------------------------#
# Parameters
xBar     <- 1
h0       <- 0
std      <- 2 # Sample standard deviation
sampsize <- 42
alpha    <- .05 # Significance level

# Compute effect size
effectSize <- (xBar - h0) / std

# Calculate power
power_calc <- pwr.t.test(d = effectSize, n = sampsize, 
                         sig.level = alpha, type = "one.sample", alternative = "two.sided")

# Print the power
cat(sprintf("Statistical power using pwr package: %.4f", power_calc$power))













#-------- Sample size for a desired power ---------#
#--------------------------------------------------#
# Parameters
power <- .8   # Desired statistical power level (1-\beta)
h0    <- 0        # Mean if H0 is true
xBar  <- 1      # Sample mean
std   <- 1.5     # Sample standard deviation

# Effect size
effectSize <- (xBar - h0) / std

# Compute sample size
sample_size_calc <- pwr.t.test(d = effectSize, power = power, sig.level = 0.05, type = "one.sample", alternative = "two.sided")

# Report required sample size
cat(sprintf("Required sample size: %d", ceiling(sample_size_calc$n)))

















#-------- Exercise 1 ---------#
#-----------------------------#
# Parameters
std <- 2
sampsize <- 41
xBars <- seq(-2, 2, length.out = 41)

# Initialize results vector
powers <- numeric(length(xBars))

# Run the experiment
for (i in 1:length(xBars)) {
  xm <- xBars[i]
  effectSize <- xm / std
  powers[i] <- pwr.t.test(d = effectSize, n = sampsize, sig.level = 0.05, type = "one.sample", alternative = "two.sided")$power
}

### Plotting
# Create a data frame for plotting
data_to_plot <- data.frame(SampleMean = xBars, Power = powers)

# Generate the plot
p <- ggplot(data_to_plot, aes(x = SampleMean, y = Power)) +
  geom_point(color = "black", fill = "grey70", shape = 22, size = 6) +
  labs(x = expression("Sample mean (H"[0] == "0)"), y = expression("Statistical power (1-" * beta * ")"), 
       title = expression("A) Power by sample mean")) +
  theme_minimal()

# Print the plot
print(p)

# Save the plot
ggsave("power_ex1a.png", p, width = 7, height = 3)


##### next part 
# Parameters
xBar <- 0.5
std <- 2  # Reusing the standard deviation from the previous example
sampleSizes <- seq(10, 250, by = 10)

# Initialize results vector
powers <- numeric(length(sampleSizes))

# The experiment
for (i in 1:length(sampleSizes)) {
  ss <- sampleSizes[i]
  effectSize <- xBar / std
  powers[i] <- pwr.t.test(d = effectSize, n = ss, sig.level = 0.05, type = "one.sample", alternative = "two.sided")$power
}

# Create a data frame for plotting
data_to_plot <- data.frame(SampleSize = sampleSizes, Power = powers)

# Generate the plot
p <- ggplot(data_to_plot, aes(x = SampleSize, y = Power)) +
  geom_point(color = "black", fill = "grey70", shape = 22, size = 6) +
  labs(x = "Sample size", y = expression("Statistical power (1-" * beta * ")"), 
       title = expression("B) Power by sample size")) +
  theme_minimal()

# Print the plot
print(p)

# Save the plot
ggsave("power_ex1b.png", p, width = 7, height = 3)




### putting it all together
xBars <- seq(-2, 2, length.out = 41)
sampleSizes <- seq(10, 250, by = 10)
std <- 2  # Standard deviation

# Initialize the results matrix
powers <- matrix(0, nrow = length(xBars), ncol = length(sampleSizes))

# Run the experiment
for (xi in 1:length(xBars)) {
  for (si in 1:length(sampleSizes)) {
    xm <- xBars[xi]
    ss <- sampleSizes[si]
    effectSize <- xm / std
    powers[xi, si] <- pwr.t.test(d = effectSize, n = ss, sig.level = 0.05, type = "one.sample", alternative = "two.sided")$power
  }
}


# Create a data frame for plotting
powers_df <- expand.grid(xBar = xBars, SampleSize = sampleSizes)
powers_df$Power <- as.vector(powers)

# Generate the heatmap
p <- ggplot(powers_df, aes(x = SampleSize, y = xBar, fill = Power)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "black") +
  labs(x = "Sample size", y = expression("Sample mean (H"[0] == "0)"), fill = "Power") +
  theme_minimal() +
  theme(axis.title.y = element_text(angle = 0))

# Print the plot
print(p)

# Save the plot
ggsave("power_ex1c.png", p, width = 4, height = 5)























#-------- Exercise 2 ---------#
#-----------------------------#
# Parameters
xBars <- seq(-2, 2, length.out = 42)
power <- seq(0.5, 0.95, length.out = 27)
std <- 2  # Standard deviation

# Initialize the results matrix
sampleSizes <- matrix(0, nrow = length(xBars), ncol = length(power))

# Run the experiment
for (xi in 1:length(xBars)) {
  for (pi in 1:length(power)) {
    xm <- xBars[xi]
    pwr <- power[pi]
    effectSize <- xm / std
    sampleSizes[xi, pi] <- pwr.t.test(d = effectSize, power = pwr, sig.level = 0.05, type = "one.sample", alternative = "two.sided")$n
  }
}


# Note: ggplot clips values beyond the color limit, so here I'm rescaling the values
#       to make the resulting heatmap comparable to what Python creates.
min_val <- 10
max_val <- 100
samples_df$RescaledSampleSize <- pmin(pmax(samples_df$SampleSize, min_val), max_val)

# Generate the heatmap with rescaled values
p <- ggplot(samples_df, aes(x = Power, y = EffectSize, fill = RescaledSampleSize)) +
  geom_tile() +
  scale_fill_gradient(low = "black", high = "white") +
  labs(x = "Desired statistical power", y = expression("Effect size (" * overline(x) / s * ")"), fill = "Sample Size") +
  theme_minimal() +
  theme(axis.title.y = element_text(angle = 0))

# Print the plot
print(p)

# Save the plot
ggsave("power_ex2a.png", p, width = 4, height = 5)



### zooming in on a few rows.
rows2plot <- seq(1, nrow(sampleSizes), by = 5)
# rows2plot <- 1:5  

# Convert matrix to long format for ggplot
plot_data <- as.data.frame(sampleSizes[rows2plot, ])
names(plot_data) <- power
plot_data$EffectSize <- xBars[rows2plot] / std
plot_data_long <- reshape2::melt(plot_data, id.vars = "EffectSize", variable.name = "Power", value.name = "SampleSize")

# Create the plot
p <- ggplot(plot_data_long, aes(x = as.numeric(Power), y = SampleSize, group = EffectSize, color = as.factor(EffectSize))) +
  geom_line() +
  geom_point(shape = 21, fill = "white", size = 3) +
  scale_color_discrete(name = "Effect size") +
  theme_minimal() +
  labs(x = "Desired power", y = "Required sample size") +
  theme(legend.position = "right")

# Print the plot
print(p)

# Save the plot
ggsave("power_ex2b.png", p, width = 8, height = 4)



















#-------- Exercise 3 ---------#
#-----------------------------#
# Parameters
std <- 2
sampsize <- 42
xBars <- seq(-2, 2, length.out = 41)

# Initialize results matrix
powers <- matrix(0, nrow = length(xBars), ncol = 3)

# Run the experiment
alphas <- c(0.001, 0.01, 0.1)

for (i in 1:length(xBars)) {
  xm <- xBars[i]
  for (j in 1:length(alphas)) {
    alpha <- alphas[j]
    effectSize <- xm / std
    powers[i, j] <- pwr.t.test(d = effectSize, n = sampsize, sig.level = alpha, type = "one.sample", alternative = "two.sided")$power
  }
}


### plot the results
# Convert matrix to data frame for plotting
plot_data <- as.data.frame(powers)
names(plot_data) <- sprintf("alpha=%.3f", alphas)
plot_data$EffectSize <- xBars / std

# Melt data for ggplot
plot_data_long <- reshape2::melt(plot_data, id.vars = "EffectSize", variable.name = "Alpha", value.name = "Power")

# Create the plot
p <- ggplot(plot_data_long, aes(x = EffectSize, y = Power, color = Alpha)) +
  geom_point(shape = 22, size = 6, aes(fill = Alpha)) +
  geom_hline(yintercept = 0.8, linetype = "dashed", color = "gray") +
  scale_fill_manual(values = c("black", "grey50", "grey80")) +
  scale_color_manual(values = c("black", "grey50", "grey80")) +
  labs(x = expression("Effect size"), y = expression("Statistical power (1-" * beta * ")")) +
  theme_minimal() +
  theme(legend.position = "right") +
  guides(fill = guide_legend(title = "Alpha"), color = guide_legend(title = "Alpha"))

# Print the plot
print(p)

# Save the plot
ggsave("power_ex3.png", p, width = 9, height = 3)































#-------- Exercise 4 ---------#
#-----------------------------#

# R does not have a power-plotting utility comparable to smp.TTestPower().plot_power in Python.
# The goal of this exercise is simply to introduce learners to that particular Python tool, so
# you can skip this one, or use the power calculating/plotting code above to recreate the same
# visualization as what I show in the book.


























#-------- Exercise 5 ---------#
#-----------------------------#
# Simulation parameters
effectSize <- 0.6  # group mean differences, divided by standard deviation.
n1 <- 50           # sample size of group 1.
ssRatio <- 2       # Ratio of sample size of group 2 to group 1. 1 means equal sample sizes.

# Compute power
power <- pwr.t2n.test(n1=n1, n2=n1*ssRatio, d=effectSize, sig.level=.05)$power

# Print result
total_sample_size <- n1 + n1 * ssRatio
cat(sprintf("Total sample size is %d+%d=%d, power is %.2f", n1, n1 * ssRatio, total_sample_size, power))


#### now for the simulation
# Total sample size
totalN <- 100

# Sample sizes in group 1
n1sampleSizes <- seq(10, 90, by = 5)

# Initialize results vector
powers <- numeric(length(n1sampleSizes))

# Run the simulation
for (i in seq_along(n1sampleSizes)) {
  n1 <- n1sampleSizes[i]
  n2 <- totalN - n1  # Calculate the n2 sample size
  sr <- n2 / n1      # The ratio
  
  # Compute and store power
  powers[i] <- pwr.t2n.test(n1 = n1, n2 = n2, d = 0.6, sig.level = 0.05)$power
}


### Plotting
# Create a data frame for plotting
power_data <- data.frame(Group1Size = n1sampleSizes, Power = powers)

# Generate the plot
p <- ggplot(power_data, aes(x = Group1Size, y = Power)) +
  geom_line() +
  geom_point(color = "black", fill = "grey80", shape = 22, size = 6) +
  labs(x = "Observations in group 1", y = "Statistical power") +
  theme_minimal()

# Print the plot
print(p)

# Save the plot
ggsave("power_ex5.png", p, width = 8, height = 3)














#-------- Exercise 6 ---------#
#-----------------------------#
# Population and sample parameters
mu <- 0.8
sigma <- 1.8
n <- 38

# Critical t-values (2-tailed)
t_critL <- qt(0.05/2, n-1, lower.tail = TRUE)
t_critR <- qt(1-0.05/2, n-1, lower.tail = TRUE)

# Simulation parameters
num_simulations <- 10000
rejectH0 <- 0 # Initialize a counter for when H0 was rejected

# Run the experiment
for (i in 1:num_simulations) {
  # Draw a sample
  sample <- rnorm(n, mu, sigma)
  sample_mean <- mean(sample)
  sample_se <- sd(sample) / sqrt(n)
  
  # Calculate the t-statistic
  tVal <- sample_mean / sample_se
  
  # Check if tVal falls into the rejection region
  if (tVal < t_critL || tVal > t_critR) {
    rejectH0 <- rejectH0 + 1
  }
}

# Estimate empirical power
powerEm <- 100 * rejectH0 / num_simulations

# Compute analytic power from formula
effectSize <- mu / sigma # Using population parameters
powerAn <- 100 * pwr.t.test(d = effectSize, n = n, sig.level = 0.05, type = "one.sample", alternative = "two.sided")$power

# Print the results
cat(sprintf("Theoretical power from formula:   %.3f%%\n", powerAn))
cat(sprintf("Empirical power from simulations: %.3f%%", powerEm))




### Note about the code in the previous cell: I wrote out the mechanics of the t-test so you
#   could see the link to the formula for computing statistical power. In practice, it's simpler
#   to use the t.test function The code below produces the same result using less code.

# Re-initialize the counter
rejectH0 <- 0

# Run the experiment
for (i in 1:num_simulations) {
  # Draw a sample
  sample <- rnorm(n, mu, sigma)
  
  # Increment the counter if the t-value is significant
  if (t.test(sample, mu = 0)$p.value < 0.05) {
    rejectH0 <- rejectH0 + 1
  }
}

# Estimate empirical power (percent of simulations where H0 was rejected)
powerEm <- 100 * rejectH0 / num_simulations

# Compute analytic power from formula
effectSize <- mu / sigma # Using population parameters
powerAn <- 100 * pwr.t.test(d = effectSize, n = n, sig.level = 0.05, type = "one.sample", alternative = "two.sided")$power

# Print the results
cat(sprintf("Analytical power from formula:    %.3f%%\n", powerAn))
cat(sprintf("Empirical power from simulations: %.3f%%", powerEm))















#-------- Exercise 7 ---------#
#-----------------------------#
# Population and sample parameters
mu <- 0.8
sigma <- 1.8
n <- 38

# Simulation parameters
num_simulations <- 10000
rejectH0 <- 0 # Initialize a counter for when H0 was rejected

# Run the experiment
for (i in 1:num_simulations) {
  # Draw a sample from a log-normal distribution
  sample <- exp(rnorm(n, mu, sigma))
  
  # Increment the counter if the t-value is significant
  if (t.test(sample, mu = 0)$p.value < 0.05) {
    rejectH0 <- rejectH0 + 1
  }
}

# Estimate empirical power (percent of simulations where H0 was rejected)
powerEm <- 100 * rejectH0 / num_simulations

# Compute analytic power from formula
popMean <- exp(mu + sigma^2 / 2)
popStd <- exp(mu + sigma^2 / 2) * sqrt(exp(sigma^2) - 1)
effectSize <- popMean / popStd # Using population parameters
powerAn <- 100 * pwr.t.test(d = effectSize, n = n, sig.level = 0.05, type = "one.sample", alternative = "two.sided")$power

# Print the results
cat(sprintf("Analytical power from formula:    %.3f%%\n", powerAn))
cat(sprintf("Empirical power from simulations: %.3f%%", powerEm))

























#-------- Exercise 8 ---------#
#-----------------------------#
# Initialize a counter for when H0 was rejected
rejectH0 <- 0

# Run the experiment
for (i in 1:num_simulations) {
  # Draw a sample from a log-normal distribution
  sample <- exp(rnorm(n, mu, sigma))
  
  # Perform Wilcoxon test and increment the counter if significant
  W <- wilcox.test(sample, mu = 0, conf.int = FALSE)
  if (W$p.value < 0.05) {
    rejectH0 <- rejectH0 + 1
  }
}

# Estimate empirical power (percent of simulations where H0 was rejected)
powerEm <- 100 * rejectH0 / num_simulations

# Print the results
cat(sprintf("Empirical power from simulations: %.3f%%", powerEm))



### now using a more realistic value
# New null hypothesis value
h0 <- 10

# Re-initialize the counter for when H0 was rejected
rejectH0 <- 0

# Run the experiment
for (i in 1:num_simulations) {
  # Draw a sample from a log-normal distribution
  sample <- exp(rnorm(n, mu, sigma))
  
  # Perform Wilcoxon test against the new H0 value and increment the counter if significant
  W <- wilcox.test(sample - h0, conf.int = FALSE)
  if (W$p.value < 0.05) {
    rejectH0 <- rejectH0 + 1
  }
}

# Estimate empirical power (percent of simulations where H0 was rejected)
powerEm <- 100 * rejectH0 / num_simulations

# Print the results
cat(sprintf("Empirical power from simulations: %.3f%%", powerEm))






















#-------- Exercise 9 ---------#
#-----------------------------#
# Import data
url <- "https://archive.ics.uci.edu/ml/machine-learning-databases/wine-quality/winequality-red.csv"
data <- read.csv(url, header = TRUE, sep = ";")

# Columns to t-test
cols2test <- names(data)
cols2test <- cols2test[cols2test != "quality"]

# Create a new column for binarized (boolean) quality
data$boolQuality <- data$quality > 5



### now for the analysis
# Statistical threshold (Bonferroni-corrected)
bonP <- .05 / length(cols2test)

# Loop over columns
results <- data.frame()
for (col in cols2test) {
  # Extract the numerical variables
  Xh <- data[data$boolQuality == TRUE, col]   # High rating
  Xl <- data[data$boolQuality == FALSE, col]  # Low rating
  
  # Sample sizes
  nh <- length(Xh)
  nl <- length(Xl)
  
  # Effect size (Cohen's d)
  es_num <- mean(Xh) - mean(Xl)
  es_den <- sqrt(((nh - 1) * var(Xh) + (nl - 1) * var(Xl)) / (nh + nl - 2))
  effect_size <- es_num / es_den
  
  # Compute power
  power <- pwr.t2n.test(n1 = nh, n2 = nl, d = effect_size, sig.level = bonP, power = NULL)$power
  
  # Run the t-test
  tres <- t.test(Xh, Xl, var.equal = FALSE)
  
  # Collect results
  tres_summary <- tidy(tres)
  tres_summary$Column <- col
  tres_summary$Power <- power
  results <- rbind(results, tres_summary)
}

# Print the results
print(results[, c("Column", "parameter", "statistic", "p.value", "Power")])
# Note: There are small differences between R's and Python's power results. These
# can be due to minor differences in algorithm implementation and precision rounding.


















#-------- Exercise 10 ---------#
#------------------------------#

# the examples I showed in the book
ans <- pwr.t2n.test(d=1, power=.8, sig.level=.05, n1=50, n2=NULL)
ans

# Statistical threshold (Bonferroni-corrected)
bonP <- .05 / length(cols2test)

# Initialize results dataframe
results <- data.frame(Column = character(), N_high = integer(), N_low = character())

# Loop over columns
for (col in cols2test) {
  # Extract the numerical variables
  Xh <- data[data$boolQuality == TRUE, col]   # High rating
  Xl <- data[data$boolQuality == FALSE, col]  # Low rating
  
  # Sample size and ratio
  nh <- length(Xh)
  nl <- length(Xl)
  
  # Effect size (Cohen's d)
  es_num <- mean(Xh) - mean(Xl)
  es_den <- sqrt(((nh - 1) * var(Xh) + (nl - 1) * var(Xl)) / (nh + nl - 2))
  effect_size <- es_num / es_den
  
  # Attempt power calculation
  calculated_sample_size <- tryCatch({
    # Compute required sample size
    pwr_result <- pwr.t2n.test(d = effect_size, power = 0.8, sig.level = bonP, n1 = nh, n2 = NULL)
    as.integer(pwr_result$n2)
  }, error = function(e) {
    "*Nope!*"
  })
  
  # Append results
  results <- rbind(results, data.frame(Column = col, N_high = nh, N_low = calculated_sample_size))
}

# Print the results
print(results)



















#-------- Exercise 11 ---------#
#------------------------------#
# Statistical threshold (Bonferroni-corrected)
bonP <- .05 / length(cols2test)

# Initialize results dataframe
results <- data.frame(Column = character(), N_high = integer(), N_low = character())

# Loop over columns
for (col in cols2test) {
  # Extract the numerical variables
  Xh <- data[data$boolQuality == TRUE, col]   # High rating
  Xl <- data[data$boolQuality == FALSE, col]  # Low rating
  
  # Sample size and ratio
  nh <- length(Xh)
  nl <- length(Xl)
  
  # Effect size (Cohen's d)
  es_num <- mean(Xh) - mean(Xl)
  es_den <- sqrt(((nh - 1) * var(Xh) + (nl - 1) * var(Xl)) / (nh + nl - 2))
  effect_size <- es_num / es_den
  
  # Attempt power calculation
  calculated_sample_size <- tryCatch({
    # Compute required sample size for high-quality group
    pwr_result <- pwr.t2n.test(d = effect_size, power = 0.8, sig.level = bonP, n1 = NULL, n2 = nl)
    as.integer(pwr_result$n1)
  }, error = function(e) {
    "*Nope!*"
  })
  
  # Append results
  results <- rbind(results, data.frame(Column = col, N_high = calculated_sample_size, N_low = nl))
}

# Print the results
print(results)











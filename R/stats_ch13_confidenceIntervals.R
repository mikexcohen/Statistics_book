#----
# Modern statistics: Intuition, Math, Python, R
## Mike X Cohen (sincxpress.com)
#### https://www.amazon.com/dp/B0CQRGWGLY
#### Code for chapter 13 (confidence intervals)

# About this code file:
### This code file will reproduce most of the figures in this chapter 
### (some figures were made in Inkscape), and illustrate the statistical 
### concepts explained in the text. The point of providing the code is not 
### just for you to recreate the figures, but for you to modify, adapt, 
### explore, and experiment with the code.
###
### Solutions to all exercises are at the bottom of the file.





# loading libraries
library(ggplot2)
library(gridExtra)
library(dplyr)
library(readr)


custom_theme <- theme_classic() + 
  theme(text = element_text(size = 20),        # font size 
        plot.title = element_text(hjust = 0))  # title location
theme_set(custom_theme)
savefig.dpi <- 300                             # output resolution






#-------- Figure 13.1: Visualization of confidence intervals ---------#
#---------------------------------------------------------------------#
# a population and its mean
popdata <- rnorm(100000, mean = 2)
popmean <- mean(popdata)

# a bunch of samples and their confidence intervals
nSamples <- 20
sampleSize <- 50

# Function to calculate confidence interval
conf_interval <- function(sample) {
  mean <- mean(sample)
  sem <- sd(sample) / sqrt(length(sample))
  error <- qt(0.975, df=length(sample)-1) * sem
  return(c(mean - error, mean + error))
}

# Collecting samples and their confidence intervals
samples <- lapply(1:nSamples, function(i) {
  sample <- sample(popdata, sampleSize)
  ci <- conf_interval(sample)
  data.frame(mean = mean(sample), CI_lower = ci[1], CI_upper = ci[2])
})
samples_df <- do.call(rbind, samples)
samples_df$Sample <- 1:nSamples

# Plotting
g <- ggplot() + 
  geom_histogram(data = data.frame(popdata), aes(x = popdata), bins = 41, fill = "gray") + 
  geom_vline(xintercept = popmean, linetype = "dashed", color = "grey70") +
  geom_errorbar(data = samples_df, aes(y = Sample, x = mean, xmin = CI_lower, xmax = CI_upper), color = "black") +
  geom_point(data = samples_df, aes(x = mean, y = Sample, color = ifelse(CI_lower < popmean & CI_upper > popmean, "black", "white")), size = 3) +
  scale_color_identity() +
  theme_minimal() +
  theme(axis.title.y = element_blank(), axis.text.y = element_blank(), axis.ticks.y = element_blank()) +
  labs(x = "Data value") 

print(g)








#-------- Figure 13.2: CI vs std ---------#
#-----------------------------------------#
# Define sample sizes and confidence level
sampleSizes <- c(100, 1000)
confLevel <- 0.95

# Function to calculate the confidence interval
conf_interval <- function(data, confLevel) {
  N <- length(data)
  mean <- mean(data)
  stdev <- sd(data)
  stderr <- stdev / sqrt(N)
  error <- qt(confLevel / 2 + 0.5, N - 1) * stderr
  return(c(mean - error, mean + error))
}

# Create list to hold plots
plot_list <- list()

# Loop through each sample size
for (i in 1:length(sampleSizes)) {
  N <- sampleSizes[i]
  
  # Generate a random sample of size N and force mean to be zero
  data <- rnorm(N, mean = 0, sd = 2)
  data <- data - mean(data)
  
  # Calculate mean, standard deviation, and confidence interval
  mean <- mean(data)
  stdev <- sd(data)
  CIs <- conf_interval(data, confLevel)
  
  # Create histogram and add lines for mean, standard deviation, and confidence interval
  p <- ggplot() +
    geom_histogram(aes(x = data), bins = 41, fill = "grey90") +
    geom_vline(xintercept = mean, color = "black", size = 1.5, linetype = "solid") +
    geom_vline(xintercept = mean - stdev, color = "grey60", size = 1, linetype = "dashed") +
    geom_vline(xintercept = mean + stdev, color = "grey60", size = 1, linetype = "dashed") +
    geom_vline(xintercept = CIs[1], color = "grey30", size = 1, linetype = "dotted") +
    geom_vline(xintercept = CIs[2], color = "grey30", size = 1, linetype = "dotted") +
    xlim(-6, 6) +
    labs(y = "Count", title = paste("Sample size =", N)) 
  
  # Add plot to the list
  plot_list[[i]] <- p
}

# Arrange the plots
grid.arrange(grobs = plot_list, ncol = 1)

# Save the plot
ggsave('confint_stdVsCI.png', width = 8, height = 5)









#-------- Analytic confidence interval ---------#
#-----------------------------------------------#
# Define the confidence level and sample size
confLevel <- 0.95
n <- 20

# Calculate the t-star value
tStar <- qt((1 - confLevel) / 2, df = n - 1, lower.tail = FALSE)
print(tStar)



# Simulation parameters
mean <- 2.3
stdev <- 3.2
N <- 48
confLevel <- 0.95

# Calculate t-star value
tStar <- qt((1-confLevel)/2, df=N-1, lower.tail = FALSE)

# Confidence interval from formula
conf_int_me <- c(mean - tStar * (stdev / sqrt(N)), 
                 mean + tStar * (stdev / sqrt(N)))

# Print the results
print(conf_int_me)









#-------- Bootstrapping ---------#
#--------------------------------#
# Define the sample (use one)
S <- c(1, 2, 3, 4)
S <- c(2, 2, 3, 3)

# Print header
cat("    Sample    |  Mean\n")
cat("----------------------\n")
cat(sprintf("%s  |  %.2f\n", toString(S), mean(S)))

# Perform bootstrap sampling
for (i in 1:5) {
  # Bootstrap a random sample
  b <- sample(S, length(S), replace = TRUE)
  
  # Print the sample and its mean
  cat(sprintf("%s  |  %.2f\n", toString(sort(b)), mean(b)))
}















#-------- [Don't peak!] How many boots? ---------#
#------------------------------------------------#
# Note about the code below: This code includes the solution to Exercise 6 (empirical confidence intervals),
# which produces Figure 13.3.
# If you want to challenge yourself on Exercise 6, don't look at the code here :P

# Parameters
samplesize <- 50

# Draw a random sample from the population
dataSample <- rnorm(samplesize)^2
dataSample <- dataSample - mean(dataSample)

# Number of bootstraps
numboots <- seq(50, 5050, by = 200)
CIs <- matrix(0, nrow = length(numboots), ncol = 2)
bmm <- numeric(length(numboots))

# Bootstrap analysis
for (i in 1:length(numboots)) {
  nb <- numboots[i]
  bootmeans <- replicate(nb, mean(sample(dataSample, samplesize, replace = TRUE)))
  CIs[i, ] <- quantile(bootmeans, c(0.025, 0.975))
  bmm[i] <- mean(bootmeans)
}

# Data frame for plotting
plot_data <- data.frame(NumBoots = numboots, Mean = bmm, 
                        Lower = bmm - CIs[, 1], Upper = CIs[, 2] - bmm)

# Plot
p <- ggplot(plot_data, aes(x = Mean, y = NumBoots)) +
  geom_errorbar(aes(xmin = Mean - Lower, xmax = Mean + Upper), width = 0.1) +
  geom_point(shape = 21, color = "black", fill = "white", size = 3) +
  xlim(-0.5, 0.5) +
  labs(y = "Number of bootstrap samples", x = "Data value") +
  theme_minimal()

print(p)

# Save the plot
ggsave("confint_nBoots.png", p, width = 4, height = 5)







#-------- CI for hypothesis testing ---------#
#--------------------------------------------#
# Simulation parameters
mean <- 1.3
stdev <- 5.2
N <- 48
confLevel <- 0.95

# Calculate confidence interval
stderr <- stdev / sqrt(N)
confint <- qt(c((1 - confLevel) / 2, 1 - (1 - confLevel) / 2), df = N - 1) * stderr
confint <- mean + confint

# Print the confidence interval
print(confint)







#-------- Figure 13.4: Qualitative interpretation of confidence intervals ---------#
#----------------------------------------------------------------------------------#
# Data
eSizes <- c(6, 1, 6, 1)
means <- c(6.6, 7, 5, 0)
labels <- c('A', 'B', 'C', 'D')

# Create a data frame for plotting
data <- data.frame(Group = labels, Mean = means, Error = eSizes)

# Plot
p <- ggplot(data, aes(x = Group, y = Mean)) +
  geom_errorbar(aes(ymin = Mean - Error, ymax = Mean + Error), 
                width = 0.1, color = "black") +
  geom_point(shape = 21, color = "black", fill = "grey80", size = 4) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey70") +
  xlim(-0.5, 3.5) +
  scale_x_discrete(limits = labels) +
  theme_minimal()

# Print the plot
print(p)

# Save the plot
ggsave("confint_qualitative.png", p, width = 3, height = 3)















#-------- Exercise 1 ---------#
#-----------------------------#

# Parameters
samplesizes <- seq(50, 1000, by = 50)
stdevs <- seq(0.1, 7, length.out = 41)

# Initialization
CIs <- matrix(0, nrow = length(samplesizes), ncol = length(stdevs))

# Experiment
for (ni in 1:length(samplesizes)) {
  for (si in 1:length(stdevs)) {
    N <- samplesizes[ni]
    s <- stdevs[si]
    
    # SEM
    sem <- s / sqrt(N)
    
    # CI width calculation
    CIs[ni, si] <- 2 * qt(0.975, df = N - 1) * sem
  }
}

# Creating a data frame for ggplot
CI_data <- expand.grid(SampleSize = samplesizes, Stdev = stdevs)
CI_data$CI_Width <- as.vector(CIs)

# Plotting
p <- ggplot(CI_data, aes(x = Stdev, y = SampleSize, fill = CI_Width)) +
  geom_tile() +
  scale_fill_gradient2(low = "white", high = "black", mid = "gray", midpoint = 0.5, limit = c(0, 1), name="C.I. width") +
  labs(x = "Standard deviation", y = "Sample size") +
  theme_minimal() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

# Print the plot
print(p)

# Save the plot
ggsave("confint_ex1.png", p, width = 8, height = 5)












#-------- Exercise 2 ---------#
#-----------------------------#
# Simulate data
popN <- 1e7  # lots and LOTS of data!!

# The data
population <- (4 * rnorm(popN))^1  # the "1" here can be changed to "2" for a different exercise

# Calculate the exact population mean
popMean <- mean(population)

# Create plots
p1 <- ggplot(data.frame(Index = seq(1, popN, by = 1000), Value = population[seq(1, popN, by = 1000)]), 
             aes(x = Index, y = Value)) +
  geom_point(color = "black", size = 0.5) +
  labs(x = "Data index", y = "Data value") +
  theme_minimal()

p2 <- ggplot(data.frame(Value = population), aes(x = Value)) +
  geom_histogram(bins = nclass.FD(population)) +
  labs(x = "Data value", y = "Count") +
  theme_minimal()

# Plot using gridExtra to arrange the plots
gridExtra::grid.arrange(p1, p2, ncol = 1)

# The population mean can be printed or used as needed
print(popMean)






# Parameters
samplesize <- 500
confidence <- 95  # in percent

# Compute sample mean and standard deviation
dataSample <- sample(population, samplesize)
samplemean <- mean(dataSample)
samplestd <- sd(dataSample)

# Compute confidence intervals
error <- qt(confidence/100 + (1 - confidence/100)/2, df = samplesize-1) * (samplestd / sqrt(samplesize))
confint <- c(samplemean - error, samplemean + error)

# Graph everything
p <- ggplot() + 
  geom_histogram(aes(x = dataSample, y = ..density..), bins = nclass.FD(dataSample), fill = "black", alpha = 0.1) +
  geom_vline(xintercept = popMean, color = "black", linetype = "dotted", size = 1.5) +
  geom_vline(xintercept = samplemean, color = "black", linetype = "dashed", size = 1.5) +
  annotate("rect", xmin = confint[1], xmax = confint[2], ymin = 0, ymax = Inf, alpha = 0.4, fill = "black") +
  theme_minimal() +
  labs(x = "Data values", y = "Count (a.u.)") +
  theme()

# Print the plot
print(p)

# Save the plot
ggsave("confint_ex2.png", p, width = 8, height = 4)









#-------- Exercise 3 ---------#
#-----------------------------#
# Set parameters
numExperiments <- 5000
samplesize <- 500
confidence <- 95  # in percent

# Initialize the matrix to record whether the population mean is inside the CI
withinCI <- numeric(numExperiments)

# Run the experiment
for (expi in 1:numExperiments) {
  # Compute sample mean and CI
  dataSample <- sample(population, samplesize)
  samplemean <- mean(dataSample)
  samplestd  <- sd(dataSample)
  error <- qt(confidence/100 + (1 - confidence/100)/2, df = samplesize-1) * (samplestd / sqrt(samplesize))
  confint <- c(samplemean - error, samplemean + error)
  
  # Determine whether the true mean is inside this CI
  withinCI[expi] <- popMean > confint[1] && popMean < confint[2]
}

# Print the percentage of sample CIs containing the true population mean
cat(sprintf("%g%% of sample C.I.'s contained the true population mean.\n", 100*mean(withinCI)))











#-------- Exercise 4 ---------#
#-----------------------------#
# No new code here, just print out the actual confidence intervals for different sample sizes.













#-------- Exercise 5 ---------#
#-----------------------------#
# The discrepency here is due to the assumptions of the analytic formula for computing confidence intervals.
#
# In Exercises 2-4, the normality assumption was met. Even when the sample size was tiny, the purely random
# sampling in combination with the purely Gaussian distribution meant that the assumptions underlying the
# confidence intervals were still met (although with a small sample size, the confidence intervals were so huge
# as to be completely useless from a practical perspective, but still valid mathematically).
#
# In contrast, Exerise 5 violated the normality assumption. Now, with large samples, the CLT kicked in and still
# gave us a good result. But the small sample sizes stretched the CLT to its limits, meaning it was no longer applicable.
# And that in turn meant that the confidence intervals were junk and not reliable.
#











#-------- Exercise 6 ---------#
#-----------------------------#
# Set parameters
samplesize <- 500
numBoots   <- 1000

# Draw a random sample from the population
dataSample <- sample(population, samplesize)

# Statistics for the sample
samplemean <- mean(dataSample)
samplestd  <- sd(dataSample)

# Initialize a vector to store the bootstrapped means
bootmeans <- numeric(numBoots)

# Bootstrapping
for (booti in 1:numBoots) {
  # Create a bootstrap sample
  bootsample <- sample(dataSample, samplesize, replace = TRUE)
  
  # Compute its mean
  bootmeans[booti] <- mean(bootsample)
}

# Find 95% confidence intervals from bootstrapped means
confintB <- quantile(bootmeans, c(0.025, 0.975))  # B for bootstrap

# Print the confidence interval
print(confintB)






# Graph everything
p <- ggplot() +
  geom_histogram(aes(x = dataSample, y = ..density..), bins = nclass.FD(dataSample), 
                 fill = "black", alpha = 0.1, label = 'Data histogram') +
  geom_histogram(aes(x = bootmeans, y = ..density..), bins = nclass.FD(bootmeans), 
                 fill = "black", alpha = 0.5, label = 'Bootstrap means') +
  geom_vline(xintercept = popMean, linetype = "dotted", color = "black", size = 1.5, label = "Pop. mean") +
  geom_vline(xintercept = samplemean, linetype = "dashed", color = "black", size = 1.5, label = "Sample mean") +
  annotate("rect", xmin = confintB[1], xmax = confintB[2], ymin = 0, ymax = Inf, alpha = 0.4, fill = "black") +
  scale_x_continuous(limits = c(confintB[1] - diff(confintB), confintB[2] + diff(confintB))) +
  labs(x = "Data values", y = "Count (a.u.)") +
  theme_minimal() +
  theme(legend.position = "bottom")

# Add a custom legend
p <- p + guides(color = guide_legend(title = ""), fill = guide_legend(title = ""))

# Print the plot
print(p)

# Save the plot
ggsave("confint_ex6.png", p, width = 8, height = 4)


#### Finally, compare the analytical and empirical confidence intervals
# Compute analytic confidence intervals (95%)
error <- qt(0.975, df = samplesize - 1) * (samplestd / sqrt(samplesize))
confintA <- c(samplemean - error, samplemean + error)

# Print both empirical (bootstrap) and analytic confidence intervals
cat(sprintf("Empirical CI(95%%) = (%.3f, %.3f)\n", confintB[1], confintB[2]))
cat(sprintf("Analytic  CI(95%%) = (%.3f, %.3f)\n", confintA[1], confintA[2]))







#-------- Exercise 7 ---------#
#-----------------------------#
# Set simulation parameters
samplesize <- 100
trueR <- 0.3  # True population correlation

# Generate the data
X <- matrix(rnorm(samplesize * 2), ncol = 2)
X[, 2] <- X[, 1] * trueR + X[, 2] * sqrt(1 - trueR^2)

# Confirmation of the correlation
cor(X)




#### Define the function for calculating confidence interval of correlation coefficient
corr_CI <- function(X, nBoots = 1000) {
  bootstrap_r <- numeric(nBoots)
  samplesize <- nrow(X)
  
  for (i in 1:nBoots) {
    boot_idx <- sample(1:samplesize, size = samplesize, replace = TRUE)
    bootstrap_r[i] <- cor(X[boot_idx, 1], X[boot_idx, 2])
  }
  
  CI <- quantile(bootstrap_r, c(0.025, 0.975))
  list(CI = CI, bootstrap_r = bootstrap_r)
}




### apply the function
# Observed correlation coefficient
obs <- cor.test(X[, 1], X[, 2])
obs_r <- obs$estimate
obs_p <- obs$p.value

# Get empirical confidence intervals
results <- corr_CI(as.data.frame(X))

# Choose a color for the CI area based on significance
areacolor <- ifelse(sign(results$CI[1]) == sign(results$CI[2]), 'gray', 'red')

# Plotting
p <- ggplot(data.frame(Corr = results$bootstrap_r), aes(x = Corr)) +
  geom_histogram(bins = 30, fill = 'gray', color = 'black', alpha = 0.6) +
  geom_vline(xintercept = obs_r, linetype = "dashed", size = 1.5, color = "black") +
  geom_vline(xintercept = trueR, linetype = "dotted", size = 1.5, color = "black") +
  annotate("rect", xmin = results$CI[1], xmax = results$CI[2], ymin = 0, ymax = Inf, fill = areacolor, alpha = 0.5) +
  xlim(-1, 1) +
  labs(title = paste("Bootstrap distribution of correlation coefficients\nPearson r =", format(obs_r, digits = 2), 
                     ", p =", format(obs_p, digits = 3)),
       x = "Correlation coefficient", y = "Count") +
  theme_minimal()

# Print the plot
print(p)

# Save the plot
ggsave("confint_ex7.png", p, width = 8, height = 5)











#-------- Exercise 8 ---------#
#-----------------------------#
#### perform the experiment
# Simulation parameters
samplesizes <- seq(10, 3010, by = 100)
trueR <- 0.3  # True population correlation

# Initialize matrices
bootCI <- matrix(0, nrow = length(samplesizes), ncol = 2)
obs_r <- numeric(length(samplesizes))

# Reduce the number of bootstraps
nBoots <- 500

# Run the experiment
for (idx in 1:length(samplesizes)) {
  N <- samplesizes[idx]
  
  # Generate the data
  X <- matrix(rnorm(N * 2), ncol = 2)
  X[, 2] <- X[, 1] * trueR + X[, 2] * sqrt(1 - trueR^2)
  
  # Observed correlation coefficient
  obs_r[idx] <- cor(X[, 1], X[, 2])
  
  # Get confidence intervals
  bootCI[idx, ] <- corr_CI(as.data.frame(X), nBoots)$CI
}



### Create the plot
# Panel A: Correlations with 95% C.I.
p1 <- ggplot(data.frame(SampleSize = samplesizes, Correlation = obs_r, 
                        Lower = bootCI[, 1], Upper = bootCI[, 2]), aes(x = SampleSize, y = Correlation)) +
  geom_errorbar(aes(ymin = Lower, ymax = Upper), width = 0.1) +
  geom_point(shape = 1) +
  geom_hline(yintercept = trueR, linetype = "dashed", color = "grey80") +
  labs(title = "A) Correlations with 95% C.I.", x = "Sample size", y = "Correlation coef.") +
  theme_minimal()

# Panel B: C.I. Ranges
# Compute CI ranges
CIRanges <- bootCI[, 2] - bootCI[, 1]
p2 <- ggplot(data.frame(SampleSize = samplesizes, CIRange = CIRanges), aes(x = SampleSize, y = CIRange)) +
  geom_point(shape = 1, color = "black", fill = "grey70") +
  labs(title = "B) C.I. ranges", x = "Sample size", y = "C.I. range") +
  theme_minimal()

# Arrange the plots side by side
grid.arrange(p1, p2, ncol = 2)

# Save the plot
ggsave("confint_ex8.png", width = 10, height = 3)











#-------- Exercise 9 ---------#
#-----------------------------#
# using the corr_CI() function defined in exercise 7

### Run the experiment!
# Simulation parameters
samplesize <- 50
coefs <- seq(0, 0.99, length.out = 42)

# Initialize matrices
bootCI <- matrix(0, nrow = length(coefs), ncol = 2)
obs_r <- numeric(length(coefs))

# Number of bootstraps
nBoots <- 500

# Run the experiment
for (idx in 1:length(coefs)) {
  r <- coefs[idx]
  
  # Generate the data
  X <- matrix(rnorm(samplesize * 2), ncol = 2)
  X[, 2] <- X[, 1] * r + X[, 2] * sqrt(1 - r^2)
  
  # Observed correlation coefficient
  obs_r[idx] <- cor(X[, 1], X[, 2])
  
  # Confidence intervals
  bootCI[idx, ] <- corr_CI(as.data.frame(X), nBoots)$CI
}



### Visualize the results!
# Panel A: Correlations with 95% C.I.
p1 <- ggplot(data.frame(PopCorrelation = coefs, ObsCorrelation = obs_r, 
                        Lower = bootCI[, 1], Upper = bootCI[, 2]), aes(x = PopCorrelation, y = ObsCorrelation)) +
  geom_errorbar(aes(ymin = Lower, ymax = Upper), width = 0.01) +
  geom_point(shape = 1) +
  geom_line(aes(x = PopCorrelation, y = PopCorrelation), linetype = "dashed", color = "grey80") +
  labs(title = "A) Correlations with 95% C.I.", x = "Population correlation", y = "Correlation coef.") +
  theme_minimal()

# Panel B: C.I. Ranges
CIRanges <- bootCI[, 2] - bootCI[, 1]
p2 <- ggplot(data.frame(PopCorrelation = coefs, CIRange = CIRanges), aes(x = PopCorrelation, y = CIRange)) +
  geom_point(shape = 1, color = "black", fill = "grey70") +
  labs(title = "B) C.I. ranges", x = "Population correlation", y = "C.I. range") +
  theme_minimal()

# Arrange the plots side by side
grid.arrange(p1, p2, ncol = 2)

# Save the plot
ggsave("confint_ex9.png", width = 10, height = 3)












#-------- Exercise 10 ---------#
#------------------------------#
### run the simulation
# Simulation parameters
means <- seq(0, 2.5, length.out = 41)
stds <- seq(0.5, 5, length.out = 51)
sampsize <- 30

# Initialize output matrix
statsmatrix <- matrix(0, nrow = length(means), ncol = length(stds))

# Run the experiment
for (mi in 1:length(means)) {
  for (si in 1:length(stds)) {
    # SEM
    sem <- stds[si] / sqrt(sampsize)
    
    # Confidence interval
    CI <- qt(c(0.025, 0.975), df = sampsize - 1) * sem + means[mi]
    
    # t/p values
    tval <- means[mi] / sem
    pval <- 2 * pt(tval, df = sampsize - 1, lower.tail = FALSE)
    
    # Build up the stats matrix according to significances
    statsmatrix[mi, si] <- as.integer(CI[1] > 0) + as.integer(pval < 0.05)
  }
}



### visualize the results
# Create data frame for plotting
plot_data <- expand.grid(Mean = means, Std = stds)
plot_data$Value <- as.vector(statsmatrix)

# Plot
p <- ggplot(plot_data, aes(x = Std, y = Mean, fill = factor(Value))) +
  geom_tile() +
  scale_fill_manual(values = c("white", "grey60", "black"), 
                    labels = c("Neither", "CI > 0", "CI > 0 & p < 0.05"),
                    name = "") +
  labs(x = "Standard deviations", y = "Means") +
  theme_minimal() +
  theme(legend.position = "bottom")

# Print the plot
print(p)

# confirm that no pixels have a value of 1
unique(plot_data$Value)

# Save the plot
ggsave("confint_ex10.png", p, width = 8, height = 6)










#-------- Exercise 11 ---------#
#------------------------------#
### generate data and perform t-test
# Set parameters
sampsize <- 30

# Generate the data
sample1 <- rnorm(sampsize)
sample2 <- rnorm(sampsize)^2
sample2 <- scale(sample2, scale = FALSE) + 0.5  # Mean-center then mean-shift

# Compute the t-test
tres <- t.test(sample1, sample2)


### Bootstrapping for confidence interval
# CI parameters
nBoots <- 1000  # Number of bootstrap samples

# Observed difference in means
obs_diff <- mean(sample1) - mean(sample2)

# Initialize bootstrap sample-mean differences
bootstrap_diffs <- numeric(nBoots)

# Generate bootstrap samples
for (i in 1:nBoots) {
  boot_sample1 <- sample(sample1, size = sampsize, replace = TRUE)
  boot_sample2 <- sample(sample2, size = sampsize, replace = TRUE)
  bootstrap_diffs[i] <- mean(boot_sample1) - mean(boot_sample2)
}

# Empirical confidence intervals (95%)
CI_B <- quantile(bootstrap_diffs, c(0.025, 0.975))

# Choose a color for the CI area based on significance
areacolor <- ifelse(sign(CI_B[1]) == sign(CI_B[2]), 'gray', 'red')


### plot the results!
# Data distributions plot
p1 <- ggplot() +
  geom_histogram(aes(x = sample1), bins = nclass.FD(sample1), fill = "black", alpha = 0.8) +
  geom_histogram(aes(x = sample2), bins = nclass.FD(sample2), fill = "black", alpha = 0.3) +
  labs(x = "Data value", y = "Count", title = sprintf("Data histograms (t = %.02f, p = %.03f)", tres$statistic, tres$p.value)) +
  theme_minimal()

# Bootstrapping distribution plot
p2 <- ggplot(data.frame(Diff = bootstrap_diffs), aes(x = Diff)) +
  geom_histogram(bins = 30, fill = "grey", color = "black", alpha = 0.6) +
  geom_vline(xintercept = obs_diff, linetype = "dashed", size = 1.5, color = "black") +
  annotate("rect", xmin = CI_B[1], xmax = CI_B[2], ymin = 0, ymax = Inf, fill = areacolor, alpha = 0.3) +
  labs(x = "Difference in Means", y = "Count", title = "Bootstrap mean difference histogram") +
  theme_minimal() +
  xlim(-2, 1)

# Arrange the plots side by side
grid.arrange(p1, p2, ncol = 2)

# Save the plot
ggsave("confint_ex11.png", width = 10, height = 5)









#-------- Exercise 12 ---------#
#------------------------------#
# Set parameters
blue   <- 40
yellow <- 30
orange <- 20
totalMarbs <- blue + yellow + orange

# Create the jar of marbles
jar <- c(rep(1, blue), rep(2, yellow), rep(3, orange))

# Draw a sample of 500 marbles (with replacement)
numDraws <- 500
marbSample <- sample(jar, size = numDraws, replace = TRUE)

# Bootstrapping for empirical confidence intervals
nBoots <- 1000
bootProps <- matrix(0, nrow = 3, ncol = nBoots)

for (i in 1:nBoots) {
  # Bootstrap sample
  bootmarbs <- sample(marbSample, size = numDraws, replace = TRUE)
  
  # Empirical proportions of this sample
  for (j in 1:3) {
    bootProps[j, i] <- sum(bootmarbs == j) / numDraws
  }
}

# Confidence intervals
CI <- apply(bootProps, 1, function(x) quantile(x, c(0.025, 0.975)))

# Empirical proportions of colors drawn
props <- c(sum(marbSample == 1) / numDraws, 
           sum(marbSample == 2) / numDraws, 
           sum(marbSample == 3) / numDraws)



### plot the results
p <- ggplot() +
  geom_bar(aes(x = factor(c('Blue', 'Yellow', 'Orange'), levels = c('Blue', 'Yellow', 'Orange')), y = props), 
           stat = "identity", fill = "grey70") +
  geom_errorbar(aes(x = factor(c('Blue', 'Yellow', 'Orange'), levels = c('Blue', 'Yellow', 'Orange')), 
                    ymin = CI[1, ], ymax = CI[2, ]),
                width = 0.2, color = "black") +
  geom_segment(aes(x = 0.8, xend = 1.2, y = blue / totalMarbs, yend = blue / totalMarbs), color = "black", size = 1.5) +
  geom_segment(aes(x = 1.8, xend = 2.2, y = yellow / totalMarbs, yend = yellow / totalMarbs), color = "black", size = 1.5) +
  geom_segment(aes(x = 2.8, xend = 3.2, y = orange / totalMarbs, yend = orange / totalMarbs), color = "black", size = 1.5) +
  labs(x = "Marble color", y = "Proportion/probability", title = "Marble Color Proportions with Confidence Intervals") +
  theme_minimal() +
  scale_x_discrete(labels = c('Blue', 'Yellow', 'Orange'))


# Print the plot
print(p)

# Save the plot
ggsave("confint_ex12.png", p, width = 10, height = 5)











#-------- Exercise 13 ---------#
#------------------------------#
## import and process the data
url <- "https://archive.ics.uci.edu/ml/machine-learning-databases/arrhythmia/arrhythmia.data"
df <- read_csv(url, col_names = FALSE, col_types = cols_only(
  X1 = col_double(), X2 = col_double(), X3 = col_double(),
  X4 = col_double(), X5 = col_double(), X6 = col_double(),
  X7 = col_double(), X8 = col_double(), X9 = col_double()
))
colnames(df) <- c('age', 'sex', 'height', 'weight', 'qrs', 'p-r', 'q-t', 't', 'p')

# Z-score normalization (excluding 'sex' column)
df_z <- df
cols2zscore <- setdiff(names(df), 'sex')
df_z[cols2zscore] <- scale(df[cols2zscore], center = TRUE, scale = TRUE)

# Define z-score threshold
zThresh <- 3.29  # p < 0.001
df_clean <- df
df_clean[abs(df_z) > zThresh] <- NA



### compute the CIs and print the results
# Function to calculate confidence interval
conf_interval <- function(x) {
  n <- sum(!is.na(x))
  stderr <- sd(x, na.rm = TRUE) / sqrt(n)
  ci <- qt(0.975, df = n - 1) * stderr
  return(ci)
}

# Print statistics for original and cleaned data
for (col in names(df)) {
  # Original data
  mean <- mean(df[[col]], na.rm = TRUE)
  std <- sd(df[[col]], na.rm = TRUE)
  n <- sum(!is.na(df[[col]]))
  ci <- conf_interval(df[[col]])
  cat(sprintf("%s initial: %.2f +/- %.2f\n", col, mean, ci))
  
  # Cleaned data
  mean_clean <- mean(df_clean[[col]], na.rm = TRUE)
  std_clean <- sd(df_clean[[col]], na.rm = TRUE)
  ci_clean <- conf_interval(df_clean[[col]])
  cat(sprintf("%s cleaned: %.2f +/- %.2f\n\n", col, mean_clean, ci_clean))
}





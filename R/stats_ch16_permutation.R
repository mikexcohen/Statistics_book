#----
# Modern statistics: Intuition, Math, Python, R
## Mike X Cohen (sincxpress.com)
#### https://www.amazon.com/dp/B0CQRGWGLY
#### Code for chapter 16 (Permutation testing)

# About this code file:
### This code file will reproduce most of the figures in this chapter 
### (some figures were made in Inkscape), and illustrate the statistical 
### concepts explained in the text. The point of providing the code is not 
### just for you to recreate the figures, but for you to modify, adapt, 
### explore, and experiment with the code.
###
### Solutions to all exercises are at the bottom of the file.





# Load libraries
library(ggplot2)
library(gridExtra)



#-------- Figure 16.1: Analytic vs empirical H0 distribution ---------#
#---------------------------------------------------------------------#
# x-axis grid
x <- seq(-4, 4, length.out = 1001)

# Compute and normalize the analytic pdf
analytical <- dnorm(x)
analytical <- analytical / max(analytical)
analytical_data <- data.frame(x = x, analytical = analytical)

# Same for empirical
empirical <- rnorm(length(x))
yy <- hist(empirical, breaks = "FD", plot = FALSE)
yy$counts <- yy$counts / max(yy$counts)
xx <- (yy$breaks[-length(yy$breaks)] + yy$breaks[-1]) / 2
empirical_data <- data.frame(xx = xx, yy = yy$counts)

# Draw the figure
p <- ggplot() +
  geom_bar(data = empirical_data, aes(x = xx, y = yy), stat = "identity", fill = "grey70", 
           color = "grey20", width = 0.2) +
  geom_line(data = analytical_data, aes(x = x, y = analytical), color = "black", size = 1) +
  labs(x = "value", y = "Density (a.u.)") +
  theme_minimal() +
  scale_y_continuous(breaks = NULL) +
  theme(legend.position = "none")

# Print the plot
print(p)

# Save the plot
ggsave("permute_empVanalyH0.png", p, width = 6, height = 3)


















#-------- Figure 16.3/4: Example in comparing two sample means ---------#
#-----------------------------------------------------------------------#

### generate the data
# Set parameters
n1 <- 50
n2 <- 70

# Create data
data1 <- rnorm(n1, 0, 1)
data2 <- rnorm(n2, 0.3, 1)

# Pool the data into one variable
alldata <- c(data1, data2)

# Corresponding labels
truelabels <- c(rep(1, n1), rep(2, n2))

# Compute the observed condition difference
true_conddif <- mean(alldata[truelabels == 1]) - mean(alldata[truelabels == 2])


### Creating a Null-Hypothesis (H0) Distribution
# Number of iterations for permutation testing
nIterations <- 1000

# Initialize output variable
permvals <- numeric(nIterations)

# Run the experiment
for (permi in 1:nIterations) {
  # Random permutation to swap the labels
  shuflabels <- sample(truelabels)
  
  # Mean differences in the shuffled data
  permvals[permi] <- mean(alldata[shuflabels == 1]) - mean(alldata[shuflabels == 2])
}


### Visualizations
# Real data plot
p1 <- ggplot() +
  geom_point(aes(x = data1, y = rep(0, n1)), color = "black", fill = "grey40", shape = 21, alpha = 0.5, size = 3) +
  geom_point(aes(x = data2, y = rep(1, n2)), color = "black", fill = "grey80", shape = 21, alpha = 0.5, size = 3) +
  geom_segment(aes(x = mean(data1), xend = mean(data1), y = 0.7, yend = 1.3), linetype = "dashed", size = 1) +
  geom_segment(aes(x = mean(data2), xend = mean(data2), y = -0.3, yend = 0.3), linetype = "dashed", size = 1) +
  scale_y_continuous(breaks = c(0, 1), labels = c("0", "1")) +
  labs(title = "A) Real data", x = "Data value", y = "Data series") +
  theme_minimal()

# Example shuffled data plot
example_shuffled <- sample(alldata)
p2 <- ggplot() +
  geom_point(aes(x = example_shuffled[shuflabels == 1], y = rep(0, n1)), color = "black", fill = "grey40", shape = 21, alpha = 0.5, size = 3) +
  geom_point(aes(x = example_shuffled[shuflabels == 2], y = rep(1, n2)), color = "black", fill = "grey80", shape = 21, alpha = 0.5, size = 3) +
  geom_segment(aes(x = mean(example_shuffled[shuflabels == 1]), xend = mean(example_shuffled[shuflabels == 1]), y = 0.7, yend = 1.3), linetype = "dashed", size = 1) +
  geom_segment(aes(x = mean(example_shuffled[shuflabels == 2]), xend = mean(example_shuffled[shuflabels == 2]), y = -0.3, yend = 0.3), linetype = "dashed", size = 1) +
  scale_y_continuous(breaks = c(0, 1), labels = c("0", "1")) +
  labs(title = "B) Shuffled data", x = "Data value", y = "Data series") +
  theme_minimal()

# Shuffling distribution plot
p3 <- ggplot(data.frame(Value = permvals), aes(x = Value)) +
  geom_histogram(binwidth = 0.05, fill = "grey70") +
  geom_vline(aes(xintercept = true_conddif), linetype = "dashed", size = 1) +
  labs(title = "C) Shuffling distribution", x = "Mean value", y = "Count") +
  theme_minimal()

# Arrange and save the plots
g <- grid.arrange(p1, p2, p3, ncol = 3)

# Save the plot
ggsave("permute_ttestIllustrated.png", g, width = 12, height = 4)










#-------- Convert to p-value ---------#
#-------------------------------------#
# Based on normalized distance
zVal <- (true_conddif - mean(permvals)) / sd(permvals)
p_z <- (1 - pnorm(abs(zVal))) * 2  # Two-tailed

# Based on counts
p_c <- sum(abs(permvals) > abs(true_conddif)) / nIterations


# Print the results
cat(sprintf("Z = %.3f, p = %.3f", zVal, p_z))
cat(sprintf("p_c = %.3f", p_c))















#-------- Figure 16.5: Margin figure about p-values ---------#
#------------------------------------------------------------#

# p_z is appropriate for (roughly) Gaussian H0 distributions
H0 <- rnorm(1000, mean = 0, sd = 2)

# Create histogram data
h <- hist(H0, breaks = "FD", plot = FALSE)
h_data <- data.frame(mid = h$mids, count = h$counts)

# Create the plot
p <- ggplot(h_data, aes(x = mid, y = count)) +
  geom_bar(stat = "identity", fill = "grey90", color = "grey60") +
  annotate("text", x = 3, y = max(h$counts), label = "Observed\nstatistic", vjust = 1, hjust = 0.5, angle = 90, size = 5) +
  annotate("text", x = mean(H0), y = max(h$counts), label = expression(bar(H[0])), vjust = 1, hjust = 0.5, size = 6, fontface = "bold") +
  annotate("segment", x = mean(H0), xend = mean(H0), y = 0, yend = max(h$counts), linetype = "dashed", color = "black", arrow = arrow(length = unit(0.2, "inches"))) +
  annotate("text", x = -sd(H0, na.rm = TRUE) * 2, y = mean(h$counts), label = expression(s[H[0]]), vjust = 0.5, hjust = 0.5, size = 6, fontface = "bold") +
  annotate("segment", x = mean(H0), xend = -sd(H0, na.rm = TRUE) * 2, y = mean(h$counts), yend = mean(h$counts), linetype = "dashed", color = "black") +
  annotate("text", x = 3.5, y = max(h$counts) / 2, label = expression(z == frac("obs" - bar(H[0]), s[H[0]])), size = 7) +
  labs(x = "Parameter value", y = "Count") +
  theme_minimal()

# Print and save the plot
print(p)
ggsave("permute_pz.png", p, width = 4, height = 5)












#-------- Figure 16.6: Margin figure about p-values ---------#
#------------------------------------------------------------#
# p_c is appropriate for any shape H0 distributions
H0 <- rexp(1000, rate = 1/2)

# Create histogram data
h <- hist(H0, breaks = "FD", plot = FALSE)
h_data <- data.frame(mid = h$mids, count = h$counts, col = ifelse(h$mids > 5, "black", "grey90"))

# Create the plot
p <- ggplot(h_data, aes(x = mid, y = count, fill = col)) +
  geom_bar(stat = "identity", color = "grey60") +
  annotate("text", x = 5, y = max(h$counts), label = "Observed\nstatistic", vjust = 1, hjust = 0.5, angle = 90, size = 5) +
  annotate("text", x = 6, y = max(h$counts) / 2, label = expression(p == frac(sum(H[0] > "obs"), N[H[0]])), size = 7) +
  labs(x = "Parameter value", y = "Count") +
  theme_minimal() +
  scale_fill_identity()

# Print and save the plot
print(p)
ggsave("permute_pc.png", p, width = 4, height = 5)




















#-------- Figure 16.7: Permutation testing for the mean of one sample ---------#
#------------------------------------------------------------------------------#
# Create non-normal data
N <- 87
data <- rgamma(N, shape = 1.2)
h0val <- 1
sampleMean <- mean(data)


# Note: Re-run the permutation testing code below, without re-running the code above.
#       This allows you to run permutation testing repeatedly on the same data.


# Permutation testing
data4perm <- data - h0val
obsMean <- mean(data4perm)

nPerms <- 1000
permMeans <- numeric(nPerms)

for (permi in 1:nPerms) {
  # Create a vector of +/- 1's
  randSigns <- sign(rnorm(N))
  
  # Mean of shuffled data
  permMeans[permi] <- mean(randSigns * abs(data4perm))
}

# Compute p-value based on extreme count
pval <- sum(abs(permMeans) > abs(obsMean)) / nPerms


## Visualize the results
# Data distribution
p1 <- ggplot(data.frame(Value = data), aes(x = Value)) +
  geom_histogram(bins = 20, fill = "grey60", color = "black") +
  geom_vline(xintercept = h0val, linetype = "dashed", size = 1) +
  geom_vline(xintercept = sampleMean, linetype = "dotted", color = "grey30", size = 1) +
  labs(title = "A) Data distribution", x = "Data value", y = "Count") +
  theme_minimal()

# Histogram of permutations (adding back h0 value for visualization)
p2 <- ggplot(data.frame(Value = permMeans + h0val), aes(x = Value)) +
  geom_histogram(bins = 20, fill = "grey90", color = "black") +
  geom_vline(xintercept = sampleMean, linetype = "dotted", color = "grey30", size = 1) +
  labs(title = sprintf("B) H0 distribution (p=%.3f)", pval), x = "Value", y = "Count") +
  theme_minimal()

# Arrange and save the plots
g <- grid.arrange(p1, p2, ncol = 2)

# Save the plot
ggsave("permute_oneSample.png", g, width = 10, height = 4)




















#-------- Figure 16.9: Number of iterations ---------#
#----------------------------------------------------#
# Number of iterations
numberOfIterations <- seq(10, 10010, by = 100)

# Initialize H0 characteristics matrix
H0chars <- matrix(0, nrow = length(numberOfIterations), ncol = 3)

# Run permutation testing
for (ni in 1:length(numberOfIterations)) {
  nPerms <- numberOfIterations[ni]
  permMeans <- numeric(nPerms)
  
  for (permi in 1:nPerms) {
    permMeans[permi] <- mean(sign(rnorm(N)) * data4perm)
  }
  
  # H0 distribution characteristics
  H0chars[ni, 1] <- mean(abs(permMeans) > abs(obsMean))  # p-value
  H0chars[ni, 2] <- mean(permMeans)                      # H0 distribution mean
  H0chars[ni, 3] <- IQR(permMeans)                       # distribution width (IQR)
}

# Adjust H0 characteristics to mean center
H0chars <- sweep(H0chars, 2, colMeans(H0chars), "-")


# Create the plot
p <- ggplot(data.frame(Iterations = rep(numberOfIterations, each = 3),
                       Value = as.vector(H0chars),
                       Characteristic = rep(c("P-value", "H0 mean", "H0 width"), times = length(numberOfIterations))),
            aes(x = Iterations, y = Value, color = Characteristic)) +
  geom_point(shape = 1, size = 3) +
  labs(x = "Number of iterations", y = "Distribution characteristic value") +
  theme_minimal() +
  scale_color_manual(values = c("P-value" = "blue", "H0 mean" = "red", "H0 width" = "green"))

# Print the plot
print(p)

# Save the plot
ggsave("permute_numIters.png", p, width = 8, height = 4)














#-------- Exercise 1 ---------#
#-----------------------------#
# Set parameters
N <- 55
nIterations <- 1000

# Generate the data and its mean
theData <- (rnorm(N, mean=0, sd=1))^2 - 1
theMean <- mean(theData)

### Perform Permutation Test
# Initialize vector for permutation means
permMeans <- numeric(nIterations)

# Run permutation test
for (permi in 1:nIterations) {
  # The data with random sign flips
  signFlippedData <- sign(rnorm(N)) * theData
  
  # Compute its mean
  permMeans[permi] <- mean(signFlippedData)
}


### Compute z-score and p-value
# Z-score relative to H0 distribution
zVal <- (theMean - mean(permMeans)) / sd(permMeans)
pVal <- 2 * (1 - pnorm(abs(zVal)))

# Print the z/p values
cat(sprintf("z = %.2f, p = %.3f", zVal, pVal))



## redefine parameters; feel free to modify :)
N <- 55
nPermTests <- 750
nIterations <- 1000


# Generate the data and compute its mean
theData <- (rnorm(N, mean = 0, sd = 1))^2 - 1
theMean <- mean(theData)

# Initialize vector for z-scores
zVals <- numeric(nPermTests)

# Loop over all the permutation tests
for (ni in 1:nPermTests) {
  permMeans <- numeric(nIterations)
  
  for (permi in 1:nIterations) {
    permMeans[permi] <- mean(sign(rnorm(N)) * theData)
  }
  
  # Z-score relative to H0 distribution
  zVals[ni] <- (theMean - mean(permMeans)) / sd(permMeans)
}



### plotting
# Create data frames for plotting
data_histogram <- data.frame(Values = theData)
zvals_histogram <- data.frame(ZValues = zVals)

# Data histogram plot
p1 <- ggplot(data_histogram, aes(x = Values)) +
  geom_histogram(bins = 25, fill = "grey90", color = "black") +
  geom_vline(xintercept = 0, linetype = "dashed", size = 1) +
  geom_vline(xintercept = theMean, linetype = "dotted", color = "grey30", size = 1) +
  labs(title = "A) Data histogram", x = "Data values", y = "Counts") +
  theme_minimal()

# Z-values histogram plot
p2 <- ggplot(zvals_histogram, aes(x = ZValues)) +
  geom_histogram(bins = 20, fill = "grey50", color = "grey20") +
  labs(title = "B) Perm-z histogram", x = "Z values", y = "Counts") +
  theme_minimal()

# Arrange and save the plots
g <- grid.arrange(p1, p2, ncol = 2)

# Save the plot
ggsave("permute_ex1.png", g, width = 10, height = 4)






















#-------- Exercise 2 ---------#
#-----------------------------#
# Create non-normal data
N <- 100
data <- runif(N, min = -1, max = 1)
data <- data - mean(data)
h0val <- -0.11

# Other simulation parameters
nPerms <- 1000
numPermTests <- 1000

# Data for permutation testing
data4perm <- data - h0val
obsMean <- mean(data4perm)


# Initialize output variables
pvals <- numeric(numPermTests)

# Outer loop over many permutation tests
for (permRepeati in 1:numPermTests) {
  permMeans <- numeric(nPerms)
  
  for (permi in 1:nPerms) {
    randSigns <- sign(rnorm(N))
    permMeans[permi] <- mean(randSigns * data4perm)
  }
  
  # Compute and store the p-value
  pvals[permRepeati] <- mean(abs(permMeans) > abs(obsMean))
}


### Plotting
p1 <- ggplot(data.frame(Index = 1:N, Value = data), aes(x = Index, y = Value)) +
  geom_point(color = "black", fill = "grey90", shape = 21, size = 3) +
  geom_hline(yintercept = h0val, linetype = "dashed", size = 1) +
  geom_hline(yintercept = 0, linetype = "dotted", color = "grey30", size = 1) +
  labs(title = "A) Data and prediction", x = "Data index", y = "Data value") +
  theme_minimal()

# P-values histogram plot
p2 <- ggplot(data.frame(PValue = pvals), aes(x = PValue)) +
  geom_histogram(bins = 20, fill = "grey70", color = "grey20") +
  geom_vline(xintercept = 0.05, linetype = "dashed", size = 1) +
  labs(title = sprintf("B) Pc histogram (%.1f%% \"sig.\")", mean(pvals < 0.05) * 100), x = "P-values", y = "Counts") +
  theme_minimal()

# Arrange and save the plots
g <- grid.arrange(p1, p2, ncol = 2)

# Save the plot
ggsave("permute_ex2.png", g, width = 10, height = 4)


















#-------- Exercise 3 ---------#
#-----------------------------#
# set parameters and initialize
n1 <- 50
n2 <- 70

# Experiment parameters
nIterations <- 1000  # In each permutation test
numRepeats <- 541    # Number of times to generate new data

truelabels <- c(rep(1, n1), rep(2, n2))
pvals <- matrix(0, nrow = numRepeats, ncol = 2)

## run the experiment
for (expi in 1:numRepeats) {
  # Create new data
  data1 <- rnorm(n1)
  data2 <- rnorm(n2, mean = 0.3)
  
  # Pool the data into one variable
  alldata <- c(data1, data2)
  true_conddif <- mean(alldata[truelabels == 1]) - mean(alldata[truelabels == 2])
  
  # Null-hypothesis (H0) distribution
  permvals <- numeric(nIterations)
  for (permi in 1:nIterations) {
    shuflabels <- sample(truelabels)
    permvals[permi] <- mean(alldata[shuflabels == 1]) - mean(alldata[shuflabels == 2])
  }
  
  # p_z
  zVal <- (true_conddif - mean(permvals)) / sd(permvals)
  pvals[expi, 1] <- 2 * (1 - pnorm(abs(zVal)))  # Two-tailed
  
  # p_c
  pvals[expi, 2] <- sum(abs(permvals) > abs(true_conddif)) / nIterations
}


## visualize
p <- ggplot(data.frame(p_z = pvals[, 1], p_c = pvals[, 2]), aes(x = p_z, y = p_c)) +
  geom_point(color = "black", fill = "grey80", shape = 21, size = 3, alpha = 0.5) +
  geom_abline(slope = 1, intercept = 0, color = "black") +
  labs(x = "p-values from z-score", y = "p-values from counting", 
       title = sprintf("r = %.3f", cor(pvals[, 1], pvals[, 2]))) +
  theme_minimal()

# Print the plot
print(p)

# Save the plot
ggsave("permute_ex3.png", p, width = 5, height = 4)






















#-------- Exercise 4 ---------#
#-----------------------------#
nPerms <- 1000
N <- 30
h0val <- 0.5
pvals <- matrix(0, nrow = 100, ncol = 2)


## run the experiment!
for (iter in 1:100) {
  # Create the data (shifted by h0 such that H0=0)
  X <- rnorm(N) - h0val
  
  # Permutation testing
  permMeans <- numeric(nPerms)
  for (permi in 1:nPerms) {
    permMeans[permi] <- mean(sample(c(-1, 1), N, replace = TRUE) * X)
  }
  
  # p-value from permutation testing
  pvals[iter, 1] <- mean(abs(permMeans) > abs(mean(X)))
  
  # p-value from 1-sample t-test
  t_test_result <- t.test(X, mu = 0)
  pvals[iter, 2] <- t_test_result$p.value
}

# Replace p=0 with p=min
pvals[pvals == 0] <- min(pvals[pvals > 0])
pvals <- log(pvals)


## visualization
p <- ggplot(data.frame(Permutation = pvals[, 1], Parametric = pvals[, 2]), aes(x = Permutation, y = Parametric)) +
  geom_point(color = "black", fill = "black", shape = 21, size = 3, alpha = 0.5) +
  geom_abline(slope = 1, intercept = 0, color = "black") +
  labs(x = "Permutation log(p)", y = "Parametric log(p)") +
  theme_minimal()

# Print the plot
print(p)

# Save the plot
ggsave("permute_ex4.png", p, width = 6, height = 5)



























#-------- Exercise 5 ---------#
#-----------------------------#
N <- 30
nPerms <- 1000
stdevs <- seq(0.1, 2, length.out = 20)

# Initializations
results <- matrix(0, nrow = length(stdevs), ncol = 3)
H0dists <- list()


## run the experiment
for (si in 1:length(stdevs)) {
  s <- stdevs[si]
  
  # Create the data
  data <- rnorm(N, mean = 0.2, sd = s)
  
  # Permutation testing
  permMeans <- numeric(nPerms)
  for (permi in 1:nPerms) {
    permMeans[permi] <- mean(sample(c(-1, 1), N, replace = TRUE) * data)
  }
  
  # Store the t/z values
  results[si, 1] <- (mean(data) - mean(permMeans)) / sd(permMeans)
  results[si, 2] <- IQR(permMeans)
  t_test_result <- t.test(data, mu = 0)
  results[si, 3] <- t_test_result$statistic
  
  # Store the extremiest H0 distributions
  if (si == 1) {
    H0dists[[1]] <- permMeans
  } else if (si == length(stdevs)) {
    H0dists[[2]] <- permMeans
  }
}


## plot the results
# Create data frames for plotting
results_df <- data.frame(Sigma = stdevs, ZValue = results[, 1], TValue = results[, 3], IQR = results[, 2])

# Plotting results
p1 <- ggplot(results_df, aes(x = Sigma)) +
  geom_line(aes(y = ZValue), color = "black", size = 1) +
  geom_line(aes(y = TValue), color = "grey60", linetype = "dashed", size = 1) +
  labs(title = "A) t and z values", x = expression("Population " * sigma), y = "t or z")

p2 <- ggplot(results_df, aes(x = Sigma, y = IQR)) +
  geom_point(color = "black", fill = "grey80", shape = 21, size = 3) +
  labs(title = expression("B) H"[0] * " distribution width tracks " * sigma), x = expression("Population " * sigma), y = expression("H"[0] * " IQR"))

# Histograms of the H0 distributions
p3 <- ggplot(data.frame(Value = H0dists[[1]]), aes(x = Value)) +
  geom_histogram(bins = 20, fill = "grey80", color = "black") +
  labs(title = sprintf("C) H0 distribution when sigma=%.2f", stdevs[1]), x = "Shuffled means", y = "Count")

p4 <- ggplot(data.frame(Value = H0dists[[2]]), aes(x = Value)) +
  geom_histogram(bins = 20, fill = "grey80", color = "black") +
  labs(title = sprintf("D) H0 distribution when sigma=%.2f", stdevs[length(stdevs)]), x = "Shuffled means", y = "Count")

# Arrange and save the plots
g <- grid.arrange(p1, p2, p3, p4, nrow = 2, ncol = 2)

# Save the plot
ggsave("permute_ex5.png", g, width = 10, height = 8)

























#-------- Exercise 6 ---------#
#-----------------------------#
## Simulation Parameters and Data Generation
n <- 50
r <- 0.2

x <- rnorm(n)
y <- rnorm(n)
y <- x * r + y * sqrt(1 - r^2)

# Mean-center
x <- x - mean(x)
y <- y - mean(y)

# Observed correlation and dot product
obs_r <- cor(x, y)
dp <- sum(x * y)


## Permutation testing
# Initialize output matrix
permRes <- matrix(0, nrow = 1000, ncol = 2)

# Permutation testing
for (permi in 1:1000) {
  # Shuffle y
  yshuf <- sample(y)
  
  # Pearson correlation
  permRes[permi, 1] <- cor(x, yshuf)
  
  # Mean-centered dot product
  permRes[permi, 2] <- sum(x * yshuf)
}

# z and p values
z_r <- (obs_r - mean(permRes[, 1])) / sd(permRes[, 1])
z_d <- (dp - mean(permRes[, 2])) / sd(permRes[, 2])

p_r <- 2 * (1 - pnorm(abs(z_r)))
p_d <- 2 * (1 - pnorm(abs(z_d)))


## visualization
# Create histograms for visualization
p1 <- ggplot(data.frame(Correlation = permRes[, 1]), aes(x = Correlation)) +
  geom_histogram(bins = 40, fill = "grey80", color = "black") +
  geom_vline(xintercept = obs_r, linetype = "dashed", color = "black", size = 1) +
  labs(title = paste("A) Correlation coeff. shuffles\n     z =", sprintf("%.3f, p = %.3f", z_r, p_r)),
       x = "Correlation coefficient", y = "Count")

p2 <- ggplot(data.frame(DotProduct = permRes[, 2]), aes(x = DotProduct)) +
  geom_histogram(bins = 40, fill = "grey80", color = "black") +
  geom_vline(xintercept = dp, linetype = "dashed", color = "black", size = 1) +
  labs(title = paste("B) Dot product shuffles\n     z =", sprintf("%.3f, p = %.3f", z_d, p_d)),
       x = "Dot product", y = "Count")

# Arrange and save the plots
g <- grid.arrange(p1, p2, ncol = 2)

# Save the plot
ggsave("permute_ex6.png", g, width = 10, height = 4)

















#-------- Exercise 7 ---------#
#-----------------------------#
anscombe <- matrix(
  c(10,  8.04, 10,  9.14, 10,  7.46,  8,  6.58,
    8,   6.95,  8,  8.14,  8,  6.77,  8,  5.76,
    13,  7.58, 13,  8.76, 13, 12.74,  8,  7.71,
    9,   8.81,  9,  8.77,  9,  7.11,  8,  8.84,
    11,  8.33, 11,  9.26, 11,  7.81,  8,  8.47,
    14,  9.96, 14,  8.10, 14,  8.84,  8,  7.04,
    6,   7.24,  6,  6.13,  6,  6.08,  8,  5.25,
    4,   4.26,  4,  3.10,  4,  5.39,  8,  5.56,
    12, 10.84, 12,  9.13, 12,  8.15,  8,  7.91,
    7,   4.82,  7,  7.26,  7,  6.42,  8,  6.89,
    5,   5.68,  5,  4.74,  5,  5.73, 19, 12.50), 
  nrow = 11, byrow = TRUE)

nSamples <- nrow(anscombe)
permRs <- numeric(1000)



## Permutation Testing and Plotting
plot_list <- list()

# Loop through the four series in Anscombe's quartet
for (i in 1:4) {
  x <- anscombe[, (i - 1) * 2 + 1]
  y <- anscombe[, (i - 1) * 2 + 2]
  
  # Compute the correlation and parametric p-value
  cor_test <- cor.test(x, y)
  r <- cor_test$estimate
  pp <- cor_test$p.value
  
  # Permutation testing
  for (permi in 1:1000) {
    permRs[permi] <- cor(x, sample(y))
  }
  pc <- mean(abs(permRs) >= abs(r))
  
  # Plotting
  p <- ggplot(data.frame(x, y), aes(x, y)) +
    geom_point(color = "black", fill = "grey70", shape = 21, size = 3) +
    labs(title = sprintf("r=%.2f, p=%.3f\npc=%.3f", r, pp, pc)) +
    theme_minimal() +
    theme(axis.text = element_blank(), axis.ticks = element_blank())
  
  plot_list[[i]] <- p
}

# Arrange and save the plots
g <- arrangeGrob(grobs = plot_list, nrow = 2, ncol = 2)
grid.arrange(grobs = plot_list, nrow = 2, ncol = 2) # in case the previous line doesn't render...

# Save the plot
ggsave("permute_ex7.png", g, width = 8, height = 6)




# btw, interesting to see that the possible permuted correlation values is limited
# due to the small sample size and limited data values... not an ideal situation for
# parametric or non-parametric analyses.

# Create histogram of permutation test results
p <- ggplot(data.frame(PermResults = permRs), aes(x = PermResults)) +
  geom_histogram(bins = 40, fill = "grey80", color = "black") +
  theme_minimal()

# Print the plot
print(p)

# Calculate and print the number of unique values
cat(sprintf("%d random permutations and only %d unique values!", length(permRs), length(unique(permRs))))


























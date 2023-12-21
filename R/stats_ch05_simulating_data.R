#----
# Modern statistics: Intuition, Math, Python, R
## Mike X Cohen (sincxpress.com)
#### https://www.amazon.com/dp/B0CQRGWGLY
#### Code for chapter 5 (simulating data)

# About this code file:
### This code file will reproduce most of the figures in this chapter 
### (some figures were made in Inkscape), and illustrate the statistical 
### concepts explained in the text. The point of providing the code is not 
### just for you to recreate the figures, but for you to modify, adapt, 
### explore, and experiment with the code.
###
### Solutions to all exercises are at the bottom of the file.
# Thanks to Oskar Soderbom for help with the translation from Python.



# import libraries
library(triangle)
library(extraDistr)
library(ggplot2)
library(gridExtra)
library(emg)










#-------- Figure 5.2: Normally distributed random data ---------#
#---------------------------------------------------------------#
means <- c(-1, 0, 1, 3)
stds <- c(1, 0.1, 3, 1.6)
samplesize <- 2500
plots <- list()

for (index in seq_along(means)) {
  X <- rnorm(n = samplesize, mean = means[index], sd = stds[index])
  empave <- mean(X)
  empstd <- sd(X)
  
  # Calculate the number of bins using the FD rule for each group
  binwidth_FD <- 2 * IQR(X) / (length(X)^(1/3))

  # Create a plot for each group
  group_label <- bquote(mu == .(means[index]) ~ ", " ~ bar(X) == .(round(empave, 4)) ~ 
                          ", " ~ sigma == .(stds[index]) ~ ", std(X)=" ~ .(round(empstd, 3)))
  plot <- ggplot(data.frame(X), aes(x = X)) +
    geom_histogram(binwidth = binwidth_FD, alpha = 0.5, position = "identity") +
    theme_minimal() +
    coord_cartesian(xlim = c(-15, 15)) + 
    ggtitle(group_label) +
    theme(axis.line.x = element_line(color = "black", linewidth = 1),
          axis.line.y = element_line(color = "black", linewidth = 1),
          panel.border = element_blank(),
          panel.background = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank())
  
  plots[[index]] <- plot
}

# Combine all plots
plot5_2 <- grid.arrange(grobs = plots, ncol = 2)
ggsave("simdat_normal4examples.png", plot = plot5_2, width = 8, height = 4)



















#-------- Figure 5.4: Uniformly distributed random data ---------#
#----------------------------------------------------------------#
ab <- sort(sample(-3:10, 2))
a <- ab[1]
b <- ab[2]
N <- 1001
Y <- runif(n = N, min = a, max = b)

# print
cat(sprintf("a: %.4f, b: %.4f, min(Y): %.4f, max(Y): %.4f\n", a, b, min(Y), max(Y)))
cat(sprintf("mean(Y): %.4f, (a+b)/2: %.4f, median(Y): %.4f\n", mean(Y), (a+b)/2, median(Y)))
cat(sprintf("var(Y): %.4f, (b-a)^2/12: %.4f\n", var(Y), (b-a)^2/12))


# parameters for the distributions
aa <- c(-1, 0, 2, 1)
bb <- c(1, 0.1, 3, 1.6)
samplesize <- 2500

plots5_4 <- list()

for (idx in seq_along(aa)) {
  # generate some data
  X <- runif(n = samplesize, min = aa[idx], max = bb[idx])
  bndL <- min(X)
  bndU <- max(X)
  
  # draw the histogram using the F-D rule for bin width
  binwidth_FD <- 2 * IQR(X) / (length(X)^(1/3))
  
  temp_df <- data.frame(X = X)
  group_label = paste0("a=", aa[idx], ", min(Y)=", round(bndL, 3), 
                       ", b=", bb[idx], ", max(Y)=", round(bndU, 3))
  
  plot5_4 <- ggplot(temp_df, aes(x = X)) +
    geom_histogram(binwidth = binwidth_FD, alpha = 0.5, position = 'identity') +
    theme_minimal() + 
    coord_cartesian(xlim = c(-1,3)) +
    ggtitle(group_label) +
    theme(axis.line.x = element_line(color = "black", linewidth = 1),
          axis.line.y = element_line(color = "black", linewidth = 1),
          panel.border = element_blank(),
          panel.background = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank())
  
  plots5_4[[idx]] <- plot5_4
  
  
}

plot5_4 <- grid.arrange(grobs = plots5_4, ncol = 2)
ggsave("simdat_uniform4examples.png", plot = plot5_4, width = 8, height = 4)


















#-------- Figure 5.5: Example Weibull distribution ---------#
#-----------------------------------------------------------#
# some data
X  <- rweibull(n = 5000, shape = 2)
df <- data.frame(X = X)


binwidth_FD <- 2 * IQR(df$X) / (length(df$X)^(1/3))

# create a histogram
plot5_5 <- ggplot(df, aes(x = X)) +
  geom_histogram(binwidth = binwidth_FD, fill = "gray80", color = "black") +
  theme_minimal() +
  labs(x = "Data values", y = "Counts", title = "Example Weibull distribution") +
  theme(axis.line.x = element_line(color = "black", linewidth = 1),
        axis.line.y = element_line(color = "black", linewidth = 1),
        panel.border = element_blank(),
        panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

print(plot5_5)
ggsave("simdat_weibull.png", plot = plot5_5, width = 8, height = 4)























#-------- Figure 5.6: Example log-normal distribution ---------#
#--------------------------------------------------------------#
# generate the data using a normal distribution passed through an exponential
Y <- rnorm(n = 5000, mean = 0, sd = 1)
X <- exp(Y * 0.5 + 1)

df <- data.frame(X = X)

plot5_6 <- ggplot(df, aes(x = X)) +
  geom_histogram(bins = 80, fill = "gray80", color = "black") +
  theme_minimal() +
  labs(x = "Data values", y = "Counts",
       title = "Example log-normal distribution") +
  theme(axis.line.x = element_line(color = "black", linewidth = 1),
        axis.line.y = element_line(color = "black", linewidth = 1),
        panel.border = element_blank(),
        panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())
print(plot5_6)

ggsave("simdat_lognormal.png", plot = plot5_6, width = 8, height = 4)



















#-------- Random integers ---------#
#----------------------------------#
z <- sample(-5:5, size = 10000, replace = TRUE)

df <- data.frame(z = z)

plot_rand_int <- ggplot(df, aes(x = z)) +
  geom_histogram(bins = length(unique(z)),
                 fill = "#313dee", color = "black") +
  theme_minimal() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

print(plot_rand_int)

z <- round(rnorm(n = 10000) * 10)
print(unique(z))
df <- data.frame(z = z)

# plot the data
plot_rand_int2 <- ggplot(df, aes(x = z)) +
  geom_histogram(fill = "#313dee", color = "black", bins = 30) +
  theme_minimal() +
  labs(x = "Data values", y = "Counts") +
  theme(axis.line.x = element_line(color = "black", linewidth = 1),
        axis.line.y = element_line(color = "black", linewidth = 1),
        panel.border = element_blank(),
        panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

print(plot_rand_int2)




















#-------- Random selection ---------#
#-----------------------------------#
s <- c(1, 2, pi, 10)
print(sample(s, 1))

#Not limited to numbers
t <- c("a", "b", "hello")
sample(t, 1)


print(sample(s, 4, replace = TRUE))
print(sample(s, 4, replace = FALSE))













#-------- Random Permutations ---------#
#--------------------------------------#
l = seq(0,4,1)
print(l)
print(sample(l))

# to randomly re-sort a dataset
theData  <- seq(-3,3)**3
newIdx   <- sample(length(theData))
shufData <- theData[newIdx]

print(theData)
print(newIdx)
print(shufData)
















#-------- Seeding the rng ---------#
#----------------------------------#
print(matrix(rnorm(n = 9), nrow = 3, ncol = 3))

set.seed(17)
print(matrix(rnorm(9), nrow = 3, ncol = 3))






















#-------- Figure 5.8: Running an experiment ---------#
#----------------------------------------------------#
# the key factor to manipulate
stds <- seq(.01, 10, length.out = 40)

# parameters to hold constant
samplesize <- 100
mean <- 0

# initialize results matrix
results <- numeric(length(stds))

# start the experiment
for (stdi in seq_along(stds)) {
  
  # data parameters for this experiment run
  thisStd <- stds[stdi]
  
  # generate data
  data <- rnorm(n = samplesize, mean = mean, sd = thisStd)
  
  # collect results
  results[stdi] <- mean(data)
}


# plot the results
df <- data.frame(stds = stds, results = results)
plot5_8 <- ggplot(df, aes(x = stds, y = results)) +
  geom_point(shape = 22, size = 4, fill = "gray90", color = "black") +
  geom_hline(yintercept = mean, linetype = "dashed", color = "gray30") +
  theme_minimal() +
  theme(axis.line.x = element_line(color = "black", linewidth = 1),
        axis.line.y = element_line(color = "black", linewidth = 1),
        panel.border = element_blank(),
        panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  labs(x = "Population standard deviation", y = "Empirical mean")

print(plot5_8)

# save the plot 
ggsave("simdat_experiment1.png", plot = plot5_8, width = 8, height = 4)
















#-------- Figure 5.9: A follow-up experiment ---------#
#-----------------------------------------------------#
# the key factors to manipulate
stds <- seq(.01, 10, length.out = 40)
samplesize <- c(100, 10000)

# Parameters to hold constant
meanvalue <- 0

# initialize results matrix
results <- matrix(0, nrow = length(stds), ncol = length(samplesize))

# start the experiment
for (stdi in seq_along(stds)) {
  for (sampi in seq_along(samplesize)) {
    
    # data parameters for this experiment run
    thisStd <- stds[stdi]
    thisN <- samplesize[sampi]
    
    # generate data
    data <- rnorm(n = thisN, mean = meanvalue, sd = thisStd)
    
    # collect results
    results[stdi, sampi] <- mean(data)
  }
}

# plot the results

df <- data.frame(stds = rep(stds, each = length(samplesize)),
                 results = c(results),
                 samplesize = rep(samplesize, times = length(stds)))

plot_5_9 <- ggplot(df, aes(x = stds, y = results, color = factor(samplesize), 
                            shape = factor(samplesize))) +
  geom_point(size = 4, fill = "gray90") +
  scale_color_manual(values = c("gray30", "gray60"), labels = c("N = 100", "N = 10000")) +
  scale_shape_manual(values = c(22, 20), labels = c("N = 100", "N = 10000")) +
  labs(x = "Population standard deviation", y = "Empirical mean", color = "N", shape = "N") +
  geom_hline(yintercept = meanvalue, linetype = "dashed", color = "gray30") +
  guides(color = guide_legend(title = NULL), shape = guide_legend(title = NULL)) +
  theme_minimal() +
  theme(axis.line.x = element_line(color = "black", linewidth = 1),
        axis.line.y = element_line(color = "black", linewidth = 1),
        panel.border = element_blank(),
        panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

print(plot_5_9)

# save the plot
ggsave("simdat_experiment2.png", plot = plot_5_10, width = 8, height = 4)















#-------- Exercise 1 ---------#
#-----------------------------#

# Reminder: the third argument in rnorm is the standard deviation 
# whereas the exercise specified the variance.
# You can input the square root of the variance.
X <- rnorm(n = 10000, mean = 0, sd = sqrt(2))

# report the mean and variance
cat(sprintf("Empirical mean = %.3f\n", mean(X)))
cat(sprintf("Empirical variance = %.3f\n", var(X)))

# sample sizes
Ns <- seq(10, 10200, by = 200)

# initialize outputs
means <- numeric(length(Ns))
vars  <- numeric(length(Ns))

# run experiment
for (i in seq_along(Ns)) {
  
  # generate random data
  X <- rnorm(n = Ns[i], mean = 0, sd = sqrt(2))
  # Note: the third input in rnorm is the standard deviation whereas 
  #the exercise specified the variance. But 1**2==1, so it works out here.
  
  # compute mean and variance
  means[i] <- mean(X)
  vars[i]  <- var(X)
}

# plot the results

df1 <- data.frame(Ns = Ns, means = means)
plot_exercise_1_1 <- ggplot(df1, aes(x = Ns, y = means)) +
  geom_point(shape = 15, size = 4, color = "gray50") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray40") +
  theme_minimal() +
  labs(x = "Sample size", y = "Mean value") +
  ggtitle("A) Mean value (expected 0)") +
  theme(axis.line.x = element_line(color = "black", linewidth = 1),
        axis.line.y = element_line(color = "black", linewidth = 1),
        panel.border = element_blank(),
        panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

df2 <- data.frame(Ns = Ns, vars = vars)
plot_exercise_1_2 <- ggplot(df2, aes(x = Ns, y = vars)) +
  geom_point(shape = 15, size = 4, color = "gray50") +
  geom_hline(yintercept = 2, linetype = "dashed", color = "gray40") +
  theme_minimal() +
  labs(x = "Sample size", y = "Variance value") +
  ggtitle("B) Variance value (expected 2)") +
  theme(axis.line.x = element_line(color = "black", linewidth = 1),
        axis.line.y = element_line(color = "black", linewidth = 1),
        panel.border = element_blank(),
        panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())


plots_ex1 <- grid.arrange(plot_exercise_1_1, plot_exercise_1_2, ncol = 1)

# save the plots
ggsave("simdat_ex1.png", plot = plots_ex1, width = 8, height = 3)























#-------- Exercise 2 ---------#
#-----------------------------#
# uniform data with boundaries [-3,8]
a <- -3
b <- 8
Y <- runif(n = 1324, min = a, max = b)

# compute mean and variance discrepancies
meanDiff <- mean(Y) - (a+b)/2
varDiff  <- (var(Y) - (b-a)^2/12)^2

# print the results
cat(sprintf("Mean discrepancy (signed): %.3f\n", meanDiff))
cat(sprintf("Variance discrepancy (squared): %.3f\n", varDiff))

# histogram
hist(Y, breaks = "FD", xlab = "Data values", ylab = "Count", main = "", 
     col="#313dee")


# sample sizes
Ns <- seq(10, 10200, by = 200)

# initialize outputs
means <- numeric(length(Ns))
vars  <- numeric(length(Ns))

# run experiment
for (i in seq_along(Ns)) {
  
  # generate random data (sorting to ensure a>b!)
  ab <- sort(sample(-3:10, 2))
  a <- ab[1]
  b <- ab[2]
  Y <- runif(n = Ns[i], min = a, max = b)
  
  # compute mean and variance discrepancies
  means[i] <- mean(Y) - (a+b)/2
  vars[i]  <- (var(Y) - (b-a)^2/12)^2
}

# plot the results

df1 <- data.frame(Ns = Ns, means = means)
df2 <- data.frame(Ns = Ns, vars = vars)

plot_exercise_2_1 <- ggplot(df1, aes(x = Ns, y = means)) +
  geom_point(shape = 15, size = 4, color = "gray40") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray40") +
  theme_minimal() +
  labs(x = "Sample size", y = "Mean discrepancy") +
  ggtitle("A) Mean value difference (expected 0)") +
  theme(axis.line.x = element_line(color = "black", linewidth = 1),
        axis.line.y = element_line(color = "black", linewidth = 1),
        panel.border = element_blank(),
        panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())


plot_exercise_2_2 <- ggplot(df2, aes(x = Ns, y = vars)) +
  geom_point(shape = 15, size = 4, color = "gray40") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray40") +
  theme_minimal() +
  labs(x = "Sample size", y = "Squared var discrepancy") +
  ggtitle("B) Variance difference (expected 0)") +
  theme(axis.line.x = element_line(color = "black", linewidth = 1),
        axis.line.y = element_line(color = "black", linewidth = 1),
        panel.border = element_blank(),
        panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

plots_ex2 <- grid.arrange(plot_exercise_2_1, plot_exercise_2_2, ncol = 1)

# export
ggsave("simdat_ex2.png", plot = plots_ex2, width = 8, height = 3)

























#-------- Exercise 3 ---------#
#-----------------------------#
# parameters
mu <- 2
sigma <- 1.5

# normally distributed numbers
normal <- rnorm(n = 10000, mean = 0, sd = 1)

# transform to log-normal
lognorm <- exp(normal*sigma + mu)

# compute the empirical mean
empMean <- mean(lognorm)

# and back-transform
empMeanInv <- log(empMean) - sigma^2/2

# report the mean and its transform
cat(sprintf("Mean of log-normal data is %.3f\n", empMean))
cat(sprintf("Transformed mean of log-normal data is %.3f\n", empMeanInv))



mus <- seq(1, 10, length.out = 13)

means <- numeric(length(mus))

# run experiment
for (i in seq_along(mus)) {
  
  # normally distributed data (different implementation compared to 
  #the previous code just to show multiple correct answers)
  
  normal <- rnorm(n = 10000) * sigma  + mus[i]
  
  # transform to log-normal
  lognorm <- exp(normal)
  
  # empirical mean
  means[i] <- mean(lognorm)
}


df1 <- data.frame(mus = mus, means = means)
plot_exercise_3_1 <- ggplot(df1, aes(x = mus, y = means)) +
  geom_point(shape = 15, size = 4, color = "gray40") +
  geom_line() +
  theme_minimal() +
  labs(x = expression(mu), y = expression(bar("Y"))) +
  theme(axis.line.x = element_line(color = "black", linewidth = 1),
        axis.line.y = element_line(color = "black", linewidth = 1),
        panel.border = element_blank(),
        panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

df2 <- data.frame(mus = mus, log_means = log(means) - sigma^2/2)
plot_exercise_3_2 <- ggplot(df2, aes(x = mus, y = log_means)) +
  geom_point(shape = 15, size = 4, color = "gray40") +
  geom_line() +
  theme_minimal() +
  labs(x = expression(mu), 
       y = expression(paste("ln(", bar("Y"), ")" - frac(1,2), sigma^2))) +
  theme(axis.line.x = element_line(color = "black", linewidth = 1),
        axis.line.y = element_line(color = "black", linewidth = 1),
        panel.border = element_blank(),
        panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())
  


plots_ex3 <- grid.arrange(plot_exercise_3_1, plot_exercise_3_2, ncol = 2)

# save the plots
ggsave("simdat_ex3.png", plot = plots_ex3, width = 5, height = 4)


# you can also compare by subtraction:
log(means) - sigma^2/2 - mus
# discrepancies are not exactly zero, 
#but they are around 6 orders of magnitude smaller than mean(lognorm)!

























#-------- Exercise 4 ---------#
#-----------------------------#
ab <- sort(sample(-3:10, 2))
a <- ab[1]
b <- ab[2]
N <- 1001
Y <- runif(n = N, min = a, max = b)

# means according to the formulas
mu_a <- sqrt(3) * sd(Y) + a
mu_b <- b - sqrt(3) * sd(Y)

# print
cat(sprintf("mu from a : %.4f\n", mu_a))
cat(sprintf("mu from b : %.4f\n", mu_b))
cat(sprintf("mean(Y)   : %.4f\n", mean(Y)))
cat(sprintf("avg bounds: %.4f\n", (a+b)/2))



# stds according to the formulas
sig_a <- (mean(Y) - a) / sqrt(3)
sig_b <- (b - mean(Y)) / sqrt(3)

# print
cat(sprintf("sigma from a : %.4f\n", sig_a))
cat(sprintf("sigma from b : %.4f\n", sig_b))
cat(sprintf("std(Y)       : %.4f\n", sd(Y)))


# now to simulate data from mu/sigma parameters
# desired parameters
mu <- 3.5
sig <- 1.8

# generate the data
U <- runif(n = 100000, min = 0, max = 1)
Y <- mu + sqrt(3)*sig*(2*U - 1)

# print
cat(sprintf("Empirical mean: %.3f\n", mean(Y)))
cat(sprintf("Empirical std : %.3f\n", sd(Y)))





















#-------- Exercise 5 ---------#
#-----------------------------#
# Expected median is the same as the mean: (a+b)/2
#
# Expected mode is any/all values! 
# The probability of any one value is the same as that of any other value.
# Of course, in a finite sample, there is likely to be one or a small number of 
# modes due to sampling variability.


























#-------- Exercise 6 ---------#
#-----------------------------#
# reference: https://en.wikipedia.org/wiki/Triangular_distribution

# parameters
a <- 0.2
c <- 0.6
b <- 0.9
N <- 10000 # sample size

# function F(c) (as defined in wiki page)
Fc <- (c - a) / (b - a)

# initialize U and y
U <- runif(N)
y <- numeric(N)

# apply transformation
y[U < Fc] <- a + sqrt( U[U < Fc] * (b - a) * (c - a) )
y[U > Fc] <- b - sqrt( (1 - U[U > Fc]) * (b - a) * (b - c) )


# create a histogram
df <- data.frame(y = y)

binwidth_FD <- 2 * IQR(df$y) / (length(df$y)^(1/3))

ggplot(df, aes(x = y)) +
geom_histogram(binwidth = binwidth_FD, fill = "gray80", color = "black") +
xlim(a - 0.2, b + 0.2) +
xlab("Data values") +
ylab("Counts") +
ggtitle(sprintf("Triangular distribution with a=%.1f, b=%.1f, c=%.1f", a, b, c)) +
theme_minimal() +
theme(axis.line.x = element_line(color = "black", linewidth = 1),
      axis.line.y = element_line(color = "black", linewidth = 1),
      panel.border = element_blank(),
      panel.background = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank())

# save the plot
ggsave("simdat_ex6.png", width = 8, height = 4)


# repeat using built-in R function
Y <- rtriangle(N, a, b, c)

# create a histogram
df <- data.frame(Y = Y)

binwidth_FD <- 2 * IQR(df$Y) / (length(df$Y)^(1/3))

ggplot(df, aes(x = Y)) +
geom_histogram(binwidth = binwidth_FD, fill = "gray80", color = "black") +
xlim(a - 0.2, b + 0.2) +
xlab("Data values") +
ylab("Counts") +
ggtitle(sprintf("Triangular distribution with a=%.1f, b=%.1f, c=%.1f", a, b, c)) +
theme_minimal() +
theme(axis.line.x = element_line(color = "black", linewidth = 1),
      axis.line.y = element_line(color = "black", linewidth = 1),
      panel.border = element_blank(),
      panel.background = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank())























#-------- Exercise 7 ---------#
#-----------------------------#
# create an normally distributed integer dataset from a standard normal
# rounding a standard normal
X <- rnorm(n = 100000, mean = 0, sd = 1)
X <- round(X)

# notice how there are few 9 unique values:
print(unique(X))



# the histogram looks... thin
df <- data.frame(X = X)
binwidth_FD <- 2 * IQR(df$X) / (length(df$X)^(1/3))

ggplot(df, aes(x = X)) +
  geom_histogram(binwidth = binwidth_FD, fill = "lightblue", 
                 color = "lightblue") +
  ggtitle(sprintf("%d unique numbers", length(unique(X)))) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  ylab(NULL)


## Technically, the solution above is correct, but it feels unsatisfying to me,
## because the number of unique elements is so small. A simple solution is to
## increase the width of the distribution.

# rounding a normal with larger sigma
X <- rnorm(n = 100000, mean = 0, sd = 15)
X <- round(X)


# the histogram looks better
df <- data.frame(X = X)
binwidth_FD <- 2 * IQR(df$X) / (length(df$X)^(1/3))



ggplot(df, aes(x = X)) +
  geom_histogram(binwidth = binwidth_FD, fill = "lightblue", color = "blue") +
  ggtitle(sprintf("%d unique numbers", length(unique(X)))) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  ylab(NULL)























#-------- Exercise 8 ---------#
#-----------------------------#
N <- 100

# create the data matrix
M <- matrix(rnorm(N * 2), ncol = 2)
M[,2] <- M[,2] + M[,1]

# correlation coefficient
# Note: the output is a matrix, and we want an off-diagonal element.
r_real <- cor(M)[2,1]

# now to shuffle the data
ridx <- sample(N)

# make a copy of the data
Mshuf <- M
Mshuf[,1] <- Mshuf[ridx,1]

# new correlation
r_shuf <- cor(Mshuf)[2,1]

# report the real and shuffled correlation coefficients
# (not formally requested in the text, but a nice addition ;) )
cat(sprintf("Real correlation r = %.3f\n", r_real))
cat(sprintf("Shuffled correlation r = %.3f\n", r_shuf))

# create a data frame
df <- data.frame(
  x = c(M[,1], Mshuf[,1]), 
  y = c(M[,2], Mshuf[,2]),
  type = rep(c("Real", "Shuffled"), each = N)
)

# plot
p1 <- ggplot(subset(df, type == "Real"), aes(x = x, y = y)) +
  geom_point() +
  xlab("Variable x") +
  ylab("Variable y") +
  ggtitle(sprintf("A) Real correlation r = %.2f", r_real)) +
  theme_minimal()  +
  theme(axis.line.x = element_line(color = "black", linewidth = 1),
        axis.line.y = element_line(color = "black", linewidth = 1),
        panel.border = element_blank(),
        panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

p2 <- ggplot(subset(df, type == "Shuffled"), aes(x = x, y = y)) +
  geom_point() +
  xlab("Variable x (shuffled)") +
  ylab("Variable y") +
  ggtitle(sprintf("B) Shuffled correlation r = %.2f", r_shuf)) +
  theme_minimal() +
  theme(axis.line.x = element_line(color = "black", linewidth = 1),
        axis.line.y = element_line(color = "black", linewidth = 1),
        panel.border = element_blank(),
        panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) 

# Arrange the plots side by side
plots_ex8 <- grid.arrange(p1, p2, ncol = 2)

ggsave("simdat_ex8.png", plot = plots_ex8, width = 10, height = 4)





















#-------- Exercise 9 ---------#
#-----------------------------#
# sample size
N <- 3000

# run one of these lines...
#data <- remg(N, lambda = 3)
#data <- rnorm(N, mean = 3, sd = 1)
data <- rlaplace(N)
#data <- rgumbel(N)

# descriptives
mean <- mean(data)
std <- sd(data)

# Histogram data preparation
breaks <- hist(data, breaks= 'FD', plot = FALSE)$breaks
bin_widths <- diff(breaks)
xx <- (breaks[-length(breaks)] + breaks[-1]) / 2

# find bars within 1 std of the mean
closeBars <- xx > (mean - std) & xx < (mean + std)
hist_counts <- hist(data, breaks= breaks,plot = FALSE)$counts


df <- data.frame(x = xx, y = hist_counts, 
                 group = ifelse(closeBars, "within", "outside"))

# show me the data!

# scatter plot of the data
scatter_9 <- ggplot(data.frame(index = 1:N, value = data), aes(x = index, y = value)) +
  geom_point(color = "gray30", fill = "gray80", alpha = 0.2, shape = 21) +
  geom_hline(yintercept = c(mean - std, mean + std), 
             linetype = "dashed", color = "black") +
  labs(x = "Data index", y = "Data value") +
  xlim(c(-10, N + 10)) +
  ggtitle(expression(bold("A) Raw data values"))) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        axis.line.x = element_line(color = "black", linewidth = 1),
              axis.line.y = element_line(color = "black", linewidth = 1),
              panel.border = element_blank(),
              panel.background = element_blank(),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank()) 

# now show the histogram
histogram_9 <- ggplot(df, aes(x = x, y = y, fill = group)) +
  geom_bar(stat = "identity", color = "black", width = bin_widths) +
  scale_fill_manual(values = c("gray70", "gray20"), 
                    labels = c(expression(x > (bar("x") %+-% sigma)), 
                               expression(x %in% (bar("x") %+-% sigma)))) +
  labs(x = "Data value", y = "Count") +
  guides(fill = guide_legend(title = "")) +
  theme_minimal() +
  theme(legend.title = element_blank(),
        axis.line.x = element_line(color = "black", linewidth = 1),
        axis.line.y = element_line(color = "black", linewidth = 1),
        panel.border = element_blank(),
        panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  ggtitle(expression(bold("B) Data histogram")))

plot_ex9 <- grid.arrange(scatter_9, histogram_9, ncol = 2)

ggsave('simdat_ex9.png', plot = plot_ex9 ,width = 10, height = 4)

# Link to a python list of distributions you can look up in R.
# https://docs.scipy.org/doc/scipy/reference/stats.html#continuous-distributions
























#-------- Exercise 10 ---------#
#------------------------------#
# the key factors to manipulate
stds <- seq(0.01, 10, length.out = 40)

# parameters to hold constant
samplesize <- 10000
mean <- 0

# initialize results vector
results <- rep(0, length(stds))
expectedMean <- rep(0, length(stds))


# start the experiment
for (stdi in seq_along(stds)) {

  # data parameters for this experiment run
  thisStd <- stds[stdi]

  # generate data
  X <- rnorm(samplesize, 0, 1)
  data <- exp(X * thisStd + mean)
  
  # collect results
  results[stdi] <- mean(data)
  
  # expected average
  expectedMean[stdi] <- exp(mean + thisStd^ 2 / 2)
}


# plot the results
df <- data.frame(stds = stds, results = log(results), expectedMean = log(expectedMean))
ggplot(df, aes(x = stds)) +
geom_point(aes(y = results), shape = 19, size = 2, fill = "gray90", color = "black") +
geom_line(aes(y = expectedMean), linetype = "dashed", color = "black") +
labs(x = "Standard deviation", y = "Empirical mean (log)") +
theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
      panel.background = element_rect(fill = "white", colour = "black"),
      axis.line = element_line(color = "black"))

# save the plot
ggsave("simdat_ex10a.png", width = 8, height = 4)

# values of sigma to illustrate
sigmas <- c(1, 10)

# create a data frame
df <- data.frame()

# make the figure
for (s in sigmas) {
  # generate data
  Y <- exp(rnorm(10000) * s)
  
  # add to the data frame
  df <- rbind(df, data.frame(Y = log(Y), sigma = s, Mean = log(mean(Y))))
}

# Custom labels for facets
labels <- c( `1` = "A) σ = 1", 
            `10` = "B) σ = 10")

# plot the results
plot_ex10b <- ggplot(df, aes(x = Y)) +
  geom_histogram(aes(fill = "Data"), bins = 100, color = "grey", fill = "black") +
  geom_vline(data = df, aes(xintercept = Mean, linetype = "Mean"), color = "gray") +
  facet_wrap(~ sigma, labeller = labeller(sigma = labels), scales = "free_x") +
  labs(x = "ln(data) value", y = "Count", fill = "", color = "") +
  theme_minimal() +
  theme(
    strip.background = element_blank(),
    strip.text = element_text(face = "bold"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "white", colour = "black"),
    axis.line = element_line(color = "black"),
    plot.title = element_text(hjust = 0.5),
    legend.position = "middle"
  ) +
  guides(fill = guide_legend(title = "Data"), color = guide_legend(title = "Mean"))
plot_ex10b

# save the plot
ggsave("simdat_ex10b.png", plot = plot_ex10b, width = 10, height = 4)











#----
# Modern statistics: Intuition, Math, Python, R
## Mike X Cohen (sincxpress.com)
#### https://www.amazon.com/dp/B0CQRGWGLY
#### Code for chapter 6 (transformations)

# About this code file:
### This code file will reproduce most of the figures in this chapter 
### (some figures were made in Inkscape), and illustrate the statistical 
### concepts explained in the text. The point of providing the code is not 
### just for you to recreate the figures, but for you to modify, adapt, 
### explore, and experiment with the code.
###
### Solutions to all exercises are at the bottom of the file.
# Thanks to Omar David Abdul Raouf Hassoun for help with the translation from Python.



#----
# import packages and define global settings
library(ggplot2)
library(gridExtra)
library(caret)
library(e1071)
library("dplyr")


# Set global plot options
theme_set(theme_bw(base_size = 14) + 
            theme(plot.title.position = "plot",  # Align title to the left
                  plot.title = element_text(hjust = 0),
                  axis.line = element_blank(),    # Remove axis lines
                  panel.border = element_blank(), # Remove border
                  axis.text.x = element_text(angle = 90, vjust = 0.5), # Optional: Rotate x axis text if needed
                  axis.ticks = element_blank())   # Remove axis ticks
          )




#-------- Code to introduce z-scoring ---------#
#----------------------------------------------#
# Define the vector X
X <- c(1, 4, -5, 2, 1)

# Calculate the mean (mu) and standard deviation (sigma) with Bessel's correction (ddof=1)
mu <- mean(X)
sigma <- sd(X)  # 'sd' function in R uses ddof=1 by default

# Compute the z-scores for the vector X
X_z <- (X - mu) / sigma

# Print the z-scores
print(X_z)

# Printing the formatted output
cat("       Original | z-transformed\n")
cat(sprintf("Mean:      %.2f | %.2f\n", mean(X), mean(X_z)))
cat(sprintf("stdev:     %.2f | %.2f\n", sd(X), sd(X_z)))





















#-------- Figure 6.1: Example distributions of height and weight ---------#
#-------------------------------------------------------------------------#

### Note about this code: This code actually provides my solution to Exercise 6.
# So if you want the full experience of the exercise, don't inspect this code too carefully!

# Fake heights and weights, in units of cm and kg
N <- 3425
height <- atanh(runif(N, -.9, .8)) * 20 + 160 + rnorm(N) * 3
weight <- atanh(runif(N, -.3, .99)) * 10 + 70 + rnorm(N) * 3

# Our imaginary individual
ind_height <- 177
ind_weight <- 70

# Z-score the distributions
height_z <- (height - mean(height)) / sd(height)
weight_z <- (weight - mean(weight)) / sd(weight)

# Z-score the individual
ind_height_z <- (ind_height - mean(height)) / sd(height)
ind_weight_z <- (ind_weight - mean(weight)) / sd(weight)


# Create a combined plot function
plot_distribution <- function(data, individual, title, xlab, xlims) {
  ggplot(data, aes(x = value)) +
    geom_histogram(bins = 30, fill = 'grey', color = 'black') +
    geom_vline(xintercept = individual, color = 'black', linetype = 'dashed') +
    labs(title = title, x = xlab, y = 'Count') +
    xlim(xlims) + 
    theme(plot.title = element_text(size = 10))
}

# Create a combined plot function for individual points
plot_individual <- function(individual, title, xlab, xlims) {
  ggplot() +
    geom_vline(xintercept = individual, color = 'black', linetype = 'dashed') +
    labs(title = title, x = xlab) +
    xlim(xlims)
}

# Create plots
p1 <- plot_individual(ind_height, expression(bold(A[1])*")" ~ "Raw height"), 'Height (cm)', c(ind_height - 50, ind_height + 50))
p2 <- plot_individual(ind_weight, expression(bold(A[1])*")" ~ "Raw weights"), 'Weight (kg)', c(ind_weight - 20, ind_weight + 20))

p3 <- plot_distribution(data.frame(value = height), ind_height, expression(bold(B[1])*")" ~ "Histogram of raw heights"), 'Height (cm)', c(ind_height - 50, ind_height + 50))
p4 <- plot_distribution(data.frame(value = weight), ind_weight, expression(bold(B[2])*")" ~ "Histogram of raw heights"), 'Weight (kg)', c(ind_weight - 20, ind_weight + 20))

p5 <- plot_distribution(data.frame(value = height_z), ind_height_z, expression(bold(C[1])*")" ~ "Histogram of z-heights"), 'Normalized height (z)', c(-3, 3))
p6 <- plot_distribution(data.frame(value = weight_z), ind_weight_z, expression(bold(C[2])*")" ~ "Histogram of z-weights"), 'Normalized weight (z)', c(-3,3))

# final plot call
p <- grid.arrange(
  p1, p3, p5,
  p2, p4, p6,
  ncol = 3
)

# Save the combined plot
ggsave('trans_zscore_example.png', plot = p, width = 12, height = 6)

























#----------- Figure 6.2: Z-scoring preserves relative values -------------#
#-------------------------------------------------------------------------#

# Generate random data
x <- rnorm(28, mean = 250, sd = 58)

# Calculate Z-transformed data
zx <- (x - mean(x)) / sd(x)

# Create a scatterplot
ggplot(data.frame(x, zx), aes(x, zx)) +
  geom_point(shape = 15, size = 4, fill = "gray40", alpha = 0.6) +
  labs(x = "Original data", y = "Z-transformed data") +
  theme_minimal()

# Save the plot as a PNG file
ggsave("trans_zRelativelyEqual.png", width = 4, height = 4)























#------------ Figure 6.3: Z-scoring a non-normal distribution ------------#
#-------------------------------------------------------------------------#
#function for FD
fd_bins <- function(x) {
  h <- 2 * IQR(x) * length(x)^(-1/3)
  max_x <- max(x, na.rm = TRUE)
  min_x <- min(x, na.rm = TRUE)
  return(ceiling((max_x - min_x) / h))
}


# Set the number of data points
N <- 5000

# Generate exponential distribution and scale
X <- (1 + rexp(N)) * 10

# Standardize the data
Xz <- scale(X)

# Create a dataframe for ggplot
df_raw <- data.frame(Value = X)
df_z <- data.frame(Value = Xz)

# Plotting the histograms
p1 <- ggplot(df_raw, aes(x=Value)) +
      geom_histogram(bins=fd_bins(X), fill="gray", color="black") +
      geom_vline(aes(xintercept=mean(Value)), color="black", linetype="dashed") +
      labs(title=  bquote(bold("A)") ~ "Histogram of raw data"), x="Raw data value", y="Count") +
      xlim(8, 80)

p2 <- ggplot(df_z, aes(x=Value)) +
      geom_histogram(bins=fd_bins(Xz), fill="gray", color="black") +
      geom_vline(aes(xintercept=mean(Value)), color="black", linetype="dashed") +
      labs(title=bquote(bold("B)") ~ " Histogram of z-transformed data"), x="Z-transformed data value", y="Count") +
      xlim(-1.2, 5)

# Arrange plots in a grid
p <- grid.arrange(p1, p2, ncol=1)

# Save the plot
ggsave("trans_zscore_positive.png", plot=p, width=8, height=7)



















#------------ Figure 6.4: Modified-z ------------#
#------------------------------------------------#
# Generate non-normal data
x1 <- rnorm(500, mean = 2.5)
x2 <- rnorm(2500, mean = -2)
y <- c(x1, x2)
y <- y - min(y) + 3

# Regular z-score
y_z <- scale(y)

# Modified z-score using MAD
MAD <- median(abs(y - median(y)))
y_zm <- qnorm(3/4) * (y - median(y)) / MAD

# Create a dataframe for ggplot
data <- data.frame(y = y, y_z = y_z, y_zm = y_zm)

# Histogram for original data
p1 <- ggplot(data, aes(y)) +
      geom_histogram(bins = 30, fill = "gray", color = "black") +
      labs(title = bquote(bold("A)") ~ "Original data dist."), x = "Data value", y = "Counts")

# Histograms for z-scores
p2 <- ggplot(data) +
  geom_freqpoly(aes(x = y_z, linetype = "regular"), bins = 30, color = "black", alpha = 0.7, show.legend = TRUE) +
  geom_freqpoly(aes(x = y_zm, linetype = "dashed"), bins = 30, color = "black", alpha = 0.4, show.legend = TRUE) +
  labs(title = bquote(bold("B)") ~ "Z-score dist."), x = "Transformed value", y = "Counts", color = NULL, linetype = NULL) +
  scale_x_continuous(limits = c(min(c(y_z, y_zm)), max(c(y_z, y_zm)))) +
  scale_linetype_manual(values = c("regular" = "solid", "dashed" = "dashed")) +
  guides(fill = guide_legend(title = NULL, override.aes = list(alpha = 1, linetype = c("regular", "dashed")))) + # Customize the legend title and linetype labels here
 theme(
    legend.position = c(.95, .95),
    legend.justification = c("right", "top"),
    legend.box.just = "right",
    legend.margin = margin(1, 1,1,1)
    )

# Scatter plot comparing z-scores
p3 <-ggplot(data, aes(x = y_z, y = y_zm)) +
  geom_point(color = "black", shape = 1) +
  geom_abline(aes(intercept = 0, slope = 1, linetype = "dashed")) +
  labs(title =  bquote(bold("C)") ~ "Comparison"), x = "\"Regular\" z", y = "Modified z") +
  scale_linetype_manual(name = NULL, 
                        values = c("dashed" = "dashed"),
                        breaks = "dashed",
                        labels = "Unity") + 
 theme(
    legend.position = c(0, 1),
    legend.justification = c("left", "top"),
    legend.box.just = "left",
    legend.margin = margin(1, 1,1,1)
    )


# Arrange plots in a grid
p <- grid.arrange(p1, p2, p3, ncol = 3, heights = c(0.3, 0.2))

# Save the plot
ggsave("trans_modVreg_zscore.png", plot=p, width = 10, height = 4)






















#------------ Figure 6.5: Min-max scaling ------------#
#-----------------------------------------------------#
minmaxScaling <- function(x) {
  # compute min/max values
  minx <- min(x)
  maxx <- max(x)

  # transformation
  xScaled <- (x - minx) / (maxx - minx)

  return(xScaled)
}
# Note: I wrote the function over multiple lines for clarity; you could reduce it to one line!


# Create some data
N <- 42
data <- log(runif(N)) * 234 + 934

# Apply min-max scaling
dataS <- minmaxScaling(data)

# Create random X offsets
randomXoffsets <- 1 + rnorm(N) / 20

# Create a data frame
df <- data.frame(Original=data, Scaled=dataS, RandomXOffsets=randomXoffsets)

# Create the plots using ggplot2
p1 <- ggplot(df, aes(x = RandomXOffsets, y = Original)) +
  geom_point(shape = 22, color = "black", fill = "white") +
  xlim(0, 2) +
  scale_x_continuous(breaks = NULL) +
  ylab("Original data scale") +
  xlab(NULL) +
  ggtitle( bquote(bold("A)") ~ "Original data"))

p2 <- ggplot(df, aes(x = RandomXOffsets, y = Scaled)) +
  geom_point(shape = 22, color = "black", fill = "white") +
  xlim(0, 2) +
  scale_x_continuous(breaks = NULL) +
  ylab("Unity-normed data scale") +
  xlab(NULL) + 
  ggtitle(bquote(bold("B)") ~ "Scaled data"))

p3 <- ggplot(df, aes(x = Original, y = Scaled)) +
  geom_point(shape = 22, color = "black", fill = "white") +
  xlab("Original values") +
  ylab("Scaled values") +
  ggtitle(bquote(bold("C)") ~ "Scatter plot"))


p = grid.arrange(p1, p2, p3, ncol = 3)
ggsave("trans_minmax.png", plot=p, width = 10, height = 4)



























#-------------- Figure 6.6: Another example of min-max scaling ----------#
#------------------------------------------------------------------------#
# Generate a Laplace distribution
x1 <- exp(-abs(3 * rnorm(40)))
x2 <- exp(-abs(3 * rnorm(40)))
x <- (x1 - x2) * 42 - 13

# Apply min-max scaling
xm <- minmaxScaling(x)

# Create a data frame
df <- data.frame(RawData = x, ScaledData = xm)

# Create histograms
p1 <- ggplot(df, aes(x = RawData)) +
  geom_histogram(bins = fd_bins(x), fill = "gray", color = "black", stat = "bin", position = "identity") +
  ylab("Count (a.u.)") +
  xlab("Raw data values") +
  ggtitle("A) Histogram of raw data")

p2 <- ggplot(df, aes(x = ScaledData)) +
  geom_histogram(bins = fd_bins(xm), fill = "gray", color = "black", stat = "bin", position = "identity") +
  ylab("Count (a.u.)") +
  xlab("Minmax-scaled values") +
  ggtitle("B) Histogram of transformed data")

# Arrange and save the plots
p <- grid.arrange(p1, p2, ncol = 1, nrow = 2)
ggsave("trans_minmax_exampleHist.png", plot=p, width = 4, height = 8)





















#------------- Percent Change -----------#
#----------------------------------------#
# Note: This code does not correspond to any figure in the book;
#  I include it here to illustrate the code.
#  Notice in the graph how the 'ref' data value turns into 0.

# A range of values for "new"
new <- seq(from = 3, to = 210, length.out = 31)

# Reference value
ref <- 135

# Compute percent change
pctchg <- 100 * (new - ref) / ref

# Create a data frame for ggplot
data <- data.frame(new = new, pctchg = pctchg)

# Visualize the transformed data
ggplot(data, aes(x = new, y = pctchg)) +
  geom_line(color = "black", size = 1) +
  geom_point(color = "black", fill = "magenta", shape = 22, size = 3) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  geom_vline(xintercept = ref, linetype = "dashed", color = "black") +
  labs(x = "Original data values", y = "Transformed data values", title = "Percent Change") +
  theme_bw() +
  theme(panel.grid.major = element_line(size = 0.5, linetype = 'solid', colour = "grey"), 
        panel.grid.minor = element_blank())




























#----------- Rank transform -------------#
#----------------------------------------#
x <- c(4, 1, 2, 3, 3)

# Rank the elements in x
ranked_x <- rank(x)
ranked_x





















#----------- Figure 6.7: Log transform ----------#
#------------------------------------------------#
# Generate data from power-law distribution
X <- rnorm(2000, mean = 0, sd = 5) ^ 2
Xlog <- log(X)

# Prepare data for ggplot
data_raw <- data.frame(Index = 1:2000, Value = X)
data_log <- data.frame(Index = 1:2000, LogValue = Xlog)

# Scatter plot of raw data
p1 <- ggplot(data_raw, aes(x = Index, y = Value)) +
  geom_point(color = "black", shape = 1, fill = "white") +
  labs(x = "Data index", y = "Data value", title =  bquote(bold("A)") ~ "Raw data"))

# Scatter plot of transformed data
p2 <- ggplot(data_log, aes(x = Index, y = LogValue)) +
  geom_point(color = "black", shape = 1, fill = "white") +
  labs(x = "Data index", y = "ln(data)", title = bquote(bold("B)") ~ "Transformed data"))

# Histogram of raw data
p3 <- ggplot(data_raw, aes(x = Value)) +
  geom_histogram(bins = 30, fill = "grey90", color = "black") +
  labs(title = bquote(bold("C)") ~ "Histogram of raw data"))

# Histogram of transformed data
p4 <- ggplot(data_log, aes(x = LogValue)) +
  geom_histogram(bins = 30, fill = "grey90", color = "black") +
  labs(title =  bquote(bold("D)") ~ "Histogram of ln(data)"))

# Arrange the plots in a 2x2 grid
p <- grid.arrange(p1, p2, p3, p4, nrow = 2, ncol = 2)
ggsave("trans_logdata_example.png", plot=p, width = 10, height = 7)

























#----------- Figure 6.8: Logarithm vs square root ----------#
#-----------------------------------------------------------#
X <- rnorm(10000)^2

# Transformations
X_log <- log(X)
X_sqrt <- sqrt(X)

# Create data frames for ggplot
data_raw <- data.frame(Value = X)
data_log <- data.frame(LogValue = X_log)
data_sqrt <- data.frame(SqrtValue = X_sqrt)

# Theory plot
q <- seq(0.1, 10, length.out = 100)
theory_data <- data.frame(q = q, ln_q = log(q), sqrt_q = sqrt(q))
p1 <- ggplot(theory_data, aes(x = q)) +
  geom_line(aes(y = ln_q, linetype = "ln(x)", color= "ln(x)"), size = 1) +
  geom_line(aes(y = sqrt_q, linetype = "sqrt(x)", color = "sqrt(x)"), size = 1) +
  labs(x = "Raw data value", y = "Transformed data value", title = bquote(bold("A)") ~ "Transformation"), color = NULL, linetype = NULL) +
  scale_linetype_manual(values = c("ln(x)" = "solid", "sqrt(x)" = "dashed")) +
  scale_color_manual(values = c("ln(x)" = "black", "sqrt(x)" = "gray")) + 
  theme_bw() +
  theme(legend.position = c(0.6, 0.4), # Position inside the plot
        legend.justification = c("right", "top"),
        legend.box.just = "right",
        legend.box.background = element_rect(color = "transparent", fill = "transparent"),
        legend.margin = margin(1, 1, 1, 1),
        legend.background = element_blank(),
        legend.box.margin = margin(),
        legend.title = element_blank())


# Histogram of raw data
p2 <- ggplot(data_raw, aes(x = Value)) +
  geom_freqpoly(bins = 100, color = "black", fill = "white") +
  labs(x = "Raw data value", y = "Count", title = bquote(bold("B)") ~ "Data histogram")) +
  theme_bw()

# Histogram of transformed data
p3 <- ggplot() +
  geom_freqpoly(data = data_log, aes(x = LogValue, linetype = "ln(x)", color= "ln(x)"), bins = 100) +
  geom_freqpoly(data = data_sqrt, aes(x = SqrtValue, linetype = "sqrt(x)", color= "sqrt(x)"), bins = 100) +
  labs(x = "Transformed data value", y = "Count", title = bquote(bold("C)") ~ "Trans data hist."), color = NULL, linetype = NULL) +
  scale_linetype_manual(values = c("ln(x)" = "solid", "sqrt(x)" = "dashed")) +
  scale_color_manual(values = c("ln(x)" = "black", "sqrt(x)" = "gray")) + 
  theme_bw() +
  theme(legend.position = c(0.6, 0.8), # Position inside the plot
        legend.justification = c("right", "top"),
        legend.box.just = "right",
        legend.box.background = element_rect(color = "transparent", fill = "transparent"),
        legend.margin = margin(1, 1, 1, 1),
        legend.background = element_blank(),
        legend.box.margin = margin(),
        legend.title = element_blank()) # Remove the legend title


p <- grid.arrange(p1, p2, p3, nrow = 1, ncol = 3, widths = c(1, 1, 1))
ggsave("trans_logsqrt.png", plot=p, width = 10, height = 4)
























#----------- Figure 6.9: Fisher-z transform ----------#
#-----------------------------------------------------#
# Set the number of data points
N <- 1000

# Uniform data in range [-1,1]
Y <- runif(N, min = -1, max = 1)

# Fisher z-transformation
fY <- atanh(Y)

# Data frames for ggplot
data_raw <- data.frame(Value = Y)
data_fisher <- data.frame(FisherValue = fY)
data_combined <- data.frame(Original = Y, Transformed = fY)

# Histogram of raw data
p1 <- ggplot(data_raw, aes(x = Value)) +
  geom_histogram(bins = 30, fill = "grey80", color = "black") +
  labs(title = bquote(bold("A)") ~ "Raw data hist."), x = "Data values", y = "Count")

# Histogram of Fisher-transformed data
p2 <- ggplot(data_fisher, aes(x = FisherValue)) +
  geom_histogram(bins = 30, fill = "grey80", color = "black") +
  labs(title = bquote(bold("B)") ~ "Fisher data hist."), x = "Data values", y = "Count") +
  xlim(-5, 5)

# Scatter plot of transformation
p3 <- ggplot(data_combined, aes(x = Original, y = Transformed)) +
  geom_point(color = "black") +
  labs(title = bquote(bold("C)") ~"Transformation"), x = "Original data", y = "Transformed data")

# Arrange the plots in a 1x3 grid and save to file
p <- grid.arrange(p1, p2, p3, nrow = 1, ncol = 3)
ggsave("trans_fisherz.png", plot=p, width = 10, height = 3)
































#----------- Figure 10: Transform any distribution to Gaussian ----------#
#------------------------------------------------------------------------#
# Set the number of data points
N <- 2000

# Generate data from a normal distribution
X <- rnorm(N, mean = 0, sd = 5)^2

# Rank the data
X_r <- rank(X)

# Scale the ranks between -0.999 and 0.999
a <- -0.999
b <- 0.999
X_r2 <- (X_r - min(X_r)) / (max(X_r) - min(X_r)) * (b - a) + a

# Apply Fisher's z-transformation
X_t <- atanh(X_r2)

# Prepare data for ggplot
data_raw <- data.frame(Value = X)
data_trans <- data.frame(TransformedValue = X_t)
data_comparison <- data.frame(Original = X, Transformed = X_t)

# Histogram of raw data
p1 <- ggplot(data_raw, aes(x = Value)) +
  geom_histogram(bins = 40, fill = "grey80", color = "black") +
  labs(title = bquote(bold("A)") ~ "Raw data hist."), x = "Data value", y = "Count")

# Histogram of transformed data
p2 <- ggplot(data_trans, aes(x = TransformedValue)) +
  geom_histogram(bins = 40, fill = "grey80", color = "black") +
  labs(title = bquote(bold("B)") ~ "Trans. data hist."), x = "Data value", y = "Count")

# Scatter plot of data comparison
p3 <- ggplot(data_comparison, aes(x = Original, y = Transformed)) +
  geom_point(color = "black", shape = 21, fill = "grey80") +
  labs(title = bquote(bold("C)") ~ "Data comparison"), x = "Original data", y = "Transformed data")

# Arrange the plots in a 1x3 grid
p <- grid.arrange(p1, p2, p3, nrow = 1, ncol = 3)
ggsave("trans_any2gauss.png", plot=p, width = 10, height = 4)


























#----------- Figure 6.11: Linearization of nonlinear relationship ----------#
#---------------------------------------------------------------------------#
# Sample size
N <- 200

# The data (note how the nonlinearity is implemented)
x <- seq(0.01, 3, length.out = N)
y <- exp(x) + rnorm(N) * seq(0.1, 1, length.out = N) * 3
y <- abs(y)

# Linear fit on transformed data
fit <- lm(log(y) ~ x)
a <- coef(fit)[2]
b <- coef(fit)[1]

# Prepare data for ggplot
data <- data.frame(x = x, y = y, log_y = log(y), fit = a * x + b)

# Original data plot
p1 <- ggplot(data, aes(x = x, y = y)) +
  geom_point(color = "black", shape = 1, fill = "white") +
  labs(x = "x", y = "y", title = bquote(bold("A)") ~ "Original data"))

# Transformed data plot with linear fit
p2 <- ggplot(data, aes(x = x, y = log_y)) +
  geom_point(color = "black", shape = 1, fill = "white") +
  geom_line(aes(y = fit), color = "black", size = 1) +
  labs(x = "x", y = "ln(y)", title = bquote(bold("B)") ~ "Transformed data"))

# Arrange the plots in a 1x2 grid
p <- grid.arrange(p1, p2, nrow = 1, ncol = 2)
ggsave("trans_linearizedFit.png", plot=p, width = 10, height = 4)




















#-------- Exercise 1 ---------#
#-----------------------------#
# This is what your equation should look like on paper:
# (x-min(x)) / (max(x)-min(x)) * (newmax-newmin) + newmin
my_minmaxScalar <- function(x, newmin, newmax) {
  # Get min and max
  minx <- min(x)
  maxx <- max(x)

  # (Intermediate) Min-max scale to [0,1]
  xS <- (x - minx) / (maxx - minx)

  # Scale to [newmin,newmax]
  xSS <- xS * (newmax - newmin) + newmin

  return(xSS)
}

# Generate some random data
data <- rnorm(10)

# Define the new range
a <- 14.3
b <- 34

# Apply the transformation
y <- my_minmaxScalar(data, a, b)
# Print min and max values of scaled data
print(paste('Min value:', min(y)))
print(paste('Max value:', max(y)))



# Define the range for scaling
new_range <- c(a, b)

#convert to dataframe because caret requires dataframe
data = as.data.frame(data)
# Preprocess the data to scale it
preProcValues <- preProcess(data, method = "range", rangeBounds = new_range)
y2 <- predict(preProcValues, data)

# Print min and max values of scaled data
print(paste('Min value:', min(y2)))
print(paste('Max value:', max(y2)))


# Combine y and y2 side by side
combined_matrix <- cbind(y, y2)
colnames(combined_matrix)[2] = "y2"
# Print out the combined matrix
print(combined_matrix)


# Scatter plot
ggplot(combined_matrix, aes(x = y, y = y2)) +
  geom_point(color = rgb(0.1, 0.2, 0.3), size = 4) +
  labs(x = "My scalar", y = "caret scalar", title = "Scatter Plot") +
  theme_minimal()






























#-------- Exercise 2 ---------#
#-----------------------------#
# Generate data
X <- (rnorm(10000) + 3)^2

# Create histograms
y_r <- hist(X, breaks = 100, plot = FALSE)
y_l <- hist(log(X), breaks = 100, plot = FALSE)
y_s <- hist(sqrt(X), breaks = 100, plot = FALSE)

# Prepare data for ggplot
data_r <- data.frame(mid = y_r$mids, count = y_r$counts)
data_l <- data.frame(mid = y_l$mids, count = y_l$counts)
data_s <- data.frame(mid = y_s$mids, count = y_s$counts)


legend_1 <- "ln(x)"
legend_2 <- "sqrt(x)"
colours <- t(c('black', 'gray'))
colnames(colours) <-c(legend_1, legend_2)
lty <- c(`ln(x)` = "solid", `sqrt(x)` = "dashed")

# Plot 1: Theory
q <- seq(0.1, 10, length.out = 100)
theory_data <- data.frame(q = q, ln_q = log(q), sqrt_q = sqrt(q))
p1 <- ggplot(theory_data, aes(x = q)) +
  geom_line(aes(y = ln_q, linetype = "ln(x)", color = "ln(x)"),  size = 1) +
  geom_line(aes(y = sqrt_q, linetype = "sqrt(x)", color = "sqrt(x)"), size = 1) +
  labs(x = "Raw data value", y = "Transformed data value", title = bquote(bold("A)") ~ "Transformation"), linetype = NULL, color = NULL) +
  theme_bw() +
  scale_linetype_manual(values = c("ln(x)" = "solid", "sqrt(x)" = "dashed")) +
  scale_color_manual(values = colours) + 
  theme(legend.position = c(0.8, 0.4), # Position inside the plot
        legend.justification = c("right", "top"),
        legend.box.just = "right",
        legend.box.background = element_rect(color = "transparent", fill = "transparent"),
        legend.margin = margin(1, 1, 1, 1),
        legend.background = element_blank(),
        legend.box.margin = margin(),
        legend.title = element_blank())

# Plot 2: Untransformed data histogram
p2 <- ggplot(data_r, aes(x = mid, y = count)) +
  geom_line(color = "black", size = 1) +
  labs(x = "Raw data value", y = "Count", title = bquote(bold("B)") ~ "Data histogram")) +
  theme_bw()


# Plot 3: Transformed data histogram
p3 <- ggplot() +
  geom_line(data = data_l, aes(x = mid, y = count, linetype = "ln(x)", color = "ln(x)"),  size = 1) +
  geom_line(data = data_s, aes(x = mid, y = count, linetype = "sqrt(x)", color = "sqrt(x)"), size = 1) +
  labs(x = "Transformed data value", y = "Count", title = bquote(bold("C)") ~ "Trans data hist."), linetype = NULL, color = NULL) +
  theme_bw() +
  scale_linetype_manual(values = lty) + 
  scale_color_manual(values = colours) + 
  theme(legend.position = c(0.6, 0.4), # Position inside the plot
        legend.justification = c("right", "top"),
        legend.box.just = "right",
        legend.box.background = element_rect(color = "transparent", fill = "transparent"),
        legend.margin = margin(1, 1, 1, 1),
        legend.background = element_blank(),
        legend.box.margin = margin(),
        legend.title = element_blank())


# Arrange the plots in a 1x3 grid
p <- grid.arrange(p1, p2, p3, nrow = 1, ncol = 3)



## now for the next part of the exercise
X1 <- (rnorm(1000) + 0)^2
X2 <- (rnorm(1000) + 3)^2

# Sqrt transform
X1s <- sqrt(X1)
X2s <- sqrt(X2)
# Plotting QQ plots
p1 <- ggplot() + 
  stat_qq(aes(sample = X1)) + 
  stat_qq_line(aes(sample = X1), color = "gray", linetype = "solid") +
  ggtitle(expression(bold("A)") ~ "QQ plot of" ~ x^2))

p2 <- ggplot() + 
  stat_qq(aes(sample = X1s)) + 
  stat_qq_line(aes(sample = X1s), color = "gray", linetype = "solid") +
  ggtitle(expression(bold("B)") ~ "QQ plot of" ~ sqrt(x^2)))

p3 <- ggplot() + 
  stat_qq(aes(sample = X2)) + 
  stat_qq_line(aes(sample = X2), color = "gray", linetype = "solid") +
  ggtitle(expression(bold("C)") ~ "QQ plot of" ~ (x + 3)^2))

p4 <- ggplot() + 
  stat_qq(aes(sample = X2s)) + 
  stat_qq_line(aes(sample = X2s), color = "gray", linetype = "solid") +
  ggtitle(expression(bold("D)") ~ "QQ plot of" ~ sqrt((x + 3)^2)))  # Title with mathematical expression

# Arrange the plots in a 2x2 grid
p <- grid.arrange(p1, p2, p3, p4, nrow = 2, ncol = 2)
ggsave("trans_ex2.png", plot=p, width = 10, height = 8)



### repeat for log transform
X1s <- log(X1)
X2s <- log(X2)

# Plotting QQ plots
p1 <- ggplot() + 
  stat_qq(aes(sample = X1)) + 
  stat_qq_line(aes(sample = X1), color = "gray", linetype = "solid") +
  ggtitle(expression(bold("A)") ~ "QQ plot of" ~ x^2))

p2 <- ggplot() + 
  stat_qq(aes(sample = X1s)) + 
  stat_qq_line(aes(sample = X1s), color = "gray", linetype = "solid") +
  ggtitle(expression(bold("B)") ~ "QQ plot of" ~ ln(x^2)))

p3 <- ggplot() + 
  stat_qq(aes(sample = X2)) + 
  stat_qq_line(aes(sample = X2), color = "gray", linetype = "solid") +
  ggtitle(expression(bold("C)") ~ "QQ plot of" ~ (x + 3)^2))

p4 <- ggplot() + 
  stat_qq(aes(sample = X2s)) + 
  stat_qq_line(aes(sample = X2s), color = "gray", linetype = "solid") +
  ggtitle(expression(bold("D)") ~ " QQ plot of" ~ ln(x + 3)^2))

# Arrange the plots in a 2x2 grid
p <- grid.arrange(p1, p2, p3, p4, nrow = 2, ncol = 2)



## final part of the exercise: shift the data to be mean-centered
X <- (rnorm(10000) + 3)^2

# Apply sqrt transform
Y <- sqrt(X)

# Shift the data to be mean-centered
Y_shifted <- Y - mean(Y)

# Create histograms
hist_Y <- hist(Y, breaks = 100, plot = FALSE)
hist_Y_shifted <- hist(Y_shifted, breaks = 100, plot = FALSE)

# Prepare data for ggplot
data_Y <- data.frame(mid = (hist_Y$breaks[-length(hist_Y$breaks)] + hist_Y$breaks[-1])/2, 
                     count = hist_Y$counts)
data_Y_shifted <- data.frame(mid = (hist_Y_shifted$breaks[-length(hist_Y_shifted$breaks)] + hist_Y_shifted$breaks[-1])/2, 
                             count = hist_Y_shifted$counts)

# Plot
ggplot() +
  geom_line(data = data_Y, aes(x = mid, y = count, color = "sqrt(x)"), size = 1) +
  geom_line(data = data_Y_shifted, aes(x = mid, y = count, color = "shifted sqrt(x)"), size = 1) +
  labs(x = "Transformed data value", y = "Count", title = "Histogram of Transformed Data", color = NULL) +
  scale_color_manual(values = c("sqrt(x)" = "blue", "shifted sqrt(x)" = "red")) +
  theme_minimal()



























#-------- Exercise 3 ---------#
#-----------------------------#
# After a bit of algebra, you should arive at the equation:
#  zs+m = x
# where: z is the z-transformed data
#        s is the standard deviation of the original data
#        m is the mean (mu) of the original data
#        x is the reconstructed data
#
# The implication is Yes, the z-score transformation is invertible,
#    but only if you store the mean and std of the pre-transformed data.


### Now for the empirical illustration:
data <- sample(4:14, 25, replace = TRUE)

# Pre-transformed descriptive statistics
orig_m <- mean(data)
orig_s <- sd(data)

# Transform to z-score
dataz <- (data - orig_m) / orig_s

# Back-transform
data_inv <- dataz * orig_s + orig_m

# Create a data frame for plotting
plot_data <- data.frame(Index = 1:length(data), Original = data, BackTransformed = data_inv)

# Plot using ggplot2
ggplot(plot_data) +
  geom_point(aes(x = Index, y = Original,  shape = "Original"), size = 5, fill = "white") +
  geom_point(aes(x = Index, y = BackTransformed, shape = "Back-transformed"),size = 5) +
  labs(x = "Data index", y = "Data value", title = NULL, shape = NULL) +
  scale_shape_manual(values = c("Original" = 0, "Back-transformed" = 4)) +
  theme_minimal() +
  theme(legend.title = element_blank(), legend.position = "bottom")



























#-------- Exercise 4 ---------#
#-----------------------------#
# Parameters
N <- 313
lo_bnd <- 3 * pi
hi_bnd <- exp(pi)

# Create data
data <- runif(N, lo_bnd, hi_bnd)

# Step 1: Scale to [-1,1] (but not exactly -1 or +1)
dataScale <- my_minmaxScalar(data, -0.999, 0.999)

# Step 2: Fisher-z transform
dataFish <- atanh(dataScale)

# Step 3: Shift mean
dataFinal <- dataFish - mean(dataFish) + (lo_bnd + hi_bnd) / 2

# Report the expected and empirical means
expected_mean <- (lo_bnd + hi_bnd) / 2
empirical_mean <- mean(dataFinal)

print(paste('Expected mean:', round(expected_mean, 3)))
print(paste('Empirical mean:', round(empirical_mean, 3)))



# Create histograms
hist_data <- hist(data, breaks = "Freedman-Diaconis", plot = FALSE)  # Original data
hist_dataFinal <- hist(dataFinal, breaks = "Freedman-Diaconis", plot = FALSE)  # Transformed data

# Prepare data for ggplot
data_hist <- data.frame(
  mid = (hist_data$breaks[-length(hist_data$breaks)] + hist_data$breaks[-1])/2, 
  count = hist_data$counts,
  type = "Original data"
)
dataFinal_hist <- data.frame(
  mid = (hist_dataFinal$breaks[-length(hist_dataFinal$breaks)] + hist_dataFinal$breaks[-1])/2, 
  count = hist_dataFinal$counts,
  type = "Transformed data"
)

# Combine the data
combined_hist <- rbind(data_hist, dataFinal_hist)

# Plot using ggplot2
ggplot(combined_hist, aes(x = mid, y = count, group = type, color = type)) +
  geom_line(size = 1) +
  geom_point(size = 3) +
  geom_vline(aes(linetype = "Average", xintercept = mean(data)), color = "grey80", size = 1) +
  labs(x = "Data value", y = "Count", title = "Histograms of Original and Transformed Data") +
  scale_color_manual(values = c("Original data" = "black", "Transformed data" = "grey70")) +
  scale_linetype_manual(values = c("Average" = "dotted")) + 
  theme_minimal() +
  theme(legend.title = element_blank())

# Note: To save the plot, uncomment the line below
ggsave("trans_ex4.png", width = 8, height = 4)






























#-------- Exercise 5 ---------#
#-----------------------------#
# Create a simple vector
data <- 3:9

# Compute z-score manually
mean_data <- mean(data)
sd_data <- sd(data)
data_z_manual <- (data - mean_data) / sd_data

# Compute z-score using built-in function (scale)
data_z_scale <- scale(data)  # scale() computes the z-score

# Print the results to confirm they match
print(data_z_manual)
print(data_z_scale)



### z-scoring columns vs a whole matrix
# Create a matrix
data <- matrix((0:11)^2, nrow = 4, ncol = 3, byrow=TRUE)

# Compute column-wise z-score
data_z_col <- scale(data)  # By default, scale() operates column-wise

# Compute matrix-wise z-score
data_vector <- as.vector(data)
mean_data   <- mean(data_vector)
sd_data     <- sd(data_vector)
data_z_mat  <- (data_vector - mean_data) / sd_data
data_z_mat  <- matrix(data_z_mat, nrow = 4, ncol = 3)

# Print the results
print("Original data matrix:")
print(data)

print(" ")
print("Column-wise z-scoring:")
print(data_z_col)

print(" ")
print("Matrix-wise z-scoring:")
print(data_z_mat)
























#-------- Exercise 6 ---------#
#-----------------------------#

# The insight here is that the Fisher-z transform is nonlinear,
# and has a bigger impact on data that are closer to |1|.
# This means you can create skew from a uniform distribution
# with asymmetric boundaries.

# Generate data
X1 <- atanh(runif(5000, min = -0.999, max = 0.999))
X2 <- atanh(runif(5000, min = -0.2, max = 0.999))
X3 <- atanh(runif(5000, min = -0.999, max = 0.8))

# Compute the empirical skews
skews <- c(skewness(X1), skewness(X2), skewness(X3))

# Create data frames for plotting
data_X1 <- data.frame(Value = X1)
data_X2 <- data.frame(Value = X2)
data_X3 <- data.frame(Value = X3)

# Plotting
p1 <- ggplot(data_X1, aes(x = Value)) +
  geom_histogram(bins = 40, fill = "grey80", color = "black") +
  ggtitle(bquote(bold("A)") ~ "skew =" ~ .(round(skews[1], 2))))

p2 <- ggplot(data_X2, aes(x = Value)) +
  geom_histogram(bins = 40, fill = "grey80", color = "black") +
  ggtitle(bquote(bold("B)") ~ "skew =" ~ .(round(skews[2], 2))))

p3 <- ggplot(data_X3, aes(x = Value)) +
  geom_histogram(bins = 40, fill = "grey80", color = "black") +
  ggtitle(bquote(bold("C)") ~ "skew =" ~ .(round(skews[3], 2))))

# Arrange the plots in a 1x3 grid
p <- grid.arrange(p1, p2, p3, nrow = 1, ncol = 3, widths = c(1, 1, 1))
ggsave("trans_ex6.png", plot=p, width = 10, height = 3.5)


























#-------- Exercise 7 ---------#
#-----------------------------#
sigmas <- seq(0.1, 1, length.out = 20)
X <- rnorm(13524)

# Initialize the data results matrices
M <- matrix(0, nrow = length(sigmas), ncol = 2)
data <- vector("list", length(sigmas))

# Compute and store all moments in a matrix
for (i in seq_along(sigmas)) {
  # Create the data
  data[[i]] <- exp(X * sigmas[i])
  
  # Compute its standard deviation
  M[i, 1] <- sd(data[[i]])
  
  # Compute its MAD
  M[i, 2] <- mad(data[[i]], constant = 1)  # constant = 1 for consistency with Python's definition of MAD
}




# Assuming data and M are already defined
# Create a data frame for M for easier ggplot2 use
M_df <- data.frame(sigma = sigmas, std = M[, 1], MAD = M[, 2], Scaled_MAD = M[, 2] / qnorm(3/4))

# Base plot
p1 <- ggplot(M_df, aes(x = sigma)) +
  geom_line(aes(y = std, colour = "std"), size = 1) +
  geom_point(aes(y = std, shape = "std"), size = 3, fill = "white") +
  geom_line(aes(y = MAD, colour = "MAD"), size = 1) +
  geom_point(aes(y = MAD, shape = "MAD"), size = 3, fill = "grey80") +
  geom_line(aes(y = Scaled_MAD, colour = "Scaled MAD"), size = 1) +
  geom_point(aes(y = Scaled_MAD, shape = "Scaled MAD"), size = 3, fill = "grey80") +
  scale_colour_manual(values = c("std" = "black", "MAD" = "black", "Scaled MAD" = "black")) +
  scale_shape_manual(values = c("std" = 0, "MAD" = 1, "Scaled MAD" = 4)) +
  labs(x = expression(sigma), y = "Dispersion value", colour = "Measure", shape = "Measure") +
  ggtitle(expression(bold("B)")  ~ "Standard deviation and MAD")) +
  theme_minimal() +
  theme(legend.position = "bottom", legend.title = NULL)

# Add arrows
arrow_data <- data.frame(
  x = c(0.1, 0.29, 0.53, 0.76, 1.00),
  y = rep(1.2, 5),
  xend = c(0.1, 0.29, 0.53, 0.76, 1.00),
  yend = rep(0, 5),
  color = c("blue", "orange", "green", "red", "purple")
)

for (i in 1:nrow(arrow_data)) {
  p1 <- p1 +
    geom_segment(data = arrow_data[i, ], aes(x = x, y = y, xend = xend, yend = yend),
                 arrow = arrow(type = "open", ends = "last", length = unit(0.3, "inches")),
                 size = 1, color = arrow_data[i, "color"]) +
    annotate("text", x = arrow_data[i, "x"], y = 1.2, label = sprintf("%.2f", arrow_data[i, "x"]),
             hjust = 0.5, vjust = 0, size = 3.5, color = arrow_data[i, "color"])
}


# Convert the list of distributions into a data frame
# Assuming that each element in 'data' is a numeric vector of data points
dist_df <- bind_rows(lapply(seq_along(data), function(i) {
  # Create a histogram for the current distribution
  hist_data <- hist(data[[i]], breaks = "FD", plot = FALSE)
  
  # Create a data frame for the current distribution
  data.frame(
    mid = hist_data$mids,
    count = hist_data$counts,
    group = as.factor(paste0("σ = ", formatC(sigmas[i], format = "f", digits = 2)))
  )
}), .id = "id")

# Assuming dist_df is already defined and contains 'mid', 'count', and 'group' columns
# Assuming you only want to display specific sigma values in the legend

# Define the specific sigma values you want to display in the legend
desired_sigmas <- c(0.10, 0.29, 0.53, 0.76, 1.00)
desired_sigmas_labels <- paste("σ =", sprintf("%.2f", desired_sigmas))

# Filter dist_df to only include the desired sigmas
# This assumes that dist_df$group is a factor whose levels are formatted like "σ = 0.10"
dist_df <- dist_df %>% filter(as.character(group) %in% desired_sigmas_labels)

# Define the colors you want to use for each group level
# The colors need to be in the same order as the desired sigma labels
colors <- c("blue", "orange", "green", "red", "purple")
names(colors) <- desired_sigmas_labels

# Create the plot
p2<- ggplot(dist_df, aes(x = mid, y = count, group = group, colour = group)) +
  geom_line() +
  geom_point() + 
  scale_colour_manual(values = colors, labels = desired_sigmas_labels) +
  labs(x = "Data value", y = "Count") +
  ggtitle(bquote(bold("A)") ~ "Distributions")) +
  theme_minimal() +
  theme(legend.position = "right", legend.title = NULL) +
  scale_x_continuous(limits = c(0, 6), breaks = 0:6)


# Arrange the plots side by side with a shared legend for the first plot
p <- grid.arrange(p1, p2, ncol = 2)
ggsave("trans_ex7.png", plot=p, width = 10, height = 5)


























#-------- Exercise 8 ---------#
#-----------------------------#
# Generate the samples
N <- 300
sample1 <- rnorm(N)^2
sample2 <- rnorm(N)^2

# Compute their difference
difference <- sample1 - sample2

# Create histogram data for sample1 and sample2
hist_sample1 <- hist(sample1, breaks = "FD", plot = FALSE)
hist_sample2 <- hist(sample2, breaks = "FD", plot = FALSE)

# Create data frames for the histograms with a group indicator
df_sample1 <- data.frame(mid = (hist_sample1$breaks[-length(hist_sample1$breaks)] + hist_sample1$breaks[-1])/2, 
                         count = hist_sample1$counts, group = 'Sample 1')
df_sample2 <- data.frame(mid = (hist_sample2$breaks[-length(hist_sample2$breaks)] + hist_sample2$breaks[-1])/2, 
                         count = hist_sample2$counts, group = 'Sample 2')

# Combine the data frames
df_samples <- rbind(df_sample1, df_sample2)

# Plot the lines for sample1 and sample2
p1 <- ggplot(df_samples, aes(x = mid, y = count, group = group, linetype = group, color = group)) +
  geom_line(size = 1) +
  scale_linetype_manual(values = c('Sample 1' = 'solid', 'Sample 2' = 'dashed')) +
  scale_color_manual(values = c('Sample 1' = 'black', 'Sample 2' = 'gray')) +
  labs(x = 'Data value', 
       y = 'Count', 
       title = bquote(bold('A)') ~'Individual Histograms'),
       color = NULL,  # Removing the legend title for color
       linetype = NULL) +  # Removing the legend title for linetype
  theme_minimal() +
  theme(legend.position = "right")  # Position the legend to the right

### Print the plot

# Create a data frame for the difference histogram
df_difference <- data.frame(value = difference)

# Plot the difference histogram
p2 <- ggplot(df_difference, aes(x = value)) +
  geom_freqpoly(bins = 30, color = 'black') +
  labs(x = 'Difference value', y = 'Count', title = bquote(bold('B)') ~ 'Difference Histogram')) +
  theme_minimal()


p <- grid.arrange(p_samples, p_difference, ncol = 2)
ggsave('trans_ex8.png', plot=p, width=10, height=4)















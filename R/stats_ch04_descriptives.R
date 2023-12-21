#----
# Modern statistics: Intuition, Math, Python, R
## Mike X Cohen (sincxpress.com)
#### https://www.amazon.com/dp/B0CQRGWGLY
#### Code for chapter 4 (Descriptive statistics)

# About this code file:
### This code file will reproduce most of the figures in this chapter 
### (some figures were made in Inkscape), and illustrate the statistical 
### concepts explained in the text. The point of providing the code is not 
### just for you to recreate the figures, but for you to modify, adapt, 
### explore, and experiment with the code.
###
### Solutions to all exercises are at the bottom of the file.
# Thanks to Enes Ahmeti for help with the translation from Python.



#----
# import packages and define global settings
library(tidyverse)
library(gridExtra)
library(ggplot2)
library(e1071) # for distributions
library(moments)  # For skewness and kurtosis

# define global figure properties
custom_theme <- theme_classic() + 
  theme(text=element_text(size=20),        # font size 
        plot.title=element_text(hjust=0))  # title location
theme_set(custom_theme)
figdpi <- 300                             # output resolution

saveRplot <- function(plot_name) {
  # helps with plots.
  png(filename = plot_name,
      width = 1200,
      height = 800)
  dev.set(which = 2)
  dev.copy(which = 4)
  dev.off()
}




#-------- Figure 4.2: Gangnam style video watching data ---------#
#----------------------------------------------------------------#
# Generate the data
timesWatched <- round(abs(rnorm(500, mean=0, sd=1) * 20), 2)
timesWatched[300] <- 70

# Create a data frame for plotting
data <- data.frame(Index=1:length(timesWatched), TimesWatched=timesWatched)

# Plot 1: Times Watched by Respondent Index
p1 <- ggplot(data, aes(x=Index, y=TimesWatched)) +
  geom_point(shape=15) +
  labs(x="Times Watched", y="Count",
       title=bquote(bold('A)') ~ " Visualized by respondent ID#") )

# Plot 2: Histogram of Times Watched
p2 <- ggplot(data, aes(x=TimesWatched)) +
  geom_histogram(bins=30, fill="gray50", color="black") +
  labs(x="Times Watched", y="Count",
    title=bquote(bold('B)') ~ " Visualzed as histogram"))

# Arrange the plots side by side
p <- grid.arrange(p1, p2, ncol = 2)

ggsave('desc_YT_visualize.png', plot=p, width=12, height=4, dpi=figdpi)








#-------- Figure 4.3: Two samples from the same population have similar distributions ---------#
#----------------------------------------------------------------------------------------------#
sampleA <- rnorm(1500, mean=0, sd=1)*2 + pi^pi
sampleB <- rnorm(1500, mean=0, sd=1)*2 + pi^pi

# Create data frames for ggplot
dfA <- data.frame(Value = sampleA)
dfB <- data.frame(Value = sampleB)

# Histogram for Sample A
pA <- ggplot(dfA, aes(x = Value)) +
  geom_histogram(bins = 30, fill = "#000000", color = "white") + # Adjust bins as needed
  xlim(30, 45) +
  labs(x="Data value", y="Count",
       title=bquote(bold('A)') ~ " Sample 'A'"))

# Histogram for Sample B
pB <- ggplot(dfB, aes(x = Value)) +
  geom_histogram(bins = 30, fill = "#000000", color = "white") + # Adjust bins as needed
  xlim(30, 45) +
  labs(x="Data value", y="Count",
       title=bquote(bold('B)') ~ " Sample 'B'"))

# Arrange the plots side by side
p <- grid.arrange(pA, pB, ncol = 2)
ggsave('desc_rand_diffHists.png', plot=p, width=12, height=4, dpi=figdpi)







#-------- Figure 4.4: Analytical Gaussian ---------#
#--------------------------------------------------#
# Code isn't shown here, because it's part of Exercise 1 :P



#-------- Figure 4.5: Examples of distributions ---------#
#--------------------------------------------------------#
# Create data frames for the first two plots
x <- seq(-5, 5, length.out = 10001)
df_gaussian <- data.frame(x = x, y = dnorm(x, mean = 0, sd = 1))
df_t <- data.frame(x = x, y = dt(x, df = 20))

# Gaussian Distribution
pA <- ggplot(df_gaussian, aes(x = x, y = y)) +
  geom_line(size = 1.2) +
  labs(x = "", y = "",
       title = bquote(bold('A)') ~ 'Gaussian ("bell curve")')) +
  theme_minimal()

# T Distribution
pB <- ggplot(df_t, aes(x = x, y = y)) +
  geom_line(size = 1.2) +
  labs(x = "", y = "",
       title = bquote(bold('B)') ~ 'T distribution (df=20)')) +
  theme_minimal()

# data for the second two plots
x <- seq(0, 10, length.out=10001)
df_f <- data.frame(x=x, y=df(x, df1=5, df2=100))
df_chi_square <- data.frame(x=x, y=dchisq(x, df=3))

# F Distribution
pC <- ggplot(df_f, aes(x = x, y = y)) +
  geom_line(size = 1.2) +
  labs(x = "", y = "",
       title = bquote(bold('C)') ~ 'F distribution (df=5, 100)')) +
  theme_minimal()

# Chi-Square Distribution
pD <- ggplot(df_chi_square, aes(x=x, y=y)) +
  geom_line(size = 1.2) +
  labs(x = "", y = "",
       title = bquote(bold('D)') ~ 'Chi-square distribution (df=3)')) +
  theme_minimal()

# Arrange the plots in a 2x2 grid
p <- grid.arrange(pA, pB, pC, pD, ncol = 2, nrow = 2)

ggsave('desc_exampleDistributions.png', plot=p, width=12, height=9, dpi=figdpi)











#-------- Figure 4.6: Examples of empirical distributions ---------#
#------------------------------------------------------------------#
# Bimodal Distribution Data
X_bimodal <- atanh(runif(10000) * 1.8 - 0.9) + 1.5
df_bimodal <- data.frame(Value = X_bimodal)

# Uniform Distribution Data
X_uniform <- runif(1000)
df_uniform <- data.frame(Value = X_uniform)

# Power Distribution Data
f <- seq(1, 10, length.out = 5001)
X_power <- 1 / f + rnorm(length(f)) / 200
X_power <- ifelse(X_power > 0.9, 0.9, X_power) # some clipping
df_power <- data.frame(Value = X_power)

# Binomial Distribution Data
x1 <- rnorm(500) - 2
x2 <- rnorm(2500) + 2
X_binomial <- c(x1, x2)
df_binomial <- data.frame(Value = X_binomial)

# Bimodal Histogram
pA <- ggplot(df_bimodal, aes(x = Value)) +
  geom_histogram(bins = 30, fill='gray50', color='black') +
  labs(x = "Data Value", y = "Bin count",
       title = bquote(bold('A)'))) 

# Uniform Histogram
pB <- ggplot(df_uniform, aes(x = Value)) +
  geom_histogram(bins = 30, fill='gray50', color='black') +
  labs(x = "Data Value", y = "Bin count",
       title = bquote(bold('B)')))

# Power Histogram
pC <- ggplot(df_power, aes(x = Value)) +
  geom_histogram(bins = 30, fill='gray50', color='black') +
  labs(x = "Data Value", y = "Bin count",
       title = bquote(bold('C)')))

# Binomial Histogram
pD <- ggplot(df_binomial, aes(x = Value)) +
  geom_histogram(bins = 30, fill='gray50', color='black') +
  labs(x = "Data Value", y = "Bin count",
       title = bquote(bold('D)')))

# Arrange the plots in a 2x2 grid
p <- grid.arrange(pA, pB, pC, pD, ncol = 2, nrow = 2)
ggsave("desc_exampleEmpHists.png", plot=p, width=10, height=7, dpi=figdpi)













#-------- Figure 4.7: Characteristics of distributions ---------#
#---------------------------------------------------------------#
# Define the x sequence and Gaussian distributions
x <- seq(-4, 4, length.out = 101)
gaus1 <- exp(-x^2 / 2)
gaus2 <- exp(-x^2 / (2*.3^2))
gaus3 <- exp(-(x-(-1))^2 / 2)

# Create a data frame
data <- data.frame(x, gaus1, gaus2, gaus3)

# Convert data from wide to long format for ggplot
data_long <- gather(data, key = "Distribution", value = "Value", -x)

# Replace the Distribution names with desired legend titles
data_long$Distribution <- factor(data_long$Distribution, 
                                 levels = c("gaus1", "gaus2", "gaus3"),
                                 labels = c("Distr. 1", "Distr. 2", "Distr. 3"))

# Plot
p <- ggplot(data_long, aes(x=x, y=Value, linetype=Distribution)) +
  geom_line() +
  scale_linetype_manual(values = c(1, 2, 3)) +
  labs(x = "", y = "", title = "")

# Display the plot
print(p)
ggsave("desc_distr_chars.png", plot=p, width=8, height=4, dpi=figdpi)













#-------- Figure 4.9: Histogram showing mean ---------#
#-----------------------------------------------------#
# Generate a Laplace distribution
x1 <- exp(-abs(3 * rnorm(4000)))
x2 <- exp(-abs(3 * rnorm(4000)))
x <- x1 - x2 + 1

# Compute mean
xBar <- mean(x)

# Create a data frame for ggplot
df <- data.frame(Value = x)

# Plot
p <- ggplot(df, aes(x = Value)) +
  geom_histogram(bins = 30, fill = "grey", color = "black") + # Adjust number of bins as needed
  geom_vline(aes(xintercept = xBar, linetype = "Mean"), color = "black", size = 1.5) +
  scale_linetype_manual(values = c(1, 3)) +
  labs(x = "Value", y = "Count", title = "") + 
  theme(
    legend.position = c(8,.25),
    legend.background = element_rect(fill = "white", color = "black")
  )

# Display the plot
print(p)
ggsave("desc_distrWithMean.png", plot=p, width=6, height=6, dpi=figdpi)













#-------- Figure 4.10: "Failure" scenarios for the mean ---------#
#----------------------------------------------------------------#
# Case 1: mean does not reflect the most common value
case1 <- (rnorm(400) + 2.5)^3 - 50

# Case 2: bimodal distribution
x1 <- rnorm(500) - 3
x2 <- rnorm(500) + 3
case2 <- c(x1, x2)

# Create data frames
df_case1 <- data.frame(Value = case1)
df_case2 <- data.frame(Value = case2)

# Histogram for Case 1
p1 <- ggplot(df_case1, aes(x = Value)) +
  geom_histogram(bins = 30, fill = "#808080", color = "black") +
  geom_vline(aes(xintercept = mean(Value), linetype = "Mean"), color = "black", size = 1.5) +
  labs(x = "Value", y = "Count",
       title = bquote(bold('A)') ~ "Non-symmetric distribution")) +
  theme_minimal()

# Histogram for Case 2
p2 <- ggplot(df_case2, aes(x = Value)) +
  geom_histogram(bins = 30, fill = "#808080", color = "black") +
  geom_vline(aes(xintercept = mean(Value), linetype = "Mean"), color = "black", size = 1.5) +
  labs(x = "Value", y = "Count",
       title = bquote(bold('B)') ~ "Bimodal distribution")) +
  theme_minimal()

# Arrange the plots side by side
p <- grid.arrange(p1, p2, ncol = 2)
ggsave("desc_meanFailures.png", plot=p, width=10, height=3, dpi=figdpi)














#-------- Figure 4.11: Median and mean ---------#
#-----------------------------------------------#
# Generate a Laplace distribution
x1 <- exp(-abs(3 * rnorm(4000)))
x2 <- exp(-abs(3 * rnorm(4000)))
x <- x1 - x2 + 1

# Calculate mean and median
xMean <- mean(x)
xMedian <- median(x)

# Create a data frame for ggplot
df <- data.frame(Value = x)

# Plot
p <- ggplot(df, aes(x = Value)) +
  geom_histogram(bins = 30, fill = "grey", color = "black") + # Adjust number of bins as needed
  geom_vline(aes(xintercept = xMean, linetype = "Mean"), color = "black", size = 1.5) +
  geom_vline(aes(xintercept = xMedian, linetype = "Median"), color = "gray", size = 1.5) +
  labs(x = "Value", y = "Count", title = "") +
  scale_linetype_manual(values = c(3, 2)) +
  guides(linetype = guide_legend(title = NULL, override.aes = list(color = c("black", "gray"), size = c(1.5, 1.5))))

# Display the plot
print(p)
ggsave("desc_distrWithMeanAndMedian.png", plot=p, width=5, height=5, dpi=figdpi)













#-------- Figure 4.12: "Failures" of the median ---------#
#--------------------------------------------------------#
# Case 1: mean does not reflect
case1 <- (rnorm(400) + 2.5)^3 - 50

# Case 2: bimodal distribution
x1 <- rnorm(500) - 3
x2 <- rnorm(500) + 3
case2 <- c(x1, x2)

# Function to create histogram with mean and median lines
create_hist <- function(dt) {
  df <- data.frame(Value = dt)
  p <- ggplot(df, aes(x = Value)) +
    geom_histogram(bins = 30, fill = "#808080", color = "black") +
    geom_vline(aes(xintercept = mean(Value), linetype = "Mean"), color = "black", size = 1.5) +
    geom_vline(aes(xintercept = median(Value), linetype = "Median"), color = "gray", size = 1.5) +
    labs(x = "Value", y = "Count", title = "") +
    scale_linetype_manual(values = c(3, 2)) +
    guides(linetype = guide_legend(title = NULL, override.aes = list(color = c("black", "gray"), size = c(1.5, 1.5))))
  return(p)
}

# Create plots
p1 <- create_hist(case1)
p2 <- create_hist(case2)

# Arrange the plots side by side
p <- grid.arrange(p1, p2, ncol = 2)
ggsave("desc_medianFailures.png", plot=p, width=10, height=3, dpi=figdpi)













#-------- Figure 4.13: Mode ---------#
#------------------------------------#
# Create data frame from the provided data
data <- data.frame(
  Day = c('Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday', 'Saturday', 'Sunday'),
  Count = c(5, 7, 14, 4, 3, 11, 14)
)

# Calculate the mode
dataMode <- as.numeric(names(sort(-table(data$Count)))[1])

# Create the bar plot
p <- ggplot(data, aes(x = Day, y = Count)) +
  geom_bar(stat = "identity", fill = "#000000") +
  geom_hline(aes(yintercept = dataMode), color = "black", size = 1.5, linetype = "dashed") +
  labs(x = "", y = "Count", title = "Preferred washing days")

# Display the plot
print(p)
ggsave("desc_washingMode.png", plot=p, dpi=figdpi)














#-------- Figure 4.14: Dispersion ---------#
#------------------------------------------#
# Generate data
X1 <- rnorm(1000) * 2 + 70
X2 <- rnorm(1000) * 8 + 70

# Create data frames
df1 <- data.frame(Index = 1:length(X1), Salary = X1)
df2 <- data.frame(Index = 1:length(X2), Salary = X2)
df_box <- data.frame(Group = rep(c("Restaurants", "Random"), each = 1000), Salary = c(X1, X2))

# Plot A: Scatter Plot for Restaurant Managers Salaries
pA <- ggplot(df1, aes(x = Index, y = Salary)) +
  geom_point() +
  labs(x = "Data sample index", y = "Salary (thousands)",
       title = bquote(bold('A)') ~ "Restaurant managers salaries")) +
  ylim(40, 100)

# Plot B: Scatter Plot for Random Salaries
pB <- ggplot(df2, aes(x = Index, y = Salary)) +
  geom_point() +
  labs(x = "Data sample index", y = "Salary (thousands)",
       title = bquote(bold('B)') ~ "Random salaries")) +
  ylim(40, 100)

# Plot C: Box Plots
pC <- ggplot(df_box, aes(x = Group, y = Salary)) +
  geom_boxplot() +
  labs(title = bquote(bold('C)') ~ "Box Plots"))

# Plot D: Histograms as Line Plots
breaks <- seq(min(X2), max(X2), length.out = 41)
y1 <- hist(X1, breaks = breaks, plot = FALSE)$counts
y2 <- hist(X2, breaks = breaks, plot = FALSE)$counts
x <- (breaks[-length(breaks)] + breaks[-1]) / 2
df_hist <- data.frame(Salary = x, Count1 = y1, Count2 = y2)

pD <- ggplot() +
  geom_line(data = df_hist, aes(x = Salary, y = Count1, linetype = "Restaurants")) +
  geom_line(data = df_hist, aes(x = Salary, y = Count2, linetype = "Random")) +
  labs(x = "Salary (thousands)", y = "Counts",
       title = bquote(bold('D)') ~ "Histogram")) +
  scale_linetype_manual(values = c(1, 2))

# Arrange the plots
p <- grid.arrange(pA, pB, pC, pD, ncol = 2, nrow = 2)
ggsave("desc_dispersion.png", plot=p, width=15, height=9, dpi=figdpi)















#-------- Figure 4.16: Homo/heteroscedasticity ---------#
#-------------------------------------------------------#
# Generate data
N <- 2345
x <- seq(1, 10, length.out = N)
ho <- rnorm(N)
he <- rnorm(N) * x

# Create data frames for plotting
df_ho <- data.frame(Index = x, Value = ho)
df_he <- data.frame(Index = x, Value = he)

# Plot A: Homoscedasticity
pA <- ggplot(df_ho, aes(x = Index, y = Value)) +
  geom_point() +
  labs(x = "Data index", y = "Data value",
       title = bquote(bold('A)') ~ "Homoscedasticity")) +
  theme(axis.text.x = element_blank(), axis.text.y = element_blank())

# Plot B: Heteroscedasticity
pB <- ggplot(df_he, aes(x = Index, y = Value)) +
  geom_point(shape = 0) +
  labs(x = "Data index", y = "Data value",
       title = bquote(bold('B)') ~ "Heteroscedasticity")) +
  theme(axis.text.x = element_blank(), axis.text.y = element_blank())

# Arrange the plots side by side
p <- grid.arrange(pA, pB, ncol = 2)
ggsave("desc_homohetero.png", plot=p, width=10, height=4, dpi=figdpi)














#-------- Figure 4.17: FWHM ---------#
#------------------------------------#
# Data preparation
x <- seq(-8, 8, length.out = 1001)
s <- 1.9
pureGaus <- exp((-x ^ 2) / (2 * s ^ 2))
fwhm <- 2 * s * sqrt(2 * log(2))
data <- data.frame(x, pureGaus)

# Create the plot
p <- ggplot(data, aes(x = x, y = pureGaus)) +
  geom_line(size = 1.2) +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  theme(axis.text.y = element_blank(), axis.ticks.y = element_blank()) +
  labs(x = "", y = "Gain", title = "FWHM = 4.47") +
  geom_vline(xintercept = c(-fwhm / 2, fwhm / 2), linetype = "dashed", size = 1) +
  geom_hline(yintercept = 0.5, linetype = "dashed", size = 1) +
  geom_segment(aes(x = -fwhm / 2, y = 0.5, xend = fwhm / 2, yend = 0.5), 
               arrow = arrow(type = "closed", length = unit(0.2, "inches")), size = 1.2) +
  annotate("text", x = 0, y = 0.53, label = "FWHM", size = 5)

# Display the plot
print(p)
ggsave("desc_FWHM_def.png", plot=p, width=10, height=6, dpi=figdpi)















#-------- Figure 4.18: Fano factor ---------#
#-------------------------------------------#
# Parameters
fanos <- c(0.1, 1, 10)
meanConstant <- 10

# Function to generate Fano data
generateFanoData <- function(i) {
  Sigma <- sqrt(i * meanConstant)
  x <- rnorm(10000, mean = meanConstant, sd = Sigma)
  data.frame(Value = x, FanoFactor = as.factor(i))
}

# Generate and bind data
plotData <- lapply(fanos, generateFanoData) %>% bind_rows()

# Create histogram data
histData <- plotData %>%
  group_by(FanoFactor) %>%
  summarise(bin = list((hist(Value, breaks = 50, plot = FALSE)$breaks[-length(hist(Value, breaks = 50, plot = FALSE)$breaks)] + hist(Value, breaks = 50, plot = FALSE)$breaks[-1]) / 2),
            count = list(hist(Value, breaks = 50, plot = FALSE)$counts)) %>%
  unnest(bin, count)

# Plot
p <- ggplot(histData, aes(x = bin, y = count, linetype = FanoFactor)) +
  geom_line(size = 1.2) +
  labs(x = "Data value", y = "Count", title = "") +
  scale_linetype_manual(values = c(1, 2, 3)) + theme(legend.position = "topright") +
  guides(linetype = guide_legend(title = "FF", override.aes = list(size = 1.2)))

# Display the plot
print(p)
ggsave("desc_randnFF.png", plot=p, width=5, height=5, dpi=figdpi)
















#-------- Figure 4.20: IQR ---------#
#-----------------------------------#
# Create a dataset
X <- exp(rnorm(1000)/3)

# Find its quartiles
quartiles <- quantile(X)
quartiles <- quartiles[2:4]

# Maximum count for histogram
max_count <- max(hist(X, breaks = "FD", plot = FALSE)$counts)

# Create a histogram
p <- ggplot(data.frame(X), aes(x = X)) +
  geom_histogram(bins = 30, fill = "grey", color = "black") +
  labs(x = "Data Value", y = "Histogram count", title = "") +
  ylim(0, max(hist(X, breaks = "FD", plot = FALSE)$counts) + 10)

# Add arrows for quartiles
p <- p +
  geom_segment(aes(x = quartiles["25%"], y = 0, xend = quartiles["25%"], yend = max_count),
               arrow = arrow(type = "closed", length = unit(0.2, "inches"), ends = "first"),
               color = "black", size = 1.2) +
  geom_segment(aes(x = quartiles["50%"], y = 0, xend = quartiles["50%"], yend = max_count),
               arrow = arrow(type = "closed", length = unit(0.2, "inches"), ends = "first"),
               color = "black", size = 1.2) +
  geom_segment(aes(x = quartiles["75%"], y = 0, xend = quartiles["75%"], yend = max_count),
               arrow = arrow(type = "closed", length = unit(0.2, "inches"), ends = "first"),
               color = "black", size = 1.2)

# Add text for quartiles
p <- p + annotate("text", x = quartiles, y = max(hist(X, breaks = "FD", plot = FALSE)$counts) + 10, 
                  label = c("Q1", "Q2", "Q3"), size = 5)

# Add arrow for IQR
p <- p + geom_segment(aes(x = quartiles["25%"], y = max(hist(X, breaks = "FD", plot = FALSE)$counts) / 2, 
                          xend = quartiles["75%"], yend = max(hist(X, breaks = "FD", plot = FALSE)$counts) / 2),
                      arrow = arrow(type = "closed", length = unit(0.2, "inches")), 
                      color = "black", size = 1.2)

# Add text for IQR
p <- p + annotate("text", x = mean(quartiles[c("25%", "75%")]), 
                  y = max(hist(X, breaks = "FD", plot = FALSE)$counts) / 2 + 5, 
                  label = "IQR", size = 6, color = "black")

# Display the plot
print(p)
ggsave("desc_IQR.png", plot=p, width=10, height=5, dpi=figdpi)















#-------- Figure 4.21: Create a QQ plot ---------#
#------------------------------------------------#
# Data preparation
N <- 1000
d1 <- rnorm(N)  # Normal
d2 <- exp(d1 * 0.8)

# Histogram data for d1
hist1 <- hist(d1, breaks = 40, plot = FALSE)
y1 <- hist1$counts / max(hist1$counts)
x1 <- (hist1$breaks[-length(hist1$breaks)] + hist1$breaks[-1]) / 2

# Histogram data for d2
hist2 <- hist(d2, breaks = 40, plot = FALSE)
y2 <- hist2$counts / max(hist2$counts)
x2 <- (hist2$breaks[-length(hist2$breaks)] + hist2$breaks[-1]) / 2

# Analytic normal distribution
x <- seq(-4, 4, length.out = 10001)
normDensity <- dnorm(x)
normDensity <- normDensity / max(normDensity)

# Data frames for ggplot
df_d1 <- data.frame(x = x1, y = y1)
df_d2 <- data.frame(x = x2, y = y2)
df_norm <- data.frame(x, y = normDensity)

# Plot A
pA <- ggplot() +
  geom_line(data = df_norm, aes(x, y), lwd = 1) +
  geom_line(data = df_d1, aes(x, y), lwd = 1, linetype = "dashed") +
  labs(x = "Data Value", y = "Probability (norm.)", title = bquote(bold('A)') ~ "Distributions")) +
  theme_bw()

# Plot B
pB <- ggplot() +
  geom_line(data = df_norm, aes(x, y), lwd = 1) +
  geom_line(data = df_d2, aes(x, y), lwd = 1, linetype = "dashed") +
  labs(x = "Data Value", y = "Probability (norm.)", title = bquote(bold('B)') ~ "Distributions")) +
  theme_bw()

# QQ plot for d1
pC <- ggplot(data.frame(d1), aes(sample = d1)) +
  stat_qq() +
  stat_qq_line(color = "darkgray", size = 1.5) +
  labs(title = bquote(bold('C)') ~ "QQ plot"), y = "Data values (sorted)") +
  theme_bw()

# QQ plot for d2
pD <- ggplot(data.frame(d2), aes(sample = d2)) +
  stat_qq() +
  stat_qq_line(color = "darkgray", size = 1.5) +
  labs(title = bquote(bold('D)') ~ "QQ plot"), y = "Data values (sorted)") +
  theme_bw()

# Arrange plots
p <- grid.arrange(pA, pB, pC, pD, ncol = 2, nrow = 2)
ggsave("desc_qq.png", plot=p, width=10, height=7, dpi=figdpi)














#-------- Figure 4.22: Table for moments ---------#
#-------------------------------------------------#
# install.packages("kableExtra")  # needed for table formatting and printing.
# install.packages("magick")
library(kableExtra)

tableData <- data.frame(
  check.names = F,
  "Moment number" = c("First", "Second", "Third", "Fourth"),
  "Name" = c("Mean", "Variance", "Skew", "Kurtosis"),
  "Description" = c("Average", "Dispersion", "Asymmetry", "Tail fatness"),
  "Formula" = c(
    "$m_1 = n^{-1}\\sum_{i=1}^n x_i$",
    "$m_2 = n^{-1}\\sum_{i=1}^n (x_i-\\bar{x})^2$",
    "$m_3 = (n\\sigma^3)^{-1}\\sum_{i=1}^n (x_i-\\bar{x})^3$",
    "$m_4 = (n\\sigma^4)^{-1}\\sum_{i=1}^n (x_i-\\bar{x})^4$"
  )
)

myTable <- kable(tableData, row.names = F) %>%  # format = "latex"
  row_spec(0, bold = TRUE, background = "#cccccc") %>%
  row_spec(0:nrow(tableData), align = "center", extra_css = "border: 1px solid;") %>%
  kable_styling() #%>%

myTable
# exporting tables is tricky; best to take a screenshot if you want to save it












#-------- Figure 4.23: Illustration of skew ---------#
#----------------------------------------------------#
# Data preparation
x <- seq(0, 4, length.out = 10001)
y <- df(x, 10, 100)
y <- y / max(y)
yBar <- sum(y) * mean(diff(x))

# Data frames for ggplot
df_left <- data.frame(x = -x, y = y)
df_right <- data.frame(x = x, y = y)

# Plot A: Left (negative) Skew
pA <- ggplot(df_left, aes(x = x, y = y)) +
  geom_line(size = 1.2) +
  geom_vline(xintercept = -yBar, linetype = "dashed", size = 1.2) +
  labs(x = "Data Value", y = "Proportion",
       title = bquote(bold('A)') ~ "Left (negative) Skew")) +
  annotate("text", x = -3, y = 0.5, label = expression(sum((x - bar(x))^3) < 0), size = 6) +
  theme_bw() +
  theme(axis.text.x = element_blank(), axis.text.y = element_blank())

# Plot B: Right (positive) Skew
pB <- ggplot(df_right, aes(x = x, y = y)) +
  geom_line(size = 1.2) +
  geom_vline(xintercept = yBar, linetype = "dashed", size = 1.2) +
  labs(x = "Data Value", y = "Proportion",
       title = bquote(bold('B)') ~ "Right (positive) Skew")) +
  annotate("text", x = 3, y = 0.5, label = expression(sum((x - bar(x))^3) > 0), size = 6) +
  theme_bw() +
  theme(axis.text.x = element_blank(), axis.text.y = element_blank())

# Arrange plots side by side
p <- grid.arrange(pA, pB, ncol = 1)
ggsave("desc_skew.png", plot=p, width=4, height=5, dpi=figdpi)










#-------- Figure 4.24: Kurtosis ---------#
#----------------------------------------#
# Generate the distributions
x <- seq(-3, 3, length.out = 10001)
g1 <- exp(-0.5 * x ^ 2)
g2 <- exp(-x ^ 2)
g3 <- exp(-10 * x ^ 4)

# Create a data frame
data <- data.frame(x, g1, g2, g3)

# Convert from wide to long format
data_long <- gather(data, key = "Distribution", value = "Value", -x)

# Replace names with legend titles
data_long$Distribution <- factor(data_long$Distribution, 
                                 levels = c("g1", "g2", "g3"),
                                 labels = c("-ve kurtosis", "no excess", "+ve kurtosis"))

# Create the plot
p <- ggplot(data_long, aes(x = x, y = Value, linetype = Distribution)) +
  geom_line(size = 1.2) +
  labs(x = "", y = "") +
  scale_linetype_manual(values = c(1, 2, 3)) +
  theme(legend.position = "topright")

# Display the plot
print(p)
ggsave("desc_kurtosis.png", plot=p, dpi=figdpi)












#-------- Figure 4.25: Different kurtosis with the same variance ---------#
#-------------------------------------------------------------------------#
x1 <- exp(-abs(3 * rnorm(4000)))
x2 <- exp(-abs(3 * rnorm(4000)))
d1 <- x1 - x2 + 1
d2 <- runif(4000)
d3 <- rnorm(4000)

allData <- list(d1, d2, d3)
stats <- data.frame(
  "d1" = rep(NA, 4),
  "d2" = rep(NA, 4),
  "d3" = rep(NA, 4),
  row.names = c("Mean", "Variance", "Skew", "Kurtosis")
)

plotData <- sapply(seq_along(allData), function(i) {
  X <- allData[[i]]
  # optional normalization
  X <- (X - mean(X)) / sd(X)
  hist <- hist(X, breaks = "FD", plot = F)
  x1 <-
    (hist$breaks[-length(hist$breaks)] + hist$breaks[2:length(hist$breaks)]) / 2
  y1 <- 100 * hist$counts / sum(hist$counts)
  stats[paste0("d", i)] <<-
    c(mean(X), var(X), skewness(X), kurtosis(X))
  return(list("x1" = x1, "y1" = y1))
})

plot(
  plotData[, 1]$x1,
  plotData[, 1]$y1,
  ylab = "Percentage",
  xlab = "Data Value",
  type = "l",
  col = "lightgray",
  lwd = 2
)
lines(
  plotData[, 2]$x1,
  plotData[, 2]$y1,
  type = "l",
  col = "black",
  lwd = 2
)
lines(
  plotData[, 3]$x1,
  plotData[, 3]$y1,
  type = "l",
  col = "darkblue",
  lwd = 2
)
legend(
  "topright",
  col = c("lightgray", "black", "darkblue"),
  lty = 1,
  legend = c("d1", "d2", "d3")
)


myTable <- kable(stats) %>%  # format = "latex"
  row_spec(0, bold = TRUE, background = "#cccccc") %>%
  row_spec(0:nrow(tableData), align = "center", extra_css = "border: 1px solid;") %>%
  kable_styling()

myTable











#-------- Figure 4.27: Histogram bins ---------#
#----------------------------------------------#
tableData <- data.frame(
  check.names = F,
  "Method" = c("Arbitrary", "Sturges", "Friedman-Diaconis"),
  "Formula" = c(
    "$k=40$",
    "$k=\\lceil \\log_2(n)\\rceil+1$",
    "$w=2\\frac{IQR}{\\sqrt[3]{n}}$"
  ),
  "Key advantage" = c("Simple", "Depends on count", "Depends on count and spread")
)

myTable <- kable(tableData) %>%  # format = "latex"
  row_spec(0, bold = TRUE, background = "#cccccc") %>%
  row_spec(0:nrow(tableData), align = "center", extra_css = "border: 1px solid;") %>%
  kable_styling()

myTable













#-------- Figure 4.28: Cityscape of variable histogram bins ---------#
#--------------------------------------------------------------------#
# Generate data
myData <- rnorm(1000)
minData <- min(myData)
maxData <- max(myData)

# Random bin boundaries
bw <- c(minData)
while (tail(bw, 1) < maxData) {
  bw <- c(bw, tail(bw, 1) + runif(1))
}

# Compute histogram data manually
histData <- hist(myData, breaks = bw, plot = FALSE)
bins <- data.frame(start = histData$breaks[-length(histData$breaks)], 
                   end = histData$breaks[-1], 
                   count = histData$counts)

# Assign colors to each bin
bins$color <- gray.colors(nrow(bins))

# Create the plot
p <- ggplot(bins, aes(xmin = start, xmax = end, ymin = 0, ymax = count, fill = color)) +
  geom_rect(color="black") +
  scale_fill_identity() +
  labs(x = "Data value", y = "Count", title = "") +
  theme_minimal()

# Display the plot
print(p)
ggsave("desc_variableBins.png", plot=p, dpi=figdpi)














#-------- Exercise 1 ---------#
#-----------------------------#
x <- seq(-3, 3, length.out = 111)
Sigma <- 0.73

# one gaussian
a <- 1 / (Sigma * sqrt(2 * pi))
eTerm <- -x ^ 2 / (2 * Sigma ^ 2)
gaus <- a * exp(eTerm)

plot(
  x = x,
  y = gaus,
  main = "Gaussian probability density",
  ylab = "Probability",
  xlab = "Numerical value",
  lwd = 2.0,
  type = "l"
)

saveRplot(plot_name = "desc_analytic_gaussian.png")

# create a family of Gaussians
N <- 50
sigmas <- seq(0.1, 3, length.out = N)
G <- matrix(nrow = 0, ncol = length(x))

for (i in 1:N) {
  eTerm <- -x ^ 2 / (2 * sigmas[i] ^ 2)
  G <- rbind(G, exp(eTerm))
}

# visualize a few
whichGaussians <- round(seq(4, N - 1, length.out = 8))

matplot(
  x,
  t(G[whichGaussians, ]),
  type = "l",
  lwd = 3,
  ylab = "Height",
  xlab = "x",
  col = 1:length(whichGaussians),
  lty = 1
)

legend(
  "topright",
  lty = 1,
  col = 1:length(whichGaussians),
  lwd = 3,
  legend = sapply(sigmas[whichGaussians],
                  function(x)
                    bquote(paste(
                      sigma, "=", .(round(x, 2))
                    )))
)


# now show as a matrix
library(Matrix)
image(
  t(G),
  xlab = "x",
  ylab = expression(sigma),
  col = gray.colors(10),
  xaxt = "n",
  yaxt = "n"
)
axis(1,
     at = seq(0, 1, length.out = 5),
     labels = seq(min(x), max(x), length.out = 5))
axis(2,
     at = seq(0, 1, length.out = 5),
     labels = round(seq(min(sigmas), max(sigmas), length.out = 5), 1))


# this code is identical to that above, just in subplots for the book figure

par(mfrow = c(1, 3))
# one gaussian
a <- 1 / (Sigma * sqrt(2 * pi))
eTerm <- -x ^ 2 / (2 * Sigma ^ 2)
gaus <- a * exp(eTerm)
plot(
  x = x,
  y = gaus,
  main = substitute(bold("A)")),
  ylab = "Probability",
  xlab = "Numerical value",
  lwd = 2.0,
  type = "l"
)

# a few guassians
N <- 50
sigmas <- seq(0.1, 3, length.out = N)
G <- matrix(nrow = 0, ncol = length(x))
for (i in 1:N) {
  eTerm <- -x ^ 2 / (2 * sigmas[i] ^ 2)
  G <- rbind(G, exp(eTerm))
}
whichGaussians <- round(seq(4, N - 1, length.out = 8))
matplot(
  x,
  t(G[whichGaussians, ]),
  type = "l",
  lwd = 3,
  ylab = "Height",
  xlab = "x",
  col = 1:length(whichGaussians),
  lty = 1,
  main = substitute(bold("B)"))
)
legend(
  "topright",
  lty = 1,
  col = 1:length(whichGaussians),
  lwd = 3,
  legend = sapply(sigmas[whichGaussians],
                  function(x)
                    bquote(paste(
                      sigma, "=", .(round(x, 2))
                    )))
)

# the gaussian family portrait
library(Matrix)
image(
  t(G),
  xlab = "x",
  ylab = expression(sigma),
  col = gray.colors(10),
  xaxt = "n",
  yaxt = "n",
  main = substitute(bold("C)"))
)
axis(1,
     at = seq(0, 1, length.out = 5),
     labels = seq(min(x), max(x), length.out = 5))
axis(2,
     at = seq(0, 1, length.out = 5),
     labels = round(seq(min(sigmas), max(sigmas), length.out = 5), 1))

saveRplot(plot_name = "desc_ex_gaussians.png")










#-------- Exercise 2 ---------#
#-----------------------------#
# here is the sum
print(rowSums(G))

# we don't want the mean
print(rowMeans(G))

# we want the discrete integral
print(rowSums(G) * mean(diff(x)))








#-------- Exercise 3 ---------#
#-----------------------------#
computeStats <- function(mydata) {
  # for convenience
  N <- length(mydata)
  
  # mean
  myMean <- sum(mydata) / N
  
  # median
  # first sort the data
  sortedData <- sort(mydata)
  
  # then compute the median based on whether it's odd or even N
  if (N %% 1 == 1) {
    # odd case
    myMedian <- sortedData[N %/% 2 + 1]
  } else {
    # even case
    myMedian <- sum(sortedData[(N %/% 2):(N %/% 2 + 1)]) / 2
  }
  
  # variance
  myVar <- sum((mydata - myMean) ^ 2) / (N - 1)
  
  return(c(
    "Mean" = myMean,
    "Median" = myMedian,
    "Variance" = myVar
  ))
}

mydata <- c(1, 7, 2, 7, 3, 7, 4, 7, 5, 7, 6, 7)
mydata <- round(runif(24, min = 4, max = 21))
rStats <-
  c(
    "Mean" = mean(mydata),
    "Median" = median(mydata),
    "Variance" = var(mydata)
  )
myStats <- computeStats(mydata)

print(cbind(myStats, rStats))












#-------- Exercise 4 ---------#
#-----------------------------#
sampleSizes <- seq(5, 100)
numExperiments <- 25

# initialize results matrix
ddofImpact <-
  matrix(nrow = length(sampleSizes), ncol = numExperiments)


# double for-loop over sample sizes and experiment repetitions
for (ni in seq_along(sampleSizes)) {
  for (expi in 1:numExperiments) {
    # generate random data
    myData <- sample(seq(-100, 100), sampleSizes[ni], replace = T)
    
    # compute variance difference
    varDiff <-
      sum((myData - mean(myData)) ^ 2 / (length(myData) - 1)) - sum((myData - mean(myData)) ^
                                                                      2 / length(myData))
    
    # uncomment the lines below for exercise 5
    d <-
      sum((myData - mean(myData)) ^ 2 / (length(myData) - 1)) - sum((myData - mean(myData)) ^
                                                                      2 / length(myData))
    a <-
      sum((myData - mean(myData)) ^ 2 / (length(myData) - 1)) + sum((myData - mean(myData)) ^
                                                                      2 / length(myData))
    # varDiff = d / a
    
    # store magnitude
    ddofImpact[ni, expi] <- varDiff
  }
}

# compute average and std across experiment runs
meanDiffs <- rowMeans(ddofImpact)
stdDiffs <- apply(ddofImpact, 1, sd)

plot(
  sampleSizes,
  meanDiffs,
  pch = 19,
  ylab = "Variance Difference",
  xlab = "Sample size",
  main = "Impact of ddof parameter",
  ylim = range(c(meanDiffs - stdDiffs, meanDiffs + stdDiffs))
)
arrows(
  x0 = sampleSizes,
  y0 = meanDiffs - stdDiffs,
  y1 = meanDiffs + stdDiffs,
  angle = 90,
  code = 3,
  length = 0.05
)
saveRplot(plot_name = "desc_ex_varDiffs.png")







#-------- Exercise 5 ---------#
#-----------------------------#

# The code solution to this exercise is in the previous exercise.
# Just uncomment the second calculation of variable varDiff.

# The reason why the normalized differences are simply 1/(2n-1) comes from
# writing out the difference using the formula for variance. You'll find
# that the summation terms cancel and only the 1/n or 1/(n-1) terms remain.
# Then you apply a bit of algebra to reduce to 1/(2n-1).









#-------- Exercise 6 ---------#
#-----------------------------#
# Compare mean and median with and without outliers, for large and small N
Ns <- c(50, 5000)

par(mfrow = c(2, 2))

means <- matrix(nrow = 2, ncol = 2)
medians <- matrix(nrow = 2, ncol = 2)

for (ni in seq_along(Ns)) {
  # create the data as normal random numbers
  myData <- rnorm(Ns[ni])
  
  for (outi in 1:2) {
    # I created an outlier by squaring the largest random sample
    maxVal <- max(myData)
    maxIdx <- which.max(myData)
    outlier <- c(1, 4)[outi]
    myData[maxIdx] <- maxVal ^ outlier
    
    # store results in matrices
    means[ni, outi] <- mean(myData)
    medians[ni, outi] <- median(myData)
    
    hist(
      myData,
      breaks = seq(min(myData), max(myData) + 1, by = 0.5),
      col = "#808080",
      border = "#808080",
      ylab = "",
      xlab = "",
      xlim = c(-3, 3),
      main = paste0("N = ", Ns[ni], ifelse(
        outi == 1, ", no outlier", ", with outlier"
      ))
    )
    abline(
      v = mean(myData),
      col = "gray",
      lwd = 3,
      lty = 3
    )
    abline(
      v = median(myData),
      col = "black",
      lwd = 2,
      lty = 2
    )
    legend(
      "topright",
      legend = c("Mean", "Median"),
      lty = c(3, 2),
      col = c("gray", "black")
    )
  }
}
saveRplot(plot_name = "desc_ex_outliersN.png")

# print results
for (ni in 1:2) {
  print(paste0(
    "With N = ",
    Ns[ni],
    ", the mean increased by ",
    round(means[ni, 2] - means[ni, 1], 2)
  ))
  print(paste0(
    "With N = ",
    Ns[ni],
    ", the median increased by ",
    round(medians[ni, 2] - medians[ni, 1], 2)
  ))
}











#-------- Exercise 7 ---------#
#-----------------------------#
library(e1071)

normValue <- rnorm(1000000, mean = 0, sd = 1)
paste0("Average: ", mean(normValue))
paste0("Variance: ", var(normValue))
paste0("Skewness: ", skewness(normValue))
paste0("Kurtosis: ", kurtosis(normValue))






#-------- Exercise 8 ---------#
#-----------------------------#
moments <- function(mydata) {
  # 1st moment is the mean
  myMean <- mean(mydata)
  
  # 2nd moment is variance
  myVar <- var(mydata)
  
  # 3rd moment is skew
  mySkew <- skewness(mydata)
  
  # 4th moment is kurtosis
  myKurt <- kurtosis(mydata)
  
  return(c(myMean, myVar, mySkew, myKurt))
}

# generate the data
sigmas <- seq(0.1, 1.2, length.out = 20)
X <- rnorm(13524)

# initialize the data results matrices
M <- matrix(nrow = 0, ncol = 4)
myData <- rep(0, length(sigmas))

for (i in seq_along(sigmas)) {
  myData[i] <- list(exp(X * sigmas[i]))
  M <- rbind(M, moments(unlist(myData[i])))
}

par(mfrow = c(1, 2))
matplot(
  x = sigmas,
  y = M,
  type = "b",
  lwd = 2,
  ylab = "Moment Value",
  col = "black",
  pch = 1:4,
  cex = 0.6,
  xlab = expression(sigma),
  main = substitute(paste(bold("A) "), "Statistical moments")),
  ylim = c(min(M), max(M)),
  log = "y"
)
legend(
  "bottomright",
  lwd = 2,
  legend = c("Mean", "Var.", "Skew", "Kurt."),
  cex = 0.8,
  pch = 1:4,
)
byVal <- 4
sapply(seq(1, length(sigmas), by = byVal), function(x) {
  arrows(
    x0 = sigmas[x],
    y0 = exp(-2),
    y1 = exp(2),
    lwd = 2,
    code = 1,
    col = ceiling(x / byVal)
  )
  text(sigmas[x], exp(2.5), round(sigmas[x], 2))
})

# now plot selected distributions
plot(
  NA,
  ylim = c(min(M), 1300),
  ylab = "Count",
  xlab = "Data Value",
  xlim = c(0, 6),
  main = substitute(paste(bold("B) "), "Distributions")),
)

for (i in seq(1, length(sigmas), by = byVal)) {
  histValues <- hist(unlist(myData[i]), breaks = "FD", plot = F)
  x <-
    (histValues$breaks[-length(histValues$breaks)] + histValues$breaks[2:length(histValues$breaks)]) / 2
  lines(
    x,
    histValues$counts,
    col = ceiling(i / byVal),
    lwd = 3,
    lty = i,
  )
}
legend(
  "topright",
  lty = 1:5,
  col = 1:5,
  lwd = 3,
  legend = sapply(seq(1, length(sigmas), by = 4),
                  function(x)
                    bquote(paste(
                      sigma, "=", .(round(sigmas[x], 2))
                    ))),
  cex = 0.8
)
saveRplot(plot_name = "desc_ex_moments.png")











#-------- Exercise 9 ---------#
#-----------------------------#
# Generate random dataset
X <- rnorm(10000)
eX <- exp(X)

iqr <- IQR(X)
std <- sd(X)

eiqr <- IQR(eX)
estd <- sd(eX)

print("Normal Distribution")
print(paste0('IQR = ', round(iqr, 3)))
print(paste0('1.35std = ', round(1.35 * std, 3)))

print("Log-normal Distribution")
print(paste0('IQR = ', round(eiqr, 3)))
print(paste0('1.35std = ', round(1.35 * estd, 3)))

par(mfrow = c(1, 2))

for (i in 1:2) {
  # select data
  myData <- if (i == 1)
    X
  else
    eX
  
  # convenience variable
  m <- mean(myData)
  std <- sd(myData) / 1.35
  
  plotTitles <- c(substitute(paste(bold("A) "), "Normal Data")),
                  substitute(paste(bold("B) "), "Non-normal Data")))
  # histogram of the data and maximum height value
  hist(
    myData,
    breaks = "FD",
    col = "#cccccc",
    border = "#cccccc",
    xlim = if (i == 2)
      c(0, 10)
    else
      c(-3, 3),
    main = plotTitles[i],
    ylab = "Count",
    xlab = "Data value"
  )
  
  # standard deviation lines
  abline(
    v = c(m - std, m + std),
    col = "black",
    lwd = 3,
    lty = 2
  )
  
  # quartile lines
  quart <- quantile(myData)
  abline(
    v = quart[c("25%", "75%")],
    col = "black",
    lwd = 3,
    lty = 1
  )
  legend(
    "topright",
    legend = c("std/1.35", "quartiles"),
    lty = c(2, 1),
    col = "black",
    lwd = 3
  )
}
saveRplot(plot_name = "desc_ex_stdiqr.png")










#-------- Exercise 10 ---------#
#------------------------------#
empFWHM <- function(x, y) {
  # normalize to [0, 1]
  y <- y - min(y)
  y <- y / max(y)
  
  # find peak
  idx <- which.max(y)
  
  # find value before peak
  prePeak <- x[which.min(abs(y[1:idx] - 0.5))]
  
  # find value after peak
  pstPeak <- x[idx - 1 + which.min(abs(y[idx:length(y)] - 0.5))]
  
  # return fwhm as that distance
  return(c(
    fwhm = pstPeak - prePeak,
    prePeak = prePeak,
    pstPeak = pstPeak
  ))
}

# try on pdf to compare with analytics
x <- seq(-8, 8, length.out = 1001)
s <- 1.9

# create an analytic Gaussian
pureGaus <- exp((-x ^ 2) / (2 * s ^ 2))

# empirical and analytical FWHM
values <- empFWHM(x, pureGaus)
afwhm <- 2 * s * sqrt(2 * log(2))

print(paste0("Empirical FWHM = ", round(values["fwhm"], 2)))
print(paste0("Analytical FWHM = ", round(afwhm, 2)))

# show the plot
plot(
  x,
  pureGaus,
  type = "l",
  lwd = 3,
  ylab = "",
  xlab = "",
  main = paste("Empirical:", round(values["fwhm"], 2), "Analytical:", round(afwhm, 2))
)
segments(
  y0 = 0.5,
  x0 = values["prePeak"],
  x1 = values["pstPeak"],
  lty = 3,
  lwd = 3
)

# try on pdf to comapre with analytic
ss <- seq(0.1, 5, length.out = 15)
fwhms <- matrix(nrow = length(ss), ncol = 2)

for (i in seq_along(ss)) {
  # create the Gaussian (don't need the 'a' term b/c we're already normalizing)
  gx <- exp((-x ^ 2) / (2 * ss[i] ^ 2))
  # compute FWHM and other related quantities
  fwhms[i, 1] <- empFWHM(x, gx)["fwhm"]
  fwhms[i, 2] <- 2 * ss[i] * sqrt(2 * log(2))
}

matplot(
  x = ss,
  y = fwhms,
  xlab = expression(paste(sigma, " value")),
  ylab = "FWHM",
  pch = c(15, 18),
  col = gray.colors(4),
  cex = 1.3
)
legend(
  "topleft",
  legend = c("Empirical", "Analytical"),
  pch = c(15, 18),
  col = gray.colors(4)
)
saveRplot(plot_name = "desc_ex_fwhm1.png")


# The problem is that the later Gaussians don't have a wide enough x-axis range,
# so the normalization distorts the Gaussian. This is illustrated in the code below.
# Increasing the range of the x-axis grid will fix the problem.

y <- gx - min(gx)
y <- y / max(y)

# plot original and normalized
matplot(
  x,
  cbind(gx, y),
  type = "l",
  lwd = 2,
  lty = 1,
  col = 1:2
)
legend(
  "center",
  legend = c("Original", "Normalized"),
  lwd = 2,
  lty = 1,
  col = 1:2
)

# Now for an empirical histogram

# try on random data
myData <- rnorm(12345)

# histogram
histValues <- hist(myData, breaks = 100, plot = F)
x <-
  (histValues$breaks[-length(histValues$breaks)] + histValues$breaks[2:length(histValues$breaks)]) / 2
y <- histValues$counts

# estimate the FWHM
values <- empFWHM(x, y)
midHeight <- (max(y) - min(y)) / 2

# and plot
plot(
  x,
  y,
  type = "b",
  xlab = "Data value",
  ylab = "Count",
  lwd = 2,
  main = paste0("Empirical FWHM = ", values["fwhm"])
)
segments(
  y0 = midHeight,
  x0 = values["prePeak"],
  x1 = values["pstPeak"],
  lty = 3,
  lwd = 2
)
saveRplot(plot_name = "desc_ex_fwhm2.png")







#-------- Exercise 11 ---------#
#------------------------------#
# generate data
N <- 100000
myData <- rnorm(N)
# myData <- runif(N)  # uncomment for uniform noise

binOpt <- list(40, "FD", "STURGES", "SCOTT")

plot(
  NA,
  ylim = c(0, N / 5),
  xlim = c(min(myData), max(myData)),
  ylab = "Count",
  xlab = "Data Value"
)
for (i in seq_along(binOpt)) {
  histValues <- hist(myData, breaks = binOpt[[i]], plot = F)
  x <-
    (histValues$breaks[-length(histValues$breaks)] + histValues$breaks[2:length(histValues$breaks)]) / 2
  y <- histValues$counts
  lines(
    x = x,
    y = y,
    lwd = 3,
    col = i,
    type = "b"
  )
}
legend(
  "topleft",
  legend = binOpt,
  lwd = 2,
  lty = 1,
  col = seq_along(binOpt)
)
saveRplot(plot_name = "desc_ex_histbins.png")

### Some observations about this exercise:
# - Some binnings give "taller" distributions, because they have fewer bins;
#   fewer bins means more data points per bin. Try normalizing the histograms
#   by plotting y/max(y)
#
# - When you use uniformly distributed data, it looks like some histograms disappear,
#   but in fact the different rules give identical bin counts so the histograms overlap.
#

#----
# Modern statistics: Intuition, Math, Python, R
## Mike X Cohen (sincxpress.com)
#### https://www.amazon.com/dp/B0CQRGWGLY
#### Code for chapter 8 (Probability theory)

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
library(kableExtra)
library(gridExtra)
library(dplyr)
















#-------- Figure 8.2: Pie charts for margin figure ---------#
#-----------------------------------------------------------#
# Function to create pie charts with contrasting labels
plot_pie <- function(k, title) {
  data <- data.frame(
    category = factor(1:k),
    count = rep(1 / k, k)
  )
  
  # Create a set of colors for the pie slices
  colors <- colorRampPalette(c("grey20", "white"))(k)
  
  ggplot(data, aes(x = "", y = count, fill = category)) +
    geom_bar(stat = "identity", width = 1, color = "black") +  # Added outline color
    coord_polar("y", start = 0) +
    scale_fill_manual(values = colors) +
    theme_void() +
    theme(legend.position = "none") +
    geom_text(aes(label = scales::percent(count)), position = position_stack(vjust = 0.5),
              color = "black") +
    ggtitle(title) +
    theme(plot.title = element_text(hjust = 0.5, vjust = -0.5))
}

# Create plots
p1 <- plot_pie(2, "Coin flip")
p2 <- plot_pie(6, "Die roll")

# Arrange and save plots
g <- grid.arrange(p1, p2, ncol = 1)

# Save the plot
ggsave("prob_probsInPies.png", g, width = 4, height = 6)
















#-------- Figure 8.4: Visualizing Probability masses and densities ---------#
#---------------------------------------------------------------------------#
### generate the data
# Categorical probability data
categoryLabels <- c('SUV', 'Convert.', 'Sports', 'Minivan', 'Coupe')
categoryData <- sample(5:30, length(categoryLabels), replace = TRUE)
categoryData <- categoryData / sum(categoryData)

# Discrete numerical probability data
empiricalIQ <- rnorm(100, mean = 100, sd = 15)

# Continuous (analytic) probability data
x <- seq(-4, 4, length.out = 101)
continuousData <- dnorm(x) * 15 + 100


## visualize
# A) pmf of car types
p1 <- ggplot(data.frame(Category = categoryLabels, Probability = categoryData), aes(x = Category, y = Probability)) +
  geom_bar(stat = "identity", fill = "grey80", color = "black") +
  labs(title = "A) pmf of car types", y = "Probability") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# B) pmf of IQ
p2 <- ggplot(data.frame(IQ = empiricalIQ), aes(x = IQ)) +
  geom_histogram(bins = 15, fill = "grey80", color = "black", aes(y = after_stat(density))) +
  labs(title = "B) pmf of IQ", x = "IQ", y = "Probability") +
  theme_minimal() +
  xlim(40, 160)

# C) pdf of IQ
p3 <- ggplot(data.frame(IQ = x * 15 + 100, Probability = continuousData), aes(x = IQ, y = Probability)) +
  geom_line(color = "black") +
  labs(title = "C) pdf of IQ", x = "IQ", y = "Probability") +
  theme_minimal() +
  xlim(40, 160)

# Arrange and save plots
g <- grid.arrange(p1, p2, p3, ncol = 3)

# Save the plot
ggsave("prob_visualizeMassDensity.png", g, width = 10, height = 3)













#-------- Figure 8.5: Probability mass function of penguin weights ---------#
#---------------------------------------------------------------------------#
# Generate penguin weight data
penguins <- atanh(runif(473) * 1.8 - 0.9) * 2 + 4.5

# Define bin edges
bin_edges <- seq(min(penguins), max(penguins), by = 0.25)

# Create histogram
p <- ggplot(data.frame(Weight = penguins), aes(x = Weight)) +
  geom_histogram(breaks = bin_edges, aes(y = ..density..), 
                 fill = "grey80", color = "black") +
  labs(x = "Penguin weight (kg)", y = "Probability") +
  theme_minimal()

# Print the plot
print(p)

# Save the plot
ggsave("prob_penguinWeightProb.png", p, width = 6, height = 4)














#-------- Figure 8.6: pdf and cdf ---------#
#------------------------------------------#
options(repr.plot.width = 6, repr.plot.height = 4, repr.plot.res = 200)
# Create the data
x <- seq(-5, 5, length.out = 501)
pdf <- dnorm(x)
cdf <- pnorm(x)

bndi <- which.min(abs(x-1))
dots <- data.frame(x = numeric(bndi + 2), y = numeric(bndi +2))
for ( i in 1:(bndi + 1)) {
  dots[i, ] <- c(x[i], pdf[i])
}

dots[bndi + 2, ] <- c(x[bndi], 0)
# Create the data frame
data <- data.frame(x = x, pdf = pdf, cdf = cdf)

# Create the plot with a legend inside
plot <- ggplot(data, aes(x)) +
  geom_line(aes(y = pdf, linetype = "pdf"), color = "black", linewidth = 1) +
  geom_line(aes(y = cdf, linetype = "cdf"), color = "black", linewidth = 1) +
  geom_vline(xintercept = 1, color = "black", linetype = "dotted") +
  geom_polygon(data = dots, aes(x = x, y = y), fill = "black", alpha = 0.4) +
  labs(x = "Data value", y = "Probability") +
  ylim(0, 1.02) +
  xlim(x[1], x[length(x)]) +
  theme_minimal() +
  theme(legend.position = c(.15, .85),
        legend.justification = c("left", "top"),
        legend.box.just = "left",
        legend.margin = margin(6, 6, 6, 6),
        legend.title = element_blank(), legend.box = "horizontal")

plot
ggsave("prob_pdf2cdf.png", plot, width = 7, height = 3)











#-------- Figure 8.9: pdf/cdf combos of some example distributions ---------#
#---------------------------------------------------------------------------#

# Define data for each distribution
# Gaussian
x_normal <- seq(-5, 5, length.out = 101)
pdf_normal <- dnorm(x_normal)
cdf_normal <- pnorm(x_normal)

# F-distribution
x_f <- seq(0, 6, length.out = 101)
pdf_f <- df(x_f, df1 = 5, df2 = 100)
cdf_f <- pf(x_f, df1 = 5, df2 = 100)

# Semicircular (these functions don't come standard in R)
# Define PDF and CDF for semicircular distribution
dsemicircular <- function(x, R = 1) {
  (x >= -R & x <= R) * (2 / (pi * R^2)) * sqrt(R^2 - x^2)
}
psemicircular <- function(x, R = 1) {
  (x >= -R & x <= R) * (0.5 + (1/pi) * (x * sqrt(R^2 - x^2) + R^2 * asin(x/R)))
}
x_sc <- seq(-1., 1., length.out = 101)
pdf_sc <- dsemicircular(x_sc)
cdf_sc <- psemicircular(x_sc)

# Create plots for each distribution
p_normal <- ggplot(data.frame(x_normal, pdf_normal, cdf_normal), aes(x = x_normal)) +
  geom_line(aes(y = pdf_normal), linetype = "dashed", size = 1) +
  geom_line(aes(y = cdf_normal), size = 1) +
  labs(title = "A) Normal") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

p_f <- ggplot(data.frame(x_f, pdf_f, cdf_f), aes(x = x_f)) +
  geom_line(aes(y = pdf_f), linetype = "dashed", size = 1) +
  geom_line(aes(y = cdf_f), size = 1) +
  labs(title = "B) F") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

p_sc <- ggplot(data.frame(x_sc, pdf_sc, cdf_sc), aes(x = x_sc)) +
  geom_line(aes(y = pdf_sc), linetype = "dashed", size = 1) +
  geom_line(aes(y = cdf_sc), size = 1) +
  labs(title = "C) Semicircular") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

# Arrange and save the plots
g <- grid.arrange(p_normal, p_f, p_sc, ncol = 3)

# Save the plot
ggsave("prob_examplePdfCdf.png", g, width = 10, height = 3)




















#-------- Figure 8.11: Softmax vs. "raw" probability ---------#
#-------------------------------------------------------------#
x <- c(4, 5, 7)

# softmax transformation
num   <- exp(x)
den   <- sum(exp(x))
sigma <- num / den
print(sigma)


# table
tabledata <- data.frame(
  Raw = formatC(x, format = "f", digits = 0),
  Softmax  = formatC(sigma, format = "f", digits = 3)
)
table <- kable(tabledata, align = "c", col.names = c("Raw", "Softmax")) %>%
  kable_styling(full_width = FALSE) %>%
  column_spec(1, bold = TRUE, border_right = TRUE) %>%
  row_spec(0, bold = TRUE, color = "white", background = "gray")
print(table)




# the data (marble color counts)
counts <- c(40, 30, 20)

# softmax
num <- exp(counts)
den <- sum(exp(counts))
sigma <- num / den

# standard probabilities
probs <- 100 * counts / sum(counts)

# print the results
cat("Softmax: \n", sigma, "\n")
cat('Probabilities: \n', probs)






options(repr.plot.width = 6, repr.plot.height = 5, repr.plot.res = 200)

x <- seq(0, 25, length.out = 21)
s <- exp(x) / sum(exp(x))
p <- x / sum(x)

# making the dataframe
data <- data.frame(x = x, Softmax = s, Probability = p)

plot <- ggplot(data, aes(x)) +
  geom_line(aes(y = Softmax, linetype = "Softmax"), color = "black", size =0.5) +
  geom_line(aes(y = Probability, linetype = "Probability"), color = "black", size = 0.4) +
  geom_point(aes(y = Softmax, shape = "Softmax"), color = "black", size = 3) +
  geom_point(aes(y = Probability, shape = "Probability"), color = "black", size = 3) +
  scale_shape_manual(name = "", values = c("Softmax" = 2, "Probability" = 1)) +
  scale_linetype_manual(name = "", values = c("Softmax" = "solid", "Probability" = "solid")) +
  theme_minimal() +
  theme(legend.position = c(0.4, 0.9),
        legend.box.background = element_rect(color = 'black', linewidth = 0.2),
        legend.box.margin = margin(0, 0, 0, 0))


print(plot)













#-------- Figure 8.10: Softmax in linear and log space ---------#
#---------------------------------------------------------------#
# Raw numerical data values
x <- seq(-6, 6, length.out = 81)
s <- exp(x) / sum(exp(x))

# Create data frame for plotting
data <- data.frame(x = x, Softmax = s)

# Plot in linear space
p1 <- ggplot(data, aes(x = x, y = Softmax)) +
  geom_line(color = "black", size = 1) +
  labs(title = "A) In linear space", x = "Raw data values", y = "Softmaxified values") +
  theme_minimal()

# Plot in log space
p2 <- ggplot(data, aes(x = x, y = Softmax)) +
  geom_line(color = "black", size = 1) +
  scale_y_log10() +
  labs(title = "B) In log space", x = "Raw data values", y = "Softmaxified values") +
  theme_minimal()

# Arrange and save the plots
g <- grid.arrange(p1, p2, ncol = 2)

# Save the plot
ggsave("prob_softmaxNumbers.png", g, width = 10, height = 4)











#-------- Exercise 1 ---------#
#-----------------------------#
# re-create the pdf
# (note: I'm using "4" in the variable names for comparisons in subsequent exercises)
x4 <- seq(-4, 4, length.out=400)
pdf4 <- dnorm(x4)

# normalize by pdf
pdf4N <- pdf4 * (x4[2]-x4[1]) 

# print sums
cat(sprintf("Sum over pdf: %.3f\n", sum(pdf4)))
cat(sprintf("Sum over normalized pdf: %.3f\n", sum(pdf4N)))










#-------- Exercise 2 ---------#
#-----------------------------#
# now with a restricted range
x2 <- seq(-2, 2, length.out = 300)
pdf2 <- dnorm(x2)

# normalize by dx
pdf2N <- pdf2 * (x2[2] - x2[1])

# normalize to sum=1 ("U" is for "unit")
pdf2U <- pdf2 / sum(pdf2)

# print sums
cat(sprintf("Sum over pdf normalize by dx : %.3f\n", sum(pdf2N)))
cat(sprintf("Sum over pdf normalized by sum: %.3f\n", sum(pdf2U)))

options(repr.plot.width = 8, repr.plot.height = 6, repr.plot.res = 200)

# plot
par(mfrow=c(1,1), mar=c(4,4,2,2))
plot(x4, pdf4N, type='l', lty=2, col='black', lwd=2, ylim=c(0, 0.008),
     xlab='x', ylab='Probability Density', main='Probability Density Functions',
     xlim=c(min(x4), max(x4)))
lines(x2, pdf2N, lty=3, col='gray', lwd=2)
lines(x2, pdf2U, col='lightgray', lwd=2)
legend('topright', legend=c('x to |4|', 'Norm by dx', 'Norm to unit'),
       lty=c(2, 3, 1), col=c('black', 'gray', 'lightgray'),x.intersp = 1.5,
       y.intersp = 1.5, lwd=2,text.width = 1, cex=0.7)

# From the explanations about exercise 1:
cat(sprintf("Sum over normalized pdf: %.9f\n", sum(pdf4N)))
ggsave("prob_ex2.png",plot, width = 8, height = 6)










#-------- Exercise 3 ---------#
#-----------------------------#
options(repr.plot.width = 8, repr.plot.height = 4, repr.plot.res = 200)

# paramter
npnts <- c(100, 1000)
colors <- c('black', 'grey')

# plot
plot <- ggplot() +
  theme_minimal()

# the loop that does it all
for (i in seq_along(npnts)) {
  x <- seq(-4, 4, length.out = npnts[i])
  
  # Evaluate the raw pdf
  pdfN <- dnorm(x) * diff(x[1:2])
  
  plot <- plot +
    geom_point(data = data.frame( x = x, pdfN = pdfN, N = as.character(npnts[i]),
                                  color = as.factor(npnts[i])),
               aes(x, pdfN, color = color),
               shape = '|', size = 4
    )
}

plot <- plot +
  xlim(range(x)) +
  labs(x = 'Data value', y = 'Probability') +
  scale_color_manual(name = '', values = colors, labels = paste("N=", npnts, ", sum = 1.00")) +
  theme(
    legend.position = c(0.85, 0.7),
    legend.box.background = element_rect(color = 'black', linewidth = 0.1),
    legend.justification = c(1, 0)
  )

print(plot)
ggsave("prob_ex3.png", plot, width = 6, height = 4)














#-------- Exercise 4 ---------#
#-----------------------------#
# Create cdf from pdf
x <- seq(-4, 4, length.out = 300)
pdf <- dnorm(x)

# R's cdf
cdf_sp <- pnorm(x)

# Manual computation
cdf_my <- cumsum(pdf)

# Normalized by dx
dx <- x[2] - x[1]
cdf_myN <- cumsum(pdf) * dx

# Create plots
p1 <- ggplot(data.frame(x, pdf), aes(x = x, y = pdf)) +
  geom_line(color = "black", size = 1) +
  labs(title = "A) Gaussian pdf (raw output)") +
  theme_minimal() +
  xlim(min(x), max(x))

p2 <- ggplot(data.frame(x, cdf_my, cdf_sp), aes(x = x)) +
  geom_line(aes(y = cdf_my), color = "black", size = 1, linetype = "dashed") +
  geom_line(aes(y = cdf_sp), color = "gray", size = 1) +
  labs(title = "B) Gaussian cdfs") +
  theme_minimal() +
  xlim(min(x), max(x))

p3 <- ggplot(data.frame(x, cdf_myN, cdf_sp), aes(x = x)) +
  geom_line(aes(y = cdf_myN), color = "black", size = 1, linetype = "dashed") +
  geom_line(aes(y = cdf_sp), color = "gray", size = 1) +
  labs(title = "C) Gaussian cdfs with normalization") +
  theme_minimal() +
  xlim(min(x), max(x))

# Arrange and save the plots
g <- grid.arrange(p1, p2, p3, ncol = 1)

# Save the plot
ggsave("prob_ex4.png", g, width = 7, height = 6)







#-------- Exercise 5 ---------#
#-----------------------------#
# High-resolution CDF
x_high <- seq(-4, 4, length.out = 5000)
pdf_high <- dnorm(x_high)
cdf_sp_high <- pnorm(x_high)
cdf_my_high <- cumsum(pdf_high) * (x_high[2] - x_high[1])

# Low-resolution CDF
x_low <- seq(-4, 4, length.out = 20)
cdf_sp_low <- pnorm(x_low)
cdf_my_low <- cumsum(dnorm(x_low)) * (x_low[2] - x_low[1])

# High-resolution plot
p_high <- ggplot() +
  geom_line(aes(x = x_high, y = cdf_sp_high), color = "gray", size = 1) +
  geom_line(aes(x = x_high, y = cdf_my_high), color = "black", size = 1, linetype = "dashed") +
  labs(title = "A) High resolution (5000 points)") +
  theme_minimal() +
  xlim(min(x_high), max(x_high))

# Low-resolution plot
p_low <- ggplot() +
  geom_line(aes(x = x_low, y = cdf_sp_low), color = "gray", size = 1) +
  geom_point(aes(x = x_low, y = cdf_sp_low), color = "gray", size = 2) +
  geom_line(aes(x = x_low, y = cdf_my_low), color = "black", size = 1, linetype = "dashed") +
  geom_point(aes(x = x_low, y = cdf_my_low), color = "black", size = 2) +
  labs(title = "B) Low resolution (20 points)") +
  theme_minimal() +
  xlim(min(x_low), max(x_low))

# Arrange and save the plots
g <- grid.arrange(p_high, p_low, ncol = 1)

# Save the plot
ggsave("prob_ex5.png", g, width = 8, height = 5)


















#-------- Exercise 6 ---------#
#-----------------------------#
# The non-standard pdf
x <- seq(-6, 6, length.out = 1001)
pdf <- dnorm(x - 2.7) + dnorm(x + 2.7)

# Simple scaling by dx
dx <- mean(diff(x))
cdf <- cumsum(pdf) * dx

# Better scaling by first unit-sum-normalizing pdf
pdfN <- pdf / sum(pdf)
cdfN <- cumsum(pdfN)

# Create plots
p_pdf <- ggplot(data.frame(x, pdf), aes(x = x, y = pdf)) +
  geom_line(color = "black", size = 1) +
  labs(title = "A) pdf") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_y_continuous(breaks = c(0, 0.3))

p_cdf <- ggplot(data.frame(x, cdf), aes(x = x, y = cdf)) +
  geom_line(color = "black", size = 1) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "grey70") +
  labs(title = "B) Improperly scaled cdf") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

p_cdfN <- ggplot(data.frame(x, cdfN), aes(x = x, y = cdfN)) +
  geom_line(color = "black", size = 1) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "grey70") +
  labs(title = "C) Properly scaled cdf") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

# Arrange and save the plots
g <- grid.arrange(p_pdf, p_cdf, p_cdfN, ncol = 1)

# Save the plot
ggsave("prob_ex6.png", g, width = 4, height = 6)











#-------- Exercise 7 ---------#
#-----------------------------#
# Create a CDF
x <- seq(0, 10, length.out = 200)
cdf <- plnorm(x, 1, 1/2)

# Empirical PDF via difference (D=difference)
pdfD <- diff(cdf)

# Analytical PDF (A=analytical)
pdfA <- dlnorm(x, 1, 1/2)
pdfA <- pdfA * diff(x)[1]

# Create plots
p_cdf <- ggplot(data.frame(x, cdf), aes(x = x, y = cdf)) +
  geom_line(color = "black", size = 1) +
  labs(title = "A) cdf of lognormal distribution") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab("Data value") + ylab("Prob")

p_pdf <- ggplot(data.frame(x = x[-length(x)], pdfD = pdfD, pdfA = pdfA[-length(pdfA)]), aes(x = x)) +
  geom_line(aes(y = pdfD), color = "black", size = 1) +
  geom_point(aes(y = pdfA), color = "grey40", fill = "white", shape = 1) +
  labs(title = "B) pdfs of lognormal distribution") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab("Data value") + ylab("Prob") +
  scale_x_continuous(limits = c(min(x), max(x)))

# Arrange and save the plots
g <- grid.arrange(p_cdf, p_pdf, ncol = 1)

# Save the plot
ggsave("prob_ex7.png", g, width = 6, height = 5)


# Note: The two pdf's appear to have some mismatch.
#       Try changing the resolution from 200 to 2000, and then to 20.









#-------- Exercise 8 ---------#
#-----------------------------#
# Colored marble counts
blue <- 40
yellow <- 30
orange <- 20
totalMarbs <- blue + yellow + orange

# Create a jar of marbles
jar <- c(rep(1, blue), rep(2, yellow), rep(3, orange))

# Draw 500 marbles (with replacement)
numDraws <- 500
marbSample <- sample(jar, size = numDraws, replace = TRUE)

# Proportions of colors drawn
propBlue <- sum(marbSample == 1) / numDraws
propYell <- sum(marbSample == 2) / numDraws
propOran <- sum(marbSample == 3) / numDraws

# Plotting
p <- ggplot(data.frame(Color = factor(c('Blue', 'Yellow', 'Orange'), levels = c('Blue', 'Yellow', 'Orange')), 
                       Proportion = c(propBlue, propYell, propOran)), aes(x = Color, y = Proportion)) +
  geom_bar(stat = "identity", fill = "grey70") +
  geom_segment(aes(x = 0.8, xend = 1.2, y = blue / totalMarbs, yend = blue / totalMarbs), color = "black", size = 1.5) +
  geom_segment(aes(x = 1.8, xend = 2.2, y = yellow / totalMarbs, yend = yellow / totalMarbs), color = "black", size = 1.5) +
  geom_segment(aes(x = 2.8, xend = 3.2, y = orange / totalMarbs, yend = orange / totalMarbs), color = "black", size = 1.5) +
  labs(x = "Marble color", y = "Proportion/probability") +
  theme_minimal() +
  scale_x_discrete(labels = c('Blue', 'Yellow', 'Orange')) +
  theme(legend.position = "none")

# Print the plot
print(p)

# Save the plot
ggsave("prob_ex8.png", p, width = 8, height = 4)














#-------- Exercise 9 ---------#
#-----------------------------#
# Colored marble counts
blue <- 40
yellow <- 30
orange <- 20
totalMarbs <- blue + yellow + orange

# Put them all in a jar
jar <- c(rep(1, blue), rep(2, yellow), rep(3, orange))

# Range of sample sizes
sampleSizes <- seq(20, 2000, by = 10)

# True probabilities
trueProbs <- c(blue / totalMarbs, yellow / totalMarbs, orange / totalMarbs)

# Initialize RMS
rms <- numeric(length(sampleSizes))

# Run the experiment
for (idx in 1:length(sampleSizes)) {
  thisN <- sampleSizes[idx]
  
  # Draw N marbles
  drawColors <- sample(jar, size = thisN, replace = TRUE)
  
  # Compute proportion
  empiProbs <- c(sum(drawColors == 1) / thisN, sum(drawColors == 2) / thisN, sum(drawColors == 3) / thisN)
  
  # Compute the sum of squared errors
  rms[idx] <- sqrt(mean((empiProbs - trueProbs)^2))
}

# Create plot
p <- ggplot(data.frame(SampleSize = sampleSizes, RMS = rms), aes(x = SampleSize, y = RMS)) +
  geom_point(color = "black", fill = "grey70", shape = 21) +
  labs(x = "Sample size", y = "RMS", title = "Empirical proportion errors") +
  theme_minimal() +
  scale_x_continuous(expand = c(0, 30))

# Print the plot
print(p)

# Save the plot
ggsave("prob_ex9.png", p, width = 8, height = 4)

















#-------- Exercise 10 ---------#
#------------------------------#

# Simulation parameters
N <- 1000  # Sample size
k <- 41    # Number of data bins

# Generate the data
data <- rnorm(N)

# Determine boundaries
bounds <- seq(-3, 3, length.out = k)

# Initialize the results (empirical proportions)
emppropsGT <- numeric(k)
emppropsLT <- numeric(k)
empprops2tail <- numeric(k)

# Loop over the boundaries
for (idx in 1:k) {
  bi <- bounds[idx]
  
  # Empirical proportions for each side separately
  emppropsGT[idx] <- sum(data > bi) / N
  emppropsLT[idx] <- sum(data < bi) / N
  
  # And for the two-sided
  empprops2tail[idx] <- sum(abs(data) > abs(bi)) / N
}

# Create data frame for plotting
plot_data <- data.frame(Bound = bounds, GT = emppropsGT, LT = emppropsLT, TwoTail = empprops2tail)

# Create plots
p1 <- ggplot(plot_data, aes(x = Bound)) +
  geom_point(aes(y = GT), shape = 1, color = "black", fill = "grey80", size = 3) +
  geom_point(aes(y = LT), shape = 1, color = "black", fill = "grey40", size = 3) +
  labs(title = "A) One-sided proportions", x = "Bound (zeta)", y = "Proportion") +
  theme_minimal() +
  ylim(-0.05, 1.05)

p2 <- ggplot(plot_data, aes(x = Bound)) +
  geom_point(aes(y = TwoTail), shape = 2, color = "black", fill = "grey80", size = 3) +
  labs(title = "B) Two-sided proportion", x = "Bound (zeta)", y = "Proportion") +
  theme_minimal() +
  ylim(-0.05, 1.05)

# Arrange and save the plots
g <- grid.arrange(p1, p2, ncol = 2)

# Save the plot
ggsave("prob_ex10.png", g, width = 10, height = 4)













#-------- Exercise 11 ---------#
#------------------------------#
# Here's the url:
# https://stat.ethz.ch/R-manual/R-devel/library/stats/html/Distributions.html




#----
# Modern statistics: Intuition, Math, Python, R
## Mike X Cohen (sincxpress.com)
#### https://www.amazon.com/dp/B0CQRGWGLY
#### Code for chapter 14 (ANOVA)

# About this code file:
### This code file will reproduce most of the figures in this chapter 
### (some figures were made in Inkscape), and illustrate the statistical 
### concepts explained in the text. The point of providing the code is not 
### just for you to recreate the figures, but for you to modify, adapt, 
### explore, and experiment with the code.
###
### Solutions to all exercises are at the bottom of the file.





# Load libraries
library(dplyr)
library(tidyr)
library(emmeans)
library(reshape)
library(rstatix)
library(stats)
library(ggplot2)
library(gridExtra)
library(esvis)
library(grid)
library(lsr)












#-------- Figure 14.3: Critical F by df's ---------#
#--------------------------------------------------#
# Define the degrees of freedom
df1_values <- 1:9
df2_values <- 5:29

# Create a matrix to store the critical F values
critFvals <- matrix(nrow = length(df2_values), ncol = length(df1_values))

# Populate the matrix with critical F values
for (i in seq_along(df1_values)) {
  for (j in seq_along(df2_values)) {
    critFvals[j,i] <- qf(0.95, df1_values[i], df2_values[j])
  }
}

# Convert the matrix to a data frame for plotting
df_for_plot <- melt(critFvals)
names(df_for_plot) <- c('Denominator_df', 'Numerator_df', 'Critical_F_Value')
df_for_plot$Denominator_df <- as.numeric(df2_values)

# Plot the heatmap
p <- ggplot(df_for_plot, aes(x = Numerator_df, y = Denominator_df, fill = Critical_F_Value)) +
  geom_tile() +
  scale_fill_gradientn(colors = gray.colors(3), limits = c(2, 5), name = "Critical F Value") +
  labs(x = "Numerator df", y = "Denominator df", title = "Critical F values\nfor df pairs") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


# Save the plot
print(p)
ggsave("anova_fCritBydf.png", plot = p, width = 4, height = 6)
















#-------- Figure 14.4: F-distributions ---------#
#-----------------------------------------------#
# Define the x range
x <- seq(0, 3.5, length.out = 1000)

# Define the degrees of freedom pairs
df_pairs <- list(c(6, 30), c(5, 25), c(4, 22), c(4, 15), c(2, 30))

# Prepare a data frame for plotting
plot_data <- data.frame(x = numeric(), F = numeric(), group = character(), crit_f_x = numeric())

# Populate the data frame
for (i in seq_along(df_pairs)) {
  df1 <- df_pairs[[i]][1]
  df2 <- df_pairs[[i]][2]
  F <- df(x, df1, df2)
  crit_f_x <- qf(0.95, df1, df2)
  group_label <- paste("F(", df1, ",", df2, ")", sep = "")
  plot_data <- rbind(plot_data, data.frame(x = x, F = F, group = group_label, crit_f_x = crit_f_x))
}

# Plot
p <- ggplot(plot_data, aes(x = x, y = F, color = group)) +
  geom_line(size = 1) +
  scale_color_manual(values = rev(gray.colors(5)))  +
  labs(title = "F-distributions for various df pairs",
       x = "F",
       y = "Probability density",
       color = "DF pairs") +
  theme_minimal()


# Add each F distribution and annotations
for (i in seq_along(df_pairs)) {
  df1 <- df_pairs[[i]][1]
  df2 <- df_pairs[[i]][2]
  F <- df(x, df1, df2)
  crit_f_x <- qf(.95, df1, df2)
  crit_f_y <- df(crit_f_x, df1, df2)
  lbl <- sprintf("Fc(%d,%d) = %.2f", df1, df2, crit_f_x)
  
  p <- p + annotate("text", x = crit_f_x, y = crit_f_y*3+.14, label = lbl,
             color = gray.colors(5)[i], size = 5, hjust = .5, angle=90) +
    geom_segment(aes(x = crit_f_x, xend = crit_f_x, yend=crit_f_y, y=crit_f_y*3),
                 arrow = arrow(type = "closed"), size = .8)
}

# Display the plot
print(p)

# Save the plot
ggsave("anova-FDists.png", plot = p, width = 10, height = 6)













#-------- Figure 14.6: Bar plot used for Tukey test description ---------#
#------------------------------------------------------------------------#
# Data
y <- c(5, 5, 10, 11)
L <- c('A', 'B', 'C', 'D')

# Create a data frame from the data
df <- data.frame(Condition = L, Outcome = y)

# Create the bar plot
p <- ggplot(df, aes(x = Condition, y = Outcome)) +
  geom_bar(stat = "identity", color = "black", fill = gray(0.3)) +
  xlab('Condition (level)') +
  ylab('Outcome variable') +
  theme_minimal()

# Save the plot
print(p)
ggsave("anova-4tukey.png", plot = p, width = 6, height = 3)
















#-------- Figure 14.7: Q-distributions with various df pairs ---------#
#---------------------------------------------------------------------#
# Note: R doesn't have a studentized range pdf function, so I created one here
# to approximate it by taking the derivative of the cdf.
studentized_range_pdf <- function(q, df1, df2, delta = 1e-3) {
  cdf_plus  <- ptukey(q + delta, df1, df2)
  cdf_minus <- ptukey(q - delta, df1, df2)
  (cdf_plus - cdf_minus) / (2 * delta)
}

# Define the x range
x <- seq(0, 6, length.out = 100)

# Define the degrees of freedom pairs
df_pairs <- list(c(6, 30), c(5, 25), c(4, 22), c(4, 15), c(2, 30))

# Prepare a data frame for plotting
plot_data <- data.frame(x = numeric(), Q = numeric(), group = character(), crit_f_x = numeric())

# Populate the data frame
for (i in seq_along(df_pairs)) {
  df1 <- df_pairs[[i]][1]
  df2 <- df_pairs[[i]][2]
  Q <- studentized_range_pdf(x, df1, df2)
  crit_q_x <- qtukey(0.95, df1, df2)
  group_label <- paste("Q(", df1, ",", df2, ")", sep = "")
  plot_data <- rbind(plot_data, data.frame(x = x, Q = Q, group = group_label, crit_q_x = crit_q_x))
}

# Plot
p <- ggplot(plot_data, aes(x = x, y = Q, color = group)) +
  geom_line(size = 1) +
  scale_color_manual(values = rev(gray.colors(5)))  +
  labs(title = "Q-distributions for various df pairs",
       x = "Q",
       y = "Probability density",
       color = "DF pairs") +
  theme_minimal()


# Add each Q distribution and annotations
for (i in seq_along(df_pairs)) {
  df1 <- df_pairs[[i]][1]
  df2 <- df_pairs[[i]][2]
  Q <- studentized_range_pdf(x, df1, df2)
  crit_q_x <- qtukey(.95, df1, df2)
  crit_q_y <- studentized_range_pdf(crit_q_x, df1, df2)
  lbl <- sprintf("Q(%d,%d) = %.2f", df1, df2, crit_q_x)
  
  p <- p + annotate("text", x=crit_q_x, y=crit_q_y*3, label = lbl,
                    color = gray.colors(5)[i], size = 5, hjust = .5, angle=90) +
    geom_segment(aes(x = crit_q_x, xend = crit_q_x, yend=crit_q_y, y=crit_q_y*2),
                 arrow = arrow(type = "closed"), size = .8)
}

# Display the plot
print(p)
ggsave("anova-QDists.png", plot = p, width = 10, height = 6)




















#-------- Figures 14.16-14.19: Example rmANOVA (the "snacks study") ---------#
#----------------------------------------------------------------------------#
# Create the data
data <- data.frame(
  Participant = rep(c('P1', 'P2', 'P3', 'P4', 'P5', 'P6', 'P7', 'P8'), times = 4),
  Snack = c(rep('Baseline', 8), rep('Chocolate', 8), rep('Chips', 8), rep('IceCream', 8)),
  Mood = c(5, 7, 6, 6, 5, 8, 7, 6,  # Baseline
           6, 8, 8, 7, 8, 9, 8, 7,  # Chocolate
           5, 7, 6, 5, 4, 6, 4, 6,  # Chips
           7, 9, 7, 8, 7, 9, 8, 9)  # Ice Cream
)

# Convert to a tibble for nicer printing (optional)
df <- as_tibble(data)

# Show the data in "long" format (displaying every 4th row)
# In the book figure 14.16, I show a screenshot from Python that would correspond to this:
df[seq(1, nrow(df), by = 4), ]

# and then Figure 14.17 is wide format:
df_wide <- df %>% 
  spread(key = Snack, value = Mood)

# View the wide format data
print(df_wide)




# Now for the repeated measures ANOVA (a screenshot of this is Figure 14.19)
rmANOVA <- aov(Mood ~ Snack + Error(Participant/Snack), df)
summary(rmANOVA)

# run post-hoc pairwise tests
pairwise_tests <- data %>%
  pairwise_t_test(Mood ~ Snack , paired = TRUE, 
                  p.adjust.method = "bonferroni")

print(pairwise_tests)





## and now for Figure 14.18
# Plot the data
p <- ggplot(df, aes(x = Snack, y = Mood, fill = Snack)) +
  geom_boxplot() +
  scale_fill_brewer(palette = "BuPu") +
  labs(title = "Mood scores by Snack type", x = "Snack", y = "Mood") +
  theme_minimal()

# Display the plot
print(p)

# Save the plot
ggsave("anova_rmSnackRes.png", plot = p, width = 8, height = 4)





## predicted values and residuals
# Calculate the mean for each group and add predicted and residual columns
df <- df %>%
  group_by(Snack) %>%
  mutate(
    Predicted = mean(Mood),  # Calculate mean Mood for each Snack group
    Residual = Mood - Predicted  # Compute residuals
  ) %>%
  ungroup()  # Remove grouping

# Show a few rows (every 4th row)
df[seq(1, nrow(df), by = 4), ]


















#-------- Figure 14.21: Inspecting ANOVA results ---------#
#---------------------------------------------------------#
# Histogram of Residuals
p1 <- ggplot(df, aes(x = Residual)) +
  geom_histogram(bins = 5, fill = "grey70", color = "black") +
  xlab("Residuals") +
  ylab("Count") +
  ggtitle(expression(bold("A)") ~ "Residuals histogram"))

# Scatter plot of Residuals vs Predicted values
p2 <- ggplot(df, aes(x = Predicted, y = Residual)) +
  geom_hline(yintercept = 0, color = "black") +
  geom_point(color = "black", fill = "grey70", shape = 21, size = 3) +
  xlab("Predicted values") +
  ylab("Residuals") +
  xlim(5, 8.5) +
  ylim(-2.5, 2.5) +
  ggtitle(expression(bold("B)") ~ "Residuals vs. " * hat(y)))

# QQ plot
p3 <- ggplot(df, aes(sample = Residual)) +
  stat_qq(distribution = qnorm, dparams = list(mean = 0, sd = 1)) +
  stat_qq_line(distribution = qnorm, dparams = list(mean = 0, sd = 1), color = "black") +
  ggtitle(expression(bold("C)") ~ "QQ-plot"))

# Arrange plots into one figure
p <- grid.arrange(p1, p2, p3, ncol = 3)
ggsave("anova_residuals.png", plot=p, width = 10, height = 3.5)




















#-------- Figure 14.24: Simulate data for a one-way ANOVA ---------#
#------------------------------------------------------------------#
### create the data
# Group means and number of levels
level_means <- c(0, 0.1, 0.5)

# Sample size and dataset size
nLevels <- length(level_means)
samplesize <- 34
nDataRows <- samplesize * nLevels # Total rows in the dataset

# Create the column with group assignments
group_column <- rep(seq_len(nLevels), each = samplesize)

# Column data (initialize as zeros, then modulate by level_means)
col_data <- numeric(nDataRows)
for (i in seq_along(level_means)) {
  # Population cell mean
  cellMean <- level_means[i]
  
  # Random data for those rows
  col_data[group_column == i] <- rnorm(samplesize, 
                                       mean = cellMean, sd = 1)
}

# Import data into a dataframe
df <- data.frame(
  Group = factor(group_column),
  Value = col_data
)


### visualize the data
dfd <- head(df, 9)
dfd$Group <- format(dfd$Group, scientific = FALSE)
dfd$Value <- sprintf("%.2f", dfd$Value)

# Create a table plot
table_plot <- tableGrob(dfd, cols = names(dfd), theme = ttheme_default(base_size = 14))

# Create a boxplot
p <- ggplot(df, aes(x = factor(Group), y = Value)) +
  geom_boxplot() +
  labs(title = expression(bold("B)") ~ " Data box plots"), x = "Group", y = "Value")

# Arrange the table and boxplot
pp <- grid.arrange(table_plot, p, ncol = 2)
print(pp)
ggsave("anova_sim1b.png", plot=pp, width = 10, height = 4)


### and now for the ANOVA
anova_result <- aov(Value ~ Group, data = df)
anova_result
















#-------- Figure 14.25: Parametric experiment on a one-way ANOVA ---------#
#-------------------------------------------------------------------------#
# Set up parameters
samplesizes <- seq(5, 150)
level_means <- c(0, 0.2, 0.4)
nLevels <- length(level_means)

# Initialize vector to store p-values
pvals <- numeric(length(samplesizes))

# Run the experiment
for (i in seq_along(samplesizes)) {
  N <- samplesizes[i]
  nDataRows <- N * nLevels
  
  # Create the group assignment
  group_column <- rep(seq_len(nLevels), each = N)
  
  # Create the data
  col_data <- numeric(nDataRows)
  for (j in seq_len(nLevels)) {
    col_data[group_column == j] <- rnorm(N, mean = level_means[j])
  }
  
  # Data frame
  df <- data.frame(Group = factor(group_column), Value = col_data)
  
  # Perform ANOVA
  anova_res <- aov(Value ~ Group, data = df)
  pvals[i] <- summary(anova_res)[[1]]["Pr(>F)"][[1]][1]
}

# Visualization
ggplot(data.frame(SampleSize = samplesizes, LogP = log(pvals)), aes(x = SampleSize, y = LogP)) +
  geom_point(color = "black", fill = "gray", shape = 22, size = 6) +
  geom_hline(yintercept = log(0.05), linetype = "dashed", color = "black") +
  labs(x = "Sample size", y = "log(p)") +
  theme_minimal() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

# Save the plot
ggsave("anova_sim1b_exp.png", width = 8, height = 4)
















#-------- Figure 14.26: Simulate data for a one-way repeated-measures ANOVA ---------#
#------------------------------------------------------------------------------------#
### create the dataset
# Set up parameters
level_means <- c(0, 0.1, 0.5)
samplesize <- 34
nLevels <- length(level_means)
nDataRows <- samplesize * nLevels

# Create columns for subject and group assignments
subject_column <- rep(1:samplesize, each = nLevels)
group_column <- rep(1:nLevels, times = samplesize)

# Create data column (initialize as zeros, then modulate by level_means)
col_data <- numeric(nDataRows)
for (i in 1:nLevels) {
  whichrows <- group_column == i
  cellMean <- level_means[i]
  col_data[whichrows] <- rnorm(sum(whichrows), mean = cellMean, sd = 1)
}

# Create a data frame
df <- data.frame(Subject = factor(subject_column),
                 Group = factor(group_column),
                 Value = col_data)

### visualize the data
# Create a formatted data frame for the table
dfd <- df
dfd$Subject <- formatC(dfd$Subject, format = "f", digits = 0)
dfd$Group <- formatC(dfd$Group, format = "f", digits = 0)
dfd$Value <- formatC(dfd$Value, format = "f", digits = 2)

# Create a table
table_df <- head(dfd, 9)
p_table <- tableGrob(table_df, rows=NULL)

# Create boxplots of data
p_boxplot <- ggplot(df, aes(x = as.factor(Group), y = Value)) +
  geom_boxplot() +
  labs(title = "B) Data box plots", x = "Group", y = "Value") +
  theme_minimal()

# Arrange the plots
pp <- grid.arrange(p_table, p_boxplot, ncol = 2)
print(pp)
ggsave('anova_sim1r.png', plot=pp, width = 10, height = 4)


### now run the one-way repeated measures ANOVA
anova_results <- aov(Value ~ Group + Error(Subject/Group), data = df)
summary(anova_results)















#-------- Figure 14.27: Simulate data for a two-way between-subjects ANOVA ---------#
#-----------------------------------------------------------------------------------#
### the data
# Set the number of subjects per group
n <- 30

# Define the population cell means for factor A (rows) and factor B (columns)
group_means <- matrix(c(1, 1, 1.5, 0.5,
                        1, 1, 0.5, 1.5), nrow = 2, byrow = TRUE)

# Get the dimensions of the group_means matrix
factA <- nrow(group_means)
factB <- ncol(group_means)

# Total number of data rows
nDataRows <- n * factA * factB

# Create the columns for factors A and B
colA <- rep(1:factA, each = n * factB)
colB <- rep(rep(1:factB, each = n), times = factA)

# Initialize the data column
col_data <- numeric(nDataRows)

# Populate the data based on group means
for (a in 1:factA) {
  for (b in 1:factB) {
    # Row selection
    whichrows <- (colA == a) & (colB == b)
    
    # Population cell mean
    cellMean <- group_means[a, b]
    
    # Random data for those rows
    col_data[whichrows] <- rnorm(sum(whichrows), mean = cellMean, sd = 1)
  }
}

# Create the dataframe
df <- data.frame(A = factor(colA), B = factor(colB), y = col_data)




### visualize the data
# Create a copy of the data for formatting
dfd <- df
dfd$A <- as.character(dfd$A)
dfd$B <- as.character(dfd$B)
dfd$y <- sprintf("%.2f", dfd$y)

# Select the first 11 rows for the table
table_data <- dfd[1:11, ]

# Create a table plot
p1 <- ggplot() +
  geom_text(data = table_data, aes(label = paste(A, B, y, sep = ", "), x = 1, y = rev(1:nrow(table_data)))) +
  theme_void() +
  theme(plot.margin = margin(5.5, 5.5, 5.5, 5.5)) +
  ggtitle(expression(bold("A)") ~ "Data format"))

# Create boxplots
p2 <- ggplot(df, aes(x = A, y = y, fill = B)) +
  geom_boxplot() +
  scale_fill_brewer(palette = "BuPu") +
  theme_minimal() +
  ggtitle(expression(bold("B)") ~ "Data box plots")) +
  theme(legend.title = element_blank())

# Arrange the plots side by side
pp <- grid.arrange(p1, p2, ncol = 2)
print(pp)
ggsave("anova_sim2b.png", plot = pp, width = 10, height = 4)



### run the ANOVA
anova_result <- aov(y ~ A*B, data = df)
summary(anova_result)


















#-------- Figure 14.28: Experiment: Interaction by standard deviation ---------#
#------------------------------------------------------------------------------#
# Define parameters
stdevs <- seq(2, 0.2, length.out = 43)
n <- 30

# Population cell means
group_means <- matrix(c(1, 1, 1.3, 0.7, 1, 1, 0.7, 1.3), nrow = 2, byrow = TRUE)
factA <- nrow(group_means)
factB <- ncol(group_means)
nDataRows <- n * factA * factB

# Initialize matrix to store p-values
intpvals <- matrix(NA, nrow = length(stdevs), ncol = 2)

# Run the experiment
for (i in seq_along(stdevs)) {
  std <- stdevs[i]
  
  # Generate data
  col_data <- numeric(nDataRows)
  for (a in 1:factA) {
    for (b in 1:factB) {
      whichrows <- ((1:nDataRows - 1) %% factA == (a - 1)) & ((1:nDataRows - 1) %/% factA %% factB == (b - 1))
      cellMean <- group_means[a, b]
      col_data <- col_data + rnorm(nDataRows, mean = cellMean, sd = std) * whichrows
    }
  }
  
  # Create dataframe
  df <- data.frame(
    A = factor(rep(1:factA, each = n * factB)),
    B = factor(rep(1:factB, times = n * factA)),
    y = col_data
  )
  
  # Perform ANOVA and store p-values
  anova_res <- aov(y ~ A*B, data = df)
  intpvals[i, ] <- summary(anova_res)[[1]][["Pr(>F)"]][2:3]
  
  # Store a sample dataframe for visualization
  if (i == round(length(stdevs) / 2)) {
    df2plot <- df
  }
}

# Visualization
p1 <- ggplot(df2plot, aes(x = A, y = y, fill = B)) +
  geom_bar(stat = "summary", fun = "mean", position = position_dodge(), color = "black") +
  geom_errorbar(stat = "summary", fun.data = mean_se, position = position_dodge(.9), width = 0.25) +
  theme_minimal() +
  ggtitle(sprintf("Bar plot of data (std=%.2f)", stdevs[length(stdevs) / 2]))

p2 <- ggplot(data.frame(std = stdevs, p_B = intpvals[, 1], p_interaction = intpvals[, 2]), aes(x = std)) +
  geom_point(aes(y = log(p_B), color = "Main effect of 'B'"), fill="white", shape = 21, size = 5) +  # Squares for 'Main effect of B'
  geom_point(aes(y = log(p_interaction), color = "Interaction"), fill="gray", shape = 22, size = 5) +  # Circles for 'Interaction'
  geom_point(data = subset(data.frame(std = stdevs, p_B = intpvals[, 1]), p_B < 0.05), 
             aes(y = log(p_B)), shape = 3, size = 3) +  # Plus signs for significant p_B
  geom_point(data = subset(data.frame(std = stdevs, p_interaction = intpvals[, 2]), p_interaction < 0.05), 
             aes(y = log(p_interaction)), shape = 3, size = 3) +  # Plus signs for significant p_interaction
  geom_hline(yintercept = log(0.05), linetype = "dashed", color = "black") +
  scale_color_manual(values = c("Main effect of 'B'" = "black", "Interaction" = "grey")) +
  theme_minimal() +
  labs(y = "log(p)", color = "Effect") +
  ggtitle("P-values")


# Combine plots
pp <- gridExtra::grid.arrange(p1, p2, ncol = 2)
ggsave('anova_sim2b_std.png', plot = pp, width = 11, height = 4)


















#-------- Figure 14.29: Two-way mixed-effects ANOVA ---------#
#------------------------------------------------------------#
### generate the data
# Subjects per group
n <- 30

# Population cell means
# "factor A" is the number of rows, "factor B" is the number of columns
# Factor B is repeated-measures; Factor A is between-subjects
group_means <- matrix(c(1.1, 2, 1.2,      2.2, 1.3, 2.5), nrow = 2, byrow = TRUE)

factA <- nrow(group_means)
factB <- ncol(group_means)
nDataRows <- n * factA * factB # Total rows in the dataset

# Create the column subject and group assignments
colA <- rep(1:factA, each = n * factB)
colB <- rep(1:factB, times = n * factA)
colS <- floor((seq_len(nDataRows) - 1) / factB)

# Column data
col_data <- numeric(nDataRows)
for (a in seq_len(factA)) {
  for (b in seq_len(factB)) {
    # Row selection
    whichrows <- (colA == a) & (colB == b)
    
    # Population cell mean
    cellMean <- group_means[a, b]
    
    # Random data for those rows
    col_data[whichrows] <- rnorm(sum(whichrows), mean = cellMean, sd = 1)
  }
}

# Create dataframe
df <- data.frame(
  A = factor(colA), # Between-subjects levels
  B = factor(colB), # Within-subjects level
  ID = factor(colS), # Subject ID (to know which data values are repeated)
  y = col_data
)

# show a bit of data
head(df)



### run the mixed-effects ANOVA
mixed_anova <- aov(y ~ A * B + Error(ID/B), data = df)
summary(mixed_anova)


### and now to visualize the data (without the data table this time)
p <- ggplot(df, aes(x = factor(A), y = y, fill = factor(B))) +
  geom_boxplot() +
  labs(title = "B) Data box plots", x = "A", y = "y") +
  theme_minimal() +
  scale_fill_brewer(palette = "BuPu")

print(p)
ggsave("anova_sim2w.png", plot = p, width = 10, height = 4)




















#-------- Exercise 1 ---------#
#-----------------------------#
# Define the raw data
elves  <- c(17, 20, 16, 22, 20, 12, 15, 23,  9, 22, 21, 19, 12    )
dwarfs <- c(15, 14, 15, 25, 19, 16, 20, 18, 18, 15, 18, 13, 14, 15)
trolls <- c(14, 16, 11, 17, 12, 13, 10, 12, 10, 18, 13, 14, 11, 20)

### Descriptive statistics
# Sample sizes
Nelves  <- length(elves)
Ndwarfs <- length(dwarfs)
Ntrolls <- length(trolls)

# Means
mean_elves  <- mean(elves)
mean_dwarfs <- mean(dwarfs)
mean_trolls <- mean(trolls)

# Standard errors
sem_elves  <- sd(elves) / sqrt(Nelves)
sem_dwarfs <- sd(dwarfs) / sqrt(Ndwarfs)
sem_trolls <- sd(trolls) / sqrt(Ntrolls)

# Print the results
cat("Elves: N =",  Nelves, ", Mean =",  mean_elves, ", SEM =",  sem_elves, "\n")
cat("Dwarfs: N =", Ndwarfs, ", Mean =", mean_dwarfs, ", SEM =", sem_dwarfs, "\n")
cat("Trolls: N =", Ntrolls, ", Mean =", mean_trolls, ", SEM =", sem_trolls, "\n")



### visualization
# Create a dataframe for plotting
data <- data.frame(
  Group = rep(c("Elves", "Dwarfs", "Trolls"), times = c(Nelves, Ndwarfs, Ntrolls)),
  SpellsPerMinute = c(elves, dwarfs, trolls)
)

# Calculate means and standard errors
group_summary <- data %>%
  group_by(Group) %>%
  summarise(
    Mean = mean(SpellsPerMinute),
    SEM = sd(SpellsPerMinute) / sqrt(n()),
    N = n()
  )

# Create the error bar plot
p <- ggplot(group_summary, aes(x = Group, y = Mean, group = Group)) +
  geom_bar(stat = "identity", fill = gray.colors(1)[1]) +
  geom_errorbar(aes(ymin = Mean - SEM, ymax = Mean + SEM), width = 0.2) +
  geom_text(aes(label = sprintf("Mean=%.1f\nN=%d", Mean, N)), size=8 ,y=10, vjust = 1) +
  labs(title = "Spell-casting speeds of elves, dwarfs, and trolls",
       y = "Number of spells per minute") +
  theme_minimal()

# Show the plot
print(p)

# Save the plot
ggsave("anova_magicalMeans.png", p, width = 8, height = 5)




### and now for the manual ANOVA calculation
# Stack the data into a single vector
all_data <- c(elves, dwarfs, trolls)

# Calculate the overall mean
total_mean <- mean(all_data)

# Calculate SS_Between
ss_between <- sum(sapply(list(elves, dwarfs, trolls), function(group) length(group) * (mean(group) - total_mean)^2))

# Calculate SS_Within
ss_within <- sum((elves - mean(elves))^2) + 
  sum((dwarfs - mean(dwarfs))^2) + 
  sum((trolls - mean(trolls))^2)

# Calculate SS Total
ss_total <- ss_between + ss_within

# Degrees of freedom
df_between <- 3 - 1
df_within <- length(all_data) - 3
df_total <- length(all_data) - 1

# Calculate MS_Between and MS_Within
ms_between <- ss_between / df_between
ms_within <- ss_within / df_within

# Calculate F statistic
f_stat <- ms_between / ms_within

# Calculate p-value
p_value <- pf(f_stat, df_between, df_within, lower.tail = FALSE)

# Print the ANOVA table
cat("Source\t| SS\t\t df\t MS\t F\t\t p-value\n")
cat(rep("-", 56), "\n")
cat(sprintf("Between\t| %.2f\t %d\t %.2f\t %.2f\t %.4f\n", ss_between, df_between, ms_between, f_stat, p_value))
cat(sprintf("Within\t| %.2f\t %d\t %.2f\n", ss_within, df_within, ms_within))
cat(sprintf("Total\t| %.2f\t %d\n", ss_total, df_total))


# Calculating effect sizes
eta2 <- ss_between / ss_total
omega2 <- (ss_between - df_between * ms_within) / (ss_total + ms_within)

# Printing the effect sizes
cat(sprintf("eta^2   = %.3f\n", eta2))
cat(sprintf("omega^2 = %.3f\n", omega2))


























#-------- Exercise 2 ---------#
#-----------------------------#
# Combine the data into one vector
data <- c(elves, dwarfs, trolls)

# Create group labels
group_labels <- c(rep('Elves', Nelves), rep('Dwarfs', Ndwarfs), rep('Trolls', Ntrolls))

# Create a DataFrame from the data
df <- data.frame(Spells = data, Creature = group_labels)

# Print every 6th row of the dataframe
df[seq(1, nrow(df), by = 6), ]


# Perform the one-way ANOVA
anova_result <- aov(Spells ~ Creature, data = df)
summary(anova_result)

# Perform Tukey's Honestly Significant Difference test
tukey_result <- TukeyHSD(anova_result)
tukey_result

# compute Hedges' g (in esvis library)
hedg_g(df,Spells ~ Creature)
















#-------- Exercise 3 ---------#
#-----------------------------#
### make some data
# Data parameters
mean1 <- 4
mean2 <- 6

# Samples per group
N1 <- 30
N2 <- 35

# Simulate the data
data1 <- rnorm(N1, mean1, 2)
data2 <- rnorm(N2, mean2, 2)

datacolumn <- c(data1, data2)

# Group labels
groups <- c(rep('1', N1), rep('2', N2))

# Convert to a dataframe
df <- data.frame(TheData = datacolumn, Group = groups)

# Print the dataframe
print(head(df))

### compare ANOVA with t-test
# Run the ANOVA
anova_result <- aov(TheData ~ Group, data = df)
anova_summary <- summary(anova_result)

# Extract F and p-value from ANOVA
F_value <- anova_summary[[1]][["F value"]][1]
p_value_anova <- anova_summary[[1]][["Pr(>F)"]][1]
df1 <- anova_summary[[1]][["Df"]][1]
df2 <- anova_summary[[1]][["Df"]][2]

# Run the t-test (must assume equal variance for t^2=F)
ttest_result <- t.test(TheData ~ Group, data = df, var.equal = TRUE)

# Print results
cat(sprintf("ANOVA: F(%d,%d) = %.3f, p = %.3f\n\n", df1, df2, F_value, p_value_anova))
cat(sprintf("T-test: t(%d) = %.2f, p = %.3f\n\n", ttest_result$parameter[[1]], ttest_result$statistic[[1]], ttest_result$p.value[[1]]))

# Compare t^2 to F-value
t_square <- ttest_result$statistic[[1]]^2
cat(sprintf("t^2 = %.3f\n", t_square))















#-------- Exercise 4 ---------#
#-----------------------------#
### Create the data
# Data parameters
N <- 20

# Simulate the data
data <- rnorm(3 * N, 0, 1)

# Replace the final two data points with outliers (fixed to 10)
data[(3 * N - 1):(3 * N)] <- 10

# Group labels
groups <- rep(1:3, each = N)

# Convert to a data frame
df <- data.frame(TheData = data, Group = as.factor(groups))

## run the ANOVA
anova_result <- aov(TheData ~ Group, data = df)
summary(anova_result)


### now for the experiment
N <- 50
nOutliers <- 3

# Experiment parameters
isSig <- 0  # counter for significant tests
nTests <- 300 # number of tests to simulate

# Run the experiment
for (i in 1:nTests) {
  
  # Simulate the data
  data <- rnorm(3 * N, 0, 1)
  data[(3 * N - nOutliers + 1):(3 * N)] <- rnorm(nOutliers, 10, 1)
  
  # Group labels
  groups <- rep(1:3, each = N)
  
  # Run an ANOVA
  df <- data.frame(TheData = data, Group = as.factor(groups))
  anova_result <- aov(TheData ~ Group, data = df)
  
  # Count if significant
  if (summary(anova_result)[[1]]$'Pr(>F)'[1] < 0.05) {
    isSig <- isSig + 1
  }
}

# Print the results
cat(sprintf("%d of %d tests (%.2f%%) had p<.05 with N=%d and %d outliers in group 3.\n", isSig, nTests, isSig * 100 / nTests, N, nOutliers))
















#-------- Exercise 5 ---------#
#-----------------------------#
# Here is one possible way to do it:
# 10 factors, each with only 1 sample, and one additional group with 20 samples.

# Numerator (between-group) df: (number of groups - 1) = (10+1 - 1) = 10
# Denominator (within-group) df: (total number of observations - number of groups) = (10 + 20 - 11) = 19
# So in this contrived example, the numerator df (10) is smaller than the denominator df (19).














#-------- Exercise 6 ---------#
#-----------------------------#
# Data parameters
N <- 10000

# Simulate the data
data1 <- rnorm(N, 0, 1)
data2 <- rnorm(N, 0.1, 1)
data <- c(data1, data2)

# Group labels
groups <- rep(1:2, each = N)

# Convert to a dataframe
df <- data.frame(TheData = data, Group = as.factor(groups))

# Run an ANOVA
anova_result <- aov(TheData ~ Group, data = df)
summary(anova_result)



### setup and run the experiment
# Sample size and experiment parameters
N <- 10000
nTests <- 300

# Initialize vectors to store results
pvals <- numeric(nTests)
eta2 <- numeric(nTests)

# Run the experiment
for (i in seq_len(nTests)) {
  
  # Simulate the data
  data1 <- rnorm(N, 0, 1)
  data2 <- rnorm(N, 0.01, 1)
  data <- c(data1, data2)
  groups <- factor(rep(1:2, each = N))
  
  # Create dataframe and run ANOVA
  df <- data.frame(TheData = data, Group = groups)
  anova_result <- aov(TheData ~ Group, data = df)
  
  # Store p-value and eta squared
  anova_summary <- summary(anova_result)
  pvals[i] <- anova_summary[[1]][["Pr(>F)"]][1]
  eta2[i] <- etaSquared(anova_result)[1, "eta.sq.part"] # from the lsr library
}

# Print the results
sig_tests <- sum(pvals < 0.05)
cat(sprintf('%d of %d tests (%.2f%%) had p < .05 with N = %d.\n', sig_tests, nTests, 100 * sig_tests / nTests, N))


### visualize the relationships
# Prepare the data for plotting
plot_data <- data.frame(
  Significance = ifelse(pvals < 0.05, "p < .05", "p > .05"),
  Pvals = pvals,
  Eta2 = eta2
)

# Plot 1: Effect sizes by significance
p1 <- ggplot(plot_data, aes(x = Significance, y = Eta2)) +
  geom_point(shape = 16, color = "black", fill = "grey70", size = 4, alpha = 0.5) +
  scale_x_discrete(limits = c("p > .05", "p < .05")) +
  ylab("Partial eta squared (%)") +
  ggtitle("Effect sizes by significance") +
  theme_minimal()

# Plot 2: Effect sizes by p-values
p2 <- ggplot(plot_data, aes(x = Pvals, y = Eta2)) +
  geom_point(shape = 16, color = "black", fill = "grey70", size = 4, alpha = 0.5) +
  xlab("P-values") +
  ylab("Partial eta squared (%)") +
  ggtitle("Effect sizes by p-values") +
  theme_minimal()

# Arrange the plots side by side
plot_grid <- cowplot::plot_grid(p1, p2, ncol = 2)

# Save the plot
print(plot_grid)
ggsave("anova_ex6.png", plot_grid, width = 10, height = 4)




### repeat for random sample size
# Experiment parameters
nTests <- 300
pvals <- numeric(nTests)
eta2 <- numeric(nTests)

# Running the experiment
for (i in 1:nTests) {
  # Random sample size
  N <- sample(10:10000, 1)
  
  # Simulate the data
  data1 <- rnorm(N, 0, 1)
  data2 <- rnorm(N, runif(1)^2, 1)
  data <- c(data1, data2)
  
  # Group labels
  groups <- factor(rep(1:2, each = N))
  
  # Create a DataFrame
  df <- data.frame(TheData = data, Group = groups)
  
  # Run an ANOVA
  anova_result <- aov(TheData ~ Group, data = df)
  anova_summary <- summary(anova_result)
  
  # Store p-value and partial eta squared
  pvals[i] <- anova_summary[[1]][["Pr(>F)"]][1]
  eta2[i] <- anova_summary[[1]][["Sum Sq"]][1] / sum(anova_summary[[1]][["Sum Sq"]])
}

# Convert eta squared to percentage
peta2 <- 100 * eta2

# report the results
significant_tests <- sum(pvals < 0.05)
cat(significant_tests, "of", nTests, "tests (", (significant_tests / nTests * 100), "%) had p < .05 with a random N.\n")


## and visualize that
# Creating the data frame for plotting
plot_data <- data.frame(
  pvals = pvals,
  peta2 = peta2,
  Significance = ifelse(pvals < 0.05, "p < .05", "p > .05")
)

# Plotting
p1 <- ggplot(plot_data, aes(x = Significance, y = peta2)) +
  geom_point(color = "black", fill = "grey", shape = 21, alpha = 0.5) +
  scale_x_discrete(labels = c("p > .05", "p < .05")) +
  labs(y = "Partial eta^2 (%)") +
  ggtitle(expression(bold("A) Effect sizes by significance")))

p2 <- ggplot(plot_data, aes(x = log(pvals), y = peta2)) +
  geom_point(color = "black", fill = "grey", shape = 21, alpha = 0.5) +
  labs(x = "log(p-values)", y = "Partial eta^2 (%)") +
  ggtitle(expression(bold("B) Effect sizes by p-values")))

# Arranging the plots side by side
pp <- grid.arrange(p1, p2, ncol = 2)

# Saving the plot
ggsave("anova_ex6b.png", plot = pp, width = 10, height = 4)























#-------- Exercise 7 ---------#
#-----------------------------#
# Simulate data
n_subjects <- 30
n_conditions <- 3
data <- matrix(rnorm(n_subjects * n_conditions), ncol = n_conditions)
data[, 2] <- data[, 2] + 0.25 # Small offset to measurement #2
data[, 3] <- data[, 3] + 0.5  # Small offset to measurement #3

# Create a DataFrame
df1 <- as.data.frame(data)
colnames(df1) <- c('Cond1', 'Cond2', 'Cond3')

# Convert to long format
df <- df1 %>%
  mutate(Subject = row.names(.)) %>%
  gather(key = "Condition", value = "Value", -Subject)

# Repeated-measures ANOVA
rmANOVA <- aov(Value ~ Condition + Error(Subject/Condition), data = df)
print("Results of a repeated-measures ANOVA:")
summary(rmANOVA)

# Between-subjects ANOVA
ANOVA <- aov(Value ~ Condition, data = df)
print("\n\nResults of a between-subjects ANOVA")
summary(ANOVA)



### now for the experiment
nReps <- 200
n_subjects <- 30
n_conditions <- 3

# Initialize a matrix to store p-values
pvals <- matrix(NA, nrow = nReps, ncol = 2)

# Start the experiment
for (i in 1:nReps) {
  
  # Generate the data
  data <- matrix(rnorm(n_subjects * n_conditions), ncol = n_conditions)
  data[, 2] <- data[, 2] + 0.25
  data[, 3] <- data[, 3] + 0.5
  
  # Convert data into DataFrame
  df1 <- as.data.frame(data)
  colnames(df1) <- c('Cond1', 'Cond2', 'Cond3')
  df <- df1 %>%
    mutate(Subject = row.names(.)) %>%
    gather(key = "Condition", value = "Value", -Subject)
  
  # Perform the ANOVAs
  rmANOVA <- aov(Value ~ Condition + Error(Subject/Condition), data = df)
  ANOVA   <- aov(Value ~ Condition, data = df)
  
  # Store the p-values
  pvals[i, 1] <- summary(rmANOVA)[[2]][[1]][["Pr(>F)"]][1] # p-value of repeated-measures ANOVA
  pvals[i, 2] <- summary(ANOVA)[[1]][["Pr(>F)"]][1]        # p-value of between-subjects ANOVA
}

# Display first few rows of p-values as a check
head(pvals)



### Visualizations
# Prepare data for plotting
pvals_df <- data.frame(
  TestNumber = 1:200,
  Repeated = pvals[, 1],
  Between = pvals[, 2]
)

# Prepare data for plotting (for scatter plot)
pvals_long <- reshape2::melt(pvals_df, id.vars = "TestNumber", variable.name = "TestType", value.name = "PValue")

# Plotting
p1 <- ggplot(pvals_long, aes(x = TestNumber, y = PValue, color = TestType)) +
  geom_point(shape = 21, size = 3, aes(fill = TestType)) +
  scale_fill_manual(values = c("Repeated" = "gray20", "Between" = "gray80")) +
  scale_color_manual(values = c("Repeated" = "black", "Between" = "black")) +
  theme_minimal() +
  labs(x = "Test number", y = "P-value") +
  ggtitle("A) P-values from both tests")

# Histogram of the differences
p2 <- ggplot(data.frame(Difference = pvals_df$Between - pvals_df$Repeated), aes(x = Difference)) +
  geom_histogram(bins = 30, fill = "gray50", color = "black") + xlim(-.5,.5) +
  theme_minimal() +
  labs(x = expression(paste("p", "between", " - ", "p", "repeated")), y = "Count") +
  ggtitle("B) Histogram of p-value differences")

# Arrange both plots together
pp <- grid.arrange(p1, p2, ncol = 2)

# Save the plot
ggsave("anova_ex7b.png", plot = pp, width = 10, height = 4)















#-------- Exercise 8 ---------#
#-----------------------------#

# Not much additional coding for this exercise. You just need to adapt the code from 
# Exercise 7 to replace this line:
data <- matrix(rnorm(n_subjects * n_conditions), ncol = n_conditions)
# with this line:
data <- matrix(rnorm(n_subjects * n_conditions), ncol = n_conditions) + replicate(3,1:n_subjects)
# This adds an integer-increasing offset to each row (each subject) of the data matrix


#### the code below is not part of the exercise but creates the figure I show in the text.


# Set parameters
n_subjects <- 30
n_conditions <- 3

# Generate data for Exercise 7
data1 <- matrix(rnorm(n_subjects * n_conditions), nrow = n_subjects, ncol = n_conditions)
data1[, 2] <- data1[, 2] + 0.25
data1[, 3] <- data1[, 3] + 0.5
data1_long <- as.data.frame(data1) %>%
  mutate(Subject = 1:nrow(.)) %>%
  pivot_longer(cols = -Subject, names_to = "Condition", values_to = "Value")

# Generate data for Exercise 8
data2 <- matrix(rnorm(n_subjects * n_conditions), nrow = n_subjects, ncol = n_conditions) + replicate(3,1:n_subjects)
data2[, 2] <- data2[, 2] + 0.25
data2[, 3] <- data2[, 3] + 0.5
data2_long <- as.data.frame(data2) %>%
  mutate(Subject = 1:nrow(.)) %>%
  pivot_longer(cols = -Subject, names_to = "Condition", values_to = "Value")



# Create the plot for ex.7 data
p1 <- ggplot(data1_long, aes(x = Condition, y = Value, group = Subject)) +
  geom_point(aes(color = Condition), size = 4) +
  scale_color_manual(values = c("black", "red", "blue")) +
  ggtitle(sprintf("A) Ex.7 data (std=%.1f)", sd(data1_long$Value))) +
  theme_minimal()

# Create a plot for ex.8 data
p2 <- ggplot(data2_long, aes(x = Condition, y = Value, group = Subject)) +
  geom_point(aes(color = Condition), size = 4) +
  scale_color_manual(values = c("black", "red", "blue")) +
  ggtitle(sprintf("B) Ex.8 data (std=%.1f)", sd(data2_long$Value))) +
  theme_minimal()

# Plot for differences
p3 <- ggplot() +
  geom_point(aes(x = rep(0, n_subjects), y = data1[,3] - data1[,1]), color = "black", size = 4) +
  geom_point(aes(x = rep(1, n_subjects), y = data2[,3] - data2[,1]), color = "black", size = 4) +
  scale_x_continuous(breaks = c(0,1), labels = c("Ex.7 data", "Ex.8 data")) +
  xlab('"2"-"0" diffs') + xlim(-.5,1.5) +
  ggtitle("C) Differences") +
  theme_minimal()

# Arrange plots
pp <- grid.arrange(p1, p2, p3, ncol = 3)

# Save the plot
ggsave("anova_ex8.png", plot = pp, width = 15, height = 5)






















#-------- Exercise 9 ---------#
#-----------------------------#
# Population cell means
group_means <- matrix(c(1, 1, 1.3, 0.7,
                        1, 1, 0.7, 1.3), nrow = 2, byrow = TRUE)

factA <- nrow(group_means)
factB <- ncol(group_means)

# Per-cell sample sizes
cellCounts <- sample(25:35, factA * factB, replace = TRUE)
nDataRows <- sum(cellCounts)  # Total rows in the dataset

# Initialize the data matrix
datamat <- matrix(nrow = nDataRows, ncol = 3)

# Fill the data matrix
rowidx <- 1
for (idx in 1:(factA*factB)) {
  a <- (idx - 1) %/% factB + 1
  b <- (idx - 1) %% factB + 1
  
  cellMean <- group_means[a, b]
  celldata <- rnorm(cellCounts[idx], mean = cellMean, sd = 1)
  
  datamat[rowidx:(rowidx + cellCounts[idx] - 1), ] <- cbind(a, b, celldata)
  rowidx <- rowidx + cellCounts[idx]
}

# Convert to dataframe
df <- as.data.frame(datamat)
names(df) <- c('A', 'B', 'val')

# Two-way ANOVAs with different SS types
fit <- aov(val ~ A * B, data = df) # this call is the same
ss_types <- c("I", "II", "III")
for (i in ss_types) {
  print(paste('Type-', i, ' ANOVA table:', sep = ''))
  
  # Type I is implemented in aov
  if (i=="I"){ print(summary(fit))  }
  # Types II and III can be specified using Anova()
  else{ print(Anova(fit, type = i)) }
  print('\n\n')
}


























#-------- Exercise 10 ---------#
#------------------------------#
# load the data
data(ToothGrowth) # puts dataframe 'ToothGrowth' into the workspace
df <- ToothGrowth

# Create the plot
p <- ggplot(df, aes(x = as.factor(dose), y = len, fill = supp)) +
  geom_boxplot(position = position_dodge(width = 0.8), color = "gray") +
  geom_jitter(aes(color = supp), position = position_jitterdodge(jitter.width = 0.2), size = 2, shape = 21, show.legend = FALSE) +
  scale_fill_manual(values = c("black", "gray")) +
  labs(x = "Vitamin C dose (mg/day)", y = "Tooth length (mm)") +
  theme_minimal()

# Save the plot
print(p)
ggsave("anova_ex10.png", p, width = 8, height = 5)


### Two-way ANOVA
# The IV columns need to be converted to 'factors'
df$supp <- factor(df$supp)
df$dose <- factor(df$dose)

# Perform the two-way ANOVA with specified contrasts
anova_result <- aov(len ~ supp * dose, data = df, contrasts = list(dose = "contr.sum"))
summary(anova_result)





















#-------- Exercise 11 ---------#
#------------------------------#
# Calculate the predicted values (mean for each group) and residuals
df <- df %>%
  group_by(dose, supp) %>%
  mutate(
    predictions = mean(len),  # Calculate mean length for each group (dose and supp)
    residuals = len - predictions  # Compute residuals
  ) %>%
  ungroup()  # Remove grouping

# Show a few rows (every 4th row)
df[seq(1, nrow(df), by = 4), ]







# Calculate the empirical correlation
cor_result <- cor.test(df$predictions, df$residuals)

# Create the scatter plot
p <- ggplot(df, aes(x = predictions, y = residuals)) +
  geom_point(color = "black", fill = "#CCCCCC", shape = 21, size = 4, alpha = 0.5) +
  xlab("Predicted length") +
  ylab("Residuals") +
  ggtitle(sprintf("Pearson r=%.4f, p=%.4f", cor_result$estimate, cor_result$p.value)) +
  theme_minimal()

# Print the plot
print(p)
ggsave("anova_ex11.png", plot = p, width = 8, height = 4)





#----
# Modern statistics: Intuition, Math, Python, R
## Mike X Cohen (sincxpress.com)
#### https://www.amazon.com/dp/B0CQRGWGLY
#### Code for chapter 18 (Biases)

# About this code file:
### This code file will reproduce most of the figures in this chapter 
### (some figures were made in Inkscape), and illustrate the statistical 
### concepts explained in the text. The point of providing the code is not 
### just for you to recreate the figures, but for you to modify, adapt, 
### explore, and experiment with the code.
###
### Solutions to all exercises are at the bottom of the file.


# import packages and define global settings
custom_theme <- theme_classic() + 
  theme(text = element_text(size = 20),        # font size 
        plot.title = element_text(hjust = 0))  # title location
theme_set(custom_theme)
savefig.dpi <- 300                             # output resolution


# Load necessary libraries
library(ggplot2)
library(plotly)
library(dplyr)
library(tidyr)
library(gridExtra)

# install.packages('reshape2')
library(reshape2)















#-------- Exercise 1 ---------#
#-----------------------------#
# generate random data, trim smallest values
N <- 30

# Create new data
data <- rnorm(N)

# Trim
dataTrim <- sort(data)[-(1:2)]  # Removing the smallest two values

# T-tests
ttestO <- t.test(data, mu = 0)
ttestT <- t.test(dataTrim, mu = 0)

# Report the results
cat(sprintf("Full: t(%d) = %.3f, p = %.3f\n", ttestO$parameter, ttestO$statistic, ttestO$p.value))
cat(sprintf("Trim: t(%d) = %.3f, p = %.3f\n", ttestT$parameter, ttestT$statistic, ttestT$p.value))





# Generate random data, trim smallest or extreme values

N <- 30
numreps <- 1000

pLessThan05 <- matrix(0, nrow = numreps, ncol = 3)
tValues <- matrix(0, nrow = numreps, ncol = 3)

for (expi in 1:numreps) {
  # Create new data
  data <- rnorm(N)
  
  # Trim
  dataTrimL <- sort(data)[-c(1:2)]       # Left side trimmed
  dataTrimB <- sort(data)[-c(1, N-1)]    # Both sides trimmed
  
  # T-tests
  ttestO <- t.test(data, mu = 0)         # O = original
  ttestL <- t.test(dataTrimL, mu = 0)    # L = left side trimmed
  ttestB <- t.test(dataTrimB, mu = 0)    # B = both sides trimmed
  
  # Store "significances"
  pLessThan05[expi, 1] <- ttestO$p.value < 0.05
  pLessThan05[expi, 2] <- ttestL$p.value < 0.05
  pLessThan05[expi, 3] <- ttestB$p.value < 0.05
  
  # Store t-values
  tValues[expi, 1] <- ttestO$statistic
  tValues[expi, 2] <- ttestL$statistic
  tValues[expi, 3] <- ttestB$statistic
}

# Report the output
cat(sprintf("   Without data trimming: %3d/%d with p<.05 (%5.2f%%)\n", sum(pLessThan05[, 1]), expi, 100 * mean(pLessThan05[, 1])))
cat(sprintf(" With symmetric trimming: %3d/%d with p<.05 (%5.2f%%)\n", sum(pLessThan05[, 3]), expi, 100 * mean(pLessThan05[, 3])))
cat(sprintf("With asymmetric trimming: %3d/%d with p<.05 (%5.2f%%)\n", sum(pLessThan05[, 2]), expi, 100 * mean(pLessThan05[, 2])))



# Visualize the change in t-values
p <- ggplot(as.data.frame(tValues), aes(x = V1)) +
  geom_point(aes(y = V3), color = "black", fill = "grey50", shape = 21, size = 3, alpha = 0.8, show.legend = FALSE) +
  geom_point(aes(y = V2), color = "black", fill = "grey80", shape = 21, size = 3, alpha = 0.8, show.legend = FALSE) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray") +
  scale_x_continuous(breaks = seq(-4, 4, by = 2)) +
  scale_y_continuous(breaks = seq(-4, 4, by = 2)) +
  labs(x = "Original t-values", y = "T-values after trimming") +
  theme_bw() +
  theme(legend.position = "none")

# Display the plot
print(p)

# Save the plot
ggsave("bias_ex1.png", plot=p, width=5, height=5, dpi=300)
















#-------- Exercise 2 ---------#
#-----------------------------#
# Generate some data in a data frame
df <- as.data.frame(matrix(rnorm(50*10), ncol = 10))
colnames(df) <- paste0("v", 0:9)

# Pearson correlation matrix
R <- cor(df)

# Mask the diagonal to ignore r=1
diag(R) <- 0

# Find indices of max pair
maxCor <- which(abs(R) == max(abs(R)), arr.ind = TRUE)
xi <- maxCor[1, "row"]
yi <- maxCor[1, "col"]

# Get p-value
pval <- cor.test(df[[xi]], df[[yi]])$p.value

# Scatter plot of the variables with the highest correlation
p <- ggplot(df, aes_string(x = names(df)[xi], y = names(df)[yi])) +
  geom_point(color = "grey80", size = 3, alpha = 0.7) +
  geom_point(shape = 1, size = 3, color = "black") +
  ggtitle(paste0("r = ", round(R[xi, yi], 2), ", p = ", round(pval, 3))) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))

# Display the plot
print(p)

# Save the plot
ggsave("bias_ex1.png", plot=p, width=6, height=5, dpi=300)







## -- optional final plot ----
# Convert R to data frames for visualization
R_df <- as.data.frame(R)

# Bonferroni correction [ formula is (M*(M-1))/2 ]
num_comparisons <- (ncol(df) * (ncol(df) - 1)) / 2
bonferroni_thresh <- 0.05 / num_comparisons

# Create a matrix of annotations
annot_array <- matrix("", nrow = ncol(R_df), ncol = ncol(R_df))

# Loop through all elements of the matrix and create a string to display
for (i in 1:nrow(R_df)) {
  for (j in 1:ncol(R_df)) {
    # Get the p-value and determine significance
    pval <- cor.test(df[[i]], df[[j]])$p.value
    significant <- pval < bonferroni_thresh
    
    # The string depends on the significance
    if (!significant) {
      # If non-significant, just the correlation coefficient
      annot_array[i, j] <- sprintf("%.2f", R_df[i, j])
    } else {
      # If significant, add an asterisk to the coefficient
      annot_array[i, j] <- sprintf("%.2f*", R_df[i, j])
    }
    
    # Don't need to report the diagonals
    if (i == j) {
      annot_array[i, j] <- ""
    }
  }
}


# Assuming R_df is your correlation matrix and annot_array contains the annotations

# Convert the annotations to a matrix (if it's not already)
annot_matrix <- matrix(annot_array, nrow=nrow(R_df), byrow=TRUE)

# Create the heatmap
fig <- plot_ly(z=matrix(R_df), x = colnames(R_df), y = rownames(R_df), type = "heatmap", colorscale = "RdBu", zmin = -0.4, zmax = 0.4)

# Add annotations
for (i in seq_len(nrow(R))) {
  for (j in seq_len(ncol(R))) {
    fig <- fig %>% add_annotations(
      x = colnames(R_df)[j],
      y = rownames(R_df)[i],
      text = annot_matrix[i, j],
      xref = "x",
      yref = "y",
      showarrow = FALSE,
      ax = 0,
      ay = 0,
      font = list(size = 25) 
    )
  }
}

# Show the plot
print(fig)






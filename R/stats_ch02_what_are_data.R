#----
# Modern statistics: Intuition, Math, Python, R
## Mike X Cohen (sincxpress.com)
#### https://www.amazon.com/dp/B0CQRGWGLY
#### Code for chapter 2 (What are data?)

# About this code file:
### This code file will reproduce most of the figures in this chapter 
### (some figures were made in Inkscape), and illustrate the statistical 
### concepts explained in the text. The point of providing the code is not 
### just for you to recreate the figures, but for you to modify, adapt, 
### explore, and experiment with the code.
###
### Solutions to all exercises are at the bottom of the file.
  



#-------- install and load libraries ---------#
#---------------------------------------------#
# install.packages("ggplot2")
library(ggplot2)
library(gridExtra)






#-------- Figure 2.5: Margin figure with noisy data ---------#
#------------------------------------------------------------#
# Generate data
n <- 30
x <- rnorm(n)
y1 <- x + rnorm(n) / 10
y2 <- x + rnorm(n)

# Perform linear regression
fit1 <- lm(y1 ~ x)
fit2 <- lm(y2 ~ x)

# Prepare data for plotting
data1 <- data.frame(x=x, y=y1, fitted = fitted(fit1))
data2 <- data.frame(x=x, y=y2, fitted = fitted(fit2))

# Create plots
p1 <- ggplot(data1, aes(x=x, y=y)) +
  geom_point(color = 'black', shape = 22, fill = 'white', size = 3) +
  geom_line(aes(y = fitted), color = 'gray') +
  ggtitle('Less noise') +
  theme_minimal() +
  theme(axis.title.x = element_blank(), axis.text.x = element_blank(), axis.ticks.x = element_blank(),
        axis.title.y = element_blank(), axis.text.y = element_blank(), axis.ticks.y = element_blank())

p2 <- ggplot(data2, aes(x=x, y=y)) +
  geom_point(color='black', shape=22, fill='white', size=3) +
  geom_line(aes(y=fitted), color='gray') +
  ggtitle('More noise') +
  theme_minimal() +
  theme(axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank(),
        axis.title.y=element_blank(), axis.text.y=element_blank(), axis.ticks.y=element_blank())

# Plot together
P = grid.arrange(p1,p2,ncol=1)

# Save plot
ggsave('whatR_noisyData.png', P, width=2, height=4)












#-------- Figure 2.5: Margin figure with outlier ---------#
#---------------------------------------------------------#
X <- rnorm(12)  # some random data
X[7] <- 2 * pi  # create one outlier

# Prepare data for plotting
data <- data.frame(index=1:12, value=X)

# Create plot
p <- ggplot(data, aes(x=index, y=value)) +
  geom_point(shape=21, color='black', fill='black', size=3) +  # Normal points
  geom_point(data=data[7,], aes(x=index, y=value), shape=21, color='black', size=3, fill='gray') +  # Outlier point
  scale_x_continuous(breaks=NULL) +  # No x-axis ticks
  scale_y_continuous(limits = c(min(X)-.6, max(X)+.6), breaks=NULL) +  # y-axis limits and no y-axis ticks
  xlab('Data index') +
  ylab('Data value')

# Plot
print(p)

# Save plot
ggsave('whatR_outlier.png', plot=p, width=4, height=2)




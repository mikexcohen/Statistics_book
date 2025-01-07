#----
# Modern statistics: Intuition, Math, Python, R
## Mike X Cohen (sincxpress.com)
#### https://www.amazon.com/dp/B0CQRGWGLY
#### Code for chapter 7 (Assess and improve data quality)

# About this code file:
### This code file will reproduce most of the figures in this chapter 
### (some figures were made in Inkscape), and illustrate the statistical 
### concepts explained in the text. The point of providing the code is not 
### just for you to recreate the figures, but for you to modify, adapt, 
### explore, and experiment with the code.
###
### Solutions to all exercises are at the bottom of the file.
# Thanks to Petteri Hyvarinen for help with the translation from Python.





# loading libraries
library(ggplot2) # for plots
library(grid) # for extra control of plots
library(egg) # for laying out subplots
library(tidyverse) # for easier data frame manipulation
library(latex2exp) # for plot label formatting
library(scales) # for scale limits & out-of-bounds handling
library(lsa) # for cosine similarity
library(MASS) # for multivariate random data

# set up global figure theme
theme_set(theme_bw(base_size = 14))
theme_update(panel.border= element_blank(), # remove border around plot area
             axis.line.x = element_line(), # add x axis line
             axis.line.y = element_line(), # add y axis line
             panel.grid = element_blank()) # by default don't plot background grid






#-------- Figure 12.1: Example of scatter plot showing correlated data ---------#
#-------------------------------------------------------------------------------#
# Number of sibling pairs
num_pairs = 40

# Simulate ratings for brothers
brothers_ratings = sample(1:10, num_pairs, replace=TRUE)

# Simulate correlated ratings for sisters based on brothers' ratings
noise = rnorm(num_pairs, 0, 2)  # some random noise
sisters_ratings = brothers_ratings + noise  # sister's ratings are brother's ratings plus some noise

# Ensure ratings are within bounds 1 and 10
sisters_ratings = squish(round(sisters_ratings), c(1, 10))

# correlation
r = cor(brothers_ratings, sisters_ratings)

# Create a scatter plot of the data
df_plot <- data.frame(brothers_ratings = brothers_ratings, sisters_ratings = sisters_ratings)
p <- ggplot(df_plot, aes(x=brothers_ratings, y=sisters_ratings)) + 
  geom_point(shape=22, color='black', fill=rgb(.7, .7, .7), size=7, stroke=1) +
  labs(x = "Brothers' ratings", 
       y="Sisters' ratings", 
       title=TeX(sprintf("Death metal preferences in siblings (r=%.2f)", r)) ) +
  scale_y_continuous(expand = expansion(mult=c(0.0, 0.0)), breaks=seq(0,10,2), limits = c(0,11)) +
  scale_x_continuous(expand = expansion(mult=c(0.0, 0.0)), breaks=seq(0,10,2), limits = c(0,11)) +
  theme(panel.grid.major = element_line(color = "grey92"))

ggsave("cor_death.png", width=6, height=5, units="in", dpi=300)
print(p)











#-------- Figure 12.2: Different correlation coefficients ---------#
#------------------------------------------------------------------#
# correlation values
rs <- c(1, .7, .2, 0, 0, 0, -.2, -.7, -1)

# sample size
N <- 188


# start the plotting!

plist <- list()
for (i in 1:length(rs)) {
  r <- rs[i]
  x <- rnorm(N)
  y <- x*r + rnorm(N)*sqrt(1-r^2)
  
  # exceptions for r = 0
  if (i == 4) {
    x = cos(seq(0, 2*pi - (2*pi/N), length.out = N))
    y = sin(seq(0, 2*pi - (2*pi/N), length.out = N))
  } else if (i == 5) {
    x = seq(-2, 2, length.out = N)
    y = x^2
  } else if (i == 6) {
    x = seq(-2, 2, length.out = floor(N/2))
    y = c(x, -x)
    x = c(x, x)
  }
  
  rho = cor(x, y)
  
  df_plot <- data.frame(x, y)
  p <- ggplot(df_plot, aes(x,y)) +
    geom_point(shape=21, size=4, fill=rgb(0.7, 0.7, 0.7), alpha=0.3) +
    labs(x="", y="", title=sprintf("r = %.2f", rho)) +
    theme(plot.title = element_text(hjust = 0.5))
  plist[[i]] <- p
}

grid.newpage()
g <- ggarrange(plots = plist, ncol = 3, draw = FALSE, newpage = FALSE)
grid.draw(g)

ggsave("cor_variousRs.png", plot = g, width=8.5, height=8, units="in", dpi=300)



















#-------- Figure 12.3: Same correlation, different slopes ---------#
#------------------------------------------------------------------#
N <- 100

# Dataset 1
x1 <- rnorm(N, 100, 10)
y1 <- 0.3*x1 + rnorm(N)*3
df1 <- data.frame(x = x1, y = y1)
model1 <- lm(y ~ x, df1)
intercept1 <- model1$coefficients[[1]]
slope1 <- model1$coefficients[[2]]
df1$pred <- intercept1 + slope1*x1
r1 <- cor(x1,y1)

# Make plot for dataset 1 and its regression line
xmin <- min(x1) -5
xmax <- max(x1) + 5
p1 <- ggplot(df1, aes(x,y)) +
  geom_point(shape=21, size=5, fill=rgb(0.3, 0.3, 0.3), alpha=0.3) + 
  geom_line(aes(y = pred), linewidth=1) +
  xlim(xmin, xmax) +
  labs(x='', y='', title=TeX(sprintf("\\bf{A}) Slope = %.2f, r = %.2f", slope1, r1)))


# Dataset 2
x2 <- rnorm(N, 10, 1) + mean(x1)
y2 <- 3*x2 + rnorm(N)*3
df2 <- data.frame(x = x2, y = y2)
model2 <- lm(y ~ x, df2)
intercept2 <- model2$coefficients[[1]]
slope2 <- model2$coefficients[[2]]
df2$pred <- intercept2 + slope2*x2
r2 <- cor(x2,y2)

# Make plot for dataset 2 and its regression line
p2 <- ggplot(df2, aes(x,y)) +
  geom_point(shape=21, size=5, fill=rgb(0.3, 0.3, 0.3), alpha=0.3) + 
  geom_line(aes(y = pred), linewidth=1) +
  xlim(xmin, xmax) +
  labs(x="", y="", title=TeX(sprintf("\\bf{B}) Slope = %.2f, r = %.2f", slope2, r2)))

# Draw the plot and save
grid.newpage()
g <- arrangeGrob(p1, p2, ncol=2)
grid.draw(g)
ggsave("cor_fitlineR.png", plot=g, width=10, height=4, units="in", dpi=300)















#-------- Creating correlated data ---------#
#-------------------------------------------#
# Method 1
x <- rnorm(40)
y <- x + rnorm(length(x))

r <- cor(x,y)

df_plot <- data.frame(x,y)
p <- ggplot(df_plot, aes(x,y)) + 
  geom_point(shape=15, size=5) + 
  labs(x="", y="", title=sprintf("r = %.2f", r))
print(p)

# Method 2
r = 0.4
x = rnorm(50)
y = rnorm(length(x))
y = x*r + y*sqrt(1-r^2)

rr <- cor(x,y)

df_plot <- data.frame(x,y)
p <- ggplot(df_plot, aes(x,y)) + 
  geom_point(shape=15, size=5) + 
  labs(x="", y="", title=sprintf("r = %.2f", rr))
print(p)

# Method 3: multivariate
C <- matrix(c(1,.4,.4,1), nrow=2, byrow=TRUE) # covariance matrix
X <- mvrnorm(n=50, mu=c(0,0), Sigma=C) # multivariate random normal
r = cor(X)

df_plot <- data.frame(x=X[,1], y=X[,2])
p <- ggplot(df_plot, aes(x,y)) + 
  geom_point(shape=15, size=5) + 
  labs(x="", y="", title=sprintf("r = %.2f", r[1,2]))
print(p)

# empirical correlations are pretty close to population param
print(r - C)


















#-------- Figure 12.6: Anscombe's quartet ---------#
#--------------------------------------------------#
plist <- list()
for (i in 1:4) {
  xvar <- as.name(sprintf("x%d", i))
  yvar <- as.name(sprintf("y%d", i))
  
  # Anscombe's quartet is by default available in R in variable 'anscombe'
  corr_p <- cor(anscombe[[xvar]], anscombe[[yvar]], method="pearson")
  corr_s <- cor(anscombe[[xvar]], anscombe[[yvar]], method="spearman")

  p <- ggplot(anscombe, aes(!!xvar, !!yvar)) + 
    geom_point(shape=21, size=5, fill=rgb(0.7, 0.7, 0.7)) +
    scale_y_continuous(expand = c(0.1, 0.1)) +
    labs(x="", y="", title=TeX(sprintf("$r_p$ = %.2f, $r_s$ = %.2f", corr_p, corr_s))) +
    theme(axis.ticks = element_blank(), 
          axis.text = element_blank(), 
          plot.title = element_text(hjust = 0.5))
    
  plist[[i]] = p
}

g <- arrangeGrob(grobs=plist, ncol=2)
grid.draw(g)
ggsave("cor_anscombe.png", plot = g, width = 8, height = 5, units = "in", dpi=300)



# Toy covariance example ----

# raw scores
h <- c(74, 63, 58, 70)
s <- c( 4,  7,  2,  9)
N <- length(h)

# demeaned
hd <- h - mean(h)
sd <- s - mean(s)

cov <- sum(hd * sd) / (N-1)

print(cov)
print(hd*sd)

















#-------- Figure 12.7: Kandall tau ---------#
#-------------------------------------------#
# The data
bro <- c(1, 2, 3, 4, 5)
sis <- c(2, 1, 4, 5, 3)

k <- cor(bro, sis, method = "kendall")

# band names (lol)
bands <- c("Unicorn Apocalypse",
           "Satan's Fluffy Bunnies",
           "Demonic Dishwashers",
           "Vampiric Vegetarians",
           "Zombie Zucchini Zephyr")


df_plot <- data.frame(bro, sis)
df_plot$bands <- bands
df_plot$band_letters <- substr(df_plot$bands, 1, 1)


p <- ggplot(df_plot, aes(x=bro, y=sis)) + 
  geom_point(shape = 22, size=10, fill=rgb(.9, .9, .9)) +
  geom_text(aes(label = band_letters), fontface = "bold") +
  labs(x="Brothers' ratings", y="Sisters' ratings", 
       title = TeX(sprintf("$\\tau$ = %.2f", k))) +
  theme(panel.grid.major = element_line("grey92"), 
        plot.title = element_text(hjust = 0.5))

tbl <- tableGrob(df_plot[order(-df_plot$bro), 1:3], 
                 cols = c("Bro", "Sis", "Band name"), 
                 rows = NULL)

grid.newpage()
g <- arrangeGrob(p, tbl, ncol = 2)
grid.draw(g)

















#-------- Figure 12.8: Statistical significance of r based on n ---------#
#------------------------------------------------------------------------#
# simulation ranges
rs <- seq(0.1, 0.8, length.out = 53)
ns <- seq(10, 511, by = 25)

# compute the matrix of t-values
rs_mat <- matrix(rep(rs, length(ns)), ncol = length(ns), byrow = FALSE)
ns_mat <- matrix(rep(ns, length(rs)), nrow = length(rs), byrow = TRUE)
num =  rs_mat * sqrt(ns_mat - 2)
den = sqrt(1 - rs_mat^2)
tmat = num / den

df_plot <- data.frame(x = c(ns_mat), y = c(rs_mat), z = c(tmat))
p <- ggplot(df_plot, aes(x, y)) +
  geom_raster(aes(fill = z)) +
  scale_fill_gradient(low="black", high="white", limits = c(2,10), oob = squish) +
  scale_x_continuous(expand=c(0,0)) + scale_y_continuous(expand = c(0,0)) +
  guides(fill = guide_colorbar(label.position = "top", 
                               frame.colour = "black", 
                               draw.ulim = FALSE, 
                               draw.llim = FALSE,
                               title = NULL,
                               barwidth = unit(0.7, "npc"))) +
  theme(panel.border = element_rect(fill = NA), legend.position = "top") +
  labs(x="Sample size", y="Correlation strength")
  
print(p)
ggsave("cor_tvals.png", width = 4, height = 6, units = "in", dpi = 300)


















#-------- Figure 12.9: Fisher-z transform on uniform data ---------#
#-------------------------------------------------------------------#
# "correlation" data and its Fisher transform
r <- runif(1000, -1, 1)
fish_r <- atanh(r)



# now for plotting
df_plot <- data.frame(r, fish_r)

df_long <- df_plot %>% 
  pivot_longer(c(r, fish_r), names_to = "var", values_to = "value") %>%
  mutate(var = factor(var, levels=c("fish_r", "r"), labels=c("F(r)","r")))


p1 <- ggplot(df_long, aes(x=value, fill=var)) + 
  geom_histogram(binwidth =  function(x) 2*IQR(x)/length(x)^(1/3), # FD
                 position="identity") +
  scale_fill_manual(values = c(rgb(0.3, 0.3, 0.3), rgb(0, 0, 0, 0.5))) +
  labs(x="Value", y="Frequency", 
       title = TeX("\\bf{A}) Histograms of r and F(r)")) +
  theme(legend.position = c(1, 0.8),
        legend.justification = "right",
        legend.title = element_blank())

p2 <- ggplot(df_plot, aes(x=r, y=fish_r)) +
  geom_point(shape=21, size = 5, fill = rgb(0.8, 0.8, 0.8), alpha = 0.5) +
  labs(x="r", y="F(r)", title = TeX("\\bf{B}) Scatter plot of r vs. F(r)"))

grid.newpage()
g <- arrangeGrob(p1, p2, ncol = 2)
grid.draw(g)

ggsave("cor_fisherFull.png", plot = g, width = 10, height = 4, units = "in", dpi = 300)













#-------- Figure 12.10: Fisher-z transform on numbers close to zero ---------#
#----------------------------------------------------------------------------#
# Same as above but with simulated H0 coefficients
X <- matrix(rnorm(100*45), ncol = 45)

# compute the correlation matrix and extract the unique r values
R <- cor(X)
r <- R[upper.tri(R, diag = FALSE)]

# fisher transform
fish_r <- atanh(r)

# now for plotting
df_plot <- data.frame(r, fish_r)

df_long <- df_plot %>% 
  pivot_longer(c(r, fish_r), names_to = "var", values_to = "value") %>%
  mutate(var = factor(var, levels=c("fish_r", "r"), labels=c("F(r)","r")))

p1 <- ggplot(df_long, aes(x=value, fill=var)) + 
  geom_histogram(binwidth =  function(x) 2*IQR(x)/length(x)^(1/3), # FD
                 position="identity") +
  scale_fill_manual(values = c(rgb(0.3, 0.3, 0.3), rgb(0, 0, 0, 0.5))) +
  labs(x="Value", y="Frequency", 
       title = TeX("\\bf{A}) Histograms of r and F(r)")) +
  theme(legend.position = c(1, 0.8),
        legend.justification = "right",
        legend.title = element_blank())

p2 <- ggplot(df_plot, aes(x=r, y=fish_r)) +
  geom_point(shape=21, size = 5, fill = rgb(0.8, 0.8, 0.8), alpha = 0.5) +
  labs(x="r", y="F(r)", title = TeX("\\bf{B}) Scatter plot of r vs. F(r)"))

grid.newpage()
g <- arrangeGrob(p1, p2, ncol = 2)
grid.draw(g)

ggsave("cor_fisherReal.png", plot = g, width = 10, height = 4, units = "in", dpi = 300)



















#-------- Figure 12.11: Subgroups paradox ---------#
#--------------------------------------------------#
# initializations
n <- 20 # sample points per group
offsets <- c(2, 3.5, 5) # mean offsets

groups <- 1:3
group_shapes <- 15:17
df <- data.frame(x = numeric(), 
                 y = numeric(), 
                 datai = numeric(), 
                 shape = numeric())

colors <- rep(NA, 3)
labels <- rep(NA, 3)
for (datai in groups) {
  
  # generate data
  x <- seq(offsets[datai] - 1, offsets[datai] + 1, length.out = n)
  y <- -x/3 + mean(x) + rnorm(n)/3
  
  # subgroup correlation
  stat <- cor.test(x,y, method="pearson")
  r <- stat$estimate[[1]]
  p <- stat$p.value
  
  df_sub <- data.frame(x = x, y = y, 
                       datai = datai, 
                       shape = group_shapes[datai])
  colors[datai] <- rgb((datai-1)/3, (datai-1)/3, (datai-1)/3)
  labels[datai] <- sprintf("r=%.2f, p=%.2f", r, p)
  df <- rbind(df, df_sub)
}

# correlation for full dataset
stat <- cor.test(~ x + y, df)
r <- stat$estimate[[1]]
p <- stat$p.value

# plot
df$shape <- factor(df$shape, labels=labels)

p <- ggplot(df, aes(x,y, shape=shape, color=shape)) +
  geom_point(size=5, alpha=0.7) +
  scale_color_manual(values = colors) +
  labs(x="", y="", title=sprintf("Total r=%.2f, p=%.3f", r, p)) +
  theme(legend.title = element_blank(),
        legend.position = c(0.02, 0.9),
        legend.justification = "left",
        plot.title = element_text(hjust = 0.5),
        axis.ticks = element_blank(),
        axis.text = element_blank())
  
print(p)

ggsave("cor_simpsons.png", width = 5, height = 6, units = "in", dpi = 300)

















#-------- Cosine similarity ---------#
#------------------------------------#
# variables
x <- c(1,2,3,4)
y <- c(1,2,3,4)
z <- c(101, 102, 103, 104)

# correlations
r_xy <- cor(x,y)
r_xz <- cor(x,z)

# cosine similarities
c_xy <- (x %*% y)/(norm(x,"2")*norm(y,"2"))
c_xz <- (x %*% z)/(norm(x,"2")*norm(z,"2"))

# print out the results
cat(sprintf("r_xy: %.3f\n", r_xy))
cat(sprintf("c_xy: %.3f\n", c_xy))
cat("\n")
cat(sprintf("r_xz: %.3f\n", r_xz))
cat(sprintf("c_xz: %.3f\n", c_xz))
















#-------- Exercise 1 ---------#
#-----------------------------#
# two random correlated variables
v <- rnorm(10)
w <- v + rnorm(length(v))

### correlation using mean-centered dot products
# mean-center
vm <- v - mean(v)
wm <- w - mean(w)

# dot products
r_me <- vm %*% wm / sqrt((vm %*% vm)*(wm %*% wm))

### correlation using R
r_R <- cor(v,w)

# print results
cat(sprintf("r from cor(): %.3f\n", r_R))
cat(sprintf("r from dot products: %.3f\n", r_me))

















#-------- Exercise 2 ---------#
#-----------------------------#
# Set parameters and create random data
N <- 43
r <- 0.4

x <- rnorm(N)
y <- rnorm(N)
y <- x*r + y*sqrt(1-r^2)

# Compute correlation and p-value using cor.test
cor_result <- cor.test(x, y, method = "pearson")

# Compute t/p-values manually
t <- cor_result$estimate * sqrt(N-2) / sqrt(1-cor_result$estimate^2)
p_man <- 2*pt(abs(t), df = N-2, lower.tail = FALSE)

# Print correlation values
cat(sprintf("r (p) from man.: %.4f (%.4f)\n", cor_result$estimate, cor_result$p.value))
cat(sprintf("r (p) from R   : %.4f (%.4f)\n", cor_result$estimate, p_man))



# next part...
### a function to calculate the p-values
getpvals <- function(x, y) {
  # Manual calculation
  r_np <- cor(x, y)
  t <- r_np * sqrt(length(x) - 2) / sqrt(1 - r_np^2)
  p_np <- 2 * pt(abs(t), df = length(x) - 2, lower.tail = FALSE)
  
  # Using cor.test
  test_result <- cor.test(x, y, method = "pearson")
  p_sp <- test_result$p.value
  
  return(c(p_np, p_sp))
}

# run the experiment!
# Range of correlation values
rvals <- seq(0, 0.99, length.out = 40)

# Results matrix
pvalues <- matrix(0, nrow = length(rvals), ncol = 2)

# Run the experiment
for (ri in 1:length(rvals)) {
  # Create the data
  x <- rnorm(44)
  y <- rnorm(44)
  y <- x * rvals[ri] + y * sqrt(1 - rvals[ri]^2)
  
  # Get the two p-values
  pvalues[ri, ] <- getpvals(x, y)
}

# Plot
p <- ggplot(data.frame(r = rvals, p_np = log(pvalues[, 1]), p_sp = log(pvalues[, 2])), aes(x = r)) +
  geom_point(aes(y = p_sp), shape = 1, color = "black", fill = "grey90", size = 3) +
  geom_point(aes(y = p_np), shape = "x", color = "black", fill = "black", size = 3) +
  labs(x = "Correlation value (r)", y = "log(p)") +
  theme_minimal() +
  theme(legend.position = "bottom") +
  scale_y_continuous(name = "log(p)", labels = scales::label_number())

# Print the plot
print(p)

# Save the plot
ggsave("cor_ex2.png", p, width = 8, height = 5)

















#-------- Exercise 3 ---------#
#-----------------------------#
# matrix of p-values

N <- 10000 # observations
M <- 15 # features

# data matrix
X <- matrix(rnorm(N*M), ncol = M)

# correlation matrix
R <- cor(X)
d <- dim(R)

# confirm that it's the right shape
cat(sprintf("Correlation matrix shape: %d x %d\n", d[1], d[2]))

# compute the t-values

Tnum <- R * sqrt(N - 2)
Tden <- sqrt(1 - R^2) + .Machine$double.eps # adding tiny number to avoid n/0

T <- Tnum / Tden

# compute the p-values
P <- 1 - pt(T, N-2)

# visualize all matrices
x <- matrix(rep(1:M, M), ncol = M, byrow = TRUE)
y <- matrix(rep(M:1, M), ncol = M, byrow = FALSE)

df_plot <- data.frame(x=c(x), y=c(y), R=c(R))

plist <- list()
plist[[1]] <- ggplot(df_plot, aes(x,y)) +
  geom_raster(aes(fill=R)) +
  scale_fill_gradient(low="black", high="white", limits = c(-0.1, 0.1), oob = squish) +
  labs(x="Features", y="Features", title=TeX("\\bf{A}) R matrix"))

df_plot <- data.frame(x=c(x), y=c(y), T=c(T))
plist[[2]] <- ggplot(df_plot, aes(x,y)) +
  geom_raster(aes(fill=T)) +
  scale_fill_gradient(low="black", high="white", limits = c(-2, 2), oob = squish) +
  labs(x="Features", y="Features", title=TeX("\\bf{B}) T matrix"))

df_plot <- data.frame(x=c(x), y=c(y), P=c(P))
plist[[3]] <- ggplot(df_plot, aes(x,y)) +
  geom_raster(aes(fill=P)) +
  scale_fill_gradient(low="black", high="white", limits = c(0, 0.05), oob = squish) +
  labs(x="Features", y="Features", title=TeX("\\bf{C}) P matrix"))


# properties common to all axes
glist <- lapply(plist, function(p) {
  p <- p + scale_x_continuous(expand=c(0,0)) + scale_y_continuous(expand = c(0,0)) +
    guides(fill = guide_colorbar(frame.colour = "black", 
                                 draw.ulim = FALSE, 
                                 draw.llim = FALSE,
                                 title = NULL,
                                 barheight = unit(0.3, "npc"))) +
    theme(panel.border = element_rect(fill = NA),
          axis.text = element_blank(),
          axis.ticks = element_blank(),
          aspect.ratio = 1)
})

g <- ggarrange(plots = glist, ncol = 3)

ggsave("cor_ex3.png", plot = g, width = 10, height = 5, units = "in", dpi = 300)



















#-------- Exercise 4 ---------#
#-----------------------------#

# create the sigma matrix
Sigma <- apply(X, 2, sd)
Sigma <- diag(Sigma)

# compute C from R
C_me <- Sigma %*% R %*% Sigma # from formula
C_r <- cov(X) # from R

# check for equality (these should all be TRUE)
print(abs(C_r - C_me) < 1e-16)


# Now compute R from C
invSigma <- 1/apply(X, 2, sd)
invSigma <- diag(invSigma)

R_me <- invSigma %*% C_r %*% invSigma

# check for equality (these should all be TRUE)
print(abs(R - R_me) < 1e-16)

















#-------- Exercise 5 ---------#
#-----------------------------#

# simulation parameters
N <- 1000 # observations
M <- 20 # features

numReps <- 30

# initialize data arrays
alldata <- array(0, dim = c(M, N))
corrmats <- array(0, dim = c(M, M, numReps + 1))

# "pure" data
covars <- seq(-1, 1, length.out = M)
dataOG <- covars %o% rnorm(N) # outer product

# random noise in each repetition
for (idx in 1:numReps) {
  
  # this run's data
  thisdata <- dataOG + 5*matrix(rnorm(M*N), ncol = N)
  
  # its correlation matrix
  corrmats[,,idx] <- cor(t(thisdata))
  
  # sum the data
  alldata <- alldata + thisdata
  
}

# correlation of data average
corrmats[,,numReps+1] <- cor(t(alldata))



### plotting
x <- matrix(rep(1:M, M), ncol = M, byrow = TRUE)
y <- matrix(rep(M:1, M), ncol = M, byrow = FALSE)

plist = list()
df_plot <- data.frame(x=c(x), y=c(y), z=c(covars %*% t(covars)))
plist[[1]] <- ggplot(df_plot, aes(x, y)) +
  geom_raster(aes(fill=z)) +
  labs(title=TeX("\\bf{A}) Ground truth")) +
  guides(fill = FALSE)

df_plot <- data.frame(x=c(x), y=c(y), z=c(apply(corrmats[,,1:numReps], c(1,2), mean)))
plist[[2]] <- ggplot(df_plot, aes(x, y)) +
  geom_raster(aes(fill=z)) +
  labs(title=TeX("\\bf{B}) Ave. of correlations")) +
  guides(fill = FALSE)

df_plot <- data.frame(x=c(x), y=c(y), z=c(corrmats[,,numReps+1]))
plist[[3]] <- ggplot(df_plot, aes(x, y)) +
  geom_raster(aes(fill=z)) +
  labs(title=TeX("\\bf{C}) Correlation of ave.")) +
  guides(fill = guide_colorbar(frame.colour = "black", 
                               draw.ulim = FALSE, 
                               draw.llim = FALSE,
                               title = NULL,
                               barheight = unit(0.5, "npc")))


# properties common to all axes
plist <- lapply(plist, function(p) {
  p <- p + 
    scale_fill_gradient(low="black", high="white", limits = c(-0.3, 0.3), oob = squish) +
    scale_x_continuous(expand=c(0,0)) + scale_y_continuous(expand=c(0,0)) +
    labs(x="Features", y="Features") +
    theme(panel.border = element_rect(fill = NA),
          axis.text = element_blank(),
          axis.ticks = element_blank(),
          aspect.ratio = 1)
})

g <- ggarrange(plots = plist, ncol = 3)

ggsave("cor_ex5.png", plot = g, width = 12, height = 5, units = "in", dpi = 300)




















#-------- Exercise 6 ---------#
#-----------------------------#
# re-initialize data arrays
alldata <- array(0, dim = c(M, N))
corrmats <- array(0, dim = c(M, M, numReps + 1))

### run the experiment
for (idx in 1:numReps) {
  
  # this run's data (only 'covars' is constant across repetitions)
  thisdata <- covars %o% rnorm(N) + matrix(rnorm(M*N), ncol = N)

  # its correlation matrix
  corrmats[,,idx] <- cor(t(thisdata))
  
  # sum the data
  alldata <- alldata + thisdata
  
}

# correlation of data average
corrmats[,,numReps+1] <- cor(t(alldata))


### plotting
x <- matrix(rep(1:M, M), ncol = M, byrow = TRUE)
y <- matrix(rep(M:1, M), ncol = M, byrow = FALSE)

plist = list()
df_plot <- data.frame(x=c(x), y=c(y), z=c(covars %*% t(covars)))
plist[[1]] <- ggplot(df_plot, aes(x, y)) +
  geom_raster(aes(fill=z)) +
  labs(title=TeX("\\bf{A}) Ground truth")) +
  guides(fill = FALSE)

df_plot <- data.frame(x=c(x), y=c(y), z=c(apply(corrmats[,,1:numReps], c(1,2), mean)))
plist[[2]] <- ggplot(df_plot, aes(x, y)) +
  geom_raster(aes(fill=z)) +
  labs(title=TeX("\\bf{B}) Ave. of correlations")) +
  guides(fill = FALSE)

df_plot <- data.frame(x=c(x), y=c(y), z=c(corrmats[,,numReps+1]))
plist[[3]] <- ggplot(df_plot, aes(x, y)) +
  geom_raster(aes(fill=z)) +
  labs(title=TeX("\\bf{C}) Correlation of ave.")) +
  guides(fill = guide_colorbar(frame.colour = "black", 
                               draw.ulim = FALSE, 
                               draw.llim = FALSE,
                               title = NULL,
                               barheight = unit(0.5, "npc")))


# properties common to all axes
plist <- lapply(plist, function(p) {
  p <- p + scale_fill_gradient(low="black", high="white", limits = c(-0.3, 0.3), oob = squish) +
    scale_x_continuous(expand=c(0,0)) + scale_y_continuous(expand=c(0,0)) +
    labs(x="Features", y="Features") +
    theme(panel.border = element_rect(fill = NA),
          axis.text = element_blank(),
          axis.ticks = element_blank(),
          aspect.ratio = 1)
})

g <- ggarrange(plots = plist, ncol = 3)

ggsave("cor_ex6.png", plot = g, width = 12, height = 5, units = "in", dpi = 300)



















#-------- Exercise 7 ---------#
#-----------------------------#
sampleSize <- 30
nSamples <- 23

corrs <- rep(0, nSamples)
tres <- c(0,0)

# loop over experiments
for (ni in 1:nSamples) {
  
  # create the data
  x <- rnorm(sampleSize)
  y <- rnorm(sampleSize)
  y <- x*0.1 + y*sqrt(1 - 0.1**2)
  
  # correlation
  corrs[ni] <- cor(x, y)
}

# now for a t-test on r values
tres[1] <- t.test(corrs)$statistic[[1]]
tres[2] <- t.test(atanh(corrs))$statistic[[1]]

# critical t-value
tCrit <- qt(1 - 0.05/2, sampleSize - 2)


cat(sprintf('t-value from "raw" coefficients   : %.4f\n', tres[1]))
cat(sprintf('t-value from Fisher-z coefficients: %.4f\n', tres[2]))
cat(sprintf('Critical t-value for p<0.05       : %.4f\n', tCrit))



# now for a wider range of r values
rs <- seq(0.01, 0.5, length.out = 21)
sampleSize <- 30
nSamples <- 23

corrs <- matrix(rep(0, length(rs)*nSamples), ncol = nSamples)
tres <- matrix(rep(0, length(rs)*2), ncol = 2)

# critical t (doesn't depend on the population r or sample size)
tCrit <- qt(1 - 0.05/2, sampleSize - 2)


# run the experiment!
for (ri in 1:length(rs)) {
  r <- rs[ri]
  
  # loop over experiments
  for (ni in 1:nSamples) {
    
    # create the data
    x <- rnorm(sampleSize)
    y <- rnorm(sampleSize)
    y <- x*r + y*sqrt(1 - r^2)
    
    # correlation
    corrs[ri, ni] <- cor(x, y)
  }
  
  # now for a t-test on r values
  tres[ri, 1] <- t.test(corrs[ri,])$statistic[[1]]
  tres[ri, 2] <- t.test(atanh(corrs[ri,]))$statistic[[1]]
}



## plot
df_plot <- data.frame(x = rs, y = apply(corrs, 1, mean))

p1 <- ggplot(df_plot, aes(x, y)) +
  geom_segment(aes(x=rs[1], y=rs[1], xend=rs[length(rs)], yend=rs[length(rs)]),
               color=rgb(0.8, 0.8, 0.8),
               linetype = "dashed", linewidth = 1.0) +
  geom_point(shape = 22, fill=rgb(0.6, 0.6, 0.6), size=5) +
  labs(x="Theoretical correlation", y="Empirical correlation",
       title=TeX("\\bf{A}) Theoretical vs. empirical r"))


df_plot <- data.frame(x = rs, raw_t = tres[,1], fisher_z = tres[,2]) %>%
  pivot_longer(c(raw_t, fisher_z), names_to = "statistic", values_to = "value")
df_plot$statistic = factor(df_plot$statistic, levels=c("raw_t", "fisher_z"), labels=c('"Raw"', "Fisher-z"))

p2 <- ggplot(df_plot, aes(x, y=value, shape=statistic, fill=statistic)) +
  geom_hline(yintercept = tCrit, color=rgb(0.8, 0.8, 0.8),
             linetype = "dashed", linewidth = 1.0,
             show.legend = TRUE) +
  geom_point(size = 5) +
  scale_shape_manual(values = c(22,21)) +
  scale_fill_manual(values = c(rgb(0.6, 0.6, 0.6), rgb(0.3, 0.3, 0.3))) +
  annotate("text", x=0.48, y=tCrit + 1, label = "Critical t") +
  labs(x="Theoretical correlation", y="T values",
       title=TeX("\\bf{B}) T-values ($r's \\neq 0$)")) +
  theme(legend.position = c(0.1, 0.8),
        legend.justification = "left",
        legend.title = element_blank())

g <- ggarrange(p1, p2, ncol=2)

ggsave("cor_ex7.png", plot = g, width = 10, height = 4, units = "in", dpi = 300)






















#-------- Exercise 8 ---------#
#-----------------------------#
# Generate some correlated random data
x <- rnorm(40)
y <- x + rnorm(length(x))

# Manual cosine similarity
cs_num <- sum(x * y)
cs_den <- sqrt(sum(x^2)) * sqrt(sum(y^2))
cs_me <- cs_num / cs_den

# Using the cosine similarity function
cs_r <- cosine(x, y)

# Print results
cat(sprintf("Manual result: %.3f\n", cs_me))
cat(sprintf("R cosine fun.: %.3f\n", cs_r))



















#-------- Exercise 9 ---------#
#-----------------------------#
# range of requested correlation coefficients
rs <- seq(-1, 1, length.out = 100)

# sample size
N <- 500

# initialize output matrix
corrs <- matrix(rep(0, length(rs)*2), ncol = 2)



# loop over a range of r values
for (ri in 1:length(rs)) {
  
  # generate data
  x <- rnorm(N)
  y <- x*rs[ri] + rnorm(N)*sqrt(1 - rs[ri]^2)
  
  # mean de-centering
  x <- x - 10
  
  # compute correlation
  corrs[ri,1] <- cor(x,y)
  
  # compute cosine similarity
  corrs[ri,2] <- (x %*% y)/(norm(x,"2")*norm(y,"2"))
  
}



## visualize the results
df_plot <- data.frame(x = rs, cor = corrs[,1], cosim = corrs[,2]) %>%
  pivot_longer(c(cor, cosim), names_to = "measure", values_to = "value")
df_plot$measure = factor(df_plot$measure, levels=c("cor", "cosim"), labels=c("Correlation", "Cosine sim."))

p1 <- ggplot(df_plot, aes(x, y=value)) +
  geom_point(aes(shape=measure, fill=measure), size=5, alpha=0.5) +
  scale_shape_manual(values = c(22, 21)) +
  scale_fill_manual(values = c(rgb(0.5, 0.5, 0.5), rgb(0.9, 0.9, 0.9))) +
  theme(legend.position = c(0.1, 0.8),
        legend.justification = "left",
        legend.title = element_blank()) +
  labs(x="Requested correlation", y=TeX("Empirical $r$ or $S_C$"),
       title=TeX("\\bf{A}) Correlation and cosine sim."))


df_plot <- data.frame(x=corrs[,1], y=corrs[,2])

p2 <- ggplot(df_plot, aes(x,y)) +
  geom_hline(yintercept = 0, color = "gray", linetype = "dashed", linewidth = 1.0) +
  geom_vline(xintercept = 0, color = "gray", linetype = "dashed", linewidth = 1.0) +
  geom_point(shape=22, size=5, fill = rgb(0.2, 0.2, 0.2), alpha=0.5) +
  labs(x="Correlation", y="Cosine similarity", title=TeX(sprintf("\\bf{B}) r=%.2f", cor(corrs)[1,2])))


g <- ggarrange(p1, p2, ncol = 2)
ggsave("cor_ex9.png", plot = g, width = 10, height = 4.5, units = "in", dpi = 300)






















#-------- Exercise 10 ---------#
#------------------------------#
# import the data
url = "https://archive.ics.uci.edu/ml/machine-learning-databases/auto-mpg/auto-mpg.data"
column_names = c('MPG','Cylinders','Displacement','Horsepower','Weight','Acceleration','Model Year','Origin','Car Name')

data <- as_tibble(read.table(url, na.strings = "?"))
colnames(data) <- column_names
print(head(data))



# examine distributions

# include only numerical variables
data_numerical <- data %>% select(-c(`Car Name`, Origin))

# draw histograms
p <- data_numerical %>% gather("variable", "value", MPG:`Model Year`, factor_key = TRUE) %>%
  ggplot(aes(x=value)) +
  geom_histogram(fill = rgb(0.7, 0.7, 0.7), color = "grey20") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.2))) +
  facet_wrap(. ~ variable, ncol = 4, scales="free", strip.position = "bottom") +
  theme(strip.background = element_blank(), strip.placement = "outside") +
  labs(x="", y="Count")

print(p)


# Create a correlation matrix
R <- cor(data_numerical, use = "complete.obs", method = "spearman")

N <- ncol(data_numerical)
x <- matrix(rep(1:N, N), ncol=N, byrow = TRUE)
y <- matrix(rep(N:1, N), ncol=N, byrow = FALSE)

df_plot <- as_tibble(data.frame(x = c(x), y = c(y), corr = c(R))) %>%
  mutate(label = sprintf("%.2f", corr))

p <- ggplot(df_plot, aes(x, y)) + 
  geom_raster(aes(fill = corr)) +
  scale_fill_gradient2(low = "darkblue", high="darkred", mid = "gray90", limits = c(-1, 1)) +
  geom_text(aes(label=label), color="white") +
  scale_y_continuous(breaks=1:N, labels=colnames(R)[N:1], expand=c(0,0)) +
  scale_x_continuous(breaks=1:N, labels=colnames(R), expand=c(0,0)) +
  guides(fill = guide_colorbar(frame.colour = "black", 
                               draw.ulim = FALSE, 
                               draw.llim = FALSE,
                               title = NULL,
                               barheight = unit(0.5, "npc"))) +
  theme(axis.line.x = element_blank(),
        axis.line.y = element_blank(),
        aspect.ratio = 1,
        axis.text.x.bottom = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
  labs(x="", y="", title=expression(bold("Correlation matrix (Spearman)")))

print(p)
ggsave("cor_ex10c.png", plot = p, width = 8, height = 6, units = "in", dpi = 300)



















#-------- Exercise 11 ---------#
#------------------------------#
# Calculate R and P matrices from Spearman correlation

# R's cor.test accepts only two vectors x and y. To get a correlation
# matrix, we need to do a bit of pivoting
df_nested <- data_numerical %>%
  pivot_longer(cols = everything(), names_to = "var", values_to = "value") %>%
  group_by(var) %>%
  nest() %>% ungroup()
df_nested$var = factor(df_nested$var, levels=colnames(data_numerical))

# get all pairs and calculate cor.test for each pair
df_pairwise <- df_nested %>% expand(nesting(var1 = var, data1 = data), nesting(var2 = var, data2 = data)) %>%
  mutate(test = map2(data1, data2, function(x,y) cor.test(x$value, y$value, method="spearman")),
         p = map_dbl(test, "p.value"),
         r = map_dbl(test, "estimate"))

# get from long format dataframe to a matrix
# p-values:
P <- df_pairwise %>% pivot_wider(id_cols = var1, names_from = var2, values_from = p) %>%
  column_to_rownames("var1")
# Spearman correlation coefficients:
R <- df_pairwise %>% pivot_wider(id_cols = var1, names_from = var2, values_from = r) %>%
  column_to_rownames("var1")

# Bonferroni correction [ formula is (M*(M-1))/2 ]
num_comparisons <- (dim(data_numerical)[2]*(dim(data_numerical)[2]-1)) / 2
bonferroni_thresh <- 0.05 / num_comparisons
significant <- P < bonferroni_thresh


# plot
N <- ncol(data_numerical)
x <- matrix(rep(1:N, N), ncol=N, byrow = TRUE)
y <- matrix(rep(N:1, N), ncol=N, byrow = FALSE)

# build dataframe, also add labels if not on the diagonal
df_plot <- as_tibble(data.frame(x = c(x), y = c(y), corr = c(as.matrix(R)), sig = c(significant))) %>%
  mutate(label = if_else(x == (7-y+1), # if we are on the diagonal
                         "",
                         paste0(sprintf("%.2f", corr), if_else(sig, "*", "") ) ))


p <- ggplot(df_plot, aes(x, y)) + 
  geom_raster(aes(fill = corr)) +
  scale_fill_gradient2(low = "darkblue", high="darkred", mid = "gray90", limits = c(-1, 1)) +
  geom_text(aes(label=label), color="white") +
  scale_y_continuous(breaks=1:N, labels=colnames(R)[N:1], expand=c(0,0)) +
  scale_x_continuous(breaks=1:N, labels=colnames(R), expand=c(0,0)) +
  guides(fill = guide_colorbar(frame.colour = "black", 
                               draw.ulim = FALSE, 
                               draw.llim = FALSE,
                               title = NULL,
                               barheight = unit(0.5, "npc"))) +
  theme(axis.line.x = element_blank(),
        axis.line.y = element_blank(),
        aspect.ratio = 1,
        axis.text.x.bottom = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
  labs(x="", y="", title=expression(bold("Correlation matrix (*p<.05 corrected)")))

print(p)
ggsave("cor_ex11.png", plot = p, width = 8, height = 6, units = "in", dpi = 300)





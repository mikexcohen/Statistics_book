#----
# Modern statistics: Intuition, Math, Python, R
## Mike X Cohen (sincxpress.com)
#### https://www.amazon.com/dp/B0CQRGWGLY
#### Code for chapter 15 (Regression)

# About this code file:
### This code file will reproduce most of the figures in this chapter 
### (some figures were made in Inkscape), and illustrate the statistical 
### concepts explained in the text. The point of providing the code is not 
### just for you to recreate the figures, but for you to modify, adapt, 
### explore, and experiment with the code.
###
### Solutions to all exercises are at the bottom of the file.
# Thanks to Dorus Wang for help with the translation from Python.




# Load libraries
library(cowplot)
library(ggplot2)
library(ggpubr)
library(polynom)
library(glue)
library(viridis)
library(plot3D)
library(broom)
library(MASS)
library(nortest)
library(scales)
library(GGally)
library(httr)
library(readxl)
library(matlib)










#-------- Figure 15.1: Geometric view of regression ---------#
#------------------------------------------------------------#
# some data
N <- 9
x <- seq(-1, 4, length=N)
y <- 1 + x + rnorm(N)

# get GLM predictions
mdl <- lm(y ~ x, data=data.frame(x, y))
yHat <- predict(mdl, data.frame(x))



fig <- ggplot(data.frame(x), aes(x=x)) +
  geom_point(aes(y=y, fill="1_obs", linetype="1_obs", shape="1_obs"), size=7, alpha=.5, show.legend=FALSE) +
  labs(x="X (regressor)", y="Y (DV)") +
  theme_classic() +
  theme(panel.grid.major=element_line(color="gray"))

# plot the regression line
fig <- fig + geom_line(aes(y=yHat, fill="2_reg", linetype="2_reg", shape="2_reg"), lwd=1)

# plot the intercept
intpnt <- predict(mdl, data.frame(x=0, y=1))
fig <- fig + geom_point(aes(x=0, y=intpnt, fill="3_icpt", linetype="3_icpt", shape="3_icpt"), size=3)

# data-point-specific projection lines
for (i in 1:N) {
  df <- data.frame(x=c(x[i], x[i]), y=c(y[i], yHat[i]))
  fig <- fig + geom_segment(data=df, aes(x=x[1], xend=x[2], y=y[1], yend=y[2]), 
                            linetype="dashed", lwd=.75, color="gray55") +
    geom_point(data=df, aes(x=x[1], y=y[1]), shape=21, color="gray55", fill="gray55", size=1) +
    geom_point(data=df, aes(x=x[2], y=y[2], fill="4_pred", linetype="4_pred", shape="4_pred"), size=5)
}

s <- c(bquote(paste("Observations (", italic(y), ")")), "Regression line", "Intercept",
       bquote(paste("Predicted (", hat(italic(y)), ")")))
fig <- fig + scale_fill_manual(name="Legend", labels=s, values=c("gray80", NA, "white", "gray40")) +
  scale_linetype_manual(name="Legend", labels=s, values=c(NA, "solid", NA, NA)) +
  scale_shape_manual(name="Legend", labels=s, values=c(21, NA, 25, 22)) +
  guides(shape=guide_legend(override.aes=list(size=c(5, 5, 2.5, 4)))) +
  theme(legend.title=element_blank(),
        legend.position=c(.125, .825),
        legend.text.align=0,
        legend.background=element_blank(),
        legend.box.background=element_rect(color="gray", fill=alpha("white", .75)),
        legend.box.margin=margin(-7, 2, -2, 2))

print(fig)
ggsave(filename="reg_picOfReg.png", width=8, height=5, units="in", dpi=300)






























#-------- Figure 15.2: Regression vs. PCA ---------#
#--------------------------------------------------#
# some data
N <- 10
x <- seq(-1.5, 1.5, length=N)
y <- x + rnorm(N)

# mean-center variables
y <- y - mean(y)
maxval <- max(abs(y)) * 1.1  # for axis scaling; PCA projections look orthogonal in square axes

# get GLM predictions
mdl <- lm(y ~ x, data=data.frame(x, y))
yHat <- predict(mdl, data.frame(x))

# compute PCA
data <- data.frame(x, y)
C <- cov(data)
evals <- eigen(C)$values
evecs <- eigen(C)$vectors
PC <- evecs[, which.max(evals)]

# projection points
pcaX <- numeric(N)
pcaY <- numeric(N)

## plot

# plot the data
fig_A <- ggplot(data.frame(x=x), aes(x=x)) +
           scale_x_continuous(limits=c(-maxval, maxval)) +
           scale_y_continuous(limits=c(-maxval, maxval)) +
           theme_classic()

fig_B <- fig_A

# plot the regression line
fig_A <- fig_A + geom_line(aes(y=yHat, color="2_pred", fill="2_pred", linetype="2_pred", shape="2_pred"), lwd=1)

# data-point-specific projection lines
for (i in 1:N) {
  df <- data.frame(x=c(x[i], x[i]), y=c(y[i], yHat[i]))
  fig_A <- fig_A + geom_segment(data=df, aes(x=x[1], xend=x[2], y=y[1], yend=y[2], 
                                color="3_err", fill="3_err", linetype="3_err", shape="3_err"), lwd=.75) +
             geom_point(data=df, aes(x=x[1], y=y[1], color="3_err", fill="3_err", linetype="3_err", shape="3_err"), size=1) +
             geom_point(aes(y=yHat, color="2_pred", fill="2_pred", linetype="2_pred", shape="2_pred"), size=5)
             
  mul <- c(as.matrix(data[i, ]) %*% PC) * PC
  pcaX[i] <- mul[1]
  pcaY[i] <- mul[2]
  df <- data.frame(x=c(x[i], pcaX[i]), y=c(y[i], pcaY[i]))
  fig_B <- fig_B + geom_segment(data=df, aes(x=x[1], xend=x[2], y=y[1], yend=y[2]),
                                color="gray70", linetype="dashed", lwd=.75) +
             geom_point(data=df, aes(x=x[1], y=y[1]), color="gray55", fill="gray55", shape=21, size=1)
}

# now plot the PCA line
df <- data.frame(x=pcaX, y=pcaY)
fig_B <- fig_B + geom_line(data=df, aes(x=x, y=y), lwd=1) +
           geom_point(data=df, aes(x=x, y=y), fill="gray40", shape=22, size=5)

fig_A <- fig_A + geom_point(aes(y=y, color="1_obs", fill="1_obs", linetype="1_obs", shape="1_obs"), size=7, alpha=.5) +
           labs(x="X", y="Y", title=bquote(paste(bold(A), ") Errors to minimize in regression")))

fig_B <- fig_B + geom_point(aes(y=y), fill="gray80", shape=21, size=7, alpha=.5) +
           labs(x="X", y="Y", title=bquote(paste(bold(B), ") Errors to minimize in PCA")))

s <- c("Observations", "Predictions", "Errors")
fig_A <- fig_A + scale_color_manual(name="Legend", labels=s, values=c(NA, "black", "gray70")) + 
           scale_fill_manual(name="Legend", labels=s, values=c("gray80", "gray40", "gray70")) +
           scale_linetype_manual(name="Legend", labels=s, values=c(NA, "solid", "dashed")) +
           scale_shape_manual(name="Legend", labels=s, values=c(21, 22, 21)) +
           guides(shape=guide_legend(override.aes=list(size=c(4, 3, .5))),
                  linetype=guide_legend(override.aes=list(lwd=c(NA, .3, .3)))) +
           theme(legend.title=element_blank(),
                 legend.position=c(.165, .87),
                 legend.key.width=unit(2, "line"),
                 legend.text.align=0,
                 legend.background=element_blank(),
                 legend.box.background=element_rect(color="gray"),
                 legend.box.margin=margin(-7, 2, -2, 2))

fig <- plot_grid(fig_A, fig_B, nrow=1)

print(fig)
ggsave(filename="reg_regVpca.png", width=10, height=5, units="in", dpi=300)





####################
# All in one plot (a bit confusing to look at...)

# plot the data
fig <- ggplot(data.frame(x=x), aes(x=x)) +
         geom_point(aes(y=y, color="1_obs", fill="1_obs", linetype="1_obs", shape="1_obs"), size=7, alpha=.5) +
         scale_x_continuous(limits=c(-maxval, maxval)) +
         scale_y_continuous(limits=c(-maxval, maxval), breaks=seq(-2, 2, .5)) +
         theme_classic()

# plot the regression line
fig <- fig + geom_line(aes(y=yHat), lwd=1) +
         geom_point(aes(y=yHat, color="2_reg", fill="2_reg", linetype="2_reg", shape="2_reg"), size=5)

# data-point-specific projection lines
for (i in 1:N) {
  df <- data.frame(x=c(x[i], x[i]), y=c(y[i], yHat[i]))
  fig <- fig + geom_segment(data=df, aes(x=x[1], xend=x[2], y=y[1], yend=y[2]), 
                            lwd=.75, color="gray", linetype="dashed") +
           geom_point(aes(y=yHat), fill="gray40", shape=22, size=5) +
           geom_point(data=df, aes(x=x[1], y=y[1]), color="gray55", fill="gray55", shape=21, size=1)
  
  mul <- c(as.matrix(data[i, ]) %*% PC) * PC
  pcaX[i] <- mul[1]
  pcaY[i] <- mul[2]
  df <- data.frame(x=c(x[i], pcaX[i]), y=c(y[i], pcaY[i]))
  fig <- fig + geom_segment(data=df, aes(x=x[1], xend=x[2], y=y[1], yend=y[2]),
                            color="darkgreen", linetype="dashed", lwd=.75) +
           geom_point(data=df, aes(x=x[1], y=y[1]), color="gray55", fill="darkgreen", shape=21, size=1)
}

# now plot the PCA line
df = data.frame(x=pcaX, y=pcaY)
fig <- fig + geom_line(data=df, aes(x=x, y=y, color="3_pca", fill="3_pca", shape="3_pca"), lwd=1) +
         geom_point(data=df, aes(x=x, y=y, color="3_pca", fill="3_pca", linetype="3_pca", shape="3_pca"), size=5)

s <- c("Observations", "Regression line", "PCA line")
fig <- fig + scale_color_manual(name="Legend", labels=s, values=c(NA, "black", "darkgreen")) + 
         scale_fill_manual(name="Legend", labels=s, values=c("gray80", "gray40", "darkgreen")) +
         scale_linetype_manual(name="Legend", labels=s, values=c(NA, NA, NA)) +
         scale_shape_manual(name="Legend", labels=s, values=c(21, 22, 22)) +
         labs(x="X", y="Y") +
         guides(linetype=guide_legend(override.aes=list(lwd=c(0, 1, 1))), 
                shape=guide_legend(override.aes=list(size=c(5, 4, 4)))) +
         theme(legend.title=element_blank(),
               legend.position=c(.1, .8),
               legend.key.width=unit(2, "line"),
               legend.text.align=0,
               legend.background=element_blank(),
               legend.box.background=element_rect(color="gray"),
               legend.box.margin=margin(-7, 2, -2, 2))

print(fig)
ggsave(filename="reg_regVpca_1.png", width=10, height=5, units="in", dpi=300)

























#-------- Figure 15.3: Joy and ice cream ---------#
#-------------------------------------------------#
# the data
icecream <- c(1, 2, 4, 5, 7)
happiness <- c(5, 6.5, 6, 8, 9)

# the plot
fig <- ggplot(data.frame(x=icecream, y=happiness), aes(x=x, y=y)) +
         geom_point(fill="gray40", shape=21, size=7) +
         labs(x="Ice cream cones eaten", y="Overall life happiness (1-10)") +
         theme_classic()
         
print(fig)
ggsave(filename="reg_icecreamjoy.png", width=4, height=5, units="in", dpi=300)




### run the regression
# list of labels for model output
IVnames <- c("Intercept", "Ice Cream")

# evaluate the regression model
regResults <- lm(happiness ~ icecream, data.frame(icecream, happiness))
names(regResults$coefficients) <- IVnames

summary(regResults)


























#-------- Figure 15.4: Reminder of the geometry of regression ---------#
#----------------------------------------------------------------------#
# some data
N <- 9
x <- seq(-1, 4, length=N)
y <- 1 + x + rnorm(N)

# get GLM predictions
mdl <- lm(y ~ x, data=data.frame(x, y))
yHat <- predict(mdl, data.frame(x))

# plot the data
# plot the data
fig <- ggplot(data.frame(x), aes(x)) +
  geom_point(aes(y=y, color="1_obs", fill="1_obs", linetype="1_obs", shape="1_obs"), size=7, alpha=.5) +
  scale_x_continuous(breaks=seq(-2, 4, 2)) +
  scale_y_continuous(breaks=seq(-1, 5, 1)) +
  labs(x="X (regressor)", y="Y (DV)") +
  theme_classic() +
  theme(panel.grid.major=element_line(color="gray"))

# plot the regression line
fig <- fig + geom_line(aes(y=yHat, color="2_reg", fill="2_reg", linetype="2_reg", shape="2_reg"), lwd=1)

# plot the intercept
intpnt <- predict(mdl, data.frame(x=0, y=1))
fig <- fig + geom_point(aes(x=0, y=intpnt, color="3_icpt", fill="3_icpt", linetype="3_icpt", shape="3_icpt"), size=3)

# data-point-specific projection lines
for (i in 1:N) {
  df <- data.frame(x=c(x[i], x[i]), y=c(y[i], yHat[i]))
  fig <- fig + geom_segment(data=df, aes(x=x[1], xend=x[2], y=y[1], yend=y[2]), 
                            linetype="dashed", lwd=.75, color="gray55") +
    geom_point(data=df, aes(x=x[1], y=y[1]), color="gray55", fill="gray55", shape=21, size=1) +
    geom_point(data=df, aes(x=x[2], y=y[2], color="4_pred", fill="4_pred", linetype="4_pred", shape="4_pred"), size=5)
}

s <- c(bquote(paste("Observations (", italic(y), ")")), "Regression line", "Intercept",
       bquote(paste("Predicted (", hat(italic(y)), ")")))
fig <- fig + scale_color_manual(name="Legend", labels=s, values=rep(NA, 4)) + 
  scale_shape_manual(name="Legend", labels=s, values=c(21, NA, 25, 22)) + #
  scale_fill_manual(name="Legend", labels=s, values=c("gray80", NA, "white", "gray40")) +
  scale_linetype_manual(name="Legend", labels=s, values=c(NA, "solid", NA, NA)) +
  guides(shape=guide_legend(override.aes=list(size=c(5, 5, 2.5, 4)))) +
  theme(legend.title=element_blank(),
        legend.position=c(.225, .875),
        legend.text.align=0,
        legend.background=element_blank(),
        legend.box.background=element_rect(color="gray", fill=alpha("white", .75)),
        legend.box.margin=margin(-7, 2, -2, 2))

print(fig)
ggsave(filename="reg_picOfReg_redux.png", width=4, height=6, units="in", dpi=300)

































#-------- Figure 15.6: Simulating regression data: example 1 ---------#
#---------------------------------------------------------------------#
# coefficients for linking the IV to the DV
B0 <- 50  # intercept in cm
B1 <-  6  # coefficient for change in age, also in cm

# number of observations
N <- 135

# the independent variable
age <- runif(N, 0, 20)

# and the noise
noise <- rnorm(N, 0, 15)

# and now put it together to simulate the data
height <- B0 + B1*age + noise

# visualization
fig <- ggplot(data.frame(x=age, y=height), aes(x=x, y=y)) +
         geom_point(fill="gray90", shape=21, size=5) +
         scale_x_continuous(breaks=seq(0, 20, 10)) +
         scale_y_continuous(breaks=seq(0, 175, 25)) +
         labs(x="Age (years)", y="Height (cm)",
              title="Scatter plot of height by age") +
         theme_classic() +
         theme(plot.title=element_text(hjust=.5))

print(fig)
ggsave(filename="reg_example1data.png", width=4, height=5, units="in", dpi=300)



### run the regression
df <- data.frame(age, height)

# evaluate the regression model
regResults <- lm(height ~ age, data=df)
summary(regResults)

# other ways of viewing regression model results that I discussed in section 15.9.1
tidy(regResults)    # basic table of regression coefficients
glance(regResults)  # info about regression model (not about individual coefficients)
augment(regResults) # detailed info include predicted values and residuals




















#-------- Figure 15.7: Visualizing the regression data ---------#
#---------------------------------------------------------------#
# plot the predicted data
yHat <- predict(regResults, df['age'])
resid <- residuals(regResults)

fig_A <- ggplot(data.frame(age), aes(x=age)) +
           geom_point(aes(y=height, fill="1_obs", shape="1_obs"), size=3, alpha=.6) +
           geom_point(aes(y=yHat, fill="2_pred", shape="2_pred"), size=4, alpha=.8) +
           labs(x="Age (years)", y="Height (cm)",
                title=bquote(paste(bold(A), ") Observed and predicted data"))) +
           theme_classic()

s <- c("Observed", "Predicted")
fig_A <- fig_A + scale_shape_manual(name="Legend", labels=s, values=c(24, 22)) +
           scale_fill_manual(name="Legend", labels=s, values=c("gray30", "white")) +
           guides(shape=guide_legend(override.aes=list(size=c(2, 3)))) +
           theme(legend.title=element_blank(),
                 legend.position=c(.125, .875),
                 legend.text.align=0,
                 legend.background=element_blank(),
                 legend.box.background=element_rect(color="gray"),
                 legend.box.margin=margin(-7, 2, -2, 0))           

fig_B <- ggplot(data.frame(x=height, y=yHat), aes(x=x, y=y)) +
           geom_point(fill="gray30", shape=21, size=4, alpha=.8) +
           labs(x="Observed height", y="Predicted height",
                title=bquote(paste(bold(B), ") Observed vs. predicted: r=")*.(format(round(cor(height, yHat), 2), nsmall=2)))) +
           theme_classic()

fig_C <- ggplot(data.frame(x=resid, y=yHat), aes(x=x, y=y)) +
           geom_point(fill="gray50", shape=22, size=4, alpha=.6) +
           labs(x="Residuals", y="Predicted height",
                title=bquote(paste(bold(C), ") Resid vs pred: r=")*.(format(round(cor(resid, yHat), 2), nsmall=2)))) +
           theme_classic()
  
fig_D <- ggplot(data.frame(x=resid), aes(x=x)) +
           geom_histogram(bins=10, color="black", fill="gray70") +
           labs(x="Residuals", y="Counts",
                title=bquote(paste(bold(D), ") Distribution of residuals"))) +
           theme_classic()

fig <- plot_grid(fig_A, fig_B, fig_C, fig_D, nrow=2, ncol=2)

print(fig)
ggsave(filename="reg_example1res.png", width=11, height=8, units="in", dpi=300)




















#-------- Figure 15.8: Simulating regression data: example 2 ---------#
#---------------------------------------------------------------------#
# create coefficients for linking the IV to the DV
B0 <- 6e2  # intercept
B1 <- -2   # coefficient for brightness manipulation
B2 <- 6e1  # coefficient for experiment condition
B3 <- -2.5 # coefficient for interaction term

# number of observations
N <- 1e2

# generate independent variables
brightness <- runif(N, 10, 100)  # continuous IV
category <- (seq(0, 1, length=N) > .5) + 0  # binary IV

# noise
noise <- rnorm(N, 0, 50)

# generate the data according to the model
RT <- B0 + B1*brightness + B2*category + B3*(brightness*category) + noise

# visualization
fig <- ggplot() +
         geom_point(data=data.frame(x=brightness[which(category==0)], y=RT[which(category==0)]),
                    aes(x=x, y=y, fill="1_car", shape="1_car"), size=5, alpha=.7) +           
         geom_point(data=data.frame(x=brightness[which(category==1)], y=RT[which(category==1)]),
                    aes(x=x, y=y, fill="2_cho", shape="2_cho"), size=5, alpha=.7) +
         labs(x="Picture brightness (% max)", y="Reaction time (ms)",
              title="RT by brightness, grouped by category") +
         theme_classic() +
         theme(plot.title=element_text(hjust=.5))

s <- c("Carrots", "Chocolate")
fig <- fig + scale_fill_manual(name="Legend", labels=s, values=c("gray90", "gray50")) +
         scale_shape_manual(name="Legend", labels=s, values=c(21, 22)) +       
         guides(shape=guide_legend(override.aes=list(size=c(3, 3)))) +
         theme(legend.title=element_blank(),
               legend.position=c(.85, .875),
               legend.text.align=0,
               legend.background=element_blank(),
               legend.box.background=element_rect(color="gray"),
               legend.box.margin=margin(-7, 2, -2, 0))  

print(fig)
ggsave(filename="reg_example2data.png", width=4, height=5, units="in", dpi=300)



### regression model
# construct the design matrix as a dataframe
df <- data.frame(Brightness=brightness, Category=category, RT=RT)
print(df)

## fit the model
model <- lm(RT ~ brightness+category, df)
# model2 <- lm(RT ~ brightness*category, df) # use this line to include an interaction term
summary(model)

# IMPORTANT: When you re-run the code to include the interaction term,
# you need to re-create the dataframe (variable df). Otherwise, df will
# contain multiple columns with the same name and the plots will not update.



















#-------- Figure 15.9/10: Visualizing example 2 ---------#
#--------------------------------------------------------#
# generate predicted RT and residuals
pred_RT <- predict(model, df)
resid <- pred_RT - RT
df <- cbind(df, `Predicted RT`=pred_RT, Residuals=resid)


### now for the visualizations
# scatter plot of observed data
carro <- df$Category==0
choco <- df$Category==1
fig_A <- ggplot() +
           geom_point(data=data.frame(x=df$Brightness[carro], y=RT[carro]), 
                      aes(x=x, y=y, fill="1_carro", shape="1_carro"), color="white", size=3) +  
           geom_point(data=data.frame(x=df$Brightness[choco], y=RT[choco]),
                      aes(x=x, y=y, fill="2_choco", shape="2_choco"), color="white", size=3) +
           geom_line(data=data.frame(x=df$Brightness[carro], y=df$`Predicted RT`[carro]), aes(x=x, y=y),
                     color="gray70", lwd=1) +
           geom_line(data=data.frame(x=df$Brightness[choco], y=df$`Predicted RT`[choco]), aes(x=x, y=y),
                     color="gray20", lwd=1) +
           labs(x="Brightness", y="Predicted RT",
                title=bquote(paste(bold(A), ") Data and predictions"))) +
           theme_classic()

s <- c("Carrots", "Chocolate")
fig_A <- fig_A + scale_fill_manual(name="Legend", labels=s, values=c("gray70", "gray20")) +
           scale_shape_manual(name="Legend", labels=s, values=rep(21, 21)) +
           guides(shape=guide_legend(override.aes=list(size=c(3, 3)))) +
           theme(legend.title=element_blank(),
                 legend.position=c(.837, .85),
                 legend.text.align=0,
                 legend.background=element_blank(),
                 legend.box.background=element_rect(color="gray"),
                 legend.box.margin=margin(-7, 2, -2, 2))

fig_B <- ggplot() +
           geom_point(data=data.frame(x=df$`Predicted RT`[carro], y=df$Residuals[carro]),
                      aes(x=x, y=y, fill="1_carro", shape="1_carro"), color="white", size=3) +
           geom_point(data=data.frame(x=df$`Predicted RT`[choco], y=df$Residuals[choco]),
                      aes(x=x, y=y, fill="2_choco", shape="2_choco"), color="white", size=3) +
           labs(x="Predicted RT", y="Residuals",
                title=bquote(paste(bold(B), ") Residuals Plot"))) +
           theme_classic()

s <- c("Carrots", "Chocolate")
fig_B <- fig_B + scale_fill_manual(name="Legend", labels=s, values=c("gray70", "gray20")) +
           scale_shape_manual(name="Legend", labels=s, values=rep(21, 21)) +
           guides(shape=guide_legend(override.aes=list(size=c(3, 3)))) +
           theme(legend.title=element_blank(),
                 legend.position=c(.842, .15),
                 legend.text.align=0,
                 legend.background=element_blank(),
                 legend.box.background=element_rect(color="gray"),
                 legend.box.margin=margin(-7, 2, -2, -2))

# histograms of residuals separated by category
fig_C <- ggplot() +
           geom_histogram(aes(x=df$Residuals[carro], fill="1_carro"), bins=8, color="black") +
           geom_histogram(aes(x=df$Residuals[choco], fill="2_choco"), bins=8, color="black") +
           scale_y_continuous(expand=c(0, 0)) +
           labs(x="Residuals", y="Count", title=bquote(paste(bold(C), ") Residuals histograms"))) +
           theme_classic()

s <- c("Carrots", "Chocolate")
fig_C <- fig_C + scale_fill_manual(name="Food", labels=s, values=c("gray70", "gray20")) +
           guides(fill=guide_legend(byrow=TRUE)) +
           theme(legend.position=c(.842, .85),
                 legend.text.align=0,
                 legend.key.width=unit(1.5, "line"),
                 legend.key.height = unit(1, "line"),
                 legend.title=element_text(hjust=.5),
                 legend.background=element_blank(),
                 legend.box.background=element_rect(color="gray"),
                 legend.box.margin=margin(-7, 2, -2, -2))

fig <- plot_grid(fig_A, fig_B, fig_C, ncol=3)
  
print(fig)
ggsave(filename="reg_example2res2.png", width=12, height=4, units="in", dpi=300)























#-------- Figure 15.13: Regression example 3 ---------#
#-----------------------------------------------------#
### create the data
exam_scores <- c()
for (ei in 0:4) {
  exam_scores <- append(exam_scores, 70*rep(1, 6) + seq(-1, 5, length=6)*ei)
}

hours_studied <- rep(seq(2, 8, length=6), 5)
ave_sleep_hrs <- seq(4, 8, length=30)


## plot the data

### stratify by hours studied

# fewer than 4 hours studied
plotidx <- hours_studied < 4.1
df <- data.frame(x=ave_sleep_hrs[plotidx], y=exam_scores[plotidx])
fig_A <- ggplot() + 
           geom_point(data=df, aes(x=x, y=y, fill="1_<4h", shape="1_<4h"), size=5) +
           theme_classic()

# 5-6 hours studied
plotidx <- "&"(hours_studied > 4.9, hours_studied < 6.1)
df <- data.frame(x=ave_sleep_hrs[plotidx], y=exam_scores[plotidx])
fig_A <- fig_A + geom_point(data=df, aes(x=x, y=y, fill="2_5-6h", shape="2_5-6h"), size=4)

# more than 6 hours
plotidx <- hours_studied > 6
df <- data.frame(x=ave_sleep_hrs[plotidx], y=exam_scores[plotidx])
fig_A <- fig_A + geom_point(data=df, aes(x=x, y=y, fill="3_>6h", shape="3_>6h"), size=5)

s <- c("<4 hours studied", "5-6 hours studied", ">7 hours studied")
fig_A <- fig_A + labs(x="Hours of sleep", y="Exam score", 
                      title=bquote(paste(bold(A), ") Visualization by grouping"))) +
           scale_fill_manual(name="Legend", labels=s, values=c("gray90", "gray60", "gray30")) +
           scale_shape_manual(name="Legend", labels=s, values=c(21, 24, 22)) +
           guides(shape=guide_legend(override.aes=list(size=c(5, 3.75, 5)))) +
           theme(legend.title=element_blank(),
                 legend.position=c(.152, .87),
                 legend.text.align=0,
                 legend.background=element_blank(),
                 legend.box.background=element_rect(color="gray"),
                 legend.box.margin=margin(-7, 2, -2, 2))

df <- data.frame(x=ave_sleep_hrs, y=exam_scores)
fig_B <- ggplot(df, aes(x=x, y=y)) + geom_point(aes(fill=hours_studied), shape=21, size=4) +
           scale_fill_viridis_c("Hours Studied", option="H",
                                guide=guide_colourbar(title.position="right")) +
           labs(x="Hours of sleep", y="Exam score",
                title=bquote(paste(bold(B), ") Visualization by color"))) +
           theme_classic() +
           theme(legend.title=element_text(angle=270),
                 legend.title.align=.5)

fig <- plot_grid(fig_A, fig_B, nrow=1)

print(fig)
ggsave(filename="reg_example3data2d.png", width=12, height=4, units="in", dpi=300)





























#-------- Figure 15.12: Multidimensional data in a multidimensional space ---------#
#----------------------------------------------------------------------------------#
# A 3D visualization of the data (looks neat, but not really practical)
scatter3D(x=ave_sleep_hrs, y=hours_studied, z=exam_scores,
          col=ramp.col(col=c("gray80", "black", "gray80"), n=100, alpha=.9),
          bty="g", colkey=FALSE, theta=120, phi=30, pch=16, cex=1.5,
          xlab="Average Sleep Hours", ylab="Hours Studied", zlab="Exam Scores", 
          ticktype="detailed", cex.axis=.7, cex.lab=0.85, d=5, expand=0.85)

png(filename="reg_example3data3d.png", width=6, height=5, units="in", res=300)








### put all the data (IVs and DV) into one df

# construct the design matrix as a dataframe
df <- data.frame(ExamScores=exam_scores,
                 Intercept=rep(1, length(exam_scores)),
                 StudyHours=hours_studied,
                 sleepHours=ave_sleep_hrs,
                 Interaction=hours_studied * ave_sleep_hrs)

# let's have a look at the dataframe
print(df)





























#-------- Figure 15.14: Piecewise regression ---------#
#-----------------------------------------------------#
## create the data
N <- 100
x <- seq(0, 10, length=N)
bp <- N %/% 3  # bp = break point (one-third of the way through)

# two different linear relationships
y1 <- 1.2 * head(x, bp)
y2 <-  .4 * tail(x, -bp)
y2 <- y2 - y2[1] + tail(y1, 1)  # shift y2 to follow y1

# combine the two parts with noise
y <- c(y1, y2) + rnorm(N, 0, .3)

### split the data
# (here we know exactly where to split; in Exercise 6 you'll write an algorithm to find the best split)
x1 <- x[x <= x[bp]]
y1 <- y[x <= x[bp]]
x2 <- x[x > x[bp]]
y2 <- y[x > x[bp]]

# fit separate linear regressions
reg1 <- lm(y1 ~ x1, data=data.frame(x1, y1))
reg2 <- lm(y2 ~ x2, data=data.frame(x2, y2))
  
# predictions
yHat1 <- predict(reg1, data.frame(x1))
yHat2 <- predict(reg2, data.frame(x2))

## plotting
fig <- ggplot() +
         geom_point(data=data.frame(x, y), aes(x=x, y=y), fill="gray95", shape=21, size=5, alpha=.6) +
         geom_line(data=data.frame(x=x1, y=yHat1), aes(x=x, y=y), lwd=1) +
         geom_line(data=data.frame(x=x2, y=yHat2), aes(x=x, y=y), lwd=1) +
         geom_vline(xintercept=x[bp], linetype="dashed", color="gray50", lwd=.75) +
         theme_classic()

print(fig)
ggsave(filename="reg_piecewise.png", width=4, height=4, units="in", dpi=300)





























#-------- Figure 15.15: Polynomial design matrix ---------#
#---------------------------------------------------------#
x <- seq(-2, 2, length=101)

maxorder <- 3

df <- data.frame(x)
colors <- c()
for (i in 0:maxorder) {
  # this regressor
  xx <- x**i
  df <- cbind(df, xx)
  c <- i / (maxorder + 1)
  colors <- append(colors, rgb(c, c, c))
}
colnames(df) <- c("x", paste("xx_", 0:3, sep=""))

fig <- ggplot(data.frame(x), aes(x=x)) +
         geom_line(aes(y=df$xx_0, color="x0"), lwd=1) +
         geom_line(aes(y=df$xx_1, color="x1"), lwd=1) +
         geom_line(aes(y=df$xx_2, color="x2"), lwd=1) +
         geom_line(aes(y=df$xx_3, color="x3"), lwd=1)

fig <- fig + labs(x="", y="") + 
         scale_x_continuous(limits=c(head(x, 1), tail(x, 1)), expand=c(0, 0)) +
         scale_y_continuous(breaks=seq(-4, 4, 2), limits=c(-4.75, 5), expand=c(0, 0)) +
         theme_classic()

s <- c(bquote(paste(italic(x)[0], " = ", italic(x)^0)),
       bquote(paste(italic(x)[1], " = ", italic(x)^1)),
       bquote(paste(italic(x)[2], " = ", italic(x)^2)),
       bquote(paste(italic(x)[3], " = ", italic(x)^3)))
fig <- fig + scale_color_manual(name="Legend", labels=s, values=colors) +
           scale_linetype_manual(name="Legend", labels=s, values=rep("solid", 4)) +
           theme(legend.title=element_blank(),
                 legend.position=c(.85, .15),
                 legend.text.align=0,
                 legend.key.height=unit(1.2, "line"),
                 legend.key.width=unit(1.75, "line"),
                 legend.background=element_blank(),
                 legend.box.background=element_rect(color="gray"),
                 legend.box.margin=margin(-7, 2, -2, 2))

print(fig)
ggsave(filename="reg_polydesmat.png", width=4, height=5, units="in", dpi=300)

























#-------- Figure 15.16: Polynomial regression ---------#
#------------------------------------------------------#
n <- 30
x <- seq(-2, 3, length=n)

# generate data
y1 <- x**2 + rnorm(n)

# R doesn't have a direct translation of the polyfit/polyval functions as in Python.
# The code below uses lm() and predict().

# beta coefficients (need only the coefficients, not a full model evaluation)
polycoefs <- as.numeric(lm(y1 ~ poly(x, 2, raw=TRUE))$coef)

# predictions
# polynom::polynomial()
yHat1 <- predict(polynomial(coef=polycoefs), x)

# and plot
fig_A <- ggplot(data.frame(x=x), aes(x=x)) +
           geom_point(aes(y=y1), fill="gray90", shape=21, size=5) +
           geom_smooth(aes(y=yHat1), color="black") +
           labs(x=NULL, y=NULL,
                title=bquote(paste(bold(A), ") ", 2^italic(nd), " order polynomial"))) +
           scale_x_continuous(limits=c(-2.2, 3.2)) +
           scale_y_continuous(limits=c(min(y)*1.3, max(y)*1.1)) +
           theme_classic() +
           theme(axis.ticks=element_blank(),
                 axis.text=element_blank())

# repeat for 3rd order polynomial
y2 <- x**2 - .4*x**3 + rnorm(length(x))*.8
polycoefs <- as.numeric(lm(y2 ~ poly(x, 3, raw=TRUE))$coef)
# yHat2 <- predict(lm(y2 ~ poly(x, 3, raw=TRUE)), data.frame(x))
yHat2 <- predict(polynomial(coef=polycoefs), x)
fig_B <- ggplot(data.frame(x=x), aes(x=x)) +
           geom_point(aes(y=y2), fill="gray90", shape=21, size=5) +
           geom_smooth(aes(y=yHat2), color="black") +
           labs(x=NULL, y=NULL,
                title=bquote(paste(bold(B), ") ", 3^italic(rd), " order polynomial"))) +
           theme_classic() +
           theme(axis.ticks=element_blank(),
                 axis.text=element_blank())

fig <- plot_grid(fig_A, fig_B, ncol=1)

print(fig)
ggsave(filename="reg_polyExample23.png", width=4, height=6, units="in", dpi=300)

























#-------- Figure 15.17: Polynomial order and overfitting ---------#
#-----------------------------------------------------------------#
n <- 30
x <- seq(-2, 3, length=n)
y <- x**2 - .4 * x**3 + rnorm(n) * .8
nrow <- 2
ncol <- 3
fig_lst <- list()
for (oi in 1:(nrow*ncol)) {
  # order number
  order <- (oi-1)*3 + 1
  polycoefs <- as.numeric(lm(y ~ poly(x, order, raw=TRUE))$coef)
  yHat <- predict(polynomial(coef=polycoefs), x)
  fig_lst[[oi]] <- ggplot(data.frame(x), aes(x)) +
                     geom_point(aes(y=y), fill="gray90", shape=21, size=5) +
                     geom_line(aes(y=!!yHat), lwd=1) +  # using `!!` to avoid the lazy evaluation
                     labs(x=NULL, y=NULL, title=glue("Order = ", order)) +
                     scale_x_continuous(limits=c(-2.2, 4.2)) +
                     scale_y_continuous(limits=c(min(y)*1.3, max(y)*1.1)) +
                     theme_classic() +
                     theme(axis.ticks=element_blank(),
                           axis.text=element_blank(),
                           plot.title=element_text(hjust=.5))
}

fig <- plot_grid(plotlist=fig_lst, nrow=2, ncol=3)

print(fig)
ggsave(filename="reg_polyManyOrders.png", width=8, height=5, units="in", dpi=300)



























#-------- Figure 15.18: Bayes Information Criteria (BIC) ---------#
#-----------------------------------------------------------------#
# initialize
maxorder <- 17
bic <- rep(0, maxorder-1)

for (i in 1:maxorder) {
  if (i == 1) {
    polycoefs <- mean(y)
  } else {
    polycoefs <- as.numeric(lm(y ~ poly(x, i-1, raw=TRUE))$coef)
  }
  yHat <- predict(polynomial(coef=polycoefs), x)  # `yhat` here but `yHat` in the previous code, so I use `yHat` all the time.
  bic[i] <- n*log(sum((yHat-y)**2)) + i*log(n)
}

fig <- ggplot(data.frame(x=0:(maxorder-1), y=bic), aes(x=x, y=y)) +
         geom_line(lwd=.5) +
         geom_point(fill="gray90", shape=21, size=5) +
         labs(x="Polynomial model order", y="BIC") +
         scale_x_continuous(breaks=seq(0, maxorder-1, 3)) +
         theme_classic()

# draw an arrow to the best BIC
bestK <- which.min(bic)
fig <- fig + annotate("segment", x=4.32, xend=3.2, y=111, yend=97.1, lwd=1.5, 
                      linejoin="mitre", arrow=arrow(type="closed", length=unit(.02, "npc")))

print(fig)
ggsave(filename="reg_polyBIC.png", width=6, height=4, units="in", dpi=300)























#-------- Figure 15.19: Log of probabilities ---------#
#-----------------------------------------------------#
p <- seq(.0001, .3, length=156)

fig <- ggplot(data.frame(x=p), aes(x=x)) +
         geom_line(aes(y=p/(1-p), color="line1", linetype="line1"), lwd=1.5) +
         geom_line(aes(y=log(p/(1-p)), color="line2", linetype="line2"), lwd=1.5) +
         labs(x="p", y="Probability ratio (raw or lag") +
         scale_x_continuous(breaks=seq(0, .3, .1), limits=c(head(p, 1), 1.02*tail(p, 1)), expand=c(0, 0)) +
         scale_y_continuous(breaks=seq(-6, 0, 2), limits=c(-6, 1)) +
         theme_classic()

s <- c("y = p/(1-p)", "y = ln[p/(1-p)]")
fig <- fig + scale_color_manual(name="Legend", labels=s, values=c("gray80", "gray30")) +
         scale_linetype_manual(name="Legend", labels=s, values=c("solid", "solid")) +
         theme(legend.title=element_blank(),
               legend.position=c(.25, .108),
               legend.text.align=0,
               legend.background=element_blank(),
               legend.box.background=element_rect(color="gray"),
               legend.box.margin=margin(-7, 2, -2, 2))

print(fig)
ggsave(filename="reg_logOdds.png", width=4, height=5, units="in", dpi=300)




















### Logistic regression example
# Generate data
N <- 100
studyHours <- runif(N, 0, 10)

# the generating equation
pass_prob <- 1 / (1 + exp(-(studyHours-5)))

# randomize pass/fail according to probability function
passed_exam <- runif(N) < pass_prob

####################

# test the model
df <- data.frame(x=studyHours, y=passed_exam)
model <- glm(y ~ x, data=df, family="binomial")
summary(model)


















#-------- Figure 15.20: Visualization of logistic regression ---------#
#---------------------------------------------------------------------#
# interpolated values for study times
xx <- seq(0, 10, length=N)

# predicted probabilities
yHat <- predict(model, data.frame(x=xx),  type="response")

# and plot
fig <- ggplot() +
         geom_point(data=data.frame(x=studyHours, y=as.integer(passed_exam)), aes(x=x, y=y, fill="1_obs", linetype="1_obs", shape="1_obs"), size=4.5, alpha=.5) + 
         geom_line(data=data.frame(x=xx, y=yHat), aes(x=x, y=y, fill="2_log", linetype="2_log", shape="2_log"), lwd=1) +
         geom_hline(yintercept=0, color="gray") +
         geom_hline(yintercept=1, color="gray") +
         geom_hline(yintercept=.5, color="gray", linetype="dashed", lwd=.3) +
         geom_segment(data=data.frame(x=c(5, 5), y=c(0, 1)), aes(x=x[1], xend=x[2], y=y[1], yend=y[2]), linetype="dashed", lwd=.3, color="gray") +
         scale_x_continuous(breaks=seq(0, 10, 2)) +
         scale_y_continuous(breaks=seq(0, 1, .2)) +
         labs(x="Hours Studied", y="Probability of Passing Exam") +
         theme_classic()

s <- c("Observed Data", "Logistic curve")
fig <- fig + scale_fill_manual(name="Legend", labels=s, values=c("gray40", NA)) +
         scale_linetype_manual(name="Legend", labels=s, values=c(NA, "solid")) +
         scale_shape_manual(name="Legend", labels=s, values=c(21, NA)) +
         theme(legend.title=element_blank(),
               legend.position=c(.854, .5),
               legend.key.width=unit(2, "line"), 
               legend.text.align=0,
               legend.background=element_blank(),
               legend.box.background=element_rect(color="gray"),
               legend.box.margin=margin(-7, 2, -2, 2))

print(fig)
ggsave(filename="reg_logistic.png", width=8, height=5, units="in", dpi=300)































#-------- Exercise 1 ---------#
#-----------------------------#
# the data (copied from the top of the code file)
icecream <- c(1, 2, 4, 5, 7)
happiness <- c(5, 6.5, 6, 8, 9)

# construct a design matrix with intercept
X <- cbind(icecream, rep(1, length(icecream)))

# compute the left inverse
leftInv <- inv(t(X)%*%X) %*% t(X)

# compute the regression coefficients
betas <- leftInv %*% happiness
print(c(betas))












#-------- Exercise 2 ---------#
#-----------------------------#
# sample size
N <- 100

# create data and design matrix
DV <- rnorm(N)
DM <- matrix(rnorm(N*1), N, 1)  # change 1 to 37

# fit the model (including intercept)
model <- lm(DV ~ DM, data=data.frame(DM, DV))

# print the r-squared terms
glue("    R-squared: ", format(round(summary(model)$r.squared, 3), nsmall=3))
glue("adj.R-squared: ", format(round(summary(model)$adj.r.squared, 3), nsmall=3))

####################

# not part of the instructions, but FYI: demo that R2 is literally r(y, yHat)^2
glue(format(cor(predict(model, data.frame(DM)), DV, method="pearson")**2, nsmall=19))
glue(format(summary(model)$r.squared, nsmall=19))

####################



# initializations
nIVs <- seq(1, N-1, 3)
results <- matrix(rep(0, length(nIVs)*2), length(nIVs), 2)

# the experiment
for (idx in seq_along(nIVs)) {
  M <- nIVs[idx]
  # loop over repetitions with new random numbers
  for (expi in 1:50) {
    # create data and design matrix
    DV <- rnorm(N)
    DM <- matrix(rnorm(N*M), N, M)
    
    # fit the model (including intercept)
    model <- lm(DV ~ DM, data=data.frame(DM, DV))
    
    # get the r-squared terms
    results[idx, 1] <- results[idx, 1] + 1e2 * summary(model)$r.squared
    results[idx, 2] <- results[idx, 2] + 1e2 * summary(model)$adj.r.squared
  }
}

# divide for the average
results <- results / expi

# now plot

fig <- ggplot(data.frame(x=nIVs/N), aes(x=x)) +
         geom_hline(yintercept=0, linetype="dashed") +
         geom_point(aes(x=x, y=results[, 1], fill="1_rsq", shape="1_rsq"), size=5) +
         geom_point(aes(x=x, y=results[, 2], fill="2_adj", shape="2_adj"), size=5) +
         scale_x_continuous(breaks=seq(0, 1, .2)) +
         scale_y_continuous(breaks=seq(0, 1e2, 20)) +
         labs(x="Number of IVs (fraction of data points)", y="Variance explained (%)") +
         theme_classic()
    
s <- c("R-squared", bquote(paste("Adjusted ", "R"^2)))
fig <- fig + scale_fill_manual(name="Legend", labels=s, values=c("gray30", "gray70")) +
           scale_shape_manual(name="Legend", labels=s, values=c(22, 21)) +
           guides(shape=guide_legend(override.aes=list(size=c(3, 3)))) +
           theme(legend.title=element_blank(),
                 legend.position=c(.1, .857),
                 legend.text.align=0,
                 legend.background=element_blank(),
                 legend.box.background=element_rect(color="gray"),
                 legend.box.margin=margin(-7, 2, -2, 2))  
  
print(fig)
ggsave(filename="reg_ex2.png", width=8, height=5, units="in", dpi=300)


####################

model <- lm(DV ~ DM, data=data.frame(DM, DV))
# testing the model on new data drawn from the same population
glue("R2 for these data:")
glue("  ", format(round(cor(predict(model, data.frame(DM)), DV)**2, 3), nsmall=3), "\n", .trim=FALSE)
glue("R2 for new data from the same population:")
glue("  ", format(round(cor(predict(model, data.frame(DM)), rnorm(N))**2, 3), nsmall=3), "\n", .trim=FALSE)






















#-------- Exercise 3 ---------#
#-----------------------------#
# Important: The data for this exercise come from the code that created Figure 15.13.
#            Run that code before running this code.

# be careful with excluding the DV from the df!
df <- data.frame(ExamScores=exam_scores,
                 StudyHours=hours_studied,
                 SleepHours=ave_sleep_hrs,
                 Interaction=hours_studied * ave_sleep_hrs)

desmat <- subset(df, select=-c(ExamScores))
DV <- df$ExamScores

# fit the model
model <- lm(DV ~ ., data=data.frame(desmat, DV))

# show the regression summary
summary(model)



####################
# visualizations

# generate predicted RT and residuals
df$PredictedScores <- predict(model, x=desmat)
df$Residuals <- df$PredictedScores - DV

### now for the visualizations
fig_A <- ggplot() +
           geom_point(data=df, aes(x=ExamScores, y=PredictedScores), color="white", fill="black", shape=21, size=3) +
           scale_x_continuous(breaks=seq(70, 90, 10)) +
           labs(x="ExamScores", y="PredictedScores",
                title=bquote(paste(bold(A), ") Observed vs. predicted"))) +
           theme_classic()

fig_B <- ggplot() +
           geom_point(data=df, aes(x=PredictedScores, y=Residuals), color="white", fill="black", shape=21, size=3) +
           scale_x_continuous(breaks=seq(70, 90, 10)) +
           scale_y_continuous(breaks=seq(-.5, .75, .25)) +
           labs(x="PredictedScores", y="Residuals",
                title=bquote(paste(bold(B), ") Residuals Plot"))) +
           theme_classic()

fig_C <- ggplot() +
           geom_histogram(data=df, aes(x=Residuals), bins=7, color="black", fill="gray30") +
           scale_y_continuous(breaks=seq(0, 10, 2), expand=c(0, 0)) +
           labs(x="Residuals", y="Count",
                title=bquote(paste(bold(C), ") Residuals histograms"))) +
           theme_classic()

fig <- plot_grid(fig_A, fig_B, fig_C, ncol=3)

print(fig)
ggsave(filename="reg_ex3.png", width=12, height=4, units="in", dpi=300)


####################

### Some observations:
#
# - The model predicts the data extremely well (panel A), which is no surprise:
#   there was no added noise and the simulation was linear.
#
# - The residuals plot looks... strange. There is no linear correlation, but there is clearly structure in there.
#   Examination of the regression summary table reveals strong skew and a significant non-normal distribution.
#
# - The residuals are strongly non-Gaussian distributed, probably due to the issues identified above.
#
# - Overall, the model diagnostics reveal some deep issues with these data, and question whether a regression is
#   appropriate in this case. On the other hand, the patterns in the data are so clear that these violations can be
#   be tolerated in the interest of quantifying the effects of the IVs (sleep/study, and their interaction).
#























#-------- Exercise 4 ---------#
#-----------------------------#
# define the formula
formula <- "ExamScores ~ StudyHours + SleepHours + StudyHours*SleepHours"

# fit and check the results
result <- lm(formula, data=df)
summary(result)




















#-------- Exercise 5 ---------#
#-----------------------------#
# extract relevant columns from the df
X <- subset(df, select=c("StudyHours", "SleepHours", "Interaction"))
y <- df$ExamScores

modelLR <- lm(y ~ ., data=data.frame(X, y))

# print the coefficients
glue("Intercept: ", summary(modelLR)$coef[1, 1])
glue(paste(c("Coefficients:", as.numeric(summary(modelLR)$coefficients[2:4, 1])), collapse=" "))

























#-------- Exercise 6 ---------#
#-----------------------------#
## create the simulated data
N <- 100
x <- seq(0, 10, length=N)
bp <- N %/% 3  # bp = break point (one-third of the way through)

# two different linear relationships
y1 <- 1.2 * head(x, bp)
y2 <-  .4 * tail(x, length(x)-bp)
y2 <- y2 - head(y2, 1) + tail(y1, 1)  # shift y2 to follow y1

# combine the two parts with noise
y <- c(y1, y2) + rnorm(N, 0, .1)

### run the experiment
# define breakpoints to evaluate, and initialize results
breakPoints2test <- seq(2, 8, length=37)
BIC <- rep(0, length(breakPoints2test))

# now for the experiment
for (idx in seq_along(breakPoints2test)) {
  breakx <- breakPoints2test[idx]
  # split the data
  x1 <- x[x <= breakx]
  y1 <- y[x <= breakx]
  x2 <- x[x >  breakx]
  y2 <- y[x >  breakx]
  
  # fit the regressions
  reg1 <- lm(y1 ~ x1, data=data.frame(x1, y1))
  reg2 <- lm(y2 ~ x2, data=data.frame(x2, y2))
  
  # take the average BICs from the two pieces
  BIC[idx] <- (glance(reg1)$BIC + glance(reg2)$BIC) / 2
}

# find and report the "optimal" breakpoint
bestBP <- breakPoints2test[which.min(BIC)]

glue("Empirical best breakpoint: x = ", format(round(bestBP, 2), nsmall=2))
glue("Ground truth breakpoint:   x = ", format(round(x[bp], 2), nsmall=2))


####################
## now to run and visualize the model

# Split the data data again
x1 <- x[x <= bestBP]
y1 <- y[x <= bestBP]
x2 <- x[x >  bestBP]
y2 <- y[x >  bestBP]

# linear regression (again)
reg1 <- lm(y1 ~ x1, data=data.frame(x1, y1))
reg2 <- lm(y2 ~ x2, data=data.frame(x2, y2))

# predictions
yHat1 <- predict(reg1, data.frame(x1))
yHat2 <- predict(reg2, data.frame(x2))


### plotting
fig_A <- ggplot(data.frame(x=breakPoints2test, y=BIC), aes(x=x, y=y)) +
           geom_point(aes(x=x, y=y), fill="white", shape=22, size=5, alpha=.6) +
           geom_vline(aes(xintercept=bestBP, color="bpl", linetype="bpl"), lwd=.75, key_glyph="path") +
           scale_y_continuous(breaks=seq(-80, 40, 20)) +
           labs(x="Breakpoint (x)", y="BIC", title=bquote(paste(bold(A), ") BIC by breakpoint"))) +
           theme_classic()

s <- "Minimum BIC"
fig_A <- fig_A + scale_color_manual(name="Legend", labels=s, values="gray") +
           scale_linetype_manual(name="Legend", labels=s, values="dashed") +
           guides(linetype=guide_legend(override.aes=list(lwd=.5))) +
           theme(legend.title=element_blank(),
                 legend.position=c(.803, .11),
                 legend.key.width=unit(2, "line"),
                 legend.text.align=0,
                 legend.background=element_blank(),
                 legend.box.background=element_rect(color="gray"),
                 legend.box.margin=margin(-7, 2, -2, 2))  

fig_B <- ggplot() +
           geom_vline(aes(xintercept=x[bp], color="1_tbp", linetype="1_tbp"), lwd=.75, key_glyph="path") +
           geom_vline(aes(xintercept=bestBP, color="2_ebp", linetype="2_ebp"), lwd=.75, key_glyph="path") +
           geom_point(data=data.frame(x, y), aes(x, y), fill="gray95", shape=21, size=4.5, alpha=.6) +
           geom_line(data=data.frame(x=x1, y=yHat1), aes(x=x, y=y), lwd=1) +
           geom_line(data=data.frame(x=x2, y=yHat2), aes(x=x, y=y), lwd=1) +
           scale_x_continuous(breaks=seq(0, 10, 2)) +
           labs(x="x", y="y", title=bquote(paste(bold(B), ") Data and predictions"))) +
           theme_classic()

s <- c("True bp", "Est. bp")
fig_B <- fig_B + scale_color_manual(name="Legend", labels=s, values=c("black", "gray")) +
           scale_linetype_manual(name="Legend", labels=s, values=c("solid", "dashed")) +
           guides(linetype=guide_legend(override.aes=list(lwd=c(.5, .5)))) +
           theme(legend.title=element_blank(),
                 legend.position=c(.85, .12),
                 legend.key.width=unit(2, "line"),
                 legend.text.align=0,
                 legend.background=element_blank(),
                 legend.box.background=element_rect(color="gray"),
                 legend.box.margin=margin(-7, 2, -2, 2))  

fig <- plot_grid(fig_A, fig_B, ncol=2)

print(fig)
ggsave(filename="reg_ex6.png", width=10, height=4, units="in", dpi=300)





















#-------- Exercise 7 ---------#
#-----------------------------#
### simulate the data

# sample size
N <- 40
x <- seq(0, 5, length=N)

# create the DV
slope <- exp(1)
y <- slope * x + pi + rnorm(N)

# fit the model
orig_model <- lm(y ~ x, data=data.frame(x, y))

### experiment
betas <- matrix(rep(0, N*2), N, 2)

for (i in 1:N) {
  yc <- y
  yc[i] <- yc[i] + 10
  
  # fit the model and get its slope (don't need to store the model)
  betas[i, 1] <- as.numeric(lm(yc ~ x, data=data.frame(x, yc))$coef)[2]
  betas[i, 2] <- as.numeric(rlm(yc ~ x, data=data.frame(x, yc))$coef)[2]
}


### plotting
fig_A <- ggplot(data.frame(x), aes(x)) +
           geom_point(aes(y=y), fill="gray90", shape=22, size=4, alpha=.5) +
           geom_line(aes(y=predict(orig_model, data.frame(x))), lwd=1) +
           scale_y_continuous(breaks=seq(0, 17.5, 2.5), expand=c(0, 2)) +
           labs(x="x", y="y", title=bquote(paste(bold(A), ") Original data"))) +
           theme_classic()
           
fig_B <- ggplot(data.frame(x), aes(x=x)) +
           geom_point(aes(y=betas[, 1], color="1_ols", fill="1_ols", linetype="1_ols", shape="1_ols"), size=3, alpha=.5) +
           geom_point(aes(y=betas[, 2], color="2_rlm", fill="2_rlm", linetype="2_rlm", shape="2_rlm"), size=3, alpha=.5) +
           geom_hline(aes(yintercept=slope, color="3_grtr", fill="3_grtr", linetype="3_grtr", shape="3_grtr")) +
           geom_hline(aes(yintercept=orig_model$coef[2], color="4_noo", fill="4_noo", linetype="4_noo", shape="4_noo")) +
           scale_y_continuous(breaks=seq(2.5, 3, .1)) +
           labs(x="x value with outlier", y=bquote(paste(beta, " coefficient")),
                title=bquote(paste(bold(B), ") Slopes by outlier position"))) +
           theme_classic()

s <- c("OLS", "RLM", "GrTr", "NoO")
fig_B <- fig_B + scale_color_manual(name="Legend", labels=s, values=c(NA, NA, "black", "gray")) +
           scale_fill_manual(name="Legend", labels=s, values=c("gray90", "gray50", NA, NA)) +
           scale_linetype_manual(name="Legend", labels=s, values=c(NA, NA, "dashed", "dotted")) +
           scale_shape_manual(name="Legend", labels=s, values=c(21, 22, -1, -1)) +
           guides(color=guide_legend(nrow=2, bycol=TRUE),
                  linetype=guide_legend(override.aes=list(lwd=c(NA, NA, .3, .3))),
                  shape=guide_legend(override.aes=list(size=c(3, 3, 0, 0)))) +
           theme(legend.title=element_blank(),
                 legend.position=c(.2, .87),
                 legend.key.width=unit(1.32, "line"),
                 legend.text.align=0,
                 legend.background=element_blank(),
                 legend.box.background=element_rect(color="gray"),
                 legend.box.margin=margin(-7, 2, -2, 0))  

# impose the outlier
yc <- y
yc[1] <- head(yc, 1) + 10
# plot the data
fig_C <- ggplot(data.frame(x), aes(x)) +
           geom_point(aes(y=!!yc), fill="gray90", shape=22, size=5, alpha=.5) +
           geom_point(aes(x=head(x, 1), y=head(!!yc, 1)), shape=4, size=3, stroke=1.5) +
           geom_line(aes(y=predict(lm(yc ~ x, data=data.frame(x, yc)), data.frame(x)))) +
           scale_y_continuous(breaks=seq(0, 18, 2.5)) +
           labs(x="x", y="y", title=bquote(paste(bold(C), ") Outlier at the beginning"))) +
           theme_classic()

# impose the outlier
yc <- y
yc[length(yc)] <- tail(yc, 1) + 10
# plot the data
fig_D <- ggplot(data.frame(x), aes(x)) +
           geom_point(aes(y=yc), fill="gray90", shape=22, size=5, alpha=.5) +
           geom_point(aes(x=tail(x, 1), y=tail(yc, 1)), shape=4, size=3, stroke=1.5) +
           geom_line(aes(y=predict(lm(yc ~ x, data=data.frame(x, yc)), data.frame(x)))) +
           scale_y_continuous(breaks=seq(0, 30, 5)) +
           labs(x="x", y="y", title=bquote(paste(bold(D), ") Outlier at the end"))) +
           theme_classic()

fig <- plot_grid(fig_A, fig_B, fig_C, fig_D, nrow=2, ncol=2)

print(fig)
ggsave(filename="reg_ex7.png", width=10, height=8, units="in", dpi=300)
























#-------- Exercise 8 ---------#
#-----------------------------#
# simulation parameters
N <- 135
x <- seq(0, 7, length=N)

# generate the data
y <- 1*x + x*rnorm(N)

# fit the model
mdl <- lm(y ~ x, data=data.frame(x, y))

# get the predicted data and residuals
yHat <- predict(mdl, data.frame(x))
resid <- summary(mdl)$resid

### plotting
fig_A <- ggplot(data.frame(x), aes(x)) +
           geom_point(aes(y=y), fill="gray30", shape=24, size=3, alpha=.6) +
           geom_line(aes(y=yHat), lwd=1) +
           scale_y_continuous(breaks=seq(-10, 15, 5)) +
           labs(x="x", y="y", title=bquote(paste(bold(A), ") Data and prediction"))) +
           theme_classic()

fig_B <- ggplot(data.frame(x=yHat, y=resid), aes(x=x, y=y)) +
           geom_point(aes(x=x, y=y), fill="gray50", shape=22, size=3, alpha=.6) +
           labs(x="Residuals", y="Counts", title=bquote(paste(bold("B"), ") Resid vs pred: r=")*.(format(round(cor(resid, yHat), 2), nsmall=2)))) +
           theme_classic()
  
fig_C <- ggplot() +
           geom_histogram(aes(x=resid), bins=20, color="black", fill="gray70") +
           scale_y_continuous(breaks=seq(0, 30, 5), expand=c(0, 0)) +
           labs(x="Residuals", y="Counts", title=bquote(paste(bold("C"), ") Distribution of residuals"))) +
           theme_classic()
  
fig <- plot_grid(fig_A, fig_B, fig_C, ncol=3)

print(fig)
ggsave(filename="reg_ex8a.png", width=12, height=4, units="in", dpi=300)


####################

# print the true and estimated betas
glue("Ground-true beta: 1")
glue("Estimated beta  : ", format(round(mdl$coef[2], 3), nsmall=3))

####################





# experiment parameters
m <- 2          # slope (fixed for now)
numreps <- 2e1  # number of repetitions in the experiment

# range of maximum heteroscedasticity values
badness <- seq(1, 10, length=15)

# results matrix
mismatch <- matrix(rep(0, length(badness)*2), length(badness), 2)

# how to simulate the noise; choose 1, 2, or 3
noiseSimulation <- 1

# start the experiment!
for (idx in seq_along(badness)) {
  maxbad <- badness[idx]
  # repeat the experiment multiple times
  for (i in 1:numreps) {
    if (noiseSimulation == 1) {
      noise <- rnorm(N) * seq(1, maxbad, length=N)
    } else if (noiseSimulation == 2) {
      noise <- rnorm(N) * seq(maxbad, maxbad, length=N)
    } else if (noiseSimulation == 3) {
      noise <- rnorm(N) * seq(1, maxbad, length=N)
      noise <- noise / sd(noise)
    }
    # generate the data
    y <- m*x + noise
    
    # fit the model
    mdl <- lm(y ~ x, data=data.frame(x, y))
    summary(mdl)
    
    # store results (beta error and -ln(p)
    mismatch[idx, 1] <- mismatch[idx, 1] + abs(100*(as.numeric(mdl$coef)[2]-m) / m)
    mismatch[idx, 2] <- mismatch[idx, 2] - log(as.numeric(ad.test(as.numeric(summary(mdl)$resid))[2]))
  } 
} 

# divide to average
mismatch <- mismatch / numreps

### plotting
fig_A <- ggplot(data.frame(x=badness, y=mismatch[, 1]), aes(x=x, y=y)) +
           geom_point(shape=22, fill="gray70", size=5) +
           scale_x_continuous(breaks=seq(2, 10, 2)) +
           labs(x="Max heteroscedasticity", y=bquote(paste("Percent error in ", beta)),
                title=bquote(paste(bold(A), ") Error in coefficient estimate"))) +
           theme_classic()

fig_B <- ggplot(data.frame(x=badness, y=mismatch[, 2]), aes(x=x, y=y)) +
           geom_point(shape=21, fill="gray70", size=5) +
           geom_hline(aes(yintercept=-log(.05), linetype="pval"), color="gray", lwd=.75) +
           scale_x_continuous(breaks=seq(2, 10, 2)) +
           scale_y_continuous(breaks=seq(floor(min(y)), ceiling(max(y)), 1)) +
           labs(x="Max heteroscedasticity", y="-ln(p) from Ombinus test",
                title=bquote(paste(bold(B), ") Residual nonnormality significance"))) +
           theme_classic()

fig_B <- fig_B + scale_linetype_manual(labels="p < .05", values="dashed") +
           guides(linetype=guide_legend(override.aes=list(lwd=.5))) +
           theme(legend.title=element_blank(),
                 legend.position=c(.125, .9),
                 legend.text.align=0,
                 legend.key.width=unit(2, "line"),
                 legend.background=element_blank(),
                 legend.box.background=element_rect(color="gray"),
                 legend.box.margin=margin(-7, 2, -2, 2))

fig <- plot_grid(fig_A, fig_B, nrow=1)

print(fig)
ggsave(filename="reg_ex8c.png", width=11, height=4, units="in", dpi=300)



























#-------- Exercise 9 ---------#
#-----------------------------#
# base sample size (will be multiplied by 10 the way I implemented the simulation)
N <- 25

# the linear term (temp in C)
temp <- rep(seq(10, 35, length=N), 10)
b_temp <- 2

# the nonlinear term (price)
price <- ceiling(seq(.01, 8, length=N*10))
b_price <- -3

# some noise
noise <- 4 * rnorm(N*10)

# the DV
sales <- 50 + b_temp*temp + b_price*(price-4)**2 + noise

# plotting
fig <- ggplot(data.frame(x=temp, y=sales), aes(x=x, y=y)) +
         geom_point(aes(fill=price), color="black", shape=21, size=4) +
         scale_fill_gradient2(midpoint = 4.5, low="blue", mid="white", high="red",
                              breaks=seq(1, 8, 1),
                              name="Price per cone (€)",
                              guide=guide_colorbar(title.position="right")) +
         scale_y_continuous(breaks=seq(20, 120, 20)) +
         labs(x="Temperature (°C)", y="Ice cream sales") +
         theme_classic() +
         theme(legend.title=element_text(angle=270),
               legend.title.align=.5)

print(fig)
ggsave(filename="reg_ex9a.png", width=6, height=4, units="in", dpi=300)


####################
# build the regression model

X <- data.frame(Sales=sales,
                Temperature=temp,
                Price=(price-4)**2,
                Interaction=temp*price)

print(X)

####################

# fit the model
model <- lm(y ~ ., data=data.frame(subset(X, select=-c(Sales)), y=X$Sales))

# show the regression summary
summary(model)

####################

### make a prediction

# predicted values of the IVs
predicted_temp <- 25
predicted_price <- 6.5

yHat <- predict(model, data.frame(Sales=1, Temperature=predicted_temp, Price=predicted_price, Interaction=0))

# plotting

fig <- ggplot(data.frame(x=temp, y=sales), aes(x=x, y=y)) +
         geom_point(aes(fill=price), color="black", shape=21, size=6) +
         geom_point(data=data.frame(x=predicted_temp, y=yHat), aes(x=x, y=y),
                    fill=rgb(.4, 1, .3), shape=22, size=7) +
         geom_point(data=data.frame(x=predicted_temp, y=yHat), aes(x=x, y=y),
                    shape=4, size=5, stroke=1.5) +
         scale_fill_gradient2(midpoint = 4.5, low="blue", mid="white", high="red",
                              breaks=seq(1, 8, 1),
                              name="Price per cone (€)",
                              guide=guide_colorbar(title.position="right")) +
         labs(x="Temperature (°C)", y="Ice cream sales",
              title=bquote("Predicted sales for "*.(predicted_temp)*" °C and €"*.(predicted_price))) +
         scale_y_continuous(breaks=seq(20, 120, 20)) +
         labs(x="Temperature (°C)", y="Ice cream sales") +
         theme_classic() +
         theme(legend.title=element_text(angle=270),
               legend.title.align=.5)

print(fig)
ggsave(filename="reg_ex9b.png", width=6.5, height=4.5, units="in", dpi=300)



















#-------- Exercise 10 ---------#
#------------------------------#
# Download the file
url <- "https://archive.ics.uci.edu/static/public/437/residential+building+data+set.zip"
httr::GET(url, httr::write_disk("residential_building_data_set.zip", overwrite = TRUE))

# Unzip the file
unzip("residential_building_data_set.zip", exdir = "residential_building_data")

# Read specific sheet from the Excel file
# NOTE: Importing these data requires some manual checking for the renamed columns corresponding to Excel columns F, V, AA, and DD
data <- readxl::read_excel("residential_building_data/Residential-Building-Data-Set.xlsx", skip=1) %>% select(c("V-2","V-20...22","V-25...27","V-9"))

# Rename columns
colnames(data) <- c('FloorArea', 'Interest', 'CPI', 'Price')

# View the data
head(data)



### pairplot
# Use ggpairs to create a pair plot
pair_plot <- ggpairs(data, 
                     mapping = ggplot2::aes(fill = "black")) +
  ggplot2::theme_bw() +
  ggplot2::theme(legend.position = "none") # Remove legend if not needed

# Save and view the plot
print(pair_plot)
ggsave("reg_ex10b.png", pair_plot, width = 10, height = 8)



# My decisions based on visual inspection:
# 1) log-transform FloorArea and Price
# 2) Binarize Interest



# Add transformed columns to the existing dataframe
data <- data %>%
  mutate(
    "logPrice" = log(Price),
    "logFloorArea" = log(FloorArea),
    "binInterest" = as.integer(Interest > 14.5)  # Convert to binary (1 if true, 0 if false)
  )

# View the updated data
print(data)


## redo the pairplot
# Get column indices for the specified columns
col_indices <- match(c('logPrice', 'logFloorArea', 'CPI'), names(data))

# Create the pair plot
pair_plot <- ggpairs(data, columns = col_indices,
                     mapping = ggplot2::aes(fill = "black")) +
  ggplot2::theme_bw() +
  ggplot2::theme(legend.position = "none") # Remove legend if not needed

# Save and view the plot
print(pair_plot)





### now for cleaning

# Threshold for outliers
zThresh <- 3  # p<.001 (not exactly .001, but z=3 is also a typical threshold, I guess because people like integers)

# Create z-scored data
data_z <- data %>% 
  select(logPrice, logFloorArea, CPI) %>%
  mutate(across(everything(), ~ (.-mean(.)) / sd(.)))

# Reshape data for plotting
data_melted <- melt(data.frame(data_z))

# Box plots of z-scored data
ggplot(data_melted, aes(x = "variable", y = "value")) +
  geom_boxplot(fill="black", color="black", width=0.4,
               outlier.shape=1, outlier.size=2.5) +
  xlab("Data feature") +
  ylab("Data value (z)") +
  theme_minimal() +
  stat_boxplot(geom ='errorbar', width=0.2)

# Save the plot
ggsave("reg_ex10z.png", width = 4, height = 5)



## remove outliers
# Print the number of rows before cleaning
print(paste("Pre-cleaned dataset has", nrow(data), "rows."))

# Calculate z-scores for specified columns
data_z <- data %>%
  select(logPrice, logFloorArea, CPI) %>%
  mutate(across(everything(), ~ (.-mean(.)) / sd(.)))

# Filter the original data based on z-scores
data_cleaned <- data %>%
  filter(abs(data_z$logPrice) <= zThresh,
         abs(data_z$logFloorArea) <= zThresh,
         abs(data_z$CPI) <= zThresh)

# Print the number of rows after cleaning
print(paste("Post-cleaned dataset has", nrow(data_cleaned), "rows."))



### correlation matrix
# Drop 'Interest' and 'bin-Interest' columns
data_selected <- data[, !(names(data) %in% c('Interest', 'binInterest'))]

# Compute the correlation matrix
R <- cor(data_selected)

# Melt the correlation matrix for ggplot
R_melted <- melt(as.matrix(R))

# Create the heatmap
ggplot(data = R_melted, aes(x = X1, y = X2, fill = value)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1, 1), space = "Lab", 
                       name="Pearson\nCorrelation") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, size = 12, hjust = 1),
        axis.text.y = element_text(size = 12)) +
  coord_fixed() +
  ggtitle("Correlation matrix")

# Save the plot
ggsave('reg_ex10d.png', width = 8, height = 6)












#-------- Exercise 11 ---------#
#------------------------------#

# Add an interaction term between 'binInterest' and 'CPI'
data_cleaned$Int_X_CPI <- data_cleaned$binInterest * data_cleaned$CPI

# Fit the model
model <- lm(logPrice ~ logFloorArea + CPI + binInterest + Int_X_CPI, data = data_cleaned)

# Show the regression summary
summary(model)




### now to visualize the results
# Generate predicted values and residuals
data_cleaned$Predicted <- predict(model, newdata = data_cleaned)
data_cleaned$Residuals <- data_cleaned$Predicted - data_cleaned$logPrice

# Define color palette
colorPalette <- c("lightgrey", "black")

# Scatter plot of observed data with model predictions
p1 <- ggplot(data_cleaned, aes(x = CPI, y = logPrice, color = factor(binInterest))) +
  geom_point(size = 3) +
  geom_line(aes(y = Predicted), size = 1.2) +
  scale_color_manual(values = colorPalette) +
  ggtitle("A) Data and predictions")

# Predicted by observed plot
p2 <- ggplot(data_cleaned, aes(x = logPrice, y = Predicted, color = factor(binInterest))) +
  geom_point(size = 3) +
  scale_color_manual(values = colorPalette) +
  ggtitle("B) Data by predictions")

# Residuals plot
p3 <- ggplot(data_cleaned, aes(x = Predicted, y = Residuals, color = factor(binInterest))) +
  geom_point(size = 3) +
  scale_color_manual(values = colorPalette) +
  ggtitle("C) Residuals Plot")

# Histograms of residuals separated by category
p4 <- ggplot(data_cleaned, aes(x = Residuals, fill = factor(binInterest))) +
  geom_histogram(bins = 30, position = "dodge") +
  scale_fill_manual(values = colorPalette) +
  labs(x = "Residuals", y = "Count") +
  ggtitle("D) Residuals histograms")

# Combine plots
combined_plot <- ggarrange(p1, p2, p3, p4, labels = c("A", "B", "C", "D"), 
                           ncol = 2, nrow = 2, font.label = list(size = 14))

# Save and display the plot
print(combined_plot)
ggsave("reg_ex11b.png", combined_plot, width = 12, height = 8)
















#-------- Exercise 12 ---------#
#------------------------------#
# Specify the columns to be standardized (not including the intercept)
cols2zscore <- c('CPI', 'logFloorArea', 'logPrice', 'binInterest', 'Int_X_CPI')

# Standardize the specified columns
dataStd <- data_cleaned
for (col in cols2zscore) {
  dataStd[[col]] <- (data_cleaned[[col]] - mean(data_cleaned[[col]], na.rm = TRUE)) / sd(data_cleaned[[col]], na.rm = TRUE)
}

# Fit the model
formulaStd <- as.formula("logPrice ~ CPI + logFloorArea + binInterest + Int_X_CPI")
modelStd <- lm(formulaStd, data = dataStd)

# Show the regression summary
summary(modelStd)






## now standardize the betas
# Standard deviations of the data columns
stds <- sapply(data_cleaned, sd, na.rm = TRUE)

# Print the top row of the table
cat("     Variable:  Unstd  | Beta-std | Data-std\n")
cat("-------------:---------|----------|---------\n")

# Loop through the variable names
unstd_betas <- coef(model)[-1] # Excluding intercept
std_betas <- coef(modelStd)[-1] # Excluding intercept

names <- names(unstd_betas)

for (name in names) {
  beta <- unstd_betas[name]
  betaDataStd <- std_betas[name]
  
  # Compute the standardized beta from the variable stds
  betaStd <- beta * stds[name] / stds['logPrice']
  
  # Print everything!
  cat(sprintf("%13s: %7.4f | %7.4f  | %7.4f\n", name, beta, betaStd, betaDataStd))
}


## finally, report the condition numbers
cat(sprintf("Condition number of the unstandardized design matrix: %8.2f\n", kappa(model)))
cat(sprintf("Condition number of the standardized design matrix  : %8.2f\n", kappa(modelStd)))

# Note: Condition number can be numerically difficult to calculate and there might be some differences with Python.




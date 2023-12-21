#----
# Modern statistics: Intuition, Math, Python, R
## Mike X Cohen (sincxpress.com)
#### https://www.amazon.com/dp/B0CQRGWGLY
#### Code for chapter 9 (Sampling and distributions)

# About this code file:
### This code file will reproduce most of the figures in this chapter 
### (some figures were made in Inkscape), and illustrate the statistical 
### concepts explained in the text. The point of providing the code is not 
### just for you to recreate the figures, but for you to modify, adapt, 
### explore, and experiment with the code.
###
### Solutions to all exercises are at the bottom of the file.
# Thanks to Dorus Wang for help with the translation from Python.


# import libraries
library(cowplot)
library(ggplot2)
library(glue)
library(pracma)
library(reshape)
library(scales)
library(truncnorm)









#-------- Figure 10.2: Empirical distribution under H0 ---------#
#---------------------------------------------------------------#
N <- 1e2  # per group per sample
numExps <- 1e3
meandiff <- numeric(numExps)

# run the experiment
for (i in 1:numExps) {
  pre <- rtruncnorm(N, a=-5, b=10, mean=6, sd=2)
  pst <- rtruncnorm(N, a=-5, b=10, mean=6, sd=2)
  meandiff[i] <- mean(pst) - mean(pre)
}

df <- data.frame(x=meandiff)
fig <- ggplot(df, aes(x=x)) +
         geom_histogram(bins=20, color="black", fill="gray50") +
         scale_x_continuous(breaks=seq(-1, 1, .25), limits=c(-1, 1)) +
         scale_y_continuous(breaks=seq(0, 150, 25), expand=c(0, 0)) +
         labs(x=expression(paste("Difference value (", Delta, ")")), y="Count") +
         theme_classic()

fig
ggsave(filename="hyp_empH0.png", width=8, height=4, units="in", dpi=300)















#-------- Figure 10.3: Distribution assuming H0 is true ---------#
#----------------------------------------------------------------#
fig <- ggplot(df, aes(x=x)) +
         geom_histogram(aes(linetype="H"), bins=20, color="black", fill="gray90", show.legend=FALSE) +
         geom_vline(aes(xintercept=.1, linetype="b"), lwd=1, key_glyph="path") +
         geom_vline(aes(xintercept=.7, linetype="c"), lwd=1, key_glyph="path") + 
         scale_x_continuous(breaks=seq(-1, 1, .25), limits=c(-1, 1)) +
         scale_y_continuous(breaks=seq(0, 140, 20), expand=c(0, 0)) +
         scale_linetype_manual(labels=c(expression(paste('"A" (', Delta, " = .1)")), 
                               expression(paste('"B" (', Delta, " = .7)")),
                               expression(paste("H"[0], " dist."))),
                               values=c(H="solid", b="dashed", c="dotted")) +
         labs(x="Difference value", y="Count") +
         theme_classic() +
         guides(linetype=guide_legend(override.aes=list(linetype=c("dashed", "dotted", "solid"),
                                                        size=10, linewidth=c(.75, .75, 3),
                                                        color=c("black", "black", "gray")))) +
         theme(legend.title=element_blank(),
               legend.position=c(.12, .75),
               legend.text.align=0,
               legend.background=element_blank(),
               legend.box.background=element_rect(colour = "black"),
               legend.box.margin=margin(-7, 2, -2, 2))  # top, right, bottom, left

fig
ggsave(filename="hyp_empH0_withAs.png", width=8, height=4, units="in", dpi=300)
















#-------- Figure 10.4: Analytical vs. empirical H0 distribution ---------#
#------------------------------------------------------------------------#
empirical <- rnorm(1e4)
x <- seq(-4, 4, length=1001)
analytical <- dnorm(x) * diff(x[1:2])

fig_A <- ggplot(data.frame(x=x, y=analytical), aes(x=x, y=y)) +
           geom_smooth(color="black", lwd=.75) +
           scale_x_continuous(expand=c(0, 0)) +
           scale_y_continuous(breaks=NULL) +
           labs(title=expression(paste(bold("A)  "), "Analytical ", "H"[0], " distribution")),
                x="", y="Probability") +
           theme_classic() +
           theme(plot.title=element_text(size=14))

fig_B <- ggplot(data.frame(x=empirical), aes(x=x)) +
           geom_histogram(bins=60, color="black", fill="gray80") +
           scale_x_continuous(limit=c(-4, 4)) +
           scale_y_continuous(breaks=NULL, expand=c(0, 0)) +
           labs(title=expression(paste(bold("B)  "), "Empirical ", "H"[0], " distribution")),
                x="Test statistic value", y="Count") +
           theme_classic() +
           theme(plot.title=element_text(size=14))

fig <- plot_grid(fig_A, fig_B, ncol=1)

fig
ggsave(filename="hyp_empVanalyH0.png", width=4, height=6, units="in", dpi=300)
















#-------- Figure 10.5: p-values and thresholds ---------#
#-------------------------------------------------------#
# create a Gaussian probability curve
zvals <- seq(-3, 3, length=1001)
zpdf <- dnorm(zvals)

# plot the probability function and the vertical lines
df1 <- data.frame(x=zvals, y=zpdf)
fig_A <- ggplot(df1, aes(x=x, y=y)) +
           geom_smooth(color="black") +
           scale_x_continuous(limit=c(head(zvals, 1), tail(zvals, 1)), expand=c(0, 0)) +
           scale_y_continuous(limit=c(0, .42), breaks=NULL, expand=c(0, 0)) +
           labs(x="", y=expression(paste("Prob. given ", "H"[0]))) +
           theme_classic()

# two-tailed p-values
mn_idx <- which.min(zvals**2)
pvalsL <- pnorm(zvals[1:mn_idx])
pvalsR <- 1 - pnorm(zvals[(mn_idx+1):length(zvals)])
pvals2 <- 2 * c(pvalsL, pvalsR)

# plot the probability function
df2 <- data.frame(x=zvals, y=pvals2)
fig_B <- ggplot(df2, aes(x=x, y=y)) +
           geom_line(color="black") +
           geom_hline(yintercept=.05, color="gray50", linetype="dashed") +
           scale_x_continuous(limit=c(head(zvals, 1), tail(zvals, 1)), expand=c(0, 0)) +
           labs(x="", y="P-value") +
           theme_classic()

# draw vertical lines
vline1 <- geom_vline(aes(xintercept=zvals[which.min((zvals-qnorm(.025))**2)]), color="darkgray",
                     linetype="dotted", linewidth=.75)
vline2 <- geom_vline(aes(xintercept=zvals[which.min((zvals-qnorm(.975))**2)]), color="darkgray",
                     linetype="dotted", linewidth=.75)
fig_A <- fig_A + vline1 + vline2
fig_B <- fig_B + vline1 + vline2

# draw patches for significant regions
zidx <- which.min((zvals-qnorm(.025))**2)
fig_A <- fig_A + geom_ribbon(data=subset(df1, x <= x[zidx]), aes(ymin=0, ymax=y), alpha=.4)
fig_B <- fig_B + geom_ribbon(data=subset(df2, x <= x[zidx]), aes(ymin=0, ymax=y), alpha=.4)

zidx <- which.min((zvals-qnorm(.975))**2)
fig_A <- fig_A + geom_ribbon(data=subset(df1, x >= x[zidx]), aes(ymin=0, ymax=y), alpha=.4)
fig_B <- fig_B + geom_ribbon(data=subset(df2, x >= x[zidx]), aes(ymin=0, ymax=y), alpha=.4)

fig_C <- fig_B

# indicators
fig_A <- fig_A + annotate("text", x=0, y=.3, label='\"Not Significant\"') +
  annotate("segment", x=-2.35, xend=-2.2, y=.12, yend=.05, lwd=1.25, linejoin="mitre",
           arrow=arrow(type="closed", length=unit(.02, "npc"))) +
  annotate("text", x=-2.375, y=.15, label='\"Significant\"') +
  annotate("segment", x=2.35, xend=2.2, y=.12, yend=.05, lwd=1.25, linejoin="mitre",
           arrow=arrow(type="closed", length=unit(.02, "npc"))) +
  annotate("text", x=2.375, y=.15, label='\"Significant\"')

fig_B <- fig_B + annotate("text", x=2.4, y=.12, label="p=.05")
fig_C <- fig_C + annotate("text", x=2.4, y=.0975, label="p=.05")

# panel titles
fig_A <- fig_A + ggtitle(expression(paste(bold("A)  "), "Test statistic distribution if ", "H"[0], " were true")))
fig_B <- fig_B + ggtitle(expression(paste(bold("B)  "), "P-value for each test statistic")))
fig_C <- fig_C + ggtitle(expression(paste(bold("C)  "), "Same as panel ", bold("B"), " but in log scale")))

fig_B <- fig_B + scale_y_continuous(limits=c(0, 1.03), breaks=seq(0, 1, .5), expand=c(0, 0))
fig_C <- fig_C + xlab("Test statistic (z-score)") +
           scale_y_log10(labels=trans_format("log10", math_format(10^.x))) +
           annotation_logticks(sides="l", outside=TRUE, size=.25,
                               short=unit(.075, "cm"), mid=unit(.125, "cm"), long=unit(.175, "cm")) +
           coord_cartesian(clip="off") +
           theme(axis.ticks.y=element_blank())

fig <- plot_grid(fig_A, fig_B, fig_C, ncol=1, align="v", axis="l")

fig
ggsave(filename="hyp_sigRegionsZandP.png", width=7, height=7, units="in", dpi=300)




















#-------- Figure 10.6: H0 distribution with critical value ---------#
#-------------------------------------------------------------------#
# create a Gaussian probability curve
x <- seq(-4, 4, length=1001)
gpdf <- dnorm(x)

# the find the indices of the 95% of the distribution
ubndi <- which.min(abs(x-qnorm(.95)))
df <- data.frame(x=x, y=gpdf)
fig_0 <- ggplot(df, aes(x=x, y=y)) +
           geom_smooth(color="black") +
           scale_x_continuous(limits=c(head(x, 1), tail(x, 1)), expand=c(0, 0)) +
           scale_y_continuous(limits=c(0, .42), expand=c(0, 0)) +
           labs(x="Test statistic", y="Probability") +
           theme_classic() +
           theme(line=element_blank(), axis.text=element_blank())

fig_A <- fig_0 + ggtitle(expression(paste(bold("A)  "), "One-tailed test")))

# create patches for the significant area
fig_A <- fig_A + geom_ribbon(data=subset(df, x >= x[ubndi]), 
                             aes(ymin=0, ymax=y), alpha=.4)

# annotations
tailx <- which.min(abs(x-2.2))
fig_A <- fig_A + annotate("text", x=2.72, y=.1, label="5%") +
  annotate("segment", x=2.55, xend=2.2, y=0.09, yend=0.05, lwd=1.5, linejoin="mitre",
           arrow=arrow(type="closed", length=unit(.01, "npc")))

# significance threshold line
fig_A <- fig_A + geom_vline(xintercept=x[ubndi], linetype="dashed", lwd=.75) +
           annotate("text", x=x[ubndi]+.15, y=.25, size=4, label="Sig. threshold", angle=90)

# the find the indices of the 2.5% and 97.5%
lbndi <- which.min(abs(x-qnorm(.025)))
ubndi <- which.min(abs(x-qnorm(1-.025)))

# plot the probability function and the vertical lines
fig_B <- fig_0 + ggtitle(expression(paste(bold("B)  "), "Two-tailed test")))


# now create patches for the significant area
fig_B <- fig_B + geom_ribbon(data=subset(df, x <= x[lbndi]), aes(ymin=0, ymax=y), alpha=.4) +
           geom_ribbon(data=subset(df, x >= x[ubndi]), aes(ymin=0, ymax=y), alpha=.4)

# significance threshold line
fig_B <- fig_B + geom_vline(xintercept=x[lbndi], linetype="dashed", lwd=.75) +
           geom_vline(xintercept=x[ubndi], linetype="dashed", lwd=.75)           

# annotations  
fig_B <- fig_B + annotate("text", x=x[lbndi]-.15, y=.25, size=4, label="Sig. threshold", angle=90) +
           annotate("text", x=x[ubndi]+.15, y=.25, size=4, label="Sig. threshold", angle=90) +
           annotate("text", x=-2.77, y=.105, label="2.5%") +
           annotate("segment", x=-2.55, xend=-2.2, y=0.09, yend=0.05, lwd=1.5, linejoin="mitre",
                    arrow=arrow(type="closed", length=unit(.01, "npc"))) +
           annotate("text", x=2.79, y=.105, label="2.5%") +
           annotate("segment", x=2.55, xend=2.2, y=0.09, yend=0.05, lwd=1.5, linejoin="mitre",
                    arrow=arrow(type="closed", length=unit(.01, "npc")))

fig <- plot_grid(fig_A, fig_B, ncol=1, align="v")

fig
ggsave(filename="hyp_tails.png", width=7, height=5, units="in", dpi=300)





















#-------- Figure 10.7: Area of z>1 ---------#
#-------------------------------------------#
# create a Gaussian probability curve
z <- seq(-4, 4, length=1001)
gpdf <- dnorm(z)

# note that the cdf returns the area *left* of the input value,
# so we subtract 1 to get the area to the *right*.
areaZ1 <- 1 - pnorm(1)

# plot the probability function and the vertical lines
df <- data.frame(x=z, y=gpdf)
fig <- ggplot(df, aes(x=x, y=y)) +
         geom_smooth(color="black") +
         scale_x_continuous(breaks=seq(head(z, 1), tail(z, 1), 1), expand=c(0, 0)) +
         scale_y_continuous(limits=c(0, .42), expand=c(0, 0)) +
         labs(x="Z value", y="Probability") +
         theme_classic() +
         theme(axis.ticks.y=element_blank(),
               axis.text.y=element_blank())

xidx <- which.min(abs(z-1))
fig <- fig + geom_ribbon(data=subset(df, x >= z[xidx]), aes(ymin=0, ymax=y), color="black", alpha=.4) +
         geom_vline(xintercept=z[xidx], linetype="dashed", lwd=.75)

tailx <- which.min(abs(z-1.7))
fig <- fig + annotate("text", x=2.875, y=.105, label=glue(format(round(100*areaZ1, 2), nsmall=2), "%")) +
         annotate("segment", x=2.55, xend=2.2, y=.09, yend=.05, lwd=1.25, linejoin="mitre",
                  arrow=arrow(type="closed", length=unit(.01, "npc")))

fig
ggsave(filename="hyp_zgt1.png", width=6, height=3, units="in", dpi=300)






















#-------- Figure 10.8: p-z pairs ---------#
#-----------------------------------------#
# critical p-values to draw
ps2draw <- c(.05, .01, .001)

stds <- seq(-4, 4, length=1001)
probs <- dnorm(stds)

# draw the lines
fig_A <- ggplot(data.frame(x=stds, y=probs), aes(x=x, y=y)) +
           geom_smooth(color="black") +
           scale_x_continuous(breaks=seq(head(stds, 1), tail(stds, 1), 1), expand=c(0, 0)) +
           scale_y_continuous(limits=c(0, .42), expand=c(0, 0)) +
           labs(x="", y="Probability") +
           theme_classic() +
           theme(axis.ticks.y=element_blank(), axis.text.y=element_blank())

fig_B <- fig_A


## one-tailed
styles <- c("dashed", "dotted", "dotdash")
for (i in seq_along(ps2draw)) {
  zval <- qnorm(1-ps2draw[i])
  c <- i / length(ps2draw) * .8
  fig_A <- fig_A + geom_vline(xintercept=zval, color=rgb(c, c, c), lwd=.575, linetype=styles[i]) +
             annotate("text", x=0, y=.2-(i-2)/10, color=rgb(c, c, c),
                      label=glue("p=", ps2draw[i], ", z=", format(round(zval, 2), nsmall=2))) +
             annotate("segment", x=.7+(i-1)*.02, xend=zval-.075, y=.2-(i-2)/10, yend=.2-(i-2)/10,
                      color=rgb(c, c, c), lwd=2, linejoin="mitre", arrow=arrow(type="closed", length=unit(.01, "npc")))
}

## two-tailed
for (i in seq_along(ps2draw)) {
  # z-value for this p-value
  zval <- qnorm(ps2draw[i]/2)
  
  # vertical line
  c <- i / length(ps2draw) * .8  # line color
  fig_B <- fig_B + geom_vline(xintercept=-zval, color=rgb(c, c, c), lwd=.575, linetype=styles[i]) +
             geom_vline(xintercept=zval, color=rgb(c, c, c), lwd=.575, linetype=styles[i]) +
             annotate("text", x=0, y=.2-(i-2)/10, color=rgb(c, c, c),
                      label=glue("p=", ps2draw[i], ", z=|", format(round(abs(zval), 2), nsmall=2), "|")) +
             annotate("segment", x=-.7-(i-1)*.02, xend=zval+.075, y=.2-(i-2)/10, yend=.2-(i-2)/10, 
                      color=rgb(c, c, c), lwd=2, linejoin="mitre", arrow=arrow(type="closed", length=unit(.01, "npc"))) +
             annotate("segment", x=.7+(i-1)*.02, xend=-zval-.075, y=.2-(i-2)/10, yend=.2-(i-2)/10,
                      color=rgb(c, c, c), lwd=2, linejoin="mitre", arrow=arrow(type="closed", length=unit(.01, "npc")))
}

fig_A <- fig_A + ggtitle(expression(paste(bold("A)  "), "One-tailed ", italic(p), "-z pairs")))
fig_B <- fig_B + labs(title=expression(paste(bold("B)  "), "Two-tailed ", italic(p), "-z pairs")),
                      x="Standard deviations (z)")

fig <- plot_grid(fig_A, fig_B, ncol=1)

fig
ggsave(filename="hyp_pz_combos2know.png", width=7, height=7, units="in", dpi=300)






# Getting p-values from z-values
zval <- 1
pval <- pnorm(zval)
glue("The p-value for z = ", format(round(zval, 2), nsmall=2), " is p = ", round(1-pval, 4), ".")


























#-------- Figure 10:14 Spatial image for MCC ---------#
#-----------------------------------------------------#
M <- matrix(rnorm(7*7), 7, 7)
M[M < .2] <- 0

df <- melt(M, varnames=c("x", "y"), value.name="value")
fig <- ggplot(df, aes(x=.data[["x"]], y=.data[["y"]], fill=.data[["value"]])) +
         geom_raster(data=df, aes(x=.data[["x"]], y=.data[["y"]], fill=.data[["value"]])) +
         scale_fill_continuous(high="white", low="black") +
         scale_x_continuous(expand=c(0, 0)) +
         scale_y_continuous(expand=c(0, 0)) +
         theme_classic() +
         theme(line=element_blank(), 
               axis.ticks=element_blank(), 
               text=element_blank(),
               legend.position="None",
               plot.margin=unit(c(0, 0, -.95, -.95), "mm"))

fig
ggsave(filename="spatial_mcc.png", width=7, height=7, units="in", dpi=300)


















#-------- Exercise 1 ---------#
#-----------------------------#

# No extra code here; find the relevant code cell, copy/paste that code here,
#  and modify it to have the p-value be soft-coded. Make sure you also update
#  the text in the figures!










#-------- Exercise 2 ---------#
#-----------------------------#
X <- data.frame(values=rnorm(20))
p <- t.test(X, mu=0)$p.value

glue("The mean is ", format(round(mean(X$values), 2), nsmall=2),
     " and the p-value from a t-test is ", format(round(p, 4), nsmall=4), ".")

####################

# simulate data and t-test
alphaRange <- seq(.001, .9, length=15)

M <- 1e2
n <- length(alphaRange)
type1errors <- matrix(0, M, n)
meansAndPvals <- matrix(0, M*n, 2)

# loop over alpha's
for (ai in seq_along(alphaRange)) {
  alpha <- alphaRange[ai]
  # loop over experiments
  for (expi in 1:M-1) {
    # generate the data and compute the p-value from a t-test
    X <- rnorm(20)
    p <- t.test(X, mu=0)$p.value
    
    # store (as Boolean) whether this test was subthreshold
    type1errors[expi, ai] <- p < alpha
    
    # gather the mean and p-value from this test
    meansAndPvals[expi*n + ai,] <- c(mean(X), p)
  }
}

df1 <- data.frame(x=alphaRange, y=apply(type1errors, 2, mean))
fig_A <- ggplot(df1, aes(x=x, y=y)) +
           geom_smooth(method="lm", formula=y~x, se=FALSE, color="black", lwd=.75, linetype="dashed") +
           geom_point(shape=22, size=5, color="black", fill="gray80") +
           scale_x_continuous(breaks=seq(0, 1, .2), expand=c(.025, 0)) +
           scale_y_continuous(breaks=seq(0, 1, .2), expand=c(.025, 0)) +
           labs(title=expression(paste(bold("A)  "), "Expected vs. empirical false alarm rate")),
                x="Predicted FA proportion", y="Observed FA proportion") +
           theme_classic()

df2 <- data.frame(x=meansAndPvals[, 1], y=meansAndPvals[, 2])
fig_B <- ggplot(df2, aes(x=x, y=y)) +
           geom_jitter(shape=20) +
           scale_y_continuous(breaks=seq(0, 1, .2), expand=c(.01, 0)) +
           labs(title=expression(paste(bold("B)  "), "Means and p-values")),
                x="Sample means", y="P-values") +
           theme_classic()

fig <- plot_grid(fig_A, fig_B, nrow=1, align="h")

fig
ggsave(filename="hyp_ex2.png", width=10, height=4, units="in", dpi=300)



####################

# Note about the code above:
#  You can implement many tests simultaneously in a matrix, with columns containing experiments.
#  I'll introduce this in Chapter 12, but I show the code here FYI.

type1errors <- numeric(length(alphaRange))  # note: vector not matrix!
for (ai in seq_along(alphaRange)) {
  df <- data.frame(matrix(rnorm(20*M, 0, 1), 20, M))
  p <- apply(df, 2, function(x) t.test(x)$p.value)
  type1errors[ai] <- mean(p < alphaRange[ai])
}

fig <- ggplot(data.frame(x=alphaRange, y=type1errors), aes(x=x, y=y)) +
         geom_point(shape=22, size=7, color="black", fill="gray80") +
         scale_x_continuous(breaks=seq(0, 1, .2), expand=c(.025, 0)) +
         scale_y_continuous(breaks=seq(0, 1, .2), expand=c(.025, 0)) +
         theme_classic() +
         theme(axis.title=element_blank(),
               plot.margin=unit(c(10, 12, 10, 12), "mm"))

fig
ggsave(filename="hyp_ex2_1.png", width=7, height=7, units="in", dpi=300)

























#-------- Exercise 3 ---------#
#-----------------------------#
# one t-value distribution
tvals <- seq(-4, 4, length=1001)

# compute the pdf
tpdf <- dt(tvals, 20)
fig <- ggplot(data.frame(x=tvals, y=tpdf), aes(x=x, y=y)) +
         geom_smooth(color="steelblue") +
         scale_x_continuous(breaks=seq(head(tvals, 1), tail(tvals, 1), 1), expand=c(0, 0)) +
         scale_y_continuous(breaks=seq(0, .4, .05)) +
         labs(x="t value", y="Probability") +
         theme_classic()

# t-distributions with different df's
tvals <- seq(-4, 4, length=1001)
dfs <- 4:40

# initialize the figure
mn <- min(dfs)
mx <- max(dfs)
for (df in dfs) {
  tpdf <- dt(tvals, df)
  c <- (df - mn) / mx
}

fig




####################

# t-distributions with different df's
tvals <- seq(-4, 4, length=1001)
dfs <- 4:40

# create and show the pdf's
fig <- ggplot()
for (df in dfs) {
  tpdf <- dt(tvals, df) * diff(tvals[1:2])  # + df / (2*1e5)
  # optional: for a nice visual effect, you can shift the pdf's:
  # tpdf += df/100000
  
  # plot
  c <- (df - min(dfs)) / max(dfs)  # color value with scaling
  fig <- fig + geom_smooth(data=data.frame(x=tvals, y=tpdf), aes(x=x, y=y), color=rgb(c, c, c))
}

# then plot zscores (using "tvals" as z-values here)
fig <- fig + geom_smooth(data=data.frame(x=tvals, y=dnorm(tvals)*diff(tvals[1:2])), aes(x=x, y=y), color="red", linetype="dashed") +
         scale_x_continuous(limits=c(head(tvals, 1), tail(tvals, 1)), breaks=seq(-4, 4, 1), expand=c(0, 0)) +
         scale_y_continuous(limits=c(0, max(tpdf)*1.02), breaks=seq(0, .003, .0005), expand=c(0, 0)) +
         labs(x="t or z value", y="Probability") +
         theme_classic()

fig
ggsave(filename="hyp_ex3.png", width=8, height=4, units="in", dpi=300)






















#-------- Exercise 4 ---------#
#-----------------------------#
options(warn=-1)

# p-value threshold (corrected)
pThresh <- .05

# set of p-values
k <- 40
pvals <- runif(k, .001, .3)**2

# step 1: sort the p-values
pvalsSort <- sort(pvals)

# step 2: linear interpolated distribution
pvalsInterp <- 1:k / k

# step 3: adjusted p-values
pvals_adjusted <- pvalsSort / pvalsInterp

# using R's built-in function for FDR
qq <- p.adjust(pvalsSort, "BH")

# I'm inputting pvalsSort instead of pvals. That's done to facilitate the
# plotting; you would normally input the vector of p-values without sorting.

## visualization!
fig <- ggplot(data.frame(x=1:k), aes(x=x)) +
         geom_line(aes(y=pThresh * pvalsInterp, shape="4_rej", fill="4_rej", linetype="4_rej"), color="darkgray") +
         geom_hline(yintercept=pThresh, linetype="dashed", color="gray80") +
         geom_point(aes(y=qq, shape="1_qq", fill="1_qq", linetype="1_qq"), color="black", size=3) +
         geom_point(aes(y=pvals_adjusted, shape="2_padj", fill="2_padj", linetype="2_adj"), color="black", size=2.5) +
         geom_point(aes(y=pvalsSort, shape="3_psort", fill="3_psort", linetype="3_psort"), color="black", size=3) +
         scale_x_continuous(breaks=seq(0, 40, 5)) +
         scale_y_continuous(breaks=seq(0, .08, .02)) +
         labs(x="Sorted index", y="P-value") +
         theme_classic()

s <- c("p.adjust()", "Manual", "Raw p-values",
       expression(paste("Rejection line (", alpha, bold(v), "/k)")))
fig <- fig + annotate("text", x=0, y=.053, label="p=.05") +
         scale_shape_manual(name="Legend", labels=s, values=c(21, 24, 22, NA)) +
         scale_fill_manual(name="Legend", labels=s, values=c("gray30", "gray60", "gray85", NA)) +
         scale_linetype_manual(name="Legend", label=s, values=c(NA, NA, NA, "solid")) +
         theme(legend.title=element_blank(),
               legend.position=c(.89, .225),
               legend.text.align=0,
               legend.background=element_blank(),
               legend.box.background=element_rect(color="gray"),
               legend.box.margin=margin(-7, 2, -2, 2))

fig
ggsave(filename="hyp_ex4.png", width=8, height=4, units="in", dpi=300)

















#-------- Exercise 5 ---------#
#-----------------------------#
# find the p-value threshold in the non-adjusted p-values

# the p-values that are significant according to FDR correction
H0rejected <- pvalsSort <= pvalsInterp * pThresh

# find the largest significant (raw) pvalue
H0rejected_pvals <- which(H0rejected)
FDR_threshold <- pvalsSort[tail(H0rejected_pvals, 1)]

glue("Uncorrected p-value threshold based on FDR: q=", format(round(FDR_threshold, 4), nsmall=4), ".")




















#-------- Exercise 6 ---------#
#-----------------------------#
N <- 1e2

# p-values
pvals <- runif(N, .001, .25)**2

# thresholds
bon_thresh <- .05 / N
q <- p.adjust(pvals, "BH")

# print messages
glue("       FDR led to ", format(round(100 * mean(q < .05), 2), width=2), "% significant tests.")
glue("Bonferroni led to ", format(round(100 * mean(pvals < bon_thresh), 2), width=2), "% significant tests.")

####################

# Experiment repetitions with the same N=100
sigTests <- matrix(0, 100, 2)
for (expi in 1:100) {
  pvals <- runif(N, .001, .25)**2
  bon_thresh <- .05 / N
  q <- p.adjust(pvals, "BH")
  # record the results
  sigTests[expi, 1] <- 100 * mean(q < .05)
  sigTests[expi, 2] <- 100 * mean(pvals < bon_thresh)
}

# report the average and std
glue("       FDR: mean of ", format(mean(sigTests[, 1]), width=5, nsmall=2), "% (std: ",
     format(round(sd(sigTests[, 1]), 2), nsmall=2), ") significant tests.") 
glue("Bonferroni: mean of ", format(mean(sigTests[, 2]), width=5, nsmall=2), "% (std: ",
     format(round(sd(sigTests[, 2]), 2), nsamll=2), ") significant tests.")















#-------- Exercise 7 ---------#
#-----------------------------#
# Note: You need to have run the code for exercise 6 before this code.

# the p-value set size
Ns <- apply(data.frame(logspace(log(2, 10), log(5e2, 10), 25)), 1, as.integer)

# number of experiment repetitions
nRepetitions <- 1e2

# results matrix (note: not storing the result of each repetition)
sigTests <- matrix(0, length(Ns), 3)

# and away we go!
for (ni in seq_along(Ns)) {
  # loop over experiment repetitions
  n <- Ns[ni]
  for (i in 1:nRepetitions) {
    # p-values and corrections
    pvals <- runif(n, .001, .25)**2
    bon_thresh <- .05 / n
    q <- p.adjust(pvals, "BH")
    
    # record the results (note the summation)
    sigTests[ni, 1] <- sigTests[ni, 1] + 100 * mean(pvals < .05)
    sigTests[ni, 2] <- sigTests[ni, 2] + 100 * mean(q < .05)
    sigTests[ni, 3] <- sigTests[ni, 3] + 100 * mean(pvals < bon_thresh)
  }
}

# the code above keeps summing, so now divide by M repetitions to average
sigTests <- sigTests / nRepetitions

# now for the visualization
fig <- ggplot(data.frame(x=Ns), aes(x=x)) +
         geom_point(aes(y=sigTests[, 1], shape="1_unc", fill="1_unc"), size=3) +
         geom_point(aes(y=sigTests[, 2], shape="2_fdr", fill="2_fdr"), size=3) +
         geom_point(aes(y=sigTests[, 3], shape="3_bon", fill="3_bon"), size=3) +
         scale_x_continuous(limits=c(0, 510), expand=c(0, 0)) +
         scale_y_continuous(limits=c(0, 95), breaks=seq(0, 80, 20), expand=c(0, 0)) +
         labs(x="Number of p-values", y="Average % sub-threshold p-vals") +
         theme_classic()

s <- c("Uncorrected", "FDR", "Bonferroni")
fig <- fig + scale_shape_manual(name="Legend", labels=s, values=c(24, 21, 22)) +
         scale_fill_manual(name="Legend", labels=s, values=c("gray20", "gray50", "gray80")) +
         # scale_x_log10(labels=trans_format("log10")) +
         theme(legend.title=element_blank(),
               legend.position=c(.879, .44),
               legend.text.align=0,
               legend.background=element_blank(),
               legend.box.background=element_rect(color="gray"))

fig
ggsave(filename="hyp_ex7.png", width=7, height=4, units="in", dpi=300)

# In case you were wondering: the motivation for logarithmic scaling of set size
# is the most of the interesting action happens with small samples. You can try
# using a linear increase, or try setting the x-axis scale to be logarithmic.

















#-------- Exercise 8 ---------#
#-----------------------------#
# parameters
sampleSizeSkip <- 3
sampleSizeMax <- 201

# initialize data variable and output vector
data <- rnorm(5)
pvals <- numeric()
ssizes <- numeric()

while (length(data) < sampleSizeMax) {
  # compute the p-value and sample sizes
  pvals <- append(pvals, t.test(data, mu=0)$p.value)
  ssizes <- append(ssizes, length(data))
  
  # add more data!
  data <- append(data, rnorm(sampleSizeSkip))
}

df <- data.frame(x=ssizes, y=pvals)
fig <- ggplot(df, aes(x=x, y=y)) +
         geom_point(shape=21, size=5, color="black", fill="gray80") +
         geom_hline(yintercept=.05, color="black", linetype="dashed") +
         scale_x_continuous(breaks=seq(0, 200, 25)) +
         scale_y_continuous(limits=c(0, 1.05), breaks=seq(0, 1, .2), expand=c(0, 0)) +
         labs(x="Sample sizes", y="P-value", title="P-values in random Gaussian numbers") +
         theme_classic() +
         theme(plot.title=element_text(hjust=.5))

fig
ggsave(filename="hyp_ex8.png", width=8, height=4, units="in", dpi=300)





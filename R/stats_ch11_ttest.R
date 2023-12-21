#----
# Modern statistics: Intuition, Math, Python, R
## Mike X Cohen (sincxpress.com)
#### https://www.amazon.com/dp/B0CQRGWGLY
#### Code for chapter 11 (t-test family)

# About this code file:
### This code file will reproduce most of the figures in this chapter 
### (some figures were made in Inkscape), and illustrate the statistical 
### concepts explained in the text. The point of providing the code is not 
### just for you to recreate the figures, but for you to modify, adapt, 
### explore, and experiment with the code.
###
### Solutions to all exercises are at the bottom of the file.
# Thanks to Lucicleyton Farias for help with the translation from Python.



# import necessary libraries:
library(ggplot2)
library(dplyr)
library(tidyr)
library(glue)
library(stringr)
library(car)
library(purrr)
library(patchwork)
library(extraDistr)
library(LaplacesDemon)
library(emg)
library(extraDistr)

# Base ggplot theme used in the following plots
myTheme = theme(
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"),
        axis.title = element_text(size=15),
        axis.text = element_text(size=15, color="black"),
        title = element_text(size=15),
        legend.title = element_blank(),
        legend.box.background = element_rect(color="gray", linewidth = 1),
        legend.text = element_text(size = 12),
        legend.key = element_blank(),
        legend.key.width = unit(1, "cm"),
    )













#-------- Figure 11.1: Goals of the t-test ---------#
#---------------------------------------------------#
## panel A: one-sample t-test ----
data = rnorm(n = 30, mean = .5)
# convert to DF:
data = tibble(data_index = 1:length(data), data_value = data)
# plot:
pA = ggplot(data=data, aes(x=data_index, y = data_value)) + 
    geom_point(
        size=5, 
        shape=21,
        color = "black",
        fill = "gray"
        ) + 
    geom_hline(yintercept = 0, linetype="dashed") + 
    theme(
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"),
        axis.title = element_text(size=15),
        axis.text = element_text(size=15),
        title = element_text(size=15)
    ) + 
    labs(title = bquote(bold("A)")~"One sample"),
         x = "Data index", y = "Data value")

## panel B: paired-samples t-test ----
N = 20
data1 = rnorm(n=N)
data2 = data1 + .5 + rnorm(N)*.4

pB = ggplot() + 
     theme(
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"),
        axis.title = element_text(size=15),
        axis.text = element_text(size=15, color="black"),
        title = element_text(size=15)
    )

for (i in 1:N) {
    # i =1
    data = tibble(data_index = factor(c(0, 1)), 
                  data_value = c(data1[[i]], data2[[i]]))
    # pick a random color
    rgb_code = runif(1,min = 0, max = .8)
    color_ = rgb(rgb_code, rgb_code, rgb_code)
    # plotting
    pB = pB + 
        aes(x=data_index, y=data_value, group=1) + 
        geom_line(data=data) +
        geom_point(
            data=data,
            size = 5,
            color = color_,
            fill = color_,
            shape=21) 
        
}

pB = pB + 
    scale_x_discrete(labels = c("pre", "post")) + 
labs(title = bquote(bold("B)")~"Paired samples"),
         x = "", y = "Data value")

## panel C: two-samples t-test ----
pC = ggplot() + 
    theme(
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(), 
        axis.line = element_line(colour = "black"),
        axis.title = element_text(size=15),
        title = element_text(size=15),
        legend.position = c(.9, .9),
        legend.title = element_blank(),
        legend.box.background = element_rect(color="gray", linewidth = 1),
        legend.text = element_text(size = 12),
        legend.key = element_blank(),
        legend.key.width = unit(1, "cm"),
    )

for (i in seq(0, 1)) {
    # i=1
    data = rnorm(1000, i, (i+1)/2)
    # histogram
    nbreaks_ = nclass.FD(data)
    hist_ = hist(data, breaks = nbreaks_, plot=F)
    # convert to DF:
    data = tibble(data_index = hist_$mids,
                  data_value = hist_$counts,
                  category=glue("Group {i+1}"))
    # plot
    pC = pC + 
        aes(x=data_index, y = data_value, color=category) + 
        geom_line(
            data=data,
            linewidth=2
        )}

pC = pC + 
    scale_color_manual(
        values=c("Group 1" = rgb(0, 0, 0),
                 "Group 2" = rgb(0.5, 0.5, 0.5)
                 )
    ) + 
    theme(legend.position = c(.8, .8)) + 
    labs(title = bquote(bold("C)")~"Two ind. samples"),
         x = "Exam score", y = "Count")

p11.1 = pA + pB + pC + plot_layout(ncol=3)
p11.1

# saving
ggsave("ttest_ttestGoals.png", p11.1, width=18, height = 5)





















#-------- Figure 11.2: A t-pdf ---------#
#---------------------------------------#
t = seq(-4, 4, length=573)

# a pdf with df=20
tpdf = dt(t, df = 20)

# convert to DF:
data = tibble(data_index=t,
              data_value=tpdf)

p_ttest_tpdf = ggplot(data=data, aes(x=data_index, y=data_value)) + 
    geom_line() + 
    myTheme + 
    theme(axis.text.y = element_blank(),
          axis.ticks.y = element_blank()) +
    labs(y="Probability", x = "T value")


# plotting:
p_ttest_tpdf

# saving
ggsave("ttest_tpdf.png", p_ttest_tpdf, width = 3, height = 4)














#-------- Computing p-values for one-tailed and two-tailed tests ---------#
#-------------------------------------------------------------------------#
tval = 2.1
df = 13

pvalL = pt(-tval, df, lower.tail = T) # slightly different implementation from the book
pvalR = pt(tval, df, lower.tail = F)  # but the result is the same
pval2 = pvalR + pvalL

print(glue("One-tailed p-value on the left: {pvalL}"))
print(glue("One-tailed p-value on the rigth: {pvalR}"))
print(" ")
print(glue("Two-tailed p-value as the sum: {pvalR + pvalL}"))
print(glue("Two-tailed p-value by doubling: {2*pvalL}"))

# 
pvalS = pt(tval, df, lower.tail = F)
pvalC = 1 - pt(tval, df, lower.tail = T)

print(glue("P-value from 1-cdf: {pvalC}"))
print(glue("P-value from s.f.: {pvalS}"))
print(glue("Difference: {pvalC - pvalS}"))



















#-------- Figure 11.3: T-values from p-values ---------#
#------------------------------------------------------#
t = seq(-4, 4, length=75)
df = 13

# cdf based on t-values
cdf = pt(t, df) # pt is the cdf
# convert to DF:
cdf = tibble(
    data_index = t,
    data_value = cdf
)

# t-values based on cdf
pvals = seq(.001, .999, length=73)
tVals = qt(pvals, df) # qt is inverse cdf
# convert to DF
tVals = tibble(
    data_index = pvals,
    data_value = tVals
)

pA = ggplot(
    data = cdf,
    aes(x=data_index, y = data_value)
    ) + 
    geom_line(
        linewidth=2
    ) + 
    myTheme + 
    labs(y="cdf", x = "t value",
         title=bquote(bold("A)")~"CDF from t-values"))

pB = ggplot(
    data = tVals,
    aes(x=data_index, y = data_value)
    ) + 
    geom_line(
        linewidth=2
    ) + 
    myTheme + 
    labs(y="t value", x = "cdf",
         title=bquote(bold("B)")~"T-values from CDF"))

# plotting:
p11.3 = pA + pB + plot_layout(ncol=2)
p11.3            

# saving:
ggsave("ttest_tFromP.png", p11.3, width=10, height=4)






# example usage to get the t-value associated with p=.05 and df=13
pval = .05
tFromP_L = qt(pval, df, lower.tail = T) # inverse of P(X <= x)
tFromP_R1 = qt(1-pval, df, lower.tail = T) # inverse of P(X <= x)
tFromP_R2 = qt(pval, df, lower.tail = F)  # inverse of P(X > x)

print(glue('Variable tFromP_L:  {sprintf("%.3f",tFromP_L)}'))
print(glue('Variable tFromP_R1: {sprintf("%.3f", tFromP_R1)}'))
print(glue('Variable tFromP_R2: {sprintf("%.3f", tFromP_R2)}'))




















#-------- Figure 11.4: Example t-value ---------#
#-----------------------------------------------#
# empirical t-value and df:
tval = 1.6
df = 20
alpha_ = .05

# redefine the t-values and corresponding pdf
t = seq(-4, 4, length=573)
tpdf = dt(t, df = 20)

# its associated p-value (but this is one-tailed for visualization; see text and next cell!)
pval = pt(tval, df, lower.tail = F)

# critical t-value for apha
tCrit = qt(alpha_/2, df = df, lower.tail = F) # /2 for two-tailed!
pHalf = max(tpdf)/2 # 1/2 max. (vertical) p(t), used for plotting

data = tibble(
    data_index=t,
    data_value = tpdf
)

tidx1 = which.min(abs(t-tval))
tidx2 = which.min(abs(t-(tval+t[length(t)])/2))

p11.4 = ggplot(data, aes(x=data_index, y=data_value)) + 
    geom_line(
        linewidth=1
    ) +
    geom_ribbon(
        data = filter(data, data_index >= tval), 
        aes(ymax = data_value, ymin=0),
        fill="#969696") + 
    geom_vline(
        xintercept = tCrit,
        linetype = "dashed",
        color="gray"
    ) +
    annotate(
        "text", 
        x = tCrit-0.1, y = pHalf*2, 
        label = bquote(alpha~"/2" == .(alpha_/2)),
        angle = 90
    ) + 
    annotate(
        "segment", 
        x    = t[tidx2] + .8, 
        xend = 2.5,
        y    = 1.1*(pHalf/2),
        yend = tpdf[which.min(abs(t-2.5))],
        colour = "black", 
        linewidth = 2, 
        linejoin="mitre",
        lineend="butt",
        arrow = arrow(length = unit(.1, "inches"),
                                    type="closed")
    ) +  
    annotate(
        "text", 
        x = (t[tidx2]+1), y = 1.1*(pHalf/2), 
        label = bquote(.(sprintf("%.2f", 100*pval))~"%"),
    ) + 
    geom_segment(
        x = tval, xend = tval,
        y = pHalf, yend = 0,
        colour = "black", 
        arrow = arrow(length = unit(.1, "inches")) 
    ) + 
    geom_segment(
        x = tval+1, xend = tval,
        y = pHalf, yend = pHalf,
        colour = "black"
    ) +
    annotate(
        "text", size=6,
        x = 1.15*(t[tidx2]), y = pHalf, 
        label = bquote(t[df]~"="~frac(bar(x)-mu,'s /'~sqrt(n))~"="~.(tval))
    ) +
    xlim(-1, t[length(t)]) + 
    ylim(0, pHalf*2.1) + 
    myTheme + 
    labs(y = bquote(rho ~ "(t/" ~ H[0] ~ ")"), 
         x = "T value")

p11.4

# saving:
ggsave("ttest_tEmpWEq.png", p11.4, width=7, height=5)































#-------- Figure 11.5: Completion of the previous figure to show both tails ---------#
#------------------------------------------------------------------------------------#
# plot the t distribution
p11.5 = ggplot(data, aes(x=data_index, y=data_value)) + 
    geom_line(
        linewidth=1
    )

# plot shaded area and dashed line for the critical t-value on the right side
p11.5 = p11.5 +
    geom_ribbon(
        data = filter(data, data_index >= tval), 
        aes(ymax = data_value, ymin=0),
        fill="#969696") + 
    geom_vline(
        xintercept = tCrit,
        linetype = "dashed",
        color="gray"
    ) +
    annotate(
        "text", 
        x = tCrit-0.2, y = pHalf*2, 
        label = bquote(alpha~"/2" == .(alpha_/2)),
        angle = 90
    ) +
    geom_segment(
        x = tval, xend = tval,
        y = pHalf, yend = 0,
        colour = "black", 
        arrow = arrow(length = unit(.1, "inches")) 
    ) + 
    geom_label(
        aes(x=tval, y = 1.05*pHalf,
            label="label", 
            colour="white")
    ) + 
    annotate(
        "label",
        size=6,
        x = tval, y = 1.05*pHalf, 
        label.size=0,
        label = glue("t={tval}")
    )
    
## and again for the left side
p11.5 = 
    p11.5 + 
    geom_ribbon(
        data = filter(data, data_index <= -tval), 
        aes(ymax = data_value, ymin=0),
        fill="#969696") + 
    geom_vline(
        xintercept = -tCrit,
        linetype = "dashed",
        color="gray"
    ) + 
    annotate(
        "text", 
        x = -tCrit+0.2, y = pHalf*2, 
        label = bquote(alpha~"/2" == .(alpha_/2)),
        angle = 90
    ) + 
    geom_segment(
        x = -tval, xend = -tval,
        y = pHalf, yend = 0,
        colour = "black", 
        arrow = arrow(length = unit(.1, "inches")) 
    ) + 
    geom_label(
        aes(x=-tval, y = 1.05*pHalf,
            label="label", 
            colour="white")
    ) + 
    annotate(
        "label",
        size=6,
        x = -tval, y = 1.05*pHalf, 
        label.size=0,
        label = glue("t={tval}")
    )
   
## arrow and formula for the empirical t-value
# left:
tidx_left = which.min(abs(t-(-tval+t[1])/2))

p11.5 = p11.5 + 
    annotate(
        "segment", 
        x = t[tidx_left]-.5, 
        xend = t[tidx_left],
        y = pHalf/2,
        yend = tpdf[tidx_left+12],
        colour = "black", 
        linewidth = 2, 
        linejoin="mitre",
        lineend="butt",
        arrow = arrow(length = unit(.1, "inches"),
                                    type="closed")
    ) + 
    annotate(
        "text", 
        x = (t[tidx_left]-.5), y = 1.1*(pHalf/2), 
        label = bquote(.(sprintf("%.2f", 100*pval))~"%"),
    ) 

# right:
tidx_right = which.min(abs(t-(tval+t[length(t)])/2))

p11.5 = p11.5 + annotate(
        "segment", 
        x = t[tidx_right] + 1, 
        xend = t[tidx_right],
        y = pHalf/2,
        yend = tpdf[tidx_right-12],
        colour = "black", 
        linewidth = 2, 
        linejoin="mitre",
        lineend="butt",
        arrow = arrow(length = unit(.1, "inches"),
                                    type="closed")
    ) + 
    annotate(
        "text", 
        x = (t[tidx_right]+1), y = 1.1*(pHalf/2), 
        label = bquote(.(sprintf("%.2f", 100*pval))~"%"),
    )

# final adjustments
p11.5 = p11.5 + 
    ylim(0, pHalf*2.1) + 
    myTheme + 
    theme(legend.position = "none",
          axis.ticks.y = element_blank(),
          axis.text.y = element_blank()) + 
    labs(y = bquote(rho ~ "(t/" ~ H[0] ~ ")"), 
         x = "T value")

# plotting:
p11.5

# saving:
ggsave("ttest_tEmpWEq2.png", p11.5, width=8, height=6)























#-------- Figure 11.6: Testing for normality ---------#
#-----------------------------------------------------#
# the data 
data1 = rnorm(100)
data2 = exp(rnorm(100))

# omnibus test:
Otest1 = nortest::pearson.test(data1)
Otest2 = nortest::pearson.test(data2)
    
# Shapiro's test:
Stest1 = shapiro.test(data1)
Stest2 = shapiro.test(data2)

# report the results:
print(glue("Omnibus test in X1 (H0=normal): p={sprintf('%.3f', Otest1$p.value)}"))
print(glue("Omnibus test in X2 (H0=normal): p={sprintf('%.3f', Otest2$p.value)}"))
print(glue("Shapiro test in X1 (H0=normal): p={sprintf('%.3f', Stest1$p.value)}"))
print(glue("Shapiro test in X2 (H0=normal): p={sprintf('%.3f', Stest2$p.value)}"))

# show the histograms
nbreaks1 = nclass.FD(data1)
data1_df = hist(data1, breaks = nbreaks1, plot = F)
data1_df = tibble(
    data_index = data1_df$mids,
    data_value = data1_df$counts
)

nbreaks2 = nclass.FD(data2)
data2_df = hist(data2, breaks = nbreaks2, plot = F)
data2_df = tibble(
    data_index = data2_df$mids,
    data_value = data2_df$counts
)

# plotting:
p11.6 = ggplot() + 
    geom_line(
        data=data1_df,
        aes(x=data_index, y=data_value, 
            linetype="X1", color="X1"),
        linewidth=1
    ) + 
    geom_line(
        data=data2_df,
        aes(x=data_index, y=data_value, 
            linetype="X2", color="X2"),
        linewidth=1
    )

# final adjustments:
p11.6 = p11.6 +
    scale_color_manual(
        values = c("X1" = "black",
                   "X2" = "gray")
    ) + 
    scale_linetype_manual(
        values = c("X1" = "dashed",
                   "X2" = "solid")
    ) + 
    myTheme + 
    theme(legend.position = c(.8, .8)) + 
    labs(color="", linetype="", 
         y="Count", x="Data value")

p11.6

# saving:
ggsave("ttest_normTests.png", p11.6, width=4, height=4)




























#-------- Figure 11.7: Increasing the t-value ---------#
#------------------------------------------------------#
x = seq(-4, 4, length=501)

## panel A: probably not significant
g1_A = tibble(data_index = x,
            data_value = dnorm(x, -.3, 1))
g2_A = tibble(data_index = x,
            data_value = dnorm(x, .3, 1))

p11.7_A = ggplot() + 
    geom_line(
        data=g1_A, 
        aes(x=data_index, y=data_value,
            linetype="g1", color="g1"),
        linewidth=1
    ) + 
    geom_line(
        data=g2_A, 
        aes(x=data_index, y=data_value,
            linetype="g2", color="g2"),
        linewidth=1
    ) +
    scale_color_manual(
        values = c("g1" = "black",
                   "g2" = "gray")
    ) + 
    scale_linetype_manual(
        values = c("g1" = "solid",
                   "g2" = "dashed")
    ) + 
    myTheme + 
    theme(
        legend.position = "none",
        axis.ticks = element_blank(),
        axis.text = element_blank()
    ) + 
    labs(title=bquote(bold("A)")~"Non-significant"),
         x="",y="Probability")

# Panel B: significant by larger mean difference
g1_B = tibble(data_index=x, data_value=dnorm(x, -1, 1))
g2_B = tibble(data_index=x, data_value=dnorm(x, 1, 1))

p11.7_B = ggplot() + 
    geom_line(
        data=g1_B, 
        aes(x=data_index, y=data_value,
            linetype="g1", color="g1"),
        linewidth=1
    ) + 
    geom_line(
        data=g2_B, 
        aes(x=data_index, y=data_value,
            linetype="g2", color="g2"),
        linewidth=1
    ) +
    scale_color_manual(
        values = c("g1" = "black",
                   "g2" = "gray")
    ) + 
    scale_linetype_manual(
        values = c("g1" = "solid",
                   "g2" = "dashed")
    ) + 
    myTheme + 
    theme(
        legend.position = "none",
        axis.ticks = element_blank(),
        axis.text = element_blank()
    ) + 
    labs(title=bquote(bold("B)")~"Large mean distance"),
         x="",y="Probability")

# panel C: significant by reduced variance
g1_C = tibble(data_index=x, data_value=dnorm(x, -.3, .2))
g2_C = tibble(data_index=x, data_value=dnorm(x, .3, .2))

p11.7_C = ggplot() + 
    geom_line(
        data=g1_C, 
        aes(x=data_index, y=data_value,
            linetype="g1", color="g1"),
        linewidth=1
    ) + 
    geom_line(
        data=g2_C, 
        aes(x=data_index, y=data_value,
            linetype="g2", color="g2"),
        linewidth=1
    ) +
    scale_color_manual(
        values = c("g1" = "black",
                   "g2" = "gray")
    ) + 
    scale_linetype_manual(
        values = c("g1" = "solid",
                   "g2" = "dashed")
    ) + 
    myTheme + 
    theme(
        legend.position = "none",
        axis.ticks = element_blank(),
        axis.text = element_blank()
    ) + 
    labs(title=bquote(bold("C)")~"Low variance"),
         x="",y="Probability")

p11.7 = p11.7_A + p11.7_B + p11.7_C + plot_layout(ncol=3)
p11.7

# saving:
ggsave("ttest_sigMecs.png", p11.7, width=10, height = 4)



























#-------- One-sample T-test ---------#
#------------------------------------#
# given data
X = c(80, 85, 90, 70, 75, 72, 88, 77, 82, 65, 79, 81, 74, 86, 68)
h0 = 75

# descriptives
meanX = mean(X)
stdX = sd(X)
ssize = length(X)

# t-value
tval = (meanX - h0) / (stdX/sqrt(ssize))

# p-value
pval = 1 - pt(tval, ssize-1)
pval = 2*pval

# print everything out!
sprintf("Sample mean: %.2f", meanX)
sprintf("Sample std: %.2f", stdX)
print(glue("Sample size: {ssize}"))
sprintf("T-value: %.3f", tval)
sprintf("p-value: %.3f", pval)


# Repeat using the stats library:
ttest = t.test(X, mu=h0)

# the output variable is its own type:
print(class(ttest))

# let's print the results:
print("Results from t.test:")
print(sprintf("t(%d) = %.3f, p<%.3f", ttest$parameter, ttest$statistic, ttest$p.value))

# btw, data are consistent with a normal distribution
print(sprintf("Shapiro p-value = %.2f", shapiro.test(X)$p.value))






























#-------- Figure 11.8: Paired-samples t-test ---------#
#-----------------------------------------------------#
# the data
Xn = c( 60, 52, 90, 20, 33, 95, 18, 47, 78, 65 )
Xq = c( 65, 60, 84, 23, 37, 95, 17, 53, 88, 66 )
sampsize = length(Xn)

# their difference
Delta = Xq-Xn

# Visualize
data = tibble(data_index=1:sampsize,
              Xn = Xn,
              Xq = Xq,
              Delta = Delta)
data = pivot_longer(data, cols = c("Xn", "Xq", "Delta"))

p11.8_A = ggplot(filter(data, name != "Delta"),
       aes(x = factor(name), y=value, group = data_index)) + 
    geom_line(
        color="gray"
    ) + 
    geom_point(
        shape=21,
        size=5,
        fill="gray"
    ) + 
    ylim(c(0, 100)) + 
    myTheme + 
    scale_x_discrete(labels = c(bquote(X[N]), bquote(X[Q]))) + 
    theme(
        axis.text = element_text(color="black"),
        axis.text.x = element_text(vjust=-2, hjust = .5),
    ) + 
    labs(title=bquote(bold("A)")~"Raw data"),
         y="Scores", x = "")

p11.8_B = ggplot(data=filter(data, name == "Delta"),
       aes(x = factor(name), y = value)) + 
    geom_hline(yintercept = 0, linetype="dashed") + 
    geom_point(
        size=5,
        shape=21,
        fill="gray"
    ) +
    ylim(c(-50, 50)) + 
    scale_x_discrete(labels= c(bquote(Delta))) + 
    myTheme + 
    theme(
        axis.text.x = element_text(vjust=-2, hjust = .5),
    ) + 
    labs(y = "Difference scores",
         x = "",
         title=bquote(bold("B)")~"Differences"))

# final plot
p11.8 = p11.8_A + p11.8_B
p11.8

# saving:
ggsave("ttest_pairedTtest.png", p11.8, width = 8, height = 4)


### test it!
ttest = t.test(Delta, mu=0)

# print the results:
print(sprintf("t(%d) = %.3f, p<%.3f", ttest$parameter, ttest$statistic, ttest$p.value))

# btw, data are consistent with a normal distribution
print(sprintf("Xn Shapiro p-value = %.2f", shapiro.test(Xn)$p.value))
print(sprintf("Xq Shapiro p-value = %.2f", shapiro.test(Xq)$p.value))
print(sprintf("Xy Shapiro p-value = %.2f", shapiro.test(Delta)$p.value))





























#-------- Figure 11.9: Example of 2-sample ttest ---------#
#---------------------------------------------------------#
### generate data

# Exponentially modified normal:
x1 = remg(50, lambda = 1/3) # lambda = 1/K
# Gumbel distribution
x2 = extraDistr::rgumbel(n = 42)

# panel A:
data1_A = tibble(
    data_index = "x1", 
    data_value = x1
)

data2_A = tibble(
    data_index="x2", 
    data_value=x2
)

data_A = bind_rows(data1_A, data2_A)

p11.9_A = ggplot(data = data_A,
       aes(x=factor(data_index), y=data_value, 
           group=data_index,
           shape=data_index,
           fill=data_index)) + 
    geom_point(
        size=5,
        alpha=.8
    ) +
    scale_x_discrete(
        labels= c(bquote(X[1]), bquote(X[2]))
    ) +
    scale_fill_manual(
        values=c("x1" = "gray",
                 "x2" = "gray"
                 )
    ) + 
    scale_shape_manual(
        values = c("x1" = 21,
                   "x2" = 22)
    ) + 
    myTheme + 
    theme(legend.position = "none") + 
    labs(x="", y="",
         title=bquote(bold("A)")~"Data"))

# panel B:
nbreaks1_B = nclass.FD(x1)
data1_B = hist(x1, breaks=nbreaks1_B, plot=F)
data1_B = tibble(data_index=data1_B$mids,
               data_value=data1_B$counts,
               group="x1")
nbreaks2_B = nclass.FD(x2)
data2_B = hist(x2, breaks=nbreaks2_B, plot=F)
data2_B = tibble(data_index=data2_B$mids,
                 data_value=data2_B$counts,
                 group="x2")

data_B = bind_rows(data1_B, data2_B)

p11.9_B = ggplot(
    data=data_B, 
    aes(x = data_index, y = data_value, group=group,
        color=group, shape=group, fill=group, linetype=group)
    ) + 
    geom_line(
        linewidth=1
    ) + 
    geom_point(
        size=5,
        alpha=.8,
        stroke=1
    ) + 
    scale_fill_manual(
        values=c("x1" = "gray",
                 "x2" = "gray"),
        labels=c("x1" = bquote(X[1]),
                 "x2" = bquote(X[2]))
    ) + 
    scale_linetype_manual(
        values = c("x1" = "dashed",
                   "x2" = "solid"),
        labels=c("x1" = bquote(X[1]),
                 "x2" = bquote(X[2]))
    ) + 
    scale_color_manual(
        values=c("x1" = "#737373",
                 "x2" = "#737373"),
        labels=c("x1" = bquote(X[1]),
                 "x2" = bquote(X[2]))
    ) + 
    scale_shape_manual(
        values = c("x1" = 21,
                   "x2" = 22),
        labels=c("x1" = bquote(X[1]),
                 "x2" = bquote(X[2]))
    ) + 
    myTheme + 
    theme(legend.position = c(.8, .8)) + 
    labs(y = "Count", x = "Data value",
         title = bquote(bold("B)")~"Distributions"))
# final plot
p11.9 = p11.9_A + p11.9_B
p11.9

# saving:
ggsave("ttest_indTtest.png", p11.9, width=10,  height = 3.5)

# doubling rubric
s1 = sd(x1)
s2 = sd(x2)

# report
print(sprintf("Standard deviations are %.2f and %.2f", s1, s2))
print(sprintf("Ratio of max:min stdevs is %.2f", max(s1, s2)/min(s1, s2)))

# Levene's test
data_levene_ = bind_rows(
    tibble(group="x1", data=x1),
    tibble(group="x2", data=x2),
)

lres = leveneTest(data~group, data_levene_)

print(sprintf("Levene's test for homogeneity of variance: W=%.2f, p=%.3f",
              lres$`F value`[1], lres$`Pr(>F)`[1]))


## tests for normal distribution
# omnibus test
Otest1 = nortest::pearson.test(x1)
Otest2 = nortest::pearson.test(x2)

print(sprintf("Omnibus test in X1 (H0 = normal): p=%.3f", Otest1$p.value))
print(sprintf("Omnibus test in X2 (H0 = normal): p=%.3f", Otest2$p.value))

# Shapiro's test
Stest1 = shapiro.test(x1)
Stest2 = shapiro.test(x2)

print(sprintf("Shapiro test in X1 (H0 = normal): p=%.3f", Stest1$p.value))
print(sprintf("Shapiro test in X2 (H0 = normal): p=%.3f", Stest2$p.value))

# now for the t-test
tres = t.test(x1, x2, var.equal = F)
print(sprintf("t=%.2f, p=%.3f", tres$statistic, tres$p.value))

# FYI, here's the result assuming equal variance (see also Exercise 9)
tres = tres = t.test(x1, x2, var.equal = T)
print(sprintf("t=%.2f, p=%.3f", tres$statistic, tres$p.value))






















#-------- Figure 11.10: Wilcoxon signed-rank ---------#
#-----------------------------------------------------#
# the data
h0 = 1
x = rnorm(100)^2

# panel A:
data_A = tibble(
    data_index = 1:length(x),
    data_value = x
)

p11.10_A = ggplot(
    data=data_A,
    aes(x = data_index,
        y = data_value)
    ) + 
    geom_point(
        alpha=.6,
        size=5,
        shape=21,
        fill="gray"
    ) + 
    geom_hline(
        yintercept = h0,
        linewidth=1,
        linetype="dashed",
        color="#737373"
    ) + 
    geom_hline(
        yintercept = median(data_A$data_value),
        linewidth=1
    ) + 
    myTheme + 
    labs(x = "Data index", y="Data value",
         title = bquote(bold("A)")~"Data"))

# panel B:
nbreaks_B = nclass.FD(x)
data_B = hist(x, breaks=nbreaks_B, plot=F)
data_B = tibble(
    data_index = data_B$mids,
    data_value = data_B$counts
)

p11.10_B = ggplot(
    data = data_B, 
    aes(x = data_index, y = data_value)
) + 
    geom_col(
        aes(fill="Data"),
        color="black",
    ) + 
    geom_vline(
        aes(color="H0", xintercept=h0, linetype="H0"),
        linewidth=1,
        key_glyph = "path"
    ) + 
    geom_vline(
        aes(xintercept = median(data_A$data_value),
            color="Median", linetype="Median"),
        linewidth=1,
        key_glyph = "path"
    ) + 
    scale_fill_manual(
        name = NULL,
        values = c("Data" = "gray")
    ) +
    scale_color_manual(
        name = NULL,
        values = c("H0"="#737373",
                   "Median"='black'),
    ) + 
    scale_linetype_manual(
        name = NULL,
        values = c("H0" = "dashed",
                   "Median" = "solid")
    ) + 
    myTheme + 
    theme(
        legend.position = c(.8, .8),
        legend.spacing.x = unit(.3, 'cm'),
        legend.background = element_rect(fill=NA),
        legend.spacing = unit(-15, "pt")
    ) + 
    labs(color="", fill="", 
         y="Count", x="Data value", linetype="",
         title=bquote(bold("B)")~"Distribution"))

# plot:
p11.10 = p11.10_A + p11.10_B
p11.10

# saving
ggsave("ttest_ranktest.png", p11.10, width=7, height=3.35)

# the test!
wtest = wilcox.test(
    x - h0, 
    exact = F, # no exact calculation for the p-value
    correct = F, # not applying the continuity correction
    alternative = "two.sided"
)

# and print the results
# R does not provide the zstatistic. 
# displaying the statistics:
print(sprintf("Wilcoxon test: W=%.2f, p=%.3f", wtest$statistic, wtest$p.value))































#-------- Figure 11.11: Margin figure about the sign of z ---------#
#------------------------------------------------------------------#
### NOTE ABOUT THIS FIGURE:
# The purpose of this figure in the book is to highlight a "feature" of
# the Wilcoxon test in Python. The interpretation is specific to Python,
# because R does not return the Z value. Nonetheless, the code is provided
# here in case you'd like to explore it :)

# panel A:
# create and shift data
dataA = tibble(
    data_index=1:30,
    data_value=rnorm(30) - 1,
)

zA = wilcox.test(
    dataA$data_value, 
    exact = F,
    correct = F,
    alternative = "two.sided"
)$statistic    

p11.11_A = ggplot(
    data=dataA, 
    aes(x=data_value, y=data_index) 
    ) + 
    geom_point(
       size = 4,
       shape=21,
       fill="black"
    ) +
    geom_vline(
        xintercept = 0,
        linewidth=1,
        color="gray"
    ) + 
    myTheme + 
    labs(y="Data index", x = "Data values",
         title= glue("Wilcoxon W={zA}"))

# panel B:
# create and shift data
dataB = tibble(
    data_index=1:30,
    data_value=rnorm(30) + 1,
)
zB = wilcox.test(
    dataB$data_value,
    exact = F,
    correct = F,
    alternative = "two.sided"
)$statistic    

p11.11_B = ggplot(
    data=dataB, 
    aes(x=data_value, y=data_index) 
    ) + 
    geom_point(
       size = 4,
       shape=21,
       fill="black"
    ) +
    geom_vline(
        xintercept = 0,
        linewidth=1,
        color="gray"
    ) + 
    myTheme + 
    labs(y="Data index", x = "Data values",
         title= glue("Wilcoxon W={zB}"))

# final plot
p11.11 = p11.11_A / p11.11_B
p11.11

# saving:
ggsave("ttest_wilcoxonSign.png", p11.11, width=4, height=6)

























#-------- Mann-Whitney U test ---------#
#--------------------------------------#
# same data as we used for the independent-samples t test

# Exponentially modified normal:
x1 = remg(50, lambda = 1/3) # lambda = 1/K
# Gumbel distribution
x2 = extraDistr::rgumbel(n = 42)

# MW-U test
mwu = wilcox.test(x1, x2)
print(sprintf("U=%.0f, p=%.3f", mwu$statistic, mwu$p.value))

# parametric t-tet (dives the same statistical conclusion as the MWU)
tres = t.test(x1, x2, var.equal = F)
print(sprintf("t=%.0f, p=%.3f", tres$statistic, tres$p.value))























#-------- Exercise 1 ---------#
#-----------------------------#
# parameters 
N = 50
h0 = -pi/2

# create the dataset:
X = LaplacesDemon::ralaplace(
    n = N, 
    scale = sqrt(2), 
    kappa = 2
)
dataMean = mean(X)

data_ = tibble(
    data_index = 1:N,
    data_value = X
)

pE1_A = ggplot(data = data_, aes(x=data_index,y=data_value)) + 
    geom_point(
        shape=22,
        size=3,
        fill = "gray",
        alpha=.8
    ) + 
    geom_hline(
        yintercept = h0,
        linetype="dashed",
        color="black",
        linewidth=1
    ) + 
    geom_hline(
        yintercept = dataMean,
        linetype="dotted",
        color="black",
        linewidth=1
    ) + 
    myTheme + 
    labs(x = "Data index", y = "Data Value",
         title=bquote(bold("A)")~"Raw Data"))

# panel B:
data_B = hist(X, breaks = nclass.FD(X), plot = F)
data_B = tibble(
    data_index = data_B$mids,
    data_value = data_B$counts
)

pE1_B = ggplot(
    data = data_B, 
    aes(x = data_index, y = data_value)
) + 
    geom_col(
        fill="gray",
        color="black",
    ) + 
    geom_vline(
        aes(color="H0 value", xintercept=h0, linetype="H0 value"),
        linewidth=1,
        key_glyph = "path"
    ) + 
    geom_vline(
        aes(xintercept = dataMean,
            color="Emp. mean", linetype="Emp. mean"),
        linewidth=1,
        key_glyph = "path"
    ) +
    scale_color_manual(
        name = NULL,
        values = c("H0 value"="#737373",
                   "Emp. mean"='black'),
    ) + 
    scale_linetype_manual(
        name = NULL,
        values = c("H0 value" = "dashed",
                   "Emp. mean" = "dotted")
    ) + 
    myTheme + 
    theme(
        legend.position = "right",
        legend.spacing.x = unit(.3, 'cm'),
        legend.background = element_rect(fill=NA),
        legend.spacing = unit(-15, "pt"),
    ) + 
    labs(color="", fill="", 
         y="Count", x="Data value", linetype="",
         title=bquote(bold("B)")~"Histogram and means"))

# final plot
pE1 = pE1_A + pE1_B
pE1

# saving:
ggsave("ttest_ex1.png", pE1, width=15, height = 5)



### now for the test

# manual calculation
t_num = dataMean - h0
t_den = sd(X) / sqrt(N)

tval = t_num / t_den
pval = pt(abs(tval), df=N-1, lower.tail = F)
pval = 2*pval # double it for 2-tailed

# using stats
r = t.test(X, mu=h0)
t = r$statistic
df = r$parameter
p = r$p.value

# print both results:
sprintf("manuel ttest: t(%d) = %.3f, p=%.3f", N-1, tval, pval)
sprintf("manuel ttest: t(%d) = %.3f, p=%.3f", N-1, t, p)
























#-------- Exercise 2 ---------#
#-----------------------------#
# how often do we get subthreshold results?
nExps = 500
issig = vector("logical", nExps)
means = vector("numeric", nExps)
stds  = vector("numeric", nExps)

# run the experiment
#   (Note: For a small extra challenge, you could re-implement this
#    without a for-loop using matrix input, after completing the next
#    exercise.)
for( i in seq(1, nExps, by=1) ){
    # generate data and store the mean/std
    X = LaplacesDemon::ralaplace(
        n = N, 
        scale = sqrt(2), 
        kappa = 2
    )
    means[i] = mean(X)
    stds[i] = sd(X)
    # run the ttest and store if "significant"
    r = t.test(X, mu = h0, alternative = "two.sided")
    issig[i] = r$p.value < .05
}

print(glue("p<.05 in {sum(issig)}/{nExps} times."))

# show the results
pE2_A = ggplot() + 
    geom_point(
        data = tibble(data_index=rnorm(sum(issig))/40,
                     data_value=means[issig]), 
        aes(x=data_index, y=data_value),
        shape=21,
        size=3,
        fill="gray",
        alpha=.6
    ) + 
    geom_point(
        data = tibble(data_index=rnorm(sum(!issig))/40 + 1,
                      data_value=means[!issig]), 
        aes(x=data_index, y=data_value),
        shape=22,
        size=3,
        fill="gray",
        alpha=.6
    )  +
    scale_x_continuous(
        breaks = c(0, 1),
        labels= c("p<.05", "p>.05")
    ) +
    myTheme + 
    labs(y="", x = "",
         title=bquote(bold("A)")~"Sample Means")) 

pE2_B = ggplot() + 
    geom_point(
        data = tibble(data_index=rnorm(sum(issig))/40,
                     data_value=stds[issig]), 
        aes(x=data_index, y=data_value),
        shape=21,
        size=3,
        fill="gray",
        alpha=.6
    ) + 
    geom_point(
        data = tibble(data_index=rnorm(sum(!issig))/40 + 1,
                      data_value=stds[!issig]), 
        aes(x=data_index, y=data_value),
        shape=22,
        size=3,
        fill="gray",
        alpha=.6
    )  +
    scale_x_continuous(
        breaks = c(0, 1),
        labels= c("p<.05", "p>.05")
    ) +
    myTheme + 
    labs(y="", x = "",
         title=bquote(bold("A)")~"Sample stds.")) 

# final plot
pE2 = pE2_A + pE2_B    
pE2

# saving:
ggsave("ttest_ex2.png", pE2, width=7, height=3)





















#-------- Exercise 3 ---------#
#-----------------------------#
NperSample = 40
MDatasets = 25

# data
X = matrix(rnorm(NperSample*MDatasets, mean = 1, sd = 1),
           nrow=NperSample, ncol = MDatasets)

print("Data size should be sample-size X dataset:")
print(dim(X))

# t.test with matrix input:
ttest_matrix = apply(X, 2, t.test, mu=0)

# t.test in for-loop over each column (each dataset);
ttest_4loop = vector("list", MDatasets)
for (i in seq(1, MDatasets)) {
    ttest_4loop[[i]] = t.test(X[, i], mu=0)
}

# print the results:
print("Matrix | Vector")
print("------ | ------")
for (i in seq(1,MDatasets)){
    print(sprintf("%.4f | %.4f", ttest_matrix[[i]]$statistic, ttest_4loop[[i]]$statistic))
}



















#-------- Exercise 4 ---------#
#-----------------------------#
# data parameters:
N = 40
k = 300

stds = seq(.1, 3, length=k)

# initiate the t/p vectors:
t = vector("numeric", k)
p = vector("numeric", k)
s = vector("numeric", k)

for (i in seq(1, k)) {
    X = rnorm(n = N, mean = 0, sd = stds[[i]])
    X = X - mean(X) + .5 # force mean = .5
    ttest = t.test(X, mu = 0)
    t[[i]] = ttest$statistic
    p[[i]] = ttest$p.value
    # get the sample std (used in exercise 5)
    s[[i]] = sd(X)
}

# and now plotting:
pE4_A = ggplot(
    data = tibble(data_index = stds,
                  data_value = t),
    aes(x = data_index, y = data_value)
    ) + 
    geom_point(
        shape = 22,
        color="black",
        size=4
    ) + 
    myTheme + 
    labs(y = "t-value",
         x = "Standard deviation",
         title=bquote(bold("A)")~"T-values"))

pE4_B = 
    ggplot(
        data = tibble(data_index = stds,
                      data_value = p),
        aes(x = data_index,
            y = data_value)
    ) + 
    geom_point(
        shape=22,
        color="black",
        size=3
    ) + 
    myTheme + 
    labs(y = "p-value",
         x = "Standard deviation",
         title=bquote(bold("B)")~"P-values"))

# panel C:
pE4_C = ggplot(
    data = tibble(data_index=t,
                  data_value=p),
    aes(x=data_index, y=data_value)
) + 
    geom_point(
        shape=22,
        color="black",
        size=3
    ) + 
    myTheme + 
    labs(y="p-value", x="T-value",
         title=bquote(bold("C)")~"p by t"))

# final plot
pE4 = pE4_A + pE4_B + pE4_C 
pE4

# save
ggsave("ttest_ex4.png", pE4, width=10, height = 3)





















#-------- Exercise 5 ---------#
#-----------------------------#
# No, it doesn't really matter, because even with N=40, the sample
# standard deviation is a fairly accurate estimate of the population 
# standard deviation, certainly for this range of standard deviation
# values.

# You can recreate the figure by replacing variable 'stds' with 's' 
# in the code above, and by demonstrating the strong correlation 
# between sample and theoretical standard deviation.

# Here's the correlation coefficient 
# (values close to 1 indicate a very strong relationship)
r = cor.test(stds,s)

# plot
pE5 = ggplot(
    data = tibble(data_index = stds,
                  data_value = s),
    aes(x = data_index, y = data_value)
    ) + 
    geom_point(
        shape = 21,
        fill = "black",
        size = 3
    ) + 
    myTheme + 
    theme(plot.title = element_text(hjust = .5)) + 
    labs(y = "Theoretical population standard deviations",
         x = "Empirical sample standard deviations",
         title=sprintf("Correlation: r=%.3f", r$estimate))

# final plot
pE5

























#-------- Exercise 6 ---------#
#-----------------------------#
nExperiments = 250
meanoffsets = seq(0, .3, length = 51)
samplesizes = seq(10, 811, by = 50)

# initialize
propSig = matrix(
    data = 0, 
    nrow = length(samplesizes), 
    ncol = length(meanoffsets)
)

# loop over sample sizes
for (i in seq_along(samplesizes)) {
    # i = 1
    ssize = samplesizes[[i]]
    for (j in seq_along(meanoffsets)) { 
        # j=1
        effect = meanoffsets[[j]]
        # Generate the data:
        X = matrix(
            data = rnorm(n = ssize*nExperiments, mean = effect, sd = 1.5),
            nrow = ssize, 
            ncol=nExperiments
        )
        # run the t-test and store the results
        T_ = apply(X, 2, t.test, mu=0)
        propSig[i,j] = 100*mean(map_dbl(T_, "p.value") < .05)
    }
    
}

propSig[, 2]


# Create a data frame with the values
data_ = as_tibble(propSig)
colnames(data_) = meanoffsets
data_[["SampleSizes"]] = samplesizes
data_ = pivot_longer(data_, -"SampleSizes", names_to = "MeanOffSets")
data_

# Create the ggalot
pE6_1 = ggplot(data_, aes(x= MeanOffSets, y=SampleSizes, fill=value)) + 
    #    geom_raster() + 
  geom_tile() +
  scale_fill_gradient(
      low = "black", 
      high = "white", 
      limits = c(0, 25)
    ) +
    myTheme + 
    scale_x_discrete(
        breaks = seq(min(meanoffsets), max(meanoffsets), by=.05)
    ) + 
    labs(x = "Mean offset", y = "Sample size",
         fill = "Percent tests with p<.05")
# plot
pE6_1

# save
ggsave("ttest_ex6.png", pE6_1, width=8, height=4)
























#-------- Exercise 7 ---------#
#-----------------------------#
Xn = c(60, 52, 90, 20, 33, 95, 18, 47, 78, 65)
Xq = c(65, 60, 84, 23, 37, 95, 17, 53, 88, 66)
sampsize = length(Xn)

# simple subtraction (Y1 in the text)
Ysub = Xq-Xn

# zscore subtraction (Y2 in the text)
Ysbz = scale(Xq) - scale(Xn)
Ysbz = as.vector(Ysbz)

# percent change (Y3 in the text)
Ypct = 100*(Xq-Xn) / Xn

# normalized ratio (Y4 in the text)
Ynrt = (Xq-Xn) / (Xq+Xn)

# plots
pE6_2_1 = ggplot(
    data = tibble(x = Ysub, y = Ysbz),
    aes(x = x, y = y)
    ) + 
    geom_point(
        size=5,
        shape=21,
        fill="gray"
    ) + 
    myTheme + 
    labs(x="Subtraction", y="Z-scored")

pE6_2_2 = ggplot(data = tibble(x = Ysub, y = Ypct),
               aes(x=x,y=y)) + 
    geom_point(
        size=5,
        shape=21,
        fill="gray"
    ) + 
    myTheme + 
    labs(x="Subtraction", y="Percent change")


pE6_2_3 = ggplot(data = tibble(x = Ysub, y = Ynrt),
               aes(x=x,y=y)) + 
    geom_point(
        size=5,
        shape=21,
        fill="gray"
    ) + 
    myTheme + 
    labs(x="Subtraction", y="Norm. ration")

pE6_2_4 = ggplot(data = tibble(x = Ysbz, y = Ypct),
               aes(x=x,y=y)) + 
    geom_point(
        size=5,
        shape=21,
        fill="gray"
    ) + 
    myTheme + 
    labs(x="Z-scored", y="Percent change")

pE6_2_5 = ggplot(data = tibble(x = Ysbz, y = Ynrt),
               aes(x=x,y=y)) + 
    geom_point(
        size=5,
        shape=21,
        fill="gray"
    ) + 
    myTheme + 
    labs(x="Z-scored", y="Norm. ratio")

pE6_2_6 = ggplot(data = tibble(x = Ypct, y = Ynrt),
               aes(x=x,y=y)) + 
    geom_point(
        size=5
        ,
        shape=21,
        fill="gray"
    ) + 
    myTheme + 
    labs(x="Percent change", y="Norm. ratio")

# final plot:
pE6_2 = pE6_2_1 + pE6_2_2 + pE6_2_3 + pE6_2_4 + pE6_2_5 + pE6_2_6 + plot_layout(ncol=3)
pE6_2

# saving:
ggsave("ttest_ex7.png", pE6_2, width=10, height=6)


### now for the t-tests
tSub <- t.test(Ysub, mu = 0)
tPct <- t.test(Ypct, mu = 0)
tsbz <- t.test(Ysbz, mu = 0)
tnrt <- t.test(Ynrt, mu = 0)

# print the results
cat(sprintf("Subtraction (Y1): t(%d)=%.3f, p<%.3f\n", tSub$parameter, tSub$statistic, tSub$p.value))
cat(sprintf("Percent chg (Y2): t(%d)=%.3f, p<%.3f\n", tPct$parameter, tPct$statistic, tPct$p.value))
cat(sprintf("Z subtract  (Y3): t(%d)=%.3f, p<%.3f\n", tsbz$parameter, tsbz$statistic, tsbz$p.value))
cat(sprintf("Norm. ratio (Y4): t(%d)=%.3f, p<%.3f\n", tnrt$parameter, tnrt$statistic, tnrt$p.value))


















#-------- Exercise 8 ---------#
#-----------------------------#
# parameters
mu1 = 1.2 # population mean in dataset 1
mu2 = 1 # population mean in dataset 2

# sample sizes
ns = seq(10, 201, by=10)

pE8_A = ggplot()
pE8_B = ggplot()
# start the experiment
for (i in seq_along(ns)){
    # i=1
    N = ns[[i]]
    data1 = matrix(
        rnorm(N*100, mean = mu1, sd = .5),
        nrow=N, ncol=100
    )
    
    data2 = matrix(
        rnorm(N*100, mean = mu2, sd = .5),
        nrow=N, ncol=100
    )
    
    # run the ttest:
    ttest = map2(as_tibble(data1), as_tibble(data2), t.test, var.equal=T, paired=F)
    t = map_dbl(ttest, "statistic") %>% unname
    p = map_dbl(ttest, "p.value") %>% unname
    
    # plot the t-value, colored by significance
    pE8_A = pE8_A + 
        geom_point(
            data = tibble(x = rep(N, length=sum(p > .05)),
                          y = t[p > .05]),
            aes(x = x, y = y),
            shape = 22,
            size=5,
            alpha=.3,
            fill="gray"
        ) + 
        geom_point(
            data = tibble(x = rep(N, length=sum(p < .05)),
                          y = t[p < .05]),
            aes(x = x, y = y),
            shape = 21,
            size=5,
            alpha=.3,
            fill="red"
        ) + 
        myTheme 
    
    # # plot the p-values, colored by significance
    pE8_B = pE8_B + 
        geom_point(
            data = tibble(x = rep(N, length=sum(p > .05)),
                          y = p[p > .05]),
            aes(x = x, y = y),
            shape = 22,
            size=5,
            alpha=.3,
            fill="gray"
        ) + 
        geom_point(
            data = tibble(x = rep(N, length=sum(p < .05)),
                          y = p[p < .05]),
            aes(x = x, y = y),
            shape = 21,
            size=5,
            alpha=.3,
            fill="red"
        ) + 
        myTheme 
    
}

# final adjustments:
pE8_A = pE8_A + 
    scale_x_continuous(breaks = ns) + 
    labs(title=bquote(bold("A)")~"T-values"),
         x = "Sample size (equal groups)", y = "T-test value")

pE8_B = pE8_B + 
    scale_x_continuous(breaks = ns) + 
    scale_y_log10() + 
    geom_hline(
        yintercept = 0.05,
        linetype="dashed",
        color="red",
        linewidth=1
        ) + 
    labs(title=bquote(bold("B)")~"P-values"),
         x = "Sample size (equal groups)", y = "P value")

# final plot:
pE8 = pE8_A + pE8_B + plot_layout(ncol=1)
pE8

# saving:
ggsave("ttest_ex8.png", pE8, width = 10, height = 6)



### compute critical t-values for the degrees of freedom
tCrit = map_dbl(2*ns-2, ~qt(.05/2, ., lower.tail = F))

# and visualize
ggplot(data = tibble(x = ns, y = tCrit),
       aes(x = x, y = y))+
    geom_point(
        shape=21,
        size=5,
        fill="gray"
    ) + 
    ylim(c(1.8, 2.2)) + 
    myTheme + 
    labs(x = 'Degrees of freedom',
         y = 'Critical t-values (2-tailed)')





















#-------- Exercise 9 ---------#
#-----------------------------#
# range of standard deviations
stdevs = seq(.01, 15, length=41)

# initialize results matrix
results = matrix(0, nrow = 3, ncol=length(stdevs))
tCrit = rep(0, length(stdevs))

# start the experiment!
for (i in seq_along(stdevs) ) {
    # i=1
    std = stdevs[[i]]
    
    # create two groups of data
    X1 = rnorm(mean=1, sd=1, n=50)
    X2 = rnorm(mean=1.1, sd=std, n=40)
    
    # levene's test
    levene_test = car::leveneTest(
        x ~ group,
        bind_rows(tibble(x= X1, group = "sample 1"),
                  tibble(x= X2, group = "sample 2"))
    )
    results[1, i] = log(levene_test$`Pr(>F)`[[1]])
    
    # difference of t-values
    same_var = t.test(X1, X2, var.equal = T) # equal variance
    diff_var = t.test(X1, X2, var.equal = F) # unequal variance
    results[2, i] = same_var$statistic # equal variance
    results[3, i] = diff_var$statistic # unequal variance
    
    # compute df for tCrit
    s1 = var(X1)
    s2 = var(X2)
    n1 = length(X1)
    n2 = length(X2)
    df_num = (s1/n1 + s2/n2)^2
    df_den = s1^2/(n1^2*(n1-1)) + s2^2/(n2^2*(n2-1))
    tCrit[[i]] = qt(.05/2, df =df_num/df_den, lower.tail = F)
    
}



#### plot

# levene's test results
pE9_A = ggplot(data = tibble(x = stdevs,
                     y = results[1,]),
       aes(x=x, y=y)) + 
    geom_point(
        shape=22,
        fill="#525252",
        size=5
    ) + 
    geom_hline(
        yintercept = log(.05),
        linetype="dashed",
        color="gray",
        linewidth=1
    ) + 
    annotate(
        "text",
        x = max(stdevs),
        y = log(.1),
        label = "p = .05",
        color="gray",
        size=4
    ) + 
    myTheme + 
    labs(title=bquote(bold("A)")~"Levene's P-values"),
         x = bquote("Standard deviation of"~X[2]),
         y="log(p)")

# t-tests
pE9_B = ggplot( ) + 
    geom_point(
        data = tibble(x = stdevs,
                      y = results[2, ]),
        aes(x=x, y = y, 
            shape = "Equal var.", 
            fill = "Equal var."),
        size=5
    ) + 
    geom_point(
        data = tibble(x = stdevs,
                      y = results[3, ]),
        aes(x=x, y = y, 
            shape = "Unequal var.", 
            fill = "Unequal var."),
        size=5
    ) + 
    geom_line(
        data = tibble(x = stdevs,
                      y = tCrit),
        aes(x = x, y = y, 
            color = "p == .05"),
        linetype="dashed",
        linewidth=1
    ) + 
    geom_line(
        data = tibble(x = stdevs,
                      y = -tCrit),
        aes(x = x, y = y),
        linetype="dashed",
        color="gray",
        linewidth=1
    ) + 
    scale_fill_manual(
        values = c("Equal var." = "#525252",
                   "Unequal var." = "#bdbdbd")
    ) + 
    scale_shape_manual(
        values = c("Equal var." = 22,
                   "Unequal var." = 21)
    ) + 
    scale_color_manual(
        values=c("p == .05" = "gray")
    ) + 
    myTheme + 
    theme(
        legend.position = "right",
        legend.spacing.x = unit(.3, 'cm'),
        legend.background = element_rect(fill=NA),
        legend.spacing = unit(-15, "pt")
    ) + 
    labs(fill="", shape="",
         title=bquote(bold("B)")~"T-test values"),
         x = bquote("Standard deviation of"~X[2]),
         y="T-value")

# final plot
pE9 = pE9_A + pE9_B
pE9

# saving:
ggsave("ttest_ex9.png", pE9, width=12, height=5)

# Not in the instructions, but I think it's also interesting to
#  plot the ratio of t-values as a function of standard deviation
# values >1 indicate a larger t-value for equal compared to unequal variance formula
ggplot(data = tibble(x=stdevs,
                     y = results[2,]/results[3,]),
       aes(x = x, y = y))+
    geom_point(
        shape=21,
        fill="black",
        size=5
    ) + 
    geom_hline(
        yintercept = 1,
        linetype="dashed",
        color="gray",
        linewidth=1
    ) + 
    ylim(c(.5, 1.5)) + 
    myTheme + 
    labs(y="Equal to Unequal variance t ratio",
         x="")



















#-------- Exercise 10 ---------#
#------------------------------#
# generate the data
sigmas = seq(.1, 1.2, length=20)

# null hypothesis value
h0 = .5

# initialize the results matrices
tvals = matrix(0, nrow = 3, ncol = length(sigmas))
cents = matrix(0, nrow=2, ncol = length(sigmas))

pE10_A = ggplot()

# compute and store all moments in a matrix
for (i in seq_along(sigmas) ) {
    # i = 1
    s = sigmas[[i]]
    # generate mean-centered data
    X = exp(rnorm(100)*s)
    X = X - mean(X)
    
    # compute and store the descriptives
    cents[1, i] = mean(X) - h0
    cents[2, i] = median(X) - h0
    
    # draw the histogram
    if (i %% 3 == 0) {
        mc = length(sigmas)+2
        n_breaks_ = nclass.FD(X)
        # pick a random color
        color_ = rgb(i/mc, i/mc, i/mc)
        X_hist = hist(X, breaks = n_breaks_, plot = F)
        pE10_A = pE10_A +  
            geom_line(
                data = tibble(x = X_hist$mids,
                              y = X_hist$counts),
                aes(x = x, y = y),
                linewidth=1,
                color = color_
            ) + 
            geom_vline(
                xintercept = median(X),
                linetype="dashed",
                linewidth=.8,
                color = color_
            )
    }
    # parametric t-test
    tvals[1,i] = t.test(X,mu = h0)$statistic
    
    # Wilcoxon test
    wtest = wilcox.test(
      X - h0, 
      exact = F, # no exact calculation for the p-value
      correct = F, # not applying the continuity correction
      alternative = "two.sided"
    )
    tvals[2,i] = wtest$statistic
    tvals[3,i] = wtest$p.value # storing the p-value here to show the relationship to V
    ### NOTE: I divided the Wilcoxon V value by 100 to facilitate 
    #         visual comparison with the t-value.
}
    

### A brief aside: V is a non-negative variable, and can have a small p-value
#   for values close to, or far from, 0. The plots generated below will seem 
#   opposite of those generated by Python (and shown in the book), but this is 
#   due to the relationship between V and z. In fact, smaller values of V indicate
#   that most of the data are left of the H0 value. That's apparent when looking
#   at a scatter plot between V and p, which you can see from running the line below:
#plot(log(tvals[2,]),tvals[3,])




pE10_A = pE10_A + 
    xlim(c(-1.5, 4)) + 
    myTheme + 
    labs(x="Data value", y="Count",
         title=bquote(bold("A)")~"Distributions"))


# panel B:
pE10_B = ggplot() + 
    geom_point(
        data = tibble(
            x = sigmas,
            y = cents[1,]
        ),
        aes(x = x, y = y, 
            fill = "delta to mean",
            shape = "delta to mean"),
        size = 5
    ) + 
    geom_point(
        data = tibble(
            x = sigmas,
            y = cents[2,]
        ),
        aes(x = x, y = y, 
            fill = "delta to median",
            shape = "delta to median"),
        size=5
    ) + 
    scale_fill_manual(
        labels = c(bquote(Delta~"to mean"),
                   bquote(Delta~"to median")),
        values = c("delta to mean" = rgb(.3, .3, .3),
                   "delta to median" = rgb(.8, .8, .8))
    ) + 
    scale_shape_manual(
        labels = c(bquote(Delta~"to mean"),
                   bquote(Delta~"to median")),
        values = c("delta to mean" = 22,
                   "delta to median" = 21)
    ) + 
    myTheme + 
    theme(legend.position = c(.3, .5)) + 
    labs(x = bquote(sigma~"parameter for"~exp(X~sigma)),
         y = "Mean of median dist.",
         title = bquote(bold("B)")~"Distance to"~H[0]),
         fill = "",
         shape = "")

# panel C:
pE10_C = ggplot() + 
    geom_point(
        data = tibble(
            x = sigmas,
            y = tvals[1,]
        ),
        aes(x = x, y = y, 
            fill = "Parametric t",
            shape = "Parametric t"),
        size = 5
    ) + 
    geom_point(
        data = tibble(
            x = sigmas,
            y = tvals[2,]
        ),
        aes(x = x, y = y, 
            fill = "Wilcoxon V",
            shape = "Wilcoxon V"),
        size=5
    ) + 
    scale_fill_manual(
        values = c("Parametric t" = rgb(.3, .3, .3),
                   "Wilcoxon V" = rgb(.8, .8, .8))
    ) + 
    scale_shape_manual(
        values = c("Parametric t" = 22,
                   "Wilcoxon V" = 21)
    ) + 
    myTheme + 
    theme(legend.position = c(.3, .5)) + 
    labs(x = bquote(sigma~"parameter for"~exp(X~sigma)),
         y = "statistic",
         title = bquote(bold("C)")~"Test stat. values"),
         fill = "",
         shape = "")

## final plot
pE10 = pE10_A + pE10_B + pE10_C
pE10

# saving:
ggsave('ttest_ex10a.png', pE10, width=14, height=5)




## plot showing relationship between central tendency distances and test statistic values
pE102_A = ggplot() + 
    geom_point(
        data = tibble(
            x = cents[1,],
            y = tvals[1,]
        ),
        aes(x = x, y = y),
        fill = rgb(.6, .6, .6),
        size=5
    ) + 
    xlim(c(-.6, -.4)) + 
    myTheme + 
    labs(x = "Disance: mean to .5",
         y = "T-value",
         title = bquote(bold("A)")~"One-sample t-test"))

pE102_B = ggplot() + 
    geom_point(
        data = tibble(
            x = cents[2,],
            y = tvals[2,]
        ),
        aes(x = x, y = y),
        fill = rgb(.6, .6, .6),
        size=5
    ) + 
    myTheme + 
    labs(x = "Disance: mean to .5",
         y = "V-value",
         title = bquote(bold("B)")~"Wilcoxon test"))

# final plot
pE102 = pE102_A + pE102_B
pE102

# saving
ggsave('ttest_ex10b.png', pE102, width=8, height = 4)

























#-------- Exercise 11 ---------#
#------------------------------#
# params
meanoffsets = seq(0, 2, length = 71)
samplesizes = seq(10, 811, by = 50)

# initialize
pvals = matrix(0, nrow = length(samplesizes), ncol=length(meanoffsets))
cohend = matrix(0, nrow=length(samplesizes), ncol=length(meanoffsets))
r2 = matrix(0, nrow=length(samplesizes), ncol=length(meanoffsets))

# loop over sample sizes
for (sidx in seq_along(samplesizes)) {
    # sidx = 1
    ssize = samplesizes[[sidx]]
    
    # loop over effect sizes
    for (eidx in seq_along(meanoffsets)){
        # eidx = 1
        effect = meanoffsets[[eidx]]
        
        # generate the data
        X = rnorm(mean = effect, sd = 1.5, n = ssize)
        # run the t-test and store the results
        ttest = t.test(X, mu = 0)
        pvals[sidx, eidx] = ttest$p.value
        
        # Cohen's d
        cohend[sidx, eidx] = abs( mean(X) / sd(X) )
        
        # R^2
        r2[sidx, eidx] = ttest$statistic^2 / (ttest$statistic^2 + ttest$parameter)
    
    }
}

# plotting

# panel A:
pE11_A = 
    ggplot(
        data = tibble(data_index = log(as.vector(pvals) + 1e-16),
                      data_value = as.vector(cohend)),
        aes(x = data_index, y = data_value)
    ) + 
    geom_point(
        size = 3,
        fill = rgb(.8, .8, .8),
        shape = 21
    ) + 
    myTheme + 
    labs(title = bquote(bold("A)")~"Cohen'd by p-values"),
         x = "log(p)", y = "Cohen's d")

# panel V:
pE11_B = 
    ggplot(
        data = tibble(data_index = as.vector(cohend),
                      data_value = as.vector(r2)),
        aes(x = data_index, y = data_value)
    ) + 
    geom_point(
        size = 3,
        fill = rgb(.8, .8, .8),
        shape = 21
    ) + 
    myTheme + 
    labs(title = bquote(bold("B)")~"Different effect size measures"),
         x = "Cohen's d", y = bquote(R^2))

# final plot
pE11 = pE11_A + pE11_B
pE11

# saving
ggsave("ttest_ex11.png", pE11, width=10, height=4)





















#-------- Exercise 12 ---------#
#------------------------------#
## data reference:
# P. Cortez, A. Cerdeira, F. Almeida, T. Matos and J. Reis.
# Modeling wine preferences by data mining from physicochemical properties. In Decision Support Systems, Elsevier, 47(4):547-553, 2009.
# https://archive.ics.uci.edu/ml/datasets/Wine+Quality

url = "https://archive.ics.uci.edu/ml/machine-learning-databases/wine-quality/winequality-red.csv"
data = readr::read_delim(url, delim = ";")
data

# describe the data
summary(data)

# list number of unique values per column
data %>% 
    summarise_all(n_distinct) %>% 
    tidyr::pivot_longer(everything())

# plot some data
pE12_1 = ggplot(stack(data), aes(x = ind, y = values, fill=ind)) +
    geom_boxplot() + 
    myTheme + 
    theme(
        legend.position = "none",
        axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1)
    ) + 
    labs(x = "", y = "")

# plot
pE12_1




### z-score all variables except for quality

# find the columns we want to normalize (all except quality)
cols2zscore = setdiff(names(data), "quality")

# z-score (written out for clarity)
dataz = data %>% 
    mutate(across(all_of(cols2zscore), ~scale(.x)[, 1], .names = "{.col}"))

# summary:
summary(dataz)

# check the plot again
pE12_2 = ggplot(stack(dataz), aes(x = ind, y = values, fill=ind)) +
    geom_boxplot() + 
    myTheme + 
    theme(
        legend.position = "none",
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)
    ) + 
    labs(x = "", y = "")
pE12_2




### test each variable for normality
# loop through all variables
for (col in cols2zscore) {
    # compute and print the test
    Stest = shapiro.test(dataz[[col]])
    print(sprintf('%25s: p<%.4f', col, Stest$p.value))
}

# visualize the histograms
# loop through columns and create histograms
pE12_3 = vector("list", length(names(dataz)))
for (i in seq_along(pE12_3)) {
    col = names(dataz)[[i]]
    pE12_3[[i]] = ggplot() + 
        geom_histogram(
                data = dataz,
                aes(x = .data[[col]]),
                color="black",
                bins = 30,
                fill = rgb(.7, .7, .7)
            ) + 
            myTheme + 
            theme(
                axis.text=element_blank(), 
                axis.ticks=element_blank()
            ) + 
            labs(x = col, y = "")
}

pE12_3 = reduce(pE12_3, `+`) + plot_layout(ncol = 4)
pE12_3

# saving:
ggsave('ttest_ex12.png', pE12_3, width=10, height = 5)

# binarize quality ratings into a new variable
pE12_4 = ggplot(
    data = dataz %>% count(quality) %>% mutate(quality = as.factor(quality)),
    aes(x = quality, y = n)
    ) + 
    geom_col(
        fill = rgb(.7, .7, .7),
        color="black"
    ) + 
    myTheme + 
    labs(x = "Quality rating", y = "Count")
# plot
pE12_4

# create a new column for binarized (boolean) qualitya
dataz = dataz %>% 
    mutate(boolQuality = ifelse(quality > 5, T, F))

# display data:
dataz %>% 
    select(quality, boolQuality)


















#-------- Exercise 13 ---------#
#------------------------------#

# run all t-tests and store the results

# Note about this and the next code cell: You need to compute all p-values in order
# to conduct the FDR correction. That's why I run the t-tests here and then report
# the results in the following cell.

# initialize results matrix as a dictionary
results = vector("list", length(cols2zscore))
    
# loop over column
for (i in seq_along(cols2zscore)){
    col = cols2zscore[[i]]
    # for convenience, extract the numerical variables
    Xh = dataz %>% filter(boolQuality == T) %>% pull(col) # high rating
    Xl = dataz %>% filter(boolQuality == F) %>% pull(col) # low rating
    
    # compute df
    s1 = var(Xh)
    s2 = var(Xl)
    n1 = length(Xh)
    n2 = length(Xl)
    df_num = (s1/n1 + s2/n2)^2
    df_den = s1^2/(n1^2*(n1-1)) + s2^2/(n2^2*(n2-1))
    
    # run the t-test and store the results in a dictionary
    tres = t.test(Xh,Xl, var.equal = F)
    #tres = stats.mannwhitneyu(Xh,Xl) # uncomment for Mann-Whitney U testd
    
    results[[i]] = list('t' = tres$statistic,
                        'p' = tres$p.value,
                        'df'= df_num/df_den )
}
names(results) = cols2zscore

# need FDR correction function

# bonferroni threshold
bonP = .05/length(cols2zscore)

# FDR correction (don't need p-values, only keep outcome)
fdrH = p.adjust(map_dbl(results, "p"), method = "fdr")
fdrH = fdrH < 0.05

# loop through columns and report the results!
for (i in seq_along(cols2zscore)){
    col = cols2zscore[[i]]
    # extract values
    t  = results[[col]]$t
    p  = results[[col]]$p
    df = results[[col]]$df
    
    # determine if significant
    issigB = ifelse(p < bonP, "*", " ")
    issigF = ifelse(fdrH[[i]], "+", " ")
    
    print(sprintf('%20s: t(%.0f)=%6.2f, p=%.4f, %s%s', col, df, t, p, issigB, issigF))
}




#----
# Modern statistics: Intuition, Math, Python, R
## Mike X Cohen (sincxpress.com)
#### https://www.amazon.com/dp/B0CQRGWGLY
#### Code for chapter 3 (Data visualization)

# About this code file:
### This code file will reproduce most of the figures in this chapter 
### (some figures were made in Inkscape), and illustrate the statistical 
### concepts explained in the text. The point of providing the code is not 
### just for you to recreate the figures, but for you to modify, adapt, 
### explore, and experiment with the code.
###
### Solutions to all exercises are at the bottom of the file.
# Thanks to Ali Santacruz for help with the translation from Python.


#----
# installation of the following R packages may be needed
# install.packages('tidyverse')
# install.packages('Ryacas')
# install.packages('gridExtra')
# install.packages('ggpattern')
# install.packages('scales')
# install.packages('devtools') # to install ggradar from github
# devtools::install_github("ricardo-bion/ggradar")

# import packages and define global settings
library(tidyverse)
library(Ryacas)
library(gridExtra)
library(ggpattern)
library(ggplot2)
library(scales)
library(ggradar)

# define global figure properties
custom_theme <- theme_classic() + 
  theme(text=element_text(size=20),        # font size 
        plot.title=element_text(hjust=0))  # title location
theme_set(custom_theme)
figdpi <- 300                             # output resolution







#-------- Figure 3.2: The math of the heart :) ---------#
#-------------------------------------------------------#
# Define the equation and calculate z values
x <- ysym("x")
y <- ysym("y")
f <- (x^2 + y^2 - 1)^3 - x^2 * y^3
N <- 100
val=2
x <- seq(-val, val, length=N)
y <- seq(-val, val, length=N)
f_r <- as_r(f)
z <- outer(x, y, function(x, y) eval(f_r, list(x=x, y=y)))

# create data.frame for plotting
df <- expand.grid(x=x, y=y)
df <- mutate(df, z=c(z))

# plot heart curve
ggplot(df, aes(x, y, z=z)) + 
  geom_contour(bins=2, colour="black", linewidth=2) +
  coord_equal() + 
  labs(x='', y='') +
  scale_y_continuous(breaks=-1:1)

ggsave('vis_heart.png', width=6.4, height=4.8, dpi=figdpi)









#-------- Figure 3.3: Bar plot of news sources ---------#
#-------------------------------------------------------#

# Specify the bar heights and labels
news_sources <- c(40, 10, 85, 20)
source_labels <- c("TV", "Newspapers", "Internet", "Word of mouth")

# Create data.frame from the input data
data <- data.frame(Source=factor(source_labels, levels=source_labels), 
                   PercentYes=news_sources)

# Create the bar plot and make it look a bit nicer
ggplot(data, aes(x=Source, y=PercentYes)) + 
  geom_bar(stat="identity", fill=rgb(0.2, 0.2, 0.2)) + 
  labs(title="Where do people get their news?", 
       y='% responding "yes"', x="Media type") + 
  theme(axis.text.x=element_text(angle=-30, hjust=.3, vjust=-.1),
        plot.title=element_text(hjust=.5)) +
  scale_y_continuous(expand=c(0, 0))

ggsave("vis_barplot_news1.png", width=8, height=5, dpi=figdpi)









#-------- Figure 3.4: Margin figure of bar plot ---------#
#--------------------------------------------------------#

# Data
Y <- c(1, 4, 3, 9)  # bar heights
X <- c(0, 1, 3, 4)  # bar locations

ggplot() +
  geom_bar(aes(x=X,y=Y), stat="identity")

# Create the bar plot
ggplot() +
  geom_bar(aes(x=X, y=Y), stat="identity", fill="black") +
  labs(x="X", y="Y") +
  scale_y_continuous(expand=c(0, 0))

# Save the plot to a file file
ggsave("vis_barplot_basic.png", width=5, height=2.5, dpi=figdpi)









#-------- Figure 3.5: Grouped bar plots ---------#
#------------------------------------------------#
# Create data.frame
agegroups <- c('Millenials', 'Boomers')
news_sources_df <- data.frame(
  Media_type=factor(source_labels, levels=source_labels),
  news_sources=c(12, 17, 95, 35, 90, 40, 50, 25),
  agegroups=factor(rep(agegroups, each=4), levels=agegroups)
)


# Create a bar plot for each age group
p1 <- ggplot(news_sources_df, 
             aes(x=Media_type, y=news_sources, 
                 fill=agegroups)) +
  geom_bar(stat="identity", position='dodge') +
  labs(title=bquote(bold('A)') ~ " Grouped by news source"), x="Media type", 
       y='Percentage responding "yes"', fill=NULL) +
  scale_fill_manual(values=c("black", "gray50")) +
  theme(legend.position=c(0.8, 0.8),
        legend.background=element_rect(fill=alpha("white", 0.5), 
                                         color="grey80")
        )

p2 <- ggplot(news_sources_df, 
             aes(x=agegroups, y=news_sources, pattern=Media_type, 
                 pattern_angle=Media_type)) +
  geom_bar_pattern(stat="identity", 
                   position='dodge', 
                   fill='gray80', 
                   color='black', 
                   pattern_fill="black",
                   pattern_density=0.1,
                   pattern_spacing=0.025,
                   pattern_key_scale_factor=0.6) +
  labs(title=bquote(bold('B)') ~ " Grouped by generation"), x="Generation",
       y='Percentage responding "yes"', pattern=NULL) +
  scale_pattern_manual(values=c("crosshatch", "circle", "stripe", "stripe")) +
  scale_pattern_angle_manual(values=c(45, 0, 0, 90)) +
  guides(pattern_angle='none') + 
  theme(legend.position=c(0.8, 0.8))

# Arrange the plots side-by-side
p <- grid.arrange(p1, p2, ncol=2)

# Save the plot
ggsave("vis_barplot_news2.png", plot=p, width=12, height=7, dpi=figdpi)








#-------- Figure 3.6: Note about bars from matrices ---------#
#------------------------------------------------------------#
# Data are observations (rows) X features (columns)
m <- matrix(c(10, 12, 90, 35, 85, 15, 50, 10), nrow=2, byrow=TRUE)
df <- as.data.frame(m)
Features <- c('A','B','C','D')
colnames(df) <- Features

# Add Observations column
df <- mutate(df, Observations=factor(c(0, 1), levels=c(1, 0)))

# Reshape df to long format for plotting with ggplot
df <- pivot_longer(df, cols=-Observations, names_to="Features", 
                   values_to="value") |>
  mutate(Features=factor(Features))

# Create first plot
plot1 <- ggplot(df, aes(x=Features, y=Observations, fill=value)) + 
  geom_tile() +
  geom_text(aes(label=value), size=15) +
  scale_fill_gradient(low="gray30", high="white") +
  labs(x="Features", y="Observations") +
  theme(legend.position="none") +
  coord_fixed(expand=FALSE)

# now other orientation (features X observations)
df <- mutate(df, Observations=factor(Observations, levels=c(0, 1)),
             Features=fct_rev(Features))
plot2 <- ggplot(df, aes(x=Observations, y=Features, fill=value)) + 
  geom_tile() +
  geom_text(aes(label=value), size=15) +
  scale_fill_gradient(low="gray30", high="white") +
  labs(x="Observations", y="Features") +
  theme(legend.position="none") +
  coord_fixed(expand=FALSE)


#### now for bar plots

df <- mutate(df, Features=fct_rev(Features))
plot3 <- ggplot(df, aes(x=Observations, y=value, fill=Features)) +
  geom_bar(stat="identity", position=position_dodge(width=0.9)) +
  labs(x="Index", y="Observations", fill=NULL) +
  geom_text(aes(label=value, group=Observations), 
            position=position_dodge2(width=0.9), 
            size=6, vjust=-0.2) +
  scale_y_continuous(expand=c(0, 0), limits=c(0, 100), n.breaks=6)  + 
  scale_fill_manual(values=c(A='#1f76b4', B='#ff7e0e', 
                               C='#2ca02c', D='#d62727')) +
  theme(legend.position=c(0.8, 0.8),
        legend.background=element_rect(color="grey80"))


plot4 <- ggplot(df, aes(x=Features, y=value, fill=Observations)) +
  geom_bar(stat="identity", position=position_dodge(width=.9)) +
  labs(x="Features", y="Observations", fill=NULL) +
  geom_text(aes(label=value, group=Observations), 
            position=position_dodge2(width=.9), 
            size=6, vjust=-.2) +
  scale_y_continuous(expand=c(0, 0), limits=c(0, 100), n.breaks=6) +
  scale_fill_manual(values=c('#1f76b4', '#ff7e0e')) +
  theme(legend.position=c(0.8, 0.8),
        legend.background=element_rect(color="grey80"))

# Arrange the plots side-by-side
p <- grid.arrange(plot1, plot2, plot3, plot4, ncol=2)

# Save the plot
ggsave("vis_barplotOrientations.png", plot=p, width=8, height=8, dpi=figdpi)
# Note about this figure (and many others): The font sizes are customized for export,
# and they might not look good on your screen.










#-------- Figure 3.8: Pie chart ---------#
#----------------------------------------#
# Data
mostNews <- c(15, 5, 70, 10)

# Create data.frame from input data
data <- data.frame(Source=factor(source_labels, levels=source_labels), 
                   Percentage=mostNews)

# create data.frame for placing labels outside of the pie chart
data2 <- data %>% 
  mutate(csum=rev(cumsum(rev(Percentage))), 
         pos=Percentage/2 + lead(csum, 1),
         pos=if_else(is.na(pos), Percentage/2, pos))

# Make the pie chart
ggplot(data, aes(x="", y=Percentage, fill=Source)) +
  geom_col(color='black') +
  coord_polar(theta="y", start=pi/2) +
  labs(x=NULL, y=NULL, fill=NULL) +
  scale_fill_manual(values=c("TV"='#1f76b4', "Newspapers"='#ff7e0e', 
                               "Internet"='#2ca02c', "Word of mouth"='#d62727')) +
  geom_text(aes(label=paste0(Percentage, "%")), 
            position=position_stack(vjust=0.5), size=6) +
  scale_y_continuous(breaks=data2$pos, 
                     labels=c("TV", "Newspapers", "Internet", "Word of\nmouth")) +
  theme(legend.position="none",
        axis.line=element_blank(),
        axis.ticks=element_blank())

# Save the plot to a file
ggsave("vis_pie.png", width=6.3, height=6.3, dpi=figdpi)











#-------- Figure 3.9: Box plot ---------#
#---------------------------------------#
# Random data with an outlier
data <- rnorm(100)
data[data > 2] <- 1
data[data < -2] <- -1
data[1] <- 3  # Force one outlier

# Draw the boxplot and make some color adjustments
p <- ggplot(data.frame(Data=data), aes(y=Data, x=0)) +
  geom_boxplot(fill="black", color="black", width=0.4, 
               outlier.shape=1, outlier.size=2.5) +
  labs(x=NULL, y="Data values") +
  theme(
    axis.title.x=element_blank(),
    axis.text.x=element_blank(),
    axis.ticks.x=element_blank()
  ) +
  scale_y_continuous(breaks=seq(-3, 3, by=1)) +
  xlim(-0.5, 1.5) +
  stat_boxplot(geom ='errorbar', width=0.2)

# Change median line color to white
dat <- ggplot_build(p)$data[[1]]
p + geom_segment(data=dat, aes(x=xmin, xend=xmax, y=middle, 
                                 yend=middle), 
                 colour="white", linewidth=0.8)

# Save the plot to a file
ggsave("vis_boxplotBasic.png", width=2, height=4, dpi=figdpi)











#-------- Figure 3.10: Box plots for distribution characteristics ---------#
#--------------------------------------------------------------------------#
# Box plot data
data <- data.frame(
  A=rnorm(200, mean=100, sd=10),
  B=rnorm(200, mean=100, sd=2)
)

# Convert data to long format for plotting with ggplot
data_long <- pivot_longer(data, cols=everything(),
                          names_to="Group", values_to="Value")

# Draw the boxplot and make some color adjustments
p <- ggplot(data_long, aes(x=Group, y=Value)) +
  geom_boxplot(fill="black", color="black", # width=0.4, 
               outlier.shape=1, outlier.size=2.5) +
  stat_boxplot(geom ='errorbar', width=0.2) +
  labs(x=NULL, y=NULL) +
  scale_y_continuous(breaks=c(80, 100, 120))

# Change median line color to white
dat <- ggplot_build(p)$data[[1]]
p + geom_segment(data=dat, aes(x=xmin, xend=xmax, y=middle, 
                                 yend=middle), 
                 colour="white", linewidth=0.8)

# Save the plot to a file
ggsave("vis_boxplotComp.png", width=3, height=3, dpi=figdpi)










#-------- Figure 3.11: Histogram ---------#
#-----------------------------------------#
# Version 1
X <- c(1, 2, 2, 3, 3, 4, 5, 5, 5, 5, 6, 7, 7, 7, 8, 8, 9)
df <- data.frame(X=X)

hist1 <- ggplot(df, aes(x=X)) +
  geom_histogram(binwidth=9/(length(unique(df$X)) + 1), color='black', 
                 fill='gray50', center=0.5, closed='left') +
  scale_x_continuous(breaks=min(df$X):(max(df$X))) +
  labs(x="Numerical value", y="Count") +
  scale_y_continuous(expand=c(0, 0))
hist1

# Save the plot to a file
ggsave("vis_histOfInts1.png", width=8, height=4, dpi=figdpi)














#-------- Figure 3.12 ---------#
#------------------------------#
# Using different boundaries
hist2 <- ggplot(df, aes(x=X)) +
  geom_histogram(bins=length(unique(df$X)), fill="gray50", color="black") +
  scale_x_continuous(breaks=seq(min(df$X), max(df$X))) +
  labs(x="Numerical value", y="Count") +
  scale_y_continuous(expand=c(0, 0))
hist2

# Save the plot to a file
ggsave("vis_histOfInts2.png", width=8, height=4, dpi=figdpi)


# The table of bin boundaries
x1 <- ggplot_build(hist1)$data[[1]] |> select(xmin, xmax)
x2 <- ggplot_build(hist2)$data[[1]] |> select(xmin, xmax)

for(i in 1:nrow(x1)){
  cat(paste0("Bin", i, ":  [", 
             sprintf("%.1f", x1[i, 1]), " , ", 
             sprintf("%.1f", x1[i, 2]), "]   [", 
             sprintf("%.1f", x2[i, 1]), " , ", 
             sprintf("%.1f", x2[i, 2]), "]"), "\n")
}










#-------- Figure 3.14: Histogram of mongoose lengths ---------#
#-------------------------------------------------------------#
# Generate random data for mongooses' lengths
mongooses <- data.frame(
  Length=atanh(runif(500, min=-.75, max=.75)) * 15 + 40
)

# Create histogram
ggplot(mongooses, aes(x=Length)) +
  geom_histogram(bins=30, fill="gray50", color="black") +
  labs(x="Length (cm)", y="Count", 
       title="Lengths of Babylonian mongooses") +
  scale_x_continuous(breaks=seq(25, 55, by=5)) +
  scale_y_continuous(breaks=seq(0, 30, by=5)) +
  theme(plot.title=element_text(hjust=0.5))

# Save the plot to a file
ggsave("vis_mongeese.png", width=8, height=4, dpi=figdpi)










#-------- Figure 3.15: Histograms with different bins ---------#
#--------------------------------------------------------------#
p1 <- ggplot(mongooses, aes(x=Length)) +
  geom_histogram(bins=3, fill="gray50", color="black") +
  labs(x="Length (cm)", y="Count", 
       title=bquote(bold('A)') ~ " Histogram with 3 bins")) +
  scale_y_continuous(expand=c(0, 0))

p2 <- ggplot(mongooses, aes(x=Length)) +
  geom_histogram(bins=300, fill="gray50", color="black") +
  labs(x="Length (cm)", y="Count", 
       title=bquote(bold('B)') ~  " With 300 bins")) +
  scale_y_continuous(breaks=seq(0, 6, by=2), expand=c(0, 0))

# Arrange the plots side-by-side
p <- grid.arrange(p1, p2, ncol=2)

# Save the plot to a file
ggsave("vis_mongeese_binoptions.png", plot=p, width=10, height=5, dpi=figdpi)












#-------- Figure 3.16: Distribution showing tails ---------#
#----------------------------------------------------------#
# Create a sequence of values for x
x <- seq(-4, 4, length.out=401)

# Create a Gaussian probability curve
gpdf <- dnorm(x)

# Find the indices of the 2.5% and 97.5% quantiles
lbndi <- which.min(abs(x-qnorm(.05)))
ubndi <- which.min(abs(x-qnorm(.95)))

# Create a data frame for the Gaussian probability curve
df <- data.frame(x=x, gpdf=gpdf)

left_shade <- rbind(c(x[ubndi], 0), 
                    df[ubndi:nrow(df),], 
                    c(x[length(x)], 0))

# Repeat for the right lobe
right_shade <- rbind(c(x[1], 0), 
                     df[1:lbndi,], 
                     c(df[lbndi, "x"], 0))

# Plot the probability function and create patches for the rejected area
p <- ggplot(df, aes(x, gpdf)) +
  geom_line(color="black", size=2) +
  geom_polygon(data=left_shade, aes(x, gpdf), fill='black', alpha=.4) +
  geom_polygon(data=right_shade, aes(x, gpdf), fill='black', alpha=.4) +
  labs(x=NULL, y=NULL) +
  coord_cartesian(ylim=c(0,.42), xlim=c(x[1], x[length(x)]), 
                  expand=FALSE) + 
  theme(axis.text=element_blank(),
        axis.line=element_blank(),
        axis.ticks=element_blank())

# Annotations
tailx=which.min(abs(x - -2.2))
p <- p +
  annotate('segment', xend=x[tailx], yend=dnorm(x[tailx])+.01, 
           x=x[tailx]-.4, y=dnorm(x[tailx])+.08, 
           size=1.5, arrow=arrow(length=unit(1, "cm"))) + 
  annotate("text", x=x[tailx]-.4, y=dnorm(x[tailx])+.1, 
           size=6, label="Left tail", hjust="right", fontface =2)

tailx <- which.min(abs(x - 2.2))
p <- p +
  annotate('segment', xend=x[tailx], yend=dnorm(x[tailx])+.01, 
           x=x[tailx], y=dnorm(x[tailx])+.08, 
           size=1.5, arrow=arrow(length=unit(1, "cm"))) +
  annotate("text", x=x[tailx], y=dnorm(x[tailx])+.1, 
           size=6, label="Right tail", hjust="left", fontface=2)
p

# Save the plot to a file
ggsave("vis_distribution_tails.png", width=5, height=4, 
       dpi=figdpi)










#-------- Figure 3.17: Histogram raw counts vs proportion ---------#
#------------------------------------------------------------------#
data <- runif(200)^2

# extract histogram data
hist1 <- ggplot() +
  geom_histogram(aes(x=data), bins=nclass.FD(data), boundary=0)
hist1_data <- ggplot_build(hist1)$data[[1]]

binCents <- hist1_data$x
counts <- hist1_data$count

# convert counts to proportion
proportion <- counts / sum(counts)

p1 <- ggplot() +
  geom_bar(aes(x=binCents, y=counts), stat='identity', col='black') +
  labs(x=NULL, y='Counts', title=bquote(bold('A)') ~ ' Raw counts')) +
  scale_y_continuous(expand=c(0, 0)) +
  scale_x_continuous(breaks=seq(0, 1, by=0.25))

p2 <- ggplot() +
  geom_bar(aes(x=binCents, y=proportion), stat='identity', col='black') +
  labs(x=NULL, y='Proportion', title=bquote(bold('B)') ~ ' Proportion')) +
  scale_y_continuous(expand=c(0, 0)) +
  scale_x_continuous(breaks=seq(0, 1, by=0.25))

# Arrange the plots side-by-side
p <- grid.arrange(p1, p2, ncol=2)

# Save the plot to a file
ggsave("vis_histCountVsProp.png", plot=p, width=8, height=4, dpi=figdpi)













#-------- Figure 3.18: The mongooses experiment ---------#
#--------------------------------------------------------#
mongooses_africa <- atanh(runif(n=100)*1.5-.75) * 12 + 37
mongooses_asia   <- atanh(runif(n=500)*1.5-.75) * 15 + 42

# create common bin boundaries across both data sets
alldata <- c(mongooses_africa, mongooses_asia)
binbounds <- seq(min(alldata), max(alldata), length.out=30)

# top two panels show raw histograms
p1 <- ggplot() +
  geom_histogram(aes(x=mongooses_africa), breaks=binbounds, fill='gray50', 
                 color='black') +
  labs(x=NULL, y=NULL, title=bquote(bold('A)') ~ ' Counts: African mons')) +
  xlim(binbounds[1]-1, binbounds[length(binbounds)]+1) +
  scale_y_continuous(expand=c(0, 0), 
                     limits=c(0, 30), # ylim hard-coded based on N and bins
                     labels=label_number(accuracy=.1)) 

p2 <- ggplot() +
  geom_histogram(aes(x=mongooses_asia), breaks=binbounds, fill='gray50', 
                 color='black') +
  labs(x=NULL, y=NULL, title=bquote(bold('B)') ~ ' Counts: Asian mons')) +
  xlim(binbounds[1]-1, binbounds[length(binbounds)]+1) +
  scale_y_continuous(expand=c(0,0), limits=c(0,30),
                     labels=label_number(accuracy=.1))

# bottom row for proportion
p3 <- ggplot() +
  geom_histogram(aes(x=mongooses_africa, y=after_stat(density)), 
                 breaks=binbounds, fill='gray50', color='black') +
  labs(x=NULL, y=NULL, 
       title=bquote(bold('C)') ~ ' Proportion: African mons')) +
  xlim(binbounds[1]-1, binbounds[length(binbounds)]+1) +
  scale_y_continuous(expand=c(0,0), limits=c(0,.1), 
                     breaks=seq(0,.1, by=.02))

p4 <- ggplot() +
  geom_histogram(aes(x=mongooses_asia, y=after_stat(density)), 
                 breaks=binbounds, fill='gray50', color='black') +
  labs(x=NULL, y=NULL, 
       title=bquote(bold('D)') ~ ' Proportion: Asian mons')) +
  xlim(binbounds[1]-1, binbounds[length(binbounds)] + 1) +
  scale_y_continuous(expand=c(0,0), limits=c(0,.1), 
                     breaks=seq(0, .1, by=.02))

# Arrange the plots side-by-side
p <- grid.arrange(p1, p2, p3, p4, ncol=2)

# Save the plot to a file
ggsave("vis_mongeese_rawProp.png", plot=p, width=12, height=8, dpi=figdpi)













#-------- Figure 3.20: Violin plot from histogram ---------#
#----------------------------------------------------------#
# The data
x1 <- rnorm(100) - .5
x2 <- rnorm(100) + .5
X <- c(x1, x2)

# Regular histogram
p1 <- ggplot() +
  geom_histogram(aes(x=X), bins=nclass.FD(X), fill='gray50', color='black') +
  labs(x='Data value', y='Count', 
       title=bquote(bold('A)') ~ ' Step 1: histogram')) +
  scale_y_continuous(expand=c(0, 0), limits=c(0, 40))

hist1_data <- ggplot_build(p1)$data[[1]]
x <- hist1_data$x
y <- hist1_data$count

# Smooth interpolation of histogram
xx <- seq(min(x), max(x), length=100)
yy <- spline(x, y, method="fmm", xout=xx)$y

p2 <- ggplot() +
  geom_line(aes(x=xx, y=yy), col='black', linewidth=1) +
  labs(x='Data value', y='Count', 
       title=bquote(bold('B)') ~ ' Step 2: interpolate')) +
  scale_y_continuous(expand=c(0, 0), limits=c(0, max(yy)*1.3))

# now for the violin plot
df <- as.data.frame(x=X)
p3 <- ggplot(df, aes(x=0, y=X)) +
  geom_violin(fill='gray50') +
  geom_boxplot(fill='black', width=.03) +
  geom_jitter(width=0.1) +
  labs(x='Count (norm.)', y='Data value',  
       title=bquote(bold('C)') ~ ' Step 3: rotate/mirror')) +
  scale_x_continuous(breaks=0, limits=c(-.5, .5)) +
  scale_y_continuous(expand=c(0, 0))

# Arrange the plots side-by-side
p <- grid.arrange(p1, p2, p3, nrow=1)

# Save the plot to a file
ggsave("vis_makeAviolin.png", plot=p, width=15, height=6, dpi=figdpi)













#-------- Figure 3.22: Linear vs logarithmic plot ---------#
#----------------------------------------------------------#
# simple data... just a line!
y <- seq(1, 10^4, length.out=50)

# create a figure

# plot the line (same data in all plots!)
p1 <- ggplot() +
  geom_line(aes(x=0:(length(y) - 1), y=y), color='black', linewidth=1) +
  coord_cartesian(ylim=c(0, 10^4), xlim=c(0, length(y))) +
  labs(x=NULL, y='Spacing by addition',  
       title=bquote(bold('A)') ~ ' Linear scaling')) +
  scale_x_continuous(breaks=c(0, 20, 40)) 

df <- data.frame(y=y, x=0:(length(y) - 1))
p2 <- ggplot(df) +
  geom_line(aes(x=x, y=y), color='black', linewidth=1) +
  labs(x=NULL, y='Spacing by multiplication',  
       title=bquote(bold('B)') ~ ' Log scaling')) +
  scale_y_continuous(limits=c(1, 10^4), 
                     trans='log10',
                     breaks=10^(0:4)) +
  scale_x_continuous(breaks=c(0, 20, 40)) +
  annotation_logticks(long=unit(0.15, 'cm'), mid=unit(0.075, 'cm'),
                      short=unit(0.075, 'cm'), sides='l', outside=TRUE) +
  coord_cartesian(clip="off")

p3 <- ggplot(df) +
  geom_line(aes(x=x, y=y), color='black', linewidth=1) +
  labs(x=NULL, y='Spacing by multiplication',  
       title=bquote(bold('C)') ~ ' Log scaling')) +
  scale_y_continuous(limits=c(1, 10^4), 
                     trans='log10',
                     breaks=10^(0:4),
                     labels=trans_format("log10", math_format(10^.x))) +
  scale_x_continuous(breaks=c(0, 20, 40)) +
  annotation_logticks(long=unit(0.15, 'cm'), mid=unit(0.075, 'cm'),
                      short=unit(0.075, 'cm'), sides='l', outside=TRUE) +
  coord_cartesian(clip="off")

# Arrange the plots side-by-side
p <- grid.arrange(p1, p2, p3, nrow=1)

# Save the plot to a file
ggsave("vis_linVlog_line.png", plot=p, width=15, height=5, dpi=figdpi)













#-------- Figure 3.24: Radial plots ---------#
#--------------------------------------------#
# data (from https://www.timeanddate.com/weather/%403841798/climate)
tempC  <- c(26, 25, 23, 19, 15, 11, 11, 13, 16, 19, 22, 25)
months <- c('Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 
            'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec')

df <- data.frame(
  group = 'group',
  month = factor(months, levels=months),
  tempC = tempC
)

# Arrange columns order so plot is counterclockwise and Jan is at 0 degrees
df <- pivot_wider(df, names_from=month, values_from=tempC) |>
  select(group, Apr:Jan, Dec:May)

## draw the data
ggradar(df, grid.min=0, grid.mid=15, grid.max=30,
        values.radar=c('', 15, 30),
        plot.title='High temps (°C) near Patagonia',
        group.colours='black',
        group.point.size=3,
        group.line.width=1,
        background.circle.colour='white',
        gridline.min.linetype="solid",
        gridline.mid.linetype="solid",
        gridline.max.linetype="solid",
        gridline.min.colour="gray70",
        gridline.mid.colour="gray70",
        gridline.max.colour="black",
        fill=TRUE,
        fill.alpha=0.2,
        centre.y=0) +
  theme(plot.title=element_text(hjust=0.5))

# Save the plot to a file
ggsave("vis_radialGood.png", width=6.4, height=4.8, dpi=figdpi)













#-------- Figure 3.25: When not to use a radial plot ---------#
#-------------------------------------------------------------#
# fake data
data <- data.frame(group=1, Romcom=1, Horror=8, Docu=6, Anime=3, 
                   Action=7, Scifi=9)
## draw the data
ggradar(data, grid.min=0, grid.mid=5, grid.max=10,
        values.radar=c('', 5, 10),
        plot.title='Movie genre preferences',
        group.colours='black',
        group.point.size=3,
        group.line.width=1,
        background.circle.colour='white',
        gridline.min.linetype="solid",
        gridline.mid.linetype="solid",
        gridline.max.linetype="solid",
        gridline.min.colour="gray70",
        gridline.mid.colour="gray70",
        gridline.max.colour="black",
        fill=TRUE,
        fill.alpha=0.2,
        centre.y=0) +
  theme(plot.title=element_text(hjust=0.5))

# Save the plot to a file
ggsave("vis_radialBad.png", width=6.4, height=4.8, dpi=figdpi)













#-------- Exercise 1 ---------#
#-----------------------------#
# Grouped bar plots both ways
df <- data.frame(matrix(0:11, nrow=4, ncol=3, byrow=TRUE),
                 row.names=0:3)
colnames(df) <- c('A','B','C')
t(df)

# Add Category column
df <- mutate(df, Category=row.names(df))

# Reshape df to long format for plotting with ggplot
df <- pivot_longer(df, cols=-Category, names_to='Column', values_to='Value')

p1 <- ggplot(df, aes(x=Category, y=Value, fill=Column)) +
  geom_bar(stat='identity', position='dodge') +
  labs(x='Category', y='Value', fill=NULL, title='Grouping by columns') +
  scale_fill_manual(values=c(A='#1f76b4', B='#ff7e0e', C='#2ca02c')) +
  theme(legend.position=c(0.1, 0.8),
        legend.background=element_rect(fill=alpha("white", 0.5),
                                         color="grey80")) +
  coord_cartesian(ylim=c(0, 11), xlim=c(0.5, 4.75), expand=FALSE) +
  scale_y_continuous(breaks=seq(0, 10, by=2)) 

p2 <- ggplot(df, aes(x=Column, y=Value, fill=Category)) +
  geom_bar(stat='identity', position='dodge') +
  labs(x='Category', y='Value', fill=NULL, title='Grouping by rows') +
  scale_fill_manual(values=c('#1f76b4', '#ff7e0e', '#2ca02c', '#d62727')) +
  theme(legend.position=c(0.1, 0.8),
        legend.background=element_rect(fill=alpha("white", 0.5),
                                         color="grey80")) +
  coord_cartesian(ylim=c(0, 11), xlim=c(0.5, 3.75), expand=FALSE) +
  scale_y_continuous(breaks=seq(0, 10, by=2))

# Arrange the plots side-by-side
p <- grid.arrange(p1, p2, ncol=2)











#-------- Exercise 2 (Figure 3.7) ---------#
#------------------------------------------#
## create data for the bar plot
# Data sizes
m <- 30 # rows
n <- 6 # columns

# Generate data
data <- matrix(0, nrow=m, ncol=n)

for(i in 0:(n-1)){
  data[,i+1] <- 30 * rnorm(m) * (2*i/(n-1)-1)^2 + (i+1)^2
}
# or you can specify mu and sigma as parameters
# data[, i] <- rnorm(mean=(i+1)^2, sd=30*(2*i/(n-1)-1)^2, n=m)


# Show the bars!
# Reshape data to long format for plotting with ggplot
colnames(data) <- as.character(0:(n - 1))
data <- pivot_longer(as.data.frame(data), cols=everything())

p1 <- ggplot(data, aes(x=name, y=value)) + 
  geom_bar(position="dodge", stat="summary", fun="mean", fill='black') +
  labs(x=NULL, y=NULL, title=bquote(bold('A)') ~ ' Bar plot')) 

p2 <- ggplot(data, aes(x=name, y=value)) + 
  stat_summary(fun.data=mean_sdl, fun.args=list(mult=1), 
               geom="pointrange") +
  labs(x=NULL, y=NULL, title=bquote(bold('B)') ~ ' Error plot'))

p3 <- ggplot(data, aes(x=name, y=value)) + 
  geom_bar(position="dodge", stat="summary", fun="mean", fill='gray50') +
  stat_summary(fun.data=mean_sdl, fun.args=list(mult=1), 
               geom="pointrange") +
  labs(x=NULL, y=NULL, title=bquote(bold('C)') ~ ' Error bar plot'))

# Arrange the plots side-by-side
p <- grid.arrange(p1, p2, p3, nrow=1)

# Save the plot to a file
ggsave("vis_errorbar.png", plot=p, width=10, height=3, dpi=figdpi)









#-------- Exercise 3 ---------#
#-----------------------------#
# A pie chart is appropriate here because the data
# can be converted to proportion (although they are
# not given as proportion, so transformation is needed).

# Create data.frame with data and labels
keys <- c('Chocolate', 'Vanilla', 'Strawberry', 'Pistachio')
data <- data.frame(keys=factor(keys, levels=keys), 
                   values=c(24, 16, 7, 16))

# create data.frame for placing labels outside of the pie chart
data2 <- data %>% 
  mutate(csum=rev(cumsum(rev(values))), 
         pos=values/2 + lead(csum, 1),
         pos=if_else(is.na(pos), values/2, pos))

# Make the pie chart
ggplot(data, aes(x="", y=values, fill=keys)) +
  geom_col(size=1, color='white') +
  coord_polar(theta="y", start=pi/2) +
  labs(x=NULL, y=NULL, fill=NULL) +
  scale_fill_manual(values=c("Chocolate"='#1f76b4', "Vanilla"='#ff7e0e', 
                               "Strawberry"='#2ca02c', "Pistachio"='#d62727')) +
  geom_text(aes(label=paste0(round(values/sum(values) * 100, 1), "%")), 
            position=position_stack(vjust=0.5), size=5) +
  scale_y_continuous(breaks=data2$pos, 
                     labels=keys) +
  theme(legend.position="none",
        axis.line=element_blank(),
        axis.ticks=element_blank())

# Save the plot to a file
ggsave("vis_ex_pie.png", width=6.4, height=4.8, dpi=figdpi)









#-------- Exercise 4 ---------#
#-----------------------------#
data <- rgamma(500, shape=2, scale=2)

p <- ggplot() +
  geom_histogram(aes(x=data), bins=40)
hist_data <- ggplot_build(p)$data[[1]]

# raw histogram values
y <- hist_data$y
x <- hist_data$x

# normalize to percent
yp=(100*y) / sum(y)

p1 <- ggplot() +
  geom_line(aes(x=x, y=y), linewidth=2) +
  scale_x_continuous(expand=c(0, 0), breaks=seq(0, 15, by=2.5), 
                     labels=c('', seq(2.5, 15, by=2.5))) +
  labs(x='Data values', y='Counts', 
       title=bquote(bold('A)') ~ ' Counts (sum=500)'))

p2 <- ggplot() +
  geom_line(aes(x=x, y=yp), linewidth=2) +
  scale_y_continuous(breaks=seq(0, 8, by=2)) +
  scale_x_continuous(expand=c(0, 0), breaks=seq(0, 15, by=2.5), 
                     labels=c('', seq(2.5, 15, by=2.5))) +
  labs(x='Data values', y='Percentage', 
       title=bquote(bold('B)') ~ ' Percentage (sum=100)'))

# Arrange the plots side-by-side
p <- grid.arrange(p1, p2, ncol=2)

# Save the plot to a file
ggsave("vis_ex_histCountPerc.png", plot=p, width=10, height=5, dpi=figdpi)












#-------- Exercise 5 ---------#
#-----------------------------#
# parameters
N <- 200 # sample sizes
k <- 30 # number of bins

# create the data
d1 <- rnorm(N) + 2
d2 <- exp(rnorm(N))

# define the bins
alldata <- c(d1, d2)
bins <- seq(min(alldata), max(alldata), length.out=k+1)

# get histogram values
hist1 <- ggplot() +
  geom_histogram(aes(x=d1), breaks=bins)
hist1_data <- ggplot_build(hist1)$data[[1]]
y1 <- hist1_data$y
xx <- x1 <- hist1_data$x

hist2 <- ggplot() +
  geom_histogram(aes(x=d2), breaks=bins)
hist2_data <- ggplot_build(hist2)$data[[1]]
y2 <- hist2_data$y
x2 <- hist2_data$x

### now for plotting
p1 <- ggplot() +
  geom_histogram(aes(x=d1, fill='Gaussian'), breaks=bins, alpha=.5) +
  geom_histogram(aes(x=d2, fill='Exponential'), breaks=bins, alpha=.5) +
  scale_fill_manual(values=c(Gaussian=rgb(0, 0, 0), 
                               Exponential=rgb(.6, .6, .6)),
                    breaks=c('Gaussian', 'Exponential')) +
  labs(x='Data value', y='Count', fill=NULL,
       title=bquote(bold('A)' ~ ' Histograms using bars'))) + 
  theme(legend.position=c(.6,.6),
        legend.background=element_rect(color="grey80")) +
  scale_y_continuous(breaks=c(0, 20, 40), expand=c(0, 0)) +
  scale_x_continuous(breaks=seq(0, max(alldata), by=2)) +
  guides(fill=guide_legend(
    override.aes=list(fill=c(Gaussian=rgb(.3,.3,.3),
                                 Exponential=rgb(.75, .75, .75)))))

p2 <- ggplot() +
  geom_line(aes(x=xx, y=y1, color='Gaussian'), linewidth=2) +
  geom_line(aes(x=xx, y=y2, color='Exponential'), linewidth=2) +
  scale_color_manual(values=c(Gaussian=rgb(0, 0, 0), 
                                Exponential=rgb(.6, .6, .6)),
                     breaks=c('Gaussian', 'Exponential')) +
  labs(x='Data value', y='Count', color=NULL,
       title=bquote(bold('B)' ~ ' Histograms using lines'))) + 
  theme(legend.position=c(.6,.6),
        legend.background=element_rect(color="grey80")) +
  scale_y_continuous(breaks=c(0, 20, 40), expand=c(0, 0)) +
  scale_x_continuous(breaks=seq(0, max(alldata), by=2))

# Arrange the plots side-by-side
p <- grid.arrange(p1, p2, ncol=2)

# Save the plot to a file
ggsave("vis_ex_histBarsLines.png", plot=p, width=10, height=3, dpi=figdpi)











#-------- Exercise 6 ---------#
#-----------------------------#
x <- seq(0, 6, length.out=1001)

# plot same function in both axes

p1 <- ggplot() +
  geom_line(aes(x=x, y=x, color='y=x', linetype='y=x'), linewidth=1) +
  geom_line(aes(x=x, y=exp(x), color='y=ex', linetype='y=ex'), linewidth=1) + # 'longdash'
  scale_color_manual(values=c('y=x'=rgb(0, 0, 0), 
                                'y=ex'=rgb(.7, .7, .7)),
                     breaks=c('y=x', 'y=ex'), 
                     labels=c(bquote(italic('y=x')), 
                                bquote(italic(paste("y=", e^{x}))))) +
  scale_linetype_manual(values=c('y=x'='solid', 
                                   'y=ex'='dashed'),
                        breaks=c('y=x', 'y=ex'), 
                        labels=c(bquote(italic('y=x')), 
                                   bquote(italic(paste("y=", e^{x}))))) +
  scale_y_continuous(breaks=seq(0, 100, by=20)) +
  coord_cartesian(ylim=c(0, 100), xlim=c(0, max(x)), expand=FALSE) +
  labs(x=NULL, y=NULL, linetype="", color="",
       title=bquote(bold('A)') ~ ' Linear y-axis scale')) +
  theme(legend.position=c(0.2, 0.8),
        legend.background=element_rect(color="grey80"),
        legend.margin=margin(-15, 5.5, 5.5, 5.5)) 

p2 <- ggplot() +
  geom_line(aes(x=x, y=x, color='y=x', linetype='y=x'), linewidth=1) +
  geom_line(aes(x=x, y=exp(x), color='y=ex', linetype='y=ex'), linewidth=1) + # 'longdash'
  scale_color_manual(values=c('y=x'=rgb(0, 0, 0), 
                                'y=ex'=rgb(.7, .7, .7)),
                     breaks=c('y=x', 'y=ex'), 
                     labels=c(bquote(italic('y=x')), 
                                bquote(italic(paste("y=", e^{x})))
                     )) +
  scale_linetype_manual(values=c('y=x'='solid', 
                                   'y=ex'='dashed'),
                        breaks=c('y=x', 'y=ex'), 
                        labels=c(bquote(italic('y=x')), 
                                   bquote(italic(paste("y=", e^{x}))))) +
  scale_y_continuous(trans='log10',
                     labels=trans_format("log10", math_format(10^.x))
  ) +
  annotation_logticks(long=unit(0.15, 'cm'), mid=unit(0.075, 'cm'),
                      short=unit(0.075, 'cm'), sides='l', outside=TRUE) +
  coord_cartesian(clip="off") +
  scale_x_continuous(limits=c(0, max(x)), expand=c(0, 0)) +
  labs(x=NULL, y=NULL, linetype="", color="",
       title=bquote(bold('B)') ~ ' Logarithmic y-axis scale')) +
  theme(legend.position=c(0.2, 0.8),
        legend.background=element_rect(color="grey80"),
        legend.margin=margin(-15, 5.5, 5.5, 5.5))

# Arrange the plots side-by-side
p <- grid.arrange(p1, p2, ncol=2)

# Save the plot to a file
ggsave("vis_ex_linlog.png", plot=p, width=10, height=4, dpi=figdpi)












#-------- Exercise 7 ---------#
#-----------------------------#
# create data
df <- data.frame(norm=rnorm(123), unif=runif(123))

# Reshape df to long format for plotting with ggplot 
df2 <- pivot_longer(df, cols=everything(), names_to='distr')

p1 <- ggplot(df2, aes(x=distr, y=value, 
                      fill=distr)) +
  geom_violin() +
  geom_boxplot(width=0.03, fill='black') +
  geom_jitter(aes(color=distr), width=0.2, size=2) +
  theme(legend.position="none") +
  scale_fill_manual(values=c(norm='gray30', unif='gray60')) +
  scale_color_manual(values=c(norm='black', unif='white')) +
  labs(x='Distribution type', y='Data value', 
       title=bquote(bold('A)') ~ 'Two symmetric violin plots'))

# Create density plot and extract data
p2 <- ggplot(df2, aes(y=value, fill=distr)) +
  geom_density() 
p <- ggplot_build(p2)
data <- p$data[[1]]

# Make negative the x-coordinates for the density plot of normal distribution 
data <- mutate(data, x=ifelse(fill == '#F8766D', -x, x),
               distr=ifelse(fill == '#F8766D', 'norm', 'unif'))

# Create density histograms for both distributions
p2 <- ggplot(data, aes(x=x, y=y, fill=distr)) +
  geom_polygon(color='black')+
  scale_fill_manual(values=c(norm='gray30', unif='gray60')) +
  scale_color_manual(values=c(norm='black', unif='white')) +
  labs(x='', y='Data value', 
       title=bquote(bold('B)') ~ 'One asymmetric violin plot')) +
  scale_x_continuous(breaks=0, labels='', limits=c(-1.5, 1.5)) +
  theme(legend.position=c(0.8, 0.8),
        legend.background=element_rect(color="grey80"))

# Arrange the plots side-by-side
p <- grid.arrange(p1, p2, ncol=2)

# Save the plot to a file
ggsave("vis_ex7.png", plot=p, width=12, height=6, dpi=figdpi)


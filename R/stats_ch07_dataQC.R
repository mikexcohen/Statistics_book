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
# Thanks to Lucicleyton Farias for help with the translation from Python.





# loading libraries
library(dplyr)
library(glue)
library(stringr)
library(ggplot2)
library(patchwork)
library(readr)


custom_theme <- theme_classic() + 
  theme(text = element_text(size = 20),        # font size 
        plot.title = element_text(hjust = 0))  # title location
theme_set(custom_theme)
savefig.dpi <- 300                             # output resolution






#-------- Figure 7.2: What to look for in visual inspection of data ---------#
#----------------------------------------------------------------------------#
# panel A: unexpected range
xA=c(rnorm(20), rnorm(80)*80)
data_A = tibble(data_index=1:length(xA), data_value = xA)

pA = ggplot(data_A, aes(x=data_index, y = data_value)) +
  geom_point(
    shape=22,
    size=5,
    fill = "gray",
    alpha=.8
  ) + 
  theme(
      panel.grid.major = element_blank(), 
      panel.grid.minor = element_blank(),
      panel.background = element_blank(), 
      axis.line = element_line(colour = "black"),
      axis.ticks.x = element_blank(),
      axis.text.x = element_blank(),
      axis.title = element_text(size=15),
      title = element_text(size=15)
  ) + 
  labs(title = bquote(bold("A)")~"Unexpected data range"),
       y = "Data value", x = "Data index")

# panel B: distribution shape
xB = c(5+rnorm(150), exp(1+rnorm(150)))
data_B = tibble(data_index=1:length(xB), data_value=xB)

pB = ggplot(data_B, aes(x=data_value))+
    geom_histogram(color="black", fill="gray", bins=50) + 
    theme(
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"),
        axis.ticks.x = element_blank(),
        axis.text.x = element_blank(),
        axis.title = element_text(size=15),
        title = element_text(size=15)
    ) + 
    labs(title = bquote(bold("B)")~"Nonstandard distribution"),
         y = "Count", x = "Data value")

# panel C: mixed datasets
xC = c(4+rnorm(150), rnorm(150)-4)
data_C = tibble(data_index = 1:length(xC), data_value = xC)

pC = ggplot(data_C, aes(x=data_value))+
    geom_histogram(color="black", fill="gray") + 
    theme(
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"),
        axis.ticks.x = element_blank(),
        axis.text.x = element_blank(),
        axis.title = element_text(size=15),
        title = element_text(size=15)
    ) + 
    labs(title = bquote(bold("C)")~"Mixed dataset"),
         y = "Count", x = "Data value")

# panel D: outliers
xD = rnorm(150)
data_D = tibble(data_index=1:length(xD), data_value=xD)
data_D[60, "data_value"] = 10
data_D[84, "data_value"] = 14

pD = ggplot(data_D, aes(x = 1:nrow(data_D), y = data_value)) +
    geom_point(
        shape=22,
        size=5,
        fill = "gray",
        alpha=.8
    ) + 
    theme(
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"),
        axis.ticks.x = element_blank(),
        axis.text.x = element_blank(),
        axis.title = element_text(size=15),
        title = element_text(size=15)
    ) + 
    labs(title = bquote(bold("D)")~"Outliers"),
         y = "Data value", x = "Data index")


p7.2 = pA + pB + pC + pD
p7.2

ggsave('dataQC_qualInspection.png', width=12, height=6, dpi = savefig.dpi)











#-------- Figure 7.3: Example of dataset with outliers ---------#
#---------------------------------------------------------------#
N = 100
y = rnorm(N)
data = tibble(data_index = 1:length(y), data_value = y)

# and add two random outliers in random positiions
data[sample(N, 2), "data_value"] = runif(2, min=2, max=3)^2

# and plot:
p7.3 = ggplot(data, aes(x = data_index, y = data_value)) + 
    geom_point(
        shape=22, 
        size=5,
        fill = "gray",
        alpha=.8
    ) + 
    xlim(-2, N+1) + 
    theme(
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"),
        axis.title = element_text(size=15),
        axis.text = element_text(size=15),
        title = element_text(size=15)
    ) +
    labs(y = "Data value", x = "Data index")

p7.3
ggsave('dataQC_example2outliers.png', width=8, height=4, dpi = savefig.dpi)










#-------- Figure 7.5: Z-score method for identifying outliers ---------#
#----------------------------------------------------------------------#
# outlier threshold
zThreshold = 3.09

# create some raw data
N = 135
data = exp(rnorm(N)/2) + 5

# zscore the data:
dataZ = (data - mean(data))/sd(data) # sd uses n-1 dof

# identify data indices containing outliers
outliers = abs(dataZ) > zThreshold

# and plot
data = tibble(data_index = 1:length(data), data_value = data)
p1 = ggplot(data, aes(x = data_index, y = data_value)) + 
    geom_point(
        shape=22, 
        size=5,
        fill = "#969696",
        alpha=.8
    ) + 
    theme(
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"),
        axis.title = element_text(size=15),
        axis.text = element_text(size=15),
        title = element_text(size=15)
    ) + 
    labs(title = bquote(bold("A)")~"Original data"),
         y = "Data value", x = "Data index")


dataZ = tibble(data_index = 1:length(dataZ), data_value = dataZ)
p2 = ggplot(dataZ, aes(x=data_index, y = data_value)) + 
    geom_point(
          shape=22, 
          size=5,
          fill = "gray",
          alpha=.8
    ) + 
    geom_point(
        data = dataZ[outliers,],
        aes(x = data_index, y=data_value),
        shape=7,
        stroke=1,
        size=3,
        fill = "gray",
        alpha=.8
    ) +
    geom_hline(
        yintercept = zThreshold, 
        linetype="dashed", 
        linewidth=.7,
        color="gray",
    ) + 
    theme(
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"),
        axis.title = element_text(size=15),
        axis.text = element_text(size=15),
        title = element_text(size=15)
    ) + 
    labs(title = bquote(bold("A)")~"Z-transformed data"),
         y = "Transformed data value", x = "Data index")

p7.5 = p1 + p2
p7.5

ggsave('dataQC_zMethodOutliers.png', width=10, height=4, dpi = savefig.dpi)










#-------- Figure 7.6: Impact of removing outliers on z-values ---------#
#----------------------------------------------------------------------#
# create some raw data
N = 10 # sample size
data = exp(rnorm(N)/2) + 5
data[N] = max(data) + 2 # impose an outlier (at the end for convenience)

dataZ1 = (data - mean(data)) / sd(data) # sd uses n-1 dof
dataZ2 = (data[-N] - mean(data[-N])) / sd(data[-N]) # sd uses n-1 dof

# and plot:
data = tibble(data_index = 1:length(data), data_value = data)
pA = ggplot(data, aes(x = data_index, y = data_value)) +
    geom_point(
        shape=22, 
        size=5,
        fill = "gray"
    ) + 
    theme(
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title = element_text(size=15),
        axis.text = element_text(size=15),
        title = element_text(size=15)
    ) + 
    labs(title = bquote(bold("A)")~"Raw data"),
         y = "Raw data value", x = "Data index")


dataZ1 = tibble(data_index = 1:length(dataZ1), data_value = dataZ1, id="Z with outlier")
dataZ2 = tibble(data_index = 1:length(dataZ2), data_value = dataZ2, id="Z without outlier")
dataZ = rows_append(dataZ1, dataZ2)

pB = ggplot(dataZ, 
       aes(x = data_index, y=data_value, 
           group = data_index,
           shape=id,
           fill=id)) + 
  geom_line(
        linetype="dashed", 
        linewidth=.3
    ) +
    geom_point(
        size=5
    ) + 
    
    scale_shape_manual(values=c("Z with outlier" = 22,
                                "Z without outlier" = 21)) + 
    scale_fill_manual(values=c("Z with outlier" = "#bdbdbd",
                               "Z without outlier" = "#525252")) + 
    theme(
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        legend.position = c(.8,.9),
        legend.title = element_blank(),
        legend.box.background = element_rect(color="gray", linewidth = 1),
        legend.text = element_text(size = 12),
        legend.key = element_blank(),
        legend.key.width = unit(1, "cm"),
        axis.title = element_text(size=15),
        axis.text = element_text(size=15),
        title = element_text(size=15)
    ) + 
    labs(shape="", fill="",
         title = bquote(bold("B)")~"Z-transformed data"),
         y = "Transformed data value", x = "Data index")

p7.6 = pA + pB
p7.6

ggsave('dataQC_recalculatingZ.png', width=10, height=4, dpi = savefig.dpi)









#-------- Figure 7.8: Data trimming ---------#
#--------------------------------------------#
N = 74
y = rnorm(N)^3
data = tibble(data_index=1:length(y), data_value=y)

# find largest and smallest values
k = 2
# sorting ascending order:
data = arrange(data, data_value)
largest = head(data, k)
smallest = tail(data, k)

pA = ggplot(data, aes(x = data_index, y = data_value)) + 
      geom_point(
        shape=22, 
        size=5,
        fill = "gray"
    ) + 
    geom_point(
        data = largest,
        aes(x = data_index, y = data_value),
        shape=7,
        size=5,
        stroke=1,
        fill = "gray",
        alpha=.8
    ) + 
    geom_point(
        data = smallest,
        aes(x = data_index, y = data_value),
        shape=7,
        size=5,
        stroke=1,
        fill = "gray",
        alpha=.8
    ) + 
    theme(
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"),
        axis.title = element_text(size=15),
        axis.text = element_text(size=15),
        title = element_text(size=15)
    ) + 
    labs(title = bquote(bold("A)")~"Data with k-extreme points trimmed"),
         y = "", x = "")

# create a Gaussian probability curve for the panel B
x = seq(-4,4,length=401)
gpdf = dnorm(x, mean = 0, sd = 1)
data = tibble(data_index = x, data_value = gpdf)

# find the indices of the 2.5% and 97.5%
lbndi = which.min(abs(x - qnorm(0.025))) # lower bound index
ubndi = which.min(abs(x - qnorm(0.975))) # upper bound index

# plot the probability function and the vertical lines
pB = ggplot(data = data, aes(x = data_index, y = data_value)) + 
    geom_line() + 
    geom_vline(xintercept = x[lbndi], linetype="dashed") + 
    geom_ribbon(
        data = filter(data, data_index <= x[lbndi]), 
        aes(ymax = data_value, ymin=0),
        fill="#969696") + 
     geom_vline(xintercept = x[ubndi], linetype="dashed") + 
     geom_ribbon(
        data = filter(data, data_index >= x[ubndi]), 
        aes(ymax = data_value, ymin=0),
        fill="#969696") + 
    theme(
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"),
        axis.title = element_text(size=15),
        axis.text = element_text(size=15),
        title = element_text(size=15)
    ) + 
    labs(title = bquote(bold("B)")~"Histogram showing trimmed areas"),
         y = "", x = "")

p7.8 = pA + pB + plot_layout(ncol=1)
p7.8

ggsave('dataQC_trimming.png', width=8, height=6, dpi = savefig.dpi)











#-------- Exercise 1 ---------#
#-----------------------------#
## iterative method
# Note about this code: Because of random numbers, you are not guaranteed to get a result
# that highlights the method. Try running the code several times.

# Simulate the data:
N = 30
data = rnorm(N)
data[data < -1] = data[data < -1] + 2
data[data > 1.5] = data[ data > 1.5]^2

# pick a lenient threshold just for illustration
zscorethresh = 2

# color pallete:
colorz = c("blue", "black", "red", "magenta", "cyan")

# start the skeleton of the plot:
p = ggplot() + 
    theme(
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"),
        axis.title = element_text(size=15),
        axis.text = element_text(size=15),
        title = element_text(size=15)
    ) + 
    labs(y = "Z-score", x = "Data index")

# initiate counter:
numiters = 1

# initiate the dataZ with original data:
dataZ = data

while(TRUE) {
    # convert to z-score:
    datamean = mean(dataZ, na.rm = T)
    datastd = sd(dataZ, na.rm = T)
    dataZ = (dataZ - datamean)/datastd
    
    # find data values to remoove:
    toRemove = dataZ > zscorethresh
    
    # plot points in the current iteration:
    nmb = numiters
    color_ = colorz[nmb]
    data_temp = tibble(data_index = 1:N + numiters/5, data_value = dataZ)
    p <- p +
    geom_text(
        data = data_temp,
        aes(x = data_index, y = data_value),
        label = nmb,
        size = 5,
        color = color_
    )
    
    # break out of the while loop if no points to remove:
    if (sum(toRemove, na.rm = T) == 0) {
        break
    } else{
        # otherwise, mark the outliers in the plot for the current iteration:
        outlier_data = tibble(
            data_index = which(toRemove) + numiters/4,
            data_value = dataZ[toRemove]
        )
        color_ = colorz[numiters]
        p <- p + 
            geom_point(
                data = outlier_data,
                aes(x=data_index, y=data_value),
                shape = 4,
                color = color_,
                size=2,
                stroke=1
            )
        dataZ[toRemove] = NaN
    }
    
    # update counter:
    numiters <- numiters + 1
}

p
ggsave('dataQC_iterativeZmethod.png', width=10, height=4, dpi = savefig.dpi)







#-------- Exercise 2 ---------#
#-----------------------------#
# create data
N = 10000
Y = exp(sin(rnorm(N)))

# make a copy of the data to manipulate:
data = tibble(data_index = 1:length(Y), data_value = Y)
ggplot(data, aes(x = data_value)) + 
    geom_histogram(bins = 40, fill="#2c7fb8", color="white") + 
     theme(
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"),
        axis.title = element_text(size=15),
        axis.text = element_text(size=15),
        title = element_text(size=15)
    ) + 
    labs(y = "", x = "")



# percent to remove (two-tailed)
k = 4 

# finding threshold in the distribution:
q = k/2 # percent from each side
thresh = quantile(data$data_value, c(q/100, 1-q/100))

# data with two-tailed points removed:
data_twoTailed_rmvd = data %>% 
    filter(data_value > thresh[1] & data_value < thresh[2])

# confirm the right number of points:
print(glue("Total dataset size: {nrow(data)}"))
print(glue("Total dataset size: {nrow(data_twoTailed_rmvd)}"))



# compute the mean and median (also used in the next exercise)
mean_Y = mean(data$data_value)
median_Y = median(data$data_value)

mean_Ytrimmed = mean(data_twoTailed_rmvd$data_value)
median_Ytrimmed = median(data_twoTailed_rmvd$data_value)

# print the means
print(glue("Mean of original: {sprintf('%.3f', mean_Y)}"))
print(glue("Mean of trimmed: {sprintf('%.3f', mean_Ytrimmed)}"))

# print the medians
print(glue("Median of original: {sprintf('%.3f', median_Y)}"))
print(glue("Median of trimmed: {sprintf('%.3f', median_Ytrimmed)}"))








#-------- Exercise 3 ---------#
#-----------------------------#
# the range of k values
ks = seq(1, 50, 3)

# initialize a results matrix for mean/median
results = matrix(0, length(ks), 2)

# the experiment!
for (idx in seq_along(ks)) {
    #idx = 1
    # convert that to a number of data points to remove from each tail:
    q = ks[idx]/2
    # get the thresholds:
    thresh = quantile(data$data_value, c(q/100, 1-q/100))
    
    # get the remaining data points:
    data_temp = data %>% 
        filter(data_value > thresh[1] & data_value < thresh[2])
    # collect mean and median and calculate % difference from original ones:
    results[idx, 1] = 100*(mean(data_temp$data_value) - mean(data$data_value)) / mean(data$data_value)
    results[idx, 2] = 100*(median(data_temp$data_value) - median(data$data_value)) / median(data$data_value)
    
    print(glue("Total/valid dataset size: {nrow(data)} -> {nrow(data_temp)}"))
}


ggplot(tibble(data_index = ks, mean = results[,1], median = results[,2])) + 
    geom_point(
        aes(x = data_index, y = mean, 
            color = "Mean",
            fill = "Mean",
            shape = "Mean"
            ),
        size=4
    ) + 
    geom_line(aes(x = data_index, y = mean, color = "Mean")) + 
    geom_point(
        aes(x = data_index, y = median,
            color = "Median", 
            fill = "Median", 
            shape = "Median"),
        size=4
    ) + 
    geom_line(aes(x = data_index, y = median, color="Median")) +
    scale_shape_manual(
        values = c("Mean" = 22,
                   "Median" = 21)
    ) + 
    scale_color_manual(
        values = c("Mean" = "#bdbdbd",
                   "Median" = "#525252")
    ) + 
    scale_fill_manual(
        values = c("Mean" = "#bdbdbd",
                   "Median" = "#525252")
    ) +
    theme(
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        legend.position = c(.1,.1),
        legend.title = element_blank(),
        legend.box.background = element_rect(color="gray", linewidth = 1),
        legend.text = element_text(size = 12),
        legend.key = element_blank(),
        legend.key.width = unit(1, "cm"),
        axis.line = element_line(colour = "black"),
        axis.title = element_text(size=15),
        axis.text = element_text(size=15),
        title = element_text(size=15)
    ) + 
    labs(fill= "", color = "", shape = "", 
         y = expression(paste("Descriptive value (%",Delta,")")), x = "k% to trim")
    
ggsave('dataQC_ex3.png', width=8, height=4, dpi = savefig.dpi)









#-------- Exercise 4 ---------#
#-----------------------------#
N = 1000
x = rf(df1 = 5, df2 = 100, n = N)

# zscore data
xZ = (x - mean(x)) / sd(x) # sd uses n-1 dof
zThresh = 3

# clean data
xClean = x[xZ < zThresh]

# report number of removed data points:
print(glue("Original sample size: {N}"))
print(glue("Cleaned sample size: {length(xClean)}"))
print(glue("Percent data removed: {sprintf('%.2f', 100*(1 - length(xClean)/N))}%"))

# histogram bins using FD rule
edges_fd_all = seq(min(x), max(x), length=nclass.FD(x))
y_fd_all = hist(x, breaks = edges_fd_all, plot=F)

edges_fd_clean = seq(min(xClean), max(xClean), length=nclass.FD(xClean))
y_fd_clean = hist(xClean, breaks = edges_fd_clean, plot=F)

# function to creating dataframe
convert_to_DF = function(y){
    df_ = tibble(
        data_index = y$mids,
        data_value = y$counts
    )
    return (df_)
}

y_fd_all = convert_to_DF(y_fd_all)
y_fd_clean = convert_to_DF(y_fd_clean)

# histogram bins using set boundaries
# Note that I create the bin boundaries using k+1 numbers, then input that 
# vector of boundaries into np.histogram
edges = seq(min(x), max(x), length=41)
y_40_all = hist(x, breaks = edges, plot=F)
y_40_clean = hist(xClean, breaks = edges, plot=F)

# convert to DF:
y_40_all = convert_to_DF(y_40_all)
y_40_clean = convert_to_DF(y_40_clean)

# plotting the histograms:
pA = ggplot() + 
    geom_line(
        data = y_fd_all,
        aes(x = data_index, y = data_value,
            color = "Pre-cleaned",
        ),
        linewidth = .5
    ) +
    geom_point(
        data = y_fd_all,
        aes(x = data_index, y = data_value,
            color = "Pre-cleaned", 
            fill = "Pre-cleaned",
            shape = "Pre-cleaned"
        ),
        size = 5,
        stroke = .2
    ) +
    geom_line(
        data = y_fd_clean,
        aes(x = data_index, y = data_value,
            color = "Cleaned"
        ),
        linewidth = 0.5
    ) + 
    geom_point(
        data = y_fd_clean,
        aes(x = data_index, y = data_value,
            color = "Cleaned",
            fill = "Cleaned",
            shape = "Cleaned"
        ),
        size = 4,
        stroke = .5
    ) + 
    scale_shape_manual(
        name = "",
        values = c(
            "Pre-cleaned" = 22,
            "Cleaned" = 21
        )
    )  +
    scale_color_manual(
        name = "",
        values = c(
            "Cleaned" = "#252525",
            "Pre-cleaned" = "#252525"
        )
    ) + 
    scale_fill_manual(
        name = "",
        values = c(
            "Cleaned" = "#bdbdbd",
            "Pre-cleaned" = "#525252"
        )
    )  +
    theme(
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        legend.position = c(.8,.9),
        legend.title = element_blank(),
        legend.box.background = element_rect(color="gray", linewidth = 1),
        legend.text = element_text(size = 12),
        legend.key = element_blank(),
        legend.key.width = unit(1, "cm"),
        axis.line = element_line(colour = "black"),
        axis.title = element_text(size=15),
        axis.text = element_text(size=15),
        title = element_text(size=15)
    ) + 
    labs(fill= "", color = "", shape = "", 
         title = bquote(bold("A)")~"Histogram using F-D rule"),
         y = "Count", x = "F value")

pB = ggplot() + 
    geom_line(
        data = y_40_all,
        aes(x = data_index, y = data_value,
            color = "Pre-cleaned",
        ),
        linewidth = .5
    ) +
    geom_point(
        data = y_40_all,
        aes(x = data_index, y = data_value,
            color = "Pre-cleaned", 
            fill = "Pre-cleaned",
            shape = "Pre-cleaned"
        ),
        size = 5,
        stroke = .2
    ) +
    geom_line(
        data = y_40_clean,
        aes(x = data_index, y = data_value,
            color = "Cleaned"
        ),
        linewidth = 0.5
    ) + 
    geom_point(
        data = y_40_clean,
        aes(x = data_index, y = data_value,
            color = "Cleaned",
            fill = "Cleaned",
            shape = "Cleaned"
        ),
        size = 4,
        stroke = .5
    ) + 
    scale_shape_manual(
        name = "",
        values = c(
            "Pre-cleaned" = 22,
            "Cleaned" = 21
        )
    )  +
    scale_color_manual(
        name = "",
        values = c(
            "Cleaned" = "#252525",
            "Pre-cleaned" = "#252525"
        )
    ) + 
    scale_fill_manual(
        name = "",
        values = c(
            "Cleaned" = "#bdbdbd",
            "Pre-cleaned" = "#525252"
        )
    )  +
    theme(
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        legend.position = c(.8,.9),
        legend.title = element_blank(),
        legend.box.background = element_rect(color="gray", linewidth = 1),
        legend.text = element_text(size = 12),
        legend.key = element_blank(),
        legend.key.width = unit(1, "cm"),
        axis.line = element_line(colour = "black"),
        axis.title = element_text(size=15),
        axis.text = element_text(size=15),
        title = element_text(size=15)
    ) + 
    labs(fill= "", color = "", shape = "", 
         title = bquote(bold("B)")~"Histogram using 40 bins"),
         y = "Count", x = "F value")


pExercise4 = pA + pB + plot_layout(ncol=1)
pExercise4

ggsave('dataQC_ex4.png', width=8, height=7, dpi=savefig.dpi)











#-------- Exercise 5 ---------#
#-----------------------------#
# import data
data <- read_csv(
    'http://archive.ics.uci.edu/ml/machine-learning-databases/arrhythmia/arrhythmia.data',
    col_select = 1:9,
    col_names = c('age','sex','height','weight','qrs','p-r','q-t','t','p')
)

# inspect
head(data, 5)


#| fig-width: 10
#| fig-height: 5
ggplot(stack(data), aes(x = ind, y = values, fill=ind)) +
    geom_boxplot() + 
    theme(
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        legend.position = "none",
        legend.title = element_blank(),
        legend.box.background = element_rect(color="gray", linewidth = 1),
        legend.text = element_text(size = 12),
        legend.key = element_blank(),
        legend.key.width = unit(1, "cm"),
        axis.line = element_line(colour = "black"),
        axis.title = element_text(size=15),
        axis.text = element_text(size=15),
        title = element_text(size=15)
    ) + 
    labs(fill= "", color = "", shape = "", 
         title = "",
         y = "Data value", x = "Data features")



# scale the data:
cols_ = setdiff(names(data), "sex") # all columns, but 'sex' feature
dataZ = data %>% 
    mutate(across(all_of(cols_), function(x) (x - mean(x))/sd(x)))
# inspecting again
head(dataZ)


#| fig-width: 10
#| fig-height: 5
# box plots of z-scored
ggplot(stack(dataZ), aes(x = ind, y = values, fill=ind)) +
    geom_boxplot() + 
    theme(
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        legend.position = "none",
        legend.title = element_blank(),
        legend.box.background = element_rect(color="gray", linewidth = 1),
        legend.text = element_text(size = 12),
        legend.key = element_blank(),
        legend.key.width = unit(1, "cm") ,
        axis.line = element_line(colour = "black"),
        axis.title = element_text(size=15),
        axis.text = element_text(size=15),
        title = element_text(size=15)
    ) + 
    labs(fill= "", color = "", shape = "", 
         title = "",
         y = "Data value", x = "Data features")


#| fig-width: 10
#| fig-height: 7
# Note: this cell combines the previous graphs to make one figure for the book
p_boxPlot_all = ggplot(stack(data), aes(x = ind, y = values, fill=ind)) +
    geom_boxplot() + 
    theme(
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        legend.position = "none",
        legend.title = element_blank(),
        legend.box.background = element_rect(color="gray", linewidth = 1),
        legend.text = element_text(size = 12),
        legend.key = element_blank(),
        legend.key.width = unit(1, "cm"),
        axis.line = element_line(colour = "black"),
        axis.title = element_text(size=15),
        axis.text = element_text(size=15),
        title = element_text(size=15)
    ) + 
    labs(fill= "", color = "", shape = "", 
         title = bquote(bold("A)")~"Raw data"),
         y = "Data value", x = "")
p_boxPlot_all

# box plots of z-scored
p_boxPlot_z = ggplot(stack(dataZ), aes(x = ind, y = values, fill=ind)) +
    geom_boxplot() + 
    theme(
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"),
        legend.position = "none",
        legend.title = element_blank(),
        legend.box.background = element_rect(color="gray", linewidth = 1),
        legend.text = element_text(size = 12),
        legend.key = element_blank(),
        legend.key.width = unit(1, "cm"),
        axis.title = element_text(size=15),
        axis.text = element_text(size=15),
        title = element_text(size=15)
    ) + 
    labs(fill= "", color = "", shape = "", 
         title = bquote(bold("B)")~"Z-transformed data"),
         y = "Data value", x = "Data features")

p_boxPlot = p_boxPlot_all + p_boxPlot_z + plot_layout(ncol = 1)
p_boxPlot

ggsave('dataQC_ex5b.png', width=10, height=7, dpi=savefig.dpi)












#-------- Exercise 6 ---------#
#-----------------------------#
# remove based on z-score threshold
zThresh = 3.09 # p<.001

# creating copy:
data_clean = data
# loop over all columns
for (col_ in colnames(dataZ)) {
    data_clean[[col_]][dataZ[[col_]] > zThresh] = NA
    data_clean[[col_]][dataZ[[col_]] < -zThresh] = NA
}


#| fig-width: 10
#| fig-height: 7
pA = ggplot(stack(data), aes(x = ind, y = values, fill=ind)) +
    geom_boxplot() + 
    theme(
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        legend.position = "none",
        legend.title = element_blank(),
        legend.box.background = element_rect(color="gray", linewidth = 1),
        legend.text = element_text(size = 12),
        legend.key = element_blank(),
        legend.key.width = unit(1, "cm"),
        axis.line = element_line(colour = "black"),
        axis.title = element_text(size=15),
        axis.text = element_text(size=15),
        title = element_text(size=15)
    ) + 
    labs(fill= "", color = "", shape = "", 
         title = "",
         y = "Data value", x = "")

pB = ggplot(stack(data_clean), aes(x = ind, y = values, fill=ind)) +
    geom_boxplot() + 
    theme(
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        
        legend.position = "none",
        legend.title = element_blank(),
        legend.box.background = element_rect(color="gray", linewidth = 1),
        legend.text = element_text(size = 12),
        legend.key = element_blank(),
        legend.key.width = unit(1, "cm"),
        axis.line = element_line(colour = "black"),
        axis.title = element_text(size=15),
        axis.text = element_text(size=15),
        title = element_text(size=15)
    ) + 
    labs(fill= "", color = "", shape = "", 
         title = "",
         y = "Data value", x = "Data features")

pA + pB + plot_layout(ncol=1)

ggsave('dataQC_ex6.png', width=10, height=7, dpi=savefig.dpi)



# print the means
raw_means = data %>% 
    summarize(across(everything(), mean))

clean_means = data_clean %>% 
    summarize(across(everything(), ~mean(.x, na.rm=T)))

for (col_ in colnames(data)) {
  print(glue('{str_pad(col_, 6, "left", " ")}: {sprintf("%6.2f", raw_means[[col_]])}  ->  {sprintf("%6.2f", clean_means[[col_]])}'))
}



#| fig-width: 9
#| fig-height: 4

# compute percent change
pctchange = 100*(clean_means - raw_means) / raw_means

ggplot(data=stack(pctchange), aes(x = ind, y = values)) + 
    geom_hline(
        yintercept = 0,
        color="black"
    ) + 
    geom_point(
        shape = 22,
        fill = "#969696",
        size=6
    ) + 

    theme_bw() + 
    theme(
        legend.position = "none",
        legend.title = element_blank(),
        legend.box.background = element_rect(color="gray", linewidth = 1),
        legend.text = element_text(size = 12),
        legend.key = element_blank(),
        legend.key.width = unit(1, "cm"),
        axis.line = element_line(colour = "black"),
        axis.title = element_text(size=15),
        axis.text = element_text(size=15),
        title = element_text(size=15),
        plot.title = element_text(hjust = .5)
        
    ) + 
    labs(title = "Change in feature means after z-score data rejection",
         y = "Percent", x = "")

ggsave('dataQC_ex6b.png', width=9, height=4, dpi=savefig.dpi)


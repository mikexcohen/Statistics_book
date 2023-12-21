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
# Thanks to Joy Poulo for help with the translation from Python.



 
 

 
## -- import libraries and define global settings
library(ggplot2) # plotting
library(cowplot) # plot_grid()
library(emg)     # remg() : exGaussian distribution
library(pracma)  # logspace()
library(tidyr)   # gather()

theme_set(theme_classic(base_size = 14))
options(scipen=999)

 
 


#-------- Figure 9.1: Sampling variability in random data ---------#
#------------------------------------------------------------------#
N <- 500
nSamples <- 50
kHistBins <- 20

# bins for histograms
edges <- seq(-3, 3, length.out = kHistBins + 1)

# declare matrices
allHistY <- matrix(0, nrow = nSamples, ncol = kHistBins)
allMeans <- rep(0,nSamples)
ldatall <- data.frame(mids=NULL,Count=NULL)


for (sampi in 1:nSamples) {
  
  # create data (same parameters in all simulations!)
  data <- rnorm(N, mean=0, sd=1)
  
  # histograms
  histY <- hist(data[abs(data) <= 3.0], breaks = edges, plot = FALSE)
  allHistY[sampi, ] <- histY$counts
  
  ldat <- data.frame(mids=histY$mids,Count=histY$counts)
  
  #get means
  allMeans[sampi] <- mean(data)
  ldatall <- rbind(ldatall,ldat)
  
}

# plots
titleA <- expression(paste(bold("A)")," Histograms of each sample"))
titleB <- bquote(bold("B)")~"Scatter of sample means (mean="~.(sprintf("%.3g",mean(allMeans)))*")")

c <- runif(50, min = 0.5, max = 0.9)

ggp1 <- ggplot(data=ldatall)+aes(x=mids,y=Count) +theme(legend.position = "none")  +
        geom_point(colour=rgb(rep(c,20),rep(c,20),rep(c,20)),size=3.5)+
        # plot the average histogram
        geom_line(stat='summary',fun='mean',linewidth=1.5) + 
        labs(x='Data value', y= 'Count', title = titleA) + 
        theme(axis.text = element_text(size=14), plot.title=element_text(vjust= -1)) 


# plot the means, and the mean of the means

ggp2 <- ggplot()+ geom_point(aes(x=1:50,y=allMeans),shape=22,size=5,fill=rgb(c,c,c),stroke=1.0) +
        geom_hline(yintercept=mean(allMeans),linetype=2,linewidth=.8) +
        labs(x='Sample number',y='Sample mean', title=titleB) + 
        theme(legend.position = "none", axis.text = element_text(size=14), plot.title=element_text(vjust= -1))

# setup figure
plot_grid(ggp1,ggp2,ncol=1,align='v')
# save the figure as 'png'
ggsave("sample_exampleWithRandom.png", device = "png", width=7,height=6)


 
 





















#-------- Figure 9.2: Samples and variability of sample means ---------#
#----------------------------------------------------------------------#
# number of samples
nSamples <- 50

# histogram resolution
k <- 30
edges <- seq(-3, 14, length.out = 31)
xx <- (edges[-1] + edges[-length(edges)]) / 2

# initialize output matrices
meenz  <- rep(0, nSamples) # sample averages
allYYs <- rep(0, k)        # average of histograms
dfallYYs <- data.frame(xx=NULL,yy=NULL)
minyy <- rep(0,nSamples)

# setup plot
ggp1 <- ggplot() +theme(legend.position = "none") + xlim(-3,14)

# loop over samples
for (i in 1:nSamples) {

  # generate random data from an exGaussian distribution
  lambda <- 1/(runif(1,min=0.1,max=5.0))
  randomX = remg(n=2000,lambda=lambda)
  
  # get its histogram and normalize
  yy <- hist(randomX[randomX >= -3.0 & randomX <= 14.0], breaks = edges, plot = FALSE)$counts
  yy <- yy / sum(yy)

  # average the distributions
  allYYs <- allYYs + yy
  dfyy <- data.frame(xx=xx,yy=yy)
  dfallYYs <- rbind(dfallYYs,dfyy)
  
  
  # store the average of the distribution
  meenz[i] <- mean(randomX)
  minyy[i] <- yy[which.min(abs(xx - meenz[i]))]
  mexy <- data.frame(x=meenz[i],y=minyy[i])
  

  # plot the line
  rc <- runif(1, min = 0.4, max = 0.8) # random color -->
  
 #plot the lines
 ggp1 <- ggp1   + geom_line(data=data.frame(xx,yy),mapping= aes(x=xx,y=yy),colour=rgb(rc,rc,rc),linewidth=.5) +
         #plot the average histogram
         geom_point(data=mexy,mapping=aes(x=x,y=y),shape=11,size=3.0,stroke=1.0,fill=rgb(rc,rc,rc),col='black')
}



titleB <- expression(paste(bold("B)")," Data distributions"))
titleC <- expression(paste(bold("C)")," Means distribution"))

# add labels and title
ggp1 <- ggp1 + labs(x='', y= 'Probability', title = titleB) +
        theme(axis.text.y = element_blank(),axis.ticks.y = element_blank() , 
              axis.text = element_text(size=14), plot.title=element_text(vjust= -1))

## the distribution of sample means
ggp2 <-  ggplot() + geom_line(data=data.frame(xx,ally=allYYs/max(allYYs)*5),mapping=aes(x=xx,y=ally),linewidth=1.0) +
         geom_histogram(data=data.frame(x=1:50, y=meenz), mapping=aes(y), bins=20, binwidth=.2, boundary=0,
                        fill=rgb(0.7,0.7,0.7), color='black') + 
         xlab('Data value')+ylab('Count')+ggtitle(titleC) + 
         theme(axis.text.y = element_blank(),axis.ticks.y = element_blank(),
               axis.text = element_text(size=14), plot.title=element_text(vjust= -1)) + 
         scale_x_continuous(expand=c(0,0)) + scale_y_continuous(expand=c(0,0))


plot_grid(ggp1,ggp2,ncol=1,align='v')

ggsave("sample_distOfExGausMeans.png", device = "png", width=4,height=6)


 















 
 
 

#-------- Figure 9.4: Law of Large Numbers (demo 1) ---------#
#------------------------------------------------------------# 
# generate "population"
population <- c(1, 2, 3, 4)
for (i in 1:20) {
  population <- c(population, population)
}

nPop <- length(population)
expval <- mean(population)

print(paste("Expected value (population mean):", expval))
print(paste("Population size:", nPop))


 
## experiment: draw larger and larger samples

k <- 1500  # maximum number of samples
sampleAves <- rep(0,k)

for (i in 1:k) { 
  # get a sample
  sample <- sample(population,size=i+1, replace = TRUE)

  # compute and store its mean
  sampleAves[i] = mean( sample )
}

# visualize!
ggp1 <- ggplot() + geom_point(mapping=aes(x=1:k,y=sampleAves,col='Sample average'), 
                              shape=22, size=3, stroke=0.8, fill=rgb(.9,.9,.9), show.legend=T) + 
  geom_segment(linewidth=2,aes(linetype = "Population average",x=0,xend=k,y=expval,yend=expval)) + 
  xlab('Sample size') + ylab('Value') + 
  scale_linetype_manual(values = "solid") + scale_color_manual(values=rgb(.6,.6,.6) ) + 
  scale_x_continuous(limits=c(-15,1525),expand = expansion(c(0,0)) ) +
  theme(legend.position = c(.85,0.85),legend.title=element_blank(),legend.spacing.y = unit(0, "mm"), 
        legend.margin = margin(c(1,1,1,1)), legend.background = element_blank(),
        legend.box.background = element_rect(colour = "black"),legend.text=element_text(size=14), 
        axis.text = element_text(size=14)) + 
  guides(linetype=guide_legend(override.aes=list(shape=NA)), colour = guide_legend(order = 1))

ggp1
ggsave('sample_LLNdemo1.png',device = "png", width=8,height=4)


 
 





















 

#-------- Figure 9.5: Law of Large Numbers (demo 2) ---------#
#------------------------------------------------------------# 
# parameters and initializations
samplesize   <- 30
numberOfExps <- 50
samplemeans  <- rep(0,numberOfExps)

# run the experiment!
for (expi in 1:numberOfExps) {
  # compute and store its mean
  samplemeans[expi] <- mean( sample(population,size=samplesize, replace = T) )
}



# each individual sample mean

ggp1 <- ggplot() + geom_point(aes(x=1:numberOfExps,y=samplemeans),shape=22, size=3, stroke=.8, 
                              col=rgb(.6,.6,.6), fill=rgb(.9,.9,.9)) + 
  xlab('Sample number (s)') + ylab('Mean value') +
  ggtitle(expression(paste(bold("A)")," Each sample mean"))) +
  geom_segment(linewidth=1,aes(x=0,xend=numberOfExps,y=mean(population),yend=mean(population))) +
  ylim(min(samplemeans)*.85,1.1*max(samplemeans)) + 
  scale_x_continuous(limits=c(-1,53),expand = expansion(c(0,0)) ) +
  theme(axis.text = element_text(size=14), plot.title=element_text(vjust= -1))

breaks <- seq(0,numberOfExps,10)
# cumulative average over the samples

ggp2 <- ggplot() + geom_point(mapping=aes(x=1:numberOfExps,y=cumsum(samplemeans)/(1:numberOfExps)),
                              shape=22,size=3, stroke=.8, col=rgb(.6,.6,.6),fill=rgb(.9,.9,.9)) + 
  xlab("") + ylab('Mean value') + ggtitle(expression(paste(bold("B)")," Cumulative sample means"))) +
  # multiline xtick labels
  scale_x_continuous(limits=c(-1,53),expand = expansion(c(0,0)), breaks = breaks, labels=function(x){paste0("s=",x,"\nN=",x*30)}) + 
  theme(axis.text = element_text(size=14), plot.title=element_text(vjust= -1)) +
  # mean of population
  geom_segment(linewidth=1,aes(x=0,xend=numberOfExps,y=mean(population),yend=mean(population))) +
  ylim(min(samplemeans)*.85,1.1*max(samplemeans))

plot_grid(ggp1,ggp2,ncol=1,align='v')

ggsave('sample_LLNdemo2.png',device = "png", width=7,height=5)























 
 

#-------- Figure 9.6: Visualization of sample mean variability ---------#
#-----------------------------------------------------------------------# 
N <- 512
X <- matrix(runif(2*N),ncol=2)


sample1 <- X[sample(N,size=40,replace=TRUE),]
sample2 <- X[sample(N,size=40,replace=TRUE),]


# plot all data points
ggp1 <- ggplot() +
  geom_point(data = as.data.frame(X), aes(x = X[,1], y = X[,2], color = "All data"), 
             shape = 22, fill = "white", size = 4,stroke=1) +
  # plot sample data 
  geom_point(data = as.data.frame(sample1), aes(x = sample1[,1], y = sample1[,2], color = "Sample 1"), 
             shape = 16, size=3) +
  geom_point(data = as.data.frame(sample2), aes(x = sample2[,1], y = sample2[,2], color = "Sample 2"), 
             shape = 17, size=3) +
  # plot sample means
  geom_point(data = as.data.frame(colMeans(sample1)), aes(x = colMeans(sample1)[1], y = colMeans(sample1)[2] , 
                                                          color = "Mean S1"), shape = 16, size = 6) +
  geom_point(data = as.data.frame(colMeans(sample2)), aes(x = colMeans(sample2)[1], y = colMeans(sample2)[2] , 
                                                          color = "Mean S2"), shape = 17, size = 5) +
  labs(x = "Variable 1", y = "Variable 2") +
  scale_color_manual(labels= c('All data', 'Sample 1', 'Sample 2', 'Mean S1', 'Mean S2' ),values = c("gray70", "blue", "red", "blue", "red")) +
  guides(color = guide_legend(override.aes = list(shape = c(22, 16, 17, 16, 17),size=c(4,3,3,6,5)))) +
  theme(legend.position = "right", legend.justification='top',legend.title=element_blank(), 
        legend.spacing.y = unit(0, "mm"), legend.background = element_blank(),
        legend.box.background = element_rect(colour = "black"),legend.text=element_text(size=14),
        axis.text = element_blank(),axis.ticks = element_blank())

ggp1

ggsave("sample_meanOfSamplesGeom.png", device = "png",  width = 8, height = 6)



 


























 

#-------- Figure 9.7: Sample biases can be overcome by LLN (given some assumptions) ---------#
#--------------------------------------------------------------------------------------------#
N <- 512
XD <- matrix(runif(N*2), ncol=2)

# nonrandom sorting
XD <- XD[order(rowSums(XD^2)),]

# plot all data points
ggp1 <- ggplot() + geom_point(data = as.data.frame(XD), aes(x = XD[,1], y = XD[,2]), 
                              shape = 22, color = "gray", fill = "white", size = 4) 

# nonrandom sampling to simulate a bias
sampmeans <- matrix(0,nrow=6,ncol=2) # hard-coded to 6 samples...
sampbias <- as.integer(seq(20,N-40,length.out=6))
bxid <- NULL
shapes <- c(16,17,18,42,4,43)
bshapes <- rep(shapes,each=40)
colors <- c('blue','red','magenta','black','green','cyan')
bcolors <- rep(colors,each=40)
for (si in 1:6) { 
  
  # biased sample and its mean
  xid <- (sampbias[si]+1):(sampbias[si]+40)
  bxid <- c(bxid,xid)
  sample <- XD[xid,]
  sampmeans[si,] <- colMeans(sample)  
}  
# plot samples
bXD <- XD[bxid,]
ggp1 <- ggp1 + geom_point(aes(x = bXD[,1], y = bXD[,2]), 
                          shape = bshapes, color = bcolors, fill = bcolors, size = rep(c(3,3,4,6,4,6),each=40)) +
  # plot sample mean
  geom_point(data=as.data.frame(sampmeans),mapping=aes(x = sampmeans[,1], y = sampmeans[,2], 
                                                       col=paste("a",1:6)), shape = shapes, size = c(6,6,7,9,7,9),show.legend = TRUE) +
  # plot the average of sample means
  geom_point(data = as.data.frame(sampmeans), mapping=aes(x = mean(sampmeans[,1]), y = mean(sampmeans[,2]),
                                                          col="b"), shape = 19,   size = 10) +
  # manually specify the color and legend labels
  scale_color_manual(labels= c(paste("Sample",1:6),"Average of means"),values = c(colors,'black')) +
  guides(color = guide_legend(override.aes = list(shape = c(shapes,19),size=c(3,3,4,6,4,6,10)))) +
  theme(legend.position = "right", legend.justification='top',legend.title=element_blank(),
        legend.spacing.y = unit(0, "mm"), legend.background = element_blank(),
        legend.box.background = element_rect(colour = "black"),legend.text=element_text(size=14), 
        axis.text = element_blank(),axis.ticks = element_blank()) +
  labs(x = "Variable 1", y = "Variable 2") 


ggp1
ggsave("sample_meanOfSamplesGeom_biased.png", device='png',width = 8, height = 6)

  

 

















 
 

#-------- Figure 9.8: CLT, demo 1 ---------#
#------------------------------------------#
# generate "population" (simulating a weighted die)
population <- c(1, 1, 2, 2, 3, 4, 5, 6)
for (i in 1:20) { 
    population <- c(population,population)
}
nPop <- length(population)
expval <- mean(population)
print(paste('Expected value (population mean):', expval))
print(paste('Population size:', nPop))

 
 
# parameters and initializations
samplesize   <- 30
numberOfExps <- 500
samplemeans  <- rep(0,numberOfExps)

# run the experiment!
for (expi in (1:numberOfExps)) { 
  # compute and store its mean
  samplemeans[expi] = mean( sample(population,size=samplesize, replace = TRUE) )
}

# show the results
ggp1 <- ggplot() +
        # histogram of the data
        geom_histogram( aes(x = population), breaks = seq(0.5,7.5,by=1), fill = "#CCCCCC", color = "black") +
        labs(x = "Die face", y = "Count", title=expression(paste(bold("A) "), "Distribution of population data"))) +
        scale_y_continuous(expand = expansion(c(0,0)) ) + 
        theme(axis.text = element_text(size=14), plot.title=element_text(vjust= -1))

# histogram of the sample means
ggp2 <- ggplot()+ geom_histogram( aes(x = samplemeans),bins=20,fill="#CCCCCC",color='black') +
        labs(x='Sample mean', y='Count', title=expression(paste(bold("B) "), "Distribution of sample means"))) +
        scale_y_continuous(expand = expansion(c(0,0)) ) +
        theme(axis.text = element_text(size=14), plot.title=element_text(vjust= -1))

plot_grid(ggp1,ggp2,nrow=1,align='h')

ggsave('sample_CLTdemo1.png',device = "png",  width = 10, height = 4)



 
 
















 
 

#-------- Figure 9.9: CLT, demo 2 ---------#
#------------------------------------------#
# new population!
Npop <- 1000000
population <- rnorm(Npop)**2


# parameters and initializations
samplesize   <-  30
numberOfExps <- 500
samplemeans  <- rep(0,numberOfExps)

# run the experiment!
for (expi in 1:numberOfExps) { 
  # compute and store its mean
  samplemeans[expi] <- mean( sample(population,size=samplesize, replace = TRUE) )
}

# show the results

# histogram of the data

ggp1 <- ggplot() +
  # histogram of the data
  geom_histogram( aes(x = population), bins=50, fill = "#CCCCCC", color = "black") +   
  labs(x = "Data value", y = "Count") + ggtitle(expression(paste(bold("A) "), "Distribution of population data"))) +
  scale_y_continuous(expand = expansion(c(0,0)) ) +
  theme(axis.text = element_text(size=14), plot.title=element_text(vjust= -1))

# histogram of the sample means
xx <- hist(samplemeans,20, plot=F)$breaks
ggp2 <- ggplot()+ geom_histogram( aes(x = samplemeans), breaks=xx, fill="#CCCCCC", color='black') +
  labs(x='Sample mean', y='Count', title=expression(paste(bold("B) "), "Distribution of sample means"))) + 
  scale_x_continuous(limits=c(0,4),expand = expansion(c(0,0)))  + 
  scale_y_continuous(expand = expansion(c(0,0)) ) +
  theme(axis.text = element_text(size=14), plot.title=element_text(vjust= -1))

plot_grid(ggp1,ggp2,nrow=1,align='h')

ggsave('sample_CLTdemo2.png',device = "png",  width = 10, height = 4)



 
 





 














 


#-------- Figure 9.10: CLT, demo 3 ---------#
#-------------------------------------------#
# create two data variables
x <- seq(0, 6 * pi, length.out = 10000)
s <- sin(x)
u <- 2 * runif(length(x)) - 1

# combine them into a list for convenience
datasets <- list(s, u, s + u)
axislets <- c('A', 'B', 'C', 'D', 'E', 'F') # axis labels

plist <- list()

for (i in 1:3) {
  dlab <- ifelse(i < 3, as.character(i), '1+2')
  dframe <- data.frame(yy=datasets[[i]])
  ggp1 <- ggplot(data = dframe, aes(x = x, y = yy)) +
    geom_point(color = 'black', size = .5) +
    ggtitle(bquote(bold(.(axislets[(i-1)*2+1]))*")  Data "~.(dlab))) +
    theme(axis.title = element_blank(), axis.line.x = element_blank(),
          axis.text.x = element_blank(), axis.ticks.x = element_blank(),
          axis.text.y = element_text(size=16),
          plot.title = element_text(vjust= -1))
 
  ggp2 <- ggplot(data = dframe, aes(x = yy)) +
    geom_histogram(bins = 200, color = 'black', fill = 'black',linewidth=0.1) +
    ggtitle(bquote(bold(.(axislets[i*2]))*")  Histogram "~.(dlab))) +
    theme(axis.title = element_blank(), axis.line.x = element_blank(),
          axis.text.x = element_blank(), axis.ticks.x = element_blank(),
          axis.text.y = element_text(size=16),
          plot.title = element_text(vjust= -1))
  
  
 plist[[(i-1)*2+1]]=ggp1
 plist[[(i*2)]]=ggp2

}
plot_grid(plist[[1]],plist[[2]],plist[[3]],plist[[4]],plist[[5]],plist[[6]],ncol=2)

ggsave('sample_CLTdemo3a.png', device="png", width = 7, height = 6)



 
 
  






















 

#-------- Figure 9.11: CLT requires comparable scaling ---------#
#---------------------------------------------------------------# 
# only difference from the previous figure is the amplitude-scaling!
s = 10*sin(x)

# combine them into a list for convenience
datasets <- list(s, u, s + u)
axislets <- c('A', 'B', 'C', 'D', 'E', 'F') # axis labels

# plot
plist <- list()
for (i in 1:3) {
  # axis variable label
  dlab <- ifelse(i < 3, as.character(i), '1+2')
  dframe <- data.frame(yy=datasets[[i]])
  # plot the data
  ggp1 <- ggplot(data = dframe, aes(x = x, y = yy)) +
    geom_point(color = 'black', size = .5) +
    ggtitle(bquote(bold(.(axislets[(i-1)*2+1]))*")  Data "~.(dlab))) +
    # adjust the axis properties
    theme(axis.title = element_blank(), axis.line.x = element_blank(),
          axis.text.x = element_blank(), axis.ticks.x = element_blank(),
          axis.text.y = element_text(size=14),
          plot.title = element_text(vjust= -1))
  # plot the histogram
  ggp2 <- ggplot(data = dframe, aes(x = yy)) +
    geom_histogram(bins = 200, color = 'black', fill = 'black',linewidth=0.1) +
    ggtitle(bquote(bold(.(axislets[i*2]))*")  Histogram "~.(dlab))) +
    # adjust the axis properties
    theme(axis.title = element_blank(), axis.line.x = element_blank(),
          axis.text.x = element_blank(), axis.ticks.x = element_blank(),
          axis.text.y = element_text(size=14),
          plot.title = element_text(vjust = -1))

  plist[[(i-1)*2+1]]=ggp1
  plist[[(i*2)]]=ggp2
}

plot_grid(plist[[1]],plist[[2]],plist[[3]],plist[[4]],plist[[5]],plist[[6]],ncol=2)
ggsave('sample_CLTdemo3b.png', device='png',width = 7, height = 6)











 





#-------- Exercise 1 ---------#
#-----------------------------#
# variance levels (tau^2)
tau2levels = seq(.1,10,length.out=40)

# simulation parameters
samplesize = 200
numsamples =  20
l <- length(tau2levels)
# initialize results matrix
resultm = matrix(0,nrow=numsamples,ncol=l)
resultv = matrix(0,nrow=numsamples,ncol=l)

### run the experiment!
# loop over tau levels
for (ni in seq(l)) { 

  # repeat for multiple samples
  for (sampi in seq(numsamples)) { 

    # generate sample data with tau modulation
    data = rnorm(n=samplesize,0,sqrt(tau2levels[ni]))

    # store sample mean and variance
    resultm[sampi,ni] = mean(data)
    resultv[sampi,ni] = var(data)
  }
}

### plotting

# plot the average of the sample means
lresultm <- gather(data.frame(t(resultm),tau2levels),tau,resultm,-tau2levels) # long data frame
ggp1 <- ggplot(lresultm) + aes(x=tau2levels,y=resultm) +
  geom_point(fill=rgb(.6,.6,.6),col='black',shape=22,size=4,stroke=.8) +
  stat_summary(fun='mean',geom='point',shape=23, size=4,stroke=.8, col='black',fill='white') + 
  labs(x=expression(tau^2), y='Value', title=expression(paste(bold("A)"), " Sample averages"))) +
  scale_x_continuous(limits=c(-0.2,10.5),expand = expansion(c(0,0)) ) +
  theme(axis.title = element_text(size=14), axis.text = element_text(size=14), plot.title=element_text(vjust= -1))
 
  

# plot the average within-sample variances
# plot the average across-sample variance of the sample means
ggp2 <- ggplot() + geom_point(aes(x=tau2levels,y=colMeans(resultv), shape= 'Average variances'), 
                              fill= rgb(.7,.7,.7), color='black', size=3, stroke=.8) +
  geom_point(aes(x=tau2levels,y=apply(resultm,2,var), shape= 'Variance of averages'),      
             fill= rgb(.8,.8,.8), color='black', size=3,stroke=.8) +      
  labs(x=expression(tau^2), y='Value', title=expression(paste(bold("B)"), " Sample variances"))) +
  scale_shape_manual(values=c(24,21))  + 
  scale_x_continuous(limits=c(-0.2,10.5),expand = expansion(c(0,0)) ) +
  theme(axis.text = element_text(size=14), legend.position = c(.25,0.8), legend.margin = margin(c(0,5,6,5)),
        legend.title=element_blank(), legend.background = element_blank(), 
        legend.box.background = element_rect(colour = "black"), legend.text=element_text(size=14),
        axis.title=element_text(size=14), plot.title=element_text(vjust= -1))

plot_grid(ggp1,ggp2,ncol=1)
ggsave('sample_ex1.png',  device='png',width = 7, height = 7)


















 
#-------- Exercise 2 ---------#
#-----------------------------#
# the sample sizes
samplesizes <- seq(10,1000)

# generate population data with known std
pop_std     <- 2.4
populationN <- 1000000
population  <- rnorm(populationN)
population  <- population / sd(population) # force std=1
population  <- population * pop_std # force std


# initialize results matrix
samplestds <- rep(0,length(samplesizes))

# run the experiment!
for (sampi in 1:length(samplesizes)) { 

  # pick a random sample
  sample <- sample(population,size=samplesizes[sampi], replace = TRUE)
  samplestds[sampi] <- sd(sample)
}

# show the results!

ggp1 <- ggplot()+ geom_point(mapping=aes(x=samplesizes,y=samplestds,col='Sample stds'), shape=22, size=3, 
                             stroke=0.8, fill=rgb(.9,.9,.9),show.legend=T) + 
  geom_segment(linewidth=1.5,aes(linetype = "Population std",x=3,xend=1007,y=pop_std,yend=pop_std)) + 
  xlab('Sample size') + ylab('Standard deviation value')  + 
  scale_linetype_manual(values = "solid") + 
  scale_color_manual(values=rgb(.6,.6,.6) ) + 
  theme(legend.position = c(0.85,0.9),legend.title=element_blank(),legend.spacing.y = unit(0, "mm"), 
        legend.margin = margin(c(1,5,2,5)), legend.background = element_blank(), 
        legend.box.background = element_rect(colour = "black"),legend.text=element_text(size=14), 
        axis.text = element_text(size=14),axis.title=element_text(size=14)) + 
  guides(linetype=guide_legend(override.aes=list(shape=NA)), colour = guide_legend(order = 1)) +
  scale_x_continuous(limits=c(-3,1020),expand = expansion(c(0,0)) )
  
show(ggp1)
ggsave('sample_ex2.png',device='png',width = 8, height = 4)


 
 
 
# Note about the data-generation method:
# It is not sufficient to use rnorm(N,0,2.4), because that does
# not guarantee a *population* standard deviation of 2.4. Instead, it is
# necessary to force the std by first scaling to std=1 and then multiplying.

# Here's a demonstration:
print(sd(rnorm(populationN,0,pop_std)))
print(sd(population))

 
  




















#-------- Exercise 3 ---------#
#-----------------------------#
# parameters
popMean1 <- 3
popMean2 <- 3.2


# generate the populations
population1 <- rnorm(populationN)
population1 <- population1 - mean(population1) + popMean1

population2 <- rnorm(populationN)
population2 <- population2 - mean(population2) + popMean2

# one sample
s1 <- mean( sample(population1,size=30, replace = TRUE) )
s2 <- mean( sample(population2,size=30, replace = TRUE) )

cat(sprintf("Population difference: %.3f\n", popMean1-popMean2))
cat(sprintf("Sample difference:     %.3f\n", s1-s2))

 
# initialize results matrix
samplediffs <- rep(0,length(samplesizes))

# run the experiment!
for (sampi in 1:length(samplesizes)) { 

  # pick a random sample
  s1 <- sample(population1,size=samplesizes[sampi], replace = TRUE)
  s2 <- sample(population2,size=samplesizes[sampi], replace = TRUE)
  samplediffs[sampi] = mean(s1) - mean(s2)
}

# show the results!

ggp1 <- ggplot()+ geom_point(mapping=aes(x=samplesizes,y=samplediffs,col='Sample diffs'),
                             shape=22,size=3,stroke=0.8,fill=rgb(.9,.9,.9),show.legend=T) + 
  geom_segment(linewidth=1.5,aes(linetype = "Population diff", x=3,xend=1007, y=popMean1-popMean2,
                                 yend=popMean1-popMean2)) +
  geom_segment(aes(x=3,xend=1007,y=0,yend=0),linetype=2,col=rgb(.7,.7,.7)) + 
  xlab('Sample size') + ylab('Sample differences')  + 
  scale_linetype_manual(values = "solid") + 
  scale_color_manual(values=rgb(.6,.6,.6) ) + 
  theme(legend.position = c(0.85,0.9),legend.title=element_blank(),legend.spacing.y = unit(0, "mm"), 
        legend.margin = margin(c(1,5,2,5)), legend.background = element_blank(), 
        legend.box.background = element_rect(colour = "black"),legend.text=element_text(size=15), 
        axis.text = element_text(size=14),axis.title=element_text(size=15)) +
  guides(linetype=guide_legend(override.aes=list(shape=NA)), colour = guide_legend(order = 1)) +
  scale_x_continuous(limits=c(-3,1020),expand = expansion(c(0,0)) )

  
show(ggp1)
ggsave('sample_ex3.png',device='png',width = 8, height = 4)


 
 























#-------- Exercise 4 ---------#
#-----------------------------#
N <- 1200
numbers <- matrix(0,nrow=N,ncol=3)

for (i in 1:N) { 
  nums <- sample(1:100,2, replace = TRUE)
  numbers[i,1] <- nums[1]
  numbers[i,2] <- nums[2]
  numbers[i,3] <- mean(nums)
}

# show the histograms
plist=list()
figlets <- c("A","B","C")
fignams <- c("First number","Second number", "Their average")

for (i in 1:3){
  dframe <- data.frame(x=numbers[,i]) 
  ggp <- ggplot(dframe) + aes(x = x) +
  geom_histogram(aes(y=after_stat(density)), bins=30, fill = rgb(.3,.3,.3), color = rgb(.3,.3,.3)) +   
  labs(x = "Number", y = "Proportion",title= bquote(.(figlets[i])*bold(")")~.(fignams[i]))) + 
  theme(axis.text = element_text(size=14), plot.title=element_text(vjust= -1)) + 
  scale_y_continuous(expand = expansion(c(0,0)) ) 

plist[[i]] <- ggp
} 

plot_grid(plist[[1]],plist[[2]],plist[[3]],nrow=1)
ggsave('sample_ex4.png',device='png',width = 10, height = pi)


 
 





















 
#-------- Exercise 6 ---------#
#-----------------------------#
# a population of random numbers
Npop <- 1000000
population <- rnorm(Npop)**2


# parameters and initializations
samplesizes   <- seq(5,500,8)
numberOfsamps <- 1000
samplemeans   <- rep(0,numberOfsamps)
fwhms         <- rep(0,length(samplesizes))
peakvals      <- rep(0,length(samplesizes))

# line colors
c <- seq(.9,0,length.out=length(samplesizes))
c <- cbind(c,c,c)+c[length(samplesizes)-1]




## run the experiment!

xx <- seq(.0,1.6,length.out=41)
xx <- (xx[-40]+xx[-1])/2
dframe <- data.frame(x=xx)

for (Ns in 1:length(samplesizes)) { 

  # compute the means of lots of samples
  for (expi in 1:numberOfsamps) { 
    samplemeans[expi] <- mean( sample(population,size=samplesizes[Ns], replace = TRUE) )
  }

  # make a histogram of those means

  #yy <- hist(samplemeans,seq(.0,1.6,length.out=40),plot=F)$counts
  yy <- hist(samplemeans[(samplemeans > 0) & (samplemeans < 1.6)], seq(.0,1.6,length.out=41), plot=F)$counts
  yy <- yy/sum(yy)

  ### compute FWHM
  # step 1: normalize
  yn <- yy/max(yy)

  # step 2: find peak index
  idx <- which.max(yn)

  # step 3: compute FWHM
  fwhms[Ns] <- xx[idx+which.min(abs(yn[(idx+1):40]-.5))] - xx[which.min(abs(yn[1:idx]-.5))]

  # also store mean value
  peakvals[Ns] <- xx[idx]

  # plot
 dframe <- cbind(dframe,yy)

}

dframe <- gather(data.frame(dframe),cc,dframe,-x,factor_key = T) # long data frame for ggplot

ggp1 <- ggplot(dframe) + geom_line(aes(x=x,y=dframe,col=cc), linewidth=0.5,show.legend=F) + 
  ylab('Proportion') + xlab('Sample mean value') + 
  theme(axis.text = element_text(size=14), plot.title=element_text(vjust= -1)) + 
  scale_color_manual(values=rgb(c)) + scale_x_continuous(limit=c(0,1.6),expand = expansion(c(0,0)))

show(ggp1)
ggsave('sample_ex6a.png',device='png',width = 7, height = 4)


 
 
 
 


ggp1 <- ggplot() + geom_point(aes(x=samplesizes,y=fwhms), shape=22, fill='white',size=4, stroke=.8) + 
        labs(x='Sample sizes',y='FWHM', title=expression(paste(bold("A)"), " FWHM by sample size"))) +
        xlim(-10,samplesizes[length(samplesizes)]+10) + 
  theme(axis.text = element_text(size=14), plot.title=element_text(vjust= -1))

ggp2 <- ggplot() + geom_point(aes(x=samplesizes,y=peakvals), shape=22, fill='white',size=4, stroke=.8) + 
        labs(x='Sample sizes',y='Peak values', title=expression(paste(bold("A)"), " Peaks by sample size"))) +
  xlim(-10,samplesizes[length(samplesizes)]+10) + ylim(0.4,1.6) + 
  theme(axis.text = element_text(size=14), plot.title=element_text(vjust= -1))

plot_grid(ggp1,ggp2,nrow=1)

ggsave('sample_ex6b.png',device='png',width = 10, height = 4)


 
 





















 
#-------- Exercise 7 ---------#
#-----------------------------#
# a population of random numbers
Npop <- 1000000
population <- rnorm(Npop)**2


# experiment parameters
samplesizes <- as.integer(logspace( log10(10), log10(Npop/10),25))
numExps <- 50


# theoretical standard error based on population standard deviation
theory <- sd(population) / sqrt(samplesizes)

# initialize the empirical estimates
standerror <- samplemeans <- matrix(0,nrow = numExps,ncol = length(samplesizes))



# Run the experiment!
for (expi in 1:numExps) { 
  for (idx in 1:length(samplesizes)) { 
    ssize <- samplesizes[idx]
    # generate a random sample
    rsample <- sample(population,size=ssize, replace = TRUE)

    # compute its standard error (estimate) and the sample mean
    standerror[expi,idx] <- sd(rsample) / sqrt(ssize)
    samplemeans[expi,idx] <- mean(rsample)
  }
}

## plotting

ggp1 <- ggplot() + aes(x=samplesizes,y=theory) + 
  geom_point(aes(col='Analytical SEM'), shape=22, size=4, stroke=1.0, fill=rgb(.9,.9,.9)) +
  geom_line(color='black',linewidth=1,show.legend=T) + 
  geom_point(aes(y=apply(standerror,2,mean),col='Empirical SEM'), shape=25, size=4, stroke=1.0, fill='black') + 
  geom_line(aes(y=apply(standerror,2,mean)),color=rgb(.3,.3,.3),linewidth=1,show.legend=T) +
  geom_point(aes(x=samplesizes,y=apply(samplemeans,2,sd),col='Sample means STD'), shape=21, size=4,
             stroke=1.0, fill=rgb(.9,.9,.9)) + 
  geom_line(aes(x=samplesizes,y=apply(samplemeans,2,sd)),color=rgb(.7,.7,.7),linewidth=1.,show.legend=T) +
  labs(x='Sample size' , y='Sample means variabilities', 
       title= expression(paste(bold("A)")," Estimates by sample size" )))  +
  xlim(-100,102000)+ scale_linetype_manual(values = c("solid","solid","solid")) +
  scale_color_manual(values=c('black',rgb(.3,.3,.3),rgb(.7,.7,.7) )) + 
  theme(legend.position = c(0.5,0.8),legend.title=element_blank(),legend.spacing.y = unit(0, "mm"),
        legend.margin = margin(c(1,5,2,5)), legend.background = element_blank(),
        legend.box.background = element_rect(colour = "gray"),legend.text=element_text(size=14),
        legend.key.width =unit(1,'cm'),axis.text = element_text(size=14),axis.title=element_text(size=14), 
        plot.title=element_text(vjust= -1)) +
  guides(color= guide_legend(override.aes= list(shape=c(22,25,21), color=c('black',rgb(.3,.3,.3), rgb(.7,.7,.7)),
                                                fill=c(rgb(.7,.7,.7),'black',rgb(.9,.9,.9)), linewidth=c(1,1,1))))

ggp2 <-  ggplot() + geom_point(aes(x=theory, y= apply(standerror,2, mean),
                                   col= 'Empirical SEM'),shape=21,size=6,stroke=0.5, fill=rgb(.8,.8,.8),alpha=0.5) +
  geom_point(aes(x=apply(samplemeans,2,sd), y= apply(standerror,2, mean), col= 'Sample mean std'), 
             shape=22, size=6, stroke = 0.5, fill=rgb(.3,.3,.3),alpha=0.5) + 
  labs(x='Analytical SEM' , y= expression('Emp. SEM'~italic('or')~'means STD'), 
       title= expression(paste(bold("B)")," Consistency of estimates" ))) +
  theme(legend.position = c(0.65,0.16),legend.title=element_blank(), legend.margin = margin(c(1,6,4,6)), 
        legend.background = element_blank(),legend.box.background = element_rect(colour = "gray"), 
        legend.text=element_text(size=14),axis.text = element_text(size=14),axis.title=element_text(size=14),
        plot.title=element_text(vjust= -1)) + 
  scale_color_manual(values=c('black', 'black' )) +
  guides(color=guide_legend(override.aes=list(shape=c(21,22),color=c('black','black'),
                                              fill=c(rgb(.8,.8,.8),rgb(.3,.3,.3)),linewidth=c(0.5,0.5))))
  
plot_grid(ggp1,ggp2,nrow=1)
ggsave('sample_ex7.png',device='png', width=9, height=4)


 

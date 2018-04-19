#####R commands used in the exercises######

##### Exercise 1######

help(mean)          # different methods of using help
?mean
help("mean")
help("median")

apropos("test")     # searches all functions in R for the string 'test'
help("t.test")      # gets further information on the function t.test()

help.search("plot")  # searches for all instances of the string 'plot'
help.search("plot",package="car") #only searches for plot in the package 'car'

help.start()        # starts R's html help system

installed.packages() # lists all installed packages

getwd()             # identifies the current working directory
setwd("file path")  # changes the working directory to a file path of your choice

#####Exercise 2#####

pi*((10/2)^2)        # calculates the area of a 10 cm diamter circle

log(12.43)           # natural log
log10(12.43)         # log to base 10
sqrt(12.43)          # square root
exp(12.43)           # natural antilog

(23*0.21)^(1/3)      # calculates the cube root of 23*0.21

weight <- c(69,62,57,59,59,64,56,66,67,66)
mean(weight)
var(weight)
sd(weight)
range(weight)
length(weight)
weight[1:5]

height<-scan()
summary(height)
height[c(2,3,9,10)]
height[height<=99]

bmi <- weight/(height/100)^2
bmi

seq(0,1,0.1)
rev(seq(1,10,0.5))

rep(1:3, times=3)
rep(1:3, each=3, times=2)
rep(1:5, times= 5:1)
rep(c(7,2,8,1),times= c(4,3,1,5))

height.sorted<- sort(height)
height.descend <- rev(sort(height))

names <- c("Alfred","Barbara", "James","Jane","John","Judy","Louise","Mary","Ronald","William")
height.order <- order(height)
names[height.order]    # Ronald is the tallest

weight.descend <- rev(order(weight))
names[weight.descend]  # Alfred is the heaviest

treatment <- gl(3,10)

save.image("file path and filename.RData")    # edit this command to include your file path and file name
savehistory("file path and filename.Rhistory") # edit this command to include your file path and file name
ls()
rm(treatment)
q()

#####Exercise 3#####

whale<-read.table("D:\\filepath\\whaledata.txt", header=T) # change the filepath to the location of your file
attach(whale)
names(whale)
attributes(whale)

is.factor(whale$water.noise)
whale$water.noise <- factor(whale$water.noise)
is.factor(whale$water.noise)

whale$number.whales
number.whales               # if the dataframe is attached

whale[59,4]                # the number of whales spotted is 15
whale[1:10,]              # extracts the first 10 rows of the dataframe
whale[9:17,1:4]           # extract rows 9 to 17 of the first 4 columns

whale[depth>600,4]
whale[gradient>150,]
whale[water.noise=="1" & month=="May",]

whale[order(whale[,7]),1:8]

subset(whale, water.noise=="1" & month=="October" & gradient > 100)

summary(whale)
tapply(number.whales, water.noise, mean)
tapply(number.whales,list(water.noise,month), mean)

data(iris)
new.iris<- edit(iris)
fix(iris)

children<- data.frame(names,weight,height)

#####Exercise 4#####

squid <-read.table("D:\\Alex\\Aberdeen\\Aberdeen R-Course\\Data files\\squid1.txt", header=T)
attach(squid)
str(squid)    #use either str() or names()
names(squid)
summary(squid)

plot(nid.length)   # plot variable to look for outliers
identify(nid.length)  # use identify to ID the outlier
nid.length[11] <- 43.2   # Change the value of misstyped value
plot(nid.length)

plot(dig.weight)        # plot variable to look for outliers
identify(dig.weight)     # use identify to ID the outlier
dig.weight[50] <- 10.341   # Change the value of misstyped value
plot(dig.weight)

plot(DML~weight)      #simple scatterplot of DML and weight
plot(DML~sqrt(weight))     #  plots the same graph but with square root transformation of weight
rug(DML, side=2)             # adds tick marks for each data point on the y axis
rug(sqrt(weight), side=1)   # adds tick marks for each data point on the x axis


par(mfrow=c(2,2))    # plots 2 rows of graphs with 2 plots in each row
hist(weight, freq=F)    # plots a histogram of the relative proportions
den.weig <- density(weight)  # defines the density curve
lines(den.weig)           # adds a density curve to the histogram
hist(DML, freq=F)
den.dml <- density(DML)
lines(den.dml)
hist(dig.weight, freq=F)
den.dig <- density(dig.weight)
lines(den.dig)
hist(nid.weight, freq=F)
den.nid <- density(nid.weight)
lines(den.nid)
par(mfrow=c(1,1))    # returns the graphics device to normal

boxplot(weight~maturity.stage, notch=T, xlab="Maturity stage", ylab="weight", main="Body weight of squid at various maturity stages")

stripchart(weight~maturity.stage, method="jitter", jitter=0.1, xlab="weight", ylab="maturity stage", pch=1)

pairs(squid[8:13])

panel.cor <- function(x, y, digits=2, prefix="", cex.cor)   # defines the correlation function
{
    usr <- par("usr"); on.exit(par(usr))
    par(usr = c(0, 1, 0, 1))
    r <- abs(cor(x, y))
    txt <- format(c(r, 0.123456789), digits=digits)[1]
    txt <- paste(prefix, txt, sep="")
    if(missing(cex.cor)) cex <- 0.8/strwidth(txt)
    text(0.5, 0.5, txt, cex = cex * r)
}

pairs(squid[8:13], lower.panel=panel.smooth, upper.panel=panel.cor) # Uses the panel.cor function in the upper panel

coplot(ovary.weight~weight|maturity.stage) #oavary.weight and weight conditioned by maturity.stage
coplot(ovary.weight~weight|factor(month)*factor(year)) # month and year are now factors

library(lattice)
xyplot(ovary.weight~weight|month, groups=year,auto.key=T)

plot(1:20, rep(1, l= 20), pch= 1:20, cex= 2)

plot(1:8, 1:8, type= "n", ylab="line types", xlab="")
abline(h= 1:8, lty= 1:8)

plot(DML~ovary.weight, pch=as.numeric(maturity.stage), col=as.numeric(maturity.stage), xlab="ovary weight", ylab="DML")
labs <- c("stage 1", "stage 2", "stage 3", "stage 4","stage 5")
cols <- as.numeric(levels(squid$maturity.stage))
point <- as.numeric(levels(squid$maturity.stage))
legend(38,148, labs,col=cols, pch=point)

#####Exercise 5#####

### question 1
squid2<-read.table("C://squid2.txt", header=T)
attach(squid2)

### question 2
qqnorm(weight)
qqline(weight)

shapiro.test(weight)
# p < 0.5, hence not normal
# use non-parametric test

### question 3
wilcox.test(weight, mu=280)
# p > 0.5, no evidence that average is different from 280

### question 4

new.squid <- squid2 [squid2$year !=1989,]
squid.90 <- squid2 [squid2$year ==1990,]
squid.91 <- squid2 [squid2$year ==1991,]


# I do not attach these to avoid confusion
shapiro.test(squid.90$weight)
shapiro.test(squid.91$weight)

#both rather non-normal, hence non-parametric test
wilcox.test(new.squid$weight~ new.squid$year)

### question 5

plot(nid.length, nid.weight)
# strong non-linearity

### question 6

squid.lm<-lm(nid.weight~nid.length)
summary(squid.lm)
# relationship seems significant, but is the model appropriate?
plot(squid.lm)
# non-linearity shows in residual plot!

### question 7
squid.lm.2<-lm(nid.weight~I(nid.length^2)))
plot
# improved residual plot

squid.lm.3<-lm(nid.weight~nid.length+I(nid.length^2)))
# improved residual plot
# there is still some strange behaviour to be observed;
# apparently a more sophistcated modl is needed

### question 8

squid.lm.4<-lm(DML~ eviscerate.weight+dig.weight+ nid.length+ovary.weight+as.factor(year))
summary(squid.lm.4)
squid.lm.5<-lm(DML~ eviscerate.weight+ nid.length+as.factor(year))
summary(squid.lm.5)

### question 9

pairs(squid2[8:13])
# many relationships are not linear, more sophistacted approach needed

plot(squid.lm.4)
plot(squid.lm.5)
# both show problems with non-linearity in the residuals

# on the basis of what we have learned so far we cannot tell which model
# is better; shall learn how to do this next week!






















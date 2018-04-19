#### INTRODUCTION TO R ####

#### SECTION ONE COMMANDS ####
### INTRODUCTION ###
# help functions
help(plot)
?plot
help("plot")
help.search("plot")
help(glm.diag.plots, package="boot")
help.start()
apropos(help)

#package functions
CRAN.packages()
install.packages("name of package")
install.packages(c("package1", "package2"))
installed.packages()
update.packages()
library(nlme)

# directory functions
getwd()
setwd("path of new directory")

# quitting R
q()

#### SECTION TWO COMMANDS ####
### BASIC COMMANDS ###
# assigning values to variables
b <- 20
c <- 20+20
w <-c(2,3,1,6,4,3,3,7)
a <- scan()
e <-seq(1,5,0.5)
e <-rep(2,10)
f <- rep("abc",3)
g <-rep(1:5,3)
h <-rep(1:5, rep(3,5))

# vector arithmetic functions in R
x <- c(1,2,3,4)
y <- c(5,6,7,8)
x*y
y/x
y-x
x^y

y <- c(4,2,5,6,4,3,5,6,7,4,3)
z <- 1:11
mean(y)
var(y)
sd(y)
range(z)
length(z)
summary(y)

# sorting, ordering and manipulating data
y <- c(4,2,5,6,4,3,5,6,7,4,3)
y[3]
y[c(2,4,6,8,10)]
y[3:9]
y[y>4]
y[y>=2]
y[y!=6]

y <- c(4,2,5,6,4)
y[y>=4] <- 10

y <- c(4,2,5,6,4,3,5,6,7,4,3)
sort(y)
rev(sort(y))

y <- c(4,1,5,6,4,3,5,6,7,4,3)
order(y)

z <-c(3,7,3,1,5,9,10,6,8,2,11)
o <- order(y)
o
z[o]

# the R workspace
x <- c(1,4,7,3,2,9,7,6,7)
y <- 1:9
z <- seq(1:5,0.5)
ls()
rm(x)
ls()

rm(list=ls())
ls()

# saving your work
save.image()
save.image("test1.RData")
savehistory()
loadhistory()

#### SECTION THREE COMMANDS ####
### DATAFRAMES ###

# importing data into R
petunia <-read.table("D:\\Alex\\Aberdeen\\Aberdeen R-Course\\Data files\\flower.txt", header=T) # change the file path to the location fo your file
attach(petunia)
names(petunia)
attributes(petunia)

# selecting entries in the dataframe
petunia$height
attach(petunia)
height
detach(petunia)
petunia[2,4]
petunia[1:10,1:4]
petunia[1:10,]
petunia[height>6 & treat=="tip" & nitrogen=="medium",]
petunia[order(petunia[,4]),1:8]
tipplants <- subset(petunia, treat=="tip" & nitrogen=="medium" & block=="2")
tipplants
summary(petunia)
petunia$block <- factor(petunia$block)
is.factor(petunia$block)
summary(petunia$block)
tapply(height, treat, mean)
tapply(height, treat, mean, na.rm=T)
tapply(height, treat, summary)
tapply(height,list(treat,nitrogen), mean)

#datasets included with R
data()
data(CO2)

# usings R's data editor
edit(petunia)
petunia1 <- edit(petunia)
fix(petunia)
newpetunia <-edit(data.frame())

#### SECTION FOUR COMMANDS ####
### GRAPHICS IN R ###
plot(height)
with(petunia,plot(height))
plot(weight,shootarea)
plot(shootarea ~ weight)
plot(height, type="b")
hist(height)

brk <- seq(0,18,2)
hist(height, breaks=brk)
hist(height, freq=F, breaks=brk)

dens <- density(height)
hist(height, breaks=brk, freq=F)
lines(dens)

boxplot(height)
boxplot(height~treat, notch=T)

boxplot(height~treat, notch=T)
rug(height[treat=="notip"],side=2)
rug(height[treat=="tip"],side=4)

stripchart(height~treat)
stripchart(height~treat, method="jitter", jitter=0.05)

pairs(petunia[4:8])
pairs(petunia[4:8], panel=panel.smooth)

## put (absolute) correlations on the upper panels,
## with size proportional to the correlations.
Panel.cor <- function(x, y, digits=2, prefix="", cex.cor)
{
    usr <- par("usr"); on.exit(par(usr))
    par(usr = c(0, 1, 0, 1))
    r <- abs(cor(x, y))
    txt <- format(c(r, 0.123456789), digits=digits)[1]
    txt <- paste(prefix, txt, sep="")
    if(missing(cex.cor)) cex <- 0.8/strwidth(txt)
    text(0.5, 0.5, txt, cex = cex * r)
}

pairs(petunia[4:8],lower.panel=panel.smooth, upper.panel=panel.cor)

coplot(flowers~weight|shootarea)
coplot(flowers~weight|treat*nitrogen)

library(lattice)
demo(lattice)
xyplot(flowers~shootarea|nitrogen*treat)
xyplot(flowers~shootarea|nitrogen*treat, groups=block, auto.key=T)
symb <- c(1,16)
xyplot(flowers~shootarea|nitrogen*treat, groups=block, col="black",pch=symb, key=list(points=list(pch=symb,col="black"),text=list(c("block 1","block 2"))))

# reformatting basic plots
plot(shootarea ~ weight, main="Relationship between shoot area and weight of petunia plants", xlab="weight (g)", ylab="shoot area (mm2)", xlim=c(0,25),ylim=c(0,200), pch=16,bty="l", cex=1.2)
ylab=expression(paste("shoot", " area"," (",mm^2,")"))

plot(shootarea~weight, type="n", xlab="weight (g)", ylab="shoot area (mm2)")
points(shootarea[nitrogen=="low"]~weight[nitrogen=="low"], pch=1, col="red")
points(shootarea [nitrogen=="medium"] ~ weight [nitrogen =="medium"], pch=2, col="blue")
points(shootarea[nitrogen=="high"]~weight[nitrogen=="high"], pch=3, col="black")

plot(shootarea~weight, xlab="weight (g)", ylab="shoot area (mm2)", pch=as.numeric(nitrogen), col=as.numeric (nitrogen))

locator(1)
labs <- c("low","medium", "high")
cols <- c("red", "blue", "black")
points <-c(1,2,3)
legend(5.6,192.1, labs, pch=points, col=cols)

petunia.lm <- lm(shootarea~weight, data=petunia)
abline(petunia.lm,lty=1)

locator(1)
text(22.5,187.4, "(A)", font=2)

# plotting multiple graphs
par(mfrow=(1,2)
plot(shootarea,weight)
plot(nitrogen,shootarea,xlab="nitrogen",ylab="shootarea")
par(mfrow=c(1,1))

par(bg="lavender")

#### SECTION FIVE BASIC COMMANDS ####
### BASIC STATISTICS ###

# one and two sample tests
data(trees)
attach(trees)
names(trees)
summary(trees)

t.test(Height, mu=70)
t.test(Height, mu=70, alternative="greater")

wilcox.test(Height, mu=70)

qqnorm(Height)
qqline(Height, lty=2)

shapiro.test(Height)

atmos<-read.table("D:\\Aberdeen R-Course\\atmosphere.txt", header=T)  # change the file path to the location of your file
attach(atmos)
names(atmos)
atmos
t.test(moisture~treatment)
t.test(moisture~treatment, var.equal=T)
var.test(moisture~treatment)
wilcox.test(moisture~treatment)

pollution <- read.table("D:\\Aberdeen R-Course\\pollution.txt", header=T)
attach(pollution)
names(pollution)
t.test(down,up, paired=T)
wilcox.test(down,up, paired=T)

buy <- c(45,71)	#  creates a vector of positive outcomes
total <-c((45+35),(71+32))	# creates a vector of total numbers
prop.test(buy,total)

buyers <- matrix(c(45,35,71,32),nrow= 2)
buyers
colnames(buyers) <- c("before", "after")
rownames(buyers) <- c("buy", "notbuy")
buyers
chisq.test(buyers)

# correlation
data(trees)
attach(trees)
names(trees)
cor(Height,Volume)
cor(trees)
cor(trees, use="complete.obs")

cor.test(Height, Volume)
cor.test(Height, Volume, method="spearman")

# simple linear regression
smoke <- read.table("D:\\Aberdeen R-Course\\smoking.txt", header=T)
attach(smoke)
names(smoke)
plot(mortality~smoking)
smoke.lm <- lm(mortality~smoking, data=smoke, na.action=na.exclude)
attributes(smoke.lm)
smoke.lm$coefficients
summary(smoke.lm)
summary.aov(smoke.lm)
abline(smoke.lm)

plot(resid(smoke.lm) ~ fitted(smoke.lm))
abline(y=0)

qqnorm(resid(smoke.lm))
qqline(resid(smoke.lm))

par(mfrow=c(2,2))	# plots 2 graphs in 2 rows
plot(smoke.lm)

smoke[2,]
smoke[25,]
smoke.lm2 <- update(smoke.lm, subset=-2)
summary(smoke.lm2)

par(mfrow=c(2,2))
plot(dffits(smoke.lm), type="l")
plot(rstudent(smoke.lm))
matplot(dfbetas(smoke.lm), type="l", col="black")
lines(sqrt(cooks.distance(smoke.lm)), lwd=2)











































#basic shoot area and weight plot
plot(shootarea ~ weight)

#tarted up graph of shoot area and weight
plot(shootarea ~ weight, main="Relationship between shoot area and weight of petunia plants",xlab="weight (g)", ylab="shoot area (mm2)", xlim=c(0,25),ylim=c(0,200), pch=16,bty="l", cex=1.2)


#shoot area and weight with different colour points
plot(shootarea ~ weight, main=" shoot area and total weight of petunia plants", xlab="weight (g)", ylab="shoot area (mm2)", xlim=c(0,25),ylim=c(0,200), type="n")
points(sw[[1]],ssa[[1]], pch=16, col="blue", cex=1.3)
points(sw[[2]],ssa[[2]], col="red", cex=1.3)

#alternate use of as.numeric() to print different plotting colours and symbols
plot(shootarea~weight, xlab="weight (g)", ylab="shoot area (mm2)", pch=as.numeric(nitrogen), col=as.numeric(nitrogen))

#alternative shootarea and weight with different coloured points with legend and regression line
plot(shootarea~weight, type="n", xlab="weight (g)", ylab="shoot area (mm2)")
points(shootarea[nitrogen=="low"]~weight[nitrogen=="low"], pch=1, col="red")
points(shootarea[nitrogen=="medium"]~weight[nitrogen=="medium"], pch=2, col="blue")
points(shootarea[nitrogen=="high"]~weight[nitrogen=="high"], pch=3, col="black")
labs <- c("low","medium", "high")
cols <- c("red", "blue", "black")
points <-c(1,2,3)
legend(5.6,192.1, labs,pch=points, col=cols)
petunia.lm <- lm(shootarea~weight, data=petunia)
abline(petunia.lm,lty=1)
text(22.5,187.4, "(A)", font=2)

## put (absolute) correlations on the upper panels,
## with size proportional to the correlations.
panel.cor <- function(x, y, digits=2, prefix="", cex.cor)
 {
     usr <- par("usr"); on.exit(par(usr))
     par(usr = c(0, 1, 0, 1))
     r <- abs(cor(x, y))
     txt <- format(c(r, 0.123456789), digits=digits)[1]
     txt <- paste(prefix, txt, sep="")
     if(missing(cex.cor)) cex <- 0.8/strwidth(txt)
     text(0.5, 0.5, txt, cex = cex * r)
 }
 # pairs plot for continuous petunia data
 pairs(petunia[4:8], lower.panel=panel.smooth, upper.panel=panel.cor)

coplot(flowers~weight|height, cex=1.5)
 
coplot(flowers~weight|treat*nitrogen, cex=1.5)

xyplot(flowers~shootarea|nitrogen*treat, groups=block, col=colr, pch=symb, key=list(points=list(pch=symb, col=colr),text=list(c("block 1","block 2"))))

# read flowers into R and call object petunia
petunia <- read.table("D:\\Alex\\Aberdeen\\Aberdeen R-Course\\flower.txt", header=T)
attach(petunia)
names(petunia)

# simple linear regression example
smoke <- read.table("D:\\Alex\\Aberdeen\\Aberdeen R-Course\\smoking.txt", header=T, na.action=na.exclude)
attach(smoke)
names(smoke)
summary(smoke)
plot(mortality~smoking)


#other diagnostic plots
par(mfrow=c(2,2))
plot(dffits(smoke.lm), type="l")
plot(rstudent(smoke.lm))
matplot(dfbetas(smoke.lm), type="l", col="black")
lines(sqrt(cooks.distance(smoke.lm)), lwd=2)


plot(mortality~smoking, type="n", xlab="relative smoking rate", ylab="relative mortality rate")
points(mortality~smoking,  , pch=4,cex=1.2, col="blue")
abline(smoke.lm)
segments(smoking, fitted(smoke.lm), smoking,mortality, lty=2)
 rug(smoking, side=1, ticksize=0.02)
 rug(mortality, side=2, ticksize=0.02)
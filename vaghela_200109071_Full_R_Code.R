library(mvtnorm)
library(MASS)
library(ggplot2)

#1
sample <- rnorm(100)
sample

SD <- sd(sample,FALSE)
SD

MEAN <- mean(sample)
MEAN

summary(sample)

VAR <- var(sample)
VAR

QUA <- quantile(sample)
QUA

#2
hist(sample, breaks = "Scott", xlab = "Sample_Hist", main = "Histogram")
hist(sample, breaks = "Sturges", xlab = "Sample_Hist", main = "Histogram")
hist(sample, breaks=25, xlab = "Sample_Hist", main = "Histogram")

#3
qqnorm(sample)
qqline(sample)

#4
sample2 <- rmvnorm(100,mean = c(70,170),matrix(c(27,39,39,100),2,2))
sample2

summary(sample2)

SD2 <- apply(sample2,2,sd)
SD2

VAR2 <- apply(sample2, 2, var)
VAR2

QUA2 <- apply(sample2,2,quantile)
QUA2

barplot(sample2[,1], xlab = "Height")
barplot(sample2[,2], xlab = "Weight")

boxplot(sample2[,1], xlab = "Height")
boxplot(sample2[,2], xlab = "Weight")

plot(sample2, xlab = "Height", ylab = "Weight")

#5

qqnorm(sample2[,1], main = "Normal_Distribution_Height")
qqline(sample2[,1])

qqnorm(sample2[,2], main = "Normal_Distribution_Weight")
qqline(sample2[,2])

#6

BIVAR <- kde2d(sample2[,1], sample2[,2], n = 100)

persp(BIVAR, phi = 5, theta = 10, border = "black", col="white",ticktype = "detailed", expand = 0.4, xlab = "X", ylab = "Y", zlab = "Z", main = "Bivariate_Normal_Distribution")


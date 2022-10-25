setwd("C:/Users/tolun/Desktop/Staj/Days/16_09_cuma/")
getwd()

# Libraries
library(foreign)
library(ggplot2)
library(GGally)
library(survival)
library(rgl)

# Get Data 
data <- read.csv("intReg.csv")
data <- data[-1]
data
summary(data)

# Visualization (Bivariate Plot)
ggpairs(data, lower = list(combo = "box"), upper = list(combo = "blank"))

# Lower and Upper bounds that corresponding to each Gelir obs.
by(data[, 2:3], data$Gelir, colMeans, na.rm = TRUE)

# Model
# m
data$const <- 1
Y <- with(data, Surv(HarcAlt, HarcUst,   
                     event = rep(1, nrow(data)), type = "interval"))

m <- survreg(Y ~ Gelir + const, data = data, dist = "gaussian")

summary(m)
coefficients(m)
# m1
implicitY <- data.frame((data$HarcAlt + data$HarcUst)/2, check.names = TRUE)
colnames(implicitY) = c("impicitY")

x <- data.frame(data$Gelir)

left <- data.frame(data$HarcAlt)
right <- data.frame(data$HarcUst)

dat <- cbind(implicitY, left, right, x)
colnames(dat) = c("implicitHarc", "l", "r", "gelir")

dat$const <- 1

m1 <- lm(implicitHarc ~ gelir + const, data = dat)
summary(m1)

# m2 
implicitY2 <- log(implicitY)
x2 <- log(x)
dat2 <- cbind(implicitY2, x2)
colnames(dat2) <- c("y", "x")

m2 <- lm(y ~ x, data = dat2) #log-log
summary(m2)

# m3
dat3 <- cbind(implicitY2, x)
colnames(dat3) = c("y", "x")

m3 <- lm(y ~ x, data = dat3) #log-lin
summary(m3)

# m4
library(gam)
m4 <- gam(implicitHarc ~ gelir, data = dat, family = "gaussian")
summary(m4)

m5 <- glm(implicitHarc ~ gelir, data = dat, family = "gaussian")
summary(m5)


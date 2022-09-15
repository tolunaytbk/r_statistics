
# Libraries
library(readr)

library(foreign)
library(MASS)

library(nnet)
library(stargazer)

library(erer)


# Get Data
data <- read.csv("data/GelirSira.csv")
data <- data[-1]

# Creating Model

ord_logit <- polr(as.factor(GelirKategori) ~ Cinsiyet +
                    OkulTipi  + 
                    Edebiyat  + 
                    Matematik + 
                    FenBil    +
                    Tarih,
                  data = data, 
                  Hess = TRUE,
                  method = c("logistic"))

summary(ord_logit)

# Coefficients and p-values
coeff_ord_logit <- data.frame(coef(summary(ord_logit)))
coeff_ord_logit$pval = round((pnorm(abs(coeff_ord_logit$t.value), 
                                    lower.tail = FALSE) * 2),2)

coeff_ord_logit
stargazer(ord_logit, type = "text")

# Odds Ratios
ord_logit_or <- exp(coef(ord_logit))
ord_logit_or

stargazer(ord_logit, type="text", coef=list(ord_logit_or), p.auto=FALSE)

# Percent Odds Ratios

ord_logit_or_percent <- (exp(coef(ord_logit))-1)*100
ord_logit_or_percent

stargazer(ord_logit, type="text", coef=list(ord_logit_or_percent), 
          p.auto=FALSE)

# Marginal Effects

library(erer)

me_ord_logit <- ocME(ord_logit)
me_ord_logit
me_ord_logit$out
me_ord_logit$out$ME.1
# Predicted Probabilities

ord_logit_pred <- predict(ord_logit, type = "probs")
summary(ord_logit_pred)

mean_pp <- data.frame(Low = c(mean(ord_logit_pred[,1])),
                      Medium = c(mean(ord_logit_pred[,2])),
                      High = c(mean(ord_logit_pred[,3])))
mean_pp #Predicted Probs. when all predictors are at their mean value

# Probabilities of Custom Prediction 

new_data <- data.frame(
  Edebiyat  = rep(mean(data$Edebiyat, 2)),
  Matematik = rep(mean(data$Matematik, 2)),
  FenBil    = rep(mean(data$FenBil, 2)),
  Tarih     = rep(mean(data$Tarih, 2)),
  Cinsiyet  = c(1),
  OkulTipi  = c(1)
)  #Edebiyat, Matematik, and FenBÝl variables are at their means.
   #Value of Cinsiyet and OkulTipi variables is 1.

new_data[, c("pred.prob")] <- predict(ord_logit, 
                                      newdata=new_data, 
                                      type="class")
new_data #Model predicts GelirKategori var. as 2 when independent variables have
##########custom values








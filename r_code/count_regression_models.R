
# Libraries
library(pscl)
library(AER)
library(MASS)

# Get Data
data <- read.csv("data/odul.csv")
data <- data[-1]

# Model Decision 
## if var(dependent variable) == mean(dependent variable) => Poisson
## if var(dependent variable) > mean(dependent variable) => Negative Binomial

var(data$OdulSayisi) > mean(data$OdulSayisi)

# Poisson Regression
poisson <- glm(
  OdulSayisi ~ Dil + Sayisal + Matematik + Kiz, 
  data = data,
  family = "poisson"
)

summary(poisson)
dispersiontest(poisson)             #for dispersion value
dispersiontest(poisson, trafo = 2)  #for alpha value

# Negative Binomial Regression
negBin <- glm.nb(
  OdulSayisi ~ Dil + Sayisal + Matematik + Kiz, 
  data = data,
  link = log
)

summary(negBin)

############################################################
# Hurdle or Truncated Models
h_poisson <- hurdle(
  OdulSayisi ~ Dil + Sayisal + Matematik + Kiz|Dil + Sayisal + Matematik + Kiz,
  data = data,
  link = "logit",
  dist = "poisson"
)
summary(h_poisson)

h_negBin <- hurdle(
  OdulSayisi ~ Dil + Sayisal + Matematik + Kiz|Dil + Sayisal + Matematik + Kiz,
  data = data,
  link = "logit",
  dist = "negbin"
)
summary(h_negBin)

# Zero-inflated Models
z_poisson <- zeroinfl(
  OdulSayisi ~ Dil + Sayisal + Matematik + Kiz|Dil + Sayisal + Matematik + Kiz,
  data = data,
  link = "logit",
  dist = "poisson"
)
summary(z_poisson)

z_negBin <- zeroinfl(
  OdulSayisi ~ Dil + Sayisal + Matematik + Kiz|Dil + Sayisal + Matematik + Kiz,
  data = data,
  link = "logit",
  dist = "negbin"
)
summary(z_negBin)

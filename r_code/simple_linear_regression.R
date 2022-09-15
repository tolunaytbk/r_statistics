
#install.packages("tidyverse")
library(tidyverse)
library(readr)
library(readxl)

mroz <- read_xlsx("data/mroz.xlsx")

data()
head(mroz, 10)


mroz %>%
  lm(KadinCalismaSuresi ~ KadinYasi, data = .) %>%
  summary()


model <- lm(KadinCalismaSuresi ~ KadinYasi, data = mroz)
model
summary(model)

attributes(model)
model$residuals
hist(model$residuals)


new_kadin_yasi <- data.frame(KadinYasi = c(22, 33, 44))
predict(model, new_kadin_yasi) %>% round(1)

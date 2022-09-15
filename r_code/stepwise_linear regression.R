
# Libraries
library(olsrr)
library(readxl)

# Data 
mroz <- read_xlsx("data/mroz.xlsx")

# Model
model <- lm(KadinCalismaSuresi ~ cocsay6yaskucuk + cocsay618yas + 
             KadinYasi + KadinEgitim + KadinSaatUcreti + KocaCalSuresi + 
             KocaYasi + KocaEgitimi + KocaMaasi + AileGeliri + KadinVergiOrani + 
             KadinAnneEgt + KadinBabaEgt + KentteIssizlikOrani + 
             BuyukKentteMiYasiyor + OncekiIsDeneyimi, data = mroz)

# All Possible Regressions (slow)

all_possible_regressions = ols_step_all_possible(model)
?ols_step_all_possible

# Best Subset (slow)
ols_step_best_subset(model)

?ols_step_best_subset

## Forward Stepwise Regression (Selected Variables)
ols_step_forward_p(model, penter = 0.05, progress = TRUE)

?ols_step_forward

## Backward Stepwise Regression (Removed Variables)
ols_step_backward_p(model, prem = 0.05)

?ols_step_backward

## Both-Side Stepwise Regression (fast way of best subset selection)
best_subset <- ols_step_both_p(model, pent = 0.05, prem = 0.05, progress = TRUE, 
                               details = FALSE)
best_subset

plot(best_subset)

?ols_step_both_p

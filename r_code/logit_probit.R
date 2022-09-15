setwd("C:/Users/tolun/Desktop/Staj/Days/06_09_sali/r_code/")
getwd()

# Packages
library(readxl)
library(foreign)
library(nnet)
library(stargazer)
library(mfx)
library(margins)


# Get Data
multinomial_data <- read_xlsx("brand_choice.xlsx")
binomial_data <- read_xlsx("organic_diet.xlsx")

# Binomial Logit/Probit with library(foreign)
## Logit

logit <- glm(organic_diet ~ income + education + age + healthy_diet, 
             family=binomial(link="logit"), 
             data=binomial_data)

logit

library(stargazer) # for visualization
stargazer(logit, type= "text")

### Odds Ratio

library(mfx)
odds_ratio <- logitor(organic_diet ~ income + education + age + healthy_diet, 
                      data=binomial_data)
odds_ratio
## OR ##
exp(logit$coefficients[-1])

### Marginal Effects

library(mfx)
m_effect <- logitmfx(organic_diet ~ income + education + age + healthy_diet, 
                            data=binomial_data)
m_effect

library(margins)
m_effect2 <- margins(logit)
m_effect2

## Probit

probit <- glm(organic_diet ~ income + education + age + healthy_diet, 
              family=binomial(link="probit"), 
              data=binomial_data)

probit

library(stargazer) # for visualization
stargazer(probit, type= "text")

### Odds Ratio

p_odds_ratio <- exp(probit$coefficients[-1])
p_odds_ratio

### Marginal Effects

library(mfx) 
p_m_effect <- probitmfx(organic_diet ~ income + education + age + healthy_diet, 
                     data=binomial_data)
p_m_effect # Default: Marginal Effect at Mean (MEM)/ atmean = FALSE for change

library(margins) 
p_m_effect2 <- margins(probit)
p_m_effect2 # Default: Average Marginal Effect (AME)
summary(p_m_effect2)

# Mutinomial Logit with library(foreign)

multi_logit <- multinom(brand ~ female + age, data = multinomial_data)
multi_logit

stargazer(multi_logit, type= "text")

### Relative Risk Ratios (Odds Ratios)

rrr <- exp(coef(multi_logit)) 
rrr

stargazer(multi_logit, type="text", coef=list(rrr),  p.auto=FALSE)

### Marginal Effects 

me <- marginal_effects(multi_logit)
AME <- me
AME$female <- mean(me$dydx_female)
AME$age <- mean(me$dydx_age)
AME <- AME[1, 3:4]
AME # Average Marginal Effect of Brand1?

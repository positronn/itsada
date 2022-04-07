# regression.R
library(magrittr)
library(readr)
library(dplyr)
library(ggplot2)

pizza <- read_csv('datasets/pizza_delivery.csv')

X1 <- pizza$branch
X2 <- pizza$bill
X3 <- pizza$temperature
X4 <- pizza$driver
X5 <- pizza$operator

# time  ~ branch + bill + temperature + driver
summary(
    lm(
        pizza$time ~ I(X1) + I(X2) + I(X3) + I(X4)
        )
)

# time  ~ branch + bill + temperature + operator
summary(
    lm(
        pizza$time ~ I(X1) + I(X2) + I(X3) + I(X5)
    )
)

# time ~ bill
summary(
    lm(
        pizza$time ~ I(X2)
    )
)

model_branch <- lm(pizza$time ~ X1)
summary(model_branch)
anova(model_branch)

pizza %>% 
    ggplot(aes(x=time)) +
    geom_histogram(aes(fill=driver),
                   position = 'identity',
                   alpha=0.7,
                   binwidth = 2.5,
                   color='black') +
    facet_wrap(. ~ driver) +
    theme(legend.position = 'none')


model <- lm(time ~ branch + operator + driver, data=pizza)
summary(model)
anova(model)


interaction_model_1 <- lm(temperature ~ time * branch, data=pizza)
interaction_model_2 <- lm(temperature ~ time + branch + time:branch, data=pizza)
interaction_model_3 <- lm(temperature ~ time:branch, data=pizza)

summary(interaction_model_1)
summary(interaction_model_2)
summary(interaction_model_3)


model <- lm(time ~ time * branch, data=pizza)
vcov(model)

# ------------------------------------------------------------------
library(olsrr)
ols_regress(mpg ~ disp + hp + wt + qsec, data = mtcars)
ols_regress(mpg ~ wt, data=mtcars)
model <- lm(mpg ~ disp + hp + wt + qsec, data = mtcars)
model <- lm(mpg ~ cyl + hp + wt + carb, data = mtcars)
summary(model)
ols_vif_tol(model)
ols_eigen_cindex(model)


# ------------------------------------------------------------------
# backward lm selection

library(MASS)
ms <- lm(time ~ branch + bill + operator + driver + temperature + pizzas, data=pizza)
stepAIC(ms, direction='back')

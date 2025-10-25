library(knitr)
library(remotes)
library(alr3)

setwd("~/AMS 315 Project 1")
PartB <- read.csv('Part B/422817_partB.csv', header = TRUE)
plot(PartB$y ~ PartB$x, main='Scatter : y ~ x', xlab='x', ylab='y', pch=20)
regression_model <- lm(y ~ x, data=PartB)
summary(regression_model)
confint(regression_model, level = 0.99)


kable(anova(regression_model), caption='ANOVA Table')
plot(fitted(regression_model), resid(regression_model))
abline(0, 0, col='red')

PartB_trans <- data.frame(x=PartB$x, log_y=log(PartB$y))
plot(PartB_trans$log_y ~ PartB_trans$x, main='Scatter : y ~ x', xlab='x', ylab='y', pch=20)
new_regression_model <- lm(log_y ~ x, data=PartB_trans)

groups <- cut(PartB_trans$x,breaks=c(-Inf,seq(min(PartB_trans$x)+0.03, max(PartB_trans$x)-0.03,by=0.03),Inf))
table(groups)

x <- ave(PartB_trans$x, groups)
data_bin <- data.frame(x=x, y=PartB_trans$log_y)
plot(data_bin$y ~ data_bin$x, main='Scatter : y ~ x', xlab='x', ylab='y', pch=20)
fit_b <- lm(y ~ x, data = data_bin)
plot(fitted(fit_b), resid(fit_b))
pureErrorAnova(fit_b)
summary(fit_b)
confint(fit_b, level = 0.99)


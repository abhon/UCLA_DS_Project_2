# Project 2 Part A Alec Hon -----------------------------------------------
# UCLA Data Science Intensive

corp_taxes <- data.frame(read.csv("P02_Corporate tax.csv"))
attach(corp_taxes)
lm_1 <- lm(ypcg~ctax) #first regression
summary(lm_1)

lm_2 <- lm(ypcg~ctax + ypc2000) #second regression
summary(lm_2)


lm_3 <- lm(ypcg~ctax + ypc2000 + dty + ctax*dty) #third regression
summary(lm_3)

plot(corp_taxes$ctax, corp_taxes$ypcg, xlab = 'Average Corporate Tax Rate \'00-\'08', ylab = 'Avereage GDP per capita Growth \'00-\'15', col = 'blue', pch = 20)
abline(lm_1, col = 'red')


lm_4 <- lm(ypcg~ctax + ypc2000 + ihc + dty + ctax*dty)
summary(lm_4)
#In trying different variables with the regression model, the y2000 variable has very little significance, so it was not included in the final model.
#while ihc was not always statistically significant, when added with the original model is t-value was still significant enough that it can be considered in the final model.
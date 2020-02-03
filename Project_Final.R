#Read data
epl <- read.csv(file="epldata_new.csv")

# Factor all categorical variables
clubid <- factor(epl$club_id)
bigclub <- factor(epl$big_club)
newsign <- factor(epl$new_signing)
newforeign <- factor(epl$new_foreign)
agecat <- factor(epl$age_cat)
region <- factor(epl$region)
positioncat <- factor(epl$position_cat)

# Check correlation between fpl value and market value
model1 <- lm(epl$fpl_value ~ epl$market_value)
summary(model1)

# Check correlation between fpl value and fpl points
model2 <- lm(epl$fpl_value ~ epl$fpl_points)
summary(model1)

# Check correlation between fpl value and fpl selection percentage
model3 <- lm(epl$fpl_value ~ epl$fpl_sel)
summary(model3)

# Check correlation between fpl value and page views
model4 <- lm(epl$fpl_value ~ epl$page_views)
summary(model4)

# Check correlation between fpl value and all interval level predictors
model5 <- lm(epl$fpl_value ~ epl$market_value + epl$fpl_points + epl$fpl_sel + epl$page_views)
summary(model5)

# Remove FPL selection since the P-value is high
model6 <- lm(epl$fpl_value ~ epl$market_value + epl$fpl_points)
summary(model6)

# Check for assumptions:
# Standardized residual plot:
model6.stres <- rstandard(model6)
plot(model6$fitted.values, model6.stres, pch = 16, main = "Standardized Residual Plot", xlab = "Fitted Prices", ylab = "Standardized Residuals")
abline(0,0, lty=2, col="red")
h <- hist(model6.stres)
x <- model6.stres
xfit <- seq(min(x), max(x), length = 50)
yfit <- dnorm(xfit, mean = mean(x), sd = sd(x)) 
yfit <- yfit*diff(h$mids[1:2])*length(x) 
lines(xfit, yfit, col="blue") 
qqnorm(model6.stres, main = "Normal Probability Plot", xlab = "Normal Prices ", ylab = "Standardized Residuals")
qqline(model6.stres, col = "red")
shapiro.test (model6.stres)

# Check for colinearity before interpreting the interval level variables
cor(epl[,7:8])

# Remove page views since there is a correlation
model6 <- lm(epl$fpl_value ~ epl$market_value + epl$fpl_points)
summary(model6)

# Now lets check the nominal level variables with anova for linear relationships

fit1 <- aov(epl$fpl_value ~ clubid)
summary(fit1)
TukeyHSD(fit1, conf.level = .90)


fit2 <- aov(epl$fpl_value ~ bigclub)
summary(fit2)
TukeyHSD(fit2, conf.level = .90)

fit3 <- aov(epl$fpl_value ~ newsign)
summary(fit3)
TukeyHSD(fit3, conf.level = .90)

fit4 <- aov(epl$fpl_value ~ newforeign)
summary(fit4)
TukeyHSD(fit4, conf.level = .90)

fit5 <- aov(epl$fpl_value ~ agecat)
summary(fit5)
TukeyHSD(fit5, conf.level = .90) 

fit6 <- aov(epl$fpl_value ~ region)
summary(fit6)
TukeyHSD(fit6, conf.level = .90) 

fit7 <- aov(epl$fpl_value ~ positioncat)
summary(fit7)
TukeyHSD(fit7, conf.level = .90)

# R^2 
(anova(fit1)[["Sum Sq"]][1])/((anova(fit1)[["Sum Sq"]][1])+(anova(fit1)[["Sum Sq"]][2]))
# sqrt(MSE) 
sqrt(anova(fit1)[["Sum Sq"]][2]/fit1$df.residual) 

# R^2 
(anova(fit2)[["Sum Sq"]][1])/((anova(fit2)[["Sum Sq"]][1])+(anova(fit2)[["Sum Sq"]][2]))
# sqrt(MSE) 
sqrt(anova(fit2)[["Sum Sq"]][2]/fit2$df.residual) 

# R^2 
(anova(fit3)[["Sum Sq"]][1])/((anova(fit3)[["Sum Sq"]][1])+(anova(fit3)[["Sum Sq"]][2]))
# sqrt(MSE) 
sqrt(anova(fit3)[["Sum Sq"]][2]/fit3$df.residual) 

# R^2 
(anova(fit4)[["Sum Sq"]][1])/((anova(fit4)[["Sum Sq"]][1])+(anova(fit4)[["Sum Sq"]][2]))
# sqrt(MSE) 
sqrt(anova(fit4)[["Sum Sq"]][2]/fit4$df.residual) 

# R^2 
(anova(fit5)[["Sum Sq"]][1])/((anova(fit5)[["Sum Sq"]][1])+(anova(fit5)[["Sum Sq"]][2]))
# sqrt(MSE) 
sqrt(anova(fit5)[["Sum Sq"]][2]/fit5$df.residual) 

# R^2 
(anova(fit6)[["Sum Sq"]][1])/((anova(fit6)[["Sum Sq"]][1])+(anova(fit6)[["Sum Sq"]][2]))
# sqrt(MSE) 
sqrt(anova(fit6)[["Sum Sq"]][2]/fit6$df.residual) 



# Check the model with only nominal variables now

# Check relationship with clubId
model7 <- lm(epl$fpl_value ~ epl$market_value + epl$fpl_points + clubid)
summary(model7)
epl1 <- within(epl, clubid <- relevel(clubid, ref = 15))
model8 <- lm(epl1$fpl_value ~ epl1$market_value + epl1$fpl_points + epl1$clubid)
summary(model8)

# Check relationship with bigclub
model9 <- lm(epl$fpl_value ~ epl$market_value + epl$fpl_points + bigclub)
summary(model9)
epl2 <- within(epl, bigclub <- relevel(bigclub, ref = 2))
model10 <- lm(epl2$fpl_value ~ epl2$market_value + epl2$fpl_points + epl2$bigclub)
summary(model10)

# Check relationship of fpl value with whether a player has been newly signed
model11 <- lm(epl$fpl_value ~ epl$market_value + epl$fpl_points + newsign)
summary(model11)
epl3 <- within(epl, newsign <- relevel(newsign, ref = 2))
model12 <- lm(epl3$fpl_value ~ epl3$market_value + epl3$fpl_points + epl3$newsign)
summary(model12)

# Check relationship of FPL value with whether a player is a foreign sign

model13 <- lm(epl$fpl_value ~ epl$market_value + epl$fpl_points + newforeign)
summary(model13)

# Check relationship of FPL value with age of the player

model14 <- lm(epl$fpl_value ~ epl$market_value + epl$fpl_points + agecat)
summary(model14)
epl4 <- within(epl, agecat <- relevel(agecat, ref = 6))
model15 <- lm(epl4$fpl_value ~ epl4$market_value + epl4$fpl_points + epl4$agecat)
summary(model15)

# Check relationship of FPL value with region of the player
model16 <- lm(epl$fpl_value ~ epl$market_value + epl$fpl_points + region)
summary(model16)
#epl5 <- within(epl, region <- relevel(region, ref = 2))
#model17 <- lm(epl5$fpl_value ~ epl4$market_value + epl4$fpl_points + epl4$page_views + epl4$agecat)
#summary(model17)

# Check relationship of FPL value with position category
model17 <- lm(epl$fpl_value ~ epl$market_value + epl$fpl_points + positioncat)
summary(model17)

#Final model
model18 <- lm(epl$fpl_value ~ epl$market_value + epl$fpl_points + positioncat + agecat + newforeign + epl2$bigclub + epl$region + epl$new_signing)
summary(model18)

#Final model iteration

#Final model
model19 <- lm(epl$fpl_value ~ epl$market_value + epl$fpl_points + positioncat + newforeign + epl4$agecat)
summary(model19)

model20 <- lm(epl$fpl_value ~ epl$market_value + epl$fpl_points + positioncat + newforeign + agecat + (epl$market_value + epl$fpl_points)  * agecat)
summary(model20)

model21 <- lm(epl$fpl_value ~ epl$market_value + epl$fpl_points + positioncat + newforeign + agecat + (epl$market_value  * agecat))
summary(model21)

model22 <- lm(epl$fpl_value ~ epl$market_value + epl$fpl_points + positioncat + newforeign + agecat + (epl$market_value * agecat) + (epl$market_value + epl$fpl_points)  * positioncat +bigclub + newforeign)
summary(model22)

model23 <- lm(epl$fpl_value ~ epl$market_value + epl$fpl_points + positioncat + newforeign + agecat + (epl$market_value * agecat) + (epl$market_value + epl$fpl_points)  * positioncat +(epl$market_value * bigclub) + newforeign)
summary(model23)

model24 <- lm(epl$fpl_value ~ epl$market_value + epl$fpl_points + positioncat + newforeign + agecat + (epl$market_value * agecat) + (epl$market_value + epl$fpl_points)  * positioncat +(epl$market_value * bigclub) + (epl$market_value * newforeign))
summary(model24)

model25 <- lm(epl$fpl_value ~ epl$market_value + epl$fpl_points + (epl$market_value + epl$fpl_points) * agecat + (epl$market_value+ epl$fpl_points)  * positioncat +(epl$market_value+ epl$fpl_points) * epl2$bigclub + (epl$market_value+ epl$fpl_points) * newforeign)
summary(model25)

#model26 <- lm(epl$fpl_value ~ epl$market_value + (epl$market_value ) * agecat + (epl$market_value)  * positioncat +(epl$market_value) * epl2$bigclub + (epl$market_value) * newforeign)
#summary(model26)

cor(epl[,6:12])

model25.stres <- rstandard(model25)
plot(model25$fitted.values, model25.stres, pch = 16, main = "Standardized Residual Plot", xlab = "Fitted Prices", ylab = "Standardized Residuals")
abline(0,0, lty=2, col="red")
h <- hist(model25.stres)
x <- model25.stres
xfit <- seq(min(x), max(x), length = 50)
yfit <- dnorm(xfit, mean = mean(x), sd = sd(x)) 
yfit <- yfit*diff(h$mids[1:2])*length(x) 
lines(xfit, yfit, col="blue") 
qqnorm(model25.stres, main = "Normal Probability Plot", xlab = "Normal Prices ", ylab = "Standardized Residuals")
qqline(model25.stres, col = "red")
shapiro.test (model25.stres)

summary(epl)

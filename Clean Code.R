#==============================================================================
#EXPLORATION OF DATASET
#==============================================================================
# install.packages("PPforest")
library(PPforest)
data(fishcatch)
head(fishcatch)

?fishcatch
summary(fishcatch)
str(fishcatch)
dim(fishcatch)
colnames(fishcatch)

any(is.na(fishcatch))
fishcatch
fishcatch[c(47),]
#NB: row 47 has weight of 0.0 grams - could be a mistake in recording

#==============================================================================
#PAIRS PLOT AND CORRELATION COEFFICIENTS
#==============================================================================
# Correlation panel
panel.cor <- function(x, y){
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r <- round(cor(x, y), digits=2)
  txt <- paste0("r = ", r)
  cex.cor <- 0.8/strwidth(txt)
  text(0.5, 0.5, txt, cex = cex.cor)
}
# Customize upper panel
upper.panel<-function(x, y){
  points(x,y, pch = 1, col="cornflowerblue");
  abline(lm(y~x),lwd=1,col="red")
}
# Create the plots
pairs(fishcatch, lower.panel = panel.cor, upper.panel = upper.panel)

#==============================================================================
#SCATTER PLOTS AND SIMPLE LINEAR REGRESSIONS
#==============================================================================
#Take weight as response variable
#Create a scatterplot.  Is there a relationship between the two variables?
#Plot least squares regression line using the `abline()` function.
#Use residual plots to check the assumptions of the model.
#NB: Colouring some of the plots with fishcatch$Type to demonstrate classification differences

#SLR of weight on Type:
reg_Type <- lm(weight~Type, data=fishcatch)
par(mfrow=c(1,1))
plot(Type, weight, pch=1, aes(fishcatch$Type))
par(mfrow=c(2,2))
plot(reg_Type,   pch=1, col=fishcatch$Type)

#SLR of weight on length1:
reg_length1 <- lm(weight~length1, data=fishcatch)
par(mfrow=c(1,1))
plot(length1, weight, pch=1, col="cornflowerblue")
abline(reg_length1,lwd=1,col="red")
par(mfrow=c(2,2))
plot(reg_length1,   pch=1, col="cornflowerblue")

#SLR of weight on length2:
reg_length2 <- lm(weight~length2, data=fishcatch)
par(mfrow=c(1,1))
plot(length2, weight, pch=1, col="cornflowerblue")
abline(reg_length2,lwd=1,col="red")
par(mfrow=c(2,2))
plot(reg_length2,   pch=1, col="cornflowerblue")

#SLR of weight on length3:
reg_length3 <- lm(weight~length3, data=fishcatch)
par(mfrow=c(1,1))
plot(length3, weight, pch=1, col=fishcatch$Type)
abline(reg_length3,lwd=1,col="red")
par(mfrow=c(2,2))
plot(reg_length3, pch=1, col=fishcatch$Type)

#SLR of weight on height:
reg_height <- lm(weight~height, data=fishcatch)
par(mfrow=c(1,1))
plot(height, weight, pch=16, col=fishcatch$Type)
abline(reg_height,lwd=1,col="red")
par(mfrow=c(2,2))
plot(reg_height,   pch=1, col=fishcatch$Type)

#SLR of weight on width:
reg_width <- lm(weight~width, data=fishcatch)
par(mfrow=c(1,1))
plot(width, weight, pch=16, col=fishcatch$Type)
abline(reg_width,lwd=1,col="red")
par(mfrow=c(2,2))
plot(reg_width,   pch=1, col=fishcatch$Type)

#comparing height and width:
par(mfrow=c(1,2))
plot(height, weight, pch=16, col=fishcatch$Type)
abline(reg_height,lwd=1,col="red")
plot(width, weight, pch=16, col=fishcatch$Type)
abline(reg_width,lwd=1,col="red")

#==============================================================================
#TESTING POLYNOMIAL FIT OF PREDICTOR REGRESSIONS
#==============================================================================
#NB: Type predictor is qualitative

#polynomials for length1:
par(mfrow=c(2,2))
poly_length1_1 = lm(weight~poly(length1,1))
summary(poly_length1_1)
plot(poly_length1_1)
poly_length1_2 = lm(weight~poly(length1,2))
summary(poly_length1_2)
plot(poly_length1_2)
poly_length1_3 = lm(weight~poly(length1,3))
summary(poly_length1_3)
plot(poly_length1_3)
#poly_length1_3 seems to have the best fit

#polynomials for length2:
par(mfrow=c(2,2))
poly_length2_1 = lm(weight~poly(length2,1))
summary(poly_length2_1)
plot(poly_length2_1)
poly_length2_2 = lm(weight~poly(length2,2))
summary(poly_length2_2)
plot(poly_length2_2)
poly_length2_3 = lm(weight~poly(length2,3))
summary(poly_length2_3)
plot(poly_length2_3)
#poly_length2_3 seems to have the best fit

#polynomials for length3:
par(mfrow=c(2,2))
poly_length3_1 = lm(weight~poly(length3,1))
summary(poly_length3_1)
plot(poly_length3_1)
poly_length3_2 = lm(weight~poly(length3,2))
summary(poly_length3_2)
plot(poly_length3_2)
poly_length3_3 = lm(weight~poly(length3,3))
summary(poly_length3_3)
plot(poly_length3_3)
#poly_length3_3 seems to have the best fit

#polynomials for height:
par(mfrow=c(2,2))
poly_height_1 = lm(weight~poly(height,1))
summary(poly_height_1)
plot(poly_height_1)
poly_height_2 = lm(weight~poly(height,2))
summary(poly_height_2)
plot(poly_height_2)
poly_height_3 = lm(weight~poly(height,3))
summary(poly_height_3)
plot(poly_height_3)
poly_height_4 = lm(weight~poly(height,4))
summary(poly_height_4)
plot(poly_height_4)
#poly_height_3 has the best residual plots - most randomly scattered

#polynomials for width:
par(mfrow=c(2,2))
poly_width_1 = lm(weight~poly(width,1))
summary(poly_width_1)
plot(poly_width_1)
poly_width_2 = lm(weight~poly(width,2))
summary(poly_width_2)
plot(poly_width_2)
poly_width_3 = lm(weight~poly(width,3))
summary(poly_width_3)
plot(poly_width_3)
poly_width_4 = lm(weight~poly(width,4))
summary(poly_width_4)
plot(poly_width_4)
#poly_width_1 residuals most randomly scattered

#==============================================================================
#EXPLORING LOGARITHMIC AND SQUARE ROOT RESIDUAL FITS
#==============================================================================
log_length3 = lm(weight~log(length3), data = fishcatch)
plot(log_length3)

sqrt_length3 = lm(weight^0.5~length3, data = fishcatch)
plot(sqrt_length3)

polysqrt_length3 = lm(weight^0.5~poly(length3,3), data = fishcatch)
plot(polysqrt_length3)

#==============================================================================
#CALCULATING CONFIDENCE AND PREDICTION INTERVALS FOR PREDICTOR VARIABLES
#==============================================================================
#Obtain a 95\% confidence interval for the coefficient estimates
#Use `confint()` command.

#Simple Linear Regression Models:
confint(reg_length1, level = 0.95)
confint(reg_length2, level = 0.95)
confint(reg_length3, level = 0.95)
confint(reg_height, level = 0.95)
confint(reg_width, level = 0.95)

#Polynomial Regression Models (taking best fitted polynomial):
confint(poly_length1_3, level = 0.95)
confint(poly_length2_3, level = 0.95)
confint(poly_length3_3, level = 0.95)
confint(poly_height_3, level = 0.95)
confint(poly_width_1, level = 0.95)

#Obtain a 95\% confidence  and prediction intervals  of `weight` for a
#given value of predictor variable.

#length1 predictor:
newlength1= data.frame(length1=20)
predict(reg_length1, newdata=newlength1, interval="confidence",  level = 0.95)
predict(reg_length1, newdata=newlength1, interval="prediction",  level = 0.95)

#length2 predictor:
newlength2= data.frame(length2=20)
predict(reg_length2, newdata=newlength2, interval="confidence",  level = 0.95)
predict(reg_length2, newdata=newlength2, interval="prediction",  level = 0.95)

#length3 predictor:
newlength3= data.frame(length3=20)
predict(reg_length3, newdata=newlength3, interval="confidence",  level = 0.95)
predict(reg_length3, newdata=newlength3, interval="prediction",  level = 0.95)

#height predictor:
newheight= data.frame(height=20)
predict(reg_height, newdata=newheight, interval="confidence",  level = 0.95)
predict(reg_height, newdata=newheight, interval="prediction",  level = 0.95)

#width predictor:
newwidth= data.frame(width=20)
predict(reg_width, newdata=newwidth, interval="confidence",  level = 0.95)
predict(reg_width, newdata=newwidth, interval="prediction",  level = 0.95)

#==============================================================================
#ASSESSING MULTICOLLINEARITY USING VIF
#==============================================================================
#install.packages("car")
library(car)

vif(lm(weight~., data=fishcatch))
#VIF of lengths 1, 2 and 3 very high
vif(lm(weight~.-length1-length2, data=fishcatch))
vif(lm(weight~.-length2-length3, data=fishcatch))
vif(lm(weight~.-length1-length3, data=fishcatch))

#==============================================================================
# VARIABLE SELECTION FOR MULTIPLE REGRESSION MODEL
#==============================================================================

null.fishcatch = lm(weight~ 1, data=fishcatch) # the intercept only model
summary(null.fishcatch)

full.fishcatch = lm(weight~ ., data=fishcatch) # include all predictors
summary(full.fishcatch)

#Forward selection:
forward = step(null.fishcatch, scope=list(lower=null.fishcatch, upper=full.fishcatch), direction="forward", trace=0)
summary(forward)
par(mfrow=c(2,2))
plot(forward)
plot(forward, col="cornflowerblue")
plot(forward, col=fishcatch$Type)

### Backward selection:
backward = step(full.fishcatch, direction="backward", trace=0)
summary(backward)
par(mfrow=c(2,2))
plot(backward, col="cornflowerblue")

### Stepwise   selection
step(null.fishcatch, scope = list(upper=full.fishcatch), direction="both", trace=0)
plot(step(null.fishcatch, scope = list(upper=full.fishcatch), direction="both", trace=0))

#Comparing p-values for the predictor variables, and the graphs themselves,
#forward step method produces the best model (same result as stepwise selection):
#lm(formula = weight ~ length3 + Type + height, data = fishcatch)

#Also, had previously determined that length3 would be a more suitable length
#measurement than length1 or length2 (see VIF).

#However, result doesn't look randomly distributed for residuals vs fitted values.

#==============================================================================
#EXPLORING ALTERNATIVE VARIABLE SELECTIONS MANUALLY
#==============================================================================
mymodel = lm(weight~.-length1-length2, data=fishcatch)
summary(mymodel)
par(mfrow=c(2,2))
plot(mymodel)

lm(weight~Type+height+width+length3, data=fishcatch)
summary(lm(weight~Type+height+width+length3, data=fishcatch))
par(mfrow=c(2,2))
plot(lm(weight~Type+height+width+length3, data=fishcatch))

summary(lm(weight~height), data=fishcatch)
summary(lm(weight~poly(height,2), data=fishcatch))

summary(lm(weight~poly(height,2)+length3+Type, data=fishcatch))
summary(lm(weight~poly(height,2)+length3+Type+width, data=fishcatch))
summary(lm(weight~poly(height,2)+length3+Type, data=fishcatch))
summary(lm(weight~width+length3+Type, data=fishcatch))
summary(lm(weight~poly(width,2)+length3+Type, data=fishcatch))

par(mfrow=c(2,2))
lm = lm(weight~height+length3+Type, data=fishcatch)
summary(lm)
plot(lm)

lmI2 = lm(weight~poly(height,2)+length3+Type, data=fishcatch)
par(mfrow=c(2,2))
plot(lmI2, col="cornflowerblue")
#lmI2 seems to be a marginally better model (slightly closer to normality, and
#more residuals slightly randomly spread), however, not worth the extra varables

#==============================================================================
#APPLYING TRANSFORMATIONS TO MULTIPLE REGRESSION MODEL AND REMOVING CASES
#==============================================================================
#transformation by square-rooting response variable:
sqrtlm = lm(weight^0.5~height+length3+Type, data=fishcatch)
plot(sqrtlm, col = "cornflowerblue")
summary(sqrtlm)5
#Produces much improved result, however case 47 is a significant influential
#observation.

#Therefore, remove case 47 and run model again:
fishcatch47 = fishcatch[-c(47),]
sqrtlm47 = lm(weight^0.5~height+length3+Type, data=fishcatch47)
plot(sqrtlm47, col = "cornflowerblue")
summary(sqrtlm47)
#Now, we can see that cases 104, 143 and 149 are outliers in this model.

#Refering back to original data source, case 143 was noted as being a potential
#outlier, having a large quantity of fish in its stomach.

#Therefore, remove case 143:
fishcatch47143 = fishcatch[-c(47, 143),]
par(mfrow=c(2,2))
sqrtlm47143 = lm(weight^0.5~height+length3+Type, data=fishcatch47143)
plot(sqrtlm47143, col="cornflowerblue")
summary(sqrtlm47143)
#This removal does not appear to make a significant difference to the residual
#data overall, therefore not necessary to remove.

###Finalised multiple regression model:
#Further removing case 104:
fishcatch47104143 = fishcatch[-c(47, 104, 143),]
sqrtlm47104143 = lm(weight^0.5~height+length3+Type, data=fishcatch47104143)
plot(sqrtlm47104143, col="cornflowerblue")
plot(sqrtlm47104143, col=fishcatch$Type)
summary(sqrtlm47104143)
#Normality appears to be improved, without significant outliers remaining.

#Further removing cases 101, 102 and 103:
fishcatch4710414310123 = fishcatch[-c(47, 101, 102, 103, 104, 143),]
sqrtlm4710414310123 = lm(weight^0.5~height+length3+Type, data=fishcatch4710414310123)
plot(sqrtlm4710414310123, col="cornflowerblue")
summary(sqrtlm4710414310123)
#fishcatch4710414310123 appears to be a superior model, since there is a more even
#spread of scattered points, as well as a good normality, p-values, and high R^2.
#However, perhaps it is not necessary to remove so many points.

#==============================================================================
#EXPLORING ALTERNATIVE TRANSFORMATIONS TO MULTIPLE REGRESSION MODEL
#==============================================================================

sqrtlm4710414310123 = lm(weight^0.25~height+length3+Type, data=fishcatch4710414310123)
plot(sqrtlm4710414310123, col="cornflowerblue")
summary(sqrtlm4710414310123)

sqrtlm4710414310123 = lm(weight^(1/3)~height+length3+Type, data=fishcatch4710414310123)
plot(sqrtlm4710414310123, col="cornflowerblue")
summary(sqrtlm4710414310123)

fishcatch471034143 = fishcatch[-c(47, 103, 104, 143),]
cubrtlm471034143 = lm(weight^(1/3)~height+length3+Type, data=fishcatch471034143)
plot(cubrtlm471034143, col="cornflowerblue")
summary(cubrtlm471034143)

fishcatch10124 = fishcatch[-c(101, 102, 103, 104),]
fishcatch10124
lmI210124 = lm(weight~poly(height,2)+length3+Type, data=fishcatch10124)
par(mfrow=c(2,2))
plot(lmI210124, col="cornflowerblue")

fishcatch101234
lmI2101234 = lm(weight~poly(height,2)+length3+Type, data=fishcatch101234)
par(mfrow=c(2,2))
plot(lmI2101234, col="cornflowerblue")

quadlmI2101234 = lm(poly(weight,2)~poly(height,2)+length3+Type, data=fishcatch101234)
plot(quadlmI2101234, col="cornflowerblue")

sqrtlmI2101234 = lm(weight^0.5~poly(height,2)+length3+Type, data=fishcatch101234)
plot(sqrtlmI2101234, col="cornflowerblue")

sqrtlmI2 = lm(weight^0.5~poly(height,2)+length3+Type, data=fishcatch)
plot(sqrtlmI2, col="cornflowerblue")

fishcatch47 = fishcatch[-c(47),]
sqrtlmI247 = lm(weight^0.5~poly(height,2)+length3+Type, data=fishcatch47)
plot(sqrtlmI247, col="cornflowerblue")
summary(sqrtlmI2)
summary(sqrtlmI247)

sqrtlmI247104143 = lm(weight^0.5~poly(height,2)+length3+Type, data=fishcatch47104143)
plot(sqrtlmI247104143, col="cornflowerblue")
summary(sqrtlmI247104143)

#==============================================================================
#CONFIDENCE AND PREDICTION INTERVALS FOR MULTIPLE REGRESSION MODEL
#==============================================================================
#Obtain a 95\% confidence interval for the coefficient estimates.
#Use the `confint()` command.
confint(sqrtlm47104143, level = 0.95)

#Obtain a 95\% confidence  and prediction intervals  of `weight` for a
#given value of predictor variable.
df_roach = subset(fishcatch, fishcatch$Type == "Roach")
newdata = data.frame(length3=25, Type="Roach", height=25)
predict(sqrtlm47104143, newdata=newdata, interval="confidence",  level = 0.95)
predict(sqrtlm47104143, newdata=newdata, interval="prediction",  level = 0.95)


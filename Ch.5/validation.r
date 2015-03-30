install.packages("ISLR")

#-----------------------------------------
# Setting up data for use
#-----------------------------------------
require(ISLR)     # like library, but returns false if the package is not installed
require(boot)
?cv.glm

#-----------------------------------------
# Looking at data set - initial analysis
#-----------------------------------------
# First, we will plot our dataset (Auto)
# plot can actually take in a formula right away
plot(mpg~horsepower, data=Auto)
# the plot of mpg~horsepower appears to be a decay curve

#-----------------------------------------
# Leave One Out Cross Validation
#-----------------------------------------

glm.mod = glm(mpg~horsepower, data=Auto)      # fits a linear model (by not specifying 'family')
# we see that the model created is not very good
# according to the residuals v fitted plot, we see that there is a slight curve, indicating a NON-LINEAR relationship
# this makes sense with our findings earlier, as the plot of mpg~horsepower appears to be a decay curve

# now, we will look at the cross validation of this model
# unfortunately, the version native to R is rather slow
cv1 = cv.glm(Auto, glm.mod)
# the 'delta' spits out a two values, one is the raw cross validation results, the other is a BIAS-CORRECTED version

# now, we will instead write our own function to do this quickly
loocv = function(fit) {
  h = lm.influence(fit)$h   # h is a vector
  mean((residuals(fit)/(1-h))^2)         # residuals(fit) gives us the residuals from the full fit
}

# now, see if this crazy thing works!
loovc(glm.mod)
# wow, that was fast

# so, we will use it now

cv.error = rep(0,5)         # creates a vector of 5 repeated 0's
degree = 1:5                # creates a vector of int's 1 to 5

for (d in degree) {
  glm.fit = glm(mpg~poly(horsepower,d), data = Auto)
  cv.error[d] = loocv(glm.fit)
}

plot(degree, cv.error, type='b')  # plots the degree (1:5) v. the cv.error, break the 
# we see that a degree of 1 has a relatively poor cv.error, but degree of 2 drops it right down
# increasing the degree beyond two doesn't get much better


#-----------------------------------------
# 10-Fold Cross Validation
#-----------------------------------------

cv.error10 = rep(0,5)
for (d in degree) {
  glm.fit = glm(mpg~poly(horsepower, d), data = Auto)
  cv.error10[d] = cv.glm(Auto, glm.fit, K=10)$delta[1]
}
# wow, 10-fold CV works almost as well as LOOCV!
# that is, they yield very similar values for cross validation results
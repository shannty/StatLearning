setwd("/home/shan/Documents/School/UNI/THIRD YEAR/Second Semester/Stat 311/Ch5")
install.packages("ISLR")

#-----------------------------------------
# Setting up data for use
#-----------------------------------------
require(ISLR)     # like library, but returns false if the package is not installed
require(boot)

#-----------------------------------------
# THE BOOTSTRAP
#-----------------------------------------
head(Portfolio)             # looks at the top 5 values of the Portfolio data set
plot(Portfolio)
abline(0, 1)                # too look how the data appear around a slope of 1
# x and y of Portfolio do not appear to be very related

#-----------------------------------------
# THE BOOTSTRAP
#-----------------------------------------

alpha = function(x,y) {     # given two vectors of data, x and y
  vx = var(x)
  vy = var(y)
  cxy = cov(x,y)
  (vy-cxy)/(vx+vy-2*cxy)    # return the alpha value
}

alpha(Portfolio$X, Portfolio$Y)

# now, let us find the STANDARD ERROR of alpha?

alpha.fn = function(data, index) {
  with(data[index,],alpha(X,Y))     # using the data in the dataframe, exucute the command (allows us to the use the named variables from the data frame, X and Y)
}
alpha.fn(Portfolio, 1:100)        # calculate alpha once, using data points 1:100 in Portfolio

set.seed(1)
alpha.fn(Portfolio, sample(1:100, 100, replace=TRUE))     # now, we calculate alpha using points 1:100 100 times, with replacement (essentially, performing BOOTSTRAP once)

# now, let's allow the bootstrap do its thing!
boot.out = boot(Portfolio, alpha.fn, R=1000)            # perform the bootstrap 1000 times
boot.out
# we were interested in the std. error, which was 0.08861826
# bias was 
plot(boot.out)

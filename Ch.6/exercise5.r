#-----------------------------------------
# Setting up data for use
#-----------------------------------------
load("5.R.RData")
set.seed(1)

#-----------------------------------------
# Initial Data Exploration
#-----------------------------------------

head(Xy)
plot(Xy)
# wow, these are weird graphs, can't see any distinguishable features that lead to a particular model.

lin.mod = glm(y~X1+X2, data = Xy)
lin.mod2 = lm(y~X1+X2, data = Xy)
summary(lin.mod)
# it appears that the two variables are both very significant
# we got a Std. Error for X1 as 0.02593
matplot(Xy, type="l")

# according to the plot provided, our estimate of SE for X1 is...
# actually too low! we see that upon inspection, the data within consecutive rows is very highly correlated. This has the effect of reducing our effective observations significantly (much less than the 1000 rows we have)


# now, we will use the standard bootstrap to estimate SE

require(boot)

# to do this, we need a WRAPPER FUNCTION to take care of some work for us
Xy.fxn = function(data, index) {
  coef(glm(y~X1+X2, data = data, subset = index))
}

boot.strap = boot(Xy, Xy.fxn, R=1000)
# t2 Standard Error: 0.02941065
# again, becuase we assumed that our data was independent and identically distributed (i.i.d)
# now, the idea is to perform BLOCK BOOTSTRAP

new.rows=c(101:200, 401:500, 101:200, 901:1000, 301:400, 1:100, 1:100, 801:900, 201:300, 701:800)
new.Xy = Xy[new.rows,]
head(new.Xy)

# creating our fxn that will be used by the bootstrap
newXy.fxn = function(data, index) {
  coef(glm(y~X1+X2, data = data, subset = index))
}

boot.strap2 = boot(new.Xy, newXy.fxn, R=1000)
# boot.strap3 = tsboot(new.Xy, newXy.fxn, l=100, sim="fixed", R=1000) # JK NOPE LUL

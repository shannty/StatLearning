#-----------------------------------------
# Principal Components Regression
# this code demonstrates performing the above slm (statistical learning method)
#-----------------------------------------

require(ISLR)
require(pls)

set.seed(2)
train = sample(1:777,500)

y = College$Apps                        
x = model.matrix(Apps~., College)[,-1]

pcr.mod = pcr(Apps~., data = College, scale = TRUE,  validation = "CV", subset = train)
summary(pcr.mod)
validationplot(pcr.mod, val.type = "MSEP")

v = rep(0,17)
for(i in 1:17) {
  pcr.pred = predict(pcr.mod, x[-train,], ncomp = i)
  v[i] = mean((pcr.pred-y[-train])^2)
}
v

plot(v, xlab = "ncomp", ylab = "MSE", type = "b")
# this plot essentially shows the same thing that the validationplot() command does!

v[5]
# so we see that using an ncomp value of 5 gets us a relatively low MSE, and the drop form ncomp = 5 to ncomp = 6 is not too big so we stick with 5, thus we will save it as pcr.bestMSE
pcr.bestMSE = v[5]


#-----------------------------------------
# Partial Least Squares
# this code demonstrates performing the above slm
# this differs from the above model, in that while it creates the linear combinations, it ensures that they have high correlation to the response
#-----------------------------------------

set.seed(2)
pls.mod = plsr(Apps~., data = College, subset = train, scale = TRUE)
summary(pls.mod)
validationplot(pls.mod, val.type = "MSEP")
# we see from the plot created that the MSE levels off around 5

pls.predict = predict(pls.mod, x[-train,], ncomp = 5)
pls.bestMSE = mean((pls.predict - y[-train])^2)
pls.bestMSE


# now, compare the two MSE's obtained
pls.bestMSE
pcr.bestMSE

# huh, well it looks like partial least squares is better after all!
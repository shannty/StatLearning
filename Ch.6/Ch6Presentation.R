#-----------------------------------------
# Lasso Regression
# A demonstration of using the Lasso as a method of variable selection and model fitting
#-----------------------------------------
require(ISLR)
require(glmnet)

set.seed(100)                             # to make our findings reproducible
train=sample(1:777,500)                   # take a random sample of 500 from 1:777 (this will be our training set)
head(College)

y = College$Apps                          # create our vector of response
x = model.matrix(Apps~., College)[,-1]    # put the rest of our variables into a model.matrix the [,-1] removes the first column, which corresponds to the intercept, which we do not want to include as part of our model

# We will now determine what the best value of lambda will be for our model. This is done by computing the cross-validation using the training data.
cv = cv.glmnet(x[train,], y[train])
cv$lambda.min                             # 1.859103 - this is the smallest lambda value, which will be used in creating our optimal model

lasso.mod = glmnet(x[train,], y[train])   # this is the model that holds all the lasso models we will want to check
plot(lasso.mod, xvar = "lambda", label = TRUE ) # this plot shows us the value of the different coefficients at different values of lambda. Note that the numbers appearing at the top of the graph (16, 11, 3, 1) refer to the number of variable coefficients that are not zero for the corresponding value of log(lambda)

lasso.pred = predict(lasso.mod, s = cv$lambda.min, newx = x[-train,]) # now, as we know the optimal lambda value (1.859103), we can use it to grab the corresponding model, and use it to predict values in the validation set
lasso.pred

lasso.coef = predict(lasso.mod, type="coefficients", s = cv$lambda.min, newx = x[-train,]) # this prediction returns the values of the coefficients in our optimal model
lasso.coef

# Now, we want to check and see how much of an improvement our model is over the baseline MSE
MSE = mean((lasso.pred - y[-train])^2)      # calulates the MSE from our model
BaseMSE = mean((mean(y[train]) - y[-train])^2) # calculates the baseline MSE
MSE / BaseMSE                         # 0.08971453 - compares our MSE to the BaselineMSE

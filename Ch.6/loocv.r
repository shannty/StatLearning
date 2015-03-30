#-----------------------------------------
# Leave One Out Cross Validation
# a function that computes the LOOCV very quickly
#-----------------------------------------
loocv = function(fit) {
  h = lm.influence(fit)$h   # h is a vector
  mean((residuals(fit)/(1-h))^2)         # residuals(fit) gives us the residuals from the full fit
}
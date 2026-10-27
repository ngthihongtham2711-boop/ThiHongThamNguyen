library ("rio")
install.packages("censReg")
library(censReg)
install.packages ("car")
library(car)
data <- import ("~/Downloads/MASTER SEMESTER 1 (26)/MICRO MATERIALS /fringe.dta")
##Problem set 1
### a)
hist(data$pension)
data$pension == 0
sum(data$pension == 0)/nrow (data)


### b)
reg <- censReg(pension ~ age + exper + tenure, data = data)
summary(reg)
linearHypothesis(reg, c("exper = tenure"))

# Extract coefficients
est <- coef(reg, logSigma = FALSE)
beta <- est[-length(est)]
beta1 <- beta[2]
sigma <- est[length(est)]
# Create regressor matrix X and matrix with average regressors
X <- model.matrix(pension ~ age + exper + tenure, data = data)
Xbar <- colMeans(X)
# Compute linear index evaluated at average regressors
xbarb <- sum(Xbar * beta)
# MEA of age
beta1 * pnorm(xbarb/sigma)

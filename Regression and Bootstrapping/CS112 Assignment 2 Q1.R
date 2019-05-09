set.seed(23022019)
independent <- runif(999, 0, 30)
dependent <- c()

# creatign data
for (i in independent) {
  value = 0.5*i + rnorm(1, mean = 0, sd = 30) - 1
  dependent <- c(dependent, value)
}

# adding outlier
independent_extra <- c(independent, 27)
dependent_extra <- c(dependent, -21500)

summary(lm(dependent ~ independent))
summary(lm(dependent_extra ~ independent_extra))

#################
# plot the original 999 data points
plot(independent, dependent, 
     main="The plot for the orignal 999 data points",
     xlab="Year of experience (year)", 
     ylab="Debt (1000$)")
abline(lm(dependent ~ independent), col = "blue", lwd = 2)

#################
# plot the 1000 data points 
plot(independent_extra, dependent_extra, 
     main="The plot for the 1000 data points",
     xlab="Year of experience (year)", 
     ylab="Debt (1000$)")
points(27, -21500, col= "blue", pch = 19)
abline(lm(dependent ~ independent), col = "blue", lwd = 2)
abline(lm(dependent_extra ~ independent_extra), col = "red", lwd = 2)

#################
# plot the before and after-trend but in the scale of original 999 points
plot(independent, dependent, 
     main="The zoomed-in plot for the 1000 data points",
     xlab="Year of experience (year)", 
     ylab="Debt (1000$)")
abline(lm(dependent ~ independent), col = "blue", lwd = 2)
abline(lm(dependent_extra ~ independent_extra), col = "red", lwd = 2)
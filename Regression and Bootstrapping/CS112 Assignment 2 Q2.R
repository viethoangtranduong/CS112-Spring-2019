library(arm)
data(lalonde)

head(lalonde)

set.seed(23022019)
data <- lalonde[lalonde$treat == 0, ]

lm.model.1 <- lm(re78 ~ age + educ + re74 + re75
          +  I(educ*re74) +   I(educ*re75) +  I(age*re74)
          +  I(age*re75) + I(age^2) +  I(re74*re75),  data = data)
summary(lm.model.1)
lm.model.1.sim <- sim(lm.model.1, n.sims = 10000)
  


########################
# Use median and compute expected values
median_edu <- median(data$educ)
median_re74 <- median(data$re74)
median_re75 <- median(data$re75)
plot(x = c(1:100), y = c(1:100), type = "n", 
     xlim = c(15,60), ylim = c(0,15000),
     main = "Expected earnings in 1978 given age and 
     hold other variables at their median (blue segments, red points); or
     at their 75th percentile (red segments, blue points)",
     xlab = "Age (years old)",
     ylab = "Earning ($)")

expected_value_atmedian_store <- matrix(, nrow = 3, ncol = 39)
rownames(expected_value_atmedian_store) <- c("lower 95% quantile", "median", "upper 95% quantile")
colnames(expected_value_atmedian_store) <- c(17:55)

for (age in 17:55) {
  storage <- c()
  for (i in 1:10000) {
    # plug the data the linear regression coefficients
    storage <- c(storage, sum(coef(lm.model.1.sim)[i, ]*c(1, age, median_edu, median_re74, median_re75, 
                                                     median_edu*median_re74, median_edu*median_re75,
                                                     age*median_re74, age*median_re75, age^2, 
                                                     median_re74*median_re75)))
  }
  
  expected_value_atmedian_store[, age - 16] <- c(quantile(storage, 0.025),
                                median(storage), 
                                quantile(storage, 0.975))
  
  segments(
    x0 = age,
    y0 = quantile(storage, 0.025),
    x1 = age,
    y1 = quantile(storage, 0.975),
    lwd = 2,
    col = rgb(0,0,1,.5))
  points(age, median(storage), col = "red", pch = 19)
}

########################
# Use 75 quartile and compute expected values

expected_value_at75_store <- matrix(, nrow = 3, ncol = 39)
rownames(expected_value_at75_store) <- c("lower 95% quantile", "median", "upper 95% quantile")
colnames(expected_value_at75_store) <- c(17:55)

quantile_edu <- quantile(data$educ, 0.75)
quantile_re74 <- quantile(data$re74, 0.75)
quantile_re75 <- quantile(data$re75, 0.75)


for (age in 17:55) {
  storage <- c()
  for (i in 1:10000) {
    # plug the data the linear regression coefficients
    storage <- c(storage, sum(coef(lm.model.1.sim)[i, ]*c(1, age, quantile_edu, quantile_re74, quantile_re75, 
                                                          quantile_edu*quantile_re74, quantile_edu*quantile_re75,
                                                          age*quantile_re74, age*quantile_re75, age^2, 
                                                          quantile_re74*quantile_re75)))
  }
  
  expected_value_at75_store[, age - 16] <- c(quantile(storage, 0.025),
                                                 median(storage), 
                                                 quantile(storage, 0.975))
  
  segments(
    x0 = age,
    y0 = quantile(storage, 0.025),
    x1 = age,
    y1 = quantile(storage, 0.975),
    lwd = 2,
    col = rgb(1,0,0,.5))
  points(age, median(storage), col= "blue", pch = 19)
}

########################
#predicted values at median
predicted_value_atmedian_store <- matrix(, nrow = 3, ncol = 39)
rownames(predicted_value_atmedian_store) <- c("lower 95% quantile", "median", "upper 95% quantile")
colnames(predicted_value_atmedian_store) <- c(17:55)

plot(x = c(1:100), y = c(1:100), type = "n", 
     xlim = c(15,60), ylim = c(-10000,22000),
     main = "Predicted earnings in 1978 given age and 
     hold other variables at their median (blue segments, red points); or
     at their 75th percentile (red segments, blue points)",
     xlab = "Age (years old)",
     ylab = "Earning ($)")

for (age in 17:55) {
  storage <- c()
  for (i in 1:10000) {
    # plug the data the linear regression coefficients
    storage <- c(storage, sum(coef(lm.model.1.sim)[i, ]*c(1, age, median_edu, median_re74, median_re75, 
                                                          median_edu*median_re74, median_edu*median_re75,
                                                          age*median_re74, age*median_re75, age^2, 
                                                          median_re74*median_re75))
                              + rnorm(1, mean = 0, sd = lm.model.1.sim@sigma[i]))
  }
  
  predicted_value_atmedian_store[, age - 16] <- c(quantile(storage, 0.025),
                                                 median(storage), 
                                                 quantile(storage, 0.975))
  
  
  segments(
    x0 = age,
    y0 = quantile(storage, 0.025),
    x1 = age,
    y1 = quantile(storage, 0.975),
    lwd = 2,
    col = rgb(0,0,1,.5))
  points(age, median(storage), col = "red", pch = 19)
}

########################
# predicted values at 75% quantile
predicted_value_at75_store <- matrix(, nrow = 3, ncol = 39)
rownames(predicted_value_at75_store) <- c("lower 95% quantile", "median", "upper 95% quantile")
colnames(predicted_value_at75_store) <- c(17:55)


for (age in 17:55) {
  storage <- c()
  for (i in 1:10000) {
    # plug the data the linear regression coefficients
    storage <- c(storage, sum(coef(lm.model.1.sim)[i, ]*c(1, age, quantile_edu, quantile_re74, quantile_re75, 
                                                          quantile_edu*quantile_re74, quantile_edu*quantile_re75,
                                                          age*quantile_re74, age*quantile_re75, age^2,
                                                          quantile_re74*quantile_re75))
                 + rnorm(1, mean = 0, sd = lm.model.1.sim@sigma[i]))
  }
  
  predicted_value_at75_store[, age - 16] <- c(quantile(storage, 0.025),
                                                  median(storage), 
                                                  quantile(storage, 0.975))
  
  segments(
    x0 = age,
    y0 = quantile(storage, 0.025),
    x1 = age,
    y1 = quantile(storage, 0.975),
    lwd = 2,
    col = rgb(1,0,0,.5))
  points(age, median(storage), col= "blue", pch = 19)
}

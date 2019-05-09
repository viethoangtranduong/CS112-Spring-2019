data(PlantGrowth)
data <- PlantGrowth

# delete trt2 group
data2 <- data[-which(data$group == 'trt2'), ]

# trt1 will be 1, ctrl is 0
data2$group_2 <- as.numeric(data2$group == 'trt1')

find_r_squared <- function(y , predicted_y) {
  mean_y <- mean(y)  # the observed data mean
  SSError <- sum((y - predicted_y)^2)  # sum of squared errors
  SSTotal <- sum((y - mean_y)^2)    # total sum of squares
  return(1 - SSError/SSTotal)   # r^2
}

find_r_squared(data2$weight, predict(lm(weight ~ group_2 , data = data2)))
summary(lm(weight ~ group_2 , data = data2))$r.squared



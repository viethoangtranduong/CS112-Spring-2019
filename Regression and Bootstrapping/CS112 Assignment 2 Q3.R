data(PlantGrowth)
data <- PlantGrowth
set.seed(23022019)
# delete trt2 group
data2 <- data[-which(data$group == 'trt2'), ]

# trt1 will be 1, ctrl is 0
data2$group_2 <- as.numeric(data2$group == 'trt1')

plot(data2$group_2, data2$weight, 
     main = "The plot represents the effect of treatment 
     on the weight", 
     xlab = "Treatment (0 = control group, 1 = treat1 group)", 
     ylab = "Weight")

lm.model.1 <- lm(weight ~ group_2, data = data2)
summary(lm.model.1)
summary(lm.model.1)$coef[2]
abline(lm.model.1, col = "red", lwd = 3)
#PlantGrowth <- replace(PlantGrowth, weight, from = "ctrl", to = as.integer(0))

# bootstraping
storage <- c()
for (i in 1:10000) {
  # c(1:20) because there are 20 data points those in control and treatment group
  storage[i] <- summary((lm(weight ~ group_2, 
                            data = data2[sample(c(1:20), replace = TRUE),])))$coef[2]
}


# plot the histogram and relevant statistics
hist(storage, lwd = 2,
     main = "The histogram of the observed effect",
     xlab = "Weight",
     ylab = "Frequency")
abline(v = mean(storage),
       col = rgb(1,0,0,0.3),
       lwd = 5)
abline(v = quantile(storage, 0.025),
       col = "blue",
       lwd = 4)
abline(v = quantile(storage, 0.975),
       col = "blue",
       lwd = 4)
abline(v = summary(lm.model.1)$coef[2],
       col = rgb(0,1,0,0.3),
       lwd = 2)

mean(storage)
quantile(storage, c(0.025, 0.975))
summary(lm.model.1)$coef[2]
confint(lm.model.1, level = 0.95)
summary(lm.model.1)

library(haven)

nsw <- read_dta("C:/Users/Minerva Floater 2/Documents/CS112/nsw.dta")
nsw


glm.model <- glm(treat ~ ., data=subset(nsw, select=c( -re78, -data_id )),
                         family = binomial)
glm.model

# subsetting the data into 2 groups
nsw_trt <- nsw[which(nsw$treat == 1),]
nsw_ctrl <- nsw[which(nsw$treat == 0),]

nsw_trt_storage <- c()
for (i in 1:nrow(nsw_trt)) {
  # find the logit
  logit <- sum(glm.model$coefficients*c(1, nsw_trt$age[i], nsw_trt$education[i], 
                              nsw_trt$black[i], nsw_trt$hispanic[i], 
                              nsw_trt$married[i], nsw_trt$nodegree[i], 
                              nsw_trt$re75[i]))
  # using the logit to find the probability
    prob <- exp(logit) / (1 + exp(logit))
    # store it in a vector
  nsw_trt_storage <- c(nsw_trt_storage, prob)
}

nsw_ctrl_storage <- c()
for (i in 1:nrow(nsw_ctrl)) {
  # find the logit
  logit <- sum(glm.model$coefficients*c(1, nsw_ctrl$age[i], nsw_ctrl$education[i], 
                                nsw_ctrl$black[i], nsw_ctrl$hispanic[i], 
                                nsw_ctrl$married[i], nsw_ctrl$nodegree[i], 
                                nsw_ctrl$re75[i]))
  # using the logit to find the probability
  prob <- exp(logit) / (1 + exp(logit))
  # store it in a vector
  nsw_ctrl_storage <- c(nsw_ctrl_storage, prob)
}

hist(nsw_ctrl_storage, col = rgb(0,0,1,0.5), 
     main = "The Histogram represents the distribution of the 
     predicted probability being assigned to treatment group 
     from the 2 original group: control and treatment",
     xlab = "Probability")

hist(nsw_trt_storage, col = rgb(1,0,0,0.5),
     main = "The Histogram represents the distribution 
    of the predicted probability being assigned to treatment group from the original treatment group",
     xlab = "Probability", add = T)

legend(0.5,150, bty = "n", title = 'Legend', legend = c('Control Group', 'Treatment Group 1'),
              fill = c(rgb(0,0,1,0.5), rgb(1,0,0,0.5)))


#############
# plot density plot
library(ggplot2)

ggplot() + 
  geom_density(data = data.frame(nsw_ctrl_storage), aes(x = nsw_ctrl_storage, fill = TRUE), 
               fill = rgb(0,0,1,0.5), color = "black", alpha = 0.7, lwd = 1.5) + 
  geom_density(data = data.frame(nsw_trt_storage), aes(x = nsw_trt_storage),
               fill = rgb(1,0,0,0.5), color = "black", alpha = 0.7, lwd = 1.5) + 
  labs(title = "Density Plot for the observed propability") + 
  xlab("Probability") + 
  ylab("Percentage (%)") + theme(legend.position = c(0.5, 5))
mean(nsw_ctrl_storage)
mean(nsw_trt_storage)

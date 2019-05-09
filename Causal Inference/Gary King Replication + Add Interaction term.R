library(Matching)
library(arm)
library(rgenoud)
'PEACEKEEPING WORKOUT (based on  King, Gary;Zeng, Langche, 2007, 
"Replication data for: When Can History be Our Guide? 
The Pitfalls of Counterfactual Inference", 
https://hdl.handle.net/1902.1/DXRXCFAWPK, 
Harvard Dataverse, V4, 
UNF:3:DaYlT6QSX9r0D50ye+tXpA== [fileUNF] )'
# CONSIDER USING THE JUPYTER NOTEBOOK WITH R-SERVER KERNEL (NEVER R-SAGE KERNEL)
foo <- read.csv("https://course-resources.minerva.kgi.edu/uploaded_files/mke/00086677-3767/peace.csv")

# extract relevant columns
# add 108: logdead, 52: uncint, 34: pbs2l, 35: pbs5l
foo <- foo[, c(6:8, 11:16, 99, 50, 114, 49, 63, 136, 109, 126, 48, 160, 142, 10, 108, 52, 34, 35)]

# remove 2 rows with missing data (there are better ways to handle missing data)
foo <- foo[c(-19, -47), ]

# check that all missing data is gone...
which(is.na(foo) == TRUE)

# take a peek at the data set (identify the columns)
head(foo)

### Outcome is "pbs2s3": "democracy" and "peace" within 2 years after the end of the war
### codebook is here: http://www.nyu.edu/gsas/dept/politics/faculty/cohen/codebook.pdf

### Treatment indicator is "untype4": "multidimensional peacekeeping/peacebuilding"

### How many treated units? How many controls? How do you feel about SUTVA?


########################### Begin

me.wartype <- mean(foo$wartype)
me.logcost <- mean(foo$logcost)
me.wardur <- mean(foo$wardur)
me.factnum <- mean(foo$factnum)
me.factnum2 <- mean(foo$factnum2)
me.trnsfcap <- mean(foo$trnsfcap)
me.treaty <- mean(foo$treaty)
me.develop <- mean(foo$develop)
me.exp <- mean(foo$exp)
me.decade <- mean(foo$decade)
me.logdead <- mean(foo$logdead)

# GaryKing version

glm0 <- glm(pbs2s3 ~  wartype + logcost + wardur + factnum + factnum2 +
              trnsfcap + untype4 + treaty + develop + exp + decade,
            data = foo, family = binomial)

glm00 <- glm(pbs2s3 ~  wartype + logcost + wardur + factnum + factnum2 +
               trnsfcap + untype4 + treaty + develop + exp + decade
             + I(wardur * untype4),
             data = foo, family = binomial)

glm01 <- glm(pbs2s3 ~  wartype + logcost + wardur + factnum + factnum2 +
               trnsfcap + untype4 + treaty + develop + exp + decade
             + I(logcost * untype4),
             data = foo, family = binomial)

model0 <- c()
model00 <- c()
model01 <- c()

logistic <- function(x) {
  exp(x)/(1+exp(x))
}

for (i in c(0:315)) {
  X0_treat <- c(1, me.wartype, me.logcost, i, me.factnum, 
                me.factnum2, me.trnsfcap, 1, me.treaty, 
                me.develop, me.exp, me.decade)
  X0_control <- c(1, me.wartype, me.logcost, i, me.factnum, 
                  me.factnum2, me.trnsfcap, 0, me.treaty, 
                  me.develop, me.exp, me.decade)
  
  X00_treat <- c(1, me.wartype, me.logcost, i, me.factnum, 
                 me.factnum2, me.trnsfcap, 1, me.treaty, 
                 me.develop, me.exp, me.decade, i*1)
  X00_control <- c(1, me.wartype, me.logcost, i, me.factnum, 
                   me.factnum2, me.trnsfcap, 0, me.treaty, 
                   me.develop, me.exp, me.decade, i*0)
  X01_treat <- c(1, me.wartype, me.logcost, i, me.factnum, 
                 me.factnum2, me.trnsfcap, 1, me.treaty, 
                 me.develop, me.exp, me.decade, me.logcost*1)
  X01_control <- c(1, me.wartype, me.logcost, i, me.factnum, 
                   me.factnum2, me.trnsfcap, 0, me.treaty, 
                   me.develop, me.exp, me.decade, me.logcost*0)
  
  model0 <- c(model0, 
              logistic(sum(coef(glm0)*X0_treat)) - logistic(sum(coef(glm0)*X0_control)))
  model00 <- c(model00,
               logistic(sum(coef(glm00)*X00_treat)) - logistic(sum(coef(glm00)*X00_control)))
  model01 <- c(model01,
               logistic(sum(coef(glm01)*X01_treat)) - logistic(sum(coef(glm01)*X01_control)))
  
}

plot(x = c(0:315), y = c(0:315), type = "n", 
     xlim = c(0,315), 
     ylim = c(0, 0.8), 
     main = " Causal Effect of Multidimensional UN Peacekeeping Operations", 
     ylab = "Marginal effects of UN peacekeeping operations",
     xlab = 'Duration of wars in months')
lines(c(0:315), model0, col="black",lty=2, lwd = 2)
lines(c(0:315), model00, col="red", lwd = 2)
lines(c(0:315), model01, col="blue", lwd = 2)
text(105, 0.8, "The model with I(logcost*untype4)", col = "blue")
text(70, 0.3, "The model with I(wardur*untype4)", col = "red")
text(250, 0.43, "The Original model", col = "black")








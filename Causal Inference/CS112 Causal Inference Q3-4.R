library(Matching)
library(arm)
library(rgenoud)
set.seed(2019)
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

# filter missing data in pbs5l
foo <- foo[-which(is.na(foo$pbs5l) == TRUE), ]

# check that all missing data is gone...
which(is.na(foo) == TRUE)

# Change the data

# Decode uncint
foo$uncint <- as.character(foo$uncint)
foo$uncint[which(foo$uncint == "None")] <- "0"
foo$uncint[which(foo$uncint == "Observer")] <- "2"
foo$uncint[which(foo$uncint == "PKO")] <- "3"
foo$uncint[which(foo$uncint == "Enforcement")] <- "4"
foo$uncint <- as.integer(foo$uncint)

# decode pbs2l
foo$pbs2l <- as.character(foo$pbs2l)
foo$pbs2l <- as.numeric(foo$pbs2l == "Success")

# decode pbs5l 
foo$pbs5l <- as.character(foo$pbs5l)
foo$pbs5l <- as.numeric(foo$pbs5l == "Success")


##################### BEGIN #####################
# QUestion 3
Tr <- rep(0, length(foo$uncint))
Tr[which(foo$uncint != 0 & foo$uncint != 1)] <- 1
Y2 <- foo$pbs2l
Y5 <- foo$pbs5l
# Question 4


## 4b


## 4c. 
###############
# Logistic Regression
## On pbs2l
glm_2l <- glm(Y2 ~ Tr + wartype + logcost + wardur + factnum + factnum2 +
              trnsfcap + treaty + develop + exp + decade,
            data = foo, family = binomial)

summary(glm_2l)

## On pbs5l
glm_5l <- glm(Y5 ~ Tr + wartype + logcost + wardur + factnum + factnum2 +
              trnsfcap + treaty + develop + exp + decade,
            data = foo, family = binomial)

summary(glm_5l)

TrGroup <- foo[which(Tr == 1), ]
CoGroup <- foo[which(Tr == 0), ]
TrValue <- data.frame(1, TrGroup$wartype, TrGroup$logcost,
                      TrGroup$wardur, TrGroup$factnum, TrGroup$factnum2,
                      TrGroup$trnsfcap, TrGroup$treaty, TrGroup$develop, 
                      TrGroup$exp, TrGroup$decade)
TrValue.counter <- data.frame(0, TrGroup$wartype, TrGroup$logcost,
                      TrGroup$wardur, TrGroup$factnum, TrGroup$factnum2,
                      TrGroup$trnsfcap, TrGroup$treaty, TrGroup$develop, 
                      TrGroup$exp, TrGroup$decade)
CoValue <- data.frame(0, CoGroup$wartype, CoGroup$logcost,
                      CoGroup$wardur, CoGroup$factnum, CoGroup$factnum2,
                      CoGroup$trnsfcap, CoGroup$treaty, CoGroup$develop, 
                      CoGroup$exp, CoGroup$decade)
CoValue.counter <- data.frame(1, CoGroup$wartype, CoGroup$logcost,
                      CoGroup$wardur, CoGroup$factnum, CoGroup$factnum2,
                      CoGroup$trnsfcap, CoGroup$treaty, CoGroup$develop, 
                      CoGroup$exp, CoGroup$decade)
names(TrValue) <- c('Tr', 'wartype','logcost','wardur','factnum','factnum2','trnsfcap',
                    'treaty','develop','exp','decade')
names(TrValue.counter) <- c('Tr', 'wartype','logcost','wardur','factnum','factnum2','trnsfcap',
                    'treaty','develop','exp','decade')
names(CoValue) <- c('Tr', 'wartype','logcost','wardur','factnum','factnum2','trnsfcap',
                    'treaty','develop','exp','decade')
names(CoValue.counter) <- c('Tr', 'wartype','logcost','wardur','factnum','factnum2','trnsfcap',
                    'treaty','develop','exp','decade')


### The effect of elements for pbs2l
effect_treat_2 <- c()
for(i in 1:nrow(TrGroup)){ 
  effect_treat_2 <- c(effect_treat_2, predict(glm_2l, newdata = TrValue[i,],type = 'response')- predict(glm_2l, newdata = TrValue.counter[i,], type = 'response'))
}

effect_contr_2 <- c()
for(i in 1:nrow(CoGroup)){ 
  effect_contr_2 <- c(effect_contr_2, predict(glm_2l, newdata = CoValue.counter[i,],type = 'response')- predict(glm_2l, newdata = CoValue[i,], type = 'response'))
}

mean(c(effect_treat_2, effect_contr_2))
mean(effect_treat_2)
mean(effect_contr_2)



### The effect of elements for pbs5l
effect_treat_5 <- c()
for(i in 1:nrow(TrGroup)){ 
  effect_treat_5 <- c(effect_treat_5, predict(glm_5l, newdata = TrValue[i,],type = 'response')- predict(glm_2l, newdata = TrValue.counter[i,], type = 'response'))
}

effect_contr_5 <- c()
for(i in 1:nrow(CoGroup)){ 
  effect_contr_5 <- c(effect_contr_5, predict(glm_5l, newdata = CoValue.counter[i,],type = 'response')- predict(glm_2l, newdata = CoValue[i,], type = 'response'))
}

mean(c(effect_treat_5, effect_contr_5))
mean(effect_treat_5)
mean(effect_contr_5)



### p-value 
mb3_lg <- MatchBalance(Tr ~ wartype + logcost + wardur + factnum + 
                         factnum2 + trnsfcap + untype4 + 
                         treaty + develop + exp + decade, 
                       data=foo, nboots = 500)


###############
# prospensity score

glm_ps <- glm(Tr ~ wartype + logcost + wardur + factnum + 
                factnum2 + trnsfcap + 
                treaty + develop + exp + decade, 
              data=foo, family = binomial)

PS <- glm_ps$fitted

### Match for pbs2l on PM
mout1_2l <- Match(Y=Y2, Tr=Tr, X=PS, M=1, replace = TRUE, BiasAdjust = TRUE)

mb1_2l <- MatchBalance(Tr ~ wartype + logcost + wardur + factnum + 
                         factnum2 + trnsfcap + 
                         treaty + develop + exp + decade, 
                       data=foo, match.out = mout1_2l, nboots=10000)

summary(mout1_2l)
mout1_2l$est.noadj

### Match for pbs5l on PM
mout1_5l <- Match(Y=Y5, Tr=Tr, X=PS, M=1, replace = TRUE, BiasAdjust = TRUE)


mb1_5l <- MatchBalance(Tr ~ wartype + logcost + wardur + factnum + factnum2 +
                         trnsfcap + treaty + develop + exp + decade,
                       data=foo, match.out = mout1_5l, nboots=10000)

summary(mout1_5l)
mout1_5l$est.noadj

################
# Genetic Matching

dta = data.frame(foo$wartype, foo$logcost, foo$wardur, foo$factnum,  
            foo$factnum2, foo$trnsfcap, 
            foo$treaty, foo$develop, foo$exp, foo$decade)

##### pbs2l
genout_2l <- GenMatch(Tr = Tr, X = dta, estimand="ATT",
                    pop.size = 500, max.generations = 50, wait.generations = 10)

genout1_2l <- GenMatch(Tr = Tr, X = dta, estimand="ATT",
                      pop.size = 500, max.generations = 50, wait.generations = 10,
                      starting.values = genout_2l$par)

mout2_2l <- Match(Y = Y2, Tr = Tr, X = dta, estimand="ATT", Weight.matrix=genout1_2l, BiasAdjust = T)

mb2_2l  <- MatchBalance(Tr ~ wartype + logcost + wardur + factnum + 
                       factnum2 + trnsfcap + 
                       treaty + develop + exp + decade,
                     data=foo, match.out = mout2_2l, nboots=10000)
summary(mout2_2l)
mout2_2l$est.noadj

##### pbs5l

genout_5l <- GenMatch(Tr = Tr, X = dta, estimand="ATT", 
                      pop.size = 500, max.generations = 50, wait.generations = 10)

genout1_5l <- GenMatch(Tr = Tr, X = dta, estimand="ATT", 
                      pop.size = 500, max.generations = 50, wait.generations = 10, 
                      starting.values = genout_5l$par)

mout2_5l <- Match(Y = Y2, Tr = Tr, X = dta, estimand="ATT", Weight.matrix=genout1_5l, BiasAdjust = T)

mb2_5l  <- MatchBalance(Tr ~ wartype + logcost + wardur + factnum + 
                          factnum2 + trnsfcap + 
                          treaty + develop + exp + decade,
                        data=foo, match.out = mout2_5l, nboots=10000)

summary((mout2_5l))
mout2_5l$est.noadj



######### RUN THE CODE BELOW IN R. R-STUDIO IS THE RECOMMENDED IDE. BOTH R AND R-STUDIO ARE FREE.
######### QUESTIONS SHOULD BE POSTED TO PIAZZA
######### THE ACTUAL ASSIGNMENT BEGINS ON LINE 71


### Multilateral Development Institution Data
foo <- read.csv("https://tinyurl.com/yb4phxx8") # read in the data

# column names
names(foo)

# dimensions of the data set
dim(foo)

# quick look at the data structure
head(foo)

# one thing to be very careful with (in this data set) is the use of dates. 8 columns involve dates.

# take note of the columns representing calendar dates
date.columns <- c(11, 12, 14, 15, 16, 17, 18, 25)

# these columns need some tweaking--I want to address missing values, calling the blank (empty) 
# elements "NA" instead of leaving them blank, and I wish to tell R these are "Date" objects.

for(i in date.columns)  # this "for loop" only loops through the "date.columns" -- no other columns.
  
{
  
  # identify which values are missing in the "i"th column of the foo data set
  which_values_are_missing <- which(as.character(foo[, i]) == "")
  
  # those values that are missing (blank) in the "i"th column are replaced by <NA>
  # because R knows how to handle "NA" -- NA means something special in R--blanks are handled 
  # more unpredictably (which is bad).
  foo[which_values_are_missing, i] <- NA
  
  # last step--replace each of these columns (which is structured as a column of "factor" values)
  # as a column of dates--i.e., convert them to an object of "class" = Date. They are dates, after all.
  # And if you convert them to the Date class, R will know they are dates and you can manipulate 
  # dates in a simple, straightforward way. Otherwise, you won't be able to easily manipulate them
  # arithmetically.  E.g., for simple Date operations, see lines 48-58 below...
  # **By the way, if you don't understand what a "factor" is in R, you should Google it.** 
  foo[, i] <- as.Date(as.character(foo[, i]))
  
}

# Now R knows that these columns are comprised of dates
# for example...  Replicate this yourself...

# foo[3,12]
# [1] "1968-03-13"

# foo[4,12]
# [1] "1968-07-03"

# foo[3,12] - foo[4,12]
# Time difference of -112 days

# Also, one additional helpful hint... How to eliminate rows with NAs...
# The "is.na" function--for more info, Google it or type ?is.na at the R command prompt in the console.
which.have.NAs <- which(is.na(foo$Rating == TRUE)) # for which rows is the claim "is.na" a TRUE claim?

# Then, if you wanted to, e.g., remove all those rows, retaining only the rows with ratings...
new_foo <- foo[-which.have.NAs, ]
# Notice I called this tweaked data set "new_foo" instead of rewriting over the original data set...
# It's a bit safer to do this, in case I decide I want to quickly revert back to the original data set.

###########################################################################

### ASSIGNMENT 1 -- You may want to read ALL the questions before you begin. 
### NOTE: FOR ALL QUESTIONS BELOW, ONLY CONSIDER PROJECTS WITH 
### non-missing "Circulation.Date" >= 2008-01-01. 

# using the is.na function to filter which row has the Circulation.Date as NA values
which.have.NAs <- which(is.na(foo$CirculationDate == TRUE))

# remove the rows with Circulation.Data is NA, which are which.have.NAs
data <-  foo[-which.have.NAs, ]

# using which() to identify which rows have Circulation.Date >= 2008-01-01
which.after_2008 <- which(data$CirculationDate >= as.Date("2008-01-01"))

# select only the rows with Circulation.Data >= 2008-01-01, which are which.after_2008
data <-  data[which.after_2008, ]


### EXCLUDE ALL OTHER PROJECTS FROM YOUR ANALYSIS.
### YOU MUST provide a link to your R code. ------ DON'T FORGET TO DO THIS!!!!!!!!!!!!
# Take note of the column names: i.e., you can type: names(foo)
# fyi: the column called "Rating" is the success rating at completion. 0 = lowest, 3 = highest.

# (1) When projects are approved, they are approved for a certain period of time (until the time of
# "original completion date"). While projects are active, this "original" completion date is 
# often pushed out (extended), and then there is a "revised" completion date.

# You have been told that project duration at approval is generally about 
# 2 years (24 months). In other words, (purportedly) when projects are approved, the difference 
# between the original project completion date and the the approval date is (supposedly) 
# approximately 24 months. 

# (a) Is this claim true? Explain. (Remember, for this ENTIRE assignment, only consider 
# projects with Circulation.Date >= 2008-01-01. This will be your only reminder...)

# the data_1 is those requires the rows those data$ApprovalDate doesn't have NA and 
#   the rows those data$OriginalCompletionDate doesn't have NA and 
data_1 <- data[-union(which(is.na(data$ApprovalDate == TRUE)), which(is.na(data$OriginalCompletionDate == TRUE ))), ]

# we find the days difference
difference_OCD_AD <- as.numeric(data_1$OriginalCompletionDate - data_1$ApprovalDate)

# find the basic statistics of the data
summary(difference_OCD_AD)
IQR(difference_OCD_AD)

# plot the histogram
plot(hist(difference_OCD_AD), col = rgb(0,0,1, 0.5), 
     main = "The Histogram of the time difference between Original Completion Date and Approval Date",
     xlab = "Time difference (days)",
     ylab = "Frequency",
     lwd = 3)
# plot the mean
abline(v = mean(difference_OCD_AD),
       col = "red",
       lwd = 5)
#plot the median
abline(v = median(difference_OCD_AD),
       col = "green",
       lwd = 5)
# plot the 720 days
abline(v = 720,
       col = "black",
       lwd = 5)
# store the data into the data frame
data_1$difference_OCD_AD = difference_OCD_AD

# Has project duration at approval changed over time (consider projects circulated earlier
# and circulated later). Be sure to discuss mean durations, median durations, and the
# interquartile range of durations (using the "quantile" function). 
# Approximate suggested length: 3-5 sentences

# filter the earliest Circulation Date
CD_low <- which(as.numeric(data_1$CirculationDate) <= quantile(as.numeric(data_1$CirculationDate), 0.2, na.rm = TRUE))

# filter the latest Circulation Date
CD_upper <- which(as.numeric(data_1$CirculationDate) >= quantile(as.numeric(data_1$CirculationDate), 0.8, na.rm = TRUE))

# Get the data
data_CD_low <- data_1[CD_low,]
data_CD_upper <- data_1[CD_upper,]

# Get the statistics of the data
summary(data_CD_low$difference_OCD_AD)
IQR(data_CD_low$difference_OCD_AD)
summary(data_CD_upper$difference_OCD_AD)
IQR(data_CD_upper$difference_OCD_AD)


# (b) How does original planned project duration differ from actual duration (if actual duration is 
# measured as the duration between "ApprovalDate" and "RevisedCompletionDate"?)  Once again, use
# means, medians, and interquartile ranges to explain your results. 
# Approximate suggested length: 3-5 sentences

# find the date difference: actual duration
difference_RCD_AD <- as.numeric(data_1$RevisedCompletionDate - data_1$ApprovalDate)

# actual duration
data_1$difference_RCD_AD = difference_RCD_AD

# the difference between actual duration and original planned duration
data_1$difference_AD_OPD = difference_RCD_AD - difference_OCD_AD

# get the basic statistics 
summary(data_1$difference_RCD_AD)
IQR(data_1$difference_RCD_AD)

# get the basic statistics 
summary(data_1$difference_AD_OPD)
IQR(data_1$difference_AD_OPD)

# plot the histogram
plot(hist(data_1$difference_AD_OPD), col = rgb(0,0,1, 0.5), 
     main = "The Histogram of the time difference between Actual Duration and Original Planned Duration",
     xlab = "Time difference (days)",
     ylab = "Frequency",
     lwd = 3)
# the line represents the mean
abline(v = mean(data_1$difference_AD_OPD),
       col = "red",
       lwd = 5)
# the line represents the median
abline(v = median(data_1$difference_AD_OPD),
       col = "green",
       lwd = 5)



# (2) What % of projects that have ratings were rated 0?
# What % were rated 1? What % were rated 2? What % were rated 3? Answer these questions using a table
# or a figure. Provide a title AND an explanatory sentence or two that provides the numerical % results
# rounded to the nearest percentage-point.

# get the table of the data
table((data$Rating))

# transform these to proporation variable
project_rating <- prop.table(table((data$Rating)))

# transform these to percentage variable
Percent <- round(project_rating*100)
Percent

# plot the bar graph
barplot(Percent, xlab = "Rating", ylab = "Percentage (%)", 
        main = "The Bar Graph demonstrates the Percentage of the Rating values",
        col = "blue", lwd = 3)


# (3) Repeat problem 2, but this time exclude all PPTA projects. PPTA projects are more prone to 
# negative ratings, because after a certain point in time only the low-rated PPTA projects required
# ratings.  PPTA stands for "Project Preparatory Technical Assistance" and it is basically a project
# intended to set up a loan (often a very large multi-million-dollar loan). Only PPTAs that fail to 
# "eventuate" to a loan are rated, which is why they are usually rated negatively.

# filter the PPTA data
data_3 <- data[-which(as.character(data$Type) == "PPTA"),]

# get the table of the data
project_rating <- table((data_3$Rating))
project_rating

# get the percentage table of the data
Percent <- round(prop.table(project_rating)*100)
Percent

# plot the bar graph
barplot(Percent, xlab = "Rating", ylab = "Percentage (%)", 
        main = "The Bar Graph demonstrates the Percentage of the Rating values of projects exclude PPTA",
        col = "blue", lwd = 3)


# (4) Identify the top 25% of projects by "Revised.Amount" and the bottom 25% of projects by 
# "RevisedAmount". ("RevisedAmount" shows the final project budget.)
# Compare the ratings of these projects. Can you draw a causal conclusion about the effect of 
# budget size on ratings? Why or why not? 
# Hint: Compare the characteristics of the two project groupings,
# e.g., "Dept", "Division", "Cluster", "Country"
# Approximate suggested length: 3-5 sentences.

# the lower 25%
data_RS_low <- data[which(data$RevisedAmount < quantile(data$RevisedAmount, 0.25, na.rm = TRUE)),]

# the upper 25%
data_RS_upper <- data[which(data$RevisedAmount > quantile(data$RevisedAmount, 0.75, na.rm = TRUE)),]

# the mean of the lower group
mean(data_RS_low$Rating, na.rm = TRUE)

# the mean of the upper group
mean(data_RS_upper$Rating, na.rm = TRUE)

# plot the overlay bar graph of the 2 data
barplot(prop.table(table(data_RS_low$Rating)), col = rgb(1,0,0,0.5),
        xlab = "Rating",
        ylab = "The proportion of the number of projects",
        main = "The Overlapping Bar Graph demonstrates the proportion of the Rating 
        values of projects in the upper and lower quartile of the Revise Amount")
barplot(prop.table(table(data_RS_upper$Rating)), col = rgb(0,0,1,0.5), add = T, lwd = 3)

# create the legend
legend(3.7,0.7, bty = "n", title = 'Legend', legend = c('Bottom 25%', 'Top 25%'),
       fill = c(rgb(1,0,0,0.5), rgb(0,0,1,0.5)))

# find the percentage table
round(prop.table(table(data_RS_low$Rating))*100)
round(prop.table(table(data_RS_upper$Rating))*100)

# (5) Imagine your manager asks you to apply Jeremy Howard's drivetrain model to the 
# problem of optimal budget-setting to maximize project success (i.e., "Rating"). 
# In such a situation, what would be the:
# (a) decision problem or objective?
# (b) lever or levers?
# (c) ideal RCT design?

# get the 1st and 3rd quartile data
summary(data$RevisedAmount)
IQR(data$RevisedAmount)

# (d) dependent variable(s) and independent variable(s) in the modeler
# (e) And---Why would running RCTs and modeling/optimizing over RCT results be preferable 
# to using (observational, non-RCT) "foo" data?
# Approximate suggested length: 1-3 sentences for each sub-question.

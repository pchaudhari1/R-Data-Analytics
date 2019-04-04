# San Francisco Crime Classification

rm(list=ls())

# Importing Library
library(rpart)
library(rpart.plot)
library(rattle)
library(RColorBrewer)
library(ggplot2)
install.packages("readr")
install.packages("lubridate")
library(readr)
install.packages("reshape")
library(reshape)

# DataSet
train <- read.csv("C:\\Users\\Pratik\\Desktop\\R working directory\\train.csv", header=TRUE)
test <- read.csv("C:\\Users\\Pratik\\Desktop\\R working directory\\test.csv", header = TRUE)
train$Date2 <- as.numeric(train$Dates)
test$Date2 <- as.numeric(test$Dates)
View(train)
View(test)

# Data Visualization

# To see Number of Incident occured per Category
category <- as.data.frame(table(train$Category))
names(category) <- c("Category", "Frequency")
attach(category)
ndcategory <- category[order(-Frequency),]
barplot(ndcategory$Frequency, names.arg=ndcategory$Category, las=2, cex.names=0.8,
        main="Number of incidents /  Category",
        ylim=c(0,2500))

# To see Number of Incident occured per Day of Week
day_of_week <- as.data.frame(table(train$DayOfWeek))
names(day_of_week) <- c("DayOfWeek", "Frequency")
attach(day_of_week)
ndweekday <- day_of_week[order(-Frequency),]
barplot(ndweekday$Frequency, names.arg=ndweekday$DayOfWeek, las=2, cex.names=0.8,
        main="Number of incidents / weekday",
        ylim=c(0,2000))

# To see Number of Incident occured per Police Department District
pddistrict <- as.data.frame(table(train$PdDistrict))
names(pddistrict) <- c("PdDistrict", "Frequency")
attach(pddistrict)
ndpddistrict <- pddistrict[order(-Frequency),]
barplot(ndpddistrict$Frequency, names.arg=ndpddistrict$PdDistrict, las=2, cex.names=0.8,
        main="Number of incidents / PdDistrict",
        ylim=c(0,2000))


# Source  - http://docs.ggplot2.org/0.9.3.1/geom_bar.html/

# To see count of Incident by Category in DayOfWeek

crimes_by_day <- table(train$Category,train$DayOfWeek)
crimes_by_day <- melt(crimes_by_day)
names(crimes_by_day) <- c("Category","DayOfWeek","Count")
g_by_day <- ggplot(crimes_by_day,aes(x=Category, y=Count,fill = Category)) + 
  geom_bar(stat = "Identity") + 
  coord_flip() +
  facet_grid(.~DayOfWeek) +
  theme(legend.position = "none")

g_by_day


# To see count of Incident by Category for PDDistrict

crimes_by_district <- table(train$Category,train$PdDistrict)


names(crimes_by_district) <- c("Category","PdDistrict","Count")


g_by_district


# BoxPLot

summary(train) # This gives min, max, median, first quartile and third quartile
library(ggplot2)
boxP_x <- boxplot(X) # Box Plot representation for X coordinate
boxP_y <- boxplot(Y) # Box Plot representation for Y coordinate
boxP_date <- boxplot(Date2)  # Box Plot representation for Date2
boxP_data <- boxplot(train) # Box plot representation for all
hist(X) # Histogram representation for X coordinate
hist(Y) # Histogram representation for Y coordinate
hist(Date2)
boxP_x # Outliers for X coordinate
boxP_y # Outliers for Y coordinate
boxP_date # Outliers for Date2


# Decision Tree from Rattle Log

# Build the training/validate/test datasets.



crs$input <- c("Dates", "X", "Y")

crs$numeric <- c("X", "Y")

crs$categoric <- "Dates"

crs$target  <- "Category"
crs$risk    <- NULL
crs$ident   <- NULL
crs$ignore  <- c("Descript", "DayOfWeek", "PdDistrict", "Resolution", "Address", "Date2")
crs$weights <- NULL

#============================================================
# Rattle timestamp: 2015-12-10 15:19:02 x86_64-w64-mingw32 

# Decision Tree 

rattle()

# The 'rpart' package provides the 'rpart' function.

library(rpart, quietly=TRUE)

# Reset the random number seed to obtain the same results each time.

set.seed(crv$seed)

# Build the Decision Tree model.

crs$rpart <- rpart(Category ~ .,
                   data=crs$dataset[crs$train, c(crs$input, crs$target)],
                   method="anova",
                   parms=list(split="information"),
                   control=rpart.control(minbucket=10,
                                         usesurrogate=0, 
                                         maxsurrogate=0))

# Generate a textual view of the Decision Tree model.

print(crs$rpart)
printcp(crs$rpart)
cat("\n")

# Time taken: 0.77 secs

#============================================================
# Rattle timestamp: 2015-12-10 15:19:05 x86_64-w64-mingw32 

# Plot the resulting Decision Tree. 

# We use the rpart.plot package.

fancyRpartPlot(crs$rpart, main="Decision Tree train $ Category")

# List the rules from the tree using a Rattle support function.

asRules(crs$rpart)


# Decision Tree in DECISIONTREE.R Attached
 
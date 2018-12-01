# Josemari Feliciano
# HW 8




# Declares a changeable wd and sets working directory 
wd<-"/Users/jfeliciano/Documents/advancedr/homework/Assignment8"
setwd(wd) 


# it checks if dplyr is installed via require, if not, install it before loading.
if(!require("dplyr")){
  install.packages("dplyr")
}
library("dplyr")


# Loads the file required for this homework
demo_data <- read.csv("Demographics.csv", header = TRUE)
test_master <- read.csv("TestScores_original.csv", header = TRUE)
test_makeup <- read.csv("TestScores_makeup.csv", header = TRUE)


# removes all the NA test scores -- this will prevent collision when merging combined data into wide format later
# this works for this case because students have taken at least 1 exam, so 100 students will be kept when deleting NAs
test_master <- test_master[!is.na(test_master$TestScore),]
test_master <- rbind(test_master, test_makeup)

# NOTE TO Dr. ESSERMAN: if you require something more robust than this NA trick, commented out right below is another way
# another way is here that works more robustly -- for loop
# For-loop goes row by row, repl
# for(row1 in 1:nrow(test_master)) {
#   for(row2 in 1:nrow(test_makeup)) {
#     if(test_makeup[row2,]$StudentID == test_master[row1,]$StudentID && test_makeup[row2,]$TestNumber == test_master[row1,]$TestNumber) {
#       test_master[row1,]$TestScore = test_makeup[row2,]$TestScore
#       break # exits inner for-loop since we already found it in the makeup file
#     }
#   }
# }


# Creates 'finaldata' df with the newly created wide-formatted dataset 
finaldata <- reshape(test_master, timevar="TestNumber", idvar = "StudentID", v.names = "TestScore", direction="wide")


# Merges demo data into wide data by using StudentID
finaldata <- merge(demo_data, finaldata, by="StudentID", all=TRUE)


# my code replaces NAs with 0 ... goes over columns 1:5 which corresponds to test 1 - test 5 range
for(col in 1:5){
  # uses the eval-parse-text trick for function/method calling for my own practice here, I could have made 5 separate calls or used sapply
  eval(parse(text= paste0("finaldata$TestScore.",col,"[is.na(finaldata$TestScore.",col,")] <- 0") ))
} 


#finds the lowest from tests 2 - 4, then uses that info to calculate the eventual final grade
finaldata$lowest24 <- apply(finaldata[c("TestScore.2", "TestScore.3", "TestScore.4")], 1, FUN = min) 
finaldata$FinalGrade <- round(0.20*(finaldata$TestScore.2+finaldata$TestScore.3+finaldata$TestScore.4-finaldata$lowest24)+0.30*finaldata$TestScore.1+0.30*finaldata$TestScore.5,2)


# calculates class average, classdiff, majordiff, yeardiff inside the dataset
finaldata$ClassAverage <- mean(finaldata$FinalGrade)
finaldata <- finaldata %>% 
  mutate(ClassAverage = mean(FinalGrade), ClassDiff = round(FinalGrade - ClassAverage,2)) %>%
  group_by(Major) %>%  
  mutate(MajorAverage = mean(FinalGrade), MajorDiff = round(FinalGrade-MajorAverage,2)) %>%
  group_by(Year) %>%
  mutate(YearAverage = mean(FinalGrade), YearDiff = round(FinalGrade-YearAverage,2)) 


# only keeps the variables required for the homework
finaldata <- select(finaldata, c("StudentID", "Year", "Major", "FinalGrade", "ClassDiff", "YearDiff", "MajorDiff"))


# labels the categorical variables
finaldata$Year <- factor(finaldata$Year, levels = c(1:5), labels = c("Freshman", "Sophomore", "Junior", "Senior", "Senior Plus"))
finaldata$Major <- factor(finaldata$Major, levels = c(1:6), labels = c("Chemistry", "Biology", "Mathematics", "Physics", "Psychology", "Other"))


# *******************************************************
# *******************REQUESTED OUTPUT********************
# *******************************************************c

# prints first 10 data which is already ordered by ID
head(finaldata, n=10)

# creates histogram of final grade
hist(finaldata$FinalGrade, xlab="Final Grade for Class (in %)", labels= TRUE, main="Final Grade Distribution of All Students", xlim=c(0,100), ylim=c(0,40))

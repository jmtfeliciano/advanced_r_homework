# Josemari Feliciano
# HW 8

# Declares a changeable wd and sets working directory 
wd<-"/Users/jfeliciano/Documents/biostats/bis679/advanced_r_homework/Assignment8"
setwd(wd) 

require("dplyr")

# Loads the file required for this homework
demo_data <- read.csv("Demographics.csv", header = TRUE)
test_master <- read.csv("TestScores_original.csv", header = TRUE)
test_makeup <- read.csv("TestScores_makeup.csv", header = TRUE)
testt <- read.csv("datacomp.csv", header = TRUE)

# method 1: removes all the NA test scores -- this will prevent collision when merging combined data into wide format later
test_master <- test_master[!is.na(test_master$TestScore),]
test_master <- rbind(test_master, test_makeup)

# another way -- for loop
# For-loop goes row by row, repl
# for(row1 in 1:nrow(test_master)) {
#   for(row2 in 1:nrow(test_makeup)) {
#     if(test_makeup[row2,]$StudentID == test_master[row1,]$StudentID && test_makeup[row2,]$TestNumber == test_master[row1,]$TestNumber) {
#       test_master[row1,]$TestNumber = test_makeup[row2,]$TestNumber
#       # for efficiency, exits out of inner for-loop to proceed to the next column in the original data set
#     }
#   }
# }

# logic:  if testscore is NA, replace with 0.  
# dplyr alternative:  

#test_master[is.na(test_master$TestScore)]$TestScore <- 0


# Creates 'finaldata' df by assigning the newly wide-formatted dataset 
finaldata <- reshape(test_master, timevar="TestNumber", idvar = "StudentID", v.names = "TestScore", direction="wide")

# Uses 
finaldata <- finaldata %>%
  mutate("TestScore.2", "TestScore.3", "TestScore.4")

# Merges demo data into wide data
finaldata <- merge(demo_data, finaldata, by="StudentID", all=TRUE)


# my code that replaces NAs with 0 ...  uses the eval-parse-text trick to prevent 
for(col in 1:5){
  eval(parse(text= paste0("finaldata$TestScore.",col,"[is.na(finaldata$TestScore.",col,")] <- 0") ))
} 

finaldata$lowest24 <- apply(finaldata[c("TestScore.2", "TestScore.3", "TestScore.4")], 1, FUN = min) 



finaldata$FinalGrade <- 0.20*(finaldata$TestScore.2+finaldata$TestScore.3+finaldata$TestScore.4-finaldata$lowest24)+0.30*finaldata$TestScore.1+0.30*finaldata$TestScore.5
finaldata$ClassAverage <- mean(finaldata$FinalGrade)
finaldata <- finaldata %>% mutate(mutate_mean = mean(FinalGrade), diff_mean = mutate_mean - ClassAverage)




finaldata$ClassDiff <- finaldata$FinalGrade - finaldata$ClassAverage

finaldata <- finaldata %>% 
  group_by(Major) %>%  
  mutate(MajorAverage = mean(FinalGrade), MajorDiff = FinalGrade-MajorAverage) %>%
  group_by(Year) %>%
  mutate(YearAverage = mean(FinalGrade), YearDiff = FinalGrade-YearAverage) 

finaldata <- select(finaldata, c("StudentID", "Year", "Major", "FinalGrade", "ClassDiff", "YearDiff", "MajorDiff"))

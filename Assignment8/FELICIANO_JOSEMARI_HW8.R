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

# method 1: removes all the NA test scores -- this will prevent collision when merging combined data into wide format later
# test_original <- test_original[!is.na(test_original$TestScore),]
# row_stacked_data <- rbind(test_original, test_makeup)

# another way -- for loop
# For-loop goes row by row, repl
for(row1 in 1:nrow(test_master)) {
  for(row2 in 1:nrow(test_makeup)) {
    if(test_makeup[row2,]$StudentID == test_master[row1,]$StudentID && test_makeup[row2,]$TestNumber == test_master[row1,]$TestNumber) {
      test_master[row1,]$TestNumber = test_makeup[row2,]$TestNumber
      break # for efficiency, exits out of inner for-loop to proceed to the next column in the original data set 
    }
  }
}c

# logic:  if testscore is NA, replace with 0.  
# dplyr alternative:  
test_master[is.na(test_master$TestScore),]$TestScore <- 0


# Creates 'finaldata' df by assigning the newly wide-formatted dataset 
finaldata <- reshape(test_master, timevar="TestNumber", idvar = "StudentID", v.names = "TestScore", direction="wide")

# Merges demo data into wide data
test <- merge(demo_data, finaldata, by="StudentID", all=TRUE)

finaldata <- finaldata %>%
  rowwise() %>% 
  #added rowwise() code from dplyr to restrict my min() inside mutate() for the current row it's in for variables, 
  #reason:  min() when inside mutate() would normally find the min for each variable column then compare each column min to each other
  #so rowwise() prevents complications when using summary statistics or similar methods inside mutate()
  mutate(final_grade = 0.40*(TestScore.2+TestScore.3+TestScore.4-min(TestScore.2, TestScore.3, TestScore.4))/2+0.30*TestScore.1+0.30*TestScore.5) 


# Josemari Feliciano
# HW 7

# Creates path and set current directory
wd<-"/Users/jfeliciano/Documents/advancedr/homework/Assignment7"
setwd(wd) 

# loads csv file
loaded_data <- read.csv("AdoptionData.csv", header=TRUE)

# vector with splitted list of variables requested by PDF HW6 prompt 
desired_variables <- strsplit(split = ", " , x="uniqueid, COUNTRY, LIVING, ADOPTAGE, AGETST1, WTTST1, HTTST1, INDTST1, RESTST1, CXRDONE, GENDER" )[[1]]

# subsets my data using select, selects data variables requested by hw prompt
# note to self: the dplyr way is select(loaded_data, desired_variables)
subset_data <- loaded_data[desired_variables]

# filters data if uniqueid is non-empty
# note to self:  the dplyr way is filter(subset_data, !is.na(uniqueid))
analysis <- subset_data[!is.na(subset_data$uniqueid),]

# do nested for-loops to traverse all rows and columns, and replace 99 values to NA
for(row in 1:nrow(analysis)) {
  for(col in 1:ncol(analysis)) {
    if(!is.na(analysis[row,col]) && analysis[row,col] == 99) {
      analysis[row,col] <- NA
    }
  }
}

# creates levels and labels for the following varables:
# COUNTRY, LIVING, ADOPTAGE, AGETST1, WTTST1, HTTST1, INDTST1, RESTST1, CXRDONE, GENDER
analysis$COUNTRY <- factor(analysis$COUNTRY, levels = c(1:23),
                                labels = c("China", "Russia", "Guatemala", "South Korea", "India", "Ukraine", "Thailand", 
                                           "Ethiopia", "Phillipines", "Haiti", "Kenya", "Kazhakstan", "Singapore", "Bulgaria", 
                                           "Romania", "Hawaii", "Belarus", "Cambodia", "Georgia", "Columbia", "Vietnam", 
                                           "Siberia", "Moldavia"))
analysis$LIVING <- factor(analysis$LIVING, levels = c(1:5), 
                               labels = c("Orphanage only", "Foster care only", "Both orphanage and foster care", 
                                          "Other", "Unknown"))
analysis$RESTST1 <- factor(analysis$RESTST1, levels =c(1,2), labels = c("Positive","Negative"))
analysis$CXRDONE <- factor(analysis$CXRDONE, levels = c(0,1,88), labels = c("No", "Yes", "CXR not required"))
analysis$GENDER <- factor(analysis$GENDER, levels = c(0,1), labels = c("female","male"))



# prints tables from frequency for both gender and cxrdone
table(analysis$GENDER)
table(analysis$CXRDONE)

# calculates BMI using WTTST1, HTTST1 variables, then calculates mean BMI
analysis$BMI <- (analysis$WTTST1/(analysis$HTTST1**2))*10000
mean(as.numeric(analysis$BMI), na.rm = TRUE)

                        
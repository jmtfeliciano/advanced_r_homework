# Josemari Feliciano
# HW 7

# Creates path and set current directory
wd<-"/Users/jfeliciano/Documents/advancedr/homework/Assignment7"
setwd(wd) 

# Installs and initializes dplyr library, it has select function that I will use later
install.packages("dplyr") 
library(dplyr)

# loads csv file
loaded_data <- read.csv("AdoptionData.csv", header=TRUE)

# the list of variables copied from PDF HW6 prompt in one line
desired_variables <- strsplit(split = ", " , x="uniqueid, COUNTRY, LIVING, ADOPTAGE, AGETST1, WTTST1, HTTST1, INDTST1, RESTST1, CXRDONE, GENDER" , ", ")[[1]]

# subsets my data using select, selects data variables requested by hw prompt
subset_data <- select(loaded_data, desired_variables)

# filters data if uniqueid is non-empty
filtered_data <- filter(subset_data, !is.na(uniqueid))


for(row in 1:nrow(filtered_data)) {
  for(col in 1:ncol(filtered_data)) {
    if(!is.na(filtered_data[row,col]) && filtered_data[row,col] == 99) {
      filtered_data[row,col] <- NA
    }
  }
}

# note to self:  factors tell me the order is 0,1 and want to assign female to 0 and male to 1


attributes(filtered_data$GENDER)$labels<-c("Female", "Male")


hist(filtered_data$GENDER)

                        
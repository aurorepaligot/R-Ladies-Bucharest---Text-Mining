#------------------------------------------------------
# Part of InstaCrawlR
# GitHub: https://github.com/JonasSchroeder/InstaCrawlR
# Code by Jonas Schröder
# See ReadME for instructions and examples
#
# adapted by Aurore Paligot - R Ladies Bucharest 20-10-2020
#------------------------------------------------------

library(stringr)
library(readxl)
library(dplyr)
library(WriteXLS)

#Import Table and Extract Hashtags
text <- list()
htemp <- list()
htags <- data.frame()
data <- read_excel("Data/Amsterdam_Cleaned.xlsx") #locate your data

#Run the Hashtag Extractor

data <- as.matrix(data[-1])

maxrows <- nrow(data)
for(i in 1:maxrows){
  text[i] <- as.character(data[i,2]) #the column number is hard coded
  htemp <- str_extract_all(text[i], "#\\S+", TRUE)
  
  if(ncol(htemp) != 0){
    for(j in 1:ncol(htemp)){
      htags[i,j] <- htemp[1,j]
    }  
  }
} 

#Save Hashtags as csv for Excel

df_htags <- as.data.frame(table(unlist(htags)))

write.csv(df_htags, "Amsterdam_sort.csv") #sorted list with frequencies


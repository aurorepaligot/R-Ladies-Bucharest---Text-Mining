########################################################################################

##### Text Mining with R - Social Media Mining during the COVID-19 crisis ##############

##### R Ladies Bucharest Workshop organized by Ines Teaca                 ##############

##### Speaker: Aurore Paligot                                             ##############

##### Data  : January - May 2020 | Amsterdam | Instagram                  ##############

##### Corpus & Material Creator : Aurore Paligot                          ##############

########################################################################################

library(readxl)
library(ggplot2)
library(ggthemes)
library(dplyr)
library(stringr)
library(tidyr)

######################### STEP 2 : Hashtags Frequency ##################################

data <- read_excel("Data/Amsterdam_Frequencies.xlsx")

#1. explore & format data

summary(data)

data$Corona <- as.factor(data$Corona)
data$Hashtags <- as.factor(data$Hashtags)

#2. Plot most frequent hashtags

graph <- ggplot(data = data[1:20,], #choose the number of tags
            aes(x=Hashtags, y = Frequency)) + 
            geom_bar(stat='identity')

graph 


#3. Order the hashtags by increasing or decreasing order
data$Hashtags <- factor(data$Hashtags, 
                        levels = data$Hashtags[order(data$Frequency)]) #change order

graph2 <- ggplot(data=data[1:15,], #choose the number of tags
                 aes(x=Hashtags, y=Frequency)) +
                  geom_bar(stat="identity") + 
                  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + #label orientation
                  theme_pander() #new theme (package ggthemes)

graph2 + coord_flip()

#4. filter & visualize "corona" hashtags

cdata<-  filter(data, Corona == "yes")

graph_corona <- ggplot(data=cdata[1:15,], 
                      aes(x=Hashtags, y=Frequency)) +
                      geom_bar(stat="identity") + 
                      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + 
                      theme_pander() 
              
graph_corona + coord_flip()

#5. Compare the proportion of corona vs other hashtags

# Bar plot
bp <- ggplot(data, aes(x="", y=Frequency, fill=Corona))+
             geom_bar(width = 1, stat = "identity") 
bp

#Pie chart
pie <- bp + coord_polar("y", start=0)
pie

######################### STEP 3 : Hashtags Evolution ##################################

adata <- read_excel("Data/Amsterdam_Cleaned.xlsx")

summary(adata)

#1. Identify captions that contain the hashtag or word corona

corona_vector <- stringr::str_detect(adata$captions, "corona")

summary(corona_vector)

#add this new column to the data frame

adata$corona = corona_vector

#create a subset with the corona data
adata_corona <- filter(adata, corona == TRUE)

#group by date and count the number of instagram post per day
coun_data_corona<- adata_corona %>% 
  mutate(Date = as.Date(date_time)) %>% 
  count(Date1 = as.Date(Date)) %>%
  group_by(Date1) %>% 
  complete(Date1, fill = list(n = 0))

#visualize
graph_ev_cor <- ggplot(coun_data_corona, aes(Date1,n))+ 
  geom_line() + 
  theme_pander()

graph_ev_cor 

#2. Repeat with another hashtag or word

stayhome_vector <- stringr::str_detect(adata$captions, "#stayhome")

summary(stayhome_vector)

#add this new column to the data frame

adata$stayhome = stayhome_vector

#create a subset with the corona data
adata_stayhome <- filter(adata, stayhome == TRUE)

#group by date and count the number of instagram post per day
coun_data_stayhome <- adata_stayhome %>% 
  mutate(Date = as.Date(date_time)) %>% 
  count(Date1 = as.Date(Date)) %>%
  group_by(Date1) %>% 
  complete(Date1, fill = list(n = 0))

#visualize
graph_ev_sh <- ggplot(coun_data_stayhome, aes(Date1,n))+ 
  geom_line() + 
  theme_pander()

graph_ev_sh


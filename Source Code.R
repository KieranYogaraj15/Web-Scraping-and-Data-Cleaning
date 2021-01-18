#Brief explanations are given as short comments 
#above each line of code. 

### Part 1 ###
# Q1 #
library(rvest)
library(tidyverse)

#url of Endangered sites Wikipedia website.
url <- "https://en.wikipedia.org/wiki/List_of_World_Heritage_in_Danger"

#Extracting all the data from the website.
url_data <- read_html(url)


# Q2 #
#Extract dl elements.
dl_elements <- url_data %>% html_nodes("dl")

#Scrap Legend Title and Elements.
title <- dl_elements[1] %>% html_text()
elements <- dl_elements[2] %>% html_text()

#Clean the elements.
elements <- strsplit(elements, "\n")

#Store in dataframe.
table_legend_df <- data.frame(element = elements, stringsAsFactors=FALSE)
colnames(table_legend_df) <- title


# Q3 #
#Scraping currently endangered table. 
current_table <- url_data %>% html_nodes("#mw-content-text > div > table:nth-child(15)") %>% html_table(fill=TRUE)

#Store the table as a dataframe. 
current_table <- data.frame(current_table, stringsAsFactors=FALSE)


# Q4 #
#Scrap the hyperlinks from the website. 
hyperlinks <- url_data %>% html_nodes("a")


# Q5 #
#Finding the index of "criteria" in the hyperlinks list. 
grep("criteria", hyperlinks, ignore.case=TRUE)

#Obtained the selection criteria hyperlink
criteria_link <- hyperlinks[55]

#Convert criteria_link into a string datatype.
criteria_link <- as.character(criteria_link)

#Split the string by " ".
criteria_link <- strsplit(criteria_link, " ")

#Find the index of the element in the list that contains the hyperlink. 
link_index <- grep(pattern="href", criteria_link[[1]])

#Extract the element containing the hyperlink. 
criteria_link <- criteria_link[[1]][link_index]

#Extract the Criteria hyperlink from the element
criteria_link <- sub(pattern="href=", replacement="", criteria_link)

#Storing each character in the string as elements in a list. 
criteria_link <- strsplit(criteria_link, "")

#Remove "\". 
criteria_link <- criteria_link[[1]][-1]

#Combine all elements in the list together. 
criteria_link <- paste(criteria_link, collapse="")


# Q6 #
#Extracting first half of the hyperlink.
url_a <- strsplit(url, "")
url_a <- url_a[[1]][c(1:24)]
url_a <- paste(url_a, collapse="")

#Combining hyperlinks together. 
url <- paste(url_a, criteria_link, collapse="")

#Removing the whitespace in the hyperlink. 
url <- sub(pattern=" ", replacement="", url)

#Extracting cultural and natural lists. 
ordered_elements <- read_html(url) %>% html_nodes("ol")

#Scraping cultural list.
cultural_list <- ordered_elements[1] %>% html_text()

#Cleaning the cultural list.
cultural_list <- strsplit(cultural_list, "\n")

#Store cultural list into dataframe.
cultural_df <- data.frame(cultural_list, stringsAsFactors=FALSE)
colnames(cultural_df) <- "Cultural"

#Scraping natural list.
natural_list <- ordered_elements[2] %>% html_text()

#Cleaning the natural list.
natural_list <- strsplit(natural_list, "\n")

#Store natural list into dataframe.
natural_df <- data.frame(natural_list, stringsAsFactors=FALSE)
colnames(natural_df) <- "Natural"

#Store both dataframes into a list. 
selection_criteria_list <- list(cultural_df,natural_df)



### Part 2 ###
# Q1 #
#Delete Image and Refs variables from the dataframe. 
current_table <- current_table %>% select(-c("Image","Refs"))


# Q2 #
#Separate Location variable into two variables. 
current_table <- current_table %>% separate(col=Location, into=c("Place","Coordinates"), sep="°")

#Creating a new variable country to be used in for loop.
Country <- c()

#If element in Place variable has "," then split the element. Otherwise don't split the element.
for(i in 1:nrow(current_table)){
  if(grepl(",", current_table$Place[i])){ 
    ls <- strsplit(current_table$Place[i], ",")
    Country <- rbind(Country, ls[[1]][2])
  }else{
    Country <- rbind(Country, current_table$Place[i])
  }
}

#Add the Country variable into the dataframe.
current_table$Country <- Country

#Removing numbers from the elements in the Country variable. 
current_table$Country <- gsub("[0-9]+", "", current_table$Country)

#Removing some variables.
current_table <- current_table %>% select(-c("Place","Coordinates"))

#Separate [] from the elements in Country variable.
current_table <- current_table %>% separate(col=Country, into=c("Country","Key"), sep="\\[")

#Delete Key variable.
current_table$Key <- NULL


# Q3 #
#Separate Name and Roman number in Criteria variable by ":". 
current_table <- current_table %>% separate(col=Criteria, into=c("Type","Criteria"), sep=":")


# Q4 #
#Separate Hectares and Acres in Area variable by " ".
current_table <- current_table %>% separate(col=Areaha..acre., into=c("Hectares","Acres"), sep=" ")

#Remove Hectares variable
current_table$Hectares <- NULL

#Remove "()" and "," from Acres variable. 
current_table$Acres <- gsub("[[:punct:]]", "", current_table$Acres)


# Q5 #
#Remove "–" in Endangered variable. 
current_table$Endangered <- gsub(pattern = "–", replacement = "", current_table$Endangered)

#Separate variable by ",". 
current_table <- current_table %>% separate(col=Endangered, into=c("First","Last"), sep=",")

#Replace some First variable elements with Last variable elements. 
index <- which(!is.na(current_table$Last))
current_table$First[index] <- current_table$Last[index]

#Rename First variable with Endangered.
current_table$Endangered <- current_table$First

#Remove First and Last variables. 
current_table <- current_table %>% select(-c("First", "Last"))


# Q6 #
#Converting number variables into numeric datatype.
current_table$Acres <- as.numeric(current_table$Acres)
current_table$Year..WHS. <- as.numeric(current_table$Year..WHS.)
current_table$Endangered <- as.numeric(current_table$Endangered)

#The remaining variables are character datatypes. 



### Part 3 ###
# Q1 #
#Counting the total number of cultural and natural sites. 
current_table %>% group_by(Type) %>% summarise(Counts = n())

#Cultural is the most common type of site in the endangered list. 
#There are 36 cultural type of sites and 17 natural type of sites. 


# Q2 #
#Site with largest area.
current_table$Name[which.max(current_table$Acres)]
#Air and Ténéré Natural Reserves is the largest site. 

#Site with smallest area.
current_table$Name[which.min(current_table$Acres)]
#Crac des Chevaliers and Qal’at Salah El-Din.


# Q3 #
#A histogram that shows the frequency of the years when sites were place on the endangered list. 
ggplot(current_table, aes(x=Endangered)) +
  geom_histogram() +
  xlab("Years") +
  ylab("Frequency") +
  ggtitle("Frequency of the years the sites were put on the Endangered list")


# Q4 #
#In descending order of the number of sites each country has. 
current_table %>% group_by(Country) %>% summarise(Count = n()) %>% arrange(desc(Count))
#Syria, Democratic Republic of the Congo and Libya are the countries with the most sites. 


# Q5 #
#How long for sites to be placed in endangered list. 
current_table$Time_Took <- current_table$Endangered - current_table$Year..WHS.


# Q6 #
#A histogram that shows the frequency of the time for sites to be placed on the endangered list. 
ggplot(current_table, aes(x=Time_Took)) +
  geom_histogram() +
  xlab("Years") +
  ylab("Frequency") +
  ggtitle("Frequency of the time it took for the sites to be placed on the Endangered list")



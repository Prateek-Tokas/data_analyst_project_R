#-DATA PREPERATION

# Import the "athlete_events.csv" file into the R studio and assign it to athletes. 
#       Check the imported data using head() function. View the summary statistics for the variables. 

athlete_events <- read.csv("C:/Users/prate/Downloads/athlete_events.csv")
athlete_events
head(athlete_events)
summary(athlete_events)
#  The year variable is a categorical variable. So, convert the year variable to factor values. 
str(athlete_events)
athlete_events$Year <-  as.factor(athlete_events$Year)

# Remove all the NA values in the Medal column. (Hint: != “”). Do not remove the NA values from other columns in this dataset. Highly unrecommended. 
# Remove NA values from the 'Medal' column without affecting other columns

athlete_events <- athlete_events[athlete_events$Medal != "", ]

#-TASKS

#1. Make a subset for Team USA in the dataset and assign it to USA.

USA <- subset(athlete_events, NOC=="USA") 
USA

#2. 2.In the USA dataset, group by the “sport” variable and summarize the total medals of each sport. And arrange them in the descending order based on the number of medals. (Hint: To count the total number of records under each sport)
library(tidyverse)

 # Group by 'Sport' and summarize the total medals for each sport
USA_summary <- USA %>%
  group_by(Sport) %>%
  summarise(Total_Medals = n())
 # Arrange in descending order based on the number of medals
USA_summary <- USA_summary %>% arrange(desc(Total_Medals))
 # View the summarized data
print(USA_summary)
summary(USA_summary)
length(USA_summary)
View(USA_summary)

#3. 3.In the USA dataset, let’s focus on swimming. Create a variable “usa_swim”, Make a subset for the sport swim. 

usa_swim <- subset(USA, Sport == "Swimming")
 #Vieww summarized data 
View(usa_swim)

#4. List the top 10 medal winning countries using top_n() function.

 # Filter for rows with a medal in the dataset
countries_with_medals <- athlete_events %>%
  filter(!is.na(Medal) & Medal != "") # Filter out rows with NA or no Medal

 # Group by country and calculate the total number of medals
medal_counts <- countries_with_medals %>%
  group_by(NOC) %>%
  summarise(Total_Medals = n()) # Count the number of medals per country
 
# Retrieve the top 10 medal-winning countries, highest medal count first
top_10_countries <- medal_counts %>%
  top_n(10, wt = Total_Medals) %>%  # Get the top 10 based on total medals
  arrange(desc(Total_Medals))  # Arrange in descending order of medal count

# Display the top 10 medal-winning countrie
print(top_10_countries)
View(top_10_countries)

#5. 5.Show the medal count of gold by team, display them in the descending order. 

 # Filter the dataset for rows with gold medals
gold_count <- athlete_events %>%
  filter(Medal == "Gold") %>%
  group_by(Team) %>%
  summarise(Gold_Medal_Count = n()) %>%
  arrange(desc(Gold_Medal_Count))

# Display the medal count of gold by team in descending order
print(gold_count)
View(gold_count)

#6. Calculate Medals won by women in the year 2016

 # Filter the dataset for rows with 'Sex' as 'Female' and 'Year' as '2016' and count the medals
medals_won_by_women_2016 <- athlete_events %>%
  filter(Sex == "F" & Year == 2016) %>%
  summarise(Total_Medals = n())

 # Display the total number of medals won by women in 2016
print(medals_won_by_women_2016)

























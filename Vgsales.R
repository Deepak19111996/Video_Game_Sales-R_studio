###I am choosing R studio to find the insights for a given data, will explain at every step what I am doing######

###setting working directory
setwd("C:/Users/ddeep/OneDrive/Desktop/Business Analyst course/Open Projects/Ozibook Project")

### calling usefull libraries
library(readr)
library(dplyr)
library(plyr)
library(ggplot2)
library(Metrics)
library(statip)
library(tidyverse)
library(tidyr)

###Importing the file to R studio
data = read_csv("vgsales.csv")

####lets understand the data

head(data)
view(data)
str(data)
summary(data)
dim(data)
table(data$Genre)
table(data$Platform)

###Cleaning data
##changing the data type

data$Year= as.numeric(data$Year)
data$Platform = as.factor(data$Platform)
data$Genre = as.factor(data$Genre)


###lets check is there any NA values
colSums(is.na(data))

###we have NA values in column 'year' of 271 which is less than 5% of total entries 
##handling with NA values

data= na.omit(data)

###data looks good to go
### checking the market share in percentage of genre 

Genre = data |> group_by(Genre) |> summarise(count = n()) |> mutate(Percentage = round(count/sum(count)*100,2)) |> arrange(desc(count))
Genre


####lets check the top three Genre
head(Genre, 4)

#####@@@@@'Action' with 19.9%, 'Sports' with 14.1% 'Role playing' with 9.01% are top three Genre@@@@#######

#lets look at the market distribuytion of genre on graph
ggplot((Genre), aes(x=Genre, y= count))+ geom_col (fill = 'blue')+ labs(title= "Genre Dsitribution", x= "Genre", y= "Count")


##Lets look at the total gobal sales w.r.t Genre

Genre_sale = data |> group_by(Genre) |> summarise(Totalsales = sum(Global_Sales))|> mutate(percentage_sale = Totalsales/sum(Totalsales)*100) |> arrange(desc(percentage_sale))
Genre_sale

####Top 3 genre with maximum global sales and their percenatge are -
head(Genre_sale, 3)

###Lets look this pragphically
ggplot(Genre_sale, aes(x= Genre, y = Totalsales))+ geom_col (fill = 'Orange')+ labs(title= 'Global Sales by Genre', x='Genre', y = 'Total Global sales')

######lets now check the sales w.r.t game released Years
sales_years = data |> group_by(Year) |> summarise(Totalsales= sum(Global_Sales))|> arrange(desc(Totalsales))
sales_years


###maximum sales was for 2008 released games.

###lets put this in graph
ggplot(sales_years, aes(x= Year, y = Totalsales))+ geom_col(fill = 'red')+ labs(title= 'game released Year by sales', x= 'Year', y= 'Total Sales')


#####lets explore more on sales distribution w.r.t area
colnames(data)
area_sales= data |> select(NA_Sales, EU_Sales, JP_Sales, Other_Sales, Genre) |> group_by(Genre) |> pivot_longer(c("NA_Sales","EU_Sales","JP_Sales","Other_Sales" ), values_to = "sales")
area_sales
ggplot(area_sales, aes(x= Genre, y = sales, fill= name))+geom_col()+ labs(title = "Genre Sales per Region", x= "Genre", y = "Sales")
  

#####Through graph we can see the genre sales per region

###lets now calculate the region sales w.r.t years
colnames(data)
Region_sales_year= data |> select (Year, NA_Sales, EU_Sales, JP_Sales, Other_Sales, Global_Sales) |> group_by(Year) |> summarise(EUsales = sum(EU_Sales), JPsales = sum(JP_Sales), NAsales = sum(NA_Sales), Others = sum(Other_Sales), Global= sum(Global_Sales) )
Region_sales_year
ggplot(Region_sales_year) + geom_line(aes(x= Year, y= EUsales, colour = "EUsales"), size=1)+ geom_line(aes(x= Year, y= JPsales, colour = "JPsales"), size=1)+ geom_line(aes(x= Year, y= NAsales, colour = "NAsales"), size=1)+ geom_line(aes(x= Year, y= Others, colour = "Others"), size=1)+ labs(title = "Sales tred", y = "sales")


####We can clearly see through graph sales in NA region is the maximum 

###lets find out which are the top 10 games
Name_sales = data |> group_by(Name) |> summarise(Totalsales= sum(Global_Sales))|> arrange(desc(Totalsales))
head(Name_sales, 10)                                        


######Lets find out top 10 publisher according to global sales
Publisher_sales = data |> group_by(Publisher) |> summarise(Totalsales= sum(Global_Sales))|> arrange(desc(Totalsales))
head(Publisher_sales, 10)   


######Lets find out top 10 Platform according to global sales
Platform_sales = data |> group_by(Platform) |> summarise(Totalsales= sum(Global_Sales))|> arrange(desc(Totalsales))
head(Platform_sales, 10)   


#######CONCLUSIONS
#1. Action games are more popular than other genres
#2. PS2 platform had the highest sales globally, followed by X360
#3. The year 2008 had the highest number of games released
#6. The sales trend shows that NA region had the highest sales.
#5. Nintendo had the highest sales globally

##########THANK YOU
print("Thank You")

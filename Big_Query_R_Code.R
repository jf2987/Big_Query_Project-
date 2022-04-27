

## Big Query Project

setwd("C:/Users/cogps/Downloads")

## Read CSV file

data<-read.csv("Georgia Data File.csv")
dim(data)
## 16,237 rows, 32 variables


## see column names
names(data)


## need to see the levels available for the voter file
## and for the ts file variable

levels(as.factor(data$vb_tsmart_state))
# 43 states

levels(as.factor(data$vb_vf_reg_state))
## 3 states 

## Now I need to create a variable indicating whether 
## the zip codes fall in the georgia zip codes
## But first I need to create an object with said GA zip codes. 
### https://www.gavinrozzi.com/post/an-r-package-for-zip-codes/
#install.packages("zipcodeR")
#devtools::install_github("gavinrozzi/zipcodeR")

library(zipcodeR)

GA<-search_state('GA')

View(GA)
GA_zip_Code<-GA$zipcode
View(GA$zipcode)
is.list(GA$zipcode)
GA_zip_Code_list<-as.list(GA$zipcode)
is.list(GA_zip_Code_list)
View(GA_zip_Code_list)

## Pass the list instead of an object as an argument for the 
## nrow function
## https://statisticsglobe.com/count-number-of-cases-within-each-group-of-data-frame-in-r
names(data)
nrow(data[data$vb_tsmart_zip == GA_zip_Code_list, ])
# 19 with the list 
nrow(data[data$vb_tsmart_zip == GA_zip_Code, ])
# 19 with the vector

## Pass the object for the voter file zip codes vb_vf_reg_zip

nrow(data[data$vb_vf_reg_zip == GA_zip_Code_list, ])
# 2386 with the list 
nrow(data[data$vb_vf_reg_zip == GA_zip_Code, ])
# 2386 with the vector

## Now I need to look at the number of rows with GA as a state
## both in the ts vector and the voter file vector
names(data)
nrow(data[data$vb_tsmart_state == "GA", ])
# 15709 with ts smart

nrow(data[data$vb_vf_reg_state == "GA", ])
# 13863 with voter file 


## Now I must subset based on the TS_Smart file GA for it is the one that landed 
## me the most rows-- around 15k
GA_data<-data[data$vb_tsmart_state == "GA", ]
dim(GA_data)

## But, I also need to check what the match rate is now-- between
## the state columns in this subset 
## https://www.tutorialspoint.com/how-to-compare-two-columns-in-an-r-data-frame-for-an-exact-match
names(GA_data)

GA_data$State_match<-ifelse(GA_data$vb_tsmart_state==GA_data$vb_vf_reg_state,"Match","No_Match")

GA_data$City_match<-ifelse(GA_data$vb_tsmart_city==GA_data$vb_vf_reg_city,"Match","No_Match")

GA_data$Zip_match<-ifelse(GA_data$vb_tsmart_zip==GA_data$vb_vf_reg_zip,"Match","No_Match")

## Now I need to see these results in a table
## https://www.rdocumentation.org/packages/sur/versions/1.0.4/topics/percent.table
library(sur)
percent.table(GA_data$State_match, y = NULL)
## there is an 84 percent match rate. I am okay with this

percent.table(GA_data$City_match, y = NULL)
## 81 percent match for City

percent.table(GA_data$Zip_match, y = NULL)
## 79 percent match for zip code. 

## Create a map
## https://map-rfun.library.duke.edu/01_georeference.html
library(tidyverse)
library(sf)
library(mapview)
mapview(GA_data, xcol = "vb_reg_longitude", ycol = "vb_reg_latitude", crs = 4269, grid = FALSE)

## Create a Column with latitude and longitude separated by a 
## comma 
## https://www.statology.org/unite-function-in-r/
library(tidyr)

GA_data<-GA_data %>% unite("Lat_Lon",c('vb_reg_latitude', 'vb_reg_longitude'), sep = ', ', na.rm = TRUE, remove = FALSE)
View(GA_data)

Lat_Lon<-GA_data$Lat_Lon
View(Lat_Lon)

write.csv(Lat_Lon,"C:/Users/cogps/Downloads/Lat_Lon.csv" ,row.names = FALSE)
## look at Demographic frequencies for Marietta and Macon
## Create subset for Macon and Marietta
library(tidyverse)
GA_Macon <- GA_data  %>% 
  filter(vb_tsmart_city == "MACON")

dim(GA_Macon)
table(as.factor(GA_Macon$vb_voterbase_phone_presence_flag))
## 2699 rows with Macon on the ts_Smart city
## 1531 available phone numbers

GA_Marietta <- GA_data  %>% 
  filter(vb_tsmart_city == "MARIETTA")
library(sur)
percent.table(as.factor(GA_Marietta$vb_voterbase_race))
percent.table(as.factor(GA_Macon$vb_voterbase_race))

percent.table(as.factor(GA_Macon$vb_voterbase_gender))
percent.table(as.factor(GA_Marietta$vb_voterbase_gender))
dim(GA_Marietta)
# 5665 in Marietta
table(as.factor(GA_Marietta$vb_voterbase_phone_presence_flag))
## 3610 available phone numbers


## Explore the age variable further 
## https://stackoverflow.com/questions/51095156/how-to-group-ages-in-specific-range-using-r
## https://www.statology.org/error-in-plot-new-figure-margins-too-large/
names(GA_data)
GA_data$Age <- cut(GA_data$vb_voterbase_age, c(seq(0, 120, by = 5), Inf), include.lowest = TRUE)
par(mar = c(1, 1, 1, 1))
barplot(GA_data$Age,col = "pink")
?barplot

library(sur)

percent.table(GA_data$Age)

## subsetting
## https://www.statology.org/subset-data-frame-in-r/

GA_data_C<-subset(GA_data, vb_tsmart_city == "MACON" | vb_tsmart_city == "MARIETTA")

## Creating Bar Plots with ggplot 2 
## http://www.sthda.com/english/wiki/ggplot2-barplots-quick-start-guide-r-software-and-data-visualization
library(ggplot2)
names(GA_data)

ggplot(data=GA_data_C, aes(x=vb_tsmart_city, y=vb_voterbase_age) +
  geom_bar(stat="identity", fill="steelblue")+
  geom_text(aes(label=vb_voterbase_age), vjust=1.6,color="white", size=3.5)+
  theme_minimal())
## http://www.cookbook-r.com/Graphs/Axes_(ggplot2)/

bp<-ggplot(data=GA_data_C, aes(x=vb_tsmart_city, y=vb_voterbase_age)) +
         geom_boxplot()
bp+expand_limits(y=c(0,5))

bp+coord_cartesian(ylim=c(0, 100))

bp+coord_cartesian(ylim=c(0, 130))

bp+coord_cartesian(ylim=c(0, 112))+ scale_y_continuous(breaks=seq(0, 112, 10)) 

## anova group differences in ages
## https://www.scribbr.com/statistics/anova-in-r/

one.way <- aov(vb_voterbase_age ~ vb_tsmart_city, data = GA_data_C)

summary(one.way)

## Not statistically significant

## looking at the gender variables

# Load the library.
library(MASS)

# Create a data frame from the main data set.
car.data <- data.frame(GA_data_C$vb_voterbase_gender, GA_data_C$vb_tsmart_city)

# Create a table with the needed variables.
car.data = table(GA_data_C$vb_voterbase_gender, GA_data_C$vb_tsmart_city) 
print(car.data)

# Perform the Chi-Square test.
print(chisq.test(car.data))
 ## Not statistically significant difference in Gender

## get a map with the data statistics here as well 

# Create a table with the needed variables.
car.data = table(GA_data_C$vb_voterbase_race, GA_data_C$vb_tsmart_city) 
print(car.data)

# Perform the Chi-Square test.
chisq<-chisq.test(car.data)
## statistically significant race variable
## http://sthda.com/english/wiki/chi-square-test-of-independence-in-r

contrib <- 100*chisq$residuals^2/chisq$statistic

library(corrplot)
par(mar = c(5, 4, 4, 2) + 0.1)
corrplot(contrib, is.cor = FALSE)
round(contrib, 3)

library("gplots")
par(mar = c(1,1,1,1))
balloonplot(contrib,label = FALSE,scale.range="relative", show.margins=FALSE)
## Visualize the racial differences between county
source("http://pcwww.liv.ac.uk/~william/R/crosstab.r")
# vb_voterbase_race, GA_data_C$vb_tsmart_city
crosstab(GA_data_C, col.vars = "vb_tsmart_city", row.vars = "vb_voterbase_race", type = c("r"))

crosstab(GA_data_C, col.vars = "vb_tsmart_city", row.vars = "vb_voterbase_race", type = c("f"))

# https://www.unitedstateszipcodes.org/ga/

write.csv(GA_data,"C:/Users/cogps/Downloads/GA_Data_Updates_2.csv" ,row.names = FALSE)

## So i lost hella information because R Studio is malfunctioning
# https://community.rstudio.com/t/r-studio-crashed-and-failed-to-save-unsaved-changes-lost-r-script-and-r-studio-no-longer-reads-csv-files/22571/7
# apparently this has happened before
## Might have to do something with the Tabulizer library I had downloaded
## I need to check its source code



## https://www.zipcodestogo.com/Georgia/

# https://www.unitedstateszipcodes.org/ga/
#https://zipcodes-us.com/zip/ga/atlanta
#
# https://map-rfun.library.duke.edu/01_georeference.html
# https://www.pewresearch.org/methods/2018/02/15/commercial-voter-files-and-the-study-of-u-s-politics/
# https://www.centralstationmarketing.com/website-management/tools/zip-code-manager?state=GA&county=WHITFIELD&city=ROCKY+FACE

# http://applied-r.com/extract-data-tables-from-pdf-files-in-r/
# https://datascienceplus.com/extracting-tables-from-pdfs-in-r-using-the-tabulizer-package/
# https://stackoverflow.com/questions/38592600/how-to-read-pdf-file-in-r
#https://stackoverflow.com/questions/43884603/installing-tabulizer-package-in-r
# https://stackoverflow.com/questions/21646717/convert-pdf-to-csv-with-r
# https://stackoverflow.com/questions/18078303/scraping-large-pdf-tables-which-span-across-multiple-pages

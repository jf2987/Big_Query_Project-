

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


## look at Demographic frequencies for Marietta and Macon
## Create subset for Macon and Marietta
library(tidyverse)
GA_Macon <- GA_data  %>% 
  filter(vb_tsmart_city == "MACON")
dim(GA_Macon)
## 2699 rows with Macon on the ts_Smart city

GA_Marietta <- GA_data  %>% 
  filter(vb_tsmart_city == "MARIETTA")
dim(GA_Marietta)
# 5665 in Marietta

# https://www.unitedstateszipcodes.org/ga/


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

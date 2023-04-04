# RETRIEVING DATA #

# Press Alt + O to collapse all sections in order to choose the section of interest
# To expand all sections, press Shift + Alt + O

## 1.) From a Local .xlsx/.csv File ####

### view the current working directory path
getwd()

### change working directory to filepath where datafile exists
setwd("C:/Users/ [Placeholder for rest of file path]") ## enter your filepath here
getwd()

### read .csv file
df <- read.csv("pain.csv", header = TRUE) ## enter your filename here
head(df)

### read .xlsx file
library(xlsx)
data <- read.xlsx("pain.xlsx",1) ## enter your filename here
head(data)


## 2.) From a Database ####

library(RODBC)

### list the available data sources
as.data.frame(odbcDataSources())

### establish connection to the applicable ODBC connection
ch <- odbcConnect("", uid="", pwd="") # enter the ODBC connection name, username, and password here

### list database objects (Tables)
ListOfTables <- (sqlTables(ch,tableType="TABLE"))
head(ListOfTables,20)


###  retrieve data frame via SQL Select statement (any valid statement)
df <- sqlQuery(ch, "SELECT  * FROM [TableName]")
df

### close the connection
odbcClose(ch)





## 3.) From a Data File on the Web ####

### specify URL for desired website to be scraped
url <- "https://archive.ics.uci.edu/ml/machine-learning-databases/statlog/german/german.data"

col.names <- c(
  'Status_of_existing_checking_account', 'Duration_in_month', 'Credit_history'
  , 'Purpose', 'Credit_amount', 'Savings_account/bonds'
  , 'Employment_years', 'Installment_rate_in_percentage_of_disposable_income'
  , 'Personal_status_and_sex', 'Other_debtors/guarantors', 'Present_residence_since'
  , 'Property', 'Age_in_years', 'Other_installment_plans', 'Housing', 'Number_of_existing_credits_at_this_bank'
  , 'Job', 'Number_of_people_being_liable_to_provide_maintenance_for', 'Telephone', 'Foreign_worker', 'Status'
)

### store and inspect the data
data <- read.csv(url, header=FALSE , sep=' ', col.names=col.names)

head(data)



## 4.) From an .xlsx/.csv file on the Web ####

library(gdata)

### the url for the online Excel file
url <- "http://www.huduser.org/portal/datasets/fmr/fmr2015f/FY2015F_4050_Final.xls"

### use read.xls to import
rents <- read.xls(url)

### inspect the data
rents[1:6, 1:10]






## 5.) Scraped From a Webpage ####

library(rvest)

### specify URL for desired website to be scraped and read the HTML code 
url <- "http://www.imdb.com/search/title?title_type=tv_series&release_date=2017-01-01,2018-01-15&user_rating=7.0,10.0&adult=include&count=250&sort=moviemeter,desc"
webpage <- read_html(url)

### If an error is received in the above line, use the below code to resolve connection to url (proxy issue)
# download.file(url, destfile = "scrapedpage.html", quiet=TRUE)
# webpage <- read_html("scrapedpage.html")

### Use a CSS selector to identify variables of interest

### Scrape the "Rankings" section
rank_data_html <- html_nodes(webpage, '.text-primary')

### Convert the html Ranking data to text
rank_data <- html_text(rank_data_html)
head(rank_data)

### Clean the Ranking data
rank_data <- as.numeric(rank_data)
head(rank_data)
length(rank_data) # make sure this is the # of expected observations

### Scrape the Title data
title_data_html <- html_nodes(webpage, '.lister-item-header a')

### Convert the html Title data to text
title_data <- html_text(title_data_html)
head(title_data)
length(title_data) # make sure this is the # of expected observations


### Combine all lists to form a data frame
movies_df <- data.frame(Rank = rank_data, Title = title_data)
head(movies_df) # inspect head
str(movies_df) # inspect structure



### Tip: f you encounter missing values, use the below function 
for (i in c(39,73,80,89)){ ## these values are the location of the missing values
  a <- attribute[1:(i-1)]
  b <- attribute[i:length(attribute)]
  attribute <- append(a,list("NA"))
  attribute <- append(attribute,b)
}






## 6.) Example of Scraping data from "UCI Machine Learning Data Repository" ####

library(rvest)
library(dplyr)

### specify URL for desired website to be scraped and read the HTML code 
url <- "https://archive.ics.uci.edu/ml/datasets.html"
webpage <- read_html(url)

### If an error is received in the above line, use the below code to resolve connection to url (proxy issue)
# download.file(url, destfile = "scrapedpage.html", quiet=TRUE)
# webpage <- read_html("scrapedpage.html")

### Scrape the "Table Name" section
table_name_html <- html_nodes(webpage, 'b a')

table_name <- html_text(table_name_html)
head(table_name)
length(table_name)

#### clean the "Table Name" section
trim <- function (x) gsub("^\\s+|\\s+$", "", x) # returns string w/o leading or trailing whitespace
table_name <- trim(table_name)

### Scrape the "Default Task" section
default_task_html <- html_nodes(webpage, 'td:nth-child(3) .normal')

default_task <- html_text(default_task_html)
head(default_task)
length(default_task)

#### clean the "Default Task" section
default_task <- trim(default_task)
default_task <- default_task[1:416]

### Scrape the "# of Instances" section
instance_number_html <- html_nodes(webpage, 'td:nth-child(5) .normal')

instance_number <- html_text(instance_number_html)
head(instance_number)
length(instance_number)

#### clean the "# of Instances" section
instance_number <- trim(instance_number)

### Scrape the "# of Attributes" section
attribute_number_html <- html_nodes(webpage, 'td:nth-child(6) .normal')

attribute_number <- html_text(attribute_number_html)
head(attribute_number)
length(attribute_number)

#### clean the "# of Attributes" section
attribute_number <- trim(attribute_number)

### Scrape the "Year of Data Collection" section
data_collect_year_html <- html_nodes(webpage, 'td:nth-child(7) .normal')

data_collect_year <- html_text(data_collect_year_html)
head(data_collect_year)
length(data_collect_year)

#### clean the "Year of Data Collection" section
data_collect_year <- trim(data_collect_year)

### Create Dataframe
UCI_df <- data.frame(table_name, instance_number, attribute_number, data_collect_year, default_task)


### Convert blanks to NA in data frame
UCI_df$default_task <- as.character(UCI_df$default_task) 
UCI_df$default_task[UCI_df$default_task==""] <- "NA"
UCI_df$default_task <- as.factor(UCI_df$default_task)

UCI_df$instance_number[UCI_df$instance_number==""] <- "NA"

UCI_df$attribute_number[UCI_df$attribute_number==""] <- "NA"

UCI_df$data_collect_year[UCI_df$data_collect_year==""] <- "NA"

### Rename Dataframe columns
UCI_df <- rename(UCI_df, "Table Name" = table_name, "# of Instances" = instance_number, "# of Attributes" = attribute_number, "Year Collected" = data_collect_year, "Common Associated Tasks" = default_task)

head(UCI_df)

### order the data by table name, for some reason rvest did not retain correct order
UCI_df <- UCI_df[order(UCI_df$`Table Name`),]

head(UCI_df)

## Now we have to append the Table Definitions from a different page

### specify URL for desired website to be scraped and read the HTML code 
url <- "http://archive.ics.uci.edu/ml/datasets.html?format=&task=&att=&area=&numAtt=&numIns=&type=&sort=nameUp&view=list"
webpage <- read_html(url)

### If an error is received in the above line, use the below code to resolve connection to url (proxy issue)
download.file(url, destfile = "scrapedpage.html", quiet=TRUE)
webpage <- read_html("scrapedpage.html")

### Scrape the "Table Name/Table Definition" section
table_namedef_html <- html_nodes(webpage, 'b a')

table_namedef <- html_text(table_namedef_html)
head(table_namedef)
length(table_namedef)

#### clean the "Table Name" section
table_namedef <- trim(table_namedef)

### Scrape the "Table Name/Table Definition" section
table_namedef_def_html <- html_nodes(webpage, 'td+ td td .normal')

table_namedef_def <- html_text(table_namedef_def_html)
head(table_namedef_def)
length(table_namedef_def)

#### clean the "Table Name Definition" section
table_namedef_def <- trim(table_namedef_def)
table_namedef_def <- table_namedef_def[2:length(table_namedef_def)]    

## create the data frame with a column that counts char to remove from the definition column
UCI_df2 <- data.frame(table_namedef, table_namedef_def)

UCI_df2 <- data.frame(name=UCI_df2$table_namedef,definition=UCI_df2$table_namedef_def, namecount=apply(UCI_df2,2,nchar)[,1]) 

head(UCI_df2,2)

### order the data by table name, for some reason rvest did not retain correct order
UCI_df2 <- UCI_df2[order(UCI_df2$name),]
head(UCI_df2)

### append trimmed definitions to original data frame
UCI_df$TableDefinition <- trim(substr(UCI_df2$definition,UCI_df2$namecount+6, 10000))

head(UCI_df,1)

## explore data
prop.table(table(as.array(UCI_df$`Year Collected`)))
library(dplyr)
library(shiny)
library(stringr)

adminsalaries <- read.csv("C:/Users/Jeremiah Lowhorn/Desktop/CUPAHR/adminsalaries.csv", 
                          stringsAsFactors=FALSE)
Positions <- read.csv("C:/Users/Jeremiah Lowhorn/Desktop/CUPAHR/Positions.csv", 
                      stringsAsFactors=FALSE)
CarClassCode <- read.csv("C:/Users/Jeremiah Lowhorn/Desktop/CUPAHR/CarClassCode.csv",
                         stringsAsFactors=FALSE)


####################Admin Salaries Data Set  Cleaning Operations####################

###Remove commas from numeric vectors and set data type as numeric
adminsalaries$salary <- as.numeric(gsub(",","",adminsalaries$salary))
adminsalaries$Operating.expenses <- as.numeric(gsub(",",
                                                    "",
                                                    adminsalaries$Operating.expenses))

###Set all other numeric vectors to the data type numeric
adminsalaries$Total.Student.FTE <- as.numeric(adminsalaries$Total.Student.FTE)
adminsalaries$Total.Faculty.FTE <- as.numeric(adminsalaries$Total.Faculty.FTE)
adminsalaries$Total.Staff.FTE <- as.numeric(adminsalaries$Total.Staff.FTE)

###Replace #NULL! values with 0 and set the data type to numeric within the 
###years.in.position vector
adminsalaries$years.in.position <- as.numeric(gsub("#NULL!",
                                                   NA,
                                                   adminsalaries$years.in.position))
#adminsalaries$NCAA.Division <- gsub(NA,"Not Applicable",adminsalaries$NCAA.Division)

adminsalaries <- adminsalaries %>%
  mutate(NCAA.Division = ifelse(is.na(NCAA.Division),"Not Applicable",NCAA.Division),
         NCAA.Division = ifelse(NCAA.Division == "","Not Applicable",NCAA.Division))



#######################Positions Data Set Cleaning Operations#######################

###Rename Position vectors
Positions <- rename(Positions,position=New.Position..)
Positions <- rename(Positions,
                    Occupational.Category=BLS.Standard.Occupational.Code..SOC..Category.Name)

###Join the Admin Salaries Data Set to the Positions Dataset
adminsalaries <- left_join(adminsalaries,Positions,by="position")

###Remove the Positions data set
rm(Positions)

###Remove the Job Description Vector
adminsalaries<- adminsalaries[,-18]


####################Car Class Code Data Set  Cleaning Operations####################
###Removing all operations due to insufficent variables to join and retain size of 
###original data

###Rename the Code.Carnegie vector and set as a character to allow the joining of 
###the admin salaries and Car Class Code data sets
#CarClassCode <- rename(CarClassCode,General.Carnegie.Class=Code.Carnegie)
#CarClassCode$General.Carnegie.Class <- as.character(CarClassCode$General.Carnegie.Class)
#adminsalaries$General.Carnegie.Class <- as.character(adminsalaries$General.Carnegie.Class)

###Join the Admin Salaries Data Set to the Car Class Code Data Set
#adminsalaries <- left_join(adminsalaries,CarClassCode,by="General.Carnegie.Class")

###Remove the Car Class Code Data Set
rm(CarClassCode)


###########################Final Data Cleaning Operations###########################
adminsalaries$US.Census.Code <- as.factor(adminsalaries$US.Census.Code)
adminsalaries$institution.id <- as.factor(adminsalaries$institution.id)
adminsalaries$position <- as.factor(adminsalaries$position)

###Remove all NA rows from the Admin Salaries Dataset
adminsalaries <- adminsalaries[complete.cases(adminsalaries),]


###########################Data Aggregation###########################

adminsalaries$Affiliation <- as.factor(adminsalaries$Affiliation)
adminsalaries$US.Census.Region <- as.factor(adminsalaries$US.Census.Region)
adminsalaries$Entity.Type <- as.factor(adminsalaries$Entity.Type)
adminsalaries$gender <- as.factor(adminsalaries$gender)
adminsalaries$NCAA.Division <- as.factor(adminsalaries$NCAA.Division)
adminsalaries$VETS.100.Category <- as.factor(adminsalaries$VETS.100.Category)
adminsalaries$ethnicity <- as.factor(adminsalaries$ethnicity)

NCAA_Division <- adminsalaries %>%
  group_by(NCAA.Division,VETS.100.Category) %>%
  summarize(Median_Salary = median(salary))
mean <- adminsalaries %>%
  group_by(NCAA.Division,VETS.100.Category) %>%
  summarize(Mean_Salary = mean(salary))


years <- adminsalaries %>%
  group_by(years.in.position,VETS.100.Category) %>%
  summarize(Mean_Salary = median(salary))

years <- plyr::rename(years,c("years.in.position"="Tenure"))

years <- as.data.frame(years)


threed <- adminsalaries %>%
  select(salary,Total.Faculty.FTE,Total.Student.FTE)



setwd("C:/Users/Jeremiah Lowhorn/Desktop/CUPAHR/Application")

runApp(host="0.0.0.0",port=5050)


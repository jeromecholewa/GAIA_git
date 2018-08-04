# This script if to process the GAIA time sheet export as xlsx file
####  KIM, Do Keun is missing from the list?? He is not badging in nor out...

# #####################
library("dplyr")
# install.packages("readxl")   #  if necessary
library(readxl)
library(xlsx)
library(data.table)
library(ggplot2)

# The initial file .csv must be "clean" which means:
#   - NO HEADERS
#   - the data starts directly at the line 5, in the cell(B5) - volume in liters
#   - the data has 6 columns: empty column(A), Liters(B), height filling mode (C),
#                             Ohms filling mode (D),  height draining mode (E),
#                             Ohms draining mode (F)

# setwd("~/Korea/HR/GAIA/")  # enter correct folder for original file
setwd("~/Documents/Education/Coursera/Datascience with R/GAIA/")  # enter correct folder for original file

rm(list = ls())

# filename <- "KOR+Attendance+Report_20170626_144515.xlsx"
filename <- "KOR+Attendance+Report_20180724_141248.xlsx"
gaia <- data.table(read_excel(filename, sheet = 1,
                              col_types = "text", skip = 6, na = ""))

####  Liste of employees to have a grouping by department
FileEmployList <- "ShinyGAIA/GAIA_Employee_List.xlsx"
employList <- data.table(read_excel(FileEmployList , sheet = 1  ,
                                    range = cell_cols("A:B")))
#length(employList[,Department])

# head(employList)
# head(gaia)
# str(gaia)
# sapply(gaia, class)

#gaia <- gaia[,-"Name"]  # remove column of Korean name (as it is unreadable)
# gaia[(!is.na(Clock_In_Date)) & !is.na(Clock_Out_Date)]    # 3190
# gaia[(!is.na(Clock_In_Time)) & !is.na(Clock_Out_Time)]    # 2825

# remove any lines that has either an NA in Clock_In_Time or in Clock_Out_Time
gaia <- gaia[(!is.na(Clock_In_Time)) & !is.na(Clock_Out_Time)]

# gaia[is.na(Clock_In_Time) | is.na(Clock_Out_Time)]  #  0
# gaia[is.na(Clock_In_Date) | is.na(Clock_Out_Date) | is.na(Date)]  #  0

# gaia[Name == "LIU, Bei", English_Name := "LIU, Bei"]
# gaia[Name == "HUANG, Zewen", English_Name := "HUANG, Zewen"]
# gaia[Name == "김동현", English_Name := "KIM, Dong-Hyun"]
#
# gaia[Name == "LIU, Bei", ]
# gaia[Name == "HUANG, Zewen", ]
# gaia[Name == "김동현", ]

############################################
#############################################
# add 4 lines for CSAE from January through April

# missMonthsCSAE <- gaia[English_Name == "KIM, Young-Chul"][1:4]
# missMonths <- c("2017-01-01", "2017-02-01", "2017-03-01", "2017-04-01")
# missMonthsCSAE[,Date:= missMonths]
# missMonthsCSAE[,Clock_In_Date:= missMonths]
# missMonthsCSAE[,Clock_Out_Date:= missMonths]
# missMonthsCSAE[,Clock_Out_Time:= Clock_In_Time]
#
# gaia <- rbindlist(list(gaia, missMonthsCSAE))

### convert Dates as real dates
colDate <- c("Date", "Clock_In_Date", "Clock_Out_Date")
colTime <- c("Clock_In_Time", "Clock_Out_Time")

# converts dates from "character" to "Date"
gaia[, (colDate) := lapply(.SD, as.Date), .SDcols = colDate]

### convert Dates as real dates
##############################
# function to convert Date and Time string as a real POSIXct "date and time" stamp
asTimeStamp <-  function(strangeDateString) {
  # times of the form "YYYY-MM-DD HH:MM"
  formatString = "%Y-%m-%d %H:%M"
  return(as.POSIXct(strangeDateString, format = formatString))
}


## create 2 new columns concatenating date and time and converting as POSIXct
gaia[,c("Date_Time_In", "Date_Time_Out") := list(asTimeStamp(paste(Clock_In_Date,Clock_In_Time)),
                                                 asTimeStamp(paste(Clock_Out_Date,Clock_Out_Time)))]

# add column for time of presence (includes lunch time (contractually 1 hour) and breaks)
gaia[, presenceTime := difftime( Date_Time_Out, Date_Time_In, units = "hours")]

# add column for working time = presence time - 1 hour
gaia[, WorkingTime :=  presenceTime - 1]

### correct negative working time for CSAE or other
### in first 4 months  fo 2018 (0 instead of -1)
gaia[ WorkingTime == - 1, WorkingTime := 0]

# add column for Month
gaia[, MonthN := format(Date_Time_Out,"%Y-%m") ]

setkey(gaia, English_Name)
setkey(employList, English_Name)
# add the Department depending on employee name
gaia[employList, Department := i.Department]

earlyLeave <- gaia[ WorkingTime <4.5,]
earlyLeave[, WorkingTime :=  WorkingTime + 1]
gaia <- gaia[ WorkingTime >=4.5,]  # all except early leave



earlyLeaveByMonth <- earlyLeave[, .(WorkingHours = mean(WorkingTime)),
                                by = .(MonthN, Department)][order(MonthN,
                                                                  -WorkingHours)]
earlyLeaveByDepartment <- earlyLeave[,.(WorkingHours = mean(WorkingTime),
                                        Department = unique(Department)),
                                     by = English_Name][order(Department,
                                                              -WorkingHours)]

paste("Reported period :", format(min(gaia$Date), format = "%Y-%b-%d"),
      "to", format(max(gaia$Date), format = "%Y-%b-%d"))

countEarlyleave <- earlyLeave[, .N, by = Department]

#DT[,.(V4.Sum = sum(V4)),by=V1]
#DT[, .N, by=V1]


countEarlyMessage <- "Number of days of ealy leaves:\n"
for (dept in countEarlyleave$Department) {
  countEarlyMessage <- paste0(countEarlyMessage, dept,
                              " :   ",
                              countEarlyleave[ Department == dept, N],
                              " days", "\n")
}
#countEarlyMessage
cat(countEarlyMessage)

gaia[,.(mean(WorkingTime),unique(Department)), by = English_Name]


### And the winner is ... (individually)
gaia[,.(WorkingHours = mean(WorkingTime),Department = unique(Department)),
     by = English_Name][order( -WorkingHours)]

### And the winner is (by department)
gaia[,.(WorkingHours = mean(WorkingTime),Department = unique(Department)),
     by = English_Name][order(Department, -WorkingHours)]

###  by month and by department
Av_Month_Dept <- gaia[, .(WorkingHours = mean(WorkingTime)),
                      by = .(MonthN, Department)][order(MonthN, -WorkingHours)]

# Av_Month_Dept[Department == "POAE"]

### Graph by Month and by dept
ggplot(Av_Month_Dept, aes(factor(MonthN), WorkingHours, fill = Department)) +
  geom_bar(stat="identity", position = "dodge", color = "black") +
  ggtitle("Average hours worked per day per person (2017)" ,
          subtitle = "Lunch break excluded (1 hour)")  +
  labs( x = "Months", y = "Average Nb of hours") +
  geom_hline(yintercept = 8, linetype="dashed", color = "red") #+
  #annotate("text",  min(Av_Month_Dept$MonthN),
  #         min = 1, 8, vjust = -1, label = "8h")

### Graph by Dept and by Month
ggplot(Av_Month_Dept, aes(factor(Department), WorkingHours, fill = MonthN)) +
  geom_bar(stat="identity", position = "dodge", color = "black") +
  ggtitle("Average hours worked per day per person (2017)",
          subtitle = "Lunch break excluded (1 hour)") +
  labs( x = "Department", y = "Average Nbs of hours") +
  geom_hline(yintercept = 8, linetype="dashed", color = "red")


#Av_Month_Dept_noPOAE <-  Av_Month_Dept[Department != "POAE"]
### Graph by Month and by dept (except POAE)  Jan to Jun
ggplot(Av_Month_Dept, aes(factor(MonthN), WorkingHours, fill = Department,
                                 label = round(WorkingHours, digits = 1))) +
  geom_bar(stat="identity", position = "dodge", color = "black") +
  ggtitle("Average hours worked per day per person (2017)",
          subtitle = "Lunch break excluded (1 hour)") +
  labs( x = "Months", y = "Average Nb of hours") ++
  geom_hline(yintercept = 8, linetype="dashed", color = "red") +
  geom_text(angle = 70,
            #check_overlap = TRUE,
            size = 4, aes(colour = factor(Department), y = WorkingHours + 0.8),
            position = position_dodge(0.9)) +
  guides( colour = FALSE )

### Graph by Dept and by Month (except POAE) Jan to Jun
ggplot(Av_Month_Dept, aes(factor(Department), WorkingHours, fill = MonthN,
                                 label = round(WorkingHours, digits = 1))) +
  geom_bar(stat="identity", position = "dodge", color = "black") +
  ggtitle("Average hours worked per day per person (2017)",
          subtitle = "Lunch break excluded (1 hour)") +
  labs( x = "Department", y = "Average Nb of hours") +
  geom_hline(yintercept = 8, linetype="dashed", color = "red") +
  geom_text(angle = 70,
            #check_overlap = TRUE,
            size = 4, aes(colour = factor(MonthN), y = WorkingHours + 0.8),
                          position = position_dodge(0.9)) +
  guides( colour = FALSE )

#############
##### only for a given month

MonthNumber <- "05"
Av_SpecMonth_Dept <- Av_Month_Dept[MonthN ==MonthNumber]

### Graph by Month and by dept for specific month
ggplot(Av_SpecMonth_Dept, aes(factor(Department), WorkingHours,
                                 label = round(WorkingHours, digits = 1))) +
  geom_bar(stat="identity", position = "dodge", color = "black") +
  ggtitle(paste0("Average hours worked per day per person (Month ",MonthNumber,")"),
          subtitle = "Lunch break excluded (1 hour)") +
  labs( x = "Department", y = "Average Nb of hours") +
  geom_text(angle = 70,
            #check_overlap = TRUE,
            size = 3,
            aes(y = WorkingHours + 0.8),
                #colour = factor(Department),
            position = position_dodge(0.9)) #+
  # guides( colour = FALSE )

########################
########## ONLY FOR  A GIVEN DEPARTMENT with all available months
DeptList <- Av_Month_Dept[,unique(Department)]
dept <- "PAE"   # DeptList[2]
# dept <- "Lab"
Av_SpecDept_byMonth <- Av_Month_Dept[Department == dept]

ggplot(Av_SpecDept_byMonth, aes(factor(MonthN), WorkingHours,
                              label = round(WorkingHours, digits = 1))) +
  geom_bar(stat="identity", position = "dodge", color = "black") +
  ggtitle(paste0("Average hours worked per day per person in ", dept," department"),
          subtitle = "Lunch break excluded (1 hour)") +
  labs( x = paste(dept, "Department"), y = "Average Nb of hours") +
  geom_text(angle = 70,
            #check_overlap = TRUE,
            size = 3,
            aes(y = WorkingHours + 0.8),
            #colour = factor(Department),
            position = position_dodge(0.9)) #+


######### PLAYl

gaia[Department == "POAE" & MonthN == "06"]
unique(gaia[,English_Name])
sum(gaia[,is.na(English_Name)])
gaia[is.na(English_Name)][100:149,]
gaia[English_Name == NA]   # does not work
gaia[English_Name == "LIU, Bei"]
length(gaia[Name == "LIU, Bei", English_Name])
gaia[English_Name == "LIU, Bei"]
gaia[English_Name == "HUANG, Zewen"]
gaia[ English_Name == "KIM, Dong-Hyun"]

employList[order(Department,
                   English_Name)]
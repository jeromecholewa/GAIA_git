#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# library(shiny)
# library(shinyBS)
library(readxl)
library(xlsx)
library(data.table)
library(ggplot2)

options(shiny.maxRequestSize=10*1024^2)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {

  ####  Liste of employees to have a grouping by department
  FileEmployList <- "GAIA_Employee_List.xlsx"
  employList <- data.table(read_excel(FileEmployList, sheet = 1,range = cell_cols("A:B")))

  ####### Let user download and check the Employee List
  output$downloadData <- downloadHandler(
    filename = function(file) {
      "Employee_List.xlsx"
    },
    content = function(con) {
      write.xlsx( x = employList,
                  con,
                  sheetName = "Employee List",
                  row.names = FALSE)
    }
  )

    observe({
        #filename <- "KOR+Attendance+Report_20170626_144515.xlsx"
        filename2 <- input$filenameInit
        ext <- tools::file_ext(filename2$name)
        ifelse (is.null(filename2) || (ext != "xls" && ext != "xlsx"),
                { textFILE <- "The file should be .xls or .xlsx"
                #output$FILE <- renderText({textFILE})
                output$errorMessage <- renderText({" "})
                return(NULL)},
                textFILE <- input$filenameInit$name
        )
        output$FILE <- renderText({textFILE})

        # file.rename(filename2$datapath,
        #             paste(filename2$datapath, ext, sep="."))
        # gaia <- data.table(read_excel(paste(filename2$datapath, ext, sep="."),
        gaia <- data.table(read_excel(filename2$datapath,
                                      sheet = 1, skip = 6, na = "",
                                      col_types = "text"))
        # output$GAIANames <- renderText({
        #     ifelse (is.null(input$filenameInit),
        #             "You have not uploaded any file",
        #             paste(names(gaia2), collapse = " "))
        # })

        output$FILE <- renderText({textFILE})

        if (paste(names(gaia), collapse = " ") != "ID Name English_Name Salary_Structure Date Date_type Shift Clock_In_Date Clock_In_Time Clock_Out_Date Clock_Out_Time Exception Non-Schedule Clock_In_Unread Clock_Out_Unread Early_Out_Minutes Late_In_Times") {
                    output$errorMessage <- renderText({"This is not a correct xlsx extract from GAIA"})
                    return(NULL)}

        output$errorMessage <- renderText({" "})

        #gaia2 <- data.table(read_excel(filename, sheet = 1,
        #                              skip = 6, na = ""))

        ####  Liste of employees to have a grouping by department
        # FileEmployList <- "GAIA_Employee_List.xlsx"
        # employList <- data.table(read_excel(FileEmployList, sheet = 1,range = cell_cols("A:B")))


        # ####### Let user download and check the Employee List
        # output$downloadData <- downloadHandler(
        #   filename = function(file) {
        #    "Employee_List.xlsx"
        #   },
        #   content = function(con) {
        #     write.xlsx( x = employList,
        #                 con,
        #                 sheetName = "Employee List",
        #                 row.names = FALSE)
        #   }
        # )


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
        ###  add English Names for missing English names in database

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
        gaia[,c("Date_Time_In", "Date_Time_Out") := list(asTimeStamp(paste(Clock_In_Date,
                                                                           Clock_In_Time)),
                                                         asTimeStamp(paste(Clock_Out_Date,
                                                                           Clock_Out_Time)))]

        output$period <-  renderText({
          paste("Reported period :", format(min(gaia$Date),
                       format = "%Y-%b-%d"), "to",
                format(max(gaia$Date), format = "%Y-%b-%d"))
        })

        # add column for time of presence (includes lunch time (contractually 1 hour) and breaks)
        gaia[, presenceTime := difftime( Date_Time_Out, Date_Time_In, units = "hours")]

        # add column for working time = presence time - 1 hour
        gaia[, WorkingTime :=  presenceTime - 1]

        ### correct negative working time (e.g.) for CSAE in first 4 months (0 instead of -1)
        gaia[ WorkingTime == - 1,WorkingTime := 0]

        # add column for Month
        gaia[, MonthN := format(Date_Time_Out,"%Y-%m") ]

        setkey(gaia, English_Name) ; setkey(employList, English_Name)
        # merge with the Department depending on employee name
        gaia[employList, Department := i.Department]

        earlyLeave <- gaia[ WorkingTime <4.5,]
        earlyLeave[, WorkingTime :=  WorkingTime + 1]
        gaia <- gaia[ WorkingTime >=4.5,]  # all except early leave

        earlyLeaveByMonth <- earlyLeave[,
                                        .(WorkingHours = mean(WorkingTime)),
                                        by = .(MonthN, Department)][order(MonthN,
                                                                          -WorkingHours)]
        earlyLeaveByDepartment <- earlyLeave[,
                                             .(WorkingHours = mean(WorkingTime),
                                                Department = unique(Department)),
                                             by = English_Name][order(Department,
                                                                      -WorkingHours)]

        countEarlyleave <- earlyLeave[, .N, by = Department]

        countEarlyMessage <- "Number of days of early leaves:<br/>"
        for (dept in countEarlyleave$Department) {
          countEarlyMessage <- paste0(countEarlyMessage, dept,
                                      " :   ",
                                      countEarlyleave[ Department == dept, N],
                                      " days", "<br/>")
        }
        output$messageEarly <- renderUI({
          HTML(countEarlyMessage)    })



        Av_Month_Dept <- gaia[, .(WorkingHours = mean(WorkingTime)),
                              by = .(MonthN, Department)][order(MonthN, -WorkingHours)]

        #############  Graph by Month and by dept
        output$GAIAPlot1 <- renderPlot({
            ggplot(Av_Month_Dept, aes(factor(MonthN), WorkingHours, fill = Department,
                                             label = round(WorkingHours, digits = 1))) +
                geom_bar(stat="identity", position = "dodge", color = "black") +
                ggtitle("Average hours worked per day per person",
                        subtitle = "In addition to the lunch break") +
                labs( x = "Months", y = "Average Nb of hours") +
                geom_text(angle = 70,
                          #check_overlap = TRUE,
                          size = 4, aes(colour = factor(Department), y = WorkingHours + 0.8),
                          position = position_dodge(0.9)) +
                guides( colour = FALSE ) +
            geom_hline(yintercept = 8, linetype="dotdash", color = "red")

        })

        ################  Graph by Dept and by Month
        output$GAIAPlot2 <- renderPlot({

            ggplot(Av_Month_Dept, aes(factor(Department), WorkingHours, fill = MonthN,
                                             label = round(WorkingHours, digits = 1))) +
                geom_bar(stat="identity", position = "dodge", color = "black") +
                ggtitle("Average hours worked per day per person",
                        subtitle = "In addition to the lunch break") +
                labs( x = "Department", y = "Average Nb of hours") +
                geom_text(angle = 70,
                          #check_overlap = TRUE,
                          size = 4, aes(colour = factor(MonthN), y = WorkingHours + 0.8),
                          position = position_dodge(0.9)) +
                guides( colour = FALSE ) +
            geom_hline(yintercept = 8, linetype="dotdash", color = "red")

        })

        ############    Stats for a given month (by default March)
        output$GAIAPlot3 <- renderPlot({

            MonthNumber <- input$selectYearMonth
            Av_SpecMonth_Dept <- Av_Month_Dept[MonthN ==MonthNumber]

            ### Graph by dept for specific month
            ggplot(Av_SpecMonth_Dept, aes(factor(Department), WorkingHours,
                                          label = round(WorkingHours,
                                                        digits = 1))) +
                geom_bar(stat="identity", position = "dodge", color = "black") +
                ggtitle("Average hours worked per day per person",
                        subtitle = "In addition to the lunch break") +
                labs( x = "Department", y = "Average Nb of hours") +
                geom_text(angle = 70,
                          #check_overlap = TRUE,
                          size = 5,
                          aes(y = WorkingHours + 0.8),
                          #colour = factor(Department),
                          position = position_dodge(0.9)) +
              geom_hline(yintercept = 8, linetype="dotdash", color = "blue")
        })

        output$PlotDept <- renderPlot({
          dept <- input$selectDept
          Av_SpecDept <- Av_Month_Dept[Department == dept]
          ### Graph by Month for specific dept
          ggplot(Av_SpecDept, aes(factor(MonthN), WorkingHours,
                                          label = round(WorkingHours,
                                                        digits = 1))) +
            geom_bar(stat="identity", position = "dodge", color = "black") +
            ggtitle("Average hours worked per day per person",
                    subtitle = "Lunch break excluded (1 hour)") +
            labs( x = paste(dept, "Department"), y = "Average Nb of hours") +
            geom_text(angle = 70,
                      #check_overlap = TRUE,
                      size = 5,
                      aes(y = WorkingHours + 0.8),
                      #colour = factor(Department),
                      position = position_dodge(0.9)) +
            geom_hline(yintercept = 8, linetype="dotdash", color = "blue")
        })

    })
})

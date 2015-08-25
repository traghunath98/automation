## The objective of these sets of functions are to automate the analysis of the daily scrum dashboard that gets published
## First we will read the dashborad data from a CSV file into a data frame. We will then transform the 
## the data frame to give us the necessary critical insights about the schedule and status reported by the scrum teams on a timeline scale. 
## All graphs must be generated at the click of a button.
##
## Each function is documented below in detail to highlight its specific responsibility.
##  

## This function - doSOSAnalysis is the main function that takes the file name as input and applies transformation
## 

doSOSAnalysis <- function(x_file = character()) {
    
	library(ggplot2)
	library(scales)
	
	scrum_db <- data.frame()
	if(!file.exists(x_file)){
		stop("invalid file")
	}
	scrum_db <- read.csv(x_file, stringsAsFactors=FALSE, na.strings=c("","N/A","TBD"))
	
	scrum_db <- updateColNames(scrum_db)
    scrum_db <- performComputations(scrum_db)
    temp_scrum_db <<- scrum_db
    
    #Plot the Scrum Status as RYG indicator
    g <- ggplot(data=scrum_db, aes(x=ScrumNo, y=pctSchedule, fill=Status)) + geom_bar(stat="identity") + coord_flip() + scale_fill_manual(values=c("GREEN"="green","YELLOW"="yellow","RED"="red"))
    g <- g + labs(x="Scrum No",y="Percent Elapsed Schedule", title="CCUW Scrum Dashboard")
    g <- g + theme_bw() + theme(axis.text.x = element_text(size=8), legend.text = element_text(size=7), legend.title=element_text(size=9))
	
    fileName <- paste("ScrumDashboard_Analysis",strftime(Sys.time(),"%d%b%y-%H%M"),".pdf", sep="")
    if(file.exists(fileName)){
        file.remove(fileName)
    }
    
    pdf(fileName)
        print(g)
    dev.off()
    
}

## updateColNames function takes in the sos raw data file and standardizes the column names as per usage
updateColNames <- function(sos=data.frame()){
    
    if(!(class(sos) =="data.frame")){
        stop("Invalid input - this function expects scrum dashboard data as a dataframe")
    }
    
    ## Set all the column names individually
    colnames(sos)[1] <- "RelStartDate"
    colnames(sos)[2] <- "RelEndDate"
    colnames(sos)[3] <- "ReportDate"
    colnames(sos)[4] <- "Release"
    colnames(sos)[5] <- "ScrumNo"
    colnames(sos)[6] <- "ScrumFeatures"
    colnames(sos)[7] <- "ScrumMaster"
    colnames(sos)[8] <- "Status"
    colnames(sos)[9] <- "ProjectManager"
    colnames(sos)[10] <- "PlanScrumStartDate"
    colnames(sos)[11] <- "PlanScrumEndDate"
    colnames(sos)[12] <- "ActScrumStartDate"
    colnames(sos)[13] <- "ActScrumEndDate"
    colnames(sos)[14] <- "SprintStartDate"
    colnames(sos)[15] <- "SprintEndDate"
    colnames(sos)[16] <- "MergeDate"
    colnames(sos)[17] <- "Comments"
    colnames(sos)[18] <- "Impediments"
    colnames(sos)[19] <- "ReleaseSchedule"
    colnames(sos)[20] <- "ElapsedSchedule"
    colnames(sos)[21] <- "pctElapsedSchedule"
    colnames(sos)[22] <- "ScrumPlanBaseline"
    colnames(sos)[23] <- "Scrum_Actual"
    colnames(sos)[24] <- "SprintPlan_Baseline"
    colnames(sos)[25] <- "Sprint_Actual"
    
    
    #Take care of the date formatting
    sos$RelStartDate <- as.Date(sos$RelStartDate, "%m/%d/%Y")
    sos$RelEndDate <- as.Date(sos$RelEndDate, "%m/%d/%Y")
    sos$ReportDate <- as.Date(sos$ReportDate,"%m/%d/%Y")
    sos$Release <- as.factor(sos$Release)
    sos$ScrumNo <- as.factor(sos$ScrumNo)
    sos$ScrumMaster <- as.factor(sos$ScrumMaster)
    sos$Status <- as.factor(sos$Status)
    sos$PlanScrumStartDate <- as.Date(sos$PlanScrumStartDate, "%m/%d/%Y")
    sos$PlanScrumEndDate <- as.Date(sos$PlanScrumEndDate, "%m/%d/%Y")
    sos$ActScrumStartDate <- as.Date(sos$ActScrumStartDate, "%m/%d/%Y")
    sos$ActScrumEndDate <- as.Date(sos$ActScrumEndDate, "%m/%d/%Y")
    sos$SprintStartDate <- as.Date(sos$SprintStartDate, "%m/%d/%Y")
    sos$SprintEndDate <- as.Date(sos$SprintEndDate, "%m/%d/%Y")
    sos$MergeDate <- as.Date(sos$MergeDate, "%m/%d/%Y")
    
    return(sos)
    
}
## performComputations function takes in the scrum_db dataframe and computes the schedule, elapsed schedule etc.
performComputations <- function(scrum_db=data.frame()){
    
    scrum_db$ReleaseSchedule <- as.numeric(scrum_db$RelEndDate - scrum_db$RelStartDate)
    scrum_db$ElapsedSchedule <- as.numeric(scrum_db$ReportDate - scrum_db$RelStartDate)
    scrum_db$pctElapsedSchedule <- scrum_db$ElapsedSchedule/scrum_db$ReleaseSchedule
    
    #Compute the Schedule using diff. This assumes that the dataframe is sorted by the scrum names, followed by report date
    Schedule <- 0
    size <- nrow(scrum_db)
    Schedule[2:size] <- diff(scrum_db$ReportDate)
    scrum_db$Schedule <- Schedule
    # Take care of the date differences when there is a change in scrum name
    scrum_db$Schedule[scrum_db$Schedule <=0] <- as.numeric(scrum_db$ReportDate[scrum_db$Schedule<=0] - scrum_db$RelStartDate[scrum_db$Schedule <=0])
    
    scrum_db$pctSchedule <- scrum_db$Schedule/scrum_db$ReleaseSchedule

    return(scrum_db)
}

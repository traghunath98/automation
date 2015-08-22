## The objective of these sets of functions are to automate the process of analysis of CCUW Incidents data
## First we will read the incidents data from a CSV file, taken as a dump from Service Now. We will then transform
## the data frame to give us the necessary critical insights. 
## All graphs must be generated at the click of a button.
##
##
## Each function is documented below in detail to highlight its specific responsibility.
##

## This function - analyIncidents is the main function that takes the file name as input and applies transformation
## 

analyzeIncidents <- function(x_file = character()) {
    
	library(ggplot2)
	library(grDevices)
    library(scales)
    library(reshape2)
	
    if(!file.exists(x_file)){
		stop("invalid file")
	}
	## read the defects file
	fl_incidents <- read.csv(x_file, stringsAsFactors=FALSE)

    fl_incidents <- convertToFactors(fl_incidents)
	
	#Plot incident Open and Close trend by date
    g <- ggplot(data=fl_incidents, aes(x=as.Date(open_date, "%d-%m-%Y"))) + geom_line(stat="bin", binwidth=7, col="red") + geom_line(data=fl_incidents, mapping=aes(x=as.Date(close_date, "%d-%m-%Y")), stat="bin", binwidth=7, lty=2, col="blue") + scale_x_date(labels=date_format("%d-%b"), breaks=date_breaks("week"))
    
    g <- g + labs(x="Date", y="Count of Incidents", title="CCUW Incidents - Open and Close Counts")
    g <- g + theme_bw() + theme(axis.text.x = element_text(size=8, angle=45))
    g_incident_trend <- g
    
    
    #Plot the cumulative graphs for incidents open and close
    g <- ggplot(data=fl_incidents, aes(x=as.Date(open_date, "%d-%m-%Y"))) + stat_bin(aes(y=cumsum(..count..)), geom="line", binwidth=7,col="red") + stat_bin(aes(x=as.Date(close_date,"%d-%m-%Y"), y=cumsum(..count..)), geom="line",binwidth=7, lty=2, col="blue") + scale_x_date(labels=date_format("%d-%b"), breaks=date_breaks("week")) + scale_y_continuous(limits=c(0,1.1*nrow(fl_incidents)))
    g <- g + labs(x="Date", y="Cumulative Incidents", title="CCUW Incidents - Open and Close Counts (Cumulative Basis)")
    g <- g + theme_bw() + theme(axis.text.x = element_text(size=8, angle=45))

    g_cum_incident_trend <- g
    
    x_incidents <<- fl_incidents


	fileName <- paste("CCUW_Incidents",strftime(Sys.time(),"%d%b%y-%H%M"),".pdf", sep="")
	if(file.exists(fileName)){
		file.remove(fileName)
	}

	pdf(fileName)
       	
		print(g_incident_trend)
        print(g_cum_incident_trend)
    
	dev.off()
    
}

## This function takes the incidents dataframe and converts some of the key columns into factors
## This function has to be called after the column names are modified appropriately - else it will not work
convertToFactors <- function(fl_incidents){
	
	if(!(class(fl_incidents)=="data.frame")){
		stop("Invalid input - this function expects defects as a dataframe")
	}

    fl_incidents$inc_state <- as.factor(fl_incidents$inc_state)
    fl_incidents$escalation <- as.factor(fl_incidents$escalation)
    
    #Adding a new columns for date processing
    fl_incidents$open_date <- strptime(fl_incidents$reported_date, "%Y/%m/%d %H:%M:%S")
    fl_incidents$close_date <- strptime(fl_incidents$resolved_time, "%Y/%m/%d %H:%M:%S")
    fl_incidents$res_hrs <- difftime(fl_incidents$close_date, fl_incidents$open_date, units="hours")
    
    fl_incidents$res_hrs <- as.numeric(fl_incidents$res_hrs)
    
    return(fl_incidents)
}

## This function takes a) incidents dataframe, b) Parameter for criteria c) List of values for parameter
## It then returns the subset of the dataframe based on the parameter and the values
getSubset <- function(fl_incidents=data.frame(), parameter = character(), param_values=character()){

	if(length(parameter)<1){
		stop("Insufficient Parameters")
	} 
	
	## Get the index corresponding to the parameters
	c1 <- grep(parameter,names(fl_incidents))

	## Get subset of fl_incidents by the parameter and the param_values
	sub_fl_incidents <- subset(fl_incidents, fl_incidents[,c1] %in% param_values)
    sub_fl_incidents[,c1] <- as.factor(as.character(sub_fl_incidents[,c1]))

    return(sub_fl_incidents)
}
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
    library(stringr)
	
    if(!file.exists(x_file)){
		stop("invalid file")
	}
	## read the defects file
	fl_incidents <- read.csv(x_file, stringsAsFactors=FALSE)

    fl_incidents <- convertToFactors(fl_incidents)
    fl_incidents <- splitDescriptionData(fl_incidents)
	
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
    
    
    #Plot the distribution of transactions by date
    txn_list <- c("REN","ENDO","NB","ISSUE","RB","BIND","QUOTE","PC","CBOR")
    fl_txn_incidents <- getSubset(fl_incidents,"pol_txn", txn_list)
    g <- ggplot(data=fl_txn_incidents, aes(x=as.Date(open_date,"%d-%b-%Y"),fill=pol_txn)) + geom_bar(stat="bin", binwidth=7) + scale_x_date(labels=date_format("%d-%b"), breaks=date_breaks("week"))
    g <- g + labs(x="Date", y="Count of Incidents", title="CCUW Incidents - By Policy Transaction")
    g <- g + theme_bw() + theme(axis.text.x = element_text(size=7, angle=45), legend.text=element_text(size=7), legend.title=element_text(size=9))
    g_txn_incidents <- g

    
    x_incidents <<- fl_incidents


	fileName <- paste("CCUW_Incidents",strftime(Sys.time(),"%d%b%y-%H%M"),".pdf", sep="")
	if(file.exists(fileName)){
		file.remove(fileName)
	}

	pdf(fileName)
       	
		print(g_incident_trend)
        print(g_cum_incident_trend)
        print(g_txn_incidents)
    
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

## This function analyses the description data and splits it into several columns for greater insights
## Uses the str_trim function from the stringr library to trim white spaces
splitDescriptionData <- function(fl_incidents=data.frame()){
    
    #Inidialize all variables
    fl_usr <- character()
    mf_id <- character()
    dvn <- character()
    pol_txn <- character()
    acct_nm <- character()
    sub_no <- character()
    scr_func <- character()
    act <- character()
    errMsg <- character()
    mth_clsng <- character()
    rush <- character()
    descr <- character()
    
    for(i in 1:nrow(fl_incidents)){
        
        desc <- fl_incidents$description[i]
        
        if (grepl("^Are",desc)){
            
            #Splits the description into list with individual lines with questions
            ls_desc <- strsplit(desc, split="\n")

            #FL User - Yes or No
            desc1 <- strsplit(ls_desc[[1]][1], split=":")
            fl_usr[i] <- str_trim(desc1[[1]][2],side="both")
            
            # Mainframe id of the user
            desc1 <- strsplit(ls_desc[[1]][2], split=":")
            mf_id[i] <- str_trim(desc1[[1]][2], side="both")
            
            # Division of the user
            desc1 <- strsplit(ls_desc[[1]][3], split=":")
            dvn[i] <- str_trim(desc1[[1]][2], side="both")
            
            # Policy Transaction when incident occurred
            desc1 <- strsplit(ls_desc[[1]][4], split=":")
            pol_txn[i] <- str_trim(desc1[[1]][2], side="both")
            
            # Account Name for the impacted account
            desc1 <- strsplit(ls_desc[[1]][5], split=":")
            acct_nm[i] <- str_trim(desc1[[1]][2], side="both")
            
            # Submission Number
            desc1 <- strsplit(ls_desc[[1]][6], split=":")
            sub_no[i] <- str_trim(desc1[[1]][2], side="both")
            
            # Function on the screen navigation
            desc1 <- strsplit(ls_desc[[1]][7], split=":")
            scr_func[i] <- str_trim(desc1[[1]][2], side="both")
            
            # Action performed, when the error was encountered
            desc1 <- strsplit(ls_desc[[1]][8], split=":")
            act[i] <- str_trim(desc1[[1]][2], side="both")
            
            # error message displayed on the screen
            desc1 <- strsplit(ls_desc[[1]][9], split=":")
            errMsg[i] <- str_trim(desc1[[1]][2], side="both")
            
            #Is it a month closing ticket?
            desc1 <- strsplit(ls_desc[[1]][10], split=":")
            mth_clsng[i] <- str_trim(desc1[[1]][2], side="both")
            
            # Is it a rush ticket?
            desc1 <- strsplit(ls_desc[[1]][11], split=":")
            rush[i] <- str_trim(desc1[[1]][2], side="both")
            
            #Put all the remaining lines in descr column
            descr[i] <- str_trim(ls_desc[[1]][13:length(ls_desc[[1]])], side="both")
            
            
        } else {
            #Response is not formatted and in free text.
            descr[i] <- desc
        }
    }
    
    #Make a few replacements to ensure data is consistent
    
    ren_list <- grep("ren|rn|renewal|renw|Reenewal|Ren|Renewal|RENEWAL|RN|Reb", pol_txn)
    pol_txn <- replace(pol_txn,ren_list,"REN")
    
    endo_list <- grep("endo|END|Endo|Endorsement|ENDORSE|MIDTERM|ENDP|end|endt", pol_txn)
    pol_txn <- replace(pol_txn,endo_list,"ENDO")
    
    nb_list <- grep("nb|Business|BUSINESS|Biz|New|Na|new|bsuiness", pol_txn)
    pol_txn <- replace(pol_txn,nb_list,"NB")
    
    issue_list <- grep("ISSUANCE|ISSUE|Issue|issue", pol_txn)
    pol_txn <- replace(pol_txn,issue_list,"ISSUE")

    rb_list <- grep("Revise|revise", pol_txn)
    pol_txn <- replace(pol_txn,rb_list,"RB")

    bind_list <- grep("BIND|Bind|Bound|bind", pol_txn)
    pol_txn <- replace(pol_txn,bind_list,"BIND")

    quote_list <- grep("quote|Quote|Finalize", pol_txn)
    pol_txn <- replace(pol_txn,quote_list,"QUOTE")

    pc_list <- grep("Correction|pc|corr|correction", pol_txn)
    pol_txn <- replace(pol_txn,pc_list,"PC")

    cbor_list <- grep("BOR|broker|CR", pol_txn)
    pol_txn <- replace(pol_txn,cbor_list,"CBOR")

    #Add all he columns to the dataframe
    fl_incidents$fl_usr <- fl_usr
    fl_incidents$mf_id <- mf_id
    fl_incidents$dvn <- as.factor(dvn)
    fl_incidents$pol_txn <- as.factor(pol_txn)
    fl_incidents$acct_nm <- acct_nm
    fl_incidents$sub_no <- sub_no
    fl_incidents$scr_func <- scr_func
    fl_incidents$act <- act
    fl_incidents$errMsg <- errMsg
    fl_incidents$mth_clsng <- mth_clsng
    fl_incidents$rush <- rush
    fl_incidents$descr <- descr
    
    return(fl_incidents)
}


## The objective of these sets of functions are to automate the process of analysis of defects
## First we will read the defects from a CSV file into a data frame. We will then transform the 
## the data frame to give us the necessary critical insights. 
## All graphs must be generated at the click of a button.
##
##
## Each function is documented below in detail to highlight its specific responsibility.
## 
##
## 

## This function - processDefects is the main function that takes the file name as input and applies transformation
## 

processCASDefects <- function(x_file = character()) {
    
	library(ggplot2)
	library(reshape2)
	library(scales)
	
	defects <- data.frame()
	if(!file.exists(x_file)){
		stop("invalid file")
	}
	## read the defects file
	defects <- read.csv(x_file, stringsAsFactors=FALSE)
	## set the column names appropriately
	defects <- defineColNames(defects)
	
	defects <- convertToFactors(defects)
	
	## Adopt Release Labels based on dates
 	#defects$StartDate <- as.Date(defects$DetectedDate, "%m/%d/%Y")
	#defects$DateLevels <- cut(defects$StartDate, c(as.Date("12-09-2014","%d-%m-%Y"),as.Date("05-12-2014","%d-%m-%Y"),as.Date("01-03-2015","%d-%m-%Y"),as.Date("23-05-2015","%d-%m-%Y"),as.Date("03-07-2015","%d-%m-%Y")))
	#defects$DateLabels <- cut(defects$StartDate, c(as.Date("12-09-2014","%d-%m-%Y"),as.Date("05-12-2014","%d-%m-%Y"),as.Date("01-03-2015","%d-%m-%Y"),as.Date("23-05-2015","%d-%m-%Y"),as.Date("03-07-2015","%d-%m-%Y")), labels=c("R5.0","R5.5","R6.0","R6.5"))
	#defects <- subset(defects, DetectedEnv %in% c("QA","UAT","Model","Production"))
	#defects <- subset(defects, IssueType == "Bug")


	## Get the defects counts by Severity and ones in Open Status
	status_codes <- c("New", "Open", "Analysis Complete","Blocked", "Deferred", "Deferred - Requirement Change", "Dev In Process", "Duplicate", "Need more info", "Reopen","Retest","Requirement Change")

	all_status_codes <- levels(defects$DefectStatus)
	
	resolved_status_codes <- setdiff(all_status_codes, status_codes)
     
	severity <- levels(defects$Severity) 
		
	xtab_S_St <<- getXParamCrossTab(defects, status_codes, c("DefectStatus","Severity"), confidence=1.01, col_confidence=1.01)
	df_S_St <<- melt(xtab_S_St)
	colnames(df_S_St)[1] <- "DefectStatus"
	colnames(df_S_St)[2] <- "Severity"
	colnames(df_S_St)[3] <- "Defect_Count"
	g <- ggplot(data=df_S_St, aes(x=DefectStatus, y=Defect_Count, fill=Severity)) + geom_bar(stat="identity", position="dodge")
	g <- g + scale_fill_manual(values=c("Level 1 - Critical" = "red", "Level 2 - Urgent" = "yellow", "Level 3 - High" = "blue"))
	g <- g + labs(x="Defect Status",y="Count of Defects", title="Casualty - Open Defects by Status")
    g <- g + theme_bw() + theme(axis.text.x = element_text(size=8, angle=45), legend.text = element_text(size=7), legend.title=element_text(size=9))
	g_CAS_OpenDefects <- g	
	
	xtab_S_St_res <<- getXParamCrossTab(defects, resolved_status_codes, c("DefectStatus","Severity"), confidence=1.01, col_confidence=1.01)
	df_S_St_res <<- melt(xtab_S_St_res)
	colnames(df_S_St_res)[1] <- "DefectStatus"
	colnames(df_S_St_res)[2] <- "Severity"
	colnames(df_S_St_res)[3] <- "Defect_Count"
	g <- ggplot(data=df_S_St_res, aes(x=DefectStatus, y=Defect_Count, fill=Severity)) + geom_bar(stat="identity", position="dodge")
	g <- g + scale_fill_manual(values=c("Level 1 - Critical" = "red", "Level 2 - Urgent" = "yellow", "Level 3 - High" = "blue"))
	g <- g + labs(x="Defect Status",y="Count of Defects", title="Casualty - Resolved Defects by Status")
    g <- g + theme_bw() + theme(axis.text.x = element_text(size=8, angle=45), legend.text = element_text(size=7), legend.title=element_text(size=9))
	g_CAS_ResolvedDefects <- g	
	
	
	xtab_FA_TA <<- getXParamCrossTab(defects, status_codes, c("FunctionalArea","TechnicalArea"), confidence=0.90, col_confidence=0.96)
	df_FA_TA <<- melt(xtab_FA_TA)
	colnames(df_FA_TA)[1] <- "FunctionalArea"
	colnames(df_FA_TA)[2] <- "TechnicalArea"
	colnames(df_FA_TA)[3] <- "Defect_Count"
	g <- ggplot(data=df_FA_TA, aes(x=TechnicalArea, y=Defect_Count, fill=FunctionalArea)) + geom_bar(stat="identity", position="stack")
	g <- g + labs(x="Technical Area",y="Count of Defects (Functional Area)", title="Casualty - Open Defects by Functional / Technical Area")
    g <- g + theme_bw() + theme(axis.text.x = element_text(size=8, angle=45), legend.text = element_text(size=7), legend.title=element_text(size=9))
	g_CAS_FA_TA_OpenDefects <- g	
	
	xtab_RCA_Env <<- getXParamCrossTab(defects, resolved_status_codes,c("RCACode","DetectedEnv"), confidence=1.01, col_confidence=1.01)
	df_RCA_Env <<- melt(xtab_RCA_Env)
	colnames(df_RCA_Env)[1] <- "RCACode"
	colnames(df_RCA_Env)[2] <- "DetectedEnv"
	colnames(df_RCA_Env)[3] <- "Defect_Count"
	g <- ggplot(data=df_RCA_Env, aes(x=DetectedEnv, y=Defect_Count, fill=RCACode)) + geom_bar(stat="identity", position="stack")
	g <- g + labs(x="Detected Environment",y="Count of Defects (RCA Code)", title="Casualty-Resolved Defects(RCA Code and Environment)")
    g <- g + theme_bw() + theme(axis.text.x = element_text(size=8, angle=45), legend.text = element_text(size=7), legend.title=element_text(size=9))
	g_CAS_RCA_Env_ResolvedDefects <- g	
	
	xtab_RCA_DS <<- getXParamCrossTab(defects, resolved_status_codes,c("DefectSource","RCACode"), confidence=1.01, col_confidence=1.01)
	df_RCA_DS <<- melt(xtab_RCA_DS)
	colnames(df_RCA_DS)[1] <- "DefectSource"
	colnames(df_RCA_DS)[2] <- "RCACode"
	colnames(df_RCA_DS)[3] <- "Defect_Count"
	g <- ggplot(data=df_RCA_DS, aes(x=RCACode, y=Defect_Count, fill=DefectSource)) + geom_bar(stat="identity", position="stack")
	g <- g + labs(x="RCA Code",y="Count of Defects (Defect Source)", title="Casualty - Resolved Defects by RCA Code and Defect Source")
    g <- g + theme_bw() + theme(axis.text.x = element_text(size=8, angle=45), legend.text = element_text(size=7), legend.title=element_text(size=9))
	g_CAS_RCA_DS_ResolvedDefects <- g	
	
	temp_defects <<- defects
	
	#Plot the cumulative graphs for Defects open and close
    g <- ggplot(data=defects, aes(x=as.Date(DetectedDate, "%m/%d/%Y"))) + stat_bin(aes(y=cumsum(..count..)), geom="line", binwidth=21,col="red") + stat_bin(aes(x=as.Date(ClosingDate,"%m/%d/%Y"), y=cumsum(..count..)), geom="line",binwidth=21, lty=2, col="blue") + scale_x_date(labels=date_format("%d-%b"), breaks=date_breaks("3 weeks")) + scale_y_continuous(limits=c(0,1.1*nrow(defects)))
    g <- g + labs(x="Date", y="Cumulative Defects", title="CAS Defects - Open and Close Counts (Cumulative Basis)")
	g <- g + geom_text(aes(x=as.Date(DetectedDate,"%m/%d/%Y"), y=cumsum(..count..),label=cumsum(..count..)),stat="bin", size=2)
	g <- g + geom_text(aes(x=as.Date(ClosingDate,"%m/%d/%Y"), y=cumsum(..count..),label=cumsum(..count..)),stat="bin", size=2)
    g <- g + theme_bw() + theme(axis.text.x = element_text(size=8, angle=45))
    g_cum_defect_trend <- g

	fileName <- paste("CCUW_CAS_Defects",strftime(Sys.time(),"%d%b%y-%H"),".pdf", sep="")
	if(file.exists(fileName)){
		file.remove(fileName)
	}

	pdf(fileName)
	par("cex"=0.8,"cex.axis"=0.8)
	
	print(g_CAS_OpenDefects)
	print(g_CAS_ResolvedDefects)
	print(g_CAS_FA_TA_OpenDefects)
	print(g_CAS_RCA_Env_ResolvedDefects)
	print(g_CAS_RCA_DS_ResolvedDefects)
	print(g_cum_defect_trend)

	dev.off()

}


## This function sets the correct column names that are easy to understand and use
## The function assumes that file is as per a standard template derived from HPQC

defineColNames <- function(defects) {
	if(!(class(defects) =="data.frame")){
		stop("Invalid input - this function expects defects data as a dataframe")
	}
	
	## Set all the column names individually
	colnames(defects)[1] <- "DefectID"
	colnames(defects)[2] <- "DetectedDate"
	colnames(defects)[3] <- "TargetFixDate"
	colnames(defects)[4] <- "DefectStatus"
	colnames(defects)[5] <- "Severity"
	colnames(defects)[6] <- "Priority"
	colnames(defects)[7] <- "AssignedTo"
	colnames(defects)[8] <- "ScrumTeam"
	colnames(defects)[9] <- "BusinessDescription"
	colnames(defects)[10] <- "TechnicalArea"
	colnames(defects)[11] <- "Title"
	colnames(defects)[12] <- "RelatedDefectID"
	colnames(defects)[13] <- "Product"
	colnames(defects)[14] <- "ResolutionRelNo"
	colnames(defects)[15] <- "DetectedEnv"
	colnames(defects)[16] <- "SpecialTag"
	colnames(defects)[17] <- "DetectedBy"
	colnames(defects)[18] <- "IssueType"
	colnames(defects)[19] <- "Coverages"
	colnames(defects)[20] <- "FunctionalArea"
	colnames(defects)[21] <- "FeatureArea"
 	colnames(defects)[22] <- "ESystem"
	colnames(defects)[23] <- "TransactionType"
	colnames(defects)[24] <- "ClosingDate"
	colnames(defects)[25] <- "PriorityReviewed"
	colnames(defects)[26] <- "DefectCategory"
	colnames(defects)[27] <- "DevCommitFixDate"
	colnames(defects)[28] <- "State"
	colnames(defects)[29] <- "ResolutionBuildNo"
	colnames(defects)[30] <- "Project"
	colnames(defects)[31] <- "UATDefectID"
	colnames(defects)[32] <- "DefectSource"
	colnames(defects)[35] <- "ActualFixDate"
	colnames(defects)[36] <- "CodeReviewed"
	colnames(defects)[37] <- "DetectedInCycle"
	colnames(defects)[38] <- "DetectedInRelease"
	colnames(defects)[39] <- "FormID"
	colnames(defects)[40] <- "FormType"
	colnames(defects)[41] <- "IncidentTicketCount"
	colnames(defects)[42] <- "CreateDate"
	colnames(defects)[43] <- "AnalysisCompleteDate"
	colnames(defects)[44] <- "DevCompleteDate"
	colnames(defects)[45] <- "MassDMLRequired"
	colnames(defects)[46] <- "RCACode"
	colnames(defects)[47] <- "RCADescription"
	colnames(defects)[48] <- "RCAUpdateComments"
	colnames(defects)[49] <- "UATCycle"
	
	return(defects)
}

## This function takes the defects data frame and converts some of the key columns into factors
## This function has to be called after the column names are modified appropriately - else it will not work
convertToFactors <- function(defects){
	
	if(!(class(defects)=="data.frame")){
		stop("Invalid input - this function expects defects as a dataframe")
	}

	defects$DefectStatus <- as.factor(defects$DefectStatus)
	defects$Severity <- as.factor(defects$Severity)
	defects$Priority <- as.factor(defects$Priority)
	defects$TechnicalArea <- as.factor(defects$TechnicalArea)
	defects$Product <- as.factor(defects$Product)
	defects$DetectedEnv <- as.factor(defects$DetectedEnv)
	defects$FunctionalArea <- as.factor(defects$FunctionalArea)
	defects$IssueType <- as.factor(defects$IssueType)
	defects$TransactionType <- as.factor(defects$TransactionType)
	defects$DefectCategory <- as.factor(defects$DefectCategory)
	defects$DefectSource <- as.factor(defects$DefectSource)
	defects$RCACode <- as.factor(defects$RCACode)
	
	return(defects)
}

## This function takes a) defects dataframe, b) status codes c) list of 2 parameters and returns a cross tab based on the two parameters 
## If more than 2 parameters are provided, the function takes only the first 2 parameters

getXParamCrossTab <- function(defects=data.frame(), status_codes = character(), parameters=character(), confidence=1.01,col_confidence=1.01){

	if(length(parameters)<2){
		stop("Insufficient Parameters")
	}

	## Get subset of defects by the status codes
	sub_defects <- subset(defects, defects$DefectStatus %in% status_codes)

	## Get the index corresponding to the parameters
	c1 <- grep(parameters[1],names(sub_defects))
	c2 <- grep(parameters[2], names(sub_defects))	

	df_xtab <- xtabs(~ sub_defects[,c1] + sub_defects[,c2], data=sub_defects, exclude=c("","<<None>>"))
	df_xtab <- processXTab(df_xtab, confidence, col_confidence)
     return(df_xtab)
}

## This function takes a cross tab and performs the following:
## a) Sorts by rows and columns, b) adds row sum and column sum, c) adds percentage values and d) adds cumulative percentage
## This function will work only if a cross tab is passed in the argument

processXTab <- function(df_xtab, confidence=1.01, col_confidence=1.01){

	xrow <- nrow(df_xtab)
	xcol <- ncol(df_xtab)

	## Add the row sum and sort in descending order. Then remove the row sum column
	row_sum <- apply(df_xtab,1,sum)
	df_xtab <- cbind(df_xtab, row_sum)
	df_xtab <- df_xtab[order(df_xtab[,xcol+1],decreasing=TRUE),]
	df_xtab <- df_xtab[,-(xcol+1)]

	## Add the col sum row and sort in descending order. Then delete teh col sum row
	col_sum <- apply(df_xtab,2,sum)
	df_xtab <- rbind(df_xtab, col_sum)
	df_xtab <- df_xtab[,order(df_xtab[xrow+1,],decreasing=TRUE)]

	## Add back the row sum
	row_sum <- apply(df_xtab, 1, sum)
	df_xtab <- cbind(df_xtab, row_sum)

	## Compute and add row percentages
	row_pct <- df_xtab[,"row_sum"]/df_xtab["col_sum","row_sum"]
	df_xtab <- cbind(df_xtab, row_pct)

	if(row_pct[1]>=confidence){
		confidence <- row_pct[1]
	}

	## Compute and add row cumulative percentages
	row_cumSum <- cumsum(df_xtab[,"row_pct"])
	df_xtab <- cbind(df_xtab, row_cumSum)

	df_xtab <- df_xtab[df_xtab[,"row_cumSum"]<=confidence,]
	
	## Compute and add Col sums / Percentage / Cumulative Percent to reduce the number of columns
	
	col_sum <- apply(df_xtab,2,sum)
	col_pct <- col_sum/col_sum[length(col_sum)-2]
	col_pct <- col_pct[1:(length(col_pct)-3)]

	if(col_pct[1] >= col_confidence){
		col_confidence <- 1.0
	}
	
	cum_col_pct <- cumsum(col_pct)
	
	index <- length(cum_col_pct[cum_col_pct<=col_confidence])

	df_xtab <- as.matrix(df_xtab[,1:index])
	
	return(df_xtab)
}
## This function takes a) defects dataframe, b) Parameter for criteria c) List of values for parameter 
## It then returns the subset of the dataframe based on the parameter and the values
getSubset <- function(defects=data.frame(), parameter = character(), param_values=character()){

	if(length(parameter)<1){
		stop("Insufficient Parameters")
	} 
	
	## Get the index corresponding to the parameters
	c1 <- grep(parameter,names(defects))

	## Get subset of defects by the parameter and the param_values
	sub_defects <- subset(defects, defects[,c1] %in% param_values)
	sub_defects[,c1] <- as.factor(as.character(sub_defects[,c1]))

    return(sub_defects)
}
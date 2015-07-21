## The objective of these sets of functions are to automate the process of analysis of defects
## First we will read the defects from a CSV file into a data frame. We will then transform the 
## the data frame to give us the necessary critical insights. 
## All graphs must be generated at the click of a button.
##
##
## Each function is documented below in detail to highlight its specific responsibility.
##

## This function - processGELSDefects is the main function that takes the file name as input and applies transformation
## 

processGELSDefects <- function(x_file = character()) {
    
	library(ggplot2)
	library(grDevices)
	
	defects <- data.frame()
	if(!file.exists(x_file)){
		stop("invalid file")
	}
	## read the defects file
	defects <- read.csv(x_file, stringsAsFactors=FALSE)
	## set the column names appropriately
	defects <- defineColNames(defects)
	
	defects <- convertToFactors(defects)
	
	## Get the defects counts by Severity and ones in Open Status
	status_codes <- c("New", "Open", "Deferred","Ready for QA", "Reopen","Re-Test")

	all_status_codes <- levels(defects$DefectStatus)
	
	resolved_status_codes <- setdiff(all_status_codes, status_codes)
     
	severity <- levels(defects$Severity) 
		
	#Split the data set by open defects and resolved defects
	open_defects <- getSubset(defects,"DefectStatus",status_codes)
	resolved_defects <- getSubset(defects, "DefectStatus", resolved_status_codes)
	
	#Plot open defects by status and severity
	g <- ggplot(data=open_defects, aes(x=DefectStatus, fill=Severity)) + geom_bar(stat="bin", position="stack") + facet_grid(.~Module)
	g <- g + geom_text(aes(label=..count..), stat="bin", position="stack",size=2)
	g <- g + labs(x="Defect Status", y="Defect Count", title="Open Defects by Module")
	g <- g + theme_bw() + theme(axis.text.x = element_text(size=8, angle=45), legend.text = element_text(size=7), legend.title=element_text(size=9))
	
	g_openDefects_status_sev <- g
		
	#Plot Defect Status by Module for Resolved Defects
	g <- ggplot(data=resolved_defects, aes(x=DefectStatus, fill=Severity)) + geom_bar(stat="bin",position="stack") + facet_grid(.~Module)
	g <- g + geom_text(aes(label=..count..), stat="bin", position="stack",size=2)
	g <- g + labs(x="Defect Status", y="Defect Count", title="Resolved Defects by Module")
	g <- g + theme_bw() + theme(axis.text.x = element_text(size=8, angle=45), legend.text = element_text(size=7), legend.title=element_text(size=9))
	
	g_resolvedDefects_status_Module <- g
	
	# Plot Resolved Defects by Resolution Type and Module
	g <- ggplot(data=resolved_defects, aes(x=Severity, fill=ResolutionType))	+ geom_bar() + facet_grid(.~Module)
	g <- g + geom_text(aes(label=..count..), stat="bin", position="stack", size=2)
	g <- g + labs(x="Severity", y="Defect Count", title="Resolved Defects by Resolution Type")
	g <- g + theme_bw() + theme(axis.text.x = element_text(size=8, angle=45), legend.text = element_text(size=7), legend.title=element_text(size=9))
	
	g_resolutionType_Module <- g
	
	# Plot Defect Source by Module (for all defects)
	g <- ggplot(data=resolved_defects, aes(x=Severity, fill=DefectSource)) + geom_bar() + facet_grid(.~Module)
	g <- g + geom_text(aes(label=..count..), stat="bin", position="stack", size=2)
	g <- g + labs(x="Severity",y="Defect Count", title="Defect Source by Module (Resolved defects)")
	g <- g + theme_bw() + theme(axis.text.x = element_text(size=8, angle=45), legend.text = element_text(size=7), legend.title=element_text(size=9))
	
	g_defectSource_Module <- g
	
	fileName <- paste("GELS_Defects_Analysis",strftime(Sys.time(),"%d%b%y-%H%M"),".pdf", sep="")
	if(file.exists(fileName)){
		file.remove(fileName)
	}

	pdf(fileName)

		print(g_openDefects_status_sev)
		print(g_resolvedDefects_status_Module)
		print(g_resolutionType_Module)
		print(g_defectSource_Module)
		
	dev.off()
}


## This function sets the correct column names that are easy to understand and use
## The function assumes that file is as per a standard template derived from HPQC

defineColNames <- function(defects) {
	if(!(class(defects) =="data.frame")){
		stop("Invalid input - this function expects defects data as a dataframe")
	}
	
	## Set all the column names individually
	colnames(defects)[5] <- "DefectID"
	colnames(defects)[20] <- "TargetFixDate"
	colnames(defects)[16] <- "DefectStatus"
	colnames(defects)[17] <- "Severity"
	colnames(defects)[6] <- "Priority"
	colnames(defects)[4] <- "AssignedTo"
	colnames(defects)[8] <- "BusinessDescription"
	colnames(defects)[7] <- "Title"
	colnames(defects)[1] <- "Product"
	colnames(defects)[3] <- "DetectedEnv"
	colnames(defects)[11] <- "DetectedBy"
	colnames(defects)[10] <- "DefectType"
	colnames(defects)[15] <- "DefectSource"
	colnames(defects)[14] <- "ResolutionType"
	colnames(defects)[13] <- "ResolutionSummary"
	colnames(defects)[18] <- "TestPhase"
	
	return(defects)
}

## This function takes the defects dataframe and converst some of the key columns into factors
## This function has to be called after the column names are modified appropriately - else it will not work
convertToFactors <- function(defects){
	
	if(!(class(defects)=="data.frame")){
		stop("Invalid input - this function expects defects as a dataframe")
	}

	defects$DefectStatus <- as.factor(defects$DefectStatus)
	defects$Severity <- as.factor(defects$Severity)
	defects$Priority <- as.factor(defects$Priority)
	
	defects$DetectedEnv <- as.factor(defects$DetectedEnv)

	defects$DefectType <- as.factor(defects$DefectType)
	defects$DefectSource <- as.factor(defects$DefectSource)
	defects$ResolutionType <- as.factor(defects$ResolutionType)
	
	return(defects)
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
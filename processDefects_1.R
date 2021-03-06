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

processDefects <- function(x_file = character()) {
    
	defects <- data.frame()
	if(!file.exists(x_file)){
		stop("invalid file")
	}
	## read the defects file
	defects <- read.csv(x_file, stringsAsFactors=FALSE)
	## set the column names appropriately
	defects <- defineColNames(defects)
	
	defects <- convertToFactors(defects)
	
	levels(defects$Product)[84] <- "PS"
	levels(defects$Product)[8] <- "CGC"

	## Adopt Release Labels based on dates
 	defects$StartDate <- as.Date(defects$DetectedDate, "%m/%d/%Y")
	defects$DateLevels <- cut(defects$StartDate, c(as.Date("12-09-2014","%d-%m-%Y"),as.Date("05-12-2014","%d-%m-%Y"),as.Date("01-03-2015","%d-%m-%Y"),as.Date("23-05-2015","%d-%m-%Y"),as.Date("03-07-2015","%d-%m-%Y")))
	defects$DateLabels <- cut(defects$StartDate, c(as.Date("12-09-2014","%d-%m-%Y"),as.Date("05-12-2014","%d-%m-%Y"),as.Date("01-03-2015","%d-%m-%Y"),as.Date("23-05-2015","%d-%m-%Y"),as.Date("03-07-2015","%d-%m-%Y")), labels=c("R5.0","R5.5","R6.0","R6.5"))
	defects <- subset(defects, DetectedEnv %in% c("QA","UAT","Model","Production"))
	defects <- subset(defects, IssueType == "Bug")


	## Get the defects counts by Severity and ones in Open Status
	status_codes <- c("New", "Open", "Analysis Complete","Blocked", "Deferred", "Deferred - Requirement Change", "Dev In Process", "Duplicate", "Need more info", "Reopen","Retest","Requirement Change")

	all_status_codes <- levels(defects$DefectStatus)
	
	resolved_status_codes <- setdiff(all_status_codes, status_codes)
     
	severity <- levels(defects$Severity) 
		
	xtab_S_St <<- getXParamCrossTab(defects, status_codes, c("DefectStatus","Severity"), confidence=1.01, col_confidence=1.01)

	xtab_FA_TA <<- getXParamCrossTab(defects, all_status_codes, c("FunctionalArea","TechnicalArea"), confidence=0.9, col_confidence=0.99)

	xtab_FA_P <<- getXParamCrossTab(defects, all_status_codes, c("FunctionalArea","Product"), confidence=0.8, col_confidence=0.9)

	xtab_TA_P <<- getXParamCrossTab(defects, all_status_codes, c("TechnicalArea","Product"), confidence=0.91, col_confidence=0.91)

	xtab_RCA_Env <<- getXParamCrossTab(defects, resolved_status_codes,c("RCACode","DetectedEnv"), confidence=0.96, col_confidence=0.995)

	xtab_RCA_DS <<- getXParamCrossTab(defects, resolved_status_codes,c("DefectSource","RCACode"), confidence=0.99, col_confidence=0.95)

	xtab_RCA_P <<- getXParamCrossTab(defects, resolved_status_codes,c("RCACode","Product"), confidence=0.90, col_confidence=0.85)

	xtab_DS_P <<- getXParamCrossTab(defects, resolved_status_codes,c("DefectSource","Product"), confidence=0.99, col_confidence=0.90)

temp_defects <<- defects
	
	ls_cumDefects <- cumulativeDefects(defects)
	defOpen <- ls_cumDefects[["cumOpen"]]
	defClose <- ls_cumDefects[["cumClose"]]

	fileName <- "CCUW_Defects_Analysis_7Jul15.pdf"
	if(file.exists(fileName)){
		file.remove(fileName)
	}

	pdf(fileName)
	par("cex"=0.8,"cex.axis"=0.8)
	
## Plot the first graph with Status and Severity
	n_colors <- nrow(xtab_S_St)
	n_columns <- ncol(xtab_S_St)
	
	cols <- c("yellow","red")
	
	barplot(t(xtab_S_St), ylim=c(0,1.2*max(xtab_S_St[,1])), main="Defect Status and Severity", xlab="Defect Status", ylab="Defect Count", axes=TRUE,cex.axis=par("cex"), cex.names=par("cex.axis"), beside=TRUE,col=cols,legend=TRUE)
	
	grid( round(max(xtab_S_St[,1])*1.1/20,0),lty=1,lwd=0.5)

	barplot(t(xtab_S_St), ylim=c(0,1.2*max(xtab_S_St[,1])), main="Defect Status and Severity", xlab="Defect Status", ylab="Defect Count", axes=TRUE,cex.axis=par("cex"), cex.names=par("cex.axis"), beside=TRUE,col=cols,legend=TRUE,add=TRUE)

	box()


## Plot the second graph with Functional Area and Technical Area

	n_colors <- nrow(xtab_FA_TA)
	n_columns <- ncol(xtab_FA_TA)
	
	#cols <- colors()[sample(450:550,n_colors)]
	cols <- rainbow(n_colors)
	
	barplot(xtab_FA_TA, ylim=c(0,1.2*sum(xtab_FA_TA[,1])), main="Defects by Functionality and Technology", xlab="Technical Area", ylab="Defect Count", axes=TRUE,cex.axis=par("cex"), cex.names=par("cex.axis"), beside=FALSE,col=cols,legend=TRUE)
	
	grid( round(max(xtab_FA_TA[,1])/20,0),lty=1,lwd=0.5)

	barplot(xtab_FA_TA, ylim=c(0,1.2*sum(xtab_FA_TA[,1])), main="Defects by Functionality and Technology", xlab="Technical Area", ylab="Defect Count", axes=TRUE,cex.axis=par("cex"), cex.names=par("cex.axis"), beside=FALSE,col=cols,legend=TRUE, add=TRUE)

	box()

## Plot the third graph with Functional Area and Product

	n_colors <- nrow(xtab_FA_P)
	n_columns <- ncol(xtab_FA_P)

	cols <- rainbow(n_colors)

	barplot(xtab_FA_P, ylim=c(0,1.2*sum(xtab_FA_P[,1])), main="Defects by Functionality and Product", xlab="Product", ylab="Defect Count", axes=TRUE,cex.axis=par("cex"), cex.names=par("cex.axis"), beside=FALSE,col=cols,legend=TRUE)
	
	grid( round(max(xtab_FA_P[,1])/25,0),lty=1,lwd=0.5)

	barplot(xtab_FA_P, ylim=c(0,1.2*sum(xtab_FA_P[,1])), main="Defects by Functionality and Product", xlab="Product", ylab="Defect Count", axes=TRUE,cex.axis=par("cex"), cex.names=par("cex.axis"), beside=FALSE,col=cols,legend=TRUE, add=TRUE)
	
	box()


## Plot the fourth graph with Technical Area and Product

	n_colors <- nrow(xtab_TA_P)
	n_columns <- ncol(xtab_TA_P)

	cols <- terrain.colors(n_colors)

	barplot(xtab_TA_P, ylim=c(0,1.2*sum(xtab_TA_P[,1])), main="Defects by Technical Area and Product", xlab="Product", ylab="Defect Count", axes=TRUE,cex.axis=par("cex"), cex.names=par("cex.axis"), beside=FALSE,col=cols,legend=TRUE)
	
	grid( round(max(xtab_TA_P[,1])/25,0),lty=1,lwd=0.5)

	barplot(xtab_TA_P, ylim=c(0,1.2*sum(xtab_TA_P[,1])), main="Defects by Technical Area and Product", xlab="Product", ylab="Defect Count", axes=TRUE,cex.axis=par("cex"), cex.names=par("cex.axis"), beside=FALSE,col=cols,legend=TRUE, add=TRUE)
	
	box()


## Plot the fifth graph with RCA Code and Environment Detected

	n_colors <- nrow(xtab_RCA_Env)
	n_columns <- ncol(xtab_RCA_Env)

	cols <- heat.colors(n_colors)

	barplot(xtab_RCA_Env, ylim=c(0,1.2*sum(xtab_RCA_Env[,1])), main="Defects by RCA Code and Environment detected", xlab="Environment Detected", ylab="Defect Count", axes=TRUE,cex.axis=par("cex"), cex.names=par("cex.axis"), beside=FALSE,col=cols,legend=TRUE)
	
	grid( round(max(xtab_RCA_Env[,1])/25,0),lty=1,lwd=0.5)

		barplot(xtab_RCA_Env, ylim=c(0,1.2*sum(xtab_RCA_Env[,1])), main="Defects by RCA Code and Environment detected", xlab="Environment Detected", ylab="Defect Count", axes=TRUE,cex.axis=par("cex"), cex.names=par("cex.axis"), beside=FALSE,col=cols,legend=TRUE, add=TRUE)
	
	box()

## Plot the Sixth graph with RCA Code and Defect Source

	n_colors <- nrow(xtab_RCA_DS)
	n_columns <- ncol(xtab_RCA_DS)

	cols <- cm.colors(n_colors)

	barplot(xtab_RCA_DS, ylim=c(0,1.2*sum(xtab_RCA_DS[,1])), main="Defects by RCA Code and Defect Source", xlab="RCA Code", ylab="Defect Count", axes=TRUE,cex.axis=par("cex"), cex.names=par("cex.axis"), beside=FALSE,col=cols,legend=TRUE)
	
	grid( round(max(xtab_RCA_DS[,1])/25,0),lty=1,lwd=0.5)

	barplot(xtab_RCA_DS, ylim=c(0,1.2*sum(xtab_RCA_DS[,1])), main="Defects by RCA Code and Defect Source", xlab="RCA Code",ylab="Defect Count", axes=TRUE,cex.axis=par("cex"), cex.names=par("cex.axis"), beside=FALSE,col=cols,legend=TRUE, add=TRUE)
	
	box()
	

## Plot the Seventh graph with RCA Code and Product

	n_colors <- nrow(xtab_RCA_P)
	n_columns <- ncol(xtab_RCA_P)

	cols <- cm.colors(n_colors)

	barplot(xtab_RCA_P, ylim=c(0,1.2*sum(xtab_RCA_P[,1])), main="Defects by RCA Code and Product", xlab="Product", ylab="Defect Count", axes=TRUE,cex.axis=par("cex"), cex.names=par("cex.axis"), beside=FALSE,col=cols,legend=TRUE)
	
	grid( round(max(xtab_RCA_P[,1])/25,0),lty=1,lwd=0.5)

	barplot(xtab_RCA_P, ylim=c(0,1.2*sum(xtab_RCA_P[,1])), main="Defects by RCA Code and Product", xlab="Product",ylab="Defect Count", axes=TRUE,cex.axis=par("cex"), cex.names=par("cex.axis"), beside=FALSE,col=cols,legend=TRUE, add=TRUE)
	
	box()

## Plot the Eighth graph with Defect Source and Product

	n_colors <- nrow(xtab_DS_P)
	n_columns <- ncol(xtab_DS_P)

	cols <- cm.colors(n_colors)

	barplot(xtab_DS_P, ylim=c(0,1.2*sum(xtab_DS_P[,1])), main="Defect Source by Product", xlab="Product", ylab="Defect Count", axes=TRUE,cex.axis=par("cex"), cex.names=par("cex.axis"), beside=FALSE,col=cols,legend=TRUE)
	
	grid( round(max(xtab_DS_P[,1])/25,0),lty=1,lwd=0.5)

	barplot(xtab_DS_P, ylim=c(0,1.2*sum(xtab_DS_P[,1])), main="Defect Source by Product", xlab="Product",ylab="Defect Count", axes=TRUE,cex.axis=par("cex"), cex.names=par("cex.axis"), beside=FALSE,col=cols,legend=TRUE, add=TRUE)
	
	box()


## Plot the graph with cumulative defects

	#par("labels"=FALSE)
	plot(defOpen$startDate, defOpen$cumSum, ylim=c(0,max(defOpen$cumSum)),type="o", col="red", lty=1,lwd=2, main="Cumulative Defects - (Sep'14 to Jun'15)", xlab="Month", ylab="Defect Count",axes=FALSE)

	text(defOpen$startDate, defOpen$cumSum, labels=defOpen$cumSum,pos=3, cex=0.6, col="black")	
	
	axis(side=1, at=defOpen$startDate, labels=format(defOpen$startDate,"%b-%y"))
	axis(side=2, at=NULL, labels=TRUE, las=2, ylim=c(0,1.1*max(defOpen$cumSum)))
	
	lines(defClose$closeDate, defClose$cumSum, type="o", col="blue", lty=2,lwd=2, axes=FALSE)	

	text(defClose$closeDate, defClose$cumSum, labels=defClose$cumSum,pos=3, cex=0.6, col="black")	

		
	grid(max(defOpen$cumSum)/100,lty=1,lwd=0.5)
	box()	
	legend("bottomright",c("Cumulative Defects Open", "Cumulative Defects Closed"), lty=c(1,2), col=c("red","blue"), text.col="black", cex=0.7)
	
	dev.off()

	jpeg("cumulative_defects.jpg")

	## Plot the graph with cumulative defects

	#par("labels"=FALSE)
	plot(defOpen$startDate, defOpen$cumSum, ylim=c(0,max(defOpen$cumSum)),type="o", col="red", lty=1,lwd=2, main="Cumulative Defects - (Sep'14 to Jun'15)", xlab="Month", ylab="Defect Count",axes=FALSE)

	text(defOpen$startDate, defOpen$cumSum, labels=defOpen$cumSum,pos=3, cex=0.6, col="black")	
	
	axis(side=1, at=defOpen$startDate, labels=format(defOpen$startDate,"%b-%y"))
	axis(side=2, at=NULL, labels=TRUE, las=2, ylim=c(0,1.1*max(defOpen$cumSum)))
	
	lines(defClose$closeDate, defClose$cumSum, type="o", col="blue", lty=2,lwd=2, axes=FALSE)	

	text(defClose$closeDate, defClose$cumSum, labels=defClose$cumSum,pos=3, cex=0.6, col="black")	

		
	grid(max(defOpen$cumSum)/100,lty=1,lwd=0.5)
	box()	
	legend("bottomright",c("Cumulative Defects Open", "Cumulative Defects Closed"), lty=c(1,2), col=c("red","blue"), text.col="black", cex=0.7)


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

## This function takes the defects dataframe and converst some of the key columns into factors
## This function has to be called after the column names are modified appropriately - else it will not work
convertToFactors <- function(defects){
	
	if(!(class(defects)=="data.frame")){
		stop("Invalid input - this function expects defects as a dataframe")
	}

	defects$Product <- replace(defects$Product, (grep("Portfolio Select",defects$Product)),"PS")
	defects$Product <- replace(defects$Product, (grep("Crime Guard Choice",defects$Product)),"CGC")
	defects$Product <- replace(defects$Product, (grep("Executive Edge",defects$Product)),"EXEdge")
	defects$Product <- replace(defects$Product, (grep("Excess Edge",defects$Product)),"XSEdge")
	defects$Product <- replace(defects$Product, (grep("Side A Edge",defects$Product)),"SAEdge")
	defects$Product <- replace(defects$Product, (grep("FLI Edge",defects$Product)),"FLIEdge")
	defects$Product <- replace(defects$Product, (grep("FL-Org Labor",defects$Product)),"FLOrgL")
	defects$Product <- replace(defects$Product, (grep("FL-Pub Entity",defects$Product)),"FLPubEnt")


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


## This function takes the defects and returns the cumulative defects open and close numbers data

cumulativeDefects <- function(defects){
	# Order the defects by detected date (or detected date)
	defects <- defects[order(defects[,"DetectedDate"],decreasing=FALSE),]

	defects$startDate <- as.Date(defects$DetectedDate,"%m/%d/%Y")
	defects$closeDate <- as.Date(defects$ClosingDate, "%m/%d/%Y")

	temp <- tapply(defects$DefectID, cut(defects$startDate, "month"), length)
	
	openDefects <- data.frame(startDate=as.Date(names(temp),"%Y-%m-%d"),openCnt=as.numeric(temp))
	openDefects <- openDefects[order(openDefects[,"startDate"],decreasing=FALSE),]
	openDefects$cumSum <- cumsum(openDefects$openCnt)
	
	t_closeDefects <- data.frame(ID=defects$DefectID, closeDate=defects$closeDate)
	t_closeDefects <- subset(t_closeDefects, !is.na(t_closeDefects$closeDate))
	t_closeDefects <- subset(t_closeDefects, t_closeDefects$closeDate >= min(openDefects$startDate))
	
	
	t_temp <- tapply(t_closeDefects$ID, cut(t_closeDefects$closeDate, "month"), length)
	
	closeDefects <- data.frame(closeDate=as.Date(names(t_temp),"%Y-%m-%d"), closeCnt=as.numeric(t_temp))
	closeDefects <- closeDefects[order(closeDefects[,"closeDate"],decreasing=FALSE),]
	closeDefects$cumSum <- cumsum(closeDefects$closeCnt)
	
	rm(temp)
	rm(t_temp)
		
	ls_cumDefects <- list(cumOpen=openDefects, cumClose=closeDefects)
	return(ls_cumDefects)
}
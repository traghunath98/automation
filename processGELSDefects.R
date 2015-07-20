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

processGELSDefects <- function(x_file = character()) {
    
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
		
	xtab_S_St <<- getXParamCrossTab(defects, status_codes, c("DefectStatus","Severity"), confidence=1.01, col_confidence=1.01)

	xtab_RCA_Module <<- getXParamCrossTab(defects, resolved_status_codes,c("ResolutionType","Module"), confidence=1.01, col_confidence=1.01)

	xtab_DS_Module <<- getXParamCrossTab(defects, all_status_codes,c("DefectSource","Module"), confidence=1.05, col_confidence=1.05)

	xtab_Resolved_Status_Module <<- getXParamCrossTab(defects, resolved_status_codes,c("DefectStatus","Module"), confidence=1.05, col_confidence=1.05)

	xtab_Open_Status_Module <<- getXParamCrossTab(defects, status_codes,c("DefectStatus","Module"), confidence=1.05, col_confidence=1.05)


	
	#ls_cumDefects <- cumulativeDefects(defects)
	#defOpen <- ls_cumDefects[["cumOpen"]]
	#defClose <- ls_cumDefects[["cumClose"]]

	fileName <- paste("GELS_Defects_Analysis",strftime(Sys.time(),"%d%b%y-%H%M"),".pdf", sep="")
	if(file.exists(fileName)){
		file.remove(fileName)
	}

	pdf(fileName)
	par("cex"=0.8,"cex.axis"=0.8)
	
## Plot the first graph with Status and Severity
	n_colors <- nrow(xtab_S_St)
	n_columns <- ncol(xtab_S_St)
	
	cols <- c("blue","yellow","green","red")
	
	barplot(t(xtab_S_St), ylim=c(0,1.2*max(xtab_S_St[,1])), main="Defect Status and Severity (Open Defects)", xlab="Defect Status", ylab="Defect Count", axes=TRUE,cex.axis=par("cex"), cex.names=par("cex.axis"), beside=TRUE,col=cols,legend=TRUE)
	
	grid( round(max(xtab_S_St[,1])*1.1/20,0),lty=1,lwd=0.5)

	barplot(t(xtab_S_St), ylim=c(0,1.2*max(xtab_S_St[,1])), main="Defect Status and Severity (Open Defects)", xlab="Defect Status", ylab="Defect Count", axes=TRUE,cex.axis=par("cex"), cex.names=par("cex.axis"), beside=TRUE,col=cols,legend=TRUE,add=TRUE)

	box()



## Plot the fifth graph with Status and Module

	n_colors <- nrow(xtab_Open_Status_Module)
	n_columns <- ncol(xtab_Open_Status_Module)

	cols <- rainbow(n_colors)
	
	barplot(xtab_Open_Status_Module, ylim=c(0,1.2*sum(xtab_Open_Status_Module[,1])), main="Defect Status by Module (Open Defects)", xlab="Module", ylab="Defect Count", axes=TRUE,cex.axis=par("cex"), cex.names=par("cex.axis"), beside=FALSE,col=cols,legend=TRUE)
	
	grid( round(max(xtab_Open_Status_Module[,1])/25,0),lty=1,lwd=0.5)

	barplot(xtab_Open_Status_Module, ylim=c(0,1.2*sum(xtab_Open_Status_Module[,1])), main="Defect Status by Module (Open Defects)", xlab="Module",ylab="Defect Count", axes=TRUE,cex.axis=par("cex"), cex.names=par("cex.axis"), beside=FALSE,col=cols,legend=TRUE, add=TRUE)
	text(xtab_Open_Status_Module,label=TRUE,cex=0.5)
	
	box()



## Plot the fifth graph with Status and Module

	n_colors <- nrow(xtab_Resolved_Status_Module)
	n_columns <- ncol(xtab_Resolved_Status_Module)

	cols <- rainbow(n_colors)
	
	barplot(xtab_Resolved_Status_Module, ylim=c(0,1.2*sum(xtab_Resolved_Status_Module[,1])), main="Defect Status by Module (Resolved Defects)", xlab="Module", ylab="Defect Count", axes=TRUE,cex.axis=par("cex"), cex.names=par("cex.axis"), beside=FALSE,col=cols,legend=TRUE)
	
	grid( round(max(xtab_Resolved_Status_Module[,1])/25,0),lty=1,lwd=0.5)

	barplot(xtab_Resolved_Status_Module, ylim=c(0,1.2*sum(xtab_Resolved_Status_Module[,1])), main="Defect Status by Module (Resolved Defects)", xlab="Module",ylab="Defect Count", axes=TRUE,cex.axis=par("cex"), cex.names=par("cex.axis"), beside=FALSE,col=cols,legend=TRUE, add=TRUE)
	
	box()

## Plot the second graph with RCA Code and Module 

	n_colors <- nrow(xtab_RCA_Module)
	n_columns <- ncol(xtab_RCA_Module)

	cols <- heat.colors(n_colors)
	
	barplot(xtab_RCA_Module, ylim=c(0,1.2*sum(xtab_RCA_Module[,1])), main="Resolved Defects by Resolution Type and Module", xlab="Module", ylab="Defect Count", axes=TRUE,cex.axis=par("cex"), cex.names=par("cex.axis"), beside=FALSE,col=cols,legend=TRUE)
	
	grid( round(max(xtab_RCA_Module[,1])/25,0),lty=1,lwd=0.5)

		barplot(xtab_RCA_Module, ylim=c(0,1.2*sum(xtab_RCA_Module[,1])), main="Resolved Defects by Resolution Type and Module", xlab="Module", ylab="Defect Count", axes=TRUE,cex.axis=par("cex"), cex.names=par("cex.axis"), beside=FALSE,col=cols,legend=TRUE, add=TRUE)
	
	box()


## Plot the fourth graph with Defect Source and Module

	n_colors <- nrow(xtab_DS_Module)
	n_columns <- ncol(xtab_DS_Module)

	cols <- rainbow(n_colors)
	
	barplot(xtab_DS_Module, ylim=c(0,1.2*sum(xtab_DS_Module[,1])), main="Defect Source by Module (all defects)", xlab="Module", ylab="Defect Count", axes=TRUE,cex.axis=par("cex"), cex.names=par("cex.axis"), beside=FALSE,col=cols,legend=TRUE)
	
	grid( round(max(xtab_DS_Module[,1])/25,0),lty=1,lwd=0.5)

	barplot(xtab_DS_Module, ylim=c(0,1.2*sum(xtab_DS_Module[,1])), main="Defect Source by Module (all defects)", xlab="Module",ylab="Defect Count", axes=TRUE,cex.axis=par("cex"), cex.names=par("cex.axis"), beside=FALSE,col=cols,legend=TRUE, add=TRUE)
	
	box()





## Plot the graph with cumulative defects

	#par("labels"=FALSE)
	#plot(defOpen$startDate, defOpen$cumSum, ylim=c(0,max(defOpen$cumSum)),type="o", col="red", lty=1,lwd=2, main="Cumulative Defects", xlab="Month", ylab="Defect Count",axes=FALSE)

	#text(defOpen$startDate, defOpen$cumSum, labels=defOpen$cumSum,pos=3, cex=0.6, col="black")	
	
	#axis(side=1, at=defOpen$startDate, labels=format(defOpen$startDate,"%b-%y"))
	#axis(side=2, at=NULL, labels=TRUE, las=2, ylim=c(0,1.1*max(defOpen$cumSum)))
	
	#lines(defClose$closeDate, defClose$cumSum, type="o", col="blue", lty=2,lwd=2, axes=FALSE)	

	#text(defClose$closeDate, defClose$cumSum, labels=defClose$cumSum,pos=3, cex=0.6, col="black")	

		
	#grid(max(defOpen$cumSum)/100,lty=1,lwd=0.5)
	#box()	
	#legend("bottomright",c("Cumulative Defects Open", "Cumulative Defects Closed"), lty=c(1,2), col=c("red","blue"), text.col="black", cex=0.7)
	
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
	#colnames(defects)[11] <- "DetectedDate"
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
	#colnames(defects)[21] <- "ClosingDate"
	#colnames(defects)[16] <- "DevCommitFixDate"
	colnames(defects)[15] <- "DefectSource"
	#colnames(defects)[11] <- "CreateDate"
	colnames(defects)[14] <- "ResolutionType"
	colnames(defects)[13] <- "ResolutionSummary"
	colnames(defects)[18] <- "TestPhase"
	#colnames(defects)[24] <- "TestType"

	
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

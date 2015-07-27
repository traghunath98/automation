## The objective of these sets of functions are to automate the process of analysis of Attrition data
## First we will read the base attrition data from a CSV file into a data frame. We will then transform the 
## the data frame to give us the necessary critical insights. 
## All graphs must be generated at the click of a button.
##
##
## Each function is documented below in detail to highlight its specific responsibility.
##

## This function - analyzeAttrition is the main function that takes the file name as input and applies transformation
## 

analyzeAttrition <- function(x_file = character()) {
    
	library(ggplot2)
	library(grDevices)
	
	aig_data <- data.frame()
	if(!file.exists(x_file)){
		stop("invalid file")
	}
	## read the defects file
	aig_data <- read.csv(x_file, stringsAsFactors=FALSE)

	aig_data <- convertToFactors(aig_data)
	
	#Plot attrition by competency, onsite/offshore / tenure
	g <- ggplot(data=aig_data, aes(x=Competency, fill=Tenure_Yrs)) + geom_bar(stat="bin", position="stack") + facet_grid(Sex~Shore)
	g <- g + geom_text(aes(label=..count..), stat="bin", position="stack",size=2)
	g <- g + labs(x="Competency", y="Count", title="Attrition by Tenure and Competency")
	g <- g + theme_bw() + theme(axis.text.x = element_text(size=8), legend.text = element_text(size=7), legend.title=element_text(size=9))
	
	g_competency_tenure <- g



#Plot attrition by Location, onsite/offshore / tenure
	g <- ggplot(data=aig_data, aes(x=LOC, fill=Tenure_Yrs)) + geom_bar(stat="bin", position="stack") + facet_grid(.~Shore)
	g <- g + geom_text(aes(label=..count..), stat="bin", position="stack",size=2)
	g <- g + labs(x="Location", y="Count", title="Attrition by Tenure and Location")
	g <- g + theme_bw() + theme(axis.text.x = element_text(size=8, angle=45), legend.text = element_text(size=7), legend.title=element_text(size=9))
	
	g_location_tenure <- g

#Plot attrition by Reason, Tenure - top 70% of reasons
	re <- table(aig_data$Reason)
	re <- re[order(re, decreasing=TRUE)]
	reason_codes <- names(re[cumsum(re)<0.7*sum(re)])
	aig_reasonData <- getSubset(aig_data,"Reason",reason_codes)

	g <- ggplot(data=aig_reasonData, aes(x=Reason, fill=Tenure_Yrs)) + geom_bar(stat="bin", position="stack") + facet_grid(.~Shore)
	g <- g + geom_text(aes(label=..count..), stat="bin", position="stack",size=2)
	g <- g + labs(x="Reason", y="Count", title="Attrition by Tenure and Reason(top 70%)")
	g <- g + theme_bw() + theme(axis.text.x = element_text(size=6, angle=45), legend.text = element_text(size=7), legend.title=element_text(size=8), legend.position="bottom")
	
	g_reason_tenure <- g

#Plot competency - total experience

	g <- ggplot(data=aig_reasonData, aes(x=Reason, y=Total_Exp)) + geom_boxplot() + facet_grid(.~Shore)
	g <- g + labs(x="Reason", y="Total Experience", title="Reason and Experience")
	g <- g + theme_bw() + theme(axis.text.x = element_text(size=6, angle=45))
	
	g_competency_exp <- g



#Plot Exit Date, Reason, by Competency

    aig_selectCompetency <- getSubset(aig_reasonData,"Competency",c("C2","C3","C4","C5"))
    g <- ggplot(data=aig_selectCompetency, aes(x=exitDate, y=..count.., fill=Reason)) + geom_bar(stat="bin") + facet_wrap(~Competency, ncol=2)
    g <- g + labs(x="Exit Date", y="Count", title="Reason by exit date,Competency")
    g <- g + theme_bw() + theme(axis.text.x = element_text(size=6, angle=45), legend.text = element_text(size=6), legend.title=element_text(size=7))

    g_exitDate_Reason <- g
    

# Plot relationship between prev-experience and tenure (in months) for C2-C5 competencies
    
    g <- ggplot(data=getSubset(aig_data,"Competency",c("C2","C3","C4","C5")), aes(x=Prev_Exp, y=Tenure/30, color=Sex)) + geom_point() + geom_smooth(method="lm") + scale_y_continuous(limits=c(0,150)) + facet_wrap(~Competency, ncol=2)
    g <- g + labs(x="Previous Experience (months)", y="Tenure (months)", title="Tenure and Previous Experience")
    g <- g + theme_bw() + theme(axis.text.x=element_text(size=8),legend.text=element_text(size=7),legend.title=element_text(size=8), legend.position="bottom")

    g_exp_tenure <- g

# Plot relationship between total-experience and tenure (in months) for C2-C7 competencies
    
    g <- ggplot(data=getSubset(aig_data,"Competency",c("C2","C3","C4","C5","C6","C7")), aes(x=Total_Exp, y=Tenure/30, color=Sex)) + geom_point() + geom_smooth(method="lm") + scale_y_continuous(limits=c(0,180)) + facet_wrap(~Competency, ncol=2)
    g <- g + labs(x="Total Experience (months)", y="Tenure (months)", title="Tenure and Total Experience")
    g <- g + theme_bw() + theme(axis.text.x=element_text(size=8),legend.text=element_text(size=7),legend.title=element_text(size=8), legend.position="bottom")

    g_total_exp_tenure <- g





x_attr <<-aig_data


	fileName <- paste("AIG_Attrition",strftime(Sys.time(),"%d%b%y-%H%M"),".pdf", sep="")
	if(file.exists(fileName)){
		file.remove(fileName)
	}

	pdf(fileName)
       	
		print(g_exp_tenure)
		print(g_total_exp_tenure)
		print(g_competency_tenure)
		print(g_location_tenure)
		print(g_reason_tenure)
		print(g_competency_exp)
        	print(g_exitDate_Reason)
 
				
	dev.off()
}

## This function takes the attrition dataframe and converts some of the key columns into factors
## This function has to be called after the column names are modified appropriately - else it will not work
convertToFactors <- function(aig_data){
	
	if(!(class(aig_data)=="data.frame")){
		stop("Invalid input - this function expects defects as a dataframe")
	}

	aig_data$Business <- as.factor(aig_data$Business)
	aig_data$Competency <- as.factor(aig_data$Competency)
	aig_data$Designation <- as.factor(aig_data$Designation)
	aig_data$Location <- as.factor(aig_data$Location)
	aig_data$Reason <- as.factor(aig_data$Reason)
	aig_data$Practice <- as.factor(aig_data$Practice)
	aig_data$Tenure_Yrs <- as.factor(aig_data$Tenure_Yrs)
	aig_data$Exp_Grid <- as.factor(aig_data$Exp_Grid)
	aig_data$Sex <- as.factor(aig_data$Sex)
	aig_data$LOC <- as.factor(aig_data$LOC)
	aig_data$Shore <- as.factor(aig_data$Shore)
    
    #Adding a new column called Exit Date
    aig_data$exitDate <- strptime(aig_data$DOL, "%d-%b-%y")

	return(aig_data)
}

## This function takes a) attrition data dataframe, b) Parameter for criteria c) List of values for parameter 
## It then returns the subset of the dataframe based on the parameter and the values
getSubset <- function(aig_data=data.frame(), parameter = character(), param_values=character()){

	if(length(parameter)<1){
		stop("Insufficient Parameters")
	} 
	
	## Get the index corresponding to the parameters
	c1 <- grep(parameter,names(aig_data))

	## Get subset of aig_data by the parameter and the param_values
	sub_aigData <- subset(aig_data, aig_data[,c1] %in% param_values)
	sub_aigData[,c1] <- as.factor(as.character(sub_aigData[,c1]))

    return(sub_aigData)
}
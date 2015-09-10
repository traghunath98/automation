## The objective of these sets of functions are to automate the process of analysis of financial data
## First we will read the financial data from a CSV file into a data frame. We will then transform the 
## the data frame to give us the necessary critical insights. 
## All graphs must be generated at the click of a button.
##
##
## Each function is documented below in detail to highlight its specific responsibility.
## 
##

## This function - financialReview is the main function that takes the file name as input and applies transformation
## 

financialReview <- function(x_file = character(), y_file = character()) {
    
	library(ggplot2)
	library(reshape2)
	library(scales)
	
	fin_data <- data.frame()
	if(!file.exists(x_file)){
		stop("invalid file")
	}
	## read the financial data file
	fin_data <- read.csv(x_file, stringsAsFactors=FALSE)
	fin_data <- convertToFactors(fin_data)

	## read the financial overview data file here
	fin_overview <- read.csv(y_file, stringsAsFactors=FALSE)
	fin_overview <- convertToFactors_Overview(fin_overview)
	
	## Plot the Financial Overview - Revenue, People Cost and Expenses by Project for all months.
	g <- ggplot(data=fin_overview, aes(x=as.Date(Month)))
	g <- g + geom_bar(binwidth=30, mapping=aes(y=Revenue), stat="identity", fill="green")
	g <- g + geom_bar(binwidth=30,mapping=aes(y=Total_Cost), stat="identity", fill="yellow")
	g <- g + geom_bar(binwidth=30,mapping=aes(y=People_Cost), stat="identity", fill="red")
	g <- g + scale_y_continuous(labels= dollar)
	g <- g + scale_x_date(labels=date_format("%b-%y"), breaks=date_breaks("month"))
	g <- g + facet_grid(~Project)
	g <- g + labs(x="Month",y="Revenue / Cost (in USD)", title="Overall Revenue and Cost By Project")
    g <- g + theme_bw() + theme(axis.text.x = element_text(size=8, angle=45), legend.text = element_text(size=7), legend.title=element_text(size=9))
	g_Revenue_Cost <- g	
	
	## Plot the Financial Overview - GM1 and GM2 Percentages.
	g <- ggplot(data=fin_overview, aes(x=as.Date(Month)))
	g <- g + geom_line(mapping=aes(y=PCT_GM1), stat="identity", color="red")
	g <- g + geom_line(mapping=aes(y=PCT_GM2), stat="identity", color="blue")
	g <- g + scale_y_continuous(labels= percent)
	g <- g + scale_x_date(labels=date_format("%b-%y"), breaks=date_breaks("month"))
	g <- g + facet_grid(~Project)
	g <- g + labs(x="Month",y="Gross Margin (Percentage)", title="Overall GM1 and GM2 By Project")
    g <- g + theme_bw() + theme(axis.text.x = element_text(size=8, angle=45), legend.text = element_text(size=7), legend.title=element_text(size=9))
	g_gross_margins <- g	
	
	## Plot the billable and non-billable staff counts
	g <- ggplot(data=fin_data, aes(x=as.Date(Month), fill=Billable)) + geom_bar(binwidth=30,stat="bin", position="dodge")
	g <- g + scale_fill_manual(values=c("YES" = "green", "NO" = "red"))
	g <- g + scale_x_date(labels=date_format("%b-%y"), breaks=date_breaks("month"))
	g <- g + facet_grid(Location~Project)
	g <- g + labs(x="Month", y="No. of Staff", title="Billable and Unbillable staff by Project")
	g <- g + theme_bw() + theme(axis.text.x = element_text(size=8, angle=45), legend.text = element_text(size=7), legend.title=element_text(size=9), legend.position="bottom")
	g_staff_counts <- g

	
	fin_overview <- getUpdatedFinOverview(fin_overview, fin_data)
	
	# Plot the Per Capita Revenue and Cost by project and by month
	g <- ggplot(data=fin_overview, aes(x=as.Date(Month)))
	g <- g + geom_line(mapping=aes(y=PerCapita_Revenue), stat="identity", color="green")
	g <- g + geom_line(mapping=aes(y=PerCapita_Cost), stat="identity", color="red")
	g <- g + geom_text(aes(x=as.Date(Month), y=PerCapita_Revenue,label=round(PerCapita_Revenue),size=2))
	g <- g + geom_text(aes(x=as.Date(Month), y=PerCapita_Cost,label=round(PerCapita_Cost),size=2))
	g <- g + scale_y_continuous(labels= dollar)
	g <- g + scale_x_date(labels=date_format("%b-%y"), breaks=date_breaks("month"))
	g <- g + facet_grid(~Project)
	g <- g + labs(x="Month",y="Per Capita Revenue / Cost", title="Per Capita Revenue / Cost By Project")
    g <- g + theme_bw() + theme(axis.text.x = element_text(size=8, angle=45), legend.text = element_text(size=7), legend.title=element_text(size=9), legend.position="bottom")
	g_pc_rev_cost <- g	
	
	# Plot the Cost overrun per project and by month
	g <- ggplot(data=fin_overview, aes(x=as.Date(Month), y=Cost_Overrun, color=Project)) + geom_line()
	g <- g + scale_y_continuous(labels= percent)
	g <- g + scale_x_date(labels=date_format("%b-%y"), breaks=date_breaks("month"))
	g <- g + geom_text(aes(x=as.Date(Month), y=Cost_Overrun,label=round(Cost_Overrun,2),size=2))
	g <- g + labs(x="Month",y="Cost Overrun by Project", title="Cost Overrun by Project")
    g <- g + theme_bw() + theme(axis.text.x = element_text(size=8, angle=45), legend.text = element_text(size=7), legend.title=element_text(size=9), legend.position="bottom")
	g_cost_overrun <- g	
	
	# Plot the Gap Value as a % of revenue by project and by month
	g <- ggplot(data=fin_overview, aes(x=as.Date(Month), y=gap_value/Revenue, color=Project)) + geom_line()
	g <- g + scale_y_continuous(labels= percent)
	g <- g + scale_x_date(labels=date_format("%b-%y"), breaks=date_breaks("month"))
	g <- g + geom_text(aes(x=as.Date(Month), y=gap_value/Revenue,label=round(gap_value/Revenue,2),size=2))
	g <- g + labs(x="Month",y="Gap Value by Project", title="Billing Gap by Project")
    g <- g + theme_bw() + theme(axis.text.x = element_text(size=8, angle=45), legend.text = element_text(size=7), legend.title=element_text(size=9), legend.position="bottom")
	g_billing_gap <- g
    
    
    # Plot the Onsite head-count as a % of total head-count by project and by month
    g <- ggplot(data=fin_overview, aes(x=as.Date(Month), y=((HC_ON_BILL+HC_ON_UNBILL)/(HC_OFF_BILL+HC_OFF_UNBILL + HC_ON_BILL + HC_ON_UNBILL)), color=Project)) + geom_line()
    g <- g + scale_y_continuous(labels= percent)
    g <- g + scale_x_date(labels=date_format("%b-%y"), breaks=date_breaks("month"))
    g <- g + geom_text(aes(x=as.Date(Month), y=((HC_ON_BILL+HC_ON_UNBILL)/(HC_OFF_BILL+HC_OFF_UNBILL + HC_ON_BILL + HC_ON_UNBILL)),label=round(((HC_ON_BILL+HC_ON_UNBILL)/(HC_OFF_BILL+HC_OFF_UNBILL + HC_ON_BILL + HC_ON_UNBILL)),2),size=1))
    g <- g + labs(x="Month",y="Onsite Ratio by Project", title="Onsite Ratio by Project")
    g <- g + theme_bw() + theme(axis.text.x = element_text(size=8, angle=45), legend.text = element_text(size=7), legend.title=element_text(size=9), legend.position="bottom")
    g_onsite_ratio <- g


    # Plot the PerCapita Margin by project and by month
    g <- ggplot(data=fin_overview, aes(x=as.Date(Month), y=((PerCapita_Revenue - PerCapita_Cost)/(PerCapita_Revenue)), color=Project)) + geom_line()
    g <- g + scale_y_continuous(labels= percent)
    g <- g + scale_x_date(labels=date_format("%b-%y"), breaks=date_breaks("month"))
    g <- g + geom_text(aes(x=as.Date(Month), y=((PerCapita_Revenue - PerCapita_Cost)/(PerCapita_Revenue)),label=round(((PerCapita_Revenue - PerCapita_Cost)/(PerCapita_Revenue)),2),size=1))
    g <- g + labs(x="Month",y="Per Capita Margin (%)", title="Per Capita Margin (%) by Project")
    g <- g + theme_bw() + theme(axis.text.x = element_text(size=8, angle=45), legend.text = element_text(size=7), legend.title=element_text(size=9), legend.position="bottom")
    g_PC_Margin <- g


	temp_fin_data <<- fin_data
	temp_fin_overview <<- fin_overview
	
	fileName <- paste("Financial_Review",strftime(Sys.time(),"%d%b%y-%H"),".pdf", sep="")
	if(file.exists(fileName)){
		file.remove(fileName)
	}

	pdf(file=fileName, paper="a4r")
	par("cex"=0.8,"cex.axis"=0.8)
	
	print(g_Revenue_Cost)
	print(g_gross_margins)
    print(g_PC_Margin)
	print(g_staff_counts)
	print(g_pc_rev_cost)
	print(g_cost_overrun)
	print(g_billing_gap)
    print(g_onsite_ratio)
	
	dev.off()

}

## This function gets the billable - non-billable staff counts etc. 
getUpdatedFinOverview <- function(fin_overview, fin_data){
	
	if(!(class(fin_overview)=="data.frame") | !(class(fin_data)=="data.frame")){
		stop("Invalid input - this function expects defects as a dataframe")
	}
	fin_staff_counts <- aggregate(data=fin_data, MID ~ as.Date(Month) + Location + Project + Program + Billable, FUN=length)
	names(fin_staff_counts)[1] <- "Month"
	
	sub_staff_cnt <- data.frame()
		
	fin_overview$HC_ON_BILL <- 0.0
	fin_overview$HC_ON_UNBILL <- 0.0
	fin_overview$HC_OFF_BILL <- 0.0
	fin_overview$HC_OFF_UNBILL <- 0.0
	
	fin_overview$comp_ppl_cost <- 0.0
	fin_overview$comp_revenue <- 0.0
	fin_overview$gap_value <- 0.0
	
	for(i in 1:nrow(fin_overview)){
		sub_staff_cnt <- subset(fin_staff_counts, (Month==as.Date(fin_overview[i,"Month"]) & Project==fin_overview[i,"Project"]))
		temp_counts <- xtabs(data=sub_staff_cnt, MID ~ Location + Billable)
		fin_overview$HC_ON_BILL[i] <- temp_counts["Onsite","YES"]
		fin_overview$HC_ON_UNBILL[i] <- temp_counts["Onsite","NO"]
		fin_overview$HC_OFF_BILL[i] <- temp_counts["Offshore","YES"]
		fin_overview$HC_OFF_UNBILL[i] <- temp_counts["Offshore","NO"]
		
		fin_overview$comp_ppl_cost[i] <- sum(fin_data[fin_data$Month==fin_overview[i,"Month"] & fin_data$Project==fin_overview[i,"Project"],"Cost"])
		fin_overview$comp_revenue[i] <- sum(fin_data[fin_data$Month==fin_overview[i,"Month"] & fin_data$Project==fin_overview[i,"Project"],"Revenue"])
		fin_overview$gap_value[i] <- sum(fin_data[fin_data$Month==fin_overview[i,"Month"] & fin_data$Project==fin_overview[i,"Project"],"Gap_Value"])
	}

	fin_overview$Cost_Overrun <- (fin_overview$People_Cost - fin_overview$comp_ppl_cost) / fin_overview$comp_ppl_cost
	fin_overview$PerCapita_Revenue <- fin_overview$Revenue /(fin_overview$HC_ON_BILL + fin_overview$HC_OFF_BILL + fin_overview$HC_ON_UNBILL + fin_overview$HC_OFF_UNBILL)
	fin_overview$PerCapita_Cost <- fin_overview$People_Cost /(fin_overview$HC_ON_BILL + fin_overview$HC_OFF_BILL + fin_overview$HC_ON_UNBILL + fin_overview$HC_OFF_UNBILL)
	
	return(fin_overview)
}


## This function takes the financial data frame and converts some of the key columns into factors
convertToFactors_Overview <- function(fin_overview){
	
	if(!(class(fin_overview)=="data.frame")){
		stop("Invalid input - this function expects defects as a dataframe")
	}

	fin_overview$Month <- as.factor(fin_overview$Month)
	fin_overview$Project <- as.factor(fin_overview$Project)
	fin_overview$Program <- as.factor(fin_overview$Program)

	#Transform Month attribute to calendar date
	fin_overview$Month <- strptime(fin_overview$Month, "%m/%d/%Y")	
	fin_overview$Total_Cost <- fin_overview$People_Cost + fin_overview$Expenses
	
	return(fin_overview)
}

## This function takes the financial overview data and converts some of the key columns into factors
convertToFactors <- function(fin_data){
	
	if(!(class(fin_data)=="data.frame")){
		stop("Invalid input - this function expects defects as a dataframe")
	}

	fin_data$MID <- as.factor(fin_data$MID)
	fin_data$Emp_Name <- as.factor(fin_data$Emp_Name)
	fin_data$Role <- as.factor(fin_data$Role)
	fin_data$Billable <- as.factor(fin_data$Billable)
	fin_data$Competency <- as.factor(fin_data$Competency)
	fin_data$Month <- as.factor(fin_data$Month)
	fin_data$Project <- as.factor(fin_data$Project)
	fin_data$Location <- as.factor(fin_data$Location)
	fin_data$Program <- as.factor(fin_data$Program)
	fin_data$Currency <- as.factor(fin_data$Currency)

	#Transform Month attribute to calendar date
	fin_data$Month <- strptime(fin_data$Month, "%m/%d/%Y")	
	
	return(fin_data)
}

## This function takes a) data frame, b) Parameter for criteria c) List of values for parameter 
## It then returns the subset of the dataframe based on the parameter and the values
getSubset <- function(df_data=data.frame(), parameter = character(), param_values=character()){

	if(length(parameter)<1){
		stop("Insufficient Parameters")
	} 
	
	## Get the index corresponding to the parameters
	c1 <- grep(parameter,names(df_data))

	## Get subset of the data by the parameter and the param_values
	sub_df_data <- subset(df_data, df_data[,c1] %in% param_values)
	sub_df_data[,c1] <- as.factor(as.character(sub_df_data[,c1]))

    return(sub_df_data)
}
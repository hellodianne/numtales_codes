loanA = read.csv("LoanStats3a.csv", na.strings=c("","NA"))
loanB = read.csv("LoanStats3b.csv", na.strings=c("","NA"))
loanC = read.csv("LoanStats3c.csv", na.strings=c("","NA"))
#combine 3 dataframes
loanX = rbind(loanA, loanB, loanC)

loanData <- loanX[which (is.na(loanX$member_id) == FALSE),]


#make sure that member_id is unique
nrow(loanData) == length(unique(loanData$member_id))


loanData$loan_status = gsub("Does not meet the credit policy. Status:", "", loanData$loan_status)
loanData$int_rate = as.numeric(gsub("%", "", loanData$int_rate))
loanData$revol_util = as.numeric(gsub("%", "", loanData$revol_util))

#distribution of loans

disLoan = ggplot(loanData, aes(x=loan_amnt, fill = loan_status)) + geom_histogram(position="dodge", binwidth=1000)
disLoan + ggtitle("Distribution of Loan Amount") + xlab("Loan Amount") + labs(fill= "Loan Status")
#distribution of loans without current
PastLoan = loanData[which(loanData$loan_status != "Current"),]
pie = ggplot(PastLoan, aes(x=factor(1), fill = factor(loan_status))) + geom_bar(width=1) + coord_polar(theta="y")
pie = pie + labs(title = "Status without Current Loans")

#distributino income
disIncome = ggplot(loanData, aes(x=annual_inc)) + geom_histogram(position="dodge", binwidth=10000)

#to better show the distribution of income of borrowers: ###TO DO###
hist(loanData$annual_inc, breaks = 10000)


#create a new column
loandata$income_level = NA


loanData$income_level = 
for (i in 1: length(loanData$annual_inc)) {
	if (loanData$annual_inc[i] < 20000) {
		loanData$income_level[i] = "low"
	}
	else if (loanData$annual_inc[i] > 350000){
		loanData$income_level[i] = "high"
	}
		else if (loanData$annual_inc[i] > 100000 && loanData$annual_inc[i] < 350000){
		loanData$income_level[i] = "uppermid"
	}
	else if (loanData$annual_inc[i] > 50000 && loanData$annual_inc[i] < 100000) {
		loanData$income_level[i] = "mid"
	}
	else if (loanData$annual_inc[i] > 20000 && loanData$annual_inc[i] < 50000){
		loanData$income_level[i] = "upperlow"
	}
	
}

#and the distribution of income in bar plots
incomePurpose = ggplot(loanData, aes(x=income_level, fill = purpose)) + geom_bar(width=1)
incomePurpose = incomePurpose + ggtitle("Loan Purpose across different Income Levels") + xlab("Income Level") + labs(fill= "Loan Purpose")



low = loanData[which(loanData$income_level == "low"), ]
upperlow = loanData[which(loanData$income_level == "upperlow"), ]
mid = loanData[which(loanData$income_level == "mid"), ]
uppermid = loanData[which(loanData$income_level == "uppermid"), ]
high = loanData[which(loanData$income_level == "high"), ]

my_colors = brewer.pal(6, "Set1")
names(my_colors) = levels(loanData$home_ownership)
color_scale = scale_colour_manual(name = "Home Ownership", values = my_colors)

#majority rent
low_plot = ggplot(low, aes(x=annual_inc, y=loan_amnt, colour= home_ownership)) + geom_point(alpha=0.5) + color_scale

#majority rent
upperlow_plot = ggplot(upperlow, aes(x=annual_inc, y=loan_amnt, colour= home_ownership)) + geom_point(alpha=0.5) + color_scale

#majority mortgage
mid_plot = ggplot(mid, aes(x=annual_inc, y=loan_amnt, colour= home_ownership)) + geom_point(alpha=0.5) + color_scale 

#majority mortgage
uppermid_plot = ggplot(uppermid, aes(x=annual_inc, y=loan_amnt, colour= home_ownership)) + geom_point(alpha=0.5) + color_scale

high_plot = ggplot(high, aes(x=annual_inc, y=loan_amnt, colour= home_ownership)) + geom_point(alpha=0.5) + color_scale


first = grid.arrange(low_plot, upperlow_plot, ncol=2)
second = grid.arrange(mid_plot, uppermid_plot, ncol=2)
third = grid.arrange(high_plot, ncol=2)







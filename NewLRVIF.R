loandata = read.csv("C:/Users/mtanna108360/Downloads/Data Science/Proschool/loans data.csv", stringsAsFactors = FALSE)
loandata = loandata[,!names(loandata) %in% c("ID","Amount.Funded.By.Investors")]
gsub("^\\s+|\\s+$", "", loandata)
View(loandata)
str(loandata)
summary(loandata)

loandata$Amount.Requested = as.numeric(loandata$Amount.Requested)

loandata$Interest.Rate = gsub("%","",loandata$Interest.Rate)
loandata$Interest.Rate = as.numeric(loandata$Interest.Rate)

table(loandata$Loan.Length)
sum(is.na(loandata$Loan.Length) == TRUE)
loandata[is.na(loandata$Loan.Length) == TRUE,]
loandata$Loan.Length[loandata$Loan.Length == "." & loandata$Amount.Requested < 10000] = "36 months"
loandata$Loan.Length[loandata$Loan.Length == "." & loandata$Amount.Requested > 10000] = "60 months"
loandata$Loan.Length[is.na(loandata$Loan.Length) == TRUE & loandata$Amount.Requested < 10000] = "36 months"
loandata$Loan.Length[is.na(loandata$Loan.Length) == TRUE & loandata$Amount.Requested > 10000] = "60 months"
table(loandata$Loan.Length)
unique(loandata$Loan.Length)
loandata$Loan.Length = as.numeric(gsub(" months","",loandata$Loan.Length))

table(loandata$Loan.Purpose)
sum(is.na(loandata$Loan.Purpose) == TRUE)
loandata[is.na(loandata$Loan.Purpose) == TRUE,]
loandata$Loan.Purpose[is.na(loandata$Loan.Purpose) == TRUE] = names(which.max(table(loandata$Loan.Purpose)))
table(loandata$Loan.Purpose)
unique(loandata$Loan.Purpose)
loandata$F.Loan.Purpose = as.factor(loandata$Loan.Purpose)
loandata$N.Loan.Purpose = as.numeric(loandata$F.Loan.Purpose)
table(loandata$Loan.Purpose)
table(loandata$N.Loan.Purpose)
class(loandata$N.Loan.Purpose)

#library(car)
#loandata$Dummy.Loan.Purpose = recode(loandata$Loan.Purpose,"'home_improvement'=1;'educational'= 2;'credit_card'=3; 'small_business'=4;'debt_consolidation'=5;'car'= 6;'other'=7;'major_purchase' =8;'wedding'=9;'house'=10;'medical'=11;'moving'=12;'vacation'=13;'renewable_energy'=14")


loandata$Debt.To.Income.Ratio = as.numeric(gsub("%","",loandata$Debt.To.Income.Ratio))
sum(is.na(loandata$Debt.To.Income.Ratio))
loandata[is.na(loandata$Debt.To.Income.Ratio) == TRUE,]
loandata$Debt.To.Income.Ratio[is.na(loandata$Debt.To.Income.Ratio) == TRUE] = mean(loandata$Debt.To.Income.Ratio,na.rm = TRUE)
loandata$Debt.To.Income.Ratio[loandata$Debt.To.Income.Ratio == "NA"] = mean(loandata$Debt.To.Income.Ratio,na.rm = TRUE)


loandata$State[loandata$State=="."]= names(tail(sort(table(loandata$State)),1))
loandata$State[is.na(loandata$State) == TRUE] = names(tail(sort(table(loandata$State)),1))
sum(is.na(loandata$State))
loandata[is.na(loandata$State) == TRUE,]
loandata$F.State=as.factor(loandata$State)
loandata$N.State = as.numeric(loandata$F.State)


table(loandata$Home.Ownership)
unique(loandata$Home.Ownership)
sum(is.na(loandata$Home.Ownership))
loandata[is.na(loandata$Home.Ownership) == TRUE,]
loandata$Home.Ownership[is.na(loandata$Home.Ownership) == TRUE] = "NONE"
loandata$Home.Ownership[loandata$Home.Ownership == "."] = "NONE"
loandata$F.Home.Ownership = as.factor(loandata$Home.Ownership)
loandata$N.Home.Ownership = as.numeric((loandata$F.Home.Ownership))


strsplit(loandata$FICO.Range,"-")[[1]]
strsplit(loandata$FICO.Range,"-")[[1]][1]
strsplit(loandata$FICO.Range,"-")[[1]][2]
strsplit(loandata$FICO.Range,"-")[2]
sapply(strsplit(loandata$FICO.Range,"-"), head,1)
sapply(strsplit(loandata$FICO.Range,"-"), tail,1)
loandata$FICO.Range = (as.numeric(sapply(strsplit(loandata$FICO.Range,"-"), head,1)) + as.numeric(sapply(strsplit(loandata$FICO.Range,"-"), tail,1)))/2
sum(is.na(loandata$FICO.Range))

# tapply command applied function on the variable grouped by with some other variables.

loandata$Open.CREDIT.Lines = as.numeric(loandata$Open.CREDIT.Lines)
sum(is.na(loandata$Open.CREDIT.Lines))
summary(loandata$Open.CREDIT.Lines)
hist(loandata$Open.CREDIT.Lines)
loandata$Open.CREDIT.Lines[is.na(loandata$Open.CREDIT.Lines )==TRUE] = median(loandata$Open.CREDIT.Lines,na.rm = TRUE)


loandata$Revolving.CREDIT.Balance = as.numeric(loandata$Revolving.CREDIT.Balance)
sum(is.na(loandata$Revolving.CREDIT.Balance))
hist(loandata$Revolving.CREDIT.Balance)
summary(loandata$Revolving.CREDIT.Balance)
loandata$Revolving.CREDIT.Balance[is.na(loandata$Revolving.CREDIT.Balance)==TRUE] = 270800

loandata$Inquiries.in.the.Last.6.Months = as.numeric(loandata$Inquiries.in.the.Last.6.Months)
sum(is.na(loandata$Inquiries.in.the.Last.6.Months))
hist(loandata$Inquiries.in.the.Last.6.Months)
loandata$Inquiries.in.the.Last.6.Months[is.na(loandata$Inquiries.in.the.Last.6.Months)==TRUE] = median(loandata$Inquiries.in.the.Last.6.Months,na.rm = TRUE)

table(loandata$Employment.Length)
loandata$Employment.Length[loandata$Employment.Length == "."] = names(which.max(table(loandata$Employment.Length)))
loandata$Employment.Length[is.na(loandata$Employment.Length) == TRUE] = names(which.max(table(loandata$Employment.Length)))
loandata$Employment.Length[loandata$Employment.Length == "n/a"] = names(which.max(table(loandata$Employment.Length)))
loandata$F.Employment.Length=as.factor(loandata$Employment.Length)
loandata$N.Employment.Length = as.numeric(loandata$F.Employment.Length)


str(loandata)
summary(loandata)
View(loandata)
loandata1 = loandata[,!names(loandata) %in% c(
  "Loan.Purpose",
  "F.Loan.Purpose",
  "State",
  "F.State", 
  "Home.Ownership",
  "F.Home.Ownership", 
  "Employment.Length",
  "F.Employment.Length","Debt.To.Income.Ratio","Revolving.CREDIT.Balance","N.State","N.Employment.Length","Monthly.Income")]

loandata2=loandata1[complete.cases(loandata1),]
nrow(loandata2)

#Split DAta
set.seed(1)
rowids = sample(1:nrow(loandata2), 0.75*nrow(loandata2))
DataModel=loandata2[rowids,]
test =loandata2[-rowids,]

#Model
LRM = lm(Interest.Rate ~ ., data = DataModel)
summary(LRM)

#Check for Multicolinearity Keep for VIF<5
library(car)
checkmulti=vif(LRM)


#assumption check
#1-Autocorrelation
durbinWatsonTest(LRM) #Close to 2 is good
?durbinWatsonTest

#2 - Normality of errors with mean 0
hist(residuals(LRM))

#3 - Homoskedasticity 
plot(fitted(LRM),residuals(LRM))

rmse_model = sqrt(mean(residuals(LRM))^2)
rmse_model

#Predict on test data
pred = predict(LRM, newdata=test)
Res = cbind.data.frame(Actual=test$Interest.Rate, Pred=pred, Error = (test$Interest.Rate-pred))

plot(Res$Actual,Res$Pred)

rmse_test = sqrt(mean(Res$Error^2))
rmse_test
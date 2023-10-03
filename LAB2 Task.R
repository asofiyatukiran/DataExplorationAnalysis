getwd()
churnTrain<-read.csv("D:/Bachelor's CS/Semester 3/TEB2043 - Data Science/Lab/LAB2/Churn_Train.csv")

#Check for missing values
churnTrain$Monthly.Charges
is.na(churnTrain$Monthly.Charges)

#replace the missing value
churnTrain <- churnTrain %>%
  mutate(Total.Charges=replace(Total.Charges, is.na(Total.Charges),median(Total.Charges, na.rm=T)))
is.na(churnTrain$Total.Charges)

#calculate descriptive statistic using describe()
describe(churnTrain)
describe(churnTrain, Contract, Payment.Method, Monthly.Charges) #Select columns by name
describe(churnTrain, Contract:Monthly.Charges) #Select all column between sales and incomes(include)
describe(churnTrain, -(Contract:Monthly.Charges)) #Select all column except sales until income

#test normality
normality(churnTrain)
normality(churnTrain, Contract, Payment.Method, Monthly.Charges) #Select columns by name
normality(churnTrain, Contract:Monthly.Charges) #Select all column between sales and incomes(include)
normality(churnTrain, -(Contract:Monthly.Charges)) #Select all column except sales until income

#plot normality
plot_normality(churnTrain,Monthly.Charges,Total.Charges)

#calculate correlate
correlate(churnTrain)
correlate(churnTrain, Payment.Method, Monthly.Charges, Total.Charges) #Select columns by name
correlate(churnTrain, Payment.Method:Total.Charges) #Select all column between sales and incomes(include)
correlate(churnTrain, -(Payment.Method:Total.Charges)) #Select all column except sales until income

#plot correlate
churnTrain %>%
  correlate()%>%
  plot()

correlate(churnTrain,Monthly.Charges,Total.Charges) %>%
  plot()

#EDA based on target variable
category <- target_by(churnTrain, Senior.Citizen)

#EDA when target variable is categorical, predictor is numerical
category_num <- relate(category, Monthly.Charges)
category_num
summary(category_num)
plot(category_num)

#EDA when target variable is categorical, predictor is categorical
category_cat <- relate(category, Total.Charges)
category_cat
summary(category_cat)
plot(category_cat)

#EDA when target variable is numerical, predictor is numerical
number <- target_by(churnTrain, Monthly.Charges)
number_num <- relate(number,Total.Charges)
number_num
summary(number_num)
plot(number_num)

#EDA when target variable is numerical, predictor is categorical
number_cat <- relate(number,Total.Charges)
number_cat
summary(number_cat)
plot(number_cat)


churnTrain %>%
  eda_paged_report(subtitle = "churnTrain",
                              output_dir = "./", output_file = "EDA.pdf", 
                              theme = "blue")

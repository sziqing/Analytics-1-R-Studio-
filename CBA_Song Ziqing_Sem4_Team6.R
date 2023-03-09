library(data.table)
library(tidyr)
library(ggplot2)
library(rpart)
library(rpart.plot)
library(caTools)

homeloan2.dt <- fread("homeloan2.csv")

#Q1a
#Check data type for each variable
sapply(homeloan2.dt, class)

#Correct the data types
homeloan2.dt$Gender <- as.factor(homeloan2.dt$Gender)
homeloan2.dt$Married <- as.factor(homeloan2.dt$Married)
homeloan2.dt$Dependents <- as.factor(homeloan2.dt$Dependents)
homeloan2.dt$Education <- as.factor(homeloan2.dt$Education)
homeloan2.dt$ApplicantIncome <- as.numeric(homeloan2.dt$ApplicantIncome)
homeloan2.dt$LoanAmount <- as.numeric(homeloan2.dt$LoanAmount)
homeloan2.dt$Property_Area <- as.factor(homeloan2.dt$Property_Area)
homeloan2.dt$Self_Employed <- as.factor(homeloan2.dt$Self_Employed)
homeloan2.dt$Credit_Score <- as.factor(homeloan2.dt$Credit_Score)
homeloan2.dt$Loan_Status <- as.factor(homeloan2.dt$Loan_Status)


#Q1b
sum(is.na(homeloan2.dt)) #63 NA's
sum(is.null((homeloan2.dt))) #0 NULL's
sum(duplicated(homeloan2.dt)) #0 row duplicates
sum(duplicated(homeloan2.dt$Loan_ID)) #Assuming Loan_ID = Primary key, there are 0 primary key duplicates
colSums(homeloan2.dt == "")
#Shows that Gender, Married, Dependents, Self_Employed columns have missing values
#While Loan_Amount_Term & Credit_Score columns have NA values

#Gender mode function
gender_mode_fn <- function(Gender){
  distinct_gender <- unique(Gender)
  distinct_tabulate1 <- tabulate(match(Gender, distinct_gender))
  distinct_gender[which.max(distinct_tabulate1)]
}
gender_mode_fn(homeloan2.dt$Gender) #States that there are more Males
#Therefore, replace empty values in Gender column with "Male"
homeloan2.dt$Gender[which(homeloan2.dt$Gender == "")] <- "Male"


#Married mode function
married_mode_fn <- function(Married){
  distinct_married <- unique(Married)
  distinct_tabulate2 <- tabulate(match(Married, distinct_married))
  distinct_married[which.max(distinct_tabulate2)]
}
married_mode_fn(homeloan2.dt$Married) #States that there are more "Yes"
#Therefore, replace empty values in Married column with "Yes"
homeloan2.dt$Married[which(homeloan2.dt$Married == "")] <- "Yes"


#Dependents mode function
dep_mode_fn <- function(Dependents){
  distinct_dep <- unique(Dependents)
  distinct_tabulate3 <- tabulate(match(Dependents, distinct_dep))
  distinct_dep[which.max(distinct_tabulate3)]
}
dep_mode_fn(homeloan2.dt$Dependents) #States that there are more "0"
#Therefore, replace empty values in Dependents column with 0
homeloan2.dt$Dependents[which(homeloan2.dt$Dependents == "")] <- "0"


#Self_Employed mode function
se_mode_fn <- function(Self_Employed){
  distinct_se <- unique(Self_Employed)
  distinct_tabulate4 <- tabulate(match(Self_Employed, distinct_se))
  distinct_se[which.max(distinct_tabulate4)]
}
se_mode_fn(homeloan2.dt$Self_Employed) #States that there are more "No"
#Therefore, replace empty values in Self_Employed column with "No"
homeloan2.dt$Self_Employed[which(homeloan2.dt$Self_Employed == "")] <- "No"


#Loan_Amount_Term mode function
lat_mode_fn <- function(Loan_Amount_Term){
  distinct_lat <- unique(Loan_Amount_Term)
  distinct_tabulate5 <- tabulate(match(Loan_Amount_Term, distinct_lat))
  distinct_lat[which.max(distinct_tabulate5)]
}
lat_mode_fn(homeloan2.dt$Loan_Amount_Term) #States that there are more "360"
#Therefore, replace empty values in Loan_Amount_Term column with 360
homeloan2.dt$Loan_Amount_Term[which(is.na(homeloan2.dt$Loan_Amount_Term))] <- 360
#Change back Loan_Amount_Term to integer
homeloan2.dt$Loan_Amount_Term <- as.integer(homeloan2.dt$Loan_Amount_Term)

#Credit_Score mode function
cs_mode_fn <- function(Credit_Score){
  distinct_cs <- unique(Credit_Score)
  distinct_tabulate6 <- tabulate(match(Credit_Score, distinct_cs))
  distinct_cs[which.max(distinct_tabulate6)]
}
cs_mode_fn(homeloan2.dt$Credit_Score) #States that there are more "1"
#Therefore, replace empty values in Credit_Score column with 1
homeloan2.dt$Credit_Score[which(is.na(homeloan2.dt$Credit_Score))] <- "1"

#Double check
colSums(homeloan2.dt == "") #Shows that there are no missing values in all columns

#To conclude Q1b, all missing/NA values have been replaced using the mode function. The rows with these missing/NA values should NOT be deleted. If deleted, over 100 rows will be removed, which will ~20% of the given dataset, which is not ideal as data is extremely valuable.





#Q2
summary(homeloan2.dt) #Check for the min, max, etc. for all columns, also checks for outliers in categorical data columns

#Zooming into the ApplicantIncome & CoapplicantIncome columns to see the outliers
#Assuming that values between 0 & 100 are the illogical values (Using $100 as benchmark)
which(homeloan2.dt$ApplicantIncome > 0 & homeloan2.dt$ApplicantIncome < 100) #None
which(homeloan2.dt$CoapplicantIncome > 0 & homeloan2.dt$CoapplicantIncome < 100) #Row 414

homeloan2.dt[414,]
#Row 414 shows that there is a value of 16.12 in CoapplicantIncome. This is unusual as the other values in the CoapplicantIncome column has no decimal points. 

#I then proceeded to check if other values in CoapplicantIncome have decimal points
which(round(homeloan2.dt$CoapplicantIncome) != homeloan2.dt$CoapplicantIncome) #Row 273 & 414
homeloan2.dt[273,] #CoapplicantIncome has a decimal, value is 985.8

#Key findings are these two values: 16.12 & 985.8.
#The original plan was to change 16.12 to 1612 & change 985.8 to 9858. However, before doing so, I will double check the data by finding its lower & upper limit.

#Checking for data consistency under ApplicantIncome (looking for decimals)
which(round(homeloan2.dt$ApplicantIncome) != homeloan2.dt$ApplicantIncome) #integer(0), which means data is consistent under ApplicantIncome

#Using boxplot to double check for the outliers in ApplicantIncome
boxplot(homeloan2.dt$ApplicantIncome)
summary(homeloan2.dt$ApplicantIncome)

ai_IQR = IQR(homeloan2.dt$ApplicantIncome)
ai_LL = (2887 - 1.5*ai_IQR) #Lower limit is an exaggerated amount of -1413.875, negative amount is not allowed in income.
ai_UL = (5754 + 1.5*ai_IQR) #Upper limit is 10054.88, however this does not mean that values above this should be deleted as data is valuable.


#Using boxplot to double check for the outliers in CoapplicantIncome
boxplot(homeloan2.dt$CoapplicantIncome)
summary(homeloan2.dt$CoapplicantIncome)

ai_LL2 = (0 - 1.5*ai_IQR) #Lower limit is an exaggerated amount of -4300.875, negative amount is not allowed in income. 
#However, this ultimately proves a chance that 16.12 could be accepted as data.
ai_UL2 = (2312 + 1.5*ai_IQR) #Upper limit is 6612.875, however this does not mean that values above this should be deleted as data is valuable.

sum(homeloan2.dt$LoanAmount == 0) #LoanAmount has no 0 values

#Using ggplot (bar graph) for the remaining categorical variables
ggplot(homeloan2.dt, aes(x = Gender)) + geom_bar()
ggplot(homeloan2.dt, aes(x = Married)) + geom_bar()
ggplot(homeloan2.dt, aes(x = Dependents)) + geom_bar()
ggplot(homeloan2.dt, aes(x = Education)) + geom_bar()
ggplot(homeloan2.dt, aes(x = Self_Employed)) + geom_bar()
ggplot(homeloan2.dt, aes(x = Credit_Score)) + geom_bar()
ggplot(homeloan2.dt, aes(x = Property_Area)) + geom_bar()
ggplot(homeloan2.dt, aes(x = Loan_Status)) + geom_bar()

#Given dataset is therefore fully cleaned, naming convention is changed as a habit.
c_homeloan.dt <- homeloan2.dt #Final cleaned dataset

#(As mentioned above regarding the values of 16.12 & 985.8):
#Based on logic, it is possible that a person's monthly salary could be $16.12 (although unusual). One reason could be a person is only working part-time for 1 day. The value 985.8 is a possibility as well. Therefore, no rows are being deleted nor further cleaning is required.

nrow(c_homeloan.dt) #592 rows in the cleaned dataset





#Q3a
#Use Loan_Status as Categorical Y in CART
set.seed(8)
m1 <- rpart(Loan_Status ~ Loan_ID, data = c_homeloan.dt, method = 'class', 
            control = rpart.control(minsplit = 2, cp = 0))
print(m1)
rpart.plot(m1)
printcp(m1)
plotcp(m1)

#The geometric mean cannot be found and the 10 fold cross validation error is 1 before splitting.


#Use Loan_Status as Categorical Y value in Logistic Regression 
glm_hloan <- glm(Loan_Status~., family = binomial, data = c_homeloan.dt)
summary(glm_hloan)
# When running the Logistic Regression (Loan_ID is classified as a categorical variable), the z values shown for all the Loan_ID is 0. This indicates that there is no relationship between Loan_ID and Loan_Status.

#To conclude Q3a, Loan_ID should not be used to predict Loan_Status. Variables like Loan_ID are usually the primary key of datasets and serve no purpose other than uniquely identifying a certain record in the dataset. Furthermore, the CART & Logistic Regression models have proved that there is no correlation between Loan_ID & Loan_Status. Therefore, Loan_ID shows no significance in predicting Loan_Status.


#Q3b
#Splitting into train set & test set
set.seed(8)
split = sample.split(c_homeloan.dt$Loan_Status, SplitRatio = 0.7)
train_data = subset(c_homeloan.dt, split == TRUE, select = -Loan_ID)
test_data = subset(c_homeloan.dt, split == FALSE, select = -Loan_ID)

#CART on Predictive Accuracy
#Using train set (CART)
m2_train <- rpart(Loan_Status ~., data = train_data, method = 'class', 
                  control = rpart.control(minsplit = 2, cp = 0))
print(m2_train)
rpart.plot(m2_train)
printcp(m2_train)
plotcp(m2_train) #cp = 0.077

m2_train <- prune(m2_train, cp = 0.077)
rpart.plot(m2_train, nn = T, main = "Train set: Pruned Tree with cp = 0.077")

#Trying on testset (CART)
test_cart_predict <- predict(m2_train, newdata = test_data, type = "class")
mean(test_data$Loan_Status == test_cart_predict) #0.8248588
#Therefore, CART (testset) has a predictive accuracy of 82.49%


#Logistic Regression on Predictive Accuracy
#Using trainset (Log Reg)
glm_train <- glm(Loan_Status ~.,  family = binomial, data = train_data)
summary(glm_train)

odds_ratio_ci <- exp(confint(glm_train))
odds_ratio_ci
#Shows that the key factors are: Credit_Score, Married, Property_Area
#Credit_Score is the most significant factor
#Property_Area = B is the significant factor, not A or C

glm_train <- glm(Loan_Status~ Credit_Score + Married + Property_Area, family = binomial, data = train_data)
threshold <- 0.5

#Trying on testset (Log Reg)
prob_test <- predict(glm_train, newdata = test_data, type = "response")
predict_test <- ifelse(prob_test > threshold, "Y", "N")
mean(test_data$Loan_Status == predict_test) #0.8248588
#Therefore, Logistic Regression (testset) has an accuracy of 82.49%


#Table for Q3b
cartAcc = round((mean(test_data$Loan_Status == test_cart_predict))*100,2)
logRegAcc = round((mean(test_data$Loan_Status == predict_test))*100,2)
list <- c(cartAcc, logRegAcc)

acc_table <- matrix(list, ncol = 2, nrow = 1)
colnames(acc_table) <- c("CART", "Logistic Regression")
rownames(acc_table) <- "Accuracy(%)"
as.table(acc_table)
#As shown in the table, the predictive accuracy of both CART & Logistic Regression are the same at 82.49%. However, CART is the more accurate model as it bisects the feature space into smaller and smaller regions   (10-fold cross validation), whereas Logistic Regression fits a single line to divide the space exactly into two. Also, CART handles colinearity better than Logistic Regression.


#Q3c
#CART
m2_train$variable.importance
#CART states that Credit_Score is the key factor that determines Loan_Status

#Logistic Regression
set.seed(8)
split = sample.split(c_homeloan.dt$Loan_Status, SplitRatio = 0.7)
train_data = subset(c_homeloan.dt, split == TRUE, select = -Loan_ID)
glm_train <- glm(Loan_Status ~.,  family = binomial, data = train_data)
odds_ratio_ci <- exp(confint(glm_train))
odds_ratio_ci
#The Odds Ratio Confidence Interval shows that Credit_Score, Married & Property_Area are statistically significant.
#Therefore, Credit_Score, Married & Property_Area are the key factors that determine Loan_Status.


#Q3d
#Confusion Matrix (CART)
test_cart_predict <- predict(m2_train, newdata = test_data, type = "class")
mean(test_data$Loan_Status == test_cart_predict)
cm_cart <- table(Predicted_Loan_Status_CART = test_cart_predict, Actual_Loan_Status = test_data$Loan_Status, deparse.level = 2)

#Confusion Matrix (Logistic Regression) 
glm_train <- glm(Loan_Status~ Credit_Score + Married + Property_Area, family = binomial, data = train_data)
threshold <- 0.5
prob_test <- predict(glm_train, newdata = test_data, type = "response")
predict_test <- ifelse(prob_test > threshold, "Y", "N")
cm_lr <- table(Predicted_Loan_Status_LR = predict_test, Actual_Loan_Status = test_data$Loan_Status,deparse.level = 2)

cm_cart
cm_lr
#The values of both confusion matrixes are the same.
#Using confusion matrix, the most serious prediction errors are the False Negatives & False Positives.
#There is 1 case of False Negatives: The model misclassified Loan_Status to be "N" when the actual Loan_Status is "Y".
#There are 30 cases of False Positives: The model misclassified Loan_Status to be "Y" when the actual Loan_Status is "N".





#Q5 - Based on final cleaned dataset
#Graph 1
ggplot(c_homeloan.dt, aes(y=Loan_Status, x=Gender, fill=Loan_Status)) + geom_bar(position="stack", stat="identity")

#Graph 1 shows that significantly, more males get their loans approved. However, it is not fair to directly assume this as there could be more male records in the dataset as compared to females.
nrow(c_homeloan.dt[c_homeloan.dt$Gender == 'Male']) #483
nrow(c_homeloan.dt[c_homeloan.dt$Gender == 'Female']) #109
109/592*100 #Shows that in the dataset, only 18.41% records belong to females


#As proven from the CART & Logistic Regression models: Credit_Score, Married & Property_Area are the statistically significant variables that determines Loan_Status. Therefore, I will zoom into be plotting these variables against Loan_Status for a better representation.

#Graph 2
ggplot(c_homeloan.dt, aes(x=Credit_Score, y=Loan_Status, color=Loan_Status)) + geom_jitter() + labs(title = "Does Credit Score determine Loan Status?")
#Graph 2 proves the accuracy of the CART & Logistic Regression models as it can be seen that the main reason why loans are approved is due to having a good credit score.

#Graph 3
ggplot(c_homeloan.dt, aes(x=Married, y=Loan_Status, color=Loan_Status)) + geom_jitter() + labs(title = "Does Marriage determine Loan Status?")
#Graph 3 shows that when applicants are married, they are more likely to get their loan approved. One possible reason why banks implement this reasoning is because the applicant's partner could help repay the loan in an event where the applicant is unable to.

#Graph 4
ggplot(c_homeloan.dt, aes(x=Property_Area, y=Loan_Status, color=Loan_Status)) + geom_jitter() + labs(title = "Does Property Area determine Loan Status?")
#Although not clearly visible, Graph 5 tells us that people who apply a loan to pay for Property B are more likely to get their loans approved.

#Graph 5
ggplot(c_homeloan.dt, aes(x=Dependents, y=Loan_Status, color=Dependents)) + geom_jitter() + labs(title = "Does No. of Dependents determine Loan Status?")
#Graph 5 shows that when an applicant has 0 dependents, the bank is more likely to approve the loan. This reasoning is logical as having more dependents results in having more commitments, which in turn may lower the income of the applicant. This therefore likely diminishes the ability of the applicant to repay the loan.


#In conclusion, Graphs 2-5 showcase significant variables that affect Loan Status, instead of Gender. Therefore, it is concluded that there is no evidence of gender discrimination in loan approved.


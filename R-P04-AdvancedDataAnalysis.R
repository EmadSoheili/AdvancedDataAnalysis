
# R-P04-AdvancedDataAnalysis

# Section 0 ----
# Preparation

setwd("E:/Mine/Maktab Khooneh/Data Analysis/4 - Advanced Data Analysis R/Final project")
getwd()

  # Libraries
library(ggplot2)
library(moments)
library(car)
library(dplyr)
library(plotrix)
library(e1071)


# Section 1 - Vehicle Price ----

  # Import Data
data1 = read.csv("dataset1.csv",header = T)

# Section 1 - Question 1 & 2 ----

str(data1)
  # Except the Fuel Type parameter, Other parameters are integer. Maybe we can change Fuel types to equivalent numbers (such as 1, 2, 3) to make it easier to analyse uniform data. I will make my decision about it later.

summary(data1)
  # Fortunately. There are no NAs. Minimums and maximums seems right. I think there are no wrong data or outlier (at first glance).

dim(data1)
  # There are 1325 records, containing 10 different charactristics.

head(data1)

unique(data1$FuelType)
  # There are only 3 types of fuel including: "Diesel" "Petrol" "CNG". I will change them to 1, 2, and 3 in order to make them integer. However I will put the new data to another data frame so as to keep my source untouched.

data2 = data1
head(data2,30)
data2$FuelType[data2$FuelType == "Diesel"] = 1
data2$FuelType[data2$FuelType == "Petrol"] = 2
data2$FuelType[data2$FuelType == "CNG"] = 3
data2$FuelType = as.integer(data2$FuelType)

str(data2)
  # I have changed the fuel type values to numeric. 1, 2, and 3 are standing for Diesel, Petrol, and CNG. I did it to have an all-intger dataset.

# Section 1 - Question 3 ----

  # Histogram Charts
par(mfrow = c(2,5))
for (i in 1:10) {
  hist(data2[,i], xlab = "", main = paste(names(data2)[i]," Histogram"), col = "sky blue")
}

  # Scatter Charts
par(mfrow = c(1, 3))
for (i in c(2,3,10)) {
  plot(data2[,i],data2[,1], xlab = "", ylab = "", main = paste("Price vs. ",names(data2)[i]), col = "sky blue", pch = 20, lwd = 3)
  }
  # First Chart: Obviously, age and price has negative correlation.
  # Second Chart: The correlation between KM and price is negative but it seems not linear since it has a steep decrease and the converge to e limit.
  # Third Chart: It seems like there is no pattern between price and weight.
  # Other parameters are categorical. They are not suitable for Scatter charts.

  # Correlation table.
head(data2)
round(cor(data2[,c(1,2,3,10)]),2)
  # Considering the correlation table, price is negatively affected by age (it has been illustrated in scatter charts too)
  # KM has also a negative impact on the price, but not as much as age.
  # I reckon there is no meaningful correlation between weight and price.
  # Moreover, age, KM, and weight have no meaningful impact on each other. It worth mentioning that the age and KM is a little correlated but it is not much (39%)

table (data2$FuelType)
table (data2$HP)
table (data2$MetColor)
table (data2$Automatic)
table (data2$CC)
table (data2$Doors)
  # CNG as a fuel type is very rare in the data set.
  # HP has 9 columns. I wll consider this data as a categorical data.
  # Automatic vehicles are rare in this case.
  # I consider CC as a categrical variable too.

# Section 1 - Question 4 ----

  # Devide dataset
set.seed(654321)
train_cases = sample(1:nrow(data2), nrow(data2)*0.7)
data2$tlabel = NA
data2$tlabel[train_cases] = "train"
data2$tlabel[-train_cases] = "test"
head(data2)

train = data2[train_cases,]
test = data2[-train_cases,]
  
  # I think summary would not be very helpful in this stage (we have 10 columns and 3 data set, including source data, train, and test). I would prefer have some plots instead.

  # Scatter Chart
par(mfrow = c(2,5))
for (i in 2:10){
  plot(data2[,i], data2$Price, col = ifelse(data2$tlabel == "train", "red","blue"), pch = 20, xlab = names(data2[i]), ylab = "Price", lwd = 3)
}
  # The red and blue dots are scattered pretty well all around the data. I believe they are both acceptable representatives for the source data.

par(mfrow = c(1,1))

# Section 1 - Question 5 ----

head(data2)
m1 = lm(Price ~ KM, data = train)

summary(m1)
  # r-squared = 0.2626 and it seems the correlation is not an strong one. Maybe the KM itself cannot predict price and some other parameters are involved.
  # F test: p-value is almost 0, hence there is a linear correlation.
  # t test: KM is a significant parameter.
  # residuals' median is 321 (it is almost 0 considering the maximum KM)

  # Requirements' check

hist(m1$residuals, probability = T)
lines(density(m1$residuals))
  # It seems not normal to me. This is positively skewed.

qqnorm(m1$residuals)
qqline(m1$residuals)
  # There is a strong deviation from the line. Again it seems not normal.

jarque.test(m1$residuals)
anscombe.test(m1$residuals)
  # Both tests' p-values is less than 0.05, therefore the residuals' distributin is not normal.

plot(m1)
  # The red lines is not horizontal.
  # The cook distance is OK but the leverage is considerable.

  # Conclusion:
    # We are unable to use the Least Squares Regression method.
    # There is Hedrocedasticity.
    # This is not a good model.

# Section 1 - Question 6 ----

plot(train$KM,train$Price, xlab = "KM", ylab = "Price", col = "sky blue", pch = 20)
  # It seems there is no linear correlation.

  # Quadratic Regression
m2 = lm(Price ~ KM + I(KM^2), data = train)

summary(m2)
  # r-squeared = 0.2928
  # F test: p-value is less than 0.05, thus we have a correlation.
  # t test: both KM and KM^2 are significant.
  # Residuals' median is negligible.

  # Requirements' check

hist(m2$residuals, probability = T)
lines(density(m2$residuals))
  # It seems not normal to me. Still this is positively skewed.

qqnorm(m2$residuals)
qqline(m2$residuals)
  # Still there is a strong deviation from the line. Again, it seems not normal.

jarque.test(m2$residuals)
anscombe.test(m2$residuals)
  # Still both tests' p-values is less than 0.05, therefore the residuals' distributin is not normal.

plot(m2)
  # We have a minor Hedrocedasticity, maybe negligible.
  # Cook's distance: No outlier but leverage is 0.1 (too high)

ggplot(train, aes(KM, Price))+
  geom_point()+
  geom_smooth(method = "lm", formula = y ~ x + I(x^2))+
  geom_smooth(method = "lm", formula = y ~ x, color = "red")+
  ggtitle("Price vs KM")
  # Despite the fact that the Quadratic Regression is better than the linear one in this case, I think it is not good enough. We have to improve our model.

# Section 1 - Question 7 ----

train2 = train[-which(rownames(train) == 7 | rownames(train) == 32 | rownames(train) == 33),]

dim(train)
dim(train2)
  # 3 Outlier records have been deleted.

m2_2 = lm(Price ~ KM + I(KM^2), data = train2)

summary(m2_2)
  # r-squeared = 0.3131
  # F test: p-value is less than 0.05, thus we have a correlation.
  # t test: both KM and KM^2 are significant.
  # Residuals' median is negligible.

  # Requirements' check

hist(m2_2$residuals, probability = T)
lines(density(m2_2$residuals))
  # It seems more normal.

qqnorm(m2_2$residuals)
qqline(m2_2$residuals)
  # Less deviation from the qqline, but it gets stronger at the higher values.

jarque.test(m2_2$residuals)
anscombe.test(m2_2$residuals)
  # The tests' resulsts is getting better and better.

plot(m2_2)
  # We have a minor Hedrocedasticity, maybe negligible.
  # Cook's distance: No outlier but leverage is 0.1 (too high)

  # Comparison
    #m2
      # r^2 = 0.2928
      # Price = 1.378e+04 - 7.015e-02 * KM + 1.890e-07 * KM^2
      # Residual
        #[-6280.7,8609.9]
        # Residual error = 2029

    #m2_2
      # r^2 = 0.3131
      # Price = 1.387e+04 - 7.264e-02 * KM + 1.988e-07 * KM^2
      # Residual
        #[-6369.0,6932.2]
        # Residual error = 1975

    # Conclusion
      # m2_2 is better than m2 since the residula has smaller values.
      # r-squared is a little higher.

# Section 1 - Question 8 ----

vif(m2)
  # There is multicollinearity issue for both KM and KM^2

train$KM_scaled = scale(train$KM)
head(train)

m2_3 = lm(Price ~ KM_scaled + I(KM_scaled^2), data = train)

summary(m2_3)
  # r-squeared = 0.2928
  # F test: p-value is less than 0.05, thus we have a correlation.
  # t test: both KM_scaled and KM_scaled^2 are significant.
  # Residuals' median is negligible.

# Requirements' check

hist(m2_3$residuals, probability = T)
lines(density(m2_3$residuals))
  # It seems normal up to an extent.

qqnorm(m2_3$residuals)
qqline(m2_3$residuals)
  # Still we have considerable deviation at higher values.

jarque.test(m2_3$residuals)
anscombe.test(m2_3$residuals)
  # Not normal.

plot(m2_3)
  # Still we have a minor Hedrocedasticity, maybe negligible.
  # Cook's distance: No outlier but leverage is more than 0.1 (too high)
  
  # m2_2 model was better I assume.

# Section 1 - Question 9 ----

head(train)

m3 = lm(Price ~ KM_scaled + I(KM_scaled^2) + Age + FuelType + HP + MetColor + Automatic + CC + Doors + Weight, data = train)

summary (m3)
  # r-squeared = 0.7879
  # F test: p-value is less than 0.05, thus we have at least one correlation.
  # Residuals' median is negligible.
  # t test
    # KM_scaled^2 is significant up to an extent. I will keep it for now.
    # MetColor, CC, Doors are not significant. I will remove them.

m3 = lm(Price ~ KM_scaled + I(KM_scaled^2) + Age + FuelType + HP + Automatic + Weight, data = train)

summary (m3)
  # r-squeared = 0.7867
  # F test: p-value is less than 0.05, thus we have at least one correlation.
  # Residuals' median is negligible.
  # t test
    # All parameteres are significant except FuelType. I will keep it for now.

# Section 1 - Question 10 ----

train$IfPetrol = ifelse(train$FuelType == 2, TRUE, FALSE)
head(train)

m4 = lm(Price ~ KM_scaled + I(KM_scaled^2) + Age + IfPetrol + HP + Automatic + Weight, data = train)

summary (m4)
  # r-squeared = 0.7891
  # F test: p-value is less than 0.05, thus we have at least one correlation.
  # Residuals' median is negligible.
  # t test
    # All parameteres are significant. However KM_scaled^2, HP, and Automatic needs further investigation.

# Requirements' check

hist(m4$residuals, probability = T)
lines(density(m4$residuals))
  # Further investigation is needed.

qqnorm(m4$residuals)
qqline(m4$residuals)
  # Still we have considerable deviation at lower values.

jarque.test(m4$residuals)
anscombe.test(m4$residuals)
  # Not normal.

plot(m4)
  # We have a Hedrocedasticity.
  # Cook's distance: there is an outlier and the leverage is more than 0.4 (too high)

# Section 1 - Question 11 ----

train3 = train
train3 = train[!(row.names(train) %in% c('82','83','112','114','284','491','803','948','1241','944','586','1272','77','881','354')), ]
  # I have deleted the rows 82, 83, 112, 114, 284, 491, 803, 948, 1241, 944, 586, 1272, 77, 881, 354.

15/nrow(train)
  # I have deleted 1.6% of total data. It is less than 5% and OK.

m4_2 = lm(Price ~ KM_scaled + I(KM_scaled^2) + Age + IfPetrol + HP + Automatic + Weight, data = train3)

hist(m4_2$residuals, probability = T)
lines(density(m4_2$residuals))
  # It seems pretty normal.

qqnorm(m4_2$residuals)
qqline(m4_2$residuals)
  # It seems normal to me.

jarque.test(m4_2$residuals)
anscombe.test(m4_2$residuals)
  # Both p-values are higher than 0.05 and the residual's distribution is normal.

plot(m4_2)
# We have a minor Hedrocedasticity, Maybe negligible.
# Cook's distance: there is no outlier and also the leverage is about 0.1.

summary (m4)
summary (m4_2)
  # Comparison
    # m4
      # r^2 = 0.7891
      # Residual variance: 1111
      # Residual range: [-6180.8,4221.4]
      # Regression coefficients
        # Intercept:1958.257
        # KM_scaled: -445.038
        # KM_scaled^2: -51.260
        # Age: -112.761
        # IfPetrolTRUE: 759.177
        # HP: 8.847
        # Automatic: 423.247
        # Weight: 12.340
      # All coefficients are significant but KM_scaled^2, HP, and Automatic need further investigations.
    # m4_2
      # r^2 = 0.8394
      # Residual variance: 959.9
      # Residual range: [-2858.92,2959.37]
      # Regression coefficients
        # Intercept:-7995.770
        # KM_scaled: -475.128
        # KM_scaled^2: -52.911
        # Age: -108.065
        # IfPetrolTRUE: 1634.433
        # HP: -5.294
        # Automatic: 235.024
        # Weight: 22.074
      # HP and Automatic are not significant and KM_scaled^2 needs further investigations.

# Section 1 - Question 12 ----

test$KM_scaled = scale(test$KM)
test$IfPetrol = ifelse(test$FuelType == 2, TRUE, FALSE)

test$pred = predict(m4_2, test)

test$error = test$pred - test$Price

head(test)

median(test$error)
sd(test$error)
mean(test$error)
max(abs(test$error))
min(abs(test$error))
  # Median: 32.0408
  # Standard Deviation: 1158.139
  # Mean: 95.47038
  # Maximum abs(error): 9605.427
  # Minimum abs(error): 4.909866

  #predicted vs actual
plot(test$Price, test$pred, main = "Predicted vs Actual Price", pch = 20, col = "sky blue")
abline(a = 0, b = 1, col = "red", lwd = 2)
  # Except a few outliers, it seems OK.

hist(test$error, breaks = 20, col = "Sky blue")

boxplot(test$error, col = "Sky blue")
  # There are 4 outlires.

test$error_per =100* abs(test$Price - test$pred)/test$Price

median(test$error_per)
sd(test$error_per)
mean(test$error_per)
max(abs(test$error_per))
min(abs(test$error_per))
  # Median: 7.224718
  # Standard Deviation: 9.882246
  # Mean: 9.216721
  # Maximum abs(error_per): 102.2942
  # Minimum abs(error_per): 0.0516828

quantile(test$error_per,0.8)
  # 80% of the predictions have errors in the range of [-13.97448% , +13.97448%]. Therefore it is OK.
  # The model can act satisfactory according to acceptable normal range.

# Section 1 - Question 13 ----
  
  # Challenges towards the development of this algorithm
    # We need more data.
    # Outliers are making problems.
    # The algorithm must be updatable through time.

  # Solutions
    # Collect more precise data.
    # We have to completely deal with outliers. This was an example but in a real case, we have to eliminate every one of outliers.
    # Use Machine Learning.

  # Tools needed
    # Well trained data collection team
    # Time
    # Knowledge of machine learning




# Section 2 - Question 1 - Bank Case ----

# Import Data
data3 = read.csv("dataset2.csv",header = T)

# Section 2 - Question 2 ----

head(data3)
dim(data3)
  # 5000 records in 12 Columns. There are 12 different variables.
str(data3)
  # Except CCAvg, other variables are integer. I think this would be fine.
  # Some variables are including 0, and 1. Might need a change.
summary(data3)
  # There is no NA.
  # It seems the deviation in some columns is a little high. need further investigations.
  # There are some Experience record with negative values. I have to deal with them.

rn = row.names(data3[data3$Experience < 0,])
data3[rn,2] = mean(data3$Experience)
  # I have replaced the Experience's negative values with the Experience's mean.

# Section 2 - Question 3 ----
par(mfrow = c(2,3))
for (i in c(1:3,5,7)){
  hist(data3[,i], xlab = "", main = colnames(data3[i]), col = "sky blue")
}

par(mfrow = c(1,1))

round(cor(data3[,c(1:3,5,7)]),2)
  # Experience and Age are 99% correlated. I think we have to bring one of them in to calculations.

table (data3$Family)
table (data3$Education)
table (data3$SecuritiesAccount)
table (data3$CDAccount)
table (data3$Online)
table (data3$CreditCard)
table (data3$PersonalLoan)

head(data3[,c(1:3,5,7)])
  # Numerical variables' range is widely different. I think I have to scale the data.

data4 = data3
data4[,c(1:3,5,7)] = scale(data3[,c(1:3,5,7)])
head(data4)
  # I have scaled Age, Experience, Income, CCAvg, and Mortgage.

# Section 2 - Question 4 ----

  # Devide dataset
set.seed(6543)
btrain_cases = sample(1:nrow(data4), nrow(data4) * 0.8)
btrain = data4[btrain_cases,]
btest = data4[-btrain_cases,]

  # I reckon summary would not be useful now. I prefer visualization in this step.

data_temp = data4
data_temp$train_status = NA
data_temp[btrain_cases,"train_status"] = "train"
data_temp[-btrain_cases,"train_status"] = "test"
head(data_temp,30)

par(mfrow = c(2,3))
for (i in c(1:3,5,7)){
  plot(data_temp[,i],data_temp$PersonalLoan, col = ifelse(data_temp$train_status == "train", "sky blue", "red"), pch = 20, xlab = names(data_temp[i]), lwd = 4)
}
  # The red and blue dots are scattered pretty well all around the data. I believe they are both acceptable representatives for the source data.
par(mfrow = c(1,1))

# Section 2 - Question 5 ----

bm1 = glm(PersonalLoan ~ ., data = btrain, family = "binomial")

summary(bm1)

  # Chi-square test
chi = bm1$null.deviance - bm1$deviance
chidf    = bm1$df.null - bm1$df.residual
chiprob = 1 - pchisq(chi, chidf)
chiprob
  # Result is less than 0.05 hence the model is OK.

  # AIC = 1005.7
    # We need it for comparison with other models we will create.

  # Wald test
    # Some variables are significant (p-value < 0.05) and some are not (p-value > 0.05).
  
  # Data size check
    10 * 11 / (sum(btrain$PersonalLoan)/nrow(btrain))
    # We need at least 1149 record Since we have 4000 records in btrain, it is OK.

# Section 2 - Question 6 ----

  # Age, Expeerience, and Mortgage are not significant. I will delete them.
    
bm2 = glm(PersonalLoan ~ . - Age - Experience - Mortgage, data = btrain, family = "binomial")

summary (bm2)

  # Chi-square test
chi = bm2$null.deviance - bm2$deviance
chidf    = bm2$df.null - bm2$df.residual
chiprob = 1 - pchisq(chi, chidf)
chiprob
  # Result is less than 0.05 hence the model is OK.

  # AIC = 1006.3
  # We need it for comparison with other models we will create.

  # Wald test
  # All variables are significant.

# Section 2 - Question 7 ----

btest$probs = predict(bm2, btest, type = "response")
btest$pred_lg = ifelse(btest$probs >= 0.5, 1, 0)
head(btest,50)

btest$status = ifelse(btest$pred_lg == btest$PersonalLoan, 1, 0)
100 * sum(btest$status == 1)/sum(nrow(btest))
  # Prediction rate = 94.9%
  # This rate includes positive and negative results.

  # Confusion matrix
table(btest$PersonalLoan, btest$pred_lg)
  # TP = 58
  # FN = 39
  # TN = 891
  # FP = 12
  # Total = 1000

    # Accuracy = TP + TN / Total
      # Accuracy = 0.949
(58 + 891) / 1000 
  
    # Precision = TP / TP + FP
      # Precision = 0.829
58 / (58 + 12)
      
    # Sensitivity = TP / TP + FN
      # Sensitivity = 0.598
58 / (58 + 39)

    # Specificity = TN / TN + FP
      # Specificity = 0.987
891 / (891 + 12)

  # Model is working perfectly.

# Section 2 - Question 8 ----

head(btrain)
bm3 = naiveBayes(PersonalLoan ~., data = btrain)
bm3

btest$nbp = predict(bm3, btest)
head(btest,30)

btest$nbstatus = ifelse(btest$nbp == btest$PersonalLoan, 1, 0)
100 * sum(btest$nbstatus == 1)/sum(nrow(btest))
  # Prediction rate = 86.9%
  # This rate includes positive and negative results.

  # Confusion matrix
table(btest$PersonalLoan, btest$nbp)
  # TP = 49
  # FN = 48
  # TN = 820
  # FP = 83
  # Total = 1000

  # Accuracy = TP + TN / Total
    # Accuracy = 0.869
(49 + 820) / 1000

  # Precision = TP / TP + FP
    # Precision = 0.371
49 / (49 + 83)

  # Sensitivity = TP / TP + FN
    # Sensitivity = 0.505
49 / (49 + 48)

  # Specificity = TN / TN + FP
    # Specificity = 0.908
820 / (820 + 83)

  # Model is working great.

  # Comparison
    # bm2
      # Accuracy = 0.949
      # Precision = 0.829
      # Sensitivity = 0.598
      # Specificity = 0.987
    # bm3
      # Accuracy = 0.869
      # Precision = 0.371
      # Sensitivity = 0.505
      # Specificity = 0.908

    # Although the bm2 is more accurate (and I choose it over the bm3 model), Naive Bayes algorithm is very simple and fast. It may be useful in some cases which we face too many records and calculations takes time, or when we are not able to handle to many predictive variables. Naive Bayes algorithm might be perfect for first assumptions to give us an acceptable overview of the case.

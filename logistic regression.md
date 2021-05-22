### Model - Logistic Regression
##### Feature Processing/Selection
The feature processing is to convert all text values into numeric values. Here we convert 'female' as 1 and 'male' as 0 in the Gender column. Then we did one-hot encoding to convert multiple category attributes into several new attributes.
```{r Features Processing, warning=FALSE,message=FALSE,echo=FALSE}
# 1 and 0 in 'Gender'
df1<-df
df1$Gender<- ifelse(df$Gender=="M",0, ifelse(df$Gender == "F",1,99))

# one-hot encoding for the rest
library(creditmodel)
df1=one_hot_encoding(df1,c("Marital_Status","Education_Level","Income_Category","Card_Category"))
```

We decided to remove some attributes from logistic regression modeling because logistic regression cannot handle high correlated variables well. Based on the correlation head map shown above, we found three pairs of attributes with strong correlation. They are:

- Customer_Age vs. Months_On_Book 
- Credit_Limit vs. Avg_Open_To_Buy
- Total_Trans_Amt vs. Total_Trans_Ct

We decided to remove one of them from each. We considered Months_on_book as a more direct attribute to show the relationship with customers and the bank. And Credit_Limit is easier to understand comparing with Avg_Pen_To_buy. Besides, we believed the number of transactions is more representative than the total amount of transactions because the number of transactions implies a frequency that customers cooperate with the bank. A larger transaction amount can happen when a customer decides to stop the business with the bank and transfer all money out of the bank. Therefore, we removed attributes Customer_Age, Avg_Open_To_Buy, and Total_Trans_Amt.

In addition, the same problem happens among new attributes created by one-hot encoding. When other categories are all equal to 0, the last categories must be 1. Hence there is a perfect correlation in this situation. We have to remove one category from each original attribute. Here we consider category-Unknown in Marital_Status, Education_Level, Income_Category are meaningless in our analysis of our project. And in Card_Category, the Platinum has only `r sum(df$Card_Category=='Platinum')` active values. It shows less significance in the analysis. Therefore, we removed all Unknown attributes and Card_Category.Platinum.
```{r Features Selection, echo=FALSE}
df1 <- df1 %>% 
  select(-c(Avg_Open_To_Buy,Customer_Age,Total_Trans_Amt, Marital_Status.Unknown., Education_Level.Unknown., Income_Category.Unknown.,Card_Category.Platinum.))
```

```{r Split/Standardize Data, echo=FALSE,message=FALSE}
# split the data into train(70%) and test(30%) randomly.
set.seed(53)
dt = sort(sample(nrow(df1), nrow(df1)*.7))
train<-df1[dt,]
test<-df1[-dt,]

# standardize the data
library(standardize)
sd_train <- train
sd_test <- test
my_range <- 2:30
for (i in my_range){
  train_mean = mean(train[,i])
  train_sd = sd(train[,i])
  sd_train[,i]=(train[,i]-train_mean)/train_sd
  sd_test[,i]=(test[,i]-train_mean)/train_sd
    }
```

##### Model summary
The table is sorted by Estimate in descending order. All numbers are round in 4 digits.
```{r, echo=FALSE}
# run logistic regression model
glm=glm(Attrition_Flag ~ .-Attrition_Flag ,
data=sd_train ,family =binomial)
summary(glm)$call
summary<-round(summary(glm)$coef,4)
knitr::kable(
  summary[order(abs(summary[,1]),decreasing = TRUE),], 
  caption = "Coefficient Table for Logistic Regression"
)
```

The Estimate in summary shows the importance of each attributes to the attrition of the bank. From the table, we found that the total number of transactions contributes the most importance to negatively influence the attrition of customers. And it is statistically significant in the 99% confidence level, which verifies our first finding based on the Behavioral Attribute Analysis. And Total_Relationship_Count represents the number of products held by the customer from bank. Therefore, a customer who holds fewer products from the bank is more likely to be attrition. Besides, a married person is less likely to be attrition based on the result of the model. It might be explained that a married person has a stronger ability to keep a healthy cash flow.

In addition, we also found many attributes that have positive influences on customers' churn. Contacts_Count_12_mon shows that if customers contact the bank more frequently, they probably churn because of dissatisfaction. A long inactive time also implies a churn of customers. Besides, a higher number of dependents also give pressure on customers to avoid using credit cards.

##### Validation on train/test datasets
We decided to use plot of ROC Curve and the Area Under the ROC Curve(AUC) score as the standard to judge the logistic regression model's performance fitted in train and test datasets. 

```{r, echo=FALSE,warning=FALSE,message=FALSE}
library(pROC)

#matrix evaluation for train and test
par(mfcol=c(1,2))
train_prob = predict(glm, newdata = sd_train, type = "response")
train_roc = roc(sd_train$Attrition_Flag ~ train_prob, plot = TRUE, print.auc = TRUE, main='ROC Curve for Train')
train_auc <- as.numeric(train_roc$auc)

test_prob = predict(glm, newdata = sd_test, type = "response")
test_roc = roc(sd_test$Attrition_Flag ~ test_prob, plot = TRUE, print.auc = TRUE, main='ROC Curve for Test')
 test_auc <- as.numeric(test_roc$auc)
```
The more curve closes to the left-upper corner, the better the model performs. Besides, the AUC score of the train is `r round(train_auc,4)`, and the AUC score of the test is `r round(test_auc,4)`, which shown that the model fitted two datasets pretty well. And there is overfitting phenomenon in this model.

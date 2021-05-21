### clean categorical data
```{r Categorical Variable Processing, echo=FALSE}
# 1 and 0 in 'Attrition_Flag'
df$Attrition_Flag<- ifelse(df$Attrition_Flag=="Existing Customer",1, 
                      ifelse(df$Attrition_Flag == "Attrited Customer",0,99))
# 1 and 0 in 'Gender'
df$Gender<- ifelse(df$Gender=="M",0, ifelse(df$Gender == "F",1,99))

# one hot encoding for the rest
library(creditmodel)
df1=one_hot_encoding(df,c("Marital_Status","Education_Level","Income_Category","Card_Category"))
```


### drop uncessary variables based on correlation/ split data
```{r Drop Uncessary Variable/Split data, echo=FALSE}
# drop columns below because there are perfectly correlated with others variables in model. There are shown as NA if exist.
df2 <- df1 %>% 
  select(-c(Avg_Open_To_Buy, Marital_Status.Unknown., Education_Level.Unknown., Income_Category.Unknown.,Card_Category.Platinum.))

# split the data into train(70%) and test(30%) randomly.
set.seed(53)
dt = sort(sample(nrow(df2), nrow(df2)*.7))
train<-df2[dt,]
test<-df2[-dt,]
```

### standardlize data
```{r, echo=FALSE}
# standardlize the data
library(standardize)
sd_train <- train
sd_test <- test
my_range <- 2:29

for (i in my_range){
  train_mean = mean(train[,i])
  train_sd = sd(train[,i])
  sd_train[,i]=(train[,i]-train_mean)/train_sd
  sd_test[,i]=(test[,i]-train_mean)/train_sd
    }
```


### run logistic regression model
```{r Run model, echo=FALSE}
# run logistic regression model
glm=glm(Attrition_Flag ~ .-Attrition_Flag ,
data=sd_train ,family =binomial)
summary(glm)$call
summary<-round(summary(glm)$coef,4)
knitr::kable(
  head(summary[order(abs(summary[,1]),decreasing = TRUE),],13), 
  caption = "Coefficient Table for Logistic Regression"
)

```

### matrix evaluation by AUC and plots
```{r Evaluation AUC, echo=FALSE}
library(pROC)

#matrix evaluation for train and test
par(mfcol=c(1,2))
train_prob = predict(glm, newdata = sd_train, type = "response")
train_roc = roc(sd_train$Attrition_Flag ~ train_prob, plot = TRUE, print.auc = TRUE, main='ROC for Train')
as.numeric(train_roc$auc)

test_prob = predict(glm, newdata = sd_test, type = "response")
test_roc = roc(sd_test$Attrition_Flag ~ test_prob, plot = TRUE, print.auc = TRUE, main='ROC for Test ')
as.numeric(test_roc$auc)
```

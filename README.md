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
```{r, echo=FALSE}
# standardlzie the data
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



```{r Run Logistic Regression, echo=FALSE}
# run logistic regression model
glm.fits=glm(Attrition_Flag ~ .-Attrition_Flag ,
data=train ,family =binomial(link='logit') )
summary(glm.fits)$call
round(summary(glm.fits)$coef,4)
```



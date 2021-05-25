### Model-Random Forest
We also used random forest model to predict the result. Although the random forest model has better ability to handle the high-correlated variables, we still choose the same feature selection as logistic regression that we can get a better comparison results from two models. Besides, the standardization is not necessary for random forest model.  
```{r Run randomforest model, echo=FALSE,message=FALSE}
library(randomForest)
forest <- randomForest(as.factor(Attrition_Flag) ~ .,
                            data = train, importance = TRUE)
print(forest)
```


##### Importance of varibles
```{r Plot importance, echo=FALSE}
forest_importance <- importance(forest, type = 1)

knitr::kable(
  forest_importance %>% as_tibble(rownames = "var") %>% arrange(desc(MeanDecreaseAccuracy)), 
  caption = "Importance Table for Random Forest"
)
```
From the importance table, we got a similar result with the logistic regression. The Total_Trans_Ct is still the most influential variable to customer churns. It proves the accuracy of logistic regression from other side.


##### Validation on train/test dataset
In random forest model, we also choose the AUC score as the indicator of model's performance.

```{r AUC for Random Forest, echo=FALSE, message=FALSE}
par(mfcol=c(1,2))
rf_train_prob = predict(forest,train, type='prob')
rf_train_ROC = roc(train$Attrition_Flag,rf_train_prob[,2])
rf_train_auc <- auc(rf_train_ROC)
plot(rf_train_ROC,print.auc = TRUE, main ="ROC Curve for Train")

rf_test_prob = predict(forest,test, type='prob')
rf_test_ROC = roc(test$Attrition_Flag,rf_test_prob[,2])
rf_test_auc <- auc(rf_test_ROC)
plot(rf_test_ROC,print.auc = TRUE, main = "ROC Curve for Test")
```
From figures we found that the AUC of train is up to `r round(rf_train_auc,4)` and the AUC of test is `r round(rf_test_auc,4)`, which means the random forest model performance is even better than the result of logistic regression model.


##### Partial dependence
At last, there are some figures for partial dependence.

```{r partial dependence plot, echo=FALSE}
par(mfrow=c(2,2))
partialPlot(forest, train, Total_Trans_Ct, 1)
partialPlot(forest, train, Total_Relationship_Count, 1)
partialPlot(forest, train, Contacts_Count_12_mon, 1)
partialPlot(forest, train, Months_Inactive_12_mon, 1)
```

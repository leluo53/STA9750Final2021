data <- read_csv("C:/Users/luole/Desktop/R final project/BankChurners.csv")
data <- data[ -c(22,23) ]

data$Attrition_Flag<- ifelse(data$Attrition_Flag=="Existing Customer",1, 
                      ifelse(data$Attrition_Flag == "Attrited Customer",0,99))

data$Gender<- ifelse(data$Gender=="M",0, ifelse(data$Gender == "F",1,99))

View(data)

data1=one_hot_encoding(data,c("Marital_Status","Education_Level","Income_Category","Card_Category"))
View(data1)

write.csv(data1,"C:/Users/luole/Desktop/R final project/revised_data.csv", row.names = FALSE)

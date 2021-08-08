
library(naniar)
library(dplyr)
library(ggplot2)
library(ggcorrplot)
library(gridExtra)
library(grid)
library(ggthemes)
library(sqldf)
library(plotly)
library(glmnet)
library(caret)
library(randomForest)
library(dummies)
library(class)
library(caret)
library(e1071)
library(ROCR)
library(klaR)
library(glmnet)

data_cs02 = read.csv("/Users/kalenasberry/Desktop/Test website/Simple website/CaseStudy2-data.csv")
data_cs02

str(str(data_cs02))


data_cs02$Education                <- as.factor(data_cs02$Education)
data_cs02$EnvironmentSatisfaction  <- as.factor(data_cs02$EnvironmentSatisfaction)
data_cs02$JobInvolvement           <- as.factor(data_cs02$JobInvolvement)      
data_cs02$JobLevel                 <- as.factor(data_cs02$JobLevel)
data_cs02$JobSatisfaction          <- as.factor(data_cs02$JobSatisfaction) 
data_cs02$PerformanceRating        <- as.factor(data_cs02$PerformanceRating)
data_cs02$RelationshipSatisfaction <- as.factor(data_cs02$RelationshipSatisfaction)
data_cs02$StockOptionLevel         <- as.factor(data_cs02$StockOptionLevel) 
data_cs02$WorkLifeBalance          <- as.factor(data_cs02$WorkLifeBalance)


non_predictors <- c('ID','EmployeeNumber','EmployeeCount','StandardHours','Over18')

nom_qual_predictors <- c('BusinessTravel','Department','EducationField',
                         'Gender','JobRole','MaritalStatus','OverTime')

ord_qual_predictors <- c('Education','EnvironmentSatisfaction','JobInvolvement','JobLevel',
                         'JobSatisfaction','PerformanceRating','RelationshipSatisfaction',
                         'StockOptionLevel','WorkLifeBalance')

num_predictors <- c('Age','DailyRate','DistanceFromHome','HourlyRate',
                    'MonthlyRate','NumCompaniesWorked','PercentSalaryHike','TotalWorkingYears',
                    'TrainingTimesLastYear','YearsAtCompany','YearsInCurrentRole',
                    'YearsSinceLastPromotion','YearsWithCurrManager') 

num_predictors


df <- sqldf("select Department,count(*) employee_count from data_cs02 group by  Department")
df
p  <- plot_ly(df, labels = ~Department, values = ~employee_count, type = 'pie') %>%
  layout(title = 'Number of employees per Department',
         xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))

p



age_Attrition     <- data_cs02 %>% filter(Attrition=='Yes') %>% select(Age)
age_non_attrition <- data_cs02 %>% filter(Attrition=='No') %>% select(Age)

p <- plot_ly(alpha = 0.6) %>%
  add_histogram(x = ~age_non_attrition$Age+1) %>%
  add_histogram(x = ~age_Attrition$Age) %>%
  layout(title = 'Histogram',
         barmode = "overlay",showlegend = F,xaxis = list(title = "Attrition(Yellow) and No Attrition (Blue)"))

p


mean(data_cs02$Age)
median(data_cs02$Age)
mean(age_Attrition$Age)
median(age_Attrition$Age)




df <- sqldf("select Attrition,count(*) Attrition_count from data_cs02 group by  Attrition")
df
p  <- plot_ly(df, labels = ~Attrition, values = ~Attrition_count, type = 'pie') %>%
  layout(title = 'Attrition Rate',
         xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
p


spineplot(Attrition~ Age, data = data_cs02)


df_RandD <- sqldf("select Attrition,count(*) Attrition_count from data_cs02  where Department like 'Research%' group by  Attrition")
df_RandD
df_Sales <- sqldf("select Attrition,count(*) Attrition_count from data_cs02  where Department like 'Sales%' group by  Attrition")
df_Sales
df_hr    <- sqldf("select Attrition,count(*) Attrition_count from data_cs02  where Department like 'Human%' group by  Attrition")
df_hr
p <- plot_ly() %>%
  add_pie(data = df_RandD ,labels = ~Attrition, values = ~Attrition_count,
          title = "Research & Development", domain = list(x = c(0, 0.4), y = c(0.4, 1))) %>%
  add_pie(data = df_Sales, labels = ~Attrition, values = ~Attrition_count,
          title = "Sales", domain = list(x = c(0.6, 1), y = c(0.4, 1))) %>%
  add_pie(data = df_hr, labels = ~Attrition, values = ~Attrition_count,
          title = "Human Resource", domain = list(x = c(0.25, 0.75), y = c(0, 0.6))) %>%
  layout(title = "Departmentwise Attrition", showlegend = T,
         xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))

p


df <- sqldf("select Department,JobRole,count(*) Attrition_count from data_cs02  where Attrition='Yes' group by  Department,JobRole order by Attrition_count desc")
p <- ggplotly(ggplot(data=df, aes(x=Department, y=Attrition_count, fill=JobRole)) +
                geom_bar(stat="identity", position=position_dodge())+
                geom_text(aes(label=Attrition_count),vjust = 2.0,position=position_dodge(width=0.7))+
                theme_bw()+
                theme(axis.text.x = element_text(angle=35, vjust=0.7)))
p


## JobRole only
df <- sqldf("select JobRole,count(*) Attrition_count from data_cs02  where Attrition='Yes' group by  JobRole order by Attrition_count desc")
p <- df %>%
  plot_ly(labels = ~JobRole, values = ~Attrition_count) %>%
  add_pie(hole = 0.6) %>%
  layout(title = "Attrition by Job Role",  showlegend = T,
         xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
p







###Gender by dept
df_RandD <- sqldf("select Gender,count(*) Attrition_count from data_cs02  where Attrition='Yes' and Department like 'Research%' group by  Gender")
df_RandD
df_Sales <- sqldf("select Gender,count(*) Attrition_count from data_cs02  where Attrition='Yes' and Department like 'Sales%' group by  Gender")
df_Sales
df_hr    <- sqldf("select Gender,count(*) Attrition_count from data_cs02  where Attrition='Yes' and Department like 'Human%' group by  Gender")
df_hr
p <- plot_ly() %>%
  add_pie(data = df_RandD ,labels = ~Gender, values = ~Attrition_count,
          title = "Research & Development", domain = list(x = c(0, 0.4), y = c(0.4, 1))) %>%
  add_pie(data = df_Sales, labels = ~Gender, values = ~Attrition_count,
          title = "Sales", domain = list(x = c(0.6, 1), y = c(0.4, 1))) %>%
  add_pie(data = df_hr, labels = ~Gender, values = ~Attrition_count,
          title = "Human Resource", domain = list(x = c(0.25, 0.75), y = c(0, 0.6))) %>%
  layout(title = "Departmentwise Attrition", showlegend = T,
         xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))

p

### Attrition by Satisfaction Level
df <- data_cs02 %>% filter(Attrition=='Yes')  %>% count(JobSatisfaction)
p  <- plot_ly(df, labels = ~JobSatisfaction, values = ~n, type = 'pie') %>%
  layout(title = 'Attrition by job satisfaction level',
         xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
p




### Top 3 
df_travel  <- sqldf("select BusinessTravel,count(*) Attrition_count from data_cs02  where Attrition='Yes' group by BusinessTravel  ")
df_joblevel <- sqldf("select JobLevel,count(*) Attrition_count from data_cs02  where Attrition='Yes' group by JobLevel")rownames(df_joblevel) <- c('JobLevel1','JobLevel2','JobLevel3')
df_stocklevel <- sqldf("select StockOptionLevel,count(*) Attrition_count from data_cs02  where Attrition='Yes' group by StockOptionLevel")
#rownames(df_stocklevel) <- c('Stock0','Stock1','Stock2')
df_overtime <- sqldf("select Overtime,count(*) Attrition_count from data_cs02  where Attrition='Yes' group by Overtime")
p1 <- plot_ly() %>%
  add_pie(data = df_travel ,labels = ~BusinessTravel, values = ~Attrition_count,
          title = "BusinessTravel", domain = list(row = 0, column = 0)) %>%
  add_pie(data = df_joblevel, labels = ~rownames(df_joblevel), values = ~Attrition_count,
          title = "JobLevel", domain = list(row = 0, column = 1)) %>%
  add_pie(data = df_overtime ,labels = ~OverTime, values = ~Attrition_count,
          title = "Overtime", domain = list(row = 1, column = 0)) %>%
  add_pie(data = df_stocklevel, labels = ~rownames(df_stocklevel), values = ~Attrition_count,
          title = "StockOptionLevel", domain = list(row = 1, column = 1)) %>%
  layout(title = "Attrition Rate", showlegend = T,
         grid=list(rows=2, columns=2),
         xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
p1








## Ordinal Qualtitative variables
i=1 
while(i <= length(ord_qual_predictors)) {
  index <- match(ord_qual_predictors[i], names(data_cs02))
  hypothesis_test <- fisher.test(table(data_cs02[,c(index,3)]),simulate.p.value=TRUE)
  print(paste("Feature ",ord_qual_predictors[i]," p-value ",round(hypothesis_test$p.value,2)))
  i <- i + 1
}

## Nominal Qualtitative variables
i=1 
while(i <=length(nom_qual_predictors)) {
  index <- match(nom_qual_predictors[i], names(data_cs02))
  hypothesis_test <- fisher.test(table(data_cs02[,c(index,3)])[,c(2,1)],simulate.p.value=TRUE)
  print(paste("Feature ",nom_qual_predictors[i]," p-value ",round(hypothesis_test$p.value,2)))
  i <- i + 1
}




## Classification Problem

### Random Forest



# Fit the model on the training set
set.seed(123)
model <- train(
  Attrition ~., data = train.data, method = "rf",
  trControl = trainControl("cv", number = 10,sampling="down"),
  preProcess = c("center","scale"),
  #  weights = model_weights,
  importance = TRUE
)
# Best tuning parameter
model$bestTune
model$finalModel
predicted.classes <- model %>% predict(test.data, type = 'raw')
head(predicted.classes)
mean(predicted.classes == test.data$Attrition)
predicted.classes <- relevel(predicted.classes,ref="Yes")

##CM <- confusionMatrix(table(predicted.classes, test.data$Attrition))

caret::varImp(model)
ggplot(caret::varImp(model)) + 
  geom_bar(stat = 'identity', fill = 'steelblue', color = 'black') + 
  ylab("Feature Importance - Random Forest ")+
  scale_y_continuous(limits = c(0, 105), expand = c(0, 0)) +
  theme_light()


#
# This library required for one hot encoding to convert categorical variables into numeric
#

#

#res.pca <- PCA(data_cs02[,fin_num_predictors], graph = FALSE,ncp=20)
#res.pca$eig
#corrplot(res.pca$var$cos2, is.corr=FALSE)
#fviz_contrib(res.pca, choice = "var", axes = 1:7)
# Fit the model on the training set

dataEmpA = data_cs02 %>% filter(Department == "Research & Development" | Department == "Sales")
dataEmpA

data_cs02

library(class)
library(caret)
library(e1071)
splitPerc = .69

trainEmployees = sample(1:dim(dataEmpA)[1],round(splitPerc * dim(dataEmpA)[1]))
train = dataEmpA[trainEmployees,]
test = dataEmpA[-trainEmployees,]

classifications = knn(train[,c(1,2)],test[,c(1,2)],train$Department, prob = TRUE, k = 3)
table(classifications,test$Department)
confusionMatrix(table(classifications,test$Department))


# Plot model accuracy vs different values of k

# Print the best tuning parameter k that
# maximizes model accuracy



### Naive Bayes Classifier

install.packages("klaR")

#
# Create separate dataframe of 30 predictors 1 target
#
# Inspect the data
sample_n(nb_with_target_ds, 3)
# Split the data into training and test set
set.seed(123)
training.samples <- nb_hot_encoded_ds$Attrition %>% createDataPartition(p = 0.8, list = FALSE)
train.data  <- nb_hot_encoded_ds[training.samples, ]
test.data   <- nb_hot_encoded_ds[-training.samples, ]
# Fit the model
#model <- NaiveBayes(Attrition ~., data = train.data)
#model
# Make predictions
#predicted.classes <- model %>% predict(test.data)
#pred <- prediction(pred_nb[, 2], test_color$Style)
# Model accuracy
#mean(predicted.classes$class == test.data$Attrition)
set.seed(123)
model <- train(Attrition ~., data = train.data, method = "nb", 
               trControl = trainControl("cv", number = 10,sampling="down")
)
# Make predictions
predicted.classes <- model %>% predict(test.data)
predicted.classes   <- relevel(predicted.classes,ref="Yes")
test.data$Attrition <- relevel(test.data$Attrition,ref="Yes")
# Compute model accuracy rate
mean(predicted.classes == test.data$Attrition)
CM <- confusionMatrix(table(predicted.classes, test.data$Attrition))
CM
caret::varImp(model)
ggplot(caret::varImp(model)) + 
  geom_bar(stat = 'identity', fill = 'steelblue', color = 'black') + 
  ylab("Feature Importance -Naive Bayes ")+
  scale_y_continuous(limits = c(0, 105), expand = c(0, 0)) +
  theme_light()




## Regression Problem

### Knn Regression Model



#
# Create separate dataframe of 30 predictors 1 target
#
# Split the data into training and test set
set.seed(123)
training.samples <- kg_hot_encoded_ds$MonthlyIncome %>% createDataPartition(p = 0.6, list = FALSE)
train.data  <- kg_hot_encoded_ds[training.samples, ]
test.data   <- kg_hot_encoded_ds[-training.samples, ]
# Fit the model on the training set
set.seed(123)
model_rg <- train( MonthlyIncome~., data = train.data, method = "knn",
                   trControl = trainControl("cv", number = 20),
                   preProcess = c("center","scale"),
                   tuneLength = 20
)
# Plot model error RMSE vs different values of k
# Best tuning parameter k that minimize the RMSE
model$bestTune
# Make predictions on the test data
predictions <- model_rg %>% predict(test.data)
head(predictions)
# Compute the prediction error RMSE
RMSE(predictions, test.data$MonthlyIncome)
plot(predictions, test.data$MonthlyIncome)
# Model performance metrics
data.frame(
  RMSE = RMSE(predictions, test.data$MonthlyIncome),
  Rsquare = R2(predictions, test.data$MonthlyIncome)
)
p <- plot_ly( x = ~predictions, y = ~test.data$MonthlyIncome,
              marker = list(size = 10,
                            color = 'rgba(255, 182, 193, .9)',
                            line = list(color = 'rgba(152, 0, 0, .8)',
                                        width = 2))) %>%
  layout(title = 'RMSE Plot (kNN)',
         yaxis = list(zeroline = FALSE),
         xaxis = list(zeroline = FALSE))
p



#
# Create separate dataframe of 30 predictors 1 target
#
mlp_reg_target_ds <- data_cs02[,c(fin_num_predictors,nom_qual_predictors,fin_ord_qual_predictors, "MonthlyIncome")] 
#mlp_hot_encoded_ds <- dummy.data.frame(mlp_reg_target_ds,names=c(fin_nom_qual_predictors,fin_ord_qual_predictors))
# Split the data into training and test set
set.seed(123)
training.samples <- mlp_reg_target_ds$Attrition %>% createDataPartition(p = 0.6, list = FALSE)
train.data  <- mlp_reg_target_ds[training.samples, ]
test.data <- mlp_reg_target_ds[-training.samples, ]
# Predictor variables
x <- model.matrix(MonthlyIncome~., train.data)[,-1]
# Outcome variable
y <- train.data$MonthlyIncome
# Find the best lambda using cross-validation
set.seed(123) 
cv <- cv.glmnet(x, y, alpha = 1)
# Display the best lambda value
cv$lambda.min
# Fit the final model on the training data
model <- glmnet(x, y, alpha = 1, lambda = cv$lambda.min)
# Dsiplay regression coefficients
coef(model)
summary(model$beta)
# Make predictions on the test data
x.test <- model.matrix(MonthlyIncome ~., test.data)[,-1]
predictions <- model %>% predict(x.test) %>% as.vector()
# Model performance metrics
data.frame(
  RMSE = RMSE(predictions, test.data$MonthlyIncome),
  Rsquare = R2(predictions, test.data$MonthlyIncome)
)
p <- plot_ly( x = ~predictions, y = ~test.data$MonthlyIncome,
              marker = list(size = 10,
                            color = 'rgba(255, 182, 193, .9)',
                            line = list(color = 'rgba(152, 0, 0, .8)',
                                        width = 2))) %>%
  layout(title = 'RMSE Plot(LASSO)',
         yaxis = list(zeroline = FALSE),
         xaxis = list(zeroline = FALSE))
p
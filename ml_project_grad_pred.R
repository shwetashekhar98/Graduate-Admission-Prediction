grad=read.csv("C:/Users/Sri Satya Sai/Documents/R/Admission_Predict_Ver1.1.csv",header = T)
View(grad)
str(grad)
dim(grad)
# library(dplyr)
# library(magrittr)
# grad <- grad %>%
#   select(-c(Serial.No.))
# glimpse(grad)
cor(grad[c("GRE.Score", "TOEFL.Score", "University.Rating", "SOP", "LOR", "CGPA", "Research", "Chance.of.Admit")])
ins_model <- lm(Chance.of.Admit ~.-(Serial.No.),data = grad)
summary(ins_model)

ins_model2 <- lm(Chance.of.Admit ~.-(Serial.No.+University.Rating+SOP),data = grad)
summary(ins_model2)

set.seed(50)
trainingRowIndex= sample(1: nrow(grad),0.7*nrow(grad))
TrainData= grad[trainingRowIndex,]
TestData= grad[-trainingRowIndex,]

exp_pred= predict(ins_model2,TestData)
act_pred_exp= data.frame(cbind(actual=TestData$Chance.of.Admit,predicted=exp_pred))
head(act_pred_exp)
cor(TestData$Chance.of.Admit,exp_pred)

AIC(ins_model)
AIC(ins_model2)

#### Min Max Accuracy
min_max_accuracy <- mean(apply(act_pred_exp, 1, min) / apply(act_pred_exp, 1, max))  
min_max_accuracy
#Mean Absolute Percenatage Error
mape <- mean(abs((act_pred_exp$predicted - act_pred_exp$actual))
             /act_pred_exp$actual)  
mape 

library(psych)
pairs.panels(grad[c("GRE.Score", "TOEFL.Score", "LOR", "CGPA", "Research", "Chance.of.Admit")])

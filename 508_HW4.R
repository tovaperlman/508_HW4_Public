options(scipen=10000000)

library(tidyverse)
library(kableExtra)
library(caret)
library(knitr) 
library(pscl)
library(plotROC)
library(pROC)
library(lubridate)

Housing <- read.csv("~/GitHub/Public-Policy-Analytics-Landing/DATA/Chapter6/housingSubsidy.csv")

palette5 <- c("#d01c8b","#f1b6da","#b8e186","#4dac26","#f7f7f7")
palette4 <- c("#d01c8b","#f1b6da","#b8e186","#4dac26")
palette2 <- c("#a1d76a","#e9a3c9")


#Age
Housing %>%
  dplyr::select(y,age) %>%
  gather(Variable, value, -y) %>%
  ggplot(aes(y, value, fill=y)) + 
  geom_bar(position = "dodge", stat = "summary", fun.y = "mean") + 
  facet_wrap(~Variable, scales = "free") +
  scale_fill_manual(values = palette2) +
  labs(x="Churn", y="Value", 
       title = "Feature associations with the likelihood of click",
       subtitle = "(continous outcomes)") +
  theme(legend.position = "none")

Housing %>%
  dplyr::select(y,unemploy_rate , inflation_rate, spent_on_repairs) %>%
  gather(Variable, value, -y) %>%
  ggplot(aes(y, value, fill=y)) + 
  geom_bar(position = "dodge", stat = "summary", fun.y = "mean") + 
  facet_wrap(~Variable, scales = "free") +
  scale_fill_manual(values = palette2) +
  labs(x="Churn", y="Value", 
       title = "Feature associations with the likelihood of click",
       subtitle = "(continous outcomes)") +
  theme(legend.position = )

Housing %>%
  dplyr::select(y,unemploy_rate , inflation_rate, spent_on_repairs) %>%
  gather(Variable, value, -y) %>%
  ggplot() + 
  geom_density(aes(value, color=y), fill = "transparent") + 
  facet_wrap(~Variable, scales = "free") +
  scale_fill_manual(values = palette2) +
  labs(title = "Feature distributions click vs. no click",
       subtitle = "(continous outcomes)") +
  theme(legend.position = )

#Mortgage and Marital
Housing %>%
  dplyr::select(y, mortgage, marital) %>%
  gather(Variable, value, -y) %>%
  count(Variable, value, y) %>%
  ggplot(., aes(value, n, fill = y)) +   
  geom_bar(position = "dodge", stat="identity") +
  facet_wrap(~Variable, scales="free") +
  scale_fill_manual(values = palette2) +
  labs(x="Click", y="Value",
       title = "Feature associations with the likelihood of voucher",
       subtitle = "Categorical features") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#Education
Housing %>%
  dplyr::select(y, education) %>%
  gather(Variable, value, -y) %>%
  count(Variable, value, y) %>%
  ggplot(., aes(value, n, fill = y)) +   
  geom_bar(position = "dodge", stat="identity") +
  facet_wrap(~Variable, scales="free") +
  scale_fill_manual(values = palette2) +
  labs(x="Click", y="Value",
       title = "Feature associations with the likelihood of voucher",
       subtitle = "Categorical features") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#Job
Housing %>%
  dplyr::select(y, job) %>%
  gather(Variable, value, -y) %>%
  count(Variable, value, y) %>%
  ggplot(., aes(value, n, fill = y)) +   
  geom_bar(position = "dodge", stat="identity") +
  facet_wrap(~Variable, scales="free") +
  scale_fill_manual(values = palette2) +
  labs(x="Click", y="Value",
       title = "Feature associations with the likelihood of voucher",
       subtitle = "Categorical features") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#Day of Week
Housing %>%
  dplyr::select(y, day_of_week) %>%
  gather(Variable, value, -y) %>%
  count(Variable, value, y) %>%
  ggplot(., aes(value, n, fill = y)) +   
  geom_bar(position = "dodge", stat="identity") +
  facet_wrap(~Variable, scales="free") +
  scale_fill_manual(values = palette2) +
  labs(x="Credit", y="Value",
       title = "Feature associations with the likelihood of voucher",
       subtitle = "Categorical features") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

Housing %>%
  dplyr::select(y, taxLien, mortgage, taxbill_in_phl) %>%
  gather(Variable, value, -y) %>%
  count(Variable, value, y) %>%
  filter(value == "yes") %>%
  ggplot(aes(y, n, fill = y)) +   
  geom_bar(position = "dodge", stat="identity") +
  facet_wrap(~Variable, scales = "free", ncol = 3) +
  scale_fill_manual(values = palette2) +
  labs(x="Credit", y="Count",
       title = "Feature associations with the likelihood of churn",
       subtitle = "Two category features (Yes and No)") +
  theme(legend.position = )
#contact for telephone phones
Housing %>%
  dplyr::select(y, contact) %>%
  gather(Variable, value, -y) %>%
  count(Variable, value, y) %>%
  filter(value == "telephone") %>%
  ggplot(aes(y, n, fill = y)) +   
  geom_bar(position = "dodge", stat="identity") +
  facet_wrap(~Variable, scales = "free", ncol = 3) +
  scale_fill_manual(values = palette2) +
  labs(x="Credit", y="Count",
       title = "Feature associations with the likelihood of tax credit",
       subtitle = "Fig 2.7: Two category features (Yes and No)") +
  theme(legend.title= element_text("Took Housing Credit")) , legend.position = "none")
#not enough data on taxlien


#feature engineering

#likelihood of accepting based on day contacted
Housing <- 
  Housing %>% 
  group_by(day_of_week) %>% 
  summarize(totSubsidies = sum(y_numeric), 
            n = n(), 
            DaySubsidyAvg = 100*(totSubsidies/n)) %>%
  dplyr::select(-n, -totSubsidies) %>%
  right_join(Housing, .) 

#job types
Housing <-
  Housing %>%
  mutate(middleJobs = ifelse(str_detect(Housing$job, "admin.|blue-collar|technician"), "Yes", "No"))

#out of workforce
Housing <-
  Housing %>%
  mutate(noWork = ifelse(str_detect(Housing$job, "retired|student|unemployed"), "Yes", "No"))

#lower ed
Housing <-
  Housing %>%
  mutate(lowerEd = ifelse(str_detect(Housing$education, "basic.4y|basic.6y|basic.9y|high.school"), "Yes", "No"))

#higher ed
Housing <-
  Housing %>%
  mutate(higherEd = ifelse(str_detect(Housing$education, "professional.course|university.degree"), "Yes", "No"))

#contact cellular
Housing <-
  Housing %>%
  mutate(cellular = ifelse(str_detect(Housing$contact, "cellular"), "Yes", "No"))

#married
Housing <-
  Housing %>%
  mutate(married = ifelse(str_detect(Housing$marital, "married"), "Yes", "No"))

#spent on repairs
#Housing <-
#Housing %>%
#mutate(repairsmoney = case_when(
#spent_on_repairs < 5100  ~ "Less than 5100",
#spent_on_repairs >= 5100 & spent_on_repairs <= 5200  ~ "Between 5100+5200",
#spent_on_repairs > 5200  ~ "Over 5200" ))

#Data Training
set.seed(3456)
trainIndex <- createDataPartition(y=paste(Housing$marital), p = .65,
                                  list = FALSE,
                                  times = 1)
HousingTrain <- Housing[ trainIndex,]
HousingTest  <- Housing[-trainIndex,]

#Creating the model
KitchenSink <- glm(y_numeric ~ .,
                   data=HousingTrain %>% 
                     dplyr::select(-y, -noWork, -married, -cellular, -middleJobs, -DaySubsidyAvg, -lowerEd, -higherEd),
                   family="binomial" (link="logit"))

summary(KitchenSink)

EngineeredModel <- glm(y_numeric ~ .,
                       data=HousingTrain %>% 
                         dplyr::select(-y, -job, -marital, -age, -campaign, -day_of_week, -month, -previous, -pdays, -inflation_rate, -taxLien, -education, -unemploy_rate, -poutcome, -contact, -cons.price.idx, -cons.conf.idx),
                       family="binomial" (link="logit"))

summary(EngineeredModel)

#goodness of fit
pR2(KitchenSink)
pR2(EngineeredModel)

#kitchen sink predictions
testprobs.kitchen <- data.frame(Outcome = as.factor(HousingTest$y_numeric),
                                Probs = predict(KitchenSink, HousingTest, type= "response"))
head(testprobs.kitchen)

ggplot(testprobs.kitchen, aes(x = Probs, fill = as.factor(Outcome))) + 
  geom_density() +
  facet_grid(Outcome ~ .) +
  scale_fill_manual(values = palette2) + xlim(0, 1) +
  labs(x = "y", y = "Density of probabilities",
       title = "Distribution of predicted probabilities by observed outcome") +
  theme(strip.text.x = element_text(size = 18),
        legend.position = "none")

#engineered predictions
testprobs.engineered <- data.frame(Outcome = as.factor(HousingTest$y_numeric),
                                   Probs = predict(EngineeredModel, HousingTest, type= "response"))
head(testprobs.engineered)

ggplot(testprobs.engineered, aes(x = Probs, fill = as.factor(Outcome))) + 
  geom_density() +
  facet_grid(Outcome ~ .) +
  scale_fill_manual(values = palette2) + xlim(0, 1) +
  labs(x = "y", y = "Density of probabilities",
       title = "Distribution of predicted probabilities by observed outcome") +
  theme(strip.text.x = element_text(size = 18),
        legend.position = "none")

#kitchen sink confusion matrix
testprobs.kitchen <- 
  testprobs.kitchen %>%
  mutate(predOutcome  = as.factor(ifelse(testprobs.kitchen$Probs > 0.5 , 1, 0)))

head(testprobs.kitchen)

caret::confusionMatrix(testprobs.kitchen$predOutcome,testprobs.kitchen$Outcome, 
                       positive = "1")

#engineered confusion matrix
testprobs.engineered <- 
  testprobs.engineered %>%
  mutate(predOutcome  = as.factor(ifelse(testprobs.engineered$Probs > 0.5 , 1, 0)))

head(testprobs.engineered)

caret::confusionMatrix(testprobs.engineered$predOutcome,testprobs.engineered$Outcome, 
                       positive = "1")

#kitchen sink ROC curve
ggplot(testprobs.kitchen, aes(d = as.numeric(testprobs.kitchen$Outcome), m = Probs)) +
  geom_roc(n.cuts = 50, labels = FALSE, colour = "#FE9900") +
  style_roc(theme = theme_grey) +
  geom_abline(slope = 1, intercept = 0, size = 1.5, color = 'grey') +
  labs(title = "ROC Curve - Kitchen Sink Model")

#engineered ROC curve
ggplot(testprobs.engineered, aes(d = as.numeric(testprobs.engineered$Outcome), m = Probs)) +
  geom_roc(n.cuts = 50, labels = FALSE, colour = "#FE9900") +
  style_roc(theme = theme_grey) +
  geom_abline(slope = 1, intercept = 0, size = 1.5, color = 'grey') +
  labs(title = "ROC Curve - Engineered Model")

#kitchen sink area under the curve
pROC::auc(testprobs.kitchen$Outcome, testprobs.kitchen$Probs)

#engineered area under the curve
pROC::auc(testprobs.engineered$Outcome, testprobs.engineered$Probs)

#kitchen sink cv
ctrl <- trainControl(method = "cv", number = 100, classProbs=TRUE, summaryFunction=twoClassSummary)

cvKitchenFit <- train(y ~ ., data = Housing %>% 
                        dplyr::select(
                          -y_numeric, -noWork, -married, -cellular, -middleJobs, -DaySubsidyAvg, -lowerEd, -higherEd)%>%  
                        dplyr::mutate(y = ifelse(y=="yes","c1.yes","c2.no")), 
                      method="glm", family="binomial",
                      metric="ROC", trControl = ctrl)

cvKitchenFit

#engineered cv
cvEngineeredFit <- train(y ~ ., data = Housing %>% 
                           dplyr::select(
                             -y_numeric, -job, -marital, -age, -campaign, -day_of_week, -month, -previous, -pdays, -inflation_rate, -taxLien, -education, -unemploy_rate, -poutcome, -contact, -cons.price.idx, -cons.conf.idx)%>%
                           dplyr::mutate(y = ifelse(y=="yes","c1.yes","c2.no")), 
                         method="glm", family="binomial",
                         metric="ROC", trControl = ctrl)

cvEngineeredFit

#kitchen goodness of fit
kitchensinkgf <- dplyr::select(cvKitchenFit$resample, -Resample) %>%
  gather(metric, value) %>%
  left_join(gather(cvKitchenFit$results[2:4], metric, mean)) %>%
  ggplot(aes(value)) + 
  geom_histogram(bins=35, fill = "#FF006A") +
  facet_wrap(~metric) +
  geom_vline(aes(xintercept = mean), colour = "#981FAC", linetype = 3, size = 1.5) +
  scale_x_continuous(limits = c(0, 1)) +
  labs(x="Goodness of Fit", y="Count", title="CV Goodness of Fit Metrics",
       subtitle = "Across-fold mean reprented as dotted lines") 

#engineered goodness of fit
engineeredgf <- dplyr::select(cvEngineeredFit$resample, -Resample) %>%
  gather(metric, value) %>%
  left_join(gather(cvEngineeredFit$results[2:4], metric, mean)) %>%
  ggplot(aes(value)) + 
  geom_histogram(bins=35, fill = "#FF006A") +
  facet_wrap(~metric) +
  geom_vline(aes(xintercept = mean), colour = "#981FAC", linetype = 3, size = 1.5) +
  scale_x_continuous(limits = c(0, 1)) +
  labs(x="Goodness of Fit", y="Count", title="CV Goodness of Fit Metrics",
       subtitle = "Across-fold mean reprented as dotted lines") 

cv.summary <- 
  cbind(kitchensinkgf, engineeredgf)


#FacetWrap
#kitchensinkgf %>% ggplot(data=kitchensinkgf, aes(value)) + 
  ggplot(data=cv.summary, aes(value)) + 
  geom_histogram(bins=35, fill = "#FF006A") +
  facet_wrap(~metric) +
  geom_vline(aes(xintercept = mean), colour = "#981FAC", linetype = 3, size = 1.5) +
  scale_x_continuous(limits = c(0, 1)) +
  labs(x="Goodness of Fit", y="Count", title="CV Goodness of Fit Metrics",
       subtitle = "Across-fold mean reprented as dotted lines") 

#Cost Benefit Analysis
cost_benefit_table <-
  testprobs.engineered %>%
  count(predOutcome, Outcome) %>%
  summarize(True_Negative = sum(n[predOutcome==0 & Outcome==0]),
            True_Positive = sum(n[predOutcome==1 & Outcome==1]),
            False_Negative = sum(n[predOutcome==0 & Outcome==1]),
            False_Positive = sum(n[predOutcome==1 & Outcome==0])) %>%
  gather(Variable, Count) %>%
  mutate(Benefit =
           case_when(Variable == "True_Negative"  ~ Count * 0,
                     Variable == "True_Positive"  ~ ((-5000+10000+56000) * (Count * .25)) + 
                       (-2850 * (Count)),
                     Variable == "False_Negative" ~ (-0) * Count,
                     Variable == "False_Positive" ~ (-2850) * Count)) %>%
  bind_cols(data.frame(Description = c(
    "We predicted homeowner would not take credit, no resources allocated, they did not take credit",
    "We predicted correctly homeowner would take credit, allocated marketing resources, and 25% took the credit",
    "We predicted homeowner would not take the credit, did not market allocate resources, they took the credit",
    "We predicted homeowner would take credit, allocated marketing resources, they did not take credit")))

kable(cost_benefit_table) %>% 
  kable_styling(font_size = 12, full_width = F,
                bootstrap_options = c("striped", "hover", "condensed")) %>%
  footnote(general_title = "\n",
           general = "Table 6.1")


#Optimizing for thresholds
iterateThresholds <- function(data, observedClass, predictedProbs, group) {
  #This function takes as its inputs, a data frame with an observed binomial class (1 or 0); a vector of predicted probabilities; and optionally a group indicator like race. It returns accuracy plus counts and rates of confusion matrix outcomes. It's a bit verbose because of the if (missing(group)). I don't know another way to make an optional parameter.
  observedClass <- enquo(observedClass)
  predictedProbs <- enquo(predictedProbs)
  group <- enquo(group)
  x = .01
  all_prediction <- data.frame()
  
  if (missing(group)) {
    
    while (x <= 1) {
      this_prediction <- data.frame()
      
      this_prediction <-
        data %>%
        mutate(predclass = ifelse(!!predictedProbs > x, 1,0)) %>%
        count(predclass, !!observedClass) %>%
        summarize(Count_TN = sum(n[predclass==0 & !!observedClass==0]),
                  Count_TP = sum(n[predclass==1 & !!observedClass==1]),
                  Count_FN = sum(n[predclass==0 & !!observedClass==1]),
                  Count_FP = sum(n[predclass==1 & !!observedClass==0]),
                  Rate_TP = Count_TP / (Count_TP + Count_FN),
                  Rate_FP = Count_FP / (Count_FP + Count_TN),
                  Rate_FN = Count_FN / (Count_FN + Count_TP),
                  Rate_TN = Count_TN / (Count_TN + Count_FP),
                  Accuracy = (Count_TP + Count_TN) / 
                    (Count_TP + Count_TN + Count_FN + Count_FP)) %>%
        mutate(Threshold = round(x,2))
      
      all_prediction <- rbind(all_prediction,this_prediction)
      x <- x + .01
    }
    return(all_prediction)
  }
  else if (!missing(group)) { 
    while (x <= 1) {
      this_prediction <- data.frame()
      
      this_prediction <-
        data %>%
        mutate(predclass = ifelse(!!predictedProbs > x, 1,0)) %>%
        group_by(!!group) %>%
        count(predclass, !!observedClass) %>%
        summarize(Count_TN = sum(n[predclass==0 & !!observedClass==0]),
                  Count_TP = sum(n[predclass==1 & !!observedClass==1]),
                  Count_FN = sum(n[predclass==0 & !!observedClass==1]),
                  Count_FP = sum(n[predclass==1 & !!observedClass==0]),
                  Rate_TP = Count_TP / (Count_TP + Count_FN),
                  Rate_FP = Count_FP / (Count_FP + Count_TN),
                  Rate_FN = Count_FN / (Count_FN + Count_TP),
                  Rate_TN = Count_TN / (Count_TN + Count_FP),
                  Accuracy = (Count_TP + Count_TN) / 
                    (Count_TP + Count_TN + Count_FN + Count_FP)) %>%
        mutate(Threshold = round(x,2))
      
      all_prediction <- rbind(all_prediction,this_prediction)
      x <- x + .01
    }
    return(all_prediction)
  }
}

#iterate thresholds run below
whichThreshold <- 
  iterateThresholds(
    data=testprobs.engineered, observedClass = Outcome, predictedProbs = Probs)

whichThreshold[1:5,]

#Result moved to long form and benefit calculated for each confusion metric at each threshold
whichThreshold <- 
  whichThreshold %>%
  dplyr::select(starts_with("Count"), Threshold) %>%
  gather(Variable, Count, -Threshold) %>%
  mutate(Benefit =
           case_when(Variable == "Count_TN"  ~ Count * 0,
                     Variable == "Count_TP"  ~ ((-5000+10000+56000) * (Count * .25)) + 
                       (-2850 * (Count)),
                     Variable == "Count_FN"  ~ (-0) * Count,
                     Variable == "Count_FP"  ~ (-2850) * Count))

#visualization
whichThreshold %>%
  ggplot(.,aes(Threshold, Benefit, colour = Variable)) +
  geom_point() +
  scale_colour_manual(values = palette5[c(5, 1:3)]) +    
  labs(title = "Benefit by confusion matrix type and threshold",
       y = "Benefit") +
  guides(colour=guide_legend(title = "Confusion Matrix")) 

#benefit next period
whichThreshold_benefit <- 
  whichThreshold %>% 
  mutate(actualCredit = ifelse(Variable == "Count_TP", (Count * .25), 0)) %>% 
  group_by(Threshold) %>% 
  summarize(Benefit = sum(Benefit) ,
            Actual_Credit_Count = sum(actualCredit),
            Actual_Credit_Rate = sum(actualCredit) / sum(Count) ,
            Actual_Credit_Benefit_Loss =  sum(actualCredit * -2850 + -5000))
           

whichThreshold_benefit[1:5,]

BestThresh <- whichThreshold_benefit[which.max(whichThreshold_benefit$Benefit),]

#ggplot for total revenue- right now lower than optimal threshold it is .19
whichThreshold_benefit %>%
  dplyr::select(Threshold, Benefit) %>%
  gather(Variable, Value, -Threshold) %>%
  ggplot(aes(Threshold, Value, colour = Variable)) +
  geom_point() +
  geom_vline(xintercept = pull(arrange(whichThreshold_benefit, -Benefit)[1,1])) +
  scale_colour_manual(values = palette2) +
  labs(title = "Benefit this pay period and the next by threshold",
       subtitle = "Assuming no new customers added next period. Vertical line denotes optimal threshold")

#ggplot for total count of credits- right now lower than optimal threshold
whichThreshold_benefit %>%
  dplyr::select(Threshold, Actual_Credit_Count) %>%
  gather(Variable, Value, -Threshold) %>%
  ggplot(aes(Threshold, Value, colour = Variable)) +
  geom_point() +
  geom_vline(xintercept = pull(arrange(whichThreshold_benefit, -Benefit)[1,1])) +
  scale_colour_manual(values = palette2) +
  labs(title = "Benefit this pay period and the next by threshold",
       subtitle = "Assuming no new customers added next period. Vertical line denotes optimal threshold")

#table for Benefit
Table_values <- filter(whichThreshold_benefit, whichThreshold_benefit$Threshold== .50)%>%
  dplyr::select(Threshold, Benefit, Actual_Credit_Count)

Table_values1 <- filter(whichThreshold_benefit, whichThreshold_benefit$Threshold== .19)%>%
  dplyr::select(Threshold, Benefit, Actual_Credit_Count)

Final_Table <- rbind(Table_values, Table_values1)

kable(Final_Table) %>% 
  kable_styling(font_size = 12, full_width = F,
                bootstrap_options = c("striped", "hover", "condensed")) %>%
  footnote(general_title = "\n",
           general = "Table")
# Packages

library(ggplot2)
library(tidyverse)
library(arrow)
library(broom)
library(gender)
library(wru)
library(lubridate)
library(dplyr)
library(gtsummary)
library(zoo)
library(purrr)
library(wru)
library(lubridate)
library(caret)
library(pROC)
library(gplots)
library(randomForest)

# Working directory
setwd('/Users/aladelca/Library/CloudStorage/OneDrive-McGillUniversity/MMA/Talent_analytics/talent_analytics/exercises/exercise_3')



applications = arrow::read_feather('app_data_starter_coded.feather')



applications <- applications %>%
  mutate(
    filing_year_quarter = as.yearqtr(filing_date),
    abandon_year_quarter = as.yearqtr(abandon_date),
    issue_year_quarter = as.yearqtr(patent_issue_date)
  )


panel_data <- applications %>%
  group_by(examiner_id, filing_year_quarter) %>%
  summarise(
    num_new_applications = n_distinct(application_number),
    num_abandoned_applications = sum(disposal_type == "ABN", na.rm = TRUE),
    num_issued_patents = sum(disposal_type == "ISS", na.rm = TRUE),
    num_in_process_applications = sum(disposal_type == "PEND", na.rm = TRUE),
    current_art_unit = first(examiner_art_unit),
    .groups = 'drop'
  )




art_unit_info <- applications %>%
  group_by(filing_year_quarter, examiner_art_unit) %>%
  summarise(
    num_people_in_art_unit = n_distinct(examiner_id),
    num_women_in_art_unit = sum(gender.x == "female", na.rm = TRUE),
    .groups = 'drop'
  )


panel_data <- panel_data %>%
  left_join(art_unit_info, by = c("filing_year_quarter", "current_art_unit" = "examiner_art_unit"))

# Mark the last five quarters for each examiner
panel_data <- panel_data %>%
  group_by(examiner_id) %>%
  mutate(
    # Get a list of the last five quarters of activity for each examiner
    last_five_quarters = list(tail(sort(unique(filing_year_quarter)), 5))
  ) %>%
  ungroup() %>%
  mutate(
    # Check if the current quarter is in the last five quarters of activity
    separation_indicator = if_else(map_lgl(filing_year_quarter, ~ .x %in% last_five_quarters[[1]]), 1, 0)
  )


### Aggregate data at examiner_id

examiner_data = panel_data %>% 
                  group_by(examiner_id) %>% 
                  summarise(
                    #mean_new_applications_qt = mean(num_new_applications),
                    #total_new_applications_qt = sum(num_new_applications),
                    mean_abandoned_applications_qt = mean(num_abandoned_applications),
                    total_abandoned_applications = sum(num_abandoned_applications),
                    mean_issued_patents_qt = mean(num_issued_patents),
                    total_issued_patents = sum(num_issued_patents),
                    #mean_in_process_applications_qt = mean(num_in_process_applications),
                    separation_indicator = max(separation_indicator)
                  )

examiner_data_info = applications %>% group_by(examiner_id) %>% 
                        summarise(
                          gender = max(gender.x),
                          race = max(race.x),
                          mean_tenure = mean(tenure_days.x)
                          )

examiner_data_final = merge(examiner_data, examiner_data_info, by = 'examiner_id', all = TRUE)

examiner_data_final = examiner_data_final %>% 
                        filter(!is.na(!!sym('examiner_id')))

final_data = predict(dummyVars('~.', data = examiner_data_final), newdata = examiner_data_final)

final_data = data.frame(final_data)


final_data = final_data %>% 
                mutate(genderfemale = ifelse(is.na(genderfemale),0,genderfemale),
                       gendermale = ifelse(is.na(gendermale),0,gendermale))
                
final_data = final_data %>% select(-c('examiner_id'))

#final_data$separation_indicator = as.factor(final_data$separation_indicator)
#write_feather(final_data, paste0(data_path,"final_data.feather"))


#### Train test split
set.seed(123)
splitIndex = createDataPartition(final_data$separation_indicator, p = 0.7, list = FALSE)

# Create training set
train_data = final_data[splitIndex, ]

# Create testing set
test_data = final_data[-splitIndex, ]


logistic_model = glm(separation_indicator ~ ., data = train_data, family = "binomial", maxit = 1000)
summary(logistic_model)

logistic_predictions_probas = predict(logistic_model, test_data, type = 'response')
predictions = ifelse(logistic_predictions_probas>0.5,1,0)

conf_matrix = confusionMatrix(table(predictions, test_data$separation_indicator))
conf_matrix_data = as.matrix(conf_matrix$table)


roc_curve = roc(test_data$separation_indicator, logistic_predictions_probas)
roc_plot = plot(roc_curve, main = "ROC Curve", col = "blue")
auc(roc_curve)

rf_model = randomForest(separation_indicator ~., data = train_data, class.f = TRUE)

predictions_rf = predict(rf_model, test_data, type = "response")
predictions_rf

conf_matrix_rf = confusionMatrix(table(predictions_rf, test_data$separation_indicator))
conf_matrix_data_rf = as.matrix(conf_matrix_rf$table)


roc_curve_rf = roc(test_data$separation_indicator, predictions_rf)
roc_plot_rf = plot(roc_curve_rf, main = "ROC Curve", col = "blue")
auc(roc_curve_rf)

ggroc(list(logistic = roc_curve, random_forest = roc_curve_rf))+
  labs(title = "Comparison of two models")

print(auc(roc_curve))




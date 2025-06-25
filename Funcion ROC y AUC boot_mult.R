#### ROC analysis function bootstrap ####

# loading the libraries necessaries 

library(pROC)
library(caret)

# The function Roc_analysis is created to calculate ROC curve and AUC. Note the predicto is delected in this case.
# Given that a Firth penalized logistic regression was used, the AUC was estimated using ROC 
# curves based on bootstrapping. This approach allows for the derivation of robust confidence intervals

Roc_analysis_boot_mult <- function(model, test_data ,resp, model_name = "Model") {
  
  # Calculating the predicted probabilities for the test data. Rensponse argument ensures that the output is in the form of probabilities
  
  probabilities <- predict(model, test_data ,type = "response")
  
  # Transforming response variable in the test data into a factor.
  
  test_data[[resp]]<- factor(test_data[[resp]], levels = c ("Yes", "No"))
  
  # # ROC curve with bootstrapping for confidence intervals. stratified = TRUE maintains the class proportion
  roc_obj <- roc(test_data[[resp]], probabilities, levels = c("No", "Yes"), direction = "<", boot.n = 10000, stratified = TRUE)
  
  # Plotting ROC curve
  plot(roc_obj, col = "red", lwd = 2, main = paste("Curva ROC -", model_name), print.auc = TRUE)
  
  # Searching the best cutoff (Youden)
  Cutoff <- coords(roc_obj, "best", ret = c("threshold", "sensitivity", "specificity"), transpose = FALSE)
  print(Cutoff)
  
  best_threshold <- Cutoff$threshold
  pred_clas <- ifelse(probabilities > best_threshold, "Yes", "No")
  pred_clas <- factor(pred_clas, levels = c("Yes", "No"))
  
  # Confusion matrix 
  
  conf_ma <- confusionMatrix(pred_clas, test_data[[resp]], positive = "Yes")
  print (conf_ma)  
  # Obtaining CI and SD
  auc_ci <- ci.auc(roc_obj)
  auc_sd <- (auc_ci[3] - auc_ci[1]) / 3.92
  
  # Creating a summary data frame of the ROC analysis results
  summary_df <- data.frame(
    Variable = model_name,
    AUC = round(auc(roc_obj), 2),
    SD = round(auc_sd, 2),
    CI_95 = paste0("(", round(auc_ci[1],1), "-", round(auc_ci[3],1), ")"),
    Cutoff = round(Cutoff$threshold, 2),
    Sensitivity = round(Cutoff$sensitivity * 100, 1),
    Specificity = round(Cutoff$specificity * 100, 1))
  # Returning a list containing various ROC analysis outputs
  return(list(
    roc = roc_obj,
    threshold = best_threshold,
    confusion = conf_ma,
    summary = summary_df ))
}



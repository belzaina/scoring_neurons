#' Run DECISION TREE (RPART) Algorithm
#' 
#' Inputs : 
#' 
#'    - Training DataFrame
#'    - Testing DataFrame
#'    - ... (extra argument to be passed to rpart function)
#' 
#' 
#'    - Predicted_Y_Test_Prob
#'    - Predicted_Y_Test_Class
#'    - var_ranks (tibble)
#' 
decision_tree <- function(train_dataframe, test_dataframe, ...) {
    
    rpart_model <- rpart::rpart(DEFAULT ~ ., data = train_dataframe)
    
    var_ranks <- rpart_model$variable.importance %>% 
        as.data.frame() %>%
        tibble::rownames_to_column() %>%
        `colnames<-` (c("Predictor", "Importance")) %>%
        tibble::as_tibble()
    
    predicted_probs <- predict(rpart_model, test_dataframe)[, "1"]
    
    list(
        Predicted_Y_Test_Prob  = predicted_probs,
        Predicted_Y_Test_Class = (predicted_probs > 0.5) %>% as.numeric(),
        var_ranks              = var_ranks
    )
    
}
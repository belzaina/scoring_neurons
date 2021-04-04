#' Run GRADIENT BOOSTING (XGBOOST) Algorithm
#' 
#' Inputs : 
#' 
#'    - Training DataFrame
#'    - Testing DataFrame
#'    - nrounds (max number of boosting iterations)
#'    - early_stopping_rounds (stop if no improvement for 10 consecutive trees)
#'    - nfold (for cross validation)
#'    - random_seed
#'    - var_importance_measure (Gain, Cover, or Frequency)
#' 
#' Outputs: List of 
#' 
#'    - Predicted_Y_Test_Prob
#'    - Predicted_Y_Test_Class
#'    - var_ranks (tibble)
#' 
gradient_boosting <- function(train_dataframe, test_dataframe, n_rounds = 1000,
                              early_stopping_rounds = 20, n_fold = 5, 
                              random_seed = 8081, 
                              var_importance_measure = c("Gain", "Cover", "Frequency")) {
    
    #'
    #' Cross Validation
    #' Get Optimal Number of Iterations
    #' 
    set.seed(random_seed)
    xgb_cv <- xgboost::xgb.cv(
        data = train_dataframe %>%
            dplyr::select(-DEFAULT) %>%
            as.matrix(),
        label = train_dataframe$DEFAULT %>% as.character() %>% as.numeric(),
        nrounds = n_rounds,
        nfold = n_fold,
        objective = "binary:logistic",
        verbose = 0,
        early_stopping_rounds = early_stopping_rounds,
        eval_metric = "logloss"
    )
    
    opt_ntrees <- xgb_cv$evaluation_log$test_logloss_mean %>% which.min()
    
    #'
    #' Training
    #' Train Final Model with Optimal Number of Iterations (opt_ntrees)
    #'
    set.seed(random_seed)
    xgb_fit <- xgboost::xgboost(
        data = train_dataframe %>%
            dplyr::select(-DEFAULT) %>%
            as.matrix(),
        label = train_dataframe$DEFAULT %>% as.character() %>% as.numeric(),
        nrounds = opt_ntrees,
        objective = "binary:logistic",
        verbose = 0,
        eval_metric = "logloss",
    ) 
    
    #'
    #' Variable Importance
    #'
    var_ranks <- xgboost::xgb.importance(model = xgb_fit) %>% 
        dplyr::select("Feature", var_importance_measure) %>%
        `colnames<-`(c("Predictor", "Importance")) %>%
        tibble::as_tibble()
    
    #'
    #' Predict!
    #'
    predicted_probs <- predict(
        xgb_fit, 
        test_dataframe %>%
            dplyr::select(-DEFAULT) %>%
            as.matrix()
    )
    
    list(
        Predicted_Y_Test_Prob  = predicted_probs,
        Predicted_Y_Test_Class = (predicted_probs > 0.5) %>% as.numeric(),
        var_ranks              = var_ranks,
        opt_niters             = opt_ntrees
    )
    
}
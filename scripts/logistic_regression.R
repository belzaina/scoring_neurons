#' Run Logistic Regression
#' 
#' Inputs :
#'  
#'    - Training DataFrame
#'    - Testing DataFrame
#'    - Penalty (-1: NO PENALTY, 0: RIDGE, 1: LASSO)
#'      
#'      NB: If no penalty, qualitative predictors should be encoded as R factor to avoid low rank warning
#'          In our application, use "credit_dataset_eda" tibble.
#' 
#' Output: List of
#'  
#'    - Predicted_Y_Test_Prob  
#'    - Predicted_Y_Test_Class
#'    - coef_ranks (tibble)
#' 
logistic_regression <- function(train_dataframe, test_dataframe, penalty = c(-1, 0, 1)) {
    
    if (penalty == -1) {
        
        # Train
        logistic_model <- glm(DEFAULT ~ ., data = train_dataframe, family = "binomial")
        
        # coef_ranks
        cs <- coef(logistic_model)
        coef_ranks <- cs[-1]
        
        coef_ranks <- dplyr::tibble(
            Predictor = names(coef_ranks),
            Coefficient = coef_ranks,
            Coefficient_Magnitude = abs(coef_ranks)
        ) %>% dplyr::arrange(
            dplyr::desc(Coefficient_Magnitude)
        )
        
        # Test
        predicted_probs <- predict(logistic_model, test_dataframe, type = "response")
        
        list(
            
            Predicted_Y_Test_Prob  = predicted_probs,
            Predicted_Y_Test_Class = (predicted_probs > 0.5) %>% as.numeric(),
            coef_ranks             = coef_ranks
            
        )
        
    } else {
        
        # CASE 2: RIDGE (penalty = 0) OR LASSO (penalty = 1)
        # 10-FOLD CV
        glmnet_model <- glmnet::cv.glmnet(
            x = train_dataframe %>% dplyr::select(-DEFAULT) %>% data.matrix(),
            y = train_dataframe$DEFAULT,
            family = "binomial",
            type.measure = "auc",
            nfolds = 10,
            alpha = penalty
        )
        
        # coef_ranks
        cs <- as.matrix(coef(glmnet_model, s = "lambda.min"))
        coef_ranks <- cs[-1, 1]
        
        coef_ranks <- dplyr::tibble(
            Predictor = names(coef_ranks),
            Coefficient = coef_ranks,
            Coefficient_Magnitude = abs(coef_ranks)
        ) %>% dplyr::arrange(
            dplyr::desc(Coefficient_Magnitude)
        )
        
        # MAKE PREDICTIONS
        predicted_test_class <- predict(
            glmnet_model, 
            newx = test_dataframe %>% dplyr::select(-DEFAULT) %>% data.matrix(), 
            s = "lambda.min", 
            type = "class"
        ) %>% as.numeric()
        
        predicted_test_prob <- predict(
            glmnet_model, 
            newx = test_dataframe %>% dplyr::select(-DEFAULT) %>% data.matrix(), 
            s = "lambda.min", 
            type = "response"
        ) %>% as.numeric()
        
        list(
            Predicted_Y_Test_Prob  = predicted_test_prob,
            Predicted_Y_Test_Class = predicted_test_class,
            coef_ranks             = coef_ranks
        )
        
    }
    
}
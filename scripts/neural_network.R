#' Run NEURAL NETWORK Algorithm
#' 
#' Inputs : 
#' 
#'    - Training DataFrame
#'    - Testing DataFrame
#'    - Hidden Layer Size
#' 
#' Outputs: List of 
#' 
#'    - Predicted_Y_Test_Prob
#'    - Predicted_Y_Test_Class
#'    - var_ranks (tibble)
#' 
neural_network <- function(train_dataframe, test_dataframe, hidden_size, max_iter = 100) {
    
    train_X <- train_dataframe %>%
        dplyr::select(-DEFAULT)
    
    train_Y <- train_dataframe %>%
        dplyr::select(DEFAULT)
    
    test_X <- test_dataframe %>%
        dplyr::select(-DEFAULT)
    
    test_Y <- test_dataframe %>%
        dplyr::select(DEFAULT)
    
    # SCALE DATA
    # Avoid Look-ahead bias: Compute Min & Max using the train Set only!
    maxv <- train_X %>%
        apply(2, max)
    
    minv <- train_X %>%
        apply(2, min)
    
    train_X_scaled <- train_X %>%
        scale(center = minv, scale = maxv - minv) %>%
        as.data.frame() %>%
        tibble::as_tibble()
    
    test_X_scaled <- test_X %>%
        scale(center = minv, scale = maxv - minv) %>%
        as.data.frame() %>%
        tibble::as_tibble()
    
    scaled_train <- cbind(train_Y, train_X_scaled) %>%
        tibble::as_tibble()
    
    scaled_test <- cbind(test_Y, test_X_scaled) %>%
        tibble::as_tibble()
    
    nn_fit <- nnet::nnet(DEFAULT ~ ., data = scaled_train, size = hidden_size, 
                         maxit = max_iter)
    
    var_ranks <- NeuralNetTools::olden(nn_fit, bar_plot = FALSE) %>%
        tibble::rownames_to_column() %>%
        `colnames<-` (c("Predictor", "Importance")) %>%
        tibble::as_tibble()
    
    predicted_probs <- predict(nn_fit, scaled_test)
    
    list(
        Predicted_Y_Test_Prob  = predicted_probs,
        Predicted_Y_Test_Class = (predicted_probs > 0.5) %>% as.numeric(),
        var_ranks              = var_ranks
    )
    
}
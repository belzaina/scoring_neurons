library(shiny)
library(shinydashboard)
library(ggplot2)
library(magrittr)


source("scripts/get_clean_dataset.R")
source("scripts/logistic_regression.R")
source("scripts/compute_evaluation_criteria.R")
source("scripts/random_forest.R")
source("scripts/svm.R")
source("scripts/decision_tree.R")
source("scripts/gradient_boosting.R")
source("scripts/neural_network.R")


#' Load Clean Credit Dataset
#' 
credit_dataset <- get_clean_dataset()


#' For EDA,
#' Encode Categorical Variables as Factor
#'
credit_dataset_eda <- credit_dataset %>%
    dplyr::mutate(
        SEX = as.factor(SEX),
        EDUCATION = as.factor(EDUCATION),
        MARRIAGE = as.factor(MARRIAGE),
        DEFAULT = as.factor(DEFAULT)
    )


#' For Modeling,
#' One-Hot Encode Categorical Features
#'
credit_dataset_modeling <- credit_dataset %>%
    fastDummies::dummy_columns(select_columns = c("SEX", "EDUCATION", "MARRIAGE"), 
                               remove_selected_columns = TRUE)

#' Remove
#'
rm(credit_dataset)


#' Precompute Some Statistics
#'
n_rows <- nrow(credit_dataset_eda)
n_cols <- ncol(credit_dataset_eda)
missing_values <- round(100 * sum(is.na(credit_dataset_eda)) / (n_rows * n_cols), 2)
variable_names <- sort(colnames(credit_dataset_eda), decreasing = TRUE)


#' Models Performance Summary
#'
summary_tibble <- dplyr::tibble(
    
    Learning_Algo = c(
        "NN, 5 NEURONS",
        "NN, 10 NEURONS",
        "NN, 15 NEURONS",
        "LINEAR LR",
        "LINEAR LR (RIDGE)",
        "LINEAR LR (LASSO)",
        "DECISION TREE",
        "RANDOM FOREST",
        "GRADIENT BOOSTING",
        "SVM (RADIAL)",
        "SVM (POLYNOMIAL)"
    ),
    
    AUC = c(0.7713, 0.7764, 0.7704, 0.7597, 0.7606, 0.7597, 0.6446, 0.7653, 0.7792, 0.6391, 0.635),
    
    PCC = c(0.8183, 0.8176, 0.8171, 0.8201, 0.8083, 0.8189, 0.8213, 0.8186, 0.8226, 0.7799, 0.7814)
    
)


server <- function(input, output) {
    
    output$n_rows <- renderText(n_rows)
    
    output$n_cols <- renderText(n_cols)
    
    output$missing_values <- renderText(missing_values)
    
    output$colnames_select_input <- renderUI(
        selectInput("variable_name", 
                    "Please choose a variable:", 
                    variable_names)
    )
    
    output$summary_stats <- shiny::renderUI({
        
        if (!is.null(input$variable_name)) {
            
            summarytools::st_options(
                footnote = NA,
                headings = TRUE,
                plain.ascii = FALSE,
                dfSummary.varnumbers = FALSE
            )
            
            univ_summary <- credit_dataset_eda %>%
                dplyr::select(input$variable_name) %>%
                summarytools::dfSummary()
            
            attr(univ_summary, "data_info")$Data.frame <- paste("Feature:", input$variable_name)
            
            univ_summary %<>% print(method = "render")
            
            bivar_summary <- credit_dataset_eda %>%
                dplyr::select(input$variable_name, DEFAULT) %>%
                dplyr::group_by(DEFAULT) %>%
                summarytools::dfSummary()
            
            bivar_summary[[1]] %<>% head(n = 1)
            bivar_summary[[2]] %<>% head(n = 1)
            
            attr(bivar_summary[[1]], "data_info")$Data.frame <- paste("Feature:", input$variable_name)
            
            bivar_summary %<>% print(method = "render")
            
            if (input$variable_name == "DEFAULT") {
                
                univ_summary
                
            } else {
                
                shiny::tabsetPanel(
                    
                    shiny::tabPanel(
                        strong("ALL"),
                        br(),
                        univ_summary
                    ),
                    
                    shiny::tabPanel(
                        strong("BY TARGET"),
                        br(),
                        bivar_summary
                    )
                    
                )
                
            }
            
        }
        
    })
    
    llr_results <- eventReactive(input$llr_train_button, {
        
        waiting_message <- ifelse(
            input$llr_penalty == -1,
            "I learn pretty fast. Chances are you will never notice me...",
            "Having fun while penalizing some predictors..."
        )
        
        showModal(modalDialog(waiting_message, footer = icon("smile-wink")))
        
        # Prepare Train & Test Sets
        n_train   <- round(n_rows * input$llr_fraction_train)
        set.seed(input$llr_seed)
        i_train   <- sample(1:n_rows, size = n_train)
        
        if (input$llr_penalty == -1) {
            
            # use R factor encoding to avoid rank-deficient
            train_set <- credit_dataset_eda[i_train, ]
            test_set  <- credit_dataset_eda[-i_train, ]
            
        } else {
            
            train_set <- credit_dataset_modeling[i_train, ]
            test_set  <- credit_dataset_modeling[-i_train, ]
            
        }
        
        results <- logistic_regression(train_set, test_set, penalty = input$llr_penalty)
        
        eval_metrics <- compute_evaluation_criteria(
            test_set$DEFAULT %>% as.character() %>% as.numeric(), 
            results[['Predicted_Y_Test_Prob']], 
            results[['Predicted_Y_Test_Class']]
        )
        
        removeModal()
        
        list(
            
            "eval_metrics" = eval_metrics,
            
            "coef_ranks" = results[["coef_ranks"]]
            
        )
        
    })
    
    output$llr_eval_metrics <- renderUI({
        
        eval_metrics <- llr_results()[["eval_metrics"]] %>% round(4)
        
        fluidRow(
            
            box(
                
                title = "TEST SET RESULTS",
                
                width = 12,
                
                valueBox(
                    
                    value    = eval_metrics$AUC,
                    subtitle = "Area under the ROC Curve (AUC)",
                    icon     = icon("chart-area"),
                    width    = 4,
                    color    = "blue"
                    
                ),
                
                valueBox(
                    
                    value    = eval_metrics$GINI,
                    subtitle = "GINI",
                    icon     = icon("goodreads-g"),
                    width    = 4,
                    color    = "blue"
                    
                ),
                
                valueBox(
                    
                    value    = eval_metrics$PCC,
                    subtitle = "Percent of Correct Classifcation (PCC)",
                    icon     = icon("product-hunt"),
                    width    = 4,
                    color    = "blue"
                    
                ),
                
                valueBox(
                    
                    value    = eval_metrics$BS,
                    subtitle = "Brier Score (BS)",
                    icon     = icon("bold"),
                    width    = 6,
                    color    = "blue"
                    
                ),
                
                valueBox(
                    
                    value    = eval_metrics$KS,
                    subtitle = "Kolmogorov-Smirnov Statistic (KS)",
                    icon     = icon("kickstarter-k"),
                    width    = 6,
                    color    = "blue"
                    
                )
                
            )
            
        )
        
    })
    
    output$llr_var_imp <- renderUI({
        
        coef_ranks <- llr_results()[["coef_ranks"]] %>%
            dplyr::mutate_if(is.numeric, round, 4)
        
        div(
            
            fluidRow(
                
                box(
                    
                    title = "TOP-10 PREDICTORS",
                    
                    width = 6,
                    
                    renderPlot(
                        
                        coef_ranks %>%
                            head(n = 10) %>%
                            dplyr::mutate(
                                
                                Predictor = factor(Predictor, levels = Predictor[order(Coefficient_Magnitude)])
                                
                            ) %>%
                            ggplot(aes(x = Predictor, y = Coefficient_Magnitude)) +
                            geom_bar(stat = "identity", fill = "#f68060", alpha = .6, width = .4) +
                            coord_flip() +
                            xlab("") +
                            ylab("COEFFICIENT MAGNITUDE") +
                            theme_bw()
                        
                    )
                    
                ),
                
                box(
                    
                    title = "PREDICTORS IMPORTANCE - SORTED BY COEFFICIENTS MAGNITUDE",
                    
                    width = 6,
                    
                    height = "467px",
                    
                    br(),
                    
                    DT::renderDataTable(
                        
                        coef_ranks %>% dplyr::select(Predictor, Coefficient),
                        
                        class = "display nowrap",
                        
                        rownames= FALSE,
                        
                        options = list(
                            
                            scrollX = TRUE,
                            pageLength = 5
                            
                        )
                        
                    )
                    
                )
                
            ),
            
            fluidRow(
                
                column(
                    
                    width = 12,
                    
                    align = "center",
                    
                    h3("TRAIN AGAIN?"),
                    
                    br()
                    
                )
                
            )
            
        )
        
    })
    
    rf_results <- eventReactive(input$rf_train_button, {
        
        showModal(modalDialog("There are many ways to explore the forest beyond a walk in the woods...", 
                              footer = icon("tree")))
        
        # Prepare Train & Test Sets
        n_train   <- round(n_rows * input$rf_fraction_train)
        set.seed(input$rf_seed)
        i_train   <- sample(1:n_rows, size = n_train)
        train_set <- credit_dataset_modeling[i_train, ]
        test_set  <- credit_dataset_modeling[-i_train, ]
        
        results <- random_forest(train_set, test_set, 
                                 random_seed = input$rf_seed, var_imp_type = input$rf_var_imp_metric)
        
        eval_metrics <- compute_evaluation_criteria(
            test_set$DEFAULT %>% as.character() %>% as.numeric(), 
            results[['Predicted_Y_Test_Prob']], 
            results[['Predicted_Y_Test_Class']]
        )
        
        removeModal()
        
        list(
            
            "eval_metrics" = eval_metrics,
            
            "optim_ntree" = results[["optim_ntree"]],
            
            "var_ranks" = results[["var_ranks"]]
            
        )
        
    })
    
    output$rf_eval_metrics <- renderUI({
        
        eval_metrics <- rf_results()[["eval_metrics"]] %>% round(4)
        
        fluidRow(
            
            box(
                
                title = "TEST SET RESULTS",
                
                width = 12,
                
                valueBox(
                    
                    value    = eval_metrics$AUC,
                    subtitle = "Area under the ROC Curve (AUC)",
                    icon     = icon("chart-area"),
                    width    = 4,
                    color    = "blue"
                    
                ),
                
                valueBox(
                    
                    value    = eval_metrics$GINI,
                    subtitle = "GINI",
                    icon     = icon("goodreads-g"),
                    width    = 4,
                    color    = "blue"
                    
                ),
                
                valueBox(
                    
                    value    = eval_metrics$PCC,
                    subtitle = "Percent of Correct Classifcation (PCC)",
                    icon     = icon("product-hunt"),
                    width    = 4,
                    color    = "blue"
                    
                ),
                
                valueBox(
                    
                    value    = eval_metrics$BS,
                    subtitle = "Brier Score (BS)",
                    icon     = icon("bold"),
                    width    = 4,
                    color    = "blue"
                    
                ),
                
                valueBox(
                    
                    value    = eval_metrics$KS,
                    subtitle = "Kolmogorov-Smirnov Statistic (KS)",
                    icon     = icon("kickstarter-k"),
                    width    = 4,
                    color    = "blue"
                    
                ),
                
                valueBox(
                    
                    value    = rf_results()[["optim_ntree"]],
                    subtitle = "Optimal Number of Trees",
                    icon     = icon("tree"),
                    width    = 4,
                    color    = "blue"
                    
                )
                
            )
            
        )
        
    })
    
    output$rf_var_imp <- renderUI({
        
        var_ranks <- rf_results()[["var_ranks"]] %>%
            dplyr::mutate_if(is.numeric, round, 4)
        
        div(
            
            fluidRow(
                
                box(
                    
                    title = "TOP-10 PREDICTORS",
                    
                    width = 6,
                    
                    renderPlot(
                        
                        var_ranks %>%
                            head(n = 10) %>%
                            dplyr::mutate(
                                
                                Predictor = factor(Predictor, levels = Predictor[order(Importance)])
                                
                            ) %>%
                            ggplot(aes(x = Predictor, y = Importance)) +
                            geom_bar(stat = "identity", fill = "#f68060", alpha = .6, width = .4) +
                            coord_flip() +
                            xlab("") +
                            ylab("PREDICTOR IMPORTANCE") +
                            theme_bw()
                        
                    )
                    
                ),
                
                box(
                    
                    title = "PREDICTORS IMPORTANCE - SORTED BY THE IMPORTANCE METRIC",
                    
                    width = 6,
                    
                    height = "467px",
                    
                    br(),
                    
                    DT::renderDataTable(
                        
                        var_ranks,
                        
                        class = "display nowrap",
                        
                        rownames= FALSE,
                        
                        options = list(
                            
                            scrollX = TRUE,
                            pageLength = 5
                            
                        )
                        
                    )
                    
                )
                
            ),
            
            fluidRow(
                
                column(
                    
                    width = 12,
                    
                    align = "center",
                    
                    h3("TRAIN AGAIN?"),
                    
                    br()
                    
                )
                
            )
            
        )
        
    })
    
    svm_results <- eventReactive(input$svm_train_button, {
        
        showModal(modalDialog("Trying to enforce social distancing in data by maximizing the margin...", 
                              footer = icon("grin-squint-tears")))
        
        # Prepare Train & Test Sets
        n_train   <- round(n_rows * input$svm_fraction_train)
        set.seed(input$svm_seed)
        i_train   <- sample(1:n_rows, size = n_train)
        train_set <- credit_dataset_modeling[i_train, ]
        test_set  <- credit_dataset_modeling[-i_train, ]
        
        results <- svm_learner(train_set, test_set, kernel = input$svm_kernel)
        
        eval_metrics <- compute_evaluation_criteria(
            test_set$DEFAULT %>% as.character() %>% as.numeric(), 
            results[['Predicted_Y_Test_Prob']], 
            results[['Predicted_Y_Test_Class']]
        )
        
        removeModal()
        
        list(
            
            "eval_metrics" = eval_metrics
            
        )
        
    })
    
    output$svm_eval_metrics <- renderUI({
        
        eval_metrics <- svm_results()[["eval_metrics"]] %>% round(4)
        
        div(
            
            fluidRow(
                
                box(
                    
                    title = "TEST SET RESULTS",
                    
                    width = 12,
                    
                    valueBox(
                        
                        value    = eval_metrics$AUC,
                        subtitle = "Area under the ROC Curve (AUC)",
                        icon     = icon("chart-area"),
                        width    = 4,
                        color    = "blue"
                        
                    ),
                    
                    valueBox(
                        
                        value    = eval_metrics$GINI,
                        subtitle = "GINI",
                        icon     = icon("goodreads-g"),
                        width    = 4,
                        color    = "blue"
                        
                    ),
                    
                    valueBox(
                        
                        value    = eval_metrics$PCC,
                        subtitle = "Percent of Correct Classifcation (PCC)",
                        icon     = icon("product-hunt"),
                        width    = 4,
                        color    = "blue"
                        
                    ),
                    
                    valueBox(
                        
                        value    = eval_metrics$BS,
                        subtitle = "Brier Score (BS)",
                        icon     = icon("bold"),
                        width    = 6,
                        color    = "blue"
                        
                    ),
                    
                    valueBox(
                        
                        value    = eval_metrics$KS,
                        subtitle = "Kolmogorov-Smirnov Statistic (KS)",
                        icon     = icon("kickstarter-k"),
                        width    = 6,
                        color    = "blue"
                        
                    )
                    
                )
                
            ),
            
            fluidRow(
                
                column(
                    
                    width = 12,
                    
                    align = "center",
                    
                    h3("TRAIN AGAIN?"),
                    
                    br()
                    
                )
                
            )
            
        )
        
    })
    
    dt_results <- eventReactive(input$dt_train_button, {
        
        showModal(modalDialog("First it was a seed, then a stem, and then a growing tree...", 
                              footer = icon("tree")))
        
        # Prepare Train & Test Sets
        n_train   <- round(n_rows * input$dt_fraction_train)
        set.seed(input$dt_seed)
        i_train   <- sample(1:n_rows, size = n_train)
        train_set <- credit_dataset_modeling[i_train, ]
        test_set  <- credit_dataset_modeling[-i_train, ]
        
        results <- decision_tree(train_set, test_set)
        
        eval_metrics <- compute_evaluation_criteria(
            test_set$DEFAULT %>% as.character() %>% as.numeric(), 
            results[['Predicted_Y_Test_Prob']], 
            results[['Predicted_Y_Test_Class']]
        )
        
        removeModal()
        
        list(
            
            "eval_metrics" = eval_metrics,
            
            "var_ranks" = results[["var_ranks"]]
            
        )
        
    })
    
    output$dt_eval_metrics <- renderUI({
        
        eval_metrics <- dt_results()[["eval_metrics"]] %>% round(4)
        
        fluidRow(
            
            box(
                
                title = "TEST SET RESULTS",
                
                width = 12,
                
                valueBox(
                    
                    value    = eval_metrics$AUC,
                    subtitle = "Area under the ROC Curve (AUC)",
                    icon     = icon("chart-area"),
                    width    = 4,
                    color    = "blue"
                    
                ),
                
                valueBox(
                    
                    value    = eval_metrics$GINI,
                    subtitle = "GINI",
                    icon     = icon("goodreads-g"),
                    width    = 4,
                    color    = "blue"
                    
                ),
                
                valueBox(
                    
                    value    = eval_metrics$PCC,
                    subtitle = "Percent of Correct Classifcation (PCC)",
                    icon     = icon("product-hunt"),
                    width    = 4,
                    color    = "blue"
                    
                ),
                
                valueBox(
                    
                    value    = eval_metrics$BS,
                    subtitle = "Brier Score (BS)",
                    icon     = icon("bold"),
                    width    = 4,
                    color    = "blue"
                    
                ),
                
                valueBox(
                    
                    value    = eval_metrics$KS,
                    subtitle = "Kolmogorov-Smirnov Statistic (KS)",
                    icon     = icon("kickstarter-k"),
                    width    = 4,
                    color    = "blue"
                    
                ),
                
                valueBox(
                    
                    value    = dt_results()[["var_ranks"]] %>% nrow(),
                    subtitle = "Selected Predictors",
                    icon     = icon("tree"),
                    width    = 4,
                    color    = "blue"
                    
                )
                
            )
            
        )
        
    })
    
    output$dt_var_imp <- renderUI({
        
        var_ranks <- dt_results()[["var_ranks"]] %>%
            dplyr::mutate_if(is.numeric, round, 4)
        
        div(
            
            fluidRow(
                
                box(
                    
                    title = "PREDICTORS IMPORTANCE",
                    
                    width = 6,
                    
                    renderPlot(
                        
                        var_ranks %>%
                            dplyr::mutate(
                                
                                Predictor = factor(Predictor, levels = Predictor[order(Importance)])
                                
                            ) %>%
                            ggplot(aes(x = Predictor, y = Importance)) +
                            geom_bar(stat = "identity", fill = "#f68060", alpha = .6, width = .4) +
                            coord_flip() +
                            xlab("") +
                            ylab("PREDICTOR IMPORTANCE") +
                            theme_bw()
                        
                    )
                    
                ),
                
                box(
                    
                    title = "PREDICTORS IMPORTANCE - SORTED BY THE IMPORTANCE METRIC",
                    
                    width = 6,
                    
                    height = "467px",
                    
                    br(),
                    
                    DT::renderDataTable(
                        
                        var_ranks,
                        
                        class = "display nowrap",
                        
                        rownames= FALSE,
                        
                        options = list(
                            
                            scrollX = TRUE,
                            pageLength = 5
                            
                        )
                        
                    )
                    
                )
                
            ),
            
            fluidRow(
                
                column(
                    
                    width = 12,
                    
                    align = "center",
                    
                    h3("TRAIN AGAIN?"),
                    
                    br()
                    
                )
                
            )
            
        )
        
    })
    
    gb_results <- eventReactive(input$gb_train_button, {
        
        showModal(modalDialog("It takes a while to boost trees performance...", 
                              footer = icon("tree")))
        
        # Prepare Train & Test Sets
        n_train   <- round(n_rows * input$gb_fraction_train)
        set.seed(input$gb_seed)
        i_train   <- sample(1:n_rows, size = n_train)
        train_set <- credit_dataset_modeling[i_train, ]
        test_set  <- credit_dataset_modeling[-i_train, ]
        
        results <- gradient_boosting(train_set, test_set, n_rounds = input$gb_nrounds,
                                     early_stopping_rounds = input$gb_early_stop, 
                                     n_fold = input$gb_nfolds, random_seed = input$gb_seed,
                                     var_importance_measure = input$gb_var_imp)
        
        eval_metrics <- compute_evaluation_criteria(
            test_set$DEFAULT %>% as.character() %>% as.numeric(), 
            results[['Predicted_Y_Test_Prob']], 
            results[['Predicted_Y_Test_Class']]
        )
        
        removeModal()
        
        list(
            
            "eval_metrics" = eval_metrics,
            
            "optim_ntree" = results[["opt_niters"]],
            
            "var_ranks" = results[["var_ranks"]]
            
        )
        
    })
    
    output$gb_eval_metrics <- renderUI({
        
        eval_metrics <- gb_results()[["eval_metrics"]] %>% round(4)
        
        fluidRow(
            
            box(
                
                title = "TEST SET RESULTS",
                
                width = 12,
                
                valueBox(
                    
                    value    = eval_metrics$AUC,
                    subtitle = "Area under the ROC Curve (AUC)",
                    icon     = icon("chart-area"),
                    width    = 4,
                    color    = "blue"
                    
                ),
                
                valueBox(
                    
                    value    = eval_metrics$GINI,
                    subtitle = "GINI",
                    icon     = icon("goodreads-g"),
                    width    = 4,
                    color    = "blue"
                    
                ),
                
                valueBox(
                    
                    value    = eval_metrics$PCC,
                    subtitle = "Percent of Correct Classifcation (PCC)",
                    icon     = icon("product-hunt"),
                    width    = 4,
                    color    = "blue"
                    
                ),
                
                valueBox(
                    
                    value    = eval_metrics$BS,
                    subtitle = "Brier Score (BS)",
                    icon     = icon("bold"),
                    width    = 4,
                    color    = "blue"
                    
                ),
                
                valueBox(
                    
                    value    = eval_metrics$KS,
                    subtitle = "Kolmogorov-Smirnov Statistic (KS)",
                    icon     = icon("kickstarter-k"),
                    width    = 4,
                    color    = "blue"
                    
                ),
                
                valueBox(
                    
                    value    = gb_results()[["optim_ntree"]],
                    subtitle = "Optimal Number of Iterations (Trees)",
                    icon     = icon("tree"),
                    width    = 4,
                    color    = "blue"
                    
                )
                
            )
            
        )
        
    })
    
    output$gb_var_imp <- renderUI({
        
        var_ranks <- gb_results()[["var_ranks"]] %>%
            dplyr::mutate_if(is.numeric, round, 4)
        
        div(
            
            fluidRow(
                
                box(
                    
                    title = "TOP-10 PREDICTORS",
                    
                    width = 6,
                    
                    renderPlot(
                        
                        var_ranks %>%
                            head(n = 10) %>%
                            dplyr::mutate(
                                
                                Predictor = factor(Predictor, levels = Predictor[order(Importance)])
                                
                            ) %>%
                            ggplot(aes(x = Predictor, y = Importance)) +
                            geom_bar(stat = "identity", fill = "#f68060", alpha = .6, width = .4) +
                            coord_flip() +
                            xlab("") +
                            ylab("PREDICTOR IMPORTANCE") +
                            theme_bw()
                        
                    )
                    
                ),
                
                box(
                    
                    title = "PREDICTORS IMPORTANCE - SORTED BY THE IMPORTANCE METRIC",
                    
                    width = 6,
                    
                    height = "467px",
                    
                    br(),
                    
                    DT::renderDataTable(
                        
                        var_ranks,
                        
                        class = "display nowrap",
                        
                        rownames= FALSE,
                        
                        options = list(
                            
                            scrollX = TRUE,
                            pageLength = 5
                            
                        )
                        
                    )
                    
                )
                
            ),
            
            fluidRow(
                
                column(
                    
                    width = 12,
                    
                    align = "center",
                    
                    h3("TRAIN AGAIN?"),
                    
                    br()
                    
                )
                
            )
            
        )
        
    })
    
    nn_results <- eventReactive(input$nn_train_button, {
        
        showModal(modalDialog("The brain is a machine, let's train it!", 
                              footer = icon("brain")))
        
        # Prepare Train & Test Sets
        n_train   <- round(n_rows * input$nn_fraction_train)
        set.seed(input$nn_seed)
        i_train   <- sample(1:n_rows, size = n_train)
        train_set <- credit_dataset_modeling[i_train, ]
        test_set  <- credit_dataset_modeling[-i_train, ]
        
        results <- neural_network(train_set, test_set, 
                                  hidden_size = input$nn_hidden_size, max_iter = input$nn_max_iter)
        
        eval_metrics <- compute_evaluation_criteria(
            test_set$DEFAULT %>% as.character() %>% as.numeric(), 
            results[['Predicted_Y_Test_Prob']], 
            results[['Predicted_Y_Test_Class']]
        )
        
        removeModal()
        
        list(
            
            "eval_metrics" = eval_metrics,
            
            "var_ranks" = results[["var_ranks"]]
            
        )
        
    })
    
    output$nn_eval_metrics <- renderUI({
        
        eval_metrics <- nn_results()[["eval_metrics"]] %>% round(4)
        
        fluidRow(
            
            box(
                
                title = "TEST SET RESULTS",
                
                width = 12,
                
                valueBox(
                    
                    value    = eval_metrics$AUC,
                    subtitle = "Area under the ROC Curve (AUC)",
                    icon     = icon("chart-area"),
                    width    = 4,
                    color    = "blue"
                    
                ),
                
                valueBox(
                    
                    value    = eval_metrics$GINI,
                    subtitle = "GINI",
                    icon     = icon("goodreads-g"),
                    width    = 4,
                    color    = "blue"
                    
                ),
                
                valueBox(
                    
                    value    = eval_metrics$PCC,
                    subtitle = "Percent of Correct Classifcation (PCC)",
                    icon     = icon("product-hunt"),
                    width    = 4,
                    color    = "blue"
                    
                ),
                
                valueBox(
                    
                    value    = eval_metrics$BS,
                    subtitle = "Brier Score (BS)",
                    icon     = icon("bold"),
                    width    = 6,
                    color    = "blue"
                    
                ),
                
                valueBox(
                    
                    value    = eval_metrics$KS,
                    subtitle = "Kolmogorov-Smirnov Statistic (KS)",
                    icon     = icon("kickstarter-k"),
                    width    = 6,
                    color    = "blue"
                    
                )
                
            )
            
        )
        
    })
    
    output$nn_var_imp <- renderUI({
        
        var_ranks <- nn_results()[["var_ranks"]] %>%
            dplyr::mutate_if(is.numeric, round, 4)
        
        div(
            
            fluidRow(
                
                box(
                    
                    title = "RELATIVE IMPORTANCE OF INPUT VARIABLES (Olden et al. 2004)",
                    
                    width = 6,
                    
                    renderPlot(
                        
                        var_ranks %>%
                            dplyr::mutate(
                                
                                Predictor = factor(Predictor, levels = Predictor[order(Importance)])
                                
                            ) %>%
                            ggplot(aes(x = Predictor, y = Importance)) +
                            geom_bar(stat = "identity", fill = "#f68060", alpha = .6, width = .4) +
                            coord_flip() +
                            xlab("") +
                            ylab("PREDICTOR IMPORTANCE") +
                            theme_bw()
                        
                    )
                    
                ),
                
                box(
                    
                    title = "PREDICTORS IMPORTANCE TABLE",
                    
                    width = 6,
                    
                    height = "467px",
                    
                    br(),
                    
                    DT::renderDataTable(
                        
                        var_ranks %>% dplyr::arrange(desc(Importance)),
                        
                        class = "display nowrap",
                        
                        rownames= FALSE,
                        
                        options = list(
                            
                            scrollX = TRUE,
                            pageLength = 5
                            
                        )
                        
                    )
                    
                )
                
            ),
            
            fluidRow(
                
                column(
                    
                    width = 12,
                    
                    align = "center",
                    
                    h3("TRAIN AGAIN?"),
                    
                    br()
                    
                )
                
            )
            
        )
        
    })
    
    output$summary_results_auc <- renderPlot({
        
        summary_tibble %>%
            dplyr::mutate(
                Learning_Algo = factor(Learning_Algo, levels = Learning_Algo[order(AUC)])
            ) %>%
            ggplot(aes(x = Learning_Algo, y = AUC)) +
            geom_bar(stat = "identity", fill = "#f68060", alpha = .6, width = .7) +
            coord_flip() +
            xlab("") +
            ylab("AUC") +
            geom_text(aes(label = round(AUC, 3), hjust = 1.2), color = "darkblue", size = 5) +
            theme_bw() +
            theme(axis.text = element_text(size=12, face = "bold"), axis.title.x = element_text(size=16))
        
    })
    
}















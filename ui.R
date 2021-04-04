library(shiny)
library(shinydashboard)
library(magrittr)


ui <- dashboardPage(
    
    dashboardHeader(title = 'Neural Networks - M2 ESA'),
    
    dashboardSidebar(
        
        sidebarMenu(
            
            id = "main_menu",
            
            menuItem("INTRODUCTION", tabName = "introduction", icon = icon("door-open")),
            
            menuItem("EXPLORE THE DATASET", tabName = "explore_dataset", icon = icon("chart-bar")),
            
            menuItem("MODELS TESTING", tabName = "models", icon = icon("chart-line")),
            
            menuItem("ABOUT", tabName = "about", icon = icon("at"))
            
        )
        
    ),
    
    dashboardBody(
        
        tags$head(
            
            includeCSS(path = "./www/main.css"),
            
            HTML(
                
                '<link rel="icon" href="logo_title.jpg">'
                
            )
            
        ),
        
        tabItems(
            
            tabItem(
                
                tabName = "introduction",
                
                fluidRow(
                    
                    h1("NEURAL NETWORKS PROJECT"),
                    
                    align = "center"
                    
                ),
                
                br(), br(),
                
                fluidRow(
                    
                    box(
                        
                        width = 12,
                        
                        title = p("OUTLINE", style = "font-size:28px"),
                        
                        p("In this project we use Neural Networks in order to model the credit granting decision. The performance of Neural Network in the context of credit scoring is then compared to:"),
                        
                        tags$ul(
                            
                            tags$li("Logistic Regression"),
                            
                            tags$li("Decision Trees"),
                            
                            tags$li("Random Forest"),
                            
                            tags$li("Gradient Boosting"),
                            
                            tags$li("SVM"),
                            
                        ),
                        
                        br(),
                        
                        p("This interactive application allows you to:"),
                        
                        tags$ul(
                            
                            tags$li("Explore the credit dataset"),
                            
                            tags$li("Train and test different learning algorithms in the context of credit scoring"),
                            
                            tags$li("Perform robustness checking using N x 2-fold cross-validation")
                            
                        ),
                        
                    )
                    
                )
                
            ),
            
            tabItem(
                
                tabName = "explore_dataset",
                
                fluidRow(
                    
                    valueBox(
                        
                        value    = textOutput("n_rows"),
                        subtitle = "Number of Rows",
                        icon     = icon("list"),
                        width    = 4,
                        color    = "blue"
                        
                    ),
                    
                    valueBox(
                        
                        value    = textOutput("n_cols"),
                        subtitle = "Number of Variables",
                        icon     = icon("columns"),
                        width    = 4,
                        color    = "blue"
                        
                    ),
                    
                    valueBox(
                        
                        value    = textOutput("missing_values"),
                        subtitle = "Missing Values",
                        icon     = icon("percent"),
                        width    = 4,
                        color    = "blue"
                        
                    ),
                    
                ),
                
                fluidRow(
                    
                    box(
                        
                        title       = "Exploratory Data Analysis",
                        status      = "primary",
                        solidHeader = FALSE,
                        width       = 4,
                        
                        uiOutput("colnames_select_input")
                        
                    ),
                    
                    box(
                        
                        title       = span(icon("database"), 
                                           HTML('&nbsp;'), 
                                           strong("Credit Dataset")),
                        status      = "primary",
                        solidHeader = TRUE,
                        width       = 8,
                        htmlOutput("summary_stats")
                        
                    )
                    
                )
                
            ),
            
            tabItem(
                
                tabName = "models",
                
                fluidRow(
                    
                    box(
                        
                        width = 12,
                        
                        tabsetPanel(
                            
                            tabPanel(
                                
                                title = "Neural Networks",
                                
                                br(),
                                
                                uiOutput("nn_eval_metrics"),
                                
                                uiOutput("nn_var_imp"),
                                
                                fluidRow(
                                    
                                    column(
                                        
                                        offset = 3,
                                        
                                        width = 12,
                                        
                                        box(
                                            
                                            title = "TRAINING PARAMETERS",
                                            
                                            width = 6,
                                            
                                            numericInput("nn_seed", label = h4("Seed"), value = 8081, min = 1),
                                            
                                            sliderInput("nn_fraction_train", label = h4("Fraction Used for Training"), 
                                                        min = 0.1, max = 0.9, value = 0.7, step = 0.1),
                                            
                                            br(),
                                            
                                            actionButton("nn_train_button", h4("Train & Test"), width = '100%')
                                            
                                        )
                                        
                                    )
                                    
                                )
                                
                            ),
                            
                            tabPanel(
                                
                                title = "Gradient Boosting",
                                
                                br(),
                                
                                uiOutput("gb_eval_metrics"),
                                
                                uiOutput("gb_var_imp"),
                                
                                fluidRow(
                                    
                                    column(
                                        
                                        offset = 3,
                                        
                                        width = 12,
                                        
                                        box(
                                            
                                            title = "TRAINING PARAMETERS",
                                            
                                            width = 6,
                                            
                                            numericInput("gb_seed", label = h4("Seed"), value = 8081, min = 1),
                                            
                                            sliderInput("gb_fraction_train", label = h4("Fraction Used for Training"), 
                                                        min = 0.1, max = 0.9, value = 0.7, step = 0.1),
                                            
                                            numericInput("gb_nrounds", label = h4("Number of Rounds"), value = 1000, min = 10),
                                            
                                            numericInput("gb_early_stop", label = h4("Stop if no Improvement for ... Rounds"), value = 20, min = 10),
                                            
                                            numericInput("gb_nfolds", label = h4("CV Folds"), value = 5, min = 2),
                                            
                                            radioButtons(
                                                "gb_var_imp", 
                                                h4("Variable Importance Measure"),
                                                c("Gain" = "Gain", 
                                                  "Cover" = "Cover",
                                                  "Frequency" = "Frequency")
                                            ),
                                            
                                            br(),
                                            
                                            actionButton("gb_train_button", h4("Train & Test"), width = '100%')
                                            
                                        )
                                        
                                    )
                                    
                                )
                                
                            ),
                            
                            tabPanel(
                                
                                title = "Random Forest",
                                
                                br(),
                                
                                uiOutput("rf_eval_metrics"),
                                
                                uiOutput("rf_var_imp"),
                                
                                fluidRow(
                                    
                                    column(
                                        
                                        offset = 3,
                                        
                                        width = 12,
                                        
                                        box(
                                            
                                            title = "TRAINING PARAMETERS",
                                            
                                            width = 6,
                                            
                                            numericInput("rf_seed", label = h4("Seed"), value = 8081, min = 1),
                                            
                                            sliderInput("rf_fraction_train", label = h4("Fraction Used for Training"), 
                                                        min = 0.1, max = 0.9, value = 0.7, step = 0.1),
                                            
                                            radioButtons(
                                                "rf_var_imp_metric", 
                                                h4("Variable Importance Measure"),
                                                c("Mean Decrease in Accuracy" = 1, 
                                                  "Mean Decrease in Node Impurity" = 2)
                                            ),
                                            
                                            br(),
                                            
                                            actionButton("rf_train_button", h4("Train & Test"), width = '100%')
                                            
                                        )
                                        
                                    )
                                    
                                )
                                
                            ),
                            
                            tabPanel(
                                
                                title = "Decision Trees",
                                
                                br(),
                                
                                uiOutput("dt_eval_metrics"),
                                
                                uiOutput("dt_var_imp"),
                                
                                fluidRow(
                                    
                                    column(
                                        
                                        offset = 3,
                                        
                                        width = 12,
                                        
                                        box(
                                            
                                            title = "TRAINING PARAMETERS",
                                            
                                            width = 6,
                                            
                                            numericInput("dt_seed", label = h4("Seed"), value = 8081, min = 1),
                                            
                                            sliderInput("dt_fraction_train", label = h4("Fraction Used for Training"), 
                                                        min = 0.1, max = 0.9, value = 0.7, step = 0.1),
                                            
                                            br(),
                                            
                                            actionButton("dt_train_button", h4("Train & Test"), width = '100%')
                                            
                                        )
                                        
                                    )
                                    
                                )
                                
                            ),
                            
                            tabPanel(
                                
                                title = "Linear Logistic Regression",
                                
                                br(),
                                
                                uiOutput("llr_eval_metrics"),
                                
                                uiOutput("llr_var_imp"),
                                
                                fluidRow(
                                    
                                    column(
                                        
                                        offset = 3,
                                        
                                        width = 12,
                                        
                                        box(
                                            
                                            title = "TRAINING PARAMETERS",
                                            
                                            width = 6,
                                            
                                            numericInput("llr_seed", label = h4("Seed"), value = 8081, min = 1),
                                            
                                            sliderInput("llr_fraction_train", label = h4("Fraction Used for Training"), 
                                                        min = 0.1, max = 0.9, value = 0.7, step = 0.1),
                                            
                                            radioButtons(
                                                "llr_penalty", 
                                                h4("Penalty"),
                                                c("No Penalty"          = -1, 
                                                  "Ridge"               = 0,
                                                  "LASSO"               = 1)
                                            ),
                                            
                                            br(),
                                            
                                            actionButton("llr_train_button", h4("Train & Test"), width = '100%')
                                            
                                        )
                                        
                                    )
                                    
                                )
                                
                            ),
                            
                            tabPanel(
                                
                                title = "Support Vector Machine",
                                
                                br(),
                                
                                uiOutput("svm_eval_metrics"),
                                
                                fluidRow(
                                    
                                    column(
                                        
                                        offset = 3,
                                        
                                        width = 12,
                                        
                                        box(
                                            
                                            title = "TRAINING PARAMETERS",
                                            
                                            width = 6,
                                            
                                            numericInput("svm_seed", label = h4("Seed"), value = 8081, min = 1),
                                            
                                            sliderInput("svm_fraction_train", label = h4("Fraction Used for Training"), 
                                                        min = 0.1, max = 0.9, value = 0.7, step = 0.1),
                                            
                                            radioButtons(
                                                "svm_kernel", 
                                                h4("Kernel"),
                                                c("Radial" = "radial", 
                                                  "Polynomial" = "polynomial")
                                            ),
                                            
                                            br(),
                                            
                                            actionButton("svm_train_button", h4("Train & Test"), width = '100%')
                                            
                                        )
                                        
                                    )
                                    
                                )
                                
                            )
                            
                        )
                        
                    )
                    
                )
                
            )
            
        )
        
    )
    
)
















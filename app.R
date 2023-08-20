library(shiny)
library(shinythemes)
library(lattice)
library(latticeExtra)
library(car)
library(ggplot2)
library(rcompanion)
library(corrplot)
low_birth <- read.delim("low birth weight infants.txt")
# Updating the existing 'gender' column
# Display the updated dataframe
attach(low_birth)
ui <- fluidPage(
  
  tabsetPanel(
    tabPanel("About the App",
             HTML("<h1> Predicting Head Circumference in Low Birthweight Infants</h1>
                   <p>Are you curious about predicting head circumference in low birthweight infants? Our interactive app is designed to help you explore and understand the intricate relationships between various factors that influence head circumference. Whether you're a medical professional, researcher, or just someone interested in this crucial aspect of infant health, this app offers insights and predictions that can help you gain valuable insights.</p>
                   
                   <p>Infant health is a subject of paramount importance, and one vital indicator of a child's well-being is head circumference. Our app delves into the data from a comprehensive dataset of 100 low birthweight infants, born in prestigious Boston teaching hospitals. By utilizing multiple linear regression, we unravel the complex interplay between birth weight, gestational age, sex, and other key factors that contribute to head circumference.</p>
                   
                   <p>Head circumference is an important indicator of brain growth in infants and is routinely measured during
                      well-child visits. Low birth weight infants are at a higher risk for developmental delays and neurological
                      problems, and accurate prediction of their head circumference can aid in early identification of these issues.<p>
                      
                   <p>You can explore the data visually through captivating graphical summaries. Observe how head circumference relates to gestational age, birth weight, and other variables. With just a few clicks, you can generate insightful histograms, scatterplots, and boxplots that highlight patterns and distributions within the data.</p>
             
                   <p>You can also predict head circumference in a low birthweight infant? Our app empowers you to make predictions based on custom inputs. Adjust gestational age, birth weight, and sex, and watch as the app calculates a prediction using a sophisticated regression model. Understand the predictive power of these variables and their combined impact on head circumference.</p>
                   
                   <p>Join us in this exploration of infant health and prediction. We invite you to navigate through descriptive statistics, insightful visualizations, and accurate predictions that will aid medical professionals, researchers, and anyone interested in infant well-being.</h2>
                   <p>Let's embark on a journey of understanding and prediction in the world of low birthweight infants!<p>")),
    # Introduction and Data Description Panel
    tabPanel("Introduction & Data Description",
             HTML("<h2>Abstract</h2>
                  <p>This study explores multiple linear regression to predict head circumference in low birth weight
                  infants using the dataset “Low Birth Weight Infants.” The dataset contains information for a sample of
                  100 low birth weight infants born in two teaching hospitals in Boston, Massachusetts. Predictor variables
                  include birth weight, gestational age, and various demographic and clinical characteristics of the mother.
                  The results indicate that the model has a moderate degree of accuracy in predicting head circumference
                  using birth weight, gestational age, and sex of the child.It also also reveals that there are interactions between birth weight and sex and gestational age and sex.
                  This means that the relationship between head circumference and these factors is not constant, but varies
                  depending on the sex of the child.  With an R-squared value of 0.77, the model is able to
                  explain about 77% of variations in the data.
                  </p>
                  
          
                  
                  <h2>Data Description</h2>
                  <p>The dataset includes the following variables:</p>
                  
                  <ul>
                  <li>Gestational age: measured in weeks</li>
                  <li>Birth weight: measured in grams</li>
                  <li>Length: measured in centimeters</li>
                  <li>Head circumference: measured in centimeters</li>
                  <li>Sex: male or female (0 for female and 1 for male)</li>
                  <li>Toxemia: presence of toxemia, a pregnancy complication</li>
                  <li>Systolic blood pressure (sbp): systolic blood pressure of the mother</li>
                  </ul>")),
    
    # Descriptive Statistics Panel
    tabPanel("Descriptive Statistics",
             h4("Descriptive Statistics"),
             selectInput("summary_type", "Select Summary Type",
                         choices = c("Summary", "Mean", "Median", "Min", "Max")),
             selectInput("variable_of_interest", "Select Variable of Interest",
                         choices = names(low_birth)),
             verbatimTextOutput("summary_output")
    ),
    
    # Visualizations Panel
    tabPanel("Visualizations",
             h4("Graphical Summaries"),
             selectInput("visualization_type", "Select Visualization Type",
                         choices = c("Head Circumference vs. Gestational Age",
                                     "Head Circumference vs. Birth Weight",
                                     "Histogram of Gestational Age",
                                     "Boxplot of Head Circumference",
                                     "Boxplot of Birth Weight",
                                     "Histogram of Mother's Age",
                                     "Gender Distribution",
                                     "Correlation Matrix")),
             plotOutput("selected_plot")
    ),
    
    # Predictions Panel
    tabPanel("Predictions",
             h4("Predicted Head Circumference"),
             sidebarLayout(
               sidebarPanel(
                 numericInput("gestage", "Gestational Age", value = 0),
                 numericInput("birthweight", "Birthweight", value = 0),
                 selectInput("sex", "Sex", c("Male", "Female")),
                 actionButton("predict_button", "Predict")
               ),
               mainPanel(
                 textOutput("prediction_result")
               )
             )
    )
  )
)


server <- function(input, output) {
  observeEvent(input$predict_button, {
    gestage <- input$gestage
    birthweight <- input$birthweight
    sex <- ifelse(input$sex == "Male", 1, 0)
    
    prediction <- 11.1101375 + 
      0.3021018 * gestage + 
      0.0059261 * birthweight - 
      7.8162725 * sex + 
      0.3754163 * (sex * gestage) - 
      0.0025180 * (sex * birthweight)
    
    output$prediction_result <- renderText({
      paste("Predicted Value:", round(prediction, 2))
    })
  })
  
  output$summary_output <- renderPrint({
    variable <- input$variable_of_interest
    
    if (input$summary_type == "Summary") {
      summary(low_birth[, variable])
    } else if (input$summary_type == "Mean") {
      mean(low_birth[, variable])
    } else if (input$summary_type == "Median") {
      median(low_birth[, variable])
    } else if (input$summary_type == "Min") {
      min(low_birth[, variable])
    } else if (input$summary_type == "Max") {
      max(low_birth[, variable])
    }
  })
  
  output$selected_plot <- renderPlot({
    if (input$visualization_type == "Head Circumference vs. Gestational Age") {
      ggplot(low_birth, aes(x = gestage, y = headcirc, color = factor(sex))) +
        geom_point(shape = 19, size = 3) +
        geom_smooth(method = "lm", se = FALSE, aes(group = sex)) +
        scale_color_manual(values = c("1" = "red", "0" = "darkblue"),
                           labels = c("1" = "Male", "0" = "Female")) +
        labs(title = "Head Circumference vs. Gestational Age", x = "Gestational Age", y = "Head Circumference") +
        theme_minimal() +
        theme(legend.title = element_blank())
    }else if (input$visualization_type == "Head Circumference vs. Birth Weight") {
      ggplot(low_birth, aes(x = birthwt, y = headcirc, color = factor(sex))) +
        geom_point(shape = 19, size = 3) +
        geom_smooth(method = "lm", se = FALSE, aes(group = sex)) +
        scale_color_manual(values = c("1" = "red", "0" = "darkblue"),
                           labels = c("1" = "Male", "0" = "Female")) +
        labs(title = "Head Circumference vs. Birth Weight", x = "Birth Weight", y = "Head Circumference") +
        theme_minimal() +
        theme(legend.title = element_blank())
    } else if (input$visualization_type == "Histogram of Gestational Age") {
      plotNormalHistogram(low_birth$gestage, main = "Histogram of Gestational Age")
    } else if (input$visualization_type == "Boxplot of Head Circumference") {
      ggplot(low_birth, aes(x = "Head Circumference", y = headcirc)) +
        geom_boxplot(fill = "#FF9999", color = "#FF9999") +
        labs(title = "Boxplot of Head Circumference", x = NULL, y = "Head Circumference") +
        theme_bw()
    } else if (input$visualization_type == "Boxplot of Birth Weight") {
      ggplot(low_birth, aes(x = "Birth Weight", y = birthwt)) +
        geom_boxplot(fill = "#CCCCCC", color = "#666666") +
        labs(title = "Boxplot of Birth Weight", x = NULL, y = "Birth Weight") +
        theme_bw()
    } else if (input$visualization_type == "Histogram of Mother's Age") {
      plotNormalHistogram(low_birth$momage, main = "Histogram of momage")
    } else if (input$visualization_type == "Gender Distribution") {
      ggplot(low_birth, aes(x = sex, fill = sex)) + 
        geom_bar() +
        xlab("Sex") +
        ylab("Count") +
        ggtitle("Gender Distribution")
    } else if (input$visualization_type == "Correlation Matrix") {
      corr <- cor(low_birth[,-8])
      
      corrplot(corr, method = "number", type = "upper", tl.col = "black", tl.srt = 45,
               addCoef.col = "black", col = colorRampPalette(c("#F7FBFF", "#2171B5"))(50),
               number.cex = 0.7, tl.cex = 0.8, width = 12)
    }
  })
}

shinyApp(ui, server)


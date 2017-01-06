#Predictive text app completed for the JHU data science specialization capstone
# https://www.coursera.org/specializations/jhu-data-science


library(shiny)
library(ggplot2)
library(quanteda)
library(stringr)
library(dplyr)
library(data.table)

predict_text=function(text){
  match_found = FALSE
  data_return <- data.table (stem = character(0), target = character(0), probability =numeric(0))
  #tokenize input text
  text_token<-tokenize(text,what=c("word"), removeNumbers=TRUE, removePunct = TRUE,
                       removeSymbols = TRUE, removeSeparators = TRUE, removeTwitter = TRUE,
                       removeHyphens = TRUE, removeURL = TRUE, concatenator = "_", simplify = FALSE, verbose = TRUE)
  
  text_token = toLower(text_token)
  
  text_token = paste(text_token[[1]],collapse="_")
  cat(text_token)
  
  #get length of text
  word_count<-str_count(text_token,pattern ="_")+1
  
  #Attempt to match a fourgram first
  if (word_count >= 3){
    text_fourgram<-paste0(word(text_token,-3:-1, sep="_"),collapse="_")
    
    #if a fourgram  exists
    if (any(text_fourgram %in% train_fourgram$stem)){
      data_return<-train_fourgram[which(train_fourgram$stem==text_fourgram),]
      data_return[order(probability)]
      match_found = TRUE
    }
    
  }
  
  #Attempt trigram second
  if (word_count >= 2 & match_found == FALSE){
    text_trigram<-paste0(word(text_token,-2:-1, sep="_"),collapse="_")
    
    #if a trigram exists
    if (any(text_trigram %in% train_trigram$stem)){
      data_return<-train_trigram[which(train_trigram$stem==text_trigram),]
      data_return[order(probability)]
      match_found = TRUE
    }
    
  }
  
  #Attempt bigram last
  if (word_count >= 1 & match_found == FALSE){
    text_bigram<-paste0(word(text_token,-1, sep="_"),collapse="_")
    
    #if it exists
    if (any(text_bigram %in% train_bigram$stem)){
      data_return<-train_bigram[which(train_bigram$stem==text_bigram),]
      data_return[order(probability)]
      match_found = TRUE
    }
    
  }
  #sort in order of probability
  data_return$probability = as.integer(data_return$probability*100)
  data_return = data_return[order(probability,decreasing = TRUE),]
  names(data_return) = c("stem","target", "probability")
  data_return
  

}

##Load the processed data used for prediction
load(file = "./data/train_processed_60Set_withstop-TOP10.RData")

#################################################
# Define UI for application that performs predictive text
ui <- shinyUI(fluidPage(
  
  # Application title.
  titlePanel("Predictive Text App"),
  
  # Sidebar with controls to select a dataset and specify the
  # number of observations to view. 
  sidebarLayout(
    sidebarPanel(
      textInput("text", "Input text:", 
                  value = "Enter text"),
      
      numericInput("obs", "Enter max number of text predictions:", value=5, min = 1, max= 10),
      
      helpText("Enter the text to predict and number of desired word predictions."),
      
      submitButton("Predict!")
    ),
    
    # Show a plot of the predicted next word and the stem used to match
    mainPanel(
      #h4("Text Prediction"),
      plotOutput("view"),
      h4("n-gram Match Used For Predictions:"),
      textOutput("ngram")
    )
  )
))

####################################################
# Define server logic required for predictive text
server <- shinyServer(function(input, output) {
  # Return the requested dataset
  textInput <- reactive({
    predict_text(input$text)
  })
  
  # Show the first "n" observations as input by the user
  output$view <- renderPlot({
    
     #plot the word probabilties using ggplot2 as a side bar plot
    ggplot(na.omit(textInput()[1:input$obs,2:3, with=FALSE]), aes(x=reorder(target,probability), y=probability, fill=target)) +
      geom_bar(stat='identity') +
      coord_flip() +
      theme(legend.position="none")+
      xlab("")+
      ylab("Next Word Probability (%)") +
      ggtitle("Text Predictions:") +
      theme(plot.title = element_text(size = rel(3))) +
      theme(axis.title = element_text(size = rel(2))) +
      theme(axis.text = element_text(size = rel(2), face = 'bold'))
      #geom_text(aes(label=target), hjust=1,size=18)
    
  })
  #Show the stem that was matched to the input text
  output$ngram <- renderText({
    textInput()$stem[1]
  })
  
  })

# Run the application 
shinyApp(ui = ui, server = server)


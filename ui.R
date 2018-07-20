library(shiny)

shinyUI(fluidPage(
    titlePanel("Reading Your Mind - A Text Prediction App"),
    sidebarLayout(
        sidebarPanel(
            helpText("Instruction: Type in a few words or a sentence in the Input box -> click on Submit -> 
                     the app will then predict the next word in your mind per Ngram algorism."),
            helpText("You can also visualize other possible words the prediction generated after clicking on Submit."),
            textInput("ip", "Your Input"),
            submitButton("Submit"),
            img(src='NPL.png', align = "bottom", width="100%")
        ),
        
        mainPanel(
            
            h3("Your Input has been transformed to:"),
            textOutput("NewIp"),
            br(),
            br(),
            h3("Predicted Next Word:"),
            div(textOutput("BestGuess"), style = "color:blue;font-size: 30px; font-style: italic"),
            br(),
            h3("Other words that might be in your mind are (size=probability of the predicted word):"),
            plotOutput("wcloud"),
            ##before user type in text, the word cloud viz will not generate anything
            tags$style(type="text/css",
                       ".shiny-output-error { visibility: hidden; }",
                       ".shiny-output-error:before { visibility: hidden; }"
            ),
            br(),
            br(),
            h4("To learn more about N-gram, here are some references:"),
            h4("1. https://en.wikipedia.org/wiki/N-gram"),
            h4("2. https://lagunita.stanford.edu/c4x/Engineering/CS-224N/asset/slp4.pdf"),
            h4("3. http://text-analytics101.rxnlp.com/2014/11/what-are-n-grams.html")
        )
    )
))
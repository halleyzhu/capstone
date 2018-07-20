library(shiny)
library(data.table)
library(NLP)
library(tm)
library(stringr)
library(wordcloud)

#Read in Ngram training data
load("Ngram.RData")
setkeyv(Ngram, c('unigram', 'bigram', 'trigram', 'quagram', 'freq'))

################################## Create functions to clean up user input #######################
TransfromInput <- function(Text){
    NewInput <- gsub("[^\\p{L}\\s]+", "", stripWhitespace(tolower(Text)), ignore.case=F, perl=T)
    return(NewInput)
}

TransfromInputS <- function(Text){
    NewInput <- gsub("[^\\p{L}\\s]+", "", stripWhitespace(tolower(Text)), ignore.case=F, perl=T)
    SplitedInput <- unlist(strsplit(NewInput, " "))
    return(SplitedInput)
}

##################################### Build prediction model #####################################
# Ngram & backoff: Predicting based off of biggest ngram, if failed use max ngram - 1
# If bigram has no match, predicting single word with biggest freq in the traninig data set
PredictText <- function(TextInput, numOfWords=1:3){
    if(numOfWords == 1) {
        NgramsTable <<- Ngram[list("<s>", TextInput[1])]
        NgramsTable <<- NgramsTable[NgramsTable$trigram!="<s>", ]
    }
    else if (numOfWords == 2) {
        NgramsTable <<- Ngram[list("<s>", TextInput[1], TextInput[2])]
        NgramsTable <<- NgramsTable[NgramsTable$quagram!="<s>", ]
    }
    else if (numOfWords == 3) {
        NgramsTable <<- Ngram[list("<s>", TextInput[1], TextInput[2], TextInput[3])]
        NgramsTable <<- NgramsTable[NgramsTable$fivegram!="<s>", ]
    }
    
    NgramsTable <<- NgramsTable[order(NgramsTable$freq, decreasing=TRUE), ]
    

    PossibleWords <<- as.data.frame(NgramsTable)
    if (numOfWords == 1) {
        PossibleWords <<- PossibleWords[1:5, c("trigram", "freq")]
    }
    else if (numOfWords == 2) {
        PossibleWords <<- PossibleWords[1:5, c("quagram", "freq")]
    }
    else if (numOfWords == 3) {
        PossibleWords <<- PossibleWords[1:5, c("fivegram", "freq")]
    }
    PossibleWords <<- PossibleWords[!is.na(PossibleWords$freq), ]
    PossibleWords <<- PossibleWords[!duplicated(PossibleWords), ]
    if(nrow(PossibleWords)==0){
        PossibleWords <<- data.frame(Word=NA, Likelihood=NA)
    }else{
        PossibleWords$freq <- round(PossibleWords$freq/sum(PossibleWords$freq)*100, 1)
        PossibleWords <<- PossibleWords
        colnames(PossibleWords) <<- c("Word", "Likelihood")
        rownames(PossibleWords) <<- NULL
    }
    
    if (numOfWords == 1) {
        PredictedOutput <- NgramsTable$trigram[1]
    }
    else if (numOfWords == 2) {
        PredictedOutput <- NgramsTable$quagram[1]
    }
    else if (numOfWords == 3) {
        PredictedOutput <- NgramsTable$fivegram[1]
    }
    if(is.na(PredictedOutput)|is.null(PredictedOutput)){
        if (numOfWords == 3) {
            Shortened_Input <- c(TextInput[2], TextInput[3])
            PredictedOutput <- PredictText(Shortened_Input, 2)
            if(is.na(PredictedOutput)|is.null(PredictedOutput)){
                PredictedOutput <- PredictText(TextInput[3], 1)
            }
        }
        else if (numOfWords == 2) {
            PredictedOutput <- PredictText(TextInput[2], 1)
        }
        else if (numOfWords == 1) {
            PredictedOutput <- "Sorry. Please check your input and make sure the spelling is correct."
        }
    }
    
    return(PredictedOutput)
}
##################################################END OF THE model#####################################

##################################################SERVER.R#############################################
shinyServer(function(input, output) {    

    # display the tranformed input
    output$NewIp <- renderText({
        OInput <- input$ip
        NewIp_Input <- TransfromInput(OInput)
        return(NewIp_Input)
    })
    
    # display the best predicted word 
    output$BestGuess <- renderText({
        OInput <- input$ip
        NewIp_Input <- TransfromInput(OInput)
        BestPredictedOutput <- "."
        SplitedInput <- TransfromInputS(OInput)
        wordcnt <- length(SplitedInput)
        
        if (wordcnt <= 3 && wordcnt >= 1) {
            BestPredictedOutput = PredictText(SplitedInput, wordcnt)
        }
        else if (wordcnt > 3) {
            LookUpWords <- c(SplitedInput[wordcnt - 2],
                                 SplitedInput[wordcnt - 1],
                                 SplitedInput[wordcnt])
            BestPredictedOutput <- PredictText(LookUpWords, 3)
        }
        
        return(BestPredictedOutput)
    })
    
    #visulize world cloud of possible words the model produced
    output$wcloud <- renderPlot({
        OInput <- input$ip
        SplitedInput <- TransfromInputS(OInput)
        wordcnt <- length(SplitedInput)
        if (wordcnt <= 3 && wordcnt >= 1) {
            BestPredictedOutput = PredictText(SplitedInput, wordcnt)
        }
        else if (wordcnt > 3) {
            LookUpWords <- c(SplitedInput[wordcnt - 2],
                                 SplitedInput[wordcnt - 1],
                                 SplitedInput[wordcnt])
            BestPredictedOutput <- PredictText(LookUpWords, 3)
        }
        if(exists("PossibleWords", where = -1)){
            PossibleWords
        }else{
            XNgramsTable <- data.frame(Word=NA, Likelihood=NA)
        }
        
        PossibleWords[,"dup"] <- ifelse(duplicated(PossibleWords$Word), TRUE, FALSE)
        DisplayTable <- PossibleWords[!PossibleWords$dup, c("Word", "Likelihood")]
        
        wordcloud(DisplayTable$Word, DisplayTable$Likelihood, scale=c(6,.6), min.freq=0, max.words=Inf,
                  random.order=FALSE, random.color=FALSE,
                  rot.per=.1, colors=brewer.pal(8, "Dark2"), use.r.layout=FALSE, fixed.asp=TRUE)
    })
})
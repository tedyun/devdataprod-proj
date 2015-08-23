library(shiny)
library(ggplot2)

#countryNames <- unique(factbookData$Country)
#countryNames <- as.vector(countryNames)

# remove last four group stats
#countryNames <- countryNames[0:(length(countryNames) - 4)]

statKeys <- c(
    "SIZEGDP_T2",
    "EVOPOP_T1",
    "HOUSSAVE_T1",
    "WELECGEN_T1",
    "HOURSWKD_T1"
)
statNames <- c(
    "GDP Per Capita",
    "Population",
    "Household Net Saving Rates",
    "Electricity Generation",
    "Average Hours Worked"
)

excludeCountries <- c(
    "OECD - Total",
    "OECD - Average",
    "Euro area (17 countries)",
    "European Union (28 countries)"
)

factbookData <- read.csv("FACTBOOK2014_2012.csv", header=TRUE)
names(factbookData)[1] <- "SUB"
factbookData <- factbookData[-which(factbookData$Country %in% excludeCountries),]

# sample k integer from 0 to n-1
sampleIntegers <- function(n, k){
    return(sample(1:n, k, replace = FALSE))
}

getValue <- function(countryName, statKey) {
    row <- factbookData[(factbookData$Country==countryName) & (factbookData$SUB == statKey),]
    if (nrow(row)) {
        return(row$Value[1])
    } else {
        return(NA)
    }
}

generateQuestionData <- function (statKey) {
    rows <- factbookData[(factbookData$SUB == statKey),]
    indices <- sampleIntegers(nrow(rows), 2)
    result1 <- list()
    index1 <- indices[1]
    result1$country <- as.vector(rows$Country[index1])
    result1$value <- rows$Value[index1]
    
    result2 <- list()
    index2 <- indices[2]
    result2$country <- as.vector(rows$Country[index2])
    result2$value <- rows$Value[index2]
    
    result <- list(result1, result2)
    
    unit <- rows$Unit[index1]
    powercode <- rows$PowerCode[index1]
    if (powercode != "units") {
        unit <- paste(powercode, unit)
    }
    
    result$unit <- unit
    
    result$answer <- if (result1$value > result2$value) "opt1"
                     else if (result1$value < result2$value) "opt2"
                     else "unknown"
    return(
        if (result$answer != "unknown") result
        else generateQuestionData(statKey)
    )
}

generateQuestionText <- function (qIndex) {
    return(paste("Which of the following two countries had more(larger) ", statNames[qIndex],
                 " in 2012?", sep=""))
}

getRandomStatIndex <- function () {
    return(sampleIntegers(length(statKeys), 1))
}

getStatKey <- function (qIndex) {
    return(statKeys[qIndex])
}

generateAnswerText <- function (questionData) {
    country1 <- questionData[[1]]$country
    country2 <- questionData[[2]]$country
    value1 <- questionData[[1]]$value
    value2 <- questionData[[2]]$value
    unit <- questionData$unit
    result <- paste(country1, ": ", value1, ", ", country2, ": ",
                    value2, " (", unit, ").", sep="")
    return(result)
}

shinyServer(
    function(input, output) {
        qIndex <- getRandomStatIndex()
        output$questionText <- renderText(generateQuestionText(qIndex))
        questionData <- generateQuestionData(getStatKey(qIndex))
        country1 <- questionData[[1]]$country
        country2 <- questionData[[2]]$country
        value1 <- questionData[[1]]$value
        value2 <- questionData[[2]]$value
        output$optionText1 <- renderText(country1)
        output$optionText2 <- renderText(country2)
        output$checkAnswer <- renderText({ 
            if (length(input$answer) == 0)
                ""
            else if (input$answer == questionData$answer)
                "Correct."
            else
                "Incorrect."
        })
        output$answerText <- renderText({ 
            if (length(input$answer) == 0)
                ""
            else
                generateAnswerText(questionData)
        })
        output$answerPlot <- renderPlot({
            xValues <- c(country1, country2)
            yValues <- c(value1, value2)
            if (length(input$answer) == 0)
                NULL
            else
                qplot(xValues, yValues, geom="bar",
                      main=statNames[qIndex], stat="identity",
                      xlab="", ylab=questionData$unit) +
                theme(
                    panel.background = element_rect(fill = "transparent",colour = NA),
                    panel.grid.minor = element_blank(), 
                    panel.grid.major = element_blank(),
                    plot.background = element_rect(fill = "transparent",colour = NA)
                )
        }, width=400, height=300, bg="transparent")
    }
)

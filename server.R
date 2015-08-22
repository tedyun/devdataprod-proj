library("shiny")

factbookData <- read.csv("FACTBOOK2014_2012.csv", header=TRUE)
names(factbookData)[1] <- "SUB"
countryNames <- unique(factbookData$Country)
countryNames <- as.vector(countryNames)
# remove last four group stats
countryNames <- countryNames[0:(length(countryNames) - 4)]

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
    return(result)
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
        output$optionText1 <- renderText(questionData[[1]]$country)
        output$optionText2 <- renderText(questionData[[2]]$country)
        output$answerText <- renderText(generateAnswerText(questionData))
    }
)
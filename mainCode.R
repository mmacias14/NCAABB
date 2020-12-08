library(shiny)
library(DT)
library(readxl)
library(magrittr)
library(xml2)
library(rvest)
library(janitor)
library(stringr)
library(caret)
library(MASS)
library(promises)
library(future)
library(gam)

# model <- function(){
#Set Working Directory with Excel file to convert team names

setwd("~/NCAA Basketball/NCAA Basketball 2020-2021")

#Date Inputs

#User input for game date range

startDate <- "2020-11-25"
endDate <- Sys.Date() + 2

#User input for stat date

statDate <- Sys.Date()

#Stat Scrape

options(java.parameters = "-Xmx1000m")

page<- c("offensive-efficiency",
         "defensive-efficiency",
         "three-point-pct",
         "two-point-pct",
         "free-throw-pct",
         "turnovers-per-possession",
         "offensive-rebounding-pct",
         "defensive-rebounding-pct",
         "points-per-game",
         "floor-percentage",
         "three-pointers-made-per-game",
         "free-throws-made-per-game",
         "total-rebounds-per-game",
         "total-rebounding-percentage",
         "blocks-per-game",
         "steals-per-game",
         "block-pct",
         "steals-perpossession",
         "assists-per-game",
         "turnovers-per-game",
         "personal-fouls-per-possession",
         "possessions-per-game",
         "extra-chances-per-game",
         "effective-possession-ratio",
         "opponent-effective-possession-ratio",
         "win-pct-all-games",
         "schedule-strength-by-other",
         "predictive-by-other")


statsDF <- list()

dates <- seq(as.Date(startDate), as.Date(endDate), "day") %>% format("%Y-%m-%d")

i <- which(dates == statDate)

for(j in 1:length(page)){

  #Stat scrape, Strength of schedule scrape if max(j)
  if(j == length(page)-1){
    url <- paste0("https://www.teamrankings.com/ncaa-basketball/ranking/",page[j],"?date=",dates[i])

    data <- url %>%
      read_html() %>%
      html_table(fill=T)

    data <- data[[1]]

    data$Team <- as.factor(str_sub(sub("(.*)\\(.*)\\.*", "\\1", data$Team),end =-2))

    dataSOS <- data.frame(cbind(rep(dates[i],nrow(data)),as.character(data$Team),data$Rating))

    colnames(dataSOS) <- c("Date", "Team", page[j])

  }
  else if(j == length(page)){
    url <- paste0("https://www.teamrankings.com/ncaa-basketball/ranking/",page[j],"?date=",dates[i])

    data <- url %>%
      read_html() %>%
      html_table(fill=T)

    data <- data[[1]]

    data$Team <- as.factor(str_sub(sub("(.*)\\(.*)\\.*", "\\1", data$Team),end =-2))

    dataRat <- data.frame(cbind(rep(dates[i],nrow(data)),as.character(data$Team),data$Rating))

    colnames(dataRat) <- c("Date", "Team", page[j])
  }
  else{
    url<- paste0("https://www.teamrankings.com/ncaa-basketball/stat/",page[j],"?date=",dates[i])

    data <- url %>%
      read_html() %>%
      html_table(fill=T) %>%
      as.data.frame()

    dataHome <- cbind(rep(dates[i],nrow(data)),data$Team,data$X2019,data$Home) %>%
      as.data.frame()

    dataAway <- cbind(rep(dates[i],nrow(data)),data$Team,data$X2019,data$Away) %>%
      as.data.frame()

    colnames(dataHome) <- c("Date", "Team", page[j],paste0(page[j],".Home"))
    colnames(dataAway) <- c("Date", "Team", page[j],paste0(page[j],".Away"))
  }

  if(j == 1){
    statsDF[[i]] <- dataHome
    statsDF[[i+1]] <- dataAway
  } else if(j == length(page)-1){
    statsDF[[i]] <- merge(statsDF[[i]], dataSOS, by.x = c("Date", "Team"), by.y = c("Date", "Team")) %>%
      as.data.frame()
    statsDF[[i+1]] <- merge(statsDF[[i+1]], dataSOS, by.x = c("Date", "Team"), by.y = c("Date", "Team")) %>%
      as.data.frame()
  } else if(j == length(page)){
    statsDF[[i]] <- merge(statsDF[[i]], dataRat, by.x = c("Date", "Team"), by.y = c("Date", "Team")) %>%
      as.data.frame()
    statsDF[[i+1]] <- merge(statsDF[[i+1]], dataRat, by.x = c("Date", "Team"), by.y = c("Date", "Team")) %>%
      as.data.frame()
  }
  else {
    statsDF[[i]] <- merge(statsDF[[i]], dataHome, by.x = c("Date", "Team"), by.y = c("Date", "Team")) %>%
      as.data.frame()
    statsDF[[i+1]] <- merge(statsDF[[i+1]], dataAway, by.x = c("Date", "Team"), by.y = c("Date", "Team")) %>%
      as.data.frame()
  }
}



#Scores Scrape

#Function to scape scores
ncaabScoreScraper <- function(x, y) {

  StartDate <- x
  EndDate <- y
  dates <- seq(as.Date(StartDate), as.Date(EndDate), "day") %>% format("%Y-%m-%d")
  df <- data.frame()

  ##Create loop for cycling through pages

  for(date in dates){

    tryCatch({

      split <- unlist(strsplit(date, "-"))
      year <- split[1]
      month <- split[2]
      day <- split[3]

      Page <- paste("https://www.sports-reference.com/cbb/boxscores/index.cgi?month=",month,"&day=",day,"&year=",year, sep="")
      ReadPage <- read_html(Page)
      NumGames <- length(ReadPage %>% html_nodes(".teams"))

      for(game in 1:NumGames){

        tryCatch({

          HomeTeamName <- ReadPage %>% html_nodes(paste(".nohover:nth-child(", game + 1, ") tr:nth-child(2) td:nth-child(1) a")) %>% html_text()
          HomeTeamName <- as.character(HomeTeamName)
          AwayTeamName <- ReadPage %>% html_nodes(paste(".nohover:nth-child(", game + 1, ") tr:nth-child(1) td:nth-child(1) a")) %>% html_text()
          AwayTeamName <- as.character(AwayTeamName)
          HomeTeamScore <- ReadPage %>% html_nodes(paste(".nohover:nth-child(", game + 1, ") tr:nth-child(2) td:nth-child(2)")) %>% html_text()
          HomeTeamScore <- as.numeric(HomeTeamScore)
          AwayTeamScore <- ReadPage %>% html_nodes(paste(".nohover:nth-child(", game + 1, ") tr:nth-child(1) td:nth-child(2)")) %>% html_text()
          AwayTeamScore <- as.numeric(AwayTeamScore)
          #GameDate <- ReadPage %>% html_nodes(".current strong") %>% html_text()
          GameDate <- date
          StatDate <- date

          tmp_df <- data.frame(GameDate, StatDate, HomeTeamName, HomeTeamScore, AwayTeamName, AwayTeamScore)
          tmp_df[3:6] <- tmp_df[3:6] %>%
            dplyr::mutate_if(is.factor, as.character)

          df <- dplyr::bind_rows(df, tmp_df)}, error = function(e) print(e))

        assign("scores", df, envir=globalenv())
      }}, error = function(e) print(e))
  }
}



#Call function to Scrape Scores. Data saved in "scores" DF
ncaabScoreScraper(startDate, endDate)


#Build Data Table

#Convert SportsReference names to TeamRankings names by reading in Team Names Table from Excel
teamNames <- data.frame(read_xlsx("teamNamesSR.xlsx"))


#Make scores data table with only TeamRankings teams. Data saved in "scoresTR" DF
new <- scores
new[] <- teamNames$TeamRankingsName[match(unlist(scores), teamNames$SportsReferenceName)]
scoresTR <- cbind(scores$GameDate,scores$StatDate, new$HomeTeamName,scores$HomeTeamScore,new$AwayTeamName,scores$AwayTeamScore) %>%
  as.data.frame()
colnames(scoresTR) <- c("Date.Game", "Date.Stat","Home","Home.Points","Away","Away.Points")
scoresTR <- scoresTR %>%
  dplyr::mutate_if(is.factor, as.character)


#Include Alternate Home, Away Scenario for Post Season Games

# tournamentCorrection <- scoresTR[scoresTR[,1] == "2019-03-21",]
# tournamentCorrection <- rbind(tournamentCorrection, scoresTR[scoresTR[,1] == "2019-03-22",])
#
# tournamentCorrection[,c(3,5)] <- tournamentCorrection[,c(5,3)]
#
# scoresTR <- rbind(scoresTR,tournamentCorrection)


#Bulild Table with all data

masterData <- data.frame()

Date.Stat <- rep(dates[i], nrow(scoresTR))

scoresTR$Date.Stat <- Date.Stat

dataTableHome <- merge(scoresTR, statsDF[[i]], by.x = c("Date.Stat", "Home"), by.y = c("Date", "Team"))
dataTable <- merge(dataTableHome, statsDF[[i+1]], by.x = c("Date.Stat", "Away"), by.y = c("Date", "Team"))
masterData <- rbind(masterData,dataTable)

Home.Win <- ifelse(masterData$Home.Points > masterData$Away.Points,1,0)
Score.Diff <- as.numeric(masterData$Home.Points) - as.numeric(masterData$Away.Points)
Total.Pts <- as.numeric(masterData$Home.Points) + as.numeric(masterData$Away.Points)

masterData <- data.frame(masterData,Home.Win,Score.Diff,Total.Pts)
masterData <- masterData[,c(4,1,3,2,5,6,(ncol(masterData)-2):(ncol(masterData)),7:(ncol(masterData)-3))]


#Build Table with only numeric data
analyzeData <- masterData[,c(7:ncol(masterData))]
analyzeData <- data.frame(apply(analyzeData, 2, function(x)
  as.numeric(sub("%","",as.character(x)))))
predictData <- masterData[ is.na(masterData$Score.Diff) == T,]
analyzeData$Home.Win <- as.factor(analyzeData$Home.Win)


#Linear Models

#Removed outliers
# analyzeData <- analyzeData[(rownames(analyzeData)=="2054")==F,]
# masterData <- masterData[(rownames(masterData)=="2054")==F,]

analyzeData <- analyzeData[,is.na(str_match(colnames(analyzeData),".Home")==T)]
analyzeData <- analyzeData[,is.na(str_match(colnames(analyzeData),".Away")==T)]

#Build Linear models
# score.diff.lm <- lm(Score.Diff~.-Home.Win - Total.Pts, data = analyzeData)
# total.pts.lm <- lm(Total.Pts~.-Score.Diff - Home.Win, data = analyzeData)
#win.loss.glm <- gam(Home.Win~.-Score.Diff -Total.Pts, data = na.omit(analyzeData), family = binomial)
#analyzeData.naomit <- na.omit(analyzeData)
#win.loss.glm <- glmnet(analyzeData.naomit[,-c(1,2,3)], analyzeData.naomit$Home.Win, family = "binomial")

total.pts.gam <- gam(
  as.formula(
    paste0(
      "Total.Pts ~ s(", 
      setdiff(names(analyzeData),c("Home.Win","Score.Diff","Total.Pts")) %>% paste0(collapse = ") + s("),
      ")"
    )
  ),
  data = analyzeData, family = gaussian
)

score.diff.gam <- gam(
  as.formula(
    paste0(
      "Score.Diff ~ s(", 
      setdiff(names(analyzeData),c("Home.Win","Score.Diff","Total.Pts")) %>% paste0(collapse = ") + s("),
      ")"
    )
  ),
  data = analyzeData, family = gaussian
)

win.loss.gam <- gam(
  as.formula(
    paste0(
      "Home.Win ~ s(", 
      setdiff(names(analyzeData),c("Home.Win","Score.Diff","Total.Pts")) %>% paste0(collapse = ") + s("),
      ")"
    )
  ),
  data = analyzeData, family = binomial
)

###Reduce number of predictors using Stepwise AIC
# score.diff.stepAIC.model <- stepAIC(score.diff.lm, trace = F)
# total.pts.stepAIC.model <- stepAIC(total.pts.lm, trace = F)
# win.loss.stepAIC.model <- stepAIC(win.loss.glm, trace = F)

terms <- list()

for(i in setdiff(names(analyzeData),c("Home.Win","Score.Diff","Total.Pts"))){
  terms[[i]] <- c("1", paste0("s(",i,")"))
}

score.diff.stepAIC.model <- step.Gam(score.diff.gam,scope = terms, trace = F, steps = 10)
total.pts.stepAIC.model <- step.Gam(total.pts.gam, scope = terms, trace = F, steps = 10)
win.loss.stepAIC.model <- step.Gam(win.loss.gam, scope = terms, trace = F, steps = 10)

#Model assumption diagnostics for regresion models
# opar <- par(mfrow = c(2,2), oma = c(1, 1, 1, 1))
# plot(score.diff.stepAIC.model, las = 1)
# par(opar)
#
# opar <- par(mfrow = c(2,2), oma = c(1, 1, 1, 1))
# plot(total.pts.stepAIC.model, las = 1)
# par(opar)


#Subset analyzed data to use as model inputs
analyzeData.predictors <- data.frame(apply(masterData, 2, function(x)
  as.numeric(sub("%","",as.character(x)))))
analyzeData.predictors <- analyzeData.predictors[,c(10:ncol(analyzeData.predictors))]


#Get yhat values for score.diff
score.diff.stepAIC.variables <- substr(names(score.diff.stepAIC.model$coefficients),3,str_length(names(score.diff.stepAIC.model$coefficients))-1)[2:length(score.diff.stepAIC.model$coefficients)]
analyzeData.stepAIC.predictors <- analyzeData.predictors[,score.diff.stepAIC.variables]
score.diff.stepAIC.predict <- -1*(round(predict(score.diff.stepAIC.model, analyzeData.stepAIC.predictors)*2)/2)

masterData <- cbind(score.diff.stepAIC.predict, masterData)
colnames(masterData)[1] <- "Predicted.Home.Spread.lm"


#Get yhat values for total.pts
total.pts.stepAIC.variables <- substr(names(total.pts.stepAIC.model$coefficients),3,str_length(names(total.pts.stepAIC.model$coefficients))-1)[2:length(total.pts.stepAIC.model$coefficients)]
analyzeData.stepAIC.predictors <- analyzeData.predictors[,total.pts.stepAIC.variables]
total.pts.stepAIC.predict <- round(predict(total.pts.stepAIC.model, analyzeData.stepAIC.predictors)*2)/2

masterData <- cbind(total.pts.stepAIC.predict, masterData)
colnames(masterData)[1] <- "Predicted.Total.lm"


#Get yhat values for win.loss
win.loss.stepAIC.variables <- substr(names(win.loss.stepAIC.model$coefficients),3,str_length(names(win.loss.stepAIC.model$coefficients))-1)[2:length(win.loss.stepAIC.model$coefficients)]
analyzeData.stepAIC.predictors <- analyzeData.predictors[,win.loss.stepAIC.variables]
win.loss.stepAIC.predict.all <- round(predict(win.loss.stepAIC.model, analyzeData.stepAIC.predictors, type = "response"),3)

win.loss.stepAIC.predict <- ifelse(win.loss.stepAIC.model$fitted.values <= 0.5,0,1)
confusion.matrix.gam <- confusionMatrix(as.factor(win.loss.stepAIC.predict),as.factor(na.omit(analyzeData)[,"Home.Win"]))

masterData <- cbind(win.loss.stepAIC.predict.all, masterData)
colnames(masterData)[1] <- "Prob.Home.Win.glm"
winner <- ifelse(masterData[,1] > 0.5, masterData$Home, masterData$Away)


#Implied Money Line Calculations
winner.prob <- ifelse(masterData[,1]  > 0.5, masterData[,1], 1-masterData[,1])

min.acceptable.payout <- 1/winner.prob - 1
min.acceptable.ml <- 5*round(round(ifelse(min.acceptable.payout >= 1, min.acceptable.payout*100, -100/min.acceptable.payout))/5)

masterData <- cbind(winner,winner.prob,min.acceptable.ml,masterData)
colnames(masterData)[1:3] <- c("Pred.Winner","Prob.Win.glm","Implied.Money.Line")


#App Data

#Subset Master Data for App
appData <- masterData[,c("Date.Game", "Home", "Away", "Pred.Winner", "Prob.Win.glm", "Predicted.Home.Spread.lm", "Predicted.Total.lm")]
colnames(appData) <- c("Date", "Home", "Away", "Winner", "Prob" ,"Home Spread", "Total Pts")
saveRDS(appData, "appDataMaster.rds")

todayGameData <- appData[appData$Date == Sys.Date(),]
saveRDS(todayGameData,"appDataToday.rds")

#Model Accuracy Metrics
confusion.matrix.gam
rmse.score.diff <- sqrt(mean(score.diff.stepAIC.model$residuals^2))
rmse.total.pts <- sqrt(mean(total.pts.stepAIC.model$residuals^2))

metrics <- data.frame(confusion.matrix.gam$overall[1],rmse.score.diff,rmse.total.pts)

saveRDS(metrics, "metrics.rds")
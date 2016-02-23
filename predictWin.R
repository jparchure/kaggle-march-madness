library("RSQLite")
library("stringr")
library("plyr")
library("data.table")
sqlite <- dbDriver("SQLite")
conn <- dbConnect(sqlite,"database.sqlite")
alltables <- dbListTables(conn)
winnTeamsQuery<- "select TourneyCompactResults.*, Teams.Team_Name from TourneyCompactResults inner join Teams on Teams.Team_id = TourneyCompactResults.Wteam where TourneyCompactResults.Daynum=154"
tourneySeedsQuery <- "select TourneySeeds.*, Teams.Team_Name from Teams inner join TourneySeeds on  Teams.team_id = TourneySeeds.Team order by TourneySeeds.Season, TourneySeeds.Seed, TourneySeeds.Team"
seasonResultsQuery <- "select * from RegularSeasonCompactResults"
teamListQuery <- "select * from Teams"
seasonwinsquery <- "select Wteam, Season, COUNT(*) from RegularSeasonCompactResults  GROUP BY Wteam, Season"
generateTeamwiseResultsQuery<- "Select Season, Daynum,Wteam, Wscore, Lteam, Lscore, Wloc, Numot From RegularSeasonCompactResults Union Select Season, Daynum,Lteam, Lscore, Wteam, Wscore,Wloc, Numot From RegularSeasonCompactResults"
teamwiseResults<-dbGetQuery(conn, generateTeamwiseResultsQuery)
noofwins <- dbGetQuery(conn, seasonwinsquery) #No of wins per season for all teams
tourneyWinners<- dbGetQuery(conn,winnTeamsQuery)
tourneySeeds <- dbGetQuery(conn,tourneySeedsQuery)
seasonResults<- dbGetQuery(conn,seasonResultsQuery)
teamList<- dbGetQuery(conn,teamListQuery)
extractSeedNums<- function(seedsTable, colname){
  #Function to help separate Seed numbers and Seed Zone
  regexpnum <- "[[:digit:]]+"
  regexptext<- "[[A-Z]]"
  outputnumlist <- str_extract_all(seedsTable[colname],regexpnum)
  outputtextlist<- str_extract_all(seedsTable[colname],regexptext)
  seedsTable["SeedNum"] <- outputnumlist
  seedsTable["SeedZone"]<- outputtextlist
  seedsTable["SeedNum"] <- sapply(seedsTable["SeedNum"], as.numeric)
  return(seedsTable)
  
}

tourneySeeds<- extractSeedNums(tourneySeeds, "Seed")
head(tourneySeeds)
teamwiseResults<- rename(teamwiseResults, c("Wteam" = "Team1","Wscore" = "Score1" ,"Lteam" = "Team2","Lscore"="Score2"))

teamwiseResults["Result"] <- ifelse(teamwiseResults$Score1 > teamwiseResults$Score2, "W","L")
teamwiseResults["Differential"] <- teamwiseResults$Score1 - teamwiseResults$Score2 #Calculate differential


teamwiseResults["Location"]<- ifelse(teamwiseResults$Result=="L", ifelse(teamwiseResults$Wloc=="H","A",ifelse(teamwiseResults$Wloc=="A","H","N")), teamwiseResults$Wloc) #sapply(teamwiseResults,getcourtlocation)
head(teamwiseResults)

winningSeeds <- merge(tourneyWinners, tourneySeeds,by.x = c("Season", "Wteam"), by.y=c("Season", "Team")) #Get the Seeds of Tournament Winners


noOfWinsChampions <- merge(noofwins, tourneyWinners, by=c("Wteam","Season")) #No. of Wins in regular season by the eventual champion


differential <- aggregate(teamwiseResults$Differential, by=list(Season = teamwiseResults$Season, Team = teamwiseResults$Team1),FUN=mean)

loser <- teamwiseResults[teamwiseResults$Result == "L",]
winner <- teamwiseResults[teamwiseResults$Result == "W",]
home<- teamwiseResults[teamwiseResults$Location == "H",]
away<- teamwiseResults[teamwiseResults$Location == "A",]
neutral<- teamwiseResults[teamwiseResults$Location == "N",]
differentialW <- aggregate(victor$Differential, by=list(Season = victor$Season, Team = victor$Team1),FUN=mean)
differentialL <- aggregate(loser$Differential, by=list(Season = loser$Season, Team = loser$Team1),FUN=mean)
differentialH <- aggregate(home$Differential, by=list(Season = home$Season, Team = home$Team1),FUN=mean)
differentialA <- aggregate(away$Differential, by=list(Season = away$Season, Team = away$Team1),FUN=mean)
differentialN <- aggregate(neutral$Differential, by=list(Season = neutral$Season, Team = neutral$Team1),FUN=mean)

differential<- merge(differential, differentialL, by=c("Season", "Team"))
differential<- merge(differential, differentialW, by=c("Season", "Team"))
differential<- rename(differential, replace=c("x.x" = "Average Overall", "x.y" = "Average Losses", "x"="Average Wins"))
differential<- merge(differential, differentialH, by=c("Season", "Team"))
differential<- rename(differential, replace=c("x"="Average Home"))
differential<- merge(differential, differentialA, by=c("Season", "Team"))
differential<- rename(differential, replace=c("x" = "Average Away"))
differential<- merge(differential, differentialN, by=c("Season", "Team"))
differential<- rename(differential, replace=c("x" = "Average Neutral"))

head(differential)

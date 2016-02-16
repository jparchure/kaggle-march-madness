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
teamwiseResults<- setnames(teamwiseResults, old=c("Wteam","Wscore","Lteam","Lscore"), new=c("Team1", "Score1", "Team2","Score2"))

teamwiseResults["Result"] <- ifelse(teamwiseResults$Score1 > teamwiseResults$Score2, "W","L")
teamwiseResults["Differential"] <- teamwiseResults$Score1 - teamwiseResults$Score2 #Calculate differential


teamwiseResults["Location"]<- ifelse(teamwiseResults$Result=="L", ifelse(teamwiseResults$Wloc=="H","A",ifelse(teamwiseResults$Wloc=="A","H","N")), teamwiseResults$Wloc) #sapply(teamwiseResults,getcourtlocation)
head(teamwiseResults)

winningSeeds <- merge(tourneyWinners, tourneySeeds,by.x = c("Season", "Wteam"), by.y=c("Season", "Team")) #Get the Seeds of Tournament Winners


noOfWinsChampions <- merge(noofwins, tourneyWinners, by=c("Wteam","Season")) #No. of Wins in regular season by the eventual champion





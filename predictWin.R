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
tourneyWinners<- dbGetQuery(conn,winnTeamsQuery)
tourneySeeds <- dbGetQuery(conn,tourneySeedsQuery)
seasonResults<- dbGetQuery(conn,seasonResultsQuery)
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

winningSeeds <- merge(tourneyWinners, tourneySeeds,by.x = c("Season", "Wteam"), by.y=c("Season", "Team")) #Get the Seeds of Tournament Winners
seasonwinsquery <- "select Wteam, Season, COUNT(*) from RegularSeasonCompactResults  GROUP BY Wteam, Season"
noofwins <- dbGetQuery(conn, seasonwinsquery)
noOfWinsChampions <- merge(noofwins, tourneyWinners, by=c("Wteam","Season")) #No. of Wins in regular season by the eventual champion
noOfWinsChampions["Differential"] <- noOfWinsChampions["Wscore"]- noOfWinsChampions["Lscore"]

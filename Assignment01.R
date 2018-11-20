###### Exercises
### WHO Dataset
WHO = read.csv("WHO.csv")

# Population of Malaysia
WHO.Malaysia <- subset(WHO, Country == "Malaysia")
WHO.Malaysia$Population

#1.d Country with the lowest literacy
WHO$Country [which.min(WHO$LiteracyRate)]

#1.e Richest country in Europe based on GNI
WHO.Europe = subset(WHO, Region == "Europe")
WHO.Europe$Country [which.max(WHO.Europe$GNI)]

#1.f Mean Life expectancy of countries in Africa
WHO.Africa = subset(WHO, Region == "Africa")
mean(WHO.Africa$LifeExpectancy)

#1.g Number of countries with population greater than 10,000
morethan10000 <- WHO$Population > 10
sum(morethan10000)
##OR
morethan10M <- WHO$Population > 10000
sum(morethan10M)

#1.h Top 5 countries in the Americas with the highest child mortality 
WHO.Americas = subset(WHO, Region == "Americas")
Americas.ChildMortality.SortedDesc <- WHO.Americas$Country[order(-WHO.Americas$ChildMortality)]
Americas.ChildMortality.SortedDesc[1:5]

#######################
### NBA dataset (Historical NBA Performance.xlsx)
NBA <- read.csv("Historical NBA Performance.csv")

#2.a The year Bulls has the highest winning percentage
NBA.Bulls <- subset(NBA, Team == "Bulls")
NBA.Bulls$Year[which.max(NBA.Bulls$Winning.Percentage)]

#2.b Teams with an even win-loss record in a year 
NBA.evenwinloss <- subset(NBA, Winning.Percentage == "0.5")
unique(NBA.evenwinloss$Team)

#######################
### Seasons_Statsinsta.csv  
SeasonStats <- read.csv("Seasons_Stats.csv")

#3.a Player with the highest 3-pt attempt rate in a season.  
SeasonStats$Player[which.max(SeasonStats$X3PA)]

#3.b Player with the highest free throw rate in a season.
SeasonStats$Player[which.max(SeasonStats$FTr)]

#3.c What year/season does Lebron James scored the highest?
SeasonStats.LeBron <- subset(SeasonStats, Player == "LeBron James")
SeasonStats.LeBron$Year[which.max(SeasonStats.LeBron$PTS)]

#3.d What year/season does Michael Jordan scored the highest?
SeasonStats.MichaelJordan <- subset(SeasonStats, Player == "Michael Jordan*")
SeasonStats.MichaelJordan$Year[which.max(SeasonStats.MichaelJordan$PTS)]

#3.e Player efficiency rating of Kobe Bryant in the year where his MP is the lowest? 
SeasonStats.KobeBryant <- subset(SeasonStats, Player == "Kobe Bryant")
SeasonStats.KobeBryant$PER[which.min(SeasonStats.KobeBryant$MP)]

#######################
### National Universities Rankings.csv 
NatlUniRankings <- read.csv("National Universities Rankings.csv")

#4.a University with the most number of undergrads 
NatlUniRankings$Name[which.max(gsub(",", "", NatlUniRankings$Undergrad.Enrollment, fixed = TRUE))]

#4.b Average Tuition in the Top 10 University
TopNatlUnivByTuition <- NatlUniRankings$Tuition.and.fees[order(NatlUniRankings$Rank)]
TopNatlUnivByTuition.Top10 <- TopNatlUnivByTuition[1:10]
TopNatlUnivByTuition.Top10.as.numeric <- as.numeric(gsub("$", "", gsub(",", "", TopNatlUnivByTuition.Top10, fixed = TRUE), fixed = TRUE))
mean(TopNatlUnivByTuition.Top10.as.numeric)

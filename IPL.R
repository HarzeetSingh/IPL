library(readxl)
library(dplyr)
#Required datasets
ballbyballFile=read_excel('D:/datasets/casestudy/Data Set for Case study/Ball_by_Ball.xlsx') 
MatchFile=read_excel('D:/datasets/casestudy/Data Set for Case study/Match.xlsx')
PlayerFile=read_excel('D:/datasets/casestudy/Data Set for Case study/Player.xlsx')
Player_MatchFile=read_excel('D:/datasets/casestudy/Data Set for Case study/Player_Match.xlsx')
Season_File=read_excel('D:/datasets/casestudy/Data Set for Case study/Season.xlsx')
TeamFile=read_excel('D:/datasets/casestudy/Data Set for Case study/Team.xlsx')

#Q1Top 10 batsman
#a Based on runs scored
runsscored=merge(x=ballbyballFile[,c("Striker_Id","Batsman_Scored")],y=PlayerFile[,c("Player_Id","Player_Name")],by.x = "Striker_Id",by.y = "Player_Id",all.x = TRUE)
Q1a=runsscored %>% group_by(Player_Name) %>% summarise(TopScore=sum(as.integer(Batsman_Scored),na.rm = T)) %>% arrange(-TopScore) %>% head(10)

Q1a

#b Based on number of sixes
noofsixes=merge(x=ballbyballFile[,c("Striker_Id","Batsman_Scored")],y=PlayerFile[,c("Player_Id","Player_Name")],by.x = "Striker_Id",by.y = "Player_Id",all.x = TRUE)
q1b= noofsixes %>% group_by(Player_Name) %>% summarise(TopSixHitters=sum(Batsman_Scored == 6,na.rm = T)) %>% arrange(-TopSixHitters) %>%
  head(10)

#c based on noof 100s
noof100s=merge(x=ballbyballFile[,c("Match_Id","Striker_Id","Batsman_Scored")],y=PlayerFile[,c("Player_Id","Player_Name")],by.x = "Striker_Id",by.y = "Player_Id",all.x = TRUE)
q1c= noof100s %>% group_by(Match_Id,Player_Name) %>% summarise(TotalRuns=sum(as.integer(Batsman_Scored),na.rm = T)) %>%
 group_by(Player_Name) %>% summarise(TotalHundreds=sum(TotalRuns>99,na.rm=T)) %>% arrange(-TotalHundreds) %>% head(10)

#Q2 Top10 Teams
#a Based on number of matches won
noofmatcheswon=merge(x=MatchFile[,c("Match_Winner_Id")],y=TeamFile[,c("Team_Id","Team_Name")],by.x = "Match_Winner_Id",by.y = "Team_Id",all.x = TRUE)
q2a=noofmatcheswon %>% group_by(Team_Name) %>% summarise(TotalWins=n()) %>% arrange(-TotalWins) %>% head(10)
View(q2a)
#b Based on winning the toss and the match
noofmatcheswonp2= merge(x=MatchFile[,c("Match_Winner_Id","Toss_Winner_Id")],y=TeamFile[,c("Team_Id","Team_Name")],by.x = "Match_Winner_Id",by.y = "Team_Id",all.x = TRUE)
q2b= noofmatcheswonp2 %>% group_by(Team_Name) %>% summarise(MatchWins=sum(Match_Winner_Id == Toss_Winner_Id,na.rm = T)) %>%
  arrange(-MatchWins) %>% head(10)

#c
#Which player plays for which team
tempTeam=merge(x=Player_MatchFile,y=TeamFile[,c("Team_Id","Team_Name")],by.x = "Team_Id",by.y = "Team_Id",all.x = T)

FinalPlayerTeamMatches=merge(x=tempTeam,y=PlayerFile[,c("Player_Id","Player_Name")],by.x = "Player_Id",by.y="Player_Id",all.x = T)                                                          
MatchPlayerTeamDs=merge(x=MatchFile,y=FinalPlayerTeamMatches[,c("Player_Id","Team_Id","Match_Id","Team_Name","Player_Name")],by.x="Match_Id",by.y ="Match_Id",all.x = T)
View(MatchPlayerTeamDs)

abc=MatchPlayerTeamDs %>% group_by(Season_Id,Match_Id,Team_Name) %>% summarise(temp=sum(Man_Of_The_Match_Id == Player_Id ,na.rm = T)) %>%
  group_by(Season_Id,Team_Name) %>% summarise(TotalMofSecured=sum(temp)) %>% top_n(n=1,wt=TotalMofSecured)  

abc
#3 Orange cap holder

tempOrange=merge(x=ballbyballFile[,c("Match_Id","Striker_Id","Batsman_Scored")],y=MatchFile[,c("Match_Id","Match_Date")],by.x = "Match_Id",by.y = "Match_Id",all.x = T)
SeasonOrangeHolder=merge(x=tempOrange,y=PlayerFile[,c("Player_Id","Player_Name")],by.x="Striker_Id",by.y = "Player_Id",all.x = T)
SeasonOrangeHolder$Year=format(as.Date(SeasonOrangeHolder$Match_Date,"%m-%d-%Y"),'%Y')
FinalSeasonOrangeHolder=merge(x=SeasonOrangeHolder,y=Season_File[,c("Season_Id","Season_Year")],by.x = "Year", by.y = "Season_Year", all.x = T)

q3a= FinalSeasonOrangeHolder %>% group_by(Season_Id,Player_Name) %>% summarise(TotalRunsScored=sum(as.integer(Batsman_Scored),na.rm = T)) %>%
  top_n(n=1,wt=TotalRunsScored)

q3a

#b Purple cap
tempPurple=merge(x=Season_File,y=PlayerFile[,c("Player_Id","Player_Name")],by.x="Purple_Cap_Id",by.y="Player_Id",all.x=T)
View(tempPurple)
q3b= tempPurple %>% select(Season_Year,Player_Name) %>% arrange(Season_Year)
q3b

#4 season wise team winning with gratest win margin
### You have to check the condition of win-type. Wintype must be filtered on "by runs"
tempBigMargins=merge(x=MatchFile[,c("Season_Id","Match_Date","Match_Winner_Id","Won_By")],
                     y=TeamFile[,c("Team_Id","Team_Name")],by.x="Match_Winner_Id",by.y="Team_Id",all.x = T)
q4= tempBigMargins %>% group_by(Season_Id,Team_Name) %>% summarise(TopSuccChasCount= max(as.integer(Won_By),na.rm = T)) %>%
  top_n(n=1,wt=TopSuccChasCount)

#5
#aTotal Number of matches in a season
q5a= MatchFile %>% group_by(Season_Id) %>% summarise(MatchesPlayed=n()) 
View(q5a)

#b100's scored season wise
q5b=ballbyballFile %>% group_by(Session_Id,Match_Id,Striker_Id) %>% summarise(TotalCentury=sum(as.integer(Batsman_Scored),na.rm =T)) %>%
group_by(Session_Id) %>% summarise(TotalCenturies=sum(TotalCentury>99))
View(q5b)

#c50's scored season wise
q5c=ballbyballFile %>% group_by(Session_Id,Match_Id,Striker_Id) %>% summarise(TotalFifties=sum(as.integer(Batsman_Scored),na.rm =T)) %>%
  group_by(Session_Id) %>% summarise(TotalCenturies=sum(TotalFifties>49 & TotalFifties<100))
View(q5c)


#D Max sixes scored by season wise 
q5d=ballbyballFile %>% group_by(Session_Id) %>% summarise(NumberOFSixes=sum(as.integer(Batsman_Scored) == 6,na.rm = T)) 
View(q5d)

#E Max fours scored by season wise
q5e=ballbyballFile %>% group_by(Session_Id) %>% summarise(NumberOFFours=sum(as.integer(Batsman_Scored) == 4,na.rm = T)) 
View(q5e)

#6 
teamSeasonWinTemp= merge(x=MatchFile[,c("Season_Id","Match_Date","Match_Winner_Id")],y=TeamFile[,c("Team_Id","Team_Name")],by.x="Match_Winner_Id",by.y="Team_Id",all.x=T)
View(teamSeasonWinTemp)
q6=teamSeasonWinTemp %>% group_by(Season_Id) %>% filter(Match_Date == max(Match_Date)) %>% select(Season_Id,Match_Date,Team_Name)
View(q6)

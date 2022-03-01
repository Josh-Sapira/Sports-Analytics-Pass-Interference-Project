#Set working directory and laod in packages
setwd("~/Desktop/PassInterferenceProject")
library(tidyverse)

#Load in data
pbp2021 <- read_csv("Datasets/pbp-2021.csv")
pbp2020 <- read_csv("Datasets/pbp-2020.csv")
#pbp2019 <- read_csv("Datasets/pbp-2019.csv") Ommitting 2019 because PI calls were challengable
pbp2018 <- read_csv("Datasets/pbp-2018.csv")
pbp2017 <- read_csv("Datasets/pbp-2017.csv")
pbp2016 <- read_csv("Datasets/pbp-2016.csv")
pbp2015 <- read_csv("Datasets/pbp-2015.csv")
pbp2014 <- read_csv("Datasets/pbp-2014.csv")
pbp2013 <- read_csv("Datasets/pbp-2013.csv")

#Create master table with data from 2013-2021
pbpMaster <- rbind(pbp2021, pbp2020, pbp2018, pbp2017, pbp2016, pbp2015, pbp2014, pbp2013)

#Filter out non-defensive pass interference plays and isolate variables of interest
pbpMaster <- pbpMaster %>%
  filter(PenaltyType == "DEFENSIVE PASS INTERFERENCE")%>%
  filter(PenaltyYards > 0) %>%
  group_by(Description)

pbpMaster <- pbpMaster[!grepl("]", pbpMaster$Description), ]

pbpMaster <- pbpMaster %>% 
  mutate(rDescription = unlist(strsplit(Description, split = '-', fixed = TRUE))[3])%>%
  mutate(Name = unlist(strsplit(rDescription, split = '.', fixed = TRUE))[2]) %>%
  mutate(LastName = unlist(strsplit(Name, split = ' ', fixed = TRUE))[1])%>%
  mutate(FirstInitial = unlist(strsplit(rDescription, split = '.', fixed = TRUE))[1])%>%
  select(SeasonYear, FirstInitial, LastName, Down, ToGo, YardLine, OffenseTeam, DefenseTeam, Description, 
         PassType, PenaltyYards, IsPenalty)%>%
  group_by(SeasonYear, LastName, FirstInitial) %>%
  mutate(joinName = paste(FirstInitial, LastName))

#Find player targets
targets <- rbind(pbp2021, pbp2020, pbp2018, pbp2017, pbp2016, pbp2015, pbp2014, pbp2013)
targets <- targets %>%
  filter(PlayType == "PASS") %>%
  group_by(Description) %>%
  mutate(rDescription = unlist(strsplit(Description, split = '-', fixed = TRUE))[3])%>%
  mutate(Name = unlist(strsplit(rDescription, split = '.', fixed = TRUE))[2]) %>%
  mutate(LastName = unlist(strsplit(Name, split = ' ', fixed = TRUE))[1])%>%
  mutate(FirstInitial = unlist(strsplit(rDescription, split = '.', fixed = TRUE))[1]) %>%
  select(SeasonYear, FirstInitial, LastName, IsPass, IsPenalty, PenaltyYards, Description) %>%
  group_by(SeasonYear, LastName, FirstInitial)

targetsSum <- summarise(targets, Targets = sum(IsPass)) %>%
  mutate(joinName = paste(FirstInitial, LastName)) %>%
  select(SeasonYear, FirstInitial, LastName, Targets, joinName)
targetsSum <- subset(targetsSum, !is.na(targetsSum$LastName))
targetsSum <- subset(targetsSum, targetsSum$LastName != '')
targetsSum <- subset(targetsSum, substring(targetsSum$LastName, 1, 1) != '(')

#Summary tbl
piMaster <- summarise(pbpMaster, PenaltiesDrawn = sum(IsPenalty), TotalYardage = sum(PenaltyYards)) %>%
  mutate(joinName = paste(FirstInitial, LastName)) %>%
  select(SeasonYear, FirstInitial, LastName, PenaltiesDrawn, TotalYardage, joinName) %>%
  ungroup()
piMaster <- subset(piMaster, !is.na(piMaster$LastName))
piMaster <- subset(piMaster, piMaster$LastName != '')
piMaster <- subset(piMaster, substring(piMaster$LastName, nchar(piMaster$LastName), 
                                       nchar(piMaster$LastName)) != ']')

#Make new tables for each year
piMaster2013 <- filter(piMaster, SeasonYear == 2013)
piMaster2014 <- filter(piMaster, SeasonYear == 2014)
piMaster2015 <- filter(piMaster, SeasonYear == 2015)
piMaster2016 <- filter(piMaster, SeasonYear == 2016)
piMaster2017 <- filter(piMaster, SeasonYear == 2017)
piMaster2018 <- filter(piMaster, SeasonYear == 2018)
piMaster2020 <- filter(piMaster, SeasonYear == 2020)
piMaster2021 <- filter(piMaster, SeasonYear == 2021)


#Join targets and pi data
#2013
master2013 <- merge(piMaster2013, targetsSum, by = c("LastName", "FirstInitial"), all.x = TRUE, all.y = TRUE)
master2013 <- master2013 %>%
  filter(SeasonYear.y == 2013) %>%
  select(SeasonYear.y, FirstInitial, LastName, PenaltiesDrawn, TotalYardage, Targets) %>%
  rename(SeasonYear = SeasonYear.y) %>%
  mutate(PenaltiesDrawnPerTarget = PenaltiesDrawn/Targets) %>%
  mutate(joinName = paste(FirstInitial, LastName, ""))
master2013[is.na(master2013)] <- 0
master2013 <- subset(master2013, substring(master2013$LastName, nchar(master2013$LastName), 
                                         nchar(master2013$LastName)) != ']')
#2014
master2014 <- merge(piMaster2014, targetsSum, by = c("LastName", "FirstInitial"), all.x = TRUE, all.y = TRUE)
master2014 <- master2014 %>%
  filter(SeasonYear.y == 2014) %>%
  select(SeasonYear.y, FirstInitial, LastName, PenaltiesDrawn, TotalYardage, Targets) %>%
  rename(SeasonYear = SeasonYear.y) %>%
  mutate(PenaltiesDrawnPerTarget = PenaltiesDrawn/Targets) %>%
  mutate(joinName = paste(FirstInitial, LastName, ""))
master2014[is.na(master2014)] <- 0
master2014 <- subset(master2014, substring(master2014$LastName, nchar(master2014$LastName), 
                                           nchar(master2014$LastName)) != ']')
#2015
master2015 <- merge(piMaster2015, targetsSum, by = c("LastName", "FirstInitial"), all.x = TRUE, all.y = TRUE)
master2015 <- master2015 %>%
  filter(SeasonYear.y == 2015) %>%
  select(SeasonYear.y, FirstInitial, LastName, PenaltiesDrawn, TotalYardage, Targets) %>%
  rename(SeasonYear = SeasonYear.y) %>%
  mutate(PenaltiesDrawnPerTarget = PenaltiesDrawn/Targets) %>%
  mutate(joinName = paste(FirstInitial, LastName, ""))
master2015[is.na(master2015)] <- 0
master2015 <- subset(master2015, substring(master2015$LastName, nchar(master2015$LastName), 
                                           nchar(master2015$LastName)) != ']')
#2016
master2016 <- merge(piMaster2016, targetsSum, by = c("LastName", "FirstInitial"), all.x = TRUE, all.y = TRUE)
master2016 <- master2016 %>%
  filter(SeasonYear.y == 2016) %>%
  select(SeasonYear.y, FirstInitial, LastName, PenaltiesDrawn, TotalYardage, Targets) %>%
  rename(SeasonYear = SeasonYear.y) %>%
  mutate(PenaltiesDrawnPerTarget = PenaltiesDrawn/Targets) %>%
  mutate(joinName = paste(FirstInitial, LastName, ""))
master2016[is.na(master2016)] <- 0
master2016 <- subset(master2016, substring(master2016$LastName, nchar(master2016$LastName), 
                                           nchar(master2016$LastName)) != ']')
#2017
master2017 <- merge(piMaster2017, targetsSum, by = c("LastName", "FirstInitial"), all.x = TRUE, all.y = TRUE)
master2017 <- master2017 %>%
  filter(SeasonYear.y == 2017) %>%
  select(SeasonYear.y, FirstInitial, LastName, PenaltiesDrawn, TotalYardage, Targets) %>%
  rename(SeasonYear = SeasonYear.y) %>%
  mutate(PenaltiesDrawnPerTarget = PenaltiesDrawn/Targets) %>%
  mutate(joinName = paste(FirstInitial, LastName, ""))
master2017[is.na(master2017)] <- 0
master2017 <- subset(master2017, substring(master2017$LastName, nchar(master2017$LastName), 
                                           nchar(master2017$LastName)) != ']')
#2018
master2018 <- merge(piMaster2018, targetsSum, by = c("LastName", "FirstInitial"), all.x = TRUE, all.y = TRUE)
master2018 <- master2018 %>%
  filter(SeasonYear.y == 2018) %>%
  select(SeasonYear.y, FirstInitial, LastName, PenaltiesDrawn, TotalYardage, Targets) %>%
  rename(SeasonYear = SeasonYear.y) %>%
  mutate(PenaltiesDrawnPerTarget = PenaltiesDrawn/Targets) %>%
  mutate(joinName = paste(FirstInitial, LastName, ""))
master2018[is.na(master2018)] <- 0
master2018 <- subset(master2018, substring(master2018$LastName, nchar(master2018$LastName), 
                                           nchar(master2018$LastName)) != ']')
#2020
master2020 <- merge(piMaster2020, targetsSum, by = c("LastName", "FirstInitial"), all.x = TRUE, all.y = TRUE)
master2020 <- master2020 %>%
  filter(SeasonYear.y == 2020) %>%
  select(SeasonYear.y, FirstInitial, LastName, PenaltiesDrawn, TotalYardage, Targets) %>%
  rename(SeasonYear = SeasonYear.y) %>%
  mutate(PenaltiesDrawnPerTarget = PenaltiesDrawn/Targets) %>%
  mutate(joinName = paste(FirstInitial, LastName, ""))
master2020[is.na(master2020)] <- 0
master2020 <- subset(master2020, substring(master2020$LastName, nchar(master2020$LastName), 
                                           nchar(master2020$LastName)) != ']')
#2021
master2021 <- merge(piMaster2021, targetsSum, by = c("LastName", "FirstInitial"), all.x = TRUE, all.y = TRUE)
master2021 <- master2021 %>%
  filter(SeasonYear.y == 2021) %>%
  select(SeasonYear.y, FirstInitial, LastName, PenaltiesDrawn, TotalYardage, Targets) %>%
  rename(SeasonYear = SeasonYear.y) %>%
  mutate(PenaltiesDrawnPerTarget = PenaltiesDrawn/Targets) %>%
  mutate(joinName = paste(FirstInitial, LastName, ""))
master2021[is.na(master2021)] <- 0
master2021 <- subset(master2021, substring(master2021$LastName, nchar(master2021$LastName), 
                                           nchar(master2021$LastName)) != ']')
#ALL YEARS
master <- rbind(master2013, master2014, master2015, master2016, master2017, master2018, master2020, master2021)
master <- master %>%
  mutate(PenaltiesDrawnPerTarget = PenaltiesDrawn / Targets) %>%
  filter(Targets >= 40)

#List of all correlation coefficients:
years <- c(2013, 2014, 2015, 2016, 2017, 2020) #No 2018,21 bc vector is years being compared to next year
correlationCoefficients <- c()
for(i in years){
  a1 <- master %>%
    filter(SeasonYear == i)
  a2 <- master %>%
    filter(SeasonYear == i+1)
  
  A <- inner_join(a1, a2, by = c("LastName", "FirstInitial"))
  corCoef <- cor(A$PenaltiesDrawnPerTarget.x, A$PenaltiesDrawnPerTarget.y)
  correlationCoefficients <- append(correlationCoefficients, corCoef)
}

#Standardize data
mean <- mean(master$PenaltiesDrawnPerTarget)
sd <- sd(master$PenaltiesDrawnPerTarget)
zMaster <- master %>%
  mutate(zscore = (PenaltiesDrawnPerTarget - mean) / sd)

zCorCoef <- c()
for(i in years){
  a1 <- zMaster %>%
    filter(SeasonYear == i)
  a2 <- zMaster %>%
    filter(SeasonYear == i+1)
  
  A <- inner_join(a1, a2, by = c("LastName", "FirstInitial"))
  corCoef <- cor(A$zscore.x, A$zscore.y)
  zCorCoef <- append(zCorCoef, corCoef)
}


#GRAPHS

#2021 Penalties Leaders
penLeaders2021 <- master2021[order(master2021$PenaltiesDrawn, decreasing = TRUE),]
penLeaders2021 <- penLeaders2021[1:15, ]
penLeadersPlot2021 <- ggplot(penLeaders2021, aes(x=reorder(joinName, PenaltiesDrawn), y=PenaltiesDrawn, 
                                                 fill=as.factor(PenaltiesDrawn))) +
  geom_col(show.legend = FALSE) +
  scale_fill_manual(values=c("dodgerblue3", "brown2", "palegreen4")) +
  labs(title = "League Leaders in Pass Interference Calls Drawn (2021)") +
  xlab(element_blank()) +
  ylab(element_blank()) +
  theme_light() +
  coord_flip()
#Players with 3 DPIs drawn in 2021 for footnote
master20213 <- master2021 %>%
  filter(PenaltiesDrawn == 3)


#2021 Targets Leaders
tarLeaders2021 <- master2021[order(master2021$Targets, decreasing = TRUE),]
tarLeaders2021 <- tarLeaders2021[1:15, ]
tarLeadersPlot2021 <- ggplot(tarLeaders2021, aes(x=reorder(joinName, Targets), y=Targets))+
  geom_col(fill="brown1", color="brown1", show.legend = FALSE) +
  labs(title = "League Leaders in Targets (2021)") +
  ylab(element_blank()) + 
  xlab(element_blank()) +
  theme_light() +
  coord_flip()

#2020 - 2021 DPIs/target drawn
# Remove players who did not play both seasons and create one dataframe
df202021 <- inner_join(master2020, master2021, by = c("LastName", "FirstInitial"))
df202021 <- df202021 %>%
  filter(Targets.x >= 40) %>%
  filter(Targets.y >= 40)
DPIPerTargetPlot2020_2021 <- ggplot(df202021, aes(x=PenaltiesDrawnPerTarget.x, y=PenaltiesDrawnPerTarget.y)) +
  geom_point() +
  labs(title = "DPIs Drawn Per Target 2020 vs DPIs Drawn Per Target 2021") +
  ylab("DPIs Drawn Per Target in 2021") +
  xlab("DPIs Drawn Per Target in 2020") +
  theme_light()
  
  

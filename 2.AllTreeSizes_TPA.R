
# 1. Change state codes to names
# 2. >5in. remove subcycle 99
# 3. Create size classes
# 4. Live & Dead trees >5in. pivot wider/longer to create 0s for no trees present
# 5. Calculate TPA
# 6. First Graphs

library(tidyverse)
library(dplyr)
library(ggplot2)


# Import initial 123 query
IQresults<- read.csv("4corners123Results.csv")
summary(IQresults)


# Basic clean up: 

# Delete columns dont need now
TPAIQresults1<- IQresults[,c(8,12,13,18,19,22,7)]


TPAIQresults1$SUCP<- factor(TPAIQresults1$SUCP)
TPAIQresults1$cycle_aw<- factor(TPAIQresults1$cycle_aw)
TPAIQresults1$SUBCYCLE<- factor(TPAIQresults1$SUBCYCLE)
TPAIQresults1$SPCD<- factor(TPAIQresults1$SPCD)
summary(TPAIQresults1)

TPAIQresults2<- TPAIQresults1 %>% mutate(STATECD = case_when(STATECD==4 ~ 'Arizona', STATECD==8 ~ 'Colorado', STATECD==35 ~ 'NewMexico', 
                                                       STATECD==49 ~ 'Utah'))
TPAIQresults2$STATECD<-factor(TPAIQresults2$STATECD)
summary(TPAIQresults2)

##############################################################################################
#
# Filter for PIPO, live & dead TREES >5" only 
#

TPAalltrees<- TPAIQresults2 %>% filter(SPCD==122) %>% 
  filter(STATUSCD>=1, STATUSCD<=2) %>% 
  filter(DIACALC>=5.0)
TPAalltrees$STATUSCD<- factor(TPAalltrees$STATUSCD)
TPAalltrees$SPCD<- factor(TPAalltrees$SPCD)
summary(TPAalltrees)
length(unique(TPAalltrees$SUCP)) #1612 plots, #1511 now

# Remove plots with 99 as subcycle...
TPArowstoremove<- TPAalltrees %>% filter(SUBCYCLE==99)

TPAalltreeswithremoved<- TPAalltrees[!(TPAalltrees$SUBCYCLE==99),]

length(unique(TPAalltreeswithremoved$SUCP))  #1511 



# Create size class column
# 
# Seedlings <1
# Saplings "<5.0"
# Trees 5-9.9      A
# Trees 10-14.9    B
# Trees >15        C


TPAalltreeswithremoved$SizeClass<- NA
TPAalltreeswithremoved$SizeClass[TPAalltreeswithremoved$DIACALC >=5 & TPAalltreeswithremoved$DIACALC<10]<- 'C'
TPAalltreeswithremoved$SizeClass[TPAalltreeswithremoved$DIACALC >=10 & TPAalltreeswithremoved$DIACALC <15] <- 'D'
TPAalltreeswithremoved$SizeClass[TPAalltreeswithremoved$DIACALC >=15] <- 'E'

TPAalltreeswithremoved$SizeClass<- factor(TPAalltreeswithremoved$SizeClass)
summary(TPAalltreeswithremoved)



##### LIVE TREES >5 in.

TPAalltreeslive<- TPAalltreeswithremoved %>% filter(STATUSCD==1) 
length(unique(TPAalltreeslive$SUCP)) #1418
summary(TPAalltreeslive)


#cannot have numeric column name
TPAalltreeslive2<- TPAalltreeslive %>% mutate(cycle_aw = case_when(cycle_aw==2 ~ 'cycletwo', cycle_aw==1 ~ 'cycleone'))
TPAalltreeslive2$cycle_aw<- factor(TPAalltreeslive2$cycle_aw)
summary(TPAalltreeslive2)


# Sum tree count per plot by size classes
TPAalltreeslive2$treeexist<- as.logical(TPAalltreeslive2$DIACALC)
TPAalltreeslive3<- TPAalltreeslive2 %>% group_by(STATECD, SUCP, cycle_aw, SUBCYCLE, SPCD, STATUSCD, SizeClass) %>% summarise(Treecount=sum(treeexist))
length(unique(TPAalltreeslive3$SUCP)) #1516


summary(TPAalltreeslive3)
table(TPAalltreeslive3$SUCP)

# pivot_wider
TPAalltreeslive4<- TPAalltreeslive3 %>% pivot_wider(names_from=cycle_aw, values_from = Treecount)
table(TPAalltreeslive4$SUCP)
length(unique(TPAalltreeslive4$SUCP))# 1516 unique SUCP

#change NA to 0
TPAalltreeslive4$cycletwo[is.na(TPAalltreeslive4$cycletwo)]<-0
TPAalltreeslive4$cycleone[is.na(TPAalltreeslive4$cycleone)]<-0

#back to pivot_longer
TPAalltreeslive5<- TPAalltreeslive4 %>% pivot_longer(cols = cycleone:cycletwo, names_to = "cycle_aw", 
                                       names_ptypes = list(cycle_aw=factor(levels=c("cycleone", "cycletwo"))),
                                       values_to = "Treecount")

length(unique(TPAalltreeslive5$SUCP))
summary(TPAalltreeslive5)
# 1418 unique, 3146 total!

#change cycle_aw back to 1/2
TPAalltreeslive6<- TPAalltreeslive5 %>% mutate(cycle_aw = case_when(cycle_aw=="cycleone" ~ '1', cycle_aw=="cycletwo" ~ '2'))
TPAalltreeslive6$cycle_aw<- factor(TPAalltreeslive6$cycle_aw)
summary(TPAalltreeslive6)


#### Do not run below-*****!!!!!!!!!
# But now need equal amount rows for cycle and sizelcass ( 6 rows per plot). 6 rows whether or not there was a 
# tree in the size class
# Expand/nest/
# ***** DO NOT RUN **************
alllive<- alltreeslive6 %>% expand(nesting(STATECD, SUCP, SPCD, STATUSCD), cycle_aw, SizeClass)
table(alllive$SUCP)
alllive %>% anti_join(alltreeslive6) 
alltreeslive7<- alltreeslive6 %>% right_join(alllive)

# Change NA to 0
alltreeslive7$Treecount[is.na(alltreeslive7$Treecount)]<-0

summary(alltreeslive7)

###############################################################################################################################
### Graph with tree count not TPA
ggplot(TPAalltreeslive6, aes(x=cycle_aw, y=Treecount, colour=SizeClass))+ 
  geom_boxplot()+
  facet_grid(~SizeClass)+
  ggtitle("Live Tree Count by Size Class")


# With points
ggplot(TPAalltreeslive6, aes(x=cycle_aw, y=Treecount, colour=SizeClass))+
  geom_point(position = "jitter")+
  geom_boxplot(outlier.shape = NA)+
  facet_grid(~SizeClass)+
  ggtitle("Title")

###############################################################################################################################


# Now add in TPA

# TPA= #trees * expansion factor. 
# All trees in alltreeslive7 dataframe are greater or equal to 5" so we can use regular 1/24 acre subplots size, 
# Trees >=5 inch  : 6.018046 is expansion factor. 

# For subplot, I calculated expansion factor with (4*0.04154172) = 6.018046, where 4 is the number
# of subplots in plot, and 0.04154172 is the area of a 1/24acre subplot. 

TPAalltreeslive6$ExpanFactor<- as.logical(TPAalltreeslive6$cycle_aw)

TPAalltreeslive6$ExpanFactor[is.na(TPAalltreeslive6$ExpanFactor)]<-6.018046


summary(TPAalltreeslive6)

TPAalltreeslive6$TPA<- TPAalltreeslive6$Treecount * TPAalltreeslive6$ExpanFactor
summary(TPAalltreeslive6)

TPAalltreeslive7 <- TPAalltreeslive6[,-3] 


###############################################################################################################################


### Graph with TPA
ggplot(TPAalltreeslive6, aes(x=cycle_aw, y=TPA, colour=SizeClass))+ 
  geom_boxplot()+
  facet_grid(~SizeClass)+
  ggtitle("Live TPA by Size Class")


# With points
ggplot(TPAalltreeslive6, aes(x=cycle_aw, y=TPA, colour=SizeClass))+
  geom_point(position = "jitter")+
  geom_boxplot(outlier.shape = NA)+
  facet_grid(~SizeClass)+
  stat_summary(fun.y = mean, colour="black", geom="point", shape=18, size=3, show_guide=FALSE)+
  ggtitle("Live TPA by Size Class, C: 5-10, D: 10-15, E: >15")


###############################################################################################################################
###############################################################################################################################
###############################################################################################################################


# Dead all size classes

TPAalltreesdead<- TPAalltreeswithremoved %>% filter(STATUSCD==2) 
length(unique(TPAalltreesdead$SUCP)) #781
summary(TPAalltreesdead)


#cannot have numeric column name
TPAalltreesdead2<- TPAalltreesdead %>% mutate(cycle_aw = case_when(cycle_aw==2 ~ 'cycletwo', cycle_aw==1 ~ 'cycleone'))
TPAalltreesdead2$cycle_aw<- factor(TPAalltreesdead2$cycle_aw)
summary(TPAalltreesdead2)


# Sum tree count per plot by size classes
TPAalltreesdead2$treeexist<- as.logical(TPAalltreesdead2$DIACALC)
TPAalltreesdead3<- TPAalltreesdead2 %>% group_by(STATECD, SUCP, cycle_aw, SUBCYCLE, SPCD, STATUSCD, SizeClass) %>% summarise(Treecount=sum(treeexist))
length(unique(TPAalltreesdead3$SUCP)) #781


summary(TPAalltreesdead3)
table(TPAalltreesdead3$SUCP)

# pivot_wider
TPAalltreesdead4<- TPAalltreesdead3 %>% pivot_wider(names_from=cycle_aw, values_from = Treecount)
table(TPAalltreesdead4$SUCP)
length(unique(TPAalltreesdead4$SUCP))# 781 unique SUCP

#change NA to 0
TPAalltreesdead4$cycletwo[is.na(TPAalltreesdead4$cycletwo)]<-0
TPAalltreesdead4$cycleone[is.na(TPAalltreesdead4$cycleone)]<-0

#back to pivot_longer
TPAalltreesdead5<- TPAalltreesdead4 %>% pivot_longer(cols = cycleone:cycletwo, names_to = "cycle_aw", 
                                               names_ptypes = list(cycle_aw=factor(levels=c("cycleone", "cycletwo"))),
                                               values_to = "Treecount")

length(unique(TPAalltreesdead5$SUCP))
summary(TPAalltreesdead5)
# 781, 1255 total unique, 

#change cycle_aw back to 1/2
TPAalltreesdead6<- TPAalltreesdead5 %>% mutate(cycle_aw = case_when(cycle_aw=="cycleone" ~ '1', cycle_aw=="cycletwo" ~ '2'))
TPAalltreesdead6$cycle_aw<- factor(TPAalltreesdead6$cycle_aw)
summary(TPAalltreesdead6)


#### DO not run below-!!!!!!!!!!********
# But now need equal amount rows for cycle and size class ( 6 rows per plot). 6 rows whether or not there was a 
# tree in the size class
# Expand/nest/

alldead<- alltreesdead6 %>% expand(nesting(STATECD, SUCP, SPCD, STATUSCD), cycle_aw, SizeClass)
table(alldead$SUCP) # all plots should have 6 rows 
alldead %>% anti_join(alltreesdead6) 
alltreesdead7<- alltreesdead6 %>% right_join(alldead)


# Change NA to 0
alltreesdead7$Treecount[is.na(alltreesdead7$Treecount)]<-0

summary(alltreesdead7)

###############################################################################################################################
### Graph with tree count not TPA

ggplot(TPAalltreesdead6, aes(x=cycle_aw, y=Treecount, colour=SizeClass))+ 
  geom_boxplot()+
  facet_grid(~SizeClass)+
  ggtitle("Dead Tree Count by Size Class")

# With points
ggplot(TPAalltreesdead6, aes(x=cycle_aw, y=Treecount, colour=SizeClass))+
  geom_point(position = "jitter")+
  geom_boxplot(outlier.shape = NA)+
  facet_grid(~SizeClass)+
  ggtitle("Title")

###############################################################################################################################


# Now add in TPA

# TPA= #trees * expansion factor. 
# All trees in alltreeslive7 dataframe are greater or equal to 5" so we can use regular 1/24 acre subplots size, 
# Trees >=5 inch  : 6.018046 is expansion factor. 

# For subplot, I calculated expansion factor with (4*0.04154172) = 6.018046, where 4 is the number
# of subplots in plot, and 0.04154172 is the area of a 1/24acre subplot. 

TPAalltreesdead6$ExpanFactor<- as.logical(TPAalltreesdead6$cycle_aw)

TPAalltreesdead6$ExpanFactor[is.na(TPAalltreesdead6$ExpanFactor)]<-6.018046


summary(TPAalltreesdead6)

TPAalltreesdead6$TPA<- TPAalltreesdead6$Treecount * TPAalltreesdead6$ExpanFactor
summary(TPAalltreesdead6)

TPAalltreesdead7 <- TPAalltreesdead6[,-3] 
summary(TPAalltreesdead7)

###############################################################################################################################




### Graph with TPA
ggplot(TPAalltreesdead7, aes(x=cycle_aw, y=TPA, colour=SizeClass))+ 
  geom_boxplot()+
  facet_grid(~SizeClass)+
  stat_summary(fun.y = mean, colour="black", geom="point", shape=18, size=3, show_guide=FALSE)+
  ggtitle("Dead TPA by Size Class")


# With points
ggplot(TPAalltreesdead7, aes(x=cycle_aw, y=TPA, colour=SizeClass))+
  geom_point(position = "jitter")+
  geom_boxplot(outlier.shape = NA)+
  facet_grid(~SizeClass)+
  stat_summary(fun.y = mean, colour="black", geom="point", shape=18, size=3, show_guide=FALSE)+
  ggtitle("Dead TPA by Size Class, A: 5-10, B: 10-15, C: >15")




                                




###############################################################################################################################

# merge all size classes into one dataframe
###############################################################################################################################





# Live
livesaplings<- read.csv("SaplingsLive.csv")
livesaplings <- livesaplings[,-1] 
seedlings<- read.csv("Seedlings.csv")
seedlings <- seedlings[,-1] 


liveallsizeclasses <- rbind(livesaplings, seedlings)
summary(liveallsizeclasses)
liveallsizeclasses2<- rbind(data.frame(liveallsizeclasses), data.frame(alltreeslive6))

summary(liveallsizeclasses2)
liveallsizeclasses2$SUCP<- factor(liveallsizeclasses2$SUCP)
liveallsizeclasses2$STATUSCD<- factor(liveallsizeclasses2$STATUSCD)
liveallsizeclasses2$cycle_aw<- factor(liveallsizeclasses2$cycle_aw)
liveallsizeclasses2$SUBCYCLE<- factor(liveallsizeclasses2$SUBCYCLE)
liveallsizeclasses2$SPCD<- factor(liveallsizeclasses2$SPCD)
summary(liveallsizeclasses2)


write.csv(liveallsizeclasses2, "LiveAllSizeClasses.csv")


###############################################################################################################################
liveallsizeclass<-read.csv("LiveAllSizeClasses.csv")
summary(liveallsizeclass)
liveallsizeclass$SUCP<- factor(liveallsizeclass$SUCP)
liveallsizeclass$STATUSCD<- factor(liveallsizeclass$STATUSCD)
liveallsizeclass$cycle_aw<- factor(liveallsizeclass$cycle_aw)
liveallsizeclass$SPCD<- factor(liveallsizeclass$SPCD)
summary(liveallsizeclass)


### Graph with tree count not TPA
ggplot(liveallsizeclass, aes(x=cycle_aw, y=Treecount, colour=SizeClass))+ 
  geom_boxplot()+
  facet_grid(~SizeClass)+
  ggtitle("Live Tree Count by Size Class")


# With points
ggplot(liveallsizeclass, aes(x=cycle_aw, y=Treecount, colour=SizeClass))+
  geom_point(position = "jitter")+
  geom_boxplot(outlier.shape = NA)+
  facet_grid(~SizeClass)+
  stat_summary(fun.y = mean, colour="black", geom="point", shape=18, size=3, show_guide=FALSE)+
  ggtitle("Live TPA by Size Class, A: <1, B: 1-5, C: 5-10, D: 10-15, E: >15")





# Dead

deadsaplings<- read.csv("SaplingsDead.csv")
deadsaplings <- deadsaplings[,-1] 


deadallsizeclasses<- rbind(data.frame(alltreesdead6), data.frame(deadsaplings))
summary(deadallsizeclasses)


write.csv(deadallsizeclasses, "DeadAllSizeClasses.csv")




# Read it in for future-
deadalltrees<- read.csv("DeadAllSizeClasses.csv")
summary(deadalltrees)
deadalltrees$SUCP<- factor(deadalltrees$SUCP)
deadalltrees$STATUSCD<- factor(deadalltrees$STATUSCD)
deadalltrees$cycle_aw<- factor(deadalltrees$cycle_aw)
deadalltrees$SPCD<- factor(deadalltrees$SPCD)
summary(deadalltrees)



### Graph with tree count not TPA
ggplot(deadalltrees, aes(x=cycle_aw, y=Treecount, colour=SizeClass))+ 
  geom_boxplot()+
  facet_grid(~SizeClass)+
  ggtitle("Dead Tree Count by Size Class")


# With points
ggplot(deadalltrees, aes(x=cycle_aw, y=Treecount, colour=SizeClass))+
  geom_point(position = "jitter")+
  geom_boxplot(outlier.shape = NA)+
  facet_grid(~SizeClass)+
  stat_summary(fun.y = mean, colour="black", geom="point", shape=18, size=3, show_guide=FALSE)+
  ggtitle("Dead TPA by Size Class, C: 5-10, D: 10-15, E: >15")


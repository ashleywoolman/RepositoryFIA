#NewAllTreeSizes


# 1. Basal Area for live and dead
# 2. Combining with TPA dataframe ("AllTreeSizes_TPA")
# 3. Add in fire, line 222


##############################################################################################


library(tidyverse)
library(dplyr)
library(ggplot2)


# Import initial 123 query
IQresults<- read.csv("4corners123Results.csv")
summary(IQresults)


# Basic clean up: 

# Delete columns dont need now
IQresults1<- IQresults[,c(2,3,5,8,12,13,18,19,22,7)]


IQresults1$SUCP<- factor(IQresults1$SUCP)
IQresults1$cycle_aw<- factor(IQresults1$cycle_aw)
IQresults1$SUBCYCLE<- factor(IQresults1$SUBCYCLE)
IQresults1$SPCD<- factor(IQresults1$SPCD)
summary(IQresults1)

IQresults2<- IQresults1 %>% mutate(STATECD = case_when(STATECD==4 ~ 'Arizona', STATECD==8 ~ 'Colorado', STATECD==35 ~ 'NewMexico', 
                                                       STATECD==49 ~ 'Utah'))
IQresults2$STATECD<-factor(IQresults2$STATECD)
summary(IQresults2)

##############################################################################################


##############################################################################################
#
# Filter for PIPO, live & dead TREES >5" only 
#

alltrees<- IQresults2 %>% filter(SPCD==122) %>% 
  filter(STATUSCD>=1, STATUSCD<=2) %>% 
  filter(DIACALC>=5.0)
alltrees$STATUSCD<- factor(alltrees$STATUSCD)
alltrees$SPCD<- factor(alltrees$SPCD)
summary(alltrees)
length(unique(alltrees$SUCP)) #1612 plots, #1511 now

# Remove plots with 99 as subcycle...
rowstoremove<- alltrees %>% filter(SUBCYCLE==99)

alltreeswithremoved<- alltrees[!(alltrees$SUBCYCLE==99),]

length(unique(alltreeswithremoved$SUCP))  #1511 



# Create size class column
# 
# Seedlings <1
# Saplings "<5.0"
# Trees 5-9.9      A
# Trees 10-14.9    B
# Trees >15        C


alltreeswithremoved$SizeClass<- NA
alltreeswithremoved$SizeClass[alltreeswithremoved$DIACALC >=5 & alltreeswithremoved$DIACALC<10]<- 'C'
alltreeswithremoved$SizeClass[alltreeswithremoved$DIACALC >=10 & alltreeswithremoved$DIACALC <15] <- 'D'
alltreeswithremoved$SizeClass[alltreeswithremoved$DIACALC >=15] <- 'E'

alltreeswithremoved$SizeClass<- factor(alltreeswithremoved$SizeClass)
summary(alltreeswithremoved)


##### LIVE TREES >5 in.

alltreeslive<- alltreeswithremoved %>% filter(STATUSCD==1) #6066
length(unique(alltreeslive$SUCP)) #1516
summary(alltreeslive)


#cannot have numeric column name
alltreeslive2<- alltreeslive %>% mutate(cycle_aw = case_when(cycle_aw==2 ~ 'cycletwo', cycle_aw==1 ~ 'cycleone'))
alltreeslive2$cycle_aw<- factor(alltreeslive2$cycle_aw)
summary(alltreeslive2)


### basal area######
#1. To determine BA per tree: DBH2 (SQUARED) * 0.005454. = FT2
alltreeslive2$BA_Tree_Sqft<- alltreeslive2$DIACALC^2*0.005454
#2. Per acre, to be able to add all trees per plot together & compare, *** WITHIN TREE SIZE PLOT. NOT SAPLINGS!
alltreeslive2$BA_PerAcre_Sqft<- alltreeslive2$BA_Tree_Sqft*6.018046

summary(alltreeslive2)


# Sum  BA per acre, per plot by size classes
detach(package:plyr)

alltreeslive3<- alltreeslive2 %>% group_by(STATECD, SUCP, cycle_aw, SPCD, STATUSCD, SizeClass) %>% 
  summarise(BAtotal_AcreFt2=sum(BA_PerAcre_Sqft))

length(unique(alltreeslive3$SUCP)) #1516


summary(alltreeslive3)

## Including PLT_CN and INVYR will not work.
# pivot_wider
alltreeslive4<- alltreeslive3 %>% pivot_wider(names_from=cycle_aw, values_from = BAtotal_AcreFt2)
table(alltreeslive4$SUCP)
length(unique(alltreeslive4$SUCP))# 1418 unique SUCP

#change NA to 0
alltreeslive4$cycletwo[is.na(alltreeslive4$cycletwo)]<-0
alltreeslive4$cycleone[is.na(alltreeslive4$cycleone)]<-0

#back to pivot_longer
alltreeslive5<- alltreeslive4 %>% pivot_longer(cols = cycleone:cycletwo, names_to = "cycle_aw", 
                                               names_ptypes = list(cycle_aw=factor(levels=c("cycleone", "cycletwo"))),
                                               values_to = "BAtotal_AcreFt2")

length(unique(alltreeslive5$SUCP))
summary(alltreeslive5)
# 1516 unique, 3032 total!

#change cycle_aw back to 1/2
alltreeslive6<- alltreeslive5 %>% mutate(cycle_aw = case_when(cycle_aw=="cycleone" ~ '1', cycle_aw=="cycletwo" ~ '2'))
alltreeslive6$cycle_aw<- factor(alltreeslive6$cycle_aw)
summary(alltreeslive6)


##** Merge with TPA **********
#remove expan factor
alltreeslive7<- alltreeslive6[,-8]
summary(alltreeslive7)

# TPAalltreeslive7 is from "AllTreeSizes_TPA"
liveBAandTPA<- merge (alltreeslive6, TPAalltreeslive7, by=c("STATECD", "SUCP", "SPCD", "STATUSCD", "SizeClass", "cycle_aw"))
summary(liveBAandTPA)


###############################################################################################################################
###############################################################################################################################
###############################################################################################################################


# Dead all size classes


alltreesdead<- alltreeswithremoved %>% filter(STATUSCD==2) 
length(unique(alltreesdead$SUCP)) #839
summary(alltreesdead)


#cannot have numeric column name
alltreesdead2<- alltreesdead %>% mutate(cycle_aw = case_when(cycle_aw==2 ~ 'cycletwo', cycle_aw==1 ~ 'cycleone'))
alltreesdead2$cycle_aw<- factor(alltreesdead2$cycle_aw)
summary(alltreesdead2)


### basal area######
#1. To determine BA per tree: DBH2 (SQUARED) * 0.005454. = FT2
alltreesdead2$BA_Tree_Sqft<- alltreesdead2$DIACALC^2*0.005454
#2. Per acre, to be able to add all trees per plot together & compare, *** WITHIN TREE SIZE PLOT. NOT SAPLINGS!
alltreesdead2$BA_PerAcre_Sqft<- alltreesdead2$BA_Tree_Sqft*6.018046

summary(alltreesdead2)


# Sum  BA per acre, per plot by size classes
alltreesdead3<- alltreesdead2 %>% group_by(STATECD, SUCP, cycle_aw, SPCD, STATUSCD, SizeClass) %>% 
  summarise(BAtotal_AcreFt2=sum(BA_PerAcre_Sqft))

length(unique(alltreeslive3$SUCP)) #1516
summary(alltreesdead3)
table(alltreesdead3$SUCP)

# pivot_wider
alltreesdead4<- alltreesdead3 %>% pivot_wider(names_from=cycle_aw, values_from = BAtotal_AcreFt2)
table(alltreesdead4$SUCP)
length(unique(alltreesdead4$SUCP))# 781 unique SUCP

#change NA to 0
alltreesdead4$cycletwo[is.na(alltreesdead4$cycletwo)]<-0
alltreesdead4$cycleone[is.na(alltreesdead4$cycleone)]<-0

#back to pivot_longer
alltreesdead5<- alltreesdead4 %>% pivot_longer(cols = cycleone:cycletwo, names_to = "cycle_aw", 
                                               names_ptypes = list(cycle_aw=factor(levels=c("cycleone", "cycletwo"))),
                                               values_to = "BAtotal_AcreFt2")

length(unique(alltreesdead5$SUCP))
summary(alltreesdead5)
# 781 unique, 

#change cycle_aw back to 1/2
alltreesdead6<- alltreesdead5 %>% mutate(cycle_aw = case_when(cycle_aw=="cycleone" ~ '1', cycle_aw=="cycletwo" ~ '2'))
alltreesdead6$cycle_aw<- factor(alltreesdead6$cycle_aw)
summary(alltreesdead6)



##** Merge with TPA **********

deadBAandTPA<- merge (alltreesdead6, TPAalltreesdead7, by=c("STATECD", "SUCP", "SPCD", "STATUSCD", "SizeClass", "cycle_aw"))
summary(deadBAandTPA)



write.csv(liveBAandTPA, "liveBAandTPAallsizes.csv")
write.csv(deadBAandTPA, "deadBAandTPAallsizes.csv")


##########################################################################################################################################

# 1. Combine all trees together- live and dead.
livetrees<- read.csv("liveBAandTPAallsizes.csv")
deadtrees<- read.csv("deadBAandTPAallsizes.csv")
alltrees<- rbind(livetrees, deadtrees) 
alltrees <- alltrees[,-1] 

summary(alltrees)
alltrees$cycle_aw<- factor(alltrees$cycle_aw)
length(unique(alltrees$SUCP)) #1511
testing<- alltrees %>% group_by(STATECD, SUCP) %>% summarise(test=sum(STATUSCD))


# 2. Identify fire plots:
#notreatmentfcornerscond2 is dataframe of COND of plots that were resampled and no treatment
# It contains ALL species plots
condtable<- read.csv("cond12results.csv")
summary(condtable)
condtable1<- condtable[, c(4, 9:10, 12, 17:18)]
summary(condtable1)
condtable2<- condtable1 %>% drop_na(DSTRBCD1, DSTRBYR1)
summary(condtable2)
condtable3<- condtable2 %>% filter(DSTRBCD1==30| DSTRBCD1==31| DSTRBCD1==32)
summary(condtable3)
condtable3$DSTRBCD1 [condtable3$DSTRBCD1 >29| condtable3$DSTRBCD1<33] <- "1"
condtable3$cycle_aw<- factor(condtable3$cycle_aw)
condtable3$DSTRBCD1<- factor(condtable3$DSTRBCD1)
summary(condtable3)

#Theres duplicates-
duplicated(condtable3)
condtable4<- condtable3 %>% distinct()
summary(condtable4)

length(unique(condtable4$SUCP)) #634 plots with fire, cycle 1 and cycle 2, across all species/plots within 4 corners ****#


condtable5<- condtable4 %>% filter(condtable4$cycle_aw==2)
summary(condtable5)
length(unique(condtable5$SUCP)) #385

condtable6 <- condtable5[,-1]
condtable6<- condtable6[,-3]
summary(condtable6)

# Merge newtry2 (tree data) with cond to then narrow out fire plots from plots we care about, not larger data set
newfire1<-merge(alltrees, condtable6, by=c("SUCP", "cycle_aw"), all.x=TRUE)
summary(newfire1)
length(unique(newfire1$SUCP))


# Need to match dataframe up with its paired cycle 1
subset<-unique(newfire1[newfire1$cycle_aw==2 & newfire1$DSTRBCD1==1,]$SUCP)
newfire2<- newfire1 %>% filter(cycle_aw==2 & DSTRBCD1==1 | cycle_aw==1 & SUCP%in%subset)#### I think you can only have 2 things in a 'or|' statement
# **********using newtry2 %>% filter(cycle_aw==2 & DSTRBCD1==1 | cycle_aw==1 & DSTRBCD1==0 & SUCP%in%subset) would not work###################
#################
length(unique(newfire2$SUCP)) #157 plots
summary(newfire2)

#### Need to create new column which identifies these SUCP's had a fire occur. 
newfire2$Fire= ifelse(newfire2$TPA>=0, 1,0)
summary(newfire2)
# Now, need to merge this file with the greater file. 

summary(newfire2)
AllTrees_CDE_WithFire<-merge(alltrees, newfire2, by=c("SUCP", "STATECD", "SPCD", "cycle_aw", "STATUSCD", "SizeClass",
                                         "Treecount", "ExpanFactor", "TPA", "BAtotal_AcreFt2"), all.x=TRUE)
AllTrees_CDE_WithFire$Fire[is.na(AllTrees_CDE_WithFire$Fire)]<-0
AllTrees_CDE_WithFire$Fire<-factor(AllTrees_CDE_WithFire$Fire)
AllTrees_CDE_WithFire$cycle_aw<-factor(AllTrees_CDE_WithFire$cycle_aw)

summary(AllTrees_CDE_WithFire)

# All trees (C, D, E) with fire variable
write.csv(AllTrees_CDE_WithFire, "AllTrees_CDE_WithFire.csv")

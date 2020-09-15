# Saplings only
#
# 1. TPA for live saplings
# 2. BA for live saplings
# 3. Fire for live saplings
# Repeat for dead
#


library(tidyverse)
library(dplyr)
library(ggplot2)


# Import initial 123 query
IQresults<- read.csv("4corners123Results.csv")
summary(IQresults)


# Basin clean up: 

# Delete columns dont need now
IQresults1<- IQresults[,c(8,12,13,7,18,19,22)]


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
#
# Filter for PIPO, live & dead saplings (<5.0 inch) only 
#

allsapling<- IQresults2 %>% filter(SPCD==122) %>% 
  filter(STATUSCD>=1, STATUSCD<=2) %>% 
  filter(DIACALC<5.0)
allsapling$STATUSCD<- factor(allsapling$STATUSCD)
allsapling$SPCD<- factor(allsapling$SPCD)
summary(allsapling)
length(unique(allsapling$SUCP)) #390 plots

# Remove plots with 99 as subcycle...
rowstoremove<- allsapling %>% filter(SUBCYCLE==99)

allsaplingswithremoved<- allsapling[!(allsapling$SUBCYCLE==99),]

length(unique(allsaplingswithremoved$SUCP))  #389 

# SizeClass
# saplings are <5.0 and >1.0 inch DBH
allsaplingswithremoved$SizeClass<- as.logical(allsaplingswithremoved$cycle_aw)
allsaplingswithremoved$SizeClass[is.na(allsaplingswithremoved$SizeClass)]<- 'B'

allsaplingswithremoved$SizeClass<- factor(allsaplingswithremoved$SizeClass)
summary(allsaplingswithremoved)



 ##############################################################################################
# Live Saplings

allsaplinglive<- allsaplingswithremoved %>% filter(STATUSCD==1) #400
length(unique(allsaplinglive$SUCP)) #379
summary(allsaplinglive)

#cannot have numeric column name
allsaplinglive2<- allsaplinglive %>% mutate(cycle_aw = case_when(cycle_aw==2 ~ 'cycletwo', cycle_aw==1 ~ 'cycleone'))
allsaplinglive2$cycle_aw<- factor(allsaplinglive2$cycle_aw)
summary(allsaplinglive2)

# Sum tree count per plot by size classes
allsaplinglive2$treeexist<- as.logical(allsaplinglive2$DIACALC)
summary(allsaplinglive2)

detach(package:plyr)

allsaplinglive3<- allsaplinglive2 %>% group_by(STATECD, SUCP, cycle_aw, STATUSCD, SPCD, SizeClass) %>% summarise(Treecount=sum(treeexist))
length(unique(allsaplinglive3$SUCP)) #379

summary(allsaplinglive3)
table(allsaplinglive3$SUCP)

# pivot_wider
allsaplinglive4<- allsaplinglive3 %>% pivot_wider(names_from=cycle_aw, values_from = Treecount)
table(allsaplinglive4$SUCP)
length(unique(allsaplinglive4$SUCP))# 379 unique SUCP

#change NA to 0
allsaplinglive4$cycletwo[is.na(allsaplinglive4$cycletwo)]<-0
allsaplinglive4$cycleone[is.na(allsaplinglive4$cycleone)]<-0

#back to pivot_longer
allsaplinglive5<- allsaplinglive4 %>% pivot_longer(cols = cycleone:cycletwo, names_to = "cycle_aw", 
                                               names_ptypes = list(cycle_aw=factor(levels=c("cycleone", "cycletwo"))),
                                               values_to = "Treecount")

length(unique(allsaplinglive5$SUCP))
summary(allsaplinglive5)
# 379 unique, 758 total!

#change cycle_aw back to 1/2
allsaplinglive6<- allsaplinglive5 %>% mutate(cycle_aw = case_when(cycle_aw=="cycleone" ~ '1', cycle_aw=="cycletwo" ~ '2'))
allsaplinglive6$cycle_aw<- factor(allsaplinglive6$cycle_aw)
summary(allsaplinglive6)


# Now add in TPA

# Create column for expansion factor - 
allsaplinglive6$ExpanFactor<- as.logical(allsaplinglive6$cycle_aw)
allsaplinglive6$ExpanFactor[is.na(allsaplinglive6$ExpanFactor)]<-74.96528
summary(allsaplinglive6)

allsaplinglive6$TPA<- (allsaplinglive6$Treecount*allsaplinglive6$ExpanFactor)



summary(allsaplinglive6)





##### BASAL AREA, Live
summary(allsaplingswithremoved)

sapliveBA<- allsaplingswithremoved %>% filter(STATUSCD==1) 
length(unique(sapliveBA$SUCP)) #379
summary(sapliveBA)


#cannot have numeric column name
sapliveBA2<- sapliveBA %>% mutate(cycle_aw = case_when(cycle_aw==2 ~ 'cycletwo', cycle_aw==1 ~ 'cycleone'))
sapliveBA2$cycle_aw<- factor(sapliveBA2$cycle_aw)
summary(sapliveBA2)


### basal area######
#1. To determine BA per tree: DBH2 (SQUARED) * 0.005454. = FT2
sapliveBA2$BA_Tree_Sqft<- sapliveBA2$DIACALC^2*0.005454
#2. Per acre, to be able to add all trees per plot together & compare, *** WITHIN TREE SIZE PLOT. For saplings, 74.965282 ?
sapliveBA2$BA_PerAcre_Sqft<- sapliveBA2$BA_Tree_Sqft*74.965282
summary(sapliveBA2)


# Sum  BA per acre, per plot by size classes
sapliveBA3<- sapliveBA2 %>% group_by(STATECD, SUCP, cycle_aw, SPCD, STATUSCD, SizeClass) %>% 
  summarise(BAtotal_AcreFt2=sum(BA_PerAcre_Sqft))

length(unique(sapliveBA3$SUCP)) #379

summary(sapliveBA3)

## Including PLT_CN and INVYR will not work.
# pivot_wider
sapliveBA4<- sapliveBA3 %>% pivot_wider(names_from=cycle_aw, values_from = BAtotal_AcreFt2)
table(sapliveBA4$SUCP)
length(unique(sapliveBA4$SUCP))# 379 unique SUCP

#change NA to 0
sapliveBA4$cycletwo[is.na(sapliveBA4$cycletwo)]<-0
sapliveBA4$cycleone[is.na(sapliveBA4$cycleone)]<-0

#back to pivot_longer
sapliveBA5<- sapliveBA4 %>% pivot_longer(cols = cycleone:cycletwo, names_to = "cycle_aw", 
                                               names_ptypes = list(cycle_aw=factor(levels=c("cycleone", "cycletwo"))),
                                               values_to = "BAtotal_AcreFt2")

length(unique(sapliveBA5$SUCP))
summary(sapliveBA5)
# 1516 unique, 3032 total!

#change cycle_aw back to 1/2
sapliveBA6<- sapliveBA5 %>% mutate(cycle_aw = case_when(cycle_aw=="cycleone" ~ '1', cycle_aw=="cycletwo" ~ '2'))
sapliveBA6$cycle_aw<- factor(sapliveBA6$cycle_aw)
summary(sapliveBA6)


##** Merge with TPA ***

summary(allsaplinglive6)

#remove expan factor
allsaplinglive7<- allsaplinglive6[,-8]
summary(allsaplinglive7)

sapliveBAandTPA<- merge (allsaplinglive7, sapliveBA6, by=c("STATECD", "SUCP", "SPCD", "STATUSCD", "SizeClass", "cycle_aw"))
summary(sapliveBAandTPA)




##########################
# Fire, Live saplings


# Live Saplings
summary(sapliveBAandTPA)
summary(condtable6)

saplivefire<-merge(sapliveBAandTPA, condtable6, by=c("SUCP", "cycle_aw"), all.x=TRUE) 

# Need to match dataframe up with its paired cycle 1
subset<-unique(saplivefire[saplivefire$cycle_aw==2 & saplivefire$DSTRBCD1==1,]$SUCP)
saplivefire2<- saplivefire %>% filter(cycle_aw==2 & DSTRBCD1==1 | cycle_aw==1 & SUCP%in%subset)#### I think you can only have 2 things in a 'or|' statement
# **********using newtry2 %>% filter(cycle_aw==2 & DSTRBCD1==1 | cycle_aw==1 & DSTRBCD1==0 & SUCP%in%subset) would not work###################
#################
length(unique(saplivefire2$SUCP)) #42 plots
summary(saplivefire2)

#### Need to create new column which identifies these SUCP's had a fire occur. 
saplivefire2$Fire= ifelse(saplivefire2$TPA>=0, 1,0)
summary(saplivefire2)

# Now, need to merge this file with the greater file. 
summary(saplivefire2)
saplivefire3<-merge(sapliveBAandTPA, saplivefire2, by=c("SUCP", "STATECD", "SPCD", "cycle_aw", "STATUSCD", "SizeClass",
                                                     "Treecount", "TPA", "BAtotal_AcreFt2"), all.x=TRUE)
summary(saplivefire3)

saplivefire4<- saplivefire3[,c(2,1,3,4,5,6,7,8,9,12,11)]


saplivefire4$Fire[is.na(saplivefire4$Fire)]<-0
saplivefire4$Fire<-factor(saplivefire4$Fire)
saplivefire4$cycle_aw<-factor(saplivefire4$cycle_aw)

summary(saplivefire4)

library(plyr)
library(dplyr)
revalue(saplivefire4$Fire, c("0"= "No Fire", "1"="Fire Occured"))-> saplivefire4$Fire
summary(saplivefire4)

write.csv(saplivefire4, "SaplingsLive.csv")

















##############################################################################################
# Dead Saplings

allsaplingdead<- allsaplingswithremoved %>% filter(STATUSCD==2) 
length(unique(allsaplingdead$SUCP)) #104
summary(allsaplingdead)


#cannot have numeric column name
allsaplingdead2<- allsaplingdead %>% mutate(cycle_aw = case_when(cycle_aw==2 ~ 'cycletwo', cycle_aw==1 ~ 'cycleone'))
allsaplingdead2$cycle_aw<- factor(allsaplingdead2$cycle_aw)
summary(allsaplingdead2)


# Sum tree count per plot by size classes
allsaplingdead2$treeexist<- as.logical(allsaplingdead2$DIACALC)

detach(package:plyr)

allsaplingdead3<- allsaplingdead2 %>% group_by(STATECD, SUCP, cycle_aw, SPCD, STATUSCD, SizeClass) %>% summarise(Treecount=sum(treeexist))
length(unique(allsaplingdead3$SUCP)) #104

summary(allsaplingdead3)
table(allsaplingdead3$SUCP)

# pivot_wider
allsaplingdead4<- allsaplingdead3 %>% pivot_wider(names_from=cycle_aw, values_from = Treecount)
table(allsaplingdead4$SUCP)
length(unique(allsaplingdead4$SUCP))# 104 unique SUCP

#change NA to 0
allsaplingdead4$cycletwo[is.na(allsaplingdead4$cycletwo)]<-0
allsaplingdead4$cycleone[is.na(allsaplingdead4$cycleone)]<-0

#back to pivot_longer
allsaplingdead5<- allsaplingdead4 %>% pivot_longer(cols = cycleone:cycletwo, names_to = "cycle_aw", 
                                                   names_ptypes = list(cycle_aw=factor(levels=c("cycleone", "cycletwo"))),
                                                   values_to = "Treecount")

length(unique(allsaplingdead5$SUCP))
summary(allsaplingdead5)
# 104 unique, 208 total

#change cycle_aw back to 1/2
allsaplingdead6<- allsaplingdead5 %>% mutate(cycle_aw = case_when(cycle_aw=="cycleone" ~ '1', cycle_aw=="cycletwo" ~ '2'))
allsaplingdead6$cycle_aw<- factor(allsaplingdead6$cycle_aw)
summary(allsaplingdead6)



# Now add in TPA

# Create column for expansion factor - 
allsaplingdead6$ExpanFactor<- as.logical(allsaplingdead6$cycle_aw)
allsaplingdead6$ExpanFactor[is.na(allsaplingdead6$ExpanFactor)]<-74.96528
summary(allsaplingdead6)

allsaplingdead6$TPA<- (allsaplingdead6$Treecount*allsaplingdead6$ExpanFactor)

summary(allsaplingdead6)



##### BASAL AREA, Live
summary(allsaplingswithremoved)

sapdeadBA<- allsaplingswithremoved %>% filter(STATUSCD==2) 
length(unique(sapdeadBA$SUCP)) #104
summary(sapdeadBA)


#cannot have numeric column name
sapdeadBA2<- sapdeadBA %>% mutate(cycle_aw = case_when(cycle_aw==2 ~ 'cycletwo', cycle_aw==1 ~ 'cycleone'))
sapdeadBA2$cycle_aw<- factor(sapdeadBA2$cycle_aw)
summary(sapdeadBA2)


### basal area######
#1. To determine BA per tree: DBH2 (SQUARED) * 0.005454. = FT2
sapdeadBA2$BA_Tree_Sqft<- sapdeadBA2$DIACALC^2*0.005454
#2. Per acre, to be able to add all trees per plot together & compare, *** WITHIN TREE SIZE PLOT. For saplings, 74.965282 ?
sapdeadBA2$BA_PerAcre_Sqft<- sapdeadBA2$BA_Tree_Sqft*74.965282
summary(sapdeadBA2)


# Sum  BA per acre, per plot by size classes
sapdeadBA3<- sapdeadBA2 %>% group_by(STATECD, SUCP, cycle_aw, SPCD, STATUSCD, SizeClass) %>% 
  summarise(BAtotal_AcreFt2=sum(BA_PerAcre_Sqft))

length(unique(sapdeadBA3$SUCP)) #104

summary(sapdeadBA3)

## Including PLT_CN and INVYR will not work.
# pivot_wider
sapdeadBA4<- sapdeadBA3 %>% pivot_wider(names_from=cycle_aw, values_from = BAtotal_AcreFt2)
table(sapdeadBA4$SUCP)
length(unique(sapdeadBA4$SUCP))# 104 unique SUCP

#change NA to 0
sapdeadBA4$cycletwo[is.na(sapdeadBA4$cycletwo)]<-0
sapdeadBA4$cycleone[is.na(sapdeadBA4$cycleone)]<-0

#back to pivot_longer
sapdeadBA5<- sapdeadBA4 %>% pivot_longer(cols = cycleone:cycletwo, names_to = "cycle_aw", 
                                         names_ptypes = list(cycle_aw=factor(levels=c("cycleone", "cycletwo"))),
                                         values_to = "BAtotal_AcreFt2")

length(unique(sapdeadBA5$SUCP))
summary(sapdeadBA5)
# 1516 unique, 3032 total!

#change cycle_aw back to 1/2
sapdeadBA6<- sapdeadBA5 %>% mutate(cycle_aw = case_when(cycle_aw=="cycleone" ~ '1', cycle_aw=="cycletwo" ~ '2'))
sapdeadBA6$cycle_aw<- factor(sapdeadBA6$cycle_aw)
summary(sapdeadBA6)


##** Merge with TPA ***

summary(allsaplingdead6)

#remove expan factor
allsaplingdead7<- allsaplingdead6[,-8]
summary(allsaplingdead7)

sapDeadBAandTPA<- merge (allsaplingdead7, sapdeadBA6, by=c("STATECD", "SUCP", "SPCD", "STATUSCD", "SizeClass", "cycle_aw"))
summary(sapDeadBAandTPA)


##########################
# Fire, Dead saplings


# Live Saplings
summary(sapDeadBAandTPA)
summary(condtable6)

sapDeadfire<-merge(sapDeadBAandTPA, condtable6, by=c("SUCP", "cycle_aw"), all.x=TRUE) 

# Need to match dataframe up with its paired cycle 1
subset<-unique(sapDeadfire[sapDeadfire$cycle_aw==2 & sapDeadfire$DSTRBCD1==1,]$SUCP)
sapDeadfire2<- sapDeadfire %>% filter(cycle_aw==2 & DSTRBCD1==1 | cycle_aw==1 & SUCP%in%subset)#### I think you can only have 2 things in a 'or|' statement
# **********using newtry2 %>% filter(cycle_aw==2 & DSTRBCD1==1 | cycle_aw==1 & DSTRBCD1==0 & SUCP%in%subset) would not work###################
#################
length(unique(sapDeadfire2$SUCP)) #42 plots
summary(sapDeadfire2)

#### Need to create new column which identifies these SUCP's had a fire occur. 
sapDeadfire2$Fire= ifelse(sapDeadfire2$TPA>=0, 1,0)
summary(sapDeadfire2)

# Now, need to merge this file with the greater file. 
summary(sapDeadfire2)
sapDeadfire3<-merge(sapDeadBAandTPA, sapDeadfire2, by=c("SUCP", "STATECD", "SPCD", "cycle_aw", "STATUSCD", "SizeClass",
                                                        "Treecount", "TPA", "BAtotal_AcreFt2"), all.x=TRUE)
summary(sapDeadfire3)

sapDeadfire4<- sapDeadfire3[,c(2,1,3,4,5,6,7,8,9,12,11)]


sapDeadfire4$Fire[is.na(sapDeadfire4$Fire)]<-0
sapDeadfire4$Fire<-factor(sapDeadfire4$Fire)
sapDeadfire4$cycle_aw<-factor(sapDeadfire4$cycle_aw)

summary(sapDeadfire4)


write.csv(sapDeadfire4, "SaplingsDead.csv")



ggplot(saplivefire4, aes(x=cycle_aw, y=TPA, fill=Fire))+
  geom_point(position = "jitter")+
  geom_boxplot(outlier.shape = NA)+
  scale_fill_discrete(name="Fire Presence", labels=c("No Fire", "Fire Occured"))



# Live TPA, With outliers (saplings dont have much)
ggplot(saplivefire4, aes(x=cycle_aw, y=TPA, fill=Fire))+
  geom_point(position = "jitter")+
  geom_boxplot(outlier.shape = NA)+
  labs(x="Inventory Cycle")+ labs(y="Density (Trees Per Acre)")+ labs(fill= "Fire Presence")+
  scale_fill_discrete(name="Fire Presence", labels=c("No Fire", "Fire Occured"))+
  scale_fill_manual(values = c("#008000", "#FF0000"))+
  ggtitle("Ponderosa Pine Live Saplings (>1.0 inch diameter and <5.0inch diameter)")









# Dead, With outliers (saplings dont have much)
ggplot(sapDeadfire4, aes(x=cycle_aw, y=TPA, fill=Fire))+
  geom_point(position = "jitter")+
  geom_boxplot(outlier.shape = NA)+
  labs(x="Inventory Cycle")+ labs(y="Density (Trees Per Acre)")+ labs(fill= "Fire Presence")+
  scale_fill_discrete(name="Fire Presence", labels=c("No Fire", "Fire Occured"))+
  scale_fill_manual(values = c("#008000", "#FF0000"))+
  ggtitle("Ponderosa Pine Live Saplings (>1.0 inch diameter and <5.0inch diameter)")


## Dead Saplings with fire

saplingsdead<-read.csv("SaplingsDead.csv")
summary(saplingsdead)
saplingsdead <- saplingsdead[,-1] 


sapdeadfire<-merge(saplingsdead, condtable6, by=c("SUCP", "cycle_aw"), all.x=TRUE)

# Need to match dataframe up with its paired cycle 1
subset<-unique(sapdeadfire[sapdeadfire$cycle_aw==2 & sapdeadfire$DSTRBCD1==1,]$SUCP)
sapdeadfire2<- sapdeadfire %>% filter(cycle_aw==2 & DSTRBCD1==1 | cycle_aw==1 & SUCP%in%subset)#### I think you can only have 2 things in a 'or|' statement
# **********using newtry2 %>% filter(cycle_aw==2 & DSTRBCD1==1 | cycle_aw==1 & DSTRBCD1==0 & SUCP%in%subset) would not work###################
#################
length(unique(sapdeadfire2$SUCP)) #31 plots
summary(sapdeadfire2)

#### Need to create new column which identifies these SUCP's had a fire occur. 
sapdeadfire2$Fire= ifelse(sapdeadfire2$TPA>=0, 1,0)
summary(sapdeadfire2)

# Now, need to merge this file with the greater file. 
summary(sapdeadfire2)
sapdeadfire3<-merge(saplingsdead, sapdeadfire2, by=c("SUCP", "STATECD", "SPCD", "cycle_aw", "STATUSCD", "SizeClass",
                                                     "Treecount", "ExpanFactor", "TPA"), all.x=TRUE)
summary(sapdeadfire3)

sapdeadfire4<- sapdeadfire3[,c(2,1,3,4,5,6,7,8,9,12,13,14)]


sapdeadfire4$Fire[is.na(sapdeadfire4$Fire)]<-0
sapdeadfire4$Fire<-factor(sapdeadfire4$Fire)
sapdeadfire4$cycle_aw<-factor(sapdeadfire4$cycle_aw)

summary(sapdeadfire4)


# With outliers (saplings dont have much)
ggplot(sapdeadfire4, aes(x=cycle_aw, y=TPA, fill=Fire))+
  geom_point(position = "jitter")+
  geom_boxplot(outlier.shape = NA)+
  labs(x="Inventory Cycle")+ labs(y="Density (Trees Per Acre)")+ labs(fill= "Fire Presence")+
  scale_fill_discrete(name="Fire Presence", labels=c("No Fire", "Fire Occured"))+
  scale_fill_manual(values = c("#008000", "#FF0000"))+
  ggtitle("Ponderosa Pine Dead Saplings (>1.0inch diameter and <5.0inch diameter)")



ggplot(liveCDE, aes(x=cycle_aw, y=TPA, fill=Fire))+
  geom_point(position = "jitter")+
  geom_boxplot(outlier.shape = NA)+
  facet_wrap(SizeClass~., labeller = as_labeller(facetlabels))+
  ylim(0,250)+
  labs(x="Inventory Cycle")+ labs(y="Density (Trees Per Acre)")+ labs(fill= "Fire Presence")+
  scale_fill_discrete(name="Fire Presence", labels=c("No Fire", "Fire Occured"))+
  scale_fill_manual(values = c("#008000", "#FF0000"))+
  ggtitle("Live Ponderosa Pine Density Within the 4 Corner States")

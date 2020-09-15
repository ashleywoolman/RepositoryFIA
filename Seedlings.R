########### Need to merge this with 123 file to remove treated plots/make sure they are resampled plots

## Special things about seedling table
# Need to add tree count together per plot


library(tidyverse)
library(dplyr)
library(ggplot2)

UTseed<- read.csv("UT_SEEDLING.csv")
UTseed<- UTseed %>% filter(INVYR>1999)
UTseed1<- UTseed %>% mutate(cycle_aw = case_when(CYCLE==2 ~ 'cycleone',
                                                 CYCLE==3 ~ 'cycletwo'))

AZseed<- read.csv("AZ_SEEDLING.csv")
AZseed<- AZseed %>% filter(INVYR>1999)
AZseed1<- AZseed %>% mutate(cycle_aw = case_when(CYCLE==3 ~ 'cycleone',
                                                 CYCLE==4 ~ 'cycletwo'))
COseed<- read.csv("CO_SEEDLING.csv")
COseed<- COseed %>% filter(INVYR>1999)
COseed1<- COseed %>% mutate(cycle_aw = case_when(CYCLE==2 ~ 'cycleone',
                                                 CYCLE==3 ~ 'cycletwo'))
NMseed<- read.csv("NM_SEEDLING.csv")
NMseed<- NMseed %>% filter(INVYR>1999)
NMseed1<- NMseed %>% mutate(cycle_aw = case_when(CYCLE==3 ~ 'cycleone',
                                                 CYCLE==4 ~ 'cycletwo'))
# Merge together
seedlings<- rbind(UTseed1, AZseed1, COseed1, NMseed1)
seedlings$cycle_aw<- factor(seedlings$cycle_aw)
summary(seedlings)

### create new variable that pastes together state, unit, countycd, plot: "SUCP"
cols<- c('STATECD', 'UNITCD', 'COUNTYCD', 'PLOT')
seedlings$SUCP<- apply(seedlings[,cols], 1, paste, collapse="_")
seedlings$SUCP<- factor(seedlings$SUCP)
summary(seedlings)


# Delete columns dont need now-
# These are the columns you want to keep
# Including subplot and condition (need to add up all subplots tgoether per plot)
seedlings1<- seedlings[,c(4, 8, 9, 10, 21, 22, 24, 33, 34)]

# re-order columns
seedlings2<- seedlings1[,c(1, 9, 2, 8, 7, 3, 4, 5, 6)]

seedlings3<- seedlings2 %>% mutate(STATECD = case_when(STATECD==4 ~ 'Arizona', STATECD==8 ~ 'Colorado', STATECD==35 ~ 'NewMexico', 
                                                       STATECD==49 ~ 'Utah'))


summary(seedlings3)
seedlings3$STATECD<- factor(seedlings3$STATECD)
seedlings3$SUBCYCLE<- factor(seedlings3$SUBCYCLE)
seedlings3$SPCD<- factor(seedlings3$SPCD)
seedlings3$CONDID<- factor(seedlings3$CONDID)
seedlings3$SUBP<- factor(seedlings3$SUBP)

summary(seedlings3)


# Filter only PIPO
seedlingspipo<- seedlings3 %>% filter(SPCD==122)
summary(seedlingspipo)
table(seedlingspipo$SUCP)
length(unique(seedlingspipo$SUCP)) #777


# Remove plots with 99 as subcycle...
rowstoremove<- seedlingspipo %>% filter(SUBCYCLE==99)

allseedlingswithremoved<- seedlingspipo[!(seedlingspipo$SUBCYCLE==99),]

length(unique(allseedlingswithremoved$SUCP))  #773 



# Need to add tree count by SUCP, SUBPLOT, CYCLE
detach(package:plyr)

seedlingspipo2<- allseedlingswithremoved %>% group_by(STATECD, SUCP, cycle_aw, SUBCYCLE, SPCD) %>%
  summarise(Treecount = sum(TREECOUNT_CALC))

summary(seedlingspipo2) # 1033 rows
table(seedlingspipo2$SUCP)


########################################################################################################################

# pivot_wider
pivotseed<- seedlingspipo2 %>% pivot_wider(names_from=cycle_aw, values_from = Treecount)
length(unique(statussize4$SUCP))# 1621 unique SUCP

#change NA to 0
pivotseed$cycleone[is.na(pivotseed$cycleone)]<-0
pivotseed$cycletwo[is.na(pivotseed$cycletwo)]<-0
summary(pivotseed)

#back to pivot_longer
pivotseed2<- pivotseed %>% pivot_longer(cols = cycleone:cycletwo, names_to = "cycle_aw", 
                                           names_ptypes = list(cycle_aw=factor(levels=c("cycleone", "cycletwo"))),
                                           values_to = "Treecount")

length(unique(pivotseed2$SUCP))
summary(pivotseed2)
# 1516 unique, 3032 total!

#change cycle_aw back to 1/2
seedlingspipo3<- pivotseed2 %>% mutate(cycle_aw = case_when(cycle_aw=="cycleone" ~ '1', cycle_aw=="cycletwo" ~ '2'))
seedlingspipo3$cycle_aw<- factor(seedlingspipo3$cycle_aw)

summary(seedlingspipo3)


### Graph with tree count not TPA
ggplot(seedlingspipo3, aes(x=cycle_aw, y=Treecount, colour=STATECD))+ 
  geom_boxplot()+
  facet_grid(~STATECD)

# Remove outliers and adjust scale
ggplot(seedlingspipo3, aes(x=cycle_aw, y=Treecount, colour=STATECD))+ 
  geom_boxplot(outlier.shape = NA)+
  coord_cartesian(ylim=c(0,10))+
  facet_grid(~STATECD)


summary(seedlingspipo3)

# To match other files, need to have a STATUSCD, SizeClass
# STATUSCD:
seedlingspipo3$STATUSCD<- as.logical(seedlingspipo3$cycle_aw)
seedlingspipo3$STATUSCD[is.na(seedlingspipo3$STATUSCD)]<-1

# SizeClass
# Seedlings are <1.0 inch DBH
seedlingspipo3$SizeClass<- as.logical(seedlingspipo3$cycle_aw)
seedlingspipo3$SizeClass[is.na(seedlingspipo3$SizeClass)]<- 'A'

seedlingspipo4<- seedlingspipo3[,c(1, 2, 4, 5, 3, 7, 8, 6)]
seedlingspipo4$SPCD<- factor(seedlingspipo4$SPCD)
seedlingspipo4$STATUSCD<- factor(seedlingspipo4$STATUSCD)
seedlingspipo4$SizeClass<- factor(seedlingspipo4$SizeClass)
summary(seedlingspipo4)


# Create column for expansion factor - 
seedlingspipo4$ExpanFactor<- as.logical(seedlingspipo4$cycle_aw)
seedlingspipo4$ExpanFactor[is.na(seedlingspipo4$ExpanFactor)]<-74.96528
summary(seedlingspipo4)

seedlingspipo4$TPA<- (seedlingspipo4$Treecount*seedlingspipo4$ExpanFactor)

length(unique(seedlingspipo4$SUCP))



########### Need to merge this with 123 file to remove treated plots/make sure they are resampled plots
notreatmentfcornerscond2<- read.csv("cond12results.csv")

#notreatmentfcornerscond2
length(unique(notreatmentfcornerscond2$SUCP)) # 30743

# Use merge functions to keep SUCP in seedlingspipo4 that are also in  notreatmentfcornerscond2
# If they are not in notreatmentfcornerscond2, you cannot keep them-can assume they have been treated or were not resampled

notreatmentfcornerscond3<- notreatmentfcornerscond2[,c(4, 9, 10)]


newseedlings<- seedlingspipo4[(seedlingspipo4$SUCP %in% notreatmentfcornerscond3$SUCP),]
# This removed 586, now 484 unique SUCP
length(unique(newseedlings$SUCP))
table(newseedlings$SUCP)
summary(newseedlings)


###########################
# Final seedling dataframe: newseedlings
write.csv(newseedlings, "Seedlings.csv")

seedlings<-read.csv("Seedlings.csv")
summary(seedlings)


seedlingfire<-merge(seedlings, condtable6, by=c("SUCP", "cycle_aw"), all.x=TRUE)

# Need to match dataframe up with its paired cycle 1
subset<-unique(seedlingfire[seedlingfire$cycle_aw==2 & seedlingfire$DSTRBCD1==1,]$SUCP)
seedlingfire2<- seedlingfire %>% filter(cycle_aw==2 & DSTRBCD1==1 | cycle_aw==1 & SUCP%in%subset)#### I think you can only have 2 things in a 'or|' statement
# **********using newtry2 %>% filter(cycle_aw==2 & DSTRBCD1==1 | cycle_aw==1 & DSTRBCD1==0 & SUCP%in%subset) would not work###################
#################
length(unique(seedlingfire2$SUCP)) #59 plots
summary(seedlingfire2)

#### Need to create new column which identifies these SUCP's had a fire occur. 
seedlingfire2$Fire= ifelse(seedlingfire2$TPA>=0, 1,0)
summary(seedlingfire2)

# Now, need to merge this file with the greater file. 
summary(seedlingfire2)
seedlingfire3<-merge(seedlings, seedlingfire2, by=c("SUCP", "STATECD", "SPCD", "cycle_aw", "STATUSCD", "SizeClass",
                                         "Treecount", "ExpanFactor", "TPA"), all.x=TRUE)
summary(seedlingfire3)

seedlingfire4<- seedlingfire3[,c(1,2,3,4,5,6,7,8,9,14,15,16)]


seedlingfire4$Fire[is.na(seedlingfire4$Fire)]<-0
seedlingfire4$Fire<-factor(seedlingfire4$Fire)
seedlingfire4$cycle_aw<-factor(seedlingfire4$cycle_aw)

summary(seedlingfire4)
write.csv(seedlingfire4, "Seedlings.csv")


# With outliers
ggplot(seedlingfire4, aes(x=cycle_aw, y=TPA, fill=Fire))+
  geom_point(position = "jitter")+
  geom_boxplot(outlier.shape = NA)+
  labs(x="Inventory Cycle")+ labs(y="Density (Trees Per Acre)")+ labs(fill= "Fire Presence")+
  scale_fill_discrete(name="Fire Presence", labels=c("No Fire", "Fire Occured"))+
  scale_fill_manual(values = c("#008000", "#FF0000"))+
  ggtitle("Ponderosa Pine Regeneration (<1.0 in. diameter and at least 6.0 in. length *With Outliers*")

# Without outliers
ggplot(seedlingfire4, aes(x=cycle_aw, y=TPA, fill=Fire))+
  geom_point(position = "jitter")+
  geom_boxplot(outlier.shape = NA)+
  ylim(0,1000)+
  labs(x="Inventory Cycle")+ labs(y="Density (Trees Per Acre)")+ labs(fill= "Fire Presence")+
  scale_fill_discrete(name="Fire Presence", labels=c("No Fire", "Fire Occured"))+
  scale_fill_manual(values = c("#008000", "#FF0000"))+
  ggtitle("Ponderosa Pine Regeneration (<1.0 in. diameter and at least 6.0 in. length *Without * Outliers*")




# INITIAL GRAPHS:

# Graph TPA
ggplot(newseedlings, aes(x=cycle_aw, y=TPA, colour=STATECD))+ 
  geom_boxplot()+
  stat_summary(fun.y = mean, colour="black", geom="point", show_guide=FALSE)+
  facet_grid(~STATECD)

# Remove outliers and adjust scale
ggplot(newseedlings, aes(x=cycle_aw, y=TPA, colour=STATECD))+ 
  geom_boxplot(outlier.shape = NA)+
  stat_summary(fun.y=mean, colour="darkred", geom = "point", show_guide= FALSE)+
  coord_cartesian(ylim=c(0,500))+
  facet_grid(~STATECD)



ggplot(newseedlings, aes(x=cycle_aw, y=TPA, colour=cycle_aw))+
  geom_boxplot()+
  facet_grid(~STATECD)+
  stat_summary(fun.y = mean, colour="black", geom="point", shape=18, size=3, show_guide=FALSE)+
  ggtitle("Trees Per Acre of Live Ponderosa Pine Seedlings (<1.0??? DBH)")


ggplot(newseedlings, aes(x=cycle_aw, y=TPA, colour=cycle_aw))+
  geom_boxplot(outlier.shape = NA)+
  geom_point(position = "jitter")+
  facet_grid(~STATECD)+
  stat_summary(fun.y = mean, colour="black", geom="point", shape=18, size=3, show_guide=FALSE)+
  ggtitle("Trees Per Acre of Live Ponderosa Pine Seedlings (<1.0??? DBH)")








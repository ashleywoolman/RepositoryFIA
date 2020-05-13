
# calculating basal area and trees per acre

library(tidyverse)
library(dplyr)
library(ggplot2)


## Upload *resampled, nontreated, pipo & associated sp* file
NMpipo<- read.csv("PIPOplotsResampledNontrt.csv")

#remove first column
NMpipo1 <- NMpipo[,-1] 


# Trees >4.9 dbh, live, only PIPO
# >4.9 dbh because this plot size is 1/24 acre, and trees >1 and <5 is 1/300 acre, so there is 
# different expansion factor. As I work through this, I want to create new columns that have 
# appropriate expansion factors in the same dataframe, so i can include all tree sizes. Also, 
# starting with live trees to keep it simple this round. 


#
## TPA live
#

################################################   
# Trees >4.9 dbh, LIVE, only PIPO
#########for tree count
NMpipo1$STATUSCD<- factor(NMpipo1$STATUSCD)
summary(NMpipo1)
NMpipo2<- NMpipo1 %>% filter(DIACALC>4.9, STATUSCD==1, SPCD==122)
NMpipo3<- NMpipo2 %>% mutate(STATECD = case_when(STATECD==4 ~ 'Arizona', STATECD==8 ~ 'Colorado', STATECD==35 ~ 'NewMexico', 
                                                    STATECD==49 ~ 'Utah'))

NMpipo3$STATECD<-factor(NMpipo3$STATECD)
length(unique(NMpipo3$SUCP))
# 401 NM,  1516 in 4 corners unique SUCP

#cannot have numeric column name
NMpipo4<- NMpipo3 %>% mutate(cycle_aw = case_when(cycle_aw==2 ~ 'cycletwo', cycle_aw==3 ~ 'cyclethree'))
NMpipo4$cycle_aw<- factor(NMpipo4$cycle_aw)
summary(NMpipo4)

NMpipo4$treeexist<- as.logical(NMpipo4$DIACALC)
NMpipo5<- NMpipo4 %>% group_by(STATECD, SUCP, cycle_aw, SPCD, STATUSCD) %>% summarise(treecount=sum(treeexist))
length(unique(NMpipo5$SUCP)) #401
summary(NMpipo5)
table(NMpipo5$SUCP)
# 401 unique SUCP

# pivot_wider
NMpipo6<- NMpipo5 %>% pivot_wider(names_from=cycle_aw, values_from = treecount)
table(NMpipo6$SUCP)
length(unique(NMpipo6$SUCP))# 401 unique SUCP

#change NA to 0
NMpipo6$cycletwo[is.na(NMpipo6$cycletwo)]<-0
NMpipo6$cyclethree[is.na(NMpipo6$cyclethree)]<-0

#back to pivot_longer
NMpipo7<- NMpipo6 %>% pivot_longer(cols = cycletwo:cyclethree, names_to = "cycle_aw", 
                                       names_ptypes = list(cycle_aw=factor(levels=c("cycletwo", "cyclethree"))),
                                       values_to = "treecount")

length(unique(NMpipo7$SUCP))
summary(NMpipo7)
# 401 unique, 802 total

#change cycle_aw back to 1/2
NMpipo8<- NMpipo7 %>% mutate(cycle_aw = case_when(cycle_aw=="cycletwo" ~ '1', cycle_aw=="cyclethree" ~ '2'))
NMpipo8$cycle_aw<- factor(NMpipo8$cycle_aw)
summary(NMpipo8)


# i went ahead and just changed these to cycle 1 and 2, to match the rest of the data

# now add in TPA

# TPA= #trees * expansion factor. FOr 1/24 acre plots, 6.018046 is expansion factor. 
# I calculated expansion factor with (4*0.04154172) = 6.018046, where 4 is the number
# of subplots in plot, and 0.04154172 is the area of a 1/24acre subplot. 

NMpipo8$TPA<- NMpipo8$treecount*6.018046
NMpipo8$STATECD<-factor(NMpipo8$STATECD)

NMpipoLive<- NMpipo8 %>% rename("Cycle" = "cycle_aw", "Plot" = "SUCP", "State" = "STATECD", "Species" = "SPCD",
                                 "Status"= "STATUSCD")




#plot this

boxplot(TPA~Cycle, data=NMpipoLive)
ggplot(NMpipoLive, aes(x=TPA, color=Cycle, fill=Cycle))+ geom_histogram(binwidth = 20)





################################################   
# Trees >4.9 dbh, dead, only PIPO
#########for tree count
NMpipoDead<- NMpipo1 %>% filter(DIACALC>4.9, STATUSCD==2, SPCD==122)
NMpipoDead2<- NMpipoDead %>% mutate(STATECD = case_when(STATECD==4 ~ 'Arizona', STATECD==8 ~ 'Colorado', STATECD==35 ~ 'NewMexico', 
                                                    STATECD==49 ~ 'Utah'))

NMpipoDead2$STATECD<-factor(NMpipoDead2$STATECD)
length(unique(NMpipoDead2$SUCP))
# 237 unique SUCP

#cannot have numeric column name
NMpipoDead3<- NMpipoDead2 %>% mutate(cycle_aw = case_when(cycle_aw==2 ~ 'cycletwo', cycle_aw==3 ~ 'cyclethree'))
NMpipoDead3$cycle_aw<- factor(NMpipoDead3$cycle_aw)
summary(NMpipoDead3)

NMpipoDead3$treeexist<- as.logical(NMpipoDead3$DIACALC)
NMpipoDead4<- NMpipoDead3 %>% group_by(STATECD, SUCP, cycle_aw, SPCD, STATUSCD) %>% summarise(treecount=sum(treeexist))
length(unique(NMpipoDead4$SUCP)) #237
summary(NMpipoDead4)
table(NMpipoDead4$SUCP)

# pivot_wider
NMpipoDead5<- NMpipoDead4 %>% pivot_wider(names_from=cycle_aw, values_from = treecount)
table(NMpipoDead5$SUCP)
length(unique(NMpipoDead5$SUCP))# 237 unique SUCP

#change NA to 0
NMpipoDead5$cycletwo[is.na(NMpipoDead5$cycletwo)]<-0
NMpipoDead5$cyclethree[is.na(NMpipoDead5$cyclethree)]<-0

#back to pivot_longer
NMpipoDead6<- NMpipoDead5 %>% pivot_longer(cols = cycletwo:cyclethree, names_to = "cycle_aw", 
                                       names_ptypes = list(cycle_aw=factor(levels=c("cycletwo", "cyclethree"))),
                                       values_to = "treecount")

length(unique(NMpipoDead6$SUCP))
summary(NMpipoDead6)
# 237 unique, 

#change cycle_aw back to 1/2
NMpipoDead7<- NMpipoDead6 %>% mutate(cycle_aw = case_when(cycle_aw=="cycletwo" ~ '1', cycle_aw=="cyclethree" ~ '2'))
NMpipoDead7$cycle_aw<- factor(NMpipoDead7$cycle_aw)
summary(NMpipoDead7)


# now add in TPA

# TPA= #trees * expansion factor. FOr 1/24 acre plots, 6.018046 is expansion factor. 
# I calculated expansion factor with (4*0.04154172) = 6.018046, where 4 is the number
# of subplots in plot, and 0.04154172 is the area of a 1/24acre subplot. 

NMpipoDead7$TPA<- NMpipoDead7$treecount*6.018046
NMpipoDead7$STATECD<-factor(NMpipoDead7$STATECD)

NMpipoDead8<- NMpipoDead7 %>% rename("Cycle" = "cycle_aw", "Plot" = "SUCP", "State" = "STATECD", "Species" = "SPCD",
                                 "Status"= "STATUSCD")


#plot this

boxplot(TPA~Cycle, data=NMpipoDead8)
ggplot(NMpipoDead8, aes(x=TPA, color=Cycle, fill=Cycle))+ geom_histogram(binwidth = 20)





###########fire! copy and pasted from the larger dataset, have not gone through with NM specific
firetry<- NMpipo1 %>% filter(DIACALC>4.9, STATUSCD==1 | STATUSCD==2, SPCD==122)
length(unique(firetry$SUCP)) #1612 (other is 1622)
firetry2<- firetry %>% mutate(STATECD = case_when(STATECD==4 ~ 'Arizona', STATECD==8 ~ 'Colorado', STATECD==35 ~ 'NewMexico', 
                                                  STATECD==49 ~ 'Utah'))

firetry2$STATECD<-factor(firetry2$STATECD)
length(unique(firetry2$SUCP))
# 1612 unique SUCP

#cannot have numeric column name
firetry3<- firetry2 %>% mutate(cycle_aw = case_when(cycle_aw==2 ~ 'cycletwo', cycle_aw==1 ~ 'cycleone'))
firetry3$cycle_aw<- factor(firetry3$cycle_aw)
summary(firetry3)

firetry3$treeexist<- as.logical(firetry3$DIACALC)
firetry4<- firetry3 %>% group_by(STATECD, SUCP, cycle_aw, SPCD, STATUSCD) %>% summarise(treecount=sum(treeexist))
firetry4$STATUSCD<- factor(firetry4$STATUSCD)
firetry4$SPCD<- factor(firetry4$SPCD)
length(unique(firetry4$SUCP)) #1612
summary(firetry4)
table(firetry4$SUCP)

# pivot_wider
firetry5<- firetry4 %>% pivot_wider(names_from=cycle_aw, values_from = treecount)
length(unique(firetry5$SUCP))# 1612 unique SUCP

#change NA to 0
firetry5$cycletwo[is.na(firetry5$cycletwo)]<-0
firetry5$cycleone[is.na(firetry5$cycleone)]<-0

#back to pivot_longer
firetry6<- firetry5 %>% pivot_longer(cols = cycleone:cycletwo, names_to = "cycle_aw", 
                                     names_ptypes = list(cycle_aw=factor(levels=c("cycleone", "cycletwo"))),
                                     values_to = "treecount")

length(unique(firetry6$SUCP))
summary(firetry6)


#change cycle_aw back to 1/2
firetry7<- firetry6 %>% mutate(cycle_aw = case_when(cycle_aw=="cycleone" ~ '1', cycle_aw=="cycletwo" ~ '2'))
firetry7$cycle_aw<- factor(firetry7$cycle_aw)
summary(firetry7)
length(unique(firetry7$SUCP))
# 1612!



condfire<- fcorners3 %>% filter(DSTRBCD1==30| DSTRBCD1==31| DSTRBCD3==32 |
                                  DSTRBCD2==30| DSTRBCD2==31| DSTRBCD2==32 |
                                  DSTRBCD3==30| DSTRBCD3==31| DSTRBCD3==32)
condfire2<- condfire %>% mutate(STATECD = case_when(STATECD==4 ~ 'Arizona', STATECD==8 ~ 'Colorado', STATECD==35 ~ 'NewMexico', 
                                                    STATECD==49 ~ 'Utah'))
# delete rows
condfire3<- select(condfire2, -c(1:3, 5:7, 10:19, 26:38))

#merge
firetry8<- merge(firetry7, condfire3, all.x = TRUE)

# NAs to 0s 
#change NA to 0
firetry8$DSTRBCD1[is.na(firetry8$DSTRBCD1)]<-0
firetry8$DSTRBCD2[is.na(firetry8$DSTRBCD2)]<-0
firetry8$DSTRBCD3[is.na(firetry8$DSTRBCD3)]<-0

firetry8$DSTRBYR1[is.na(firetry8$DSTRBYR1)]<-0
firetry8$DSTRBYR2[is.na(firetry8$DSTRBYR2)]<-0
firetry8$DSTRBYR3[is.na(firetry8$DSTRBYR3)]<-0

length(unique(firetry8$SUCP))

#how many plots with fire in cycle 1?
firetry9<- firetry8 %>% filter(cycle_aw==1)
firetry10<- firetry9 %>% filter(DSTRBCD1==30| DSTRBCD1==31| DSTRBCD1==32 |
                                  DSTRBCD2==30| DSTRBCD2==31| DSTRBCD2==32 |
                                  DSTRBCD3==30| DSTRBCD3==31| DSTRBCD3==32)
length(unique(firetry10$SUCP))

firetry30<- firetry9 %>% filter(DSTRBCD1==30| DSTRBCD2==30| DSTRBCD3==30)
length(unique(firetry30$SUCP))
#56
firetry31<- firetry9 %>% filter(DSTRBCD1==31| DSTRBCD2==31| DSTRBCD3==31)
length(unique(firetry31$SUCP))
#56
firetry32<- firetry9 %>% filter(DSTRBCD1==32| DSTRBCD2==32| DSTRBCD3==32)
length(unique(firetry32$SUCP))
#1





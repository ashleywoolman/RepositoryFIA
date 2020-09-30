

# Re-do of "Newall4" script to include 'subcycle' variable and download most up to date files from Datamart...
# CSV COND files downloaded from datamart 6/18/2020


# Below was copy and pasted the below from 'Newall4' File


# NM tree CSV files were downloaded 4/28 for most up to date plots
# all others downloaded in initial round (fall 2019)

### This script was used for the plots in the May committee meeting


###############
###############
###############
### 1. Identify resampled plots on COND table
### 2. Remove plots with treatment on the *Condition dataframe
### 3. Identify resampled plots on the Tree dataframe
### 4. Remove treatment plots from the resampled Tree dataframe
### 5. Select plots with PIPO and associated species
###############
###############
###############






library(tidyverse)
library(dplyr)


####################################################################################################

# Initial Steps
# Remove first cycles of inventory (pre 2000)
# We have cycle 3, 4 or 2,3  now but going to rename them to match up with other state data to 1, 2,
# Merge together 4 states
# Create SUCP

# file location: Queries-> Current-> CSV files-> NEW_COND 

AZcond<- read.csv("AZ_COND.csv") 
summary(AZcond)
# Keep only certain columns:
AZcond1<- AZcond[, c(1:9, 38:49, 51, 79:80, 33:34)]                                    
AZcond2<- AZcond1 %>% filter(INVYR>1999)
AZcond3<- AZcond2 %>% mutate(cycle_aw = case_when(CYCLE==3 ~ '1',
                                                 CYCLE==4 ~ '2'))

COcond<- read.csv("CO_COND.csv")
summary(COcond)
COcond1<- COcond[, c(1:9, 38:49, 51, 79:80, 33:34)]                                    
COcond2<- COcond1 %>% filter(INVYR>1999)
COcond3<- COcond2 %>% mutate(cycle_aw = case_when(CYCLE==2 ~ '1',
                                                 CYCLE==3 ~ '2'))

UTcond<- read.csv("UT_COND.csv")
summary(UTcond)
UTcond1<- UTcond[, c(1:9, 38:49, 51, 79:80, 33:34)]  
UTcond2<- UTcond1 %>% filter(INVYR>1999)
UTcond3<- UTcond2 %>% mutate(cycle_aw = case_when(CYCLE==2 ~ '1',
                                                 CYCLE==3 ~ '2'))

NMcond<- read.csv("NM_COND.csv")
summary(NMcond)
NMcond1<- NMcond[, c(1:9, 38:49, 51, 79:80, 33:34)]  
NMcond2<- NMcond1 %>% filter(INVYR>1999)
NMcond3<- NMcond2 %>% mutate(cycle_aw = case_when(CYCLE==3 ~ '1',
                                                 CYCLE==4 ~ '2'))
# Merge together
fcorners<- rbind(UTcond3, AZcond3, COcond3, NMcond3)
fcorners$cycle_aw<- factor(fcorners$cycle_aw)
summary(fcorners)

### create new variable that pastes together state, unit, countycd, plot: "SUCP"
cols<- c('STATECD', 'UNITCD', 'COUNTYCD', 'PLOT')
fcorners$SUCP<- apply(fcorners[,cols], 1, paste, collapse="_")
fcorners$SUCP<- factor(fcorners$SUCP)
summary(fcorners)

####################################################################################################




#
### 1. Identify resampled plots on COND table
#





# NM
NMcond3$SUCP<- apply(NMcond3[,cols], 1, paste, collapse="_")
NMcond3$SUCP<- factor(NMcond3$SUCP)
NMcond3$cycle_aw<- factor(NMcond3$cycle_aw)

nmcond2<- unique(NMcond3[NMcond3$cycle_aw==2,]$SUCP)
nmcond3<- NMcond3 %>% filter(cycle_aw==2|(cycle_aw==1 & SUCP%in%nmcond2))

dim(nmcond3) # 11238
summary(nmcond3) # cycle 1: 5600, cycle 2: 5638
length(unique(nmcond3$SUCP)) #5239
table(nmcond3$SUCP) #show many with 3 or 4 entries. this is likely because of condition?


# CO
COcond3$SUCP<- apply(COcond3[,cols], 1, paste, collapse="_")
COcond3$SUCP<- factor(COcond3$SUCP)
COcond3$cycle_aw<- factor(COcond3$cycle_aw)
cocond2<- unique(COcond3[COcond3$cycle_aw==2,]$SUCP)
cocond3<- COcond3 %>% filter(cycle_aw==2|(cycle_aw==1 & SUCP%in%cocond2))
dim(cocond3) # 17456
summary(cocond3) # 1: 8768, 2: 8688
length(unique(cocond3$SUCP)) #7822
table(cocond3$SUCP) #show many with 3 or 4 entries


# AZ
AZcond3$SUCP<- apply(AZcond3[,cols], 1, paste, collapse="_")
AZcond3$SUCP<- factor(AZcond3$SUCP)
AZcond3$cycle_aw<- factor(AZcond3$cycle_aw)
azcond2<- unique(AZcond3[AZcond3$cycle_aw==2,]$SUCP)
azcond3<- AZcond3 %>% filter(cycle_aw==2|(cycle_aw==1 & SUCP%in%azcond2))
dim(azcond3) # 20775
summary(azcond3) # 1: 10436, 2: 10339
length(unique(azcond3$SUCP)) #9805
table(azcond3$SUCP) #show many with 3 or 4 entries

# UT
UTcond3$SUCP<- apply(UTcond3[,cols], 1, paste, collapse="_")
UTcond3$SUCP<- factor(UTcond3$SUCP)
UTcond3$cycle_aw<- factor(UTcond3$cycle_aw)
utcond2<- unique(UTcond3[UTcond3$cycle_aw==2,]$SUCP)
utcond3<- UTcond3 %>% filter(cycle_aw==2|(cycle_aw==1 & SUCP%in%utcond2))
dim(utcond3) # 18221
summary(utcond3) # 1: 9199, 2: 9022
length(unique(utcond3$SUCP)) #8240
table(utcond3$SUCP) #show many with 3 or 4 entries

# merge together
fcorners2<- rbind(utcond3, azcond3, cocond3, nmcond3)
summary(fcorners2)
length(unique(fcorners2$SUCP)) #31,106 total plots
# check
fcornersck1<- fcorners2 %>% filter(cycle_aw==1)
length(unique(fcornersck1$SUCP)) #31096
fcornersck2<- fcorners2 %>% filter(cycle_aw==2)
length(unique(fcornersck2$SUCP)) # 31106
######## 10 more


# re-order columns
# * all of these plots have been resampled!
fcorners3<- fcorners2[,c(1:7, 28, 27, 23:26, 8:22)]




####################################################################################################


#
### 2. Remove plots with treatment on the *Condition dataframe
#
treatment<- fcorners3 %>% filter(fcorners3$TRTCD1==10 | TRTCD2==10 | TRTCD3==10 | 
                                   TRTCD1==20 | TRTCD2==20 | TRTCD3==20 |
                                   TRTCD1==30 | TRTCD2==30 | TRTCD3==30 |
                                   TRTCD1==50 | TRTCD2==50 | TRTCD3==50)

#how many for each?
treatment10<- fcorners3 %>% filter(fcorners3$TRTCD1==10 | TRTCD2==10 | TRTCD3==10)
length(unique(treatment10$SUCP))
treatment20<- fcorners3 %>% filter(fcorners3$TRTCD1==20 | TRTCD2==20 | TRTCD3==20)
length(unique(treatment20$SUCP))
treatment30<- fcorners3 %>% filter(fcorners3$TRTCD1==30 | TRTCD2==30 | TRTCD3==30)
length(unique(treatment30$SUCP))
treatment50<- fcorners3 %>% filter(fcorners3$TRTCD1==50 | TRTCD2==50 | TRTCD3==50)
length(unique(treatment50$SUCP))


# 10: removed 302 plots
# 20: removed 12 plots
# 30: removed 3 plot
# 50: removed 58 plots
# total is 375
length(unique(treatment$SUCP))
# total of 363 plots. I think there were some overlap

notreatmentfcornerscond<-fcorners3[!(fcorners3$SUCP %in% treatment$SUCP),]
length(unique(notreatmentfcornerscond$SUCP))
# 31106-363= 30743  THis worked.

# check these are resampled plots
fcorners4<- unique(notreatmentfcornerscond[notreatmentfcornerscond$cycle_aw==2,]$SUCP)
notreatmentfcornerscond2<- notreatmentfcornerscond %>% filter(cycle_aw==2|(cycle_aw==1 & SUCP%in%fcorners4))
length(unique(notreatmentfcornerscond2$SUCP)) # 30743 total plots 

summary(notreatmentfcornerscond2) 

#notreatmentfcornerscond2 is dataframe of COND of plots that were resampled and no treatment
write.csv(notreatmentfcornerscond2, "cond12results.csv")


####################################################################################################


#
## 3. Identify resampled plots on the Tree dataframe
# 

uttree<- read.csv("UT_TREE.csv")
summary(uttree)
uttree1<- uttree[, c(1:4, 123, 124, 5:11, 14:18, 65, 19:21, 27, 66, 25, 35)]                                    
uttree2<- uttree1 %>% filter(INVYR>1999) %>%
  mutate(cycle_aw = case_when(CYCLE==2 ~ '1', 
                              CYCLE==3 ~ '2'))

cotree<- read.csv("CO_TREE.csv")
summary(cotree)
cotree1<- cotree[, c(1:4, 123, 124, 5:11, 14:18, 65, 19:21, 27, 66, 25, 35)]                                    
cotree2<- cotree1 %>% filter(INVYR>1999) %>%
  mutate(cycle_aw = case_when(CYCLE==2 ~ '1', 
                              CYCLE==3 ~ '2'))

aztree<- read.csv("AZ_TREE.csv")
summary(aztree)
aztree1<- aztree[, c(1:4, 123, 124, 5:11, 14:18, 65, 19:21, 27, 66, 25, 35)]                                    
aztree2<- aztree1 %>% filter(INVYR>1999) %>%
  mutate(cycle_aw = case_when(CYCLE==3 ~ '1',
                              CYCLE==4 ~ '2'))

nmtree<- read.csv("NM_TREE.csv")
summary(nmtree)
nmtree1<- nmtree[, c(1:4, 123, 124, 5:11, 14:18, 65, 19:21, 27, 66, 25, 35)]                                    
nmtree2<- nmtree1 %>% filter(INVYR>1999) %>%
  mutate(cycle_aw = case_when(CYCLE==3 ~ '1',
                              CYCLE==4 ~ '2'))


treesfc<- rbind(uttree2, nmtree2, cotree2, aztree2)
treesfc$cycle_aw<- factor(treesfc$cycle_aw)
summary(treesfc) # cycle 1: 368504, 2: 26752


# create new variable that pastes together state, unit, countycd, plot: "SUCP"
cols<- c('STATECD', 'UNITCD', 'COUNTYCD', 'PLOT')
treesfc$SUCP<- apply(treesfc[,cols], 1, paste, collapse="_")
treesfc$SUCP<- factor(treesfc$SUCP)
length(unique(treesfc$SUCP)) # 14,669

## create subset of plots w cycle 2
treesfc2<- unique(treesfc[treesfc$cycle_aw==2,]$SUCP)

## use subset to get all remeasured plots
treesfc3<- treesfc %>% filter(cycle_aw==2|(cycle_aw==1 & SUCP%in%treesfc2))

# re-order columns
treesfc4<- treesfc3[,c(1:10, 28, 27, 11:26)]

length(unique(treesfc4$SUCP)) # total 10296 plots
summary(treesfc4)

#check
chkt1<- treesfc4 %>% filter(cycle_aw==1)
length(unique(chkt1$SUCP)) # 9492
chkt2<- treesfc4 %>% filter(cycle_aw==2)
length(unique(chkt2$SUCP)) # 10296


####################################################################################################



#
## 4. Remove treatment plots from the resampled Tree dataframe
# 

# Remove plots that had treatment (from the earlier COND dataframe) *can use "treatment"

treesfcnotrtmt<- treesfc4[!(treesfc4$SUCP %in% treatment$SUCP),]
#check to see how many plots were removed
length(unique(treesfcnotrtmt$SUCP)) #9939 plots

# 10296- 9939= 357 plots removed. (not 363 some already filtered out?)

####################################################################################################


#
## 5. Select plots with PIPO and associated species:
#

# what species do we have?
treesfcnotrtmt$SPCD<- factor(treesfcnotrtmt$SPCD)
summary(treesfcnotrtmt)
# how many?
length(unique(treesfcnotrtmt$SPCD)) #57 species
table(treesfcnotrtmt$SPCD)
unique(treesfcnotrtmt$SPCD)

#Create dataframe that contains plots that have had PIPO at any time, and the associated species
# I went through the FIA metadata and made a spreadsheet of what all the SPCD codes listed 
# from "unique(nm_tree5$SPCD)" were. I can send that over to you if you want. There are more than 
# I initially wrote in my proposal because for example there are 5 species of juniper found within 
# the 4 states- redberry, alligator, utah, and rocky mountain juniper. So I think what the code 
# below does is even if PIPO was removed from a plot and other species are present, that plot will 
# not be removed. 


#subset of PIPO, keeping SUCP
pipoplots<- unique(treesfcnotrtmt[treesfcnotrtmt$SPCD==122,]$SUCP)

#Create dataframe that contains plots that have had PIPO at any time, and the associated species
pipoplots2<- treesfcnotrtmt %>% filter(SPCD==122|(SPCD==814 & SUCP%in%pipoplots|
                                                    SPCD==15 & SUCP%in%pipoplots |
                                                    SPCD==18 & SUCP%in%pipoplots |
                                                    SPCD==19 & SUCP%in%pipoplots |
                                                    SPCD==51 & SUCP%in%pipoplots |
                                                    SPCD==59 & SUCP%in%pipoplots |
                                                    SPCD==63 & SUCP%in%pipoplots |
                                                    SPCD==65 & SUCP%in%pipoplots | 
                                                    SPCD==66 & SUCP%in%pipoplots |
                                                    SPCD==69 & SUCP%in%pipoplots |
                                                    SPCD==93 & SUCP%in%pipoplots |
                                                    SPCD==96 & SUCP%in%pipoplots |
                                                    SPCD==102 & SUCP%in%pipoplots |
                                                    SPCD==106 & SUCP%in%pipoplots |
                                                    SPCD==108 & SUCP%in%pipoplots |
                                                    SPCD==113 & SUCP%in%pipoplots |
                                                    SPCD==114 & SUCP%in%pipoplots |
                                                    SPCD==118 & SUCP%in%pipoplots |
                                                    SPCD==133 & SUCP%in%pipoplots |
                                                    SPCD==134 & SUCP%in%pipoplots |
                                                    SPCD==140 & SUCP%in%pipoplots |
                                                    SPCD==142 & SUCP%in%pipoplots |
                                                    SPCD==143 & SUCP%in%pipoplots |
                                                    SPCD==202 & SUCP%in%pipoplots |
                                                    SPCD==313 & SUCP%in%pipoplots |
                                                    SPCD==322 & SUCP%in%pipoplots |
                                                    SPCD==353 & SUCP%in%pipoplots |
                                                    SPCD==362 & SUCP%in%pipoplots |
                                                    SPCD==374 & SUCP%in%pipoplots |
                                                    SPCD==475 & SUCP%in%pipoplots |
                                                    SPCD==746 & SUCP%in%pipoplots |
                                                    SPCD==122 & SUCP%in%pipoplots |
                                                    SPCD==547 & SUCP%in%pipoplots |
                                                    SPCD==606 & SUCP%in%pipoplots |
                                                    SPCD==732 & SUCP%in%pipoplots |
                                                    SPCD==749 & SUCP%in%pipoplots |
                                                    SPCD==803 & SUCP%in%pipoplots |
                                                    SPCD==810 & SUCP%in%pipoplots |
                                                    SPCD==826 & SUCP%in%pipoplots |
                                                    SPCD==843 & SUCP%in%pipoplots))

summary(pipoplots2)
unique(pipoplots2$SPCD)
table(pipoplots2$SPCD)
length(unique(pipoplots2$SPCD)) # 40 associate species, it removed plots with 17 other species



length(unique(pipoplots2$SUCP)) #1622 plots
#check
pipock<- pipoplots2 %>% filter(cycle_aw==1)
length(unique(pipock$SUCP)) # 1568
pipock2<- pipoplots2 %>% filter(cycle_aw==2)
length(unique(pipock2$SUCP)) # 1622
# more in cycle 2 than 1


# random old checking:
aztreeck<- pipoplots2 %>% filter(STATECD==4)
length(unique(aztreeck$SUCP)) # 609
aztreeck2<- aztreeck %>% filter(cycle_aw==1)
length(unique(aztreeck2$SUCP)) #602
aztreeck3<- aztreeck %>% filter(cycle_aw==2)
length(unique(aztreeck3$SUCP)) #609. 7 more in cycle 2


cotreeck<- pipoplots2 %>% filter(STATECD==8)
length(unique(cotreeck$SUCP)) # 440
cotreeck2<- cotreeck %>% filter(cycle_aw==1)
length(unique(cotreeck2$SUCP)) #410
cotreeck3<- cotreeck %>% filter(cycle_aw==2)
length(unique(cotreeck3$SUCP)) #440 30 more in cycle 2

nmtreeck<- pipoplots2 %>% filter(STATECD==35)
length(unique(nmtreeck$SUCP)) # 315
nmtreeck2<- nmtreeck %>% filter(cycle_aw==1)
length(unique(nmtreeck2$SUCP)) #311
nmtreeck3<- nmtreeck %>% filter(cycle_aw==2)
length(unique(nmtreeck3$SUCP)) #315    4 more in cycle 2

uttreeck<- pipoplots2 %>% filter(STATECD==49)
length(unique(uttreeck$SUCP)) # 156
uttreeck2<- uttreeck %>% filter(cycle_aw==1)
length(unique(uttreeck2$SUCP)) #147
uttreeck3<- uttreeck %>% filter(cycle_aw==2)
length(unique(uttreeck3$SUCP)) #156    12 more in cycle 2

# use pipoplots2

write.csv(pipoplots2, "4corners123Results.csv")


####################################################################################################


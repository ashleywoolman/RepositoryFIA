

# All 4 states
#  I. Initial Cleaning steps:
# 1. Filter INVYR >1999, create cycle_aw for each state, create SUCP
# 2. Remove plots with 99 subcycle
# 3. Filter for only resampled plots * new way*
# 4. Filter for PIPO and associated species
# 5. Remove plots with STATUSCD 0 and change STATUSCD 3 to 2
# 6. Size class column
# 7. Create equal number rows cycle, size class, status: 16 without seedlings, 20 with seedligns
# 8. Merge with 'pipotesty' to fill in missing NAs

# II. Link PLT_CN to PLOT table's MEASYR

# III. Calculate BA and TPA. 
# -> BAandTPApipo.csv



library(tidyverse)
library(dplyr)



# Only need to do this step first time. After, always load in AllStates_TREE_RAW.csv
uttree<- read.csv("UT_TREE.csv")
cotree<- read.csv("CO_TREE.csv")
aztree<- read.csv("AZ_TREE.csv")
nmtree<- read.csv("NM_TREE.csv")
fctree<- rbind(uttree, cotree, aztree, nmtree)
# Edit columns we need
fctree1<- fctree[, c(1:4, 123, 124, 5:11, 14:18, 65, 19:21, 27, 66, 25, 35)]                                    
write.csv(fctree1, "AllStates_TREE_RAW.csv")

# load raw tree data
fctree1<- read.csv("AllStates_TREE_RAW.csv", row.names = 1)


##
# Annual data only: filter for INVYR >1999
fctree2<- fctree1 %>% filter(INVYR>1999)

# Create cycle_aw for each state
fctree3<- fctree2 %>% mutate(cycle_aw = case_when(STATECD==49 & CYCLE==2 ~ '2', 
                              STATECD==49 & CYCLE==3 ~ '3',
                              STATECD==8 & CYCLE==2 ~ '2',
                              STATECD==8 & CYCLE==3 ~ '3',
                              STATECD==4 & CYCLE==3 ~ '2',
                              STATECD==4 & CYCLE==4 ~ '3',
                              STATECD==35 & CYCLE==3 ~ '2',
                              STATECD==35 & CYCLE==4 ~ '3'))
fctree3$cycle_aw<- factor(fctree3$cycle_aw)

# Create new variable that pastes together state, unit, countycd, plot: "SUCP"
cols<- c('STATECD', 'UNITCD', 'COUNTYCD', 'PLOT')
fctree3$SUCP<- apply(fctree3[,cols], 1, paste, collapse="_")
fctree3$SUCP<- factor(fctree3$SUCP)
length(unique(fctree3$SUCP)) # 14,669

summary(fctree3)

# Remove plots with 99 as subcycle...
fctree4<- fctree3[!(fctree3$SUBCYCLE==99),]
summary(fctree4)
length(unique(fctree4$SUCP)) #14655



## Filter for resampled plots. The old method did not work.
summary(fctree4)
cyclefilt<- fctree4[,c(28,7,27)]
summary(cyclefilt)
cyclefilt2<- cyclefilt %>% distinct()
length(unique(cyclefilt2$SUCP)) #14655 plots
summary(cyclefilt2)

cycle2<- cyclefilt2 %>% filter(cycle_aw==2)
length(unique(cycle2$SUCP)) #13846
summary(cycle2)
cycle2<- cycle2[,c(1,2)]
summary(cycle2)

cycle3<- cyclefilt2 %>% filter(cycle_aw==3)
length(unique(cycle3$SUCP)) #10296
summary(cycle3)
cycle3<- cycle3[,c(1,2)]
summary(cycle3)

remeasured<- merge(cycle2, cycle3, all=FALSE)
length(unique(remeasured$SUCP)) #9487 REMEASURED
summary(remeasured)


# Merge with (fctree4)
summary(fctree4)
length(unique(fctree4$SUCP)) #14655 plots
fctree5<- merge(remeasured, fctree4, all.x = TRUE)
summary(fctree5)
length(unique(fctree5$SUCP)) #9487 REMEASURED

# re-order columns
fctree6<- fctree5[,c(1,2,28,6:8,14,16,17,20)]
length(unique(fctree6$SUCP)) # total 9487 plots
summary(fctree6)

#
## Select plots with PIPO and associated species:
#

# what species do we have?
fctree6$SPCD<- factor(fctree6$SPCD)
fctree6$STATUSCD<- factor(fctree6$STATUSCD)

summary(fctree6)
# how many?
length(unique(fctree6$SPCD)) #54 species
table(fctree6$SPCD)
unique(fctree6$SPCD)
list(fctree6$SPCD)
levels(fctree6$SPCD)


# First make dataframe of plots with PIEN/sp at any time AND the other species

#subset of PIPO, keeping SUCP
pipoplots<- unique(fctree6[fctree6$SPCD==93,]$SUCP)

#Create dataframe that contains plots that have had PIPO at any time, and the associated species
allspecies<- fctree6 %>% filter(SPCD==93|(SPCD==15 & SUCP%in%pipoplots |
                                          SPCD==18 & SUCP%in%pipoplots |
                                          SPCD==19 & SUCP%in%pipoplots |
                                          SPCD==51 & SUCP%in%pipoplots |
                                          SPCD==58 & SUCP%in%pipoplots |
                                          SPCD==59 & SUCP%in%pipoplots |
                                          SPCD==62 & SUCP%in%pipoplots |
                                          SPCD==63 & SUCP%in%pipoplots |
                                          SPCD==65 & SUCP%in%pipoplots | 
                                          SPCD==66 & SUCP%in%pipoplots |
                                          SPCD==69 & SUCP%in%pipoplots |
                                          SPCD==93 & SUCP%in%pipoplots |
                                          SPCD==96 & SUCP%in%pipoplots |
                                          SPCD==102 & SUCP%in%pipoplots |
                                          SPCD==106 & SUCP%in%pipoplots |
                                          SPCD==108 & SUCP%in%pipoplots |
                                          SPCD==112 & SUCP%in%pipoplots |
                                          SPCD==113 & SUCP%in%pipoplots |
                                          SPCD==114 & SUCP%in%pipoplots |
                                          SPCD==118 & SUCP%in%pipoplots |
                                          SPCD==122 & SUCP%in%pipoplots |
                                          SPCD==133 & SUCP%in%pipoplots |
                                          SPCD==134 & SUCP%in%pipoplots |
                                          SPCD==135 & SUCP%in%pipoplots |
                                          SPCD==140 & SUCP%in%pipoplots |
                                          SPCD==142 & SUCP%in%pipoplots |
                                          SPCD==143 & SUCP%in%pipoplots |
                                          SPCD==202 & SUCP%in%pipoplots |
                                          SPCD==313 & SUCP%in%pipoplots |
                                          SPCD==322 & SUCP%in%pipoplots |
                                          SPCD==353 & SUCP%in%pipoplots |
                                          SPCD==362 & SUCP%in%pipoplots |
                                          SPCD==374 & SUCP%in%pipoplots |
                                          SPCD==461 & SUCP%in%pipoplots |
                                          SPCD==462 & SUCP%in%pipoplots |
                                          SPCD==475 & SUCP%in%pipoplots |
                                          SPCD==544 & SUCP%in%pipoplots |
                                          SPCD==547 & SUCP%in%pipoplots |
                                          SPCD==552 & SUCP%in%pipoplots |
                                          SPCD==606 & SUCP%in%pipoplots |
                                          SPCD==732 & SUCP%in%pipoplots |
                                          SPCD==742 & SUCP%in%pipoplots |
                                          SPCD==745 & SUCP%in%pipoplots |
                                          SPCD==746 & SUCP%in%pipoplots |
                                          SPCD==748 & SUCP%in%pipoplots |
                                          SPCD==749 & SUCP%in%pipoplots |
                                          SPCD==756 & SUCP%in%pipoplots |
                                          SPCD==757 & SUCP%in%pipoplots |
                                          SPCD==803 & SUCP%in%pipoplots |
                                          SPCD==810 & SUCP%in%pipoplots |
                                          SPCD==814 & SUCP%in%pipoplots |
                                          SPCD==826 & SUCP%in%pipoplots |
                                          SPCD==829 & SUCP%in%pipoplots |
                                          SPCD==843 & SUCP%in%pipoplots |
                                          SPCD==846 & SUCP%in%pipoplots |
                                          SPCD==847 & SUCP%in%pipoplots |
                                          SPCD==974 & SUCP%in%pipoplots))
length(unique(allspecies$SPCD)) #21 associate species
length(unique(allspecies$SUCP)) #1177 plots
summary(allspecies)

# Edit to keep only the plot info of the plots with PIEN/sp at any time AND the other species
allspecies2<- allspecies[, c(1,2,3,4,5,6)] 
allspecies3<- allspecies2 %>% distinct()
summary(allspecies3)
length(unique(allspecies3$SUCP)) #1177

# Now filter for only PIEN/sp . This filter likely creates a dataframe that does not have all plots retained (if PIEN/sp died in second cycle, it wouldnt keep it)
species<- fctree6 %>% filter(SPCD==93)
summary(species)
length(unique(species$SUCP)) #1177

# Now merge the plot info list (allspecies3 with only pien/sp (species))
species2<- merge(allspecies3, species, all.x=TRUE )
summary(species2)



testit<- species2 %>% group_by(SUCP, STATECD, SUBCYCLE, CYCLE, INVYR, cycle_aw) %>% summarise(test=sum(DIACALC))
summary(testit)



# This is about where without removing treatment plots, the script "1.Newall4_061820_WithSubCycle" ended.



# Remove plots with any STATUSCD 0s.
code0<- species2 %>% filter(STATUSCD==0)
length(unique(code0$SUCP)) #69 plots

# Remove statuscd 0 plots
fctree8<- species2[!(species2$SUCP %in% code0$SUCP),]
length(unique(fctree8$SUCP)) # 1177-69=1108, this worked
summary(fctree8)
# there are no STATUSCD 0 and there are 791 code 3

# Change STATUSCD 3 to 2 (dead) 
code3<- fctree8 %>% filter(STATUSCD==3)
length(unique(code3$SUCP)) #17

fctree8$STATUSCD [fctree8$STATUSCD == "3"] <- 2
summary(fctree8)
fctree9<- droplevels(fctree8)
summary(fctree9)
length(unique(fctree9$SUCP)) # 1474


testit<- fctree9 %>% group_by(SUCP, STATECD, SUBCYCLE, CYCLE, INVYR, cycle_aw) %>% summarise(test=sum(DIACALC))
summary(testit) # 1474 plots both cycles



# Create size class column
# 
# Seedlings <1     A
# Saplings "<5.0"  B
# Trees 5-9.9      C
# Trees 10-14.9    D
# Trees >15        E

fctree9$SizeClass<- NA
fctree9$SizeClass[fctree9$DIACALC <1]<- 'A'
fctree9$SizeClass[fctree9$DIACALC >=1 & fctree9$DIACALC<5]<- 'B'
fctree9$SizeClass[fctree9$DIACALC >=5 & fctree9$DIACALC<10]<- 'C'
fctree9$SizeClass[fctree9$DIACALC >=10 & fctree9$DIACALC <15] <- 'D'
fctree9$SizeClass[fctree9$DIACALC >=15] <- 'E'
fctree9$SizeClass<- factor(fctree9$SizeClass)
summary(fctree9)

library(tidyr)
# need equal amount rows for cycle and sizelcass ( 6 rows per plot). 6 rows whether or not there was a 
# tree in the size class
# Expand/nest/
# every plot should have 24- 4(size classe) * 2(status)=8 * 2(cycles)=16


pipo2<- fctree9 %>% expand(nesting(STATECD, SUCP, SPCD ), cycle_aw, STATUSCD, SizeClass) 
table(pipo2$SUCP)
pipo2 %>% anti_join(fctree9) 
pipo3<- fctree9 %>% right_join(pipo2)
summary(pipo3)

# missing INVYR for many plots. delete it now and add back in. 
pipo4<- pipo3[, c(1,2,3,8:11)] 
summary(pipo4)

# testit 
summary(testit) 
pipotesty<- testit[, c(1:6)] 
summary(pipotesty)
pipotesty2<- pipotesty %>% distinct()
length(unique(pipotesty2$SUCP)) #1108 plots
summary(pipotesty2)

# Remove CYLCE, SUBCYLCE, INVYR from pipo3 and merge pipo3 with an original df that contains SUCP, CYCLE, SUBCYCLE, INVYR, and merge by cycle_aw
pipo5<- merge (pipo4, pipotesty2, by=c("SUCP", "STATECD", "cycle_aw"))
pipo5$DIACALC[is.na(pipo5$DIACALC)]<-0
pipo5$SPCD[is.na(pipo5$SPCD)]<-93 #### ****** FILL IN FOR PROPER SPECIES CODE***
summary(pipo5)
length(unique(pipo5$SUCP)) #1108

# TEST: (dont need to run) Get tree count per size class
pipo5$treeexist<- as.logical(pipo5$DIACALC)
pipo5test<- pipo5 %>% group_by(SUCP, STATECD, cycle_aw,
                           SPCD, STATUSCD, SizeClass) %>% summarise(Treecount=sum(treeexist))
length(unique(pipo5test$SUCP)) #1108

# This works because 2*2*4=16. 23584/16= 1474 plots.

summary(pipo5test)


# Use Plot table data to get MEASYR variable 

# Can link PLT_CN to PLOT table's MEASYR
azplot<- read.csv("AZ_PLOT.csv")
coplot<- read.csv("CO_PLOT.csv")
NMplot<- read.csv("NM_PLOT.csv")
UTplot<- read.csv("UT_PLOT.csv")
fcplot<- rbind(azplot, coplot, NMplot, UTplot)
summary(fcplot)
# Edit columns we need
fcplot2<- fcplot[, c(1, 5:9, 12:14,17,22,45,46)]                    
summary(fcplot2)
write.csv(fcplot2, "AllStates_PLOT_RAW.csv")

# Load
fcplot2<- read.csv("AllStates_PLOT_RAW.csv", row.names = 1)
summary(fcplot2)
# *** this has latlong coords, CN, SRV_CN, CTY_CN

# Create SUCP
cols<- c('STATECD', 'UNITCD', 'COUNTYCD', 'PLOT')
fcplot2$SUCP<- apply(fcplot2[,cols], 1, paste, collapse="_")
fcplot2$SUCP<- factor(fcplot2$SUCP)
length(unique(fcplot2$SUCP)) # 82862

# Annual data only: filter for INVYR >1999
fcplot3<- fcplot2 %>% filter(INVYR>1999)

# Create cycle_aw for each state
fcplot4<- fcplot3 %>% mutate(cycle_aw = case_when(STATECD==49 & CYCLE==2 ~ '2', 
                                                  STATECD==49 & CYCLE==3 ~ '3',
                                                  STATECD==8 & CYCLE==2 ~ '2',
                                                  STATECD==8 & CYCLE==3 ~ '3',
                                                  STATECD==4 & CYCLE==3 ~ '2',
                                                  STATECD==4 & CYCLE==4 ~ '3',
                                                  STATECD==35 & CYCLE==3 ~ '2',
                                                  STATECD==35 & CYCLE==4 ~ '3'))
fcplot4$cycle_aw<- factor(fcplot4$cycle_aw)
summary(fcplot4)

# Remove plots with 99 as subcycle...
fcplot5<- fcplot4[!(fcplot4$SUBCYCLE==99),]
summary(fcplot5)
length(unique(fcplot5$SUCP)) 

# Edit columns we need
fcplot6<- fcplot5[, c(14,3,2,7,15,10,12,13)] 
summary(fcplot6)
length(unique(fcplot6$SUCP)) # tons plots- 45781

# plot list from trees:
summary(pipotesty2)

plotinfo<- merge(pipotesty2, fcplot6, all.x = TRUE)
summary(plotinfo)
length(unique(plotinfo$SUCP)) #1108 plots


plotinfo$SUCP<- factor(plotinfo$SUCP)
plotinfo$STATECD<- factor(plotinfo$STATECD)
plotinfo$CYCLE<- factor(plotinfo$CYCLE)
plotinfo$SUBCYCLE<- factor(plotinfo$SUBCYCLE)
summary(plotinfo)



summary(pipo5)
pipo6<- merge (pipo5, plotinfo, by=c("SUCP", "STATECD", "cycle_aw", "CYCLE", "SUBCYCLE", "INVYR"))
length(unique(pipo6$SUCP)) #1108 plots
summary(pipo6)

pipo6$STATECD<- factor(pipo6$STATECD)
pipo6$STATUSCD<- factor(pipo6$STATUSCD)
pipo6$SPCD<- factor(pipo6$SPCD)
summary(pipo6)

# TEST: (dont need to run) Get tree count per size class
pipo6$treeexist<- as.logical(pipo6$DIACALC)
pipo6test<- pipo6 %>% group_by(SUCP, STATECD, cycle_aw, STATUSCD, 
                           SPCD, SizeClass) %>% summarise(Treecount=sum(treeexist))
length(unique(pipo6test$SUCP)) #1474
summary(pipo6test)

# edit columns
pipo7 <- pipo6[,c(1,2,3,12,13, 6,7,10,8,9)]
summary(pipo7)



#
##### Trees per acre
#
summary(pipo7)
# Sum tree count per plot by size classes
pipo7$treeexist<- as.logical(pipo7$DIACALC)
TPApipo<- pipo7 %>% group_by(SUCP, STATECD, INVYR, MEASYEAR, cycle_aw, SPCD, STATUSCD, SizeClass) %>% summarise(Treecount=sum(treeexist))
summary(TPApipo)
length(unique(TPApipo$SUCP)) #1108 plots

# For subplot, I calculated expansion factor with (4*0.04154172) = 6.018046, where 4 is the number
# of subplots in plot, and 0.04154172 is the area of a 1/24acre subplot. 
# Expan Factor for saplings (microplot)= 74.96528

TPApipo$ExpanFactor = ifelse(TPApipo$SizeClass=="B", 74.96528, 6.018046)
summary(TPApipo)

TPApipo$TPA<- TPApipo$Treecount * TPApipo$ExpanFactor
summary(TPApipo)

# Remove expan factor variable
TPApipo2 <- TPApipo[,-10] 
summary(TPApipo2)

# TPA: TPApipo2



#
##### Basal Area
#
pipo7 <- pipo6[,c(1,2,3,12,13, 6,7,10,8,9)]
summary(pipo7) #(rerun to get rid of treeexist)

# To determine BA per tree: DBH2 (SQUARED) * 0.005454. = FT2
pipo7$BA_Tree_Sqft<- pipo7$DIACALC^2*0.005454

# Expan Factor
pipo7$ExpanFactor = ifelse(pipo7$SizeClass=="B", 74.96528, 6.018046)
summary(pipo7)
# Per acre, to be able to add all trees per plot together & compare, *** WITHIN TREE SIZE PLOT. NOT SAPLINGS!
pipo7$BA_PerAcre_Sqft<- pipo7$BA_Tree_Sqft*pipo7$ExpanFactor

summary(pipo7)

# Sum BA per plot, sizeclass, statuscd, in BA/acre/ft2
BApipo<- pipo7 %>% group_by(SUCP, STATECD, INVYR, MEASYEAR, cycle_aw, SPCD, STATUSCD, SizeClass) %>% 
  summarise(BAtotal_AcreFt2=sum(BA_PerAcre_Sqft))
summary(BApipo)

length(unique(BApipo$SUCP)) #1108

#
## Merge BA and TPA together
#

##** Merge with TPA **********

# TPAalltreeslive7 is from "AllTreeSizes_TPA"
BAandTPApipo<- merge (TPApipo2, BApipo, by=c("SUCP", "STATECD","INVYR", "MEASYEAR", "cycle_aw", "SPCD", "STATUSCD", "SizeClass"))
summary(BAandTPApipo)
BAandTPApipo$SPCD[is.na(BAandTPApipo$SPCD)]<-93 # *** if there are still any NA's change species code
summary(BAandTPApipo)


write.csv(BAandTPApipo, "BAandTPApipo.csv")

# add seedlings and fire data from JS

library(ggplot2)
ggplot(BAandTPApipo, aes(x=cycle_aw, y=TPA, color=SizeClass, linetype=STATUSCD))+
  facet_grid(~SizeClass)+
  geom_smooth(aes(group=STATUSCD), method = "glm", size=1, colour="black")+
  theme_set(theme_bw())+
  labs(x= "Inventory Cycle")+
  labs(y= "TPA")+
  theme(axis.title.y = element_text(size=15))+
  theme(strip.text=element_text(size=10))+
  ggtitle("PIPO Saplings and Trees")





mtbs<- read.csv("Severity_MTBS_1984_2018.csv")
summary(mtbs)

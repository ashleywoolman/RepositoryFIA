




### Only Annual Seedlings

summary(BAandTPApipo)
# Merge ^ with PLT_CN from 'withunit'

# 1.  Create plot list of 1474 plots
all4plotlist<- BAandTPApipo[, c(1,2,3,4,5,6)]
all4plotlist2<- all4plotlist %>% distinct()
summary(all4plotlist2)
length(unique(all4plotlist2$SUCP)) # 1108



allseed<- read.csv("All4Cseedlings_RAW.csv",row.names = 1)

allseed2<- allseed %>% filter(INVYR>1999)

allseed3<- allseed2 %>% mutate(cycle_aw = case_when(STATECD==49 & CYCLE==2 ~ '2', 
                                                  STATECD==49 & CYCLE==3 ~ '3',
                                                  STATECD==8 & CYCLE==2 ~ '2',
                                                  STATECD==8 & CYCLE==3 ~ '3',
                                                  STATECD==4 & CYCLE==3 ~ '2',
                                                  STATECD==4 & CYCLE==4 ~ '3',
                                                  STATECD==35 & CYCLE==3 ~ '2',
                                                  STATECD==35 & CYCLE==4 ~ '3'))


### create new variable that pastes together state, unit, countycd, plot: "SUCP"
cols<- c('STATECD', 'UNITCD', 'COUNTYCD', 'PLOT')
allseed3$SUCP<- apply(allseed3[,cols], 1, paste, collapse="_")
allseed3$SUCP<- factor(allseed3$SUCP)
summary(allseed3)


# Delete columns dont need now-
# These are the columns you want to keep
# Including subplot and condition (need to add up all subplots tgoether per plot)
allseed4<- allseed3[,c(1,2,3,7,8,9,20,22,23,32,33)]
summary(allseed4)

# re-order columns
allseed5<- allseed4[,c(1,11,3,2,10,8,9,4,5,6,7)]
summary(allseed5)

# tree count by plot
allseed6<- allseed5 %>% group_by(PLT_CN, SUCP, STATECD, INVYR, cycle_aw, CYCLE, SUBCYCLE, SPCD) %>%
  summarise(Treecount = sum(TREECOUNT_CALC))
summary(allseed6) 

# filter pipo
allseed7<-  allseed6 %>% filter(SPCD==93)

# change PLT_CN to CN
colnames(allseed7)[colnames(allseed7) == "PLT_CN"] <- "CN"
allseed8<- merge(all4plotlist2, allseed7, all.x = TRUE)

# ^ that got rid of subycle 99 plots

# Clean up, delete subycle and cycle now
summary(allseed8) 
allseed9<- allseed8[,c(1:6,10)]

#Create values for NA's
allseed9$Treecount[is.na(allseed9$Treecount)]<-0
summary(allseed9) 


# To match other files, need to have a STATUSCD, SizeClass
# STATUSCD:
allseed9$STATUSCD<- as.logical(allseed9$cycle_aw)
allseed9$STATUSCD[is.na(allseed9$STATUSCD)]<-1

# SizeClass
# Seedlings are <1.0 inch DBH
allseed9$SizeClass<- as.logical(allseed9$cycle_aw)
allseed9$SizeClass[is.na(allseed9$SizeClass)]<- 'A'
summary(allseed9)

allseed9$SPCD<- factor(allseed9$SPCD)
allseed9$STATUSCD<- factor(allseed9$STATUSCD)
allseed9$SizeClass<- factor(allseed9$SizeClass)
summary(allseed9)

allseed10<- allseed9 %>% distinct()
summary(allseed10)



## Create rows for DEAD seedlings (0) but to match all other data
summary(BAandTPApipo)
deadseed<- BAandTPApipo %>% filter(STATUSCD==2 & SizeClass=="B")
summary(deadseed)

deadseed2<- deadseed[, c(1:8)]
summary(deadseed2)
deadseed3<- deadseed2 %>% distinct()
summary(deadseed3)

# Create A
deadseed3$SizeClass = ifelse(deadseed3$STATUSCD==2,1,0)
deadseed3$SizeClass[deadseed3$SizeClass=="1"]<- "A"
deadseed3$SizeClass<- factor(deadseed3$SizeClass)
summary(deadseed3)
length(unique(deadseed3$SUCP)) #1472
deadseed3$Treecount = ifelse(deadseed3$STATUSCD==2,0,1)
summary(deadseed3)
deadseed3$SPCD<- factor(deadseed3$SPCD)
deadseed3$STATUSCD<- factor(deadseed3$STATUSCD)
summary(deadseed3)

summary(deadseed3) #they got duplicated!!!!!!
deadseed4<- deadseed3 %>% distinct()
summary(deadseed4) #numbers match now.

summary(deadseed4)

# Merge 'fake dead seedling data' with live seedlig data
allseed12<- merge(allseed10, deadseed4, all = TRUE)
summary(allseed11)
allseed12<- allseed11 %>% distinct()
summary(allseed12)


# Calculate TPA
#  Create column for expansion factor - 
allseed12$ExpanFactor<- as.logical(allseed12$cycle_aw)
allseed12$ExpanFactor[is.na(allseed12$ExpanFactor)]<-74.96528
summary(allseed12)

allseed12$TPA<- (allseed12$Treecount*allseed12$ExpanFactor)
summary(allseed12)

# Clean up columns
allseed13<- allseed12[, c(1:9,11)]
summary(allseed12)


summary(BAandTPApipo)
dim(BAandTPApipo)

BAandTPApipo2<- merge(allseed12, BAandTPApipo, all=TRUE)

# Change seedlings BA to 0
BAandTPApipo2$BAtotal_AcreFt2[is.na(BAandTPApipo2$BAtotal_AcreFt2)]<-0
summary(BAandTPApipo2)

ggplot(BAandTPApipo2, aes(x=cycle_aw, y=Treecount, linetype=STATUSCD))+
  facet_grid(~SizeClass)+
  geom_smooth(aes(group=STATUSCD), method = "glm", size=1)+
  theme_set(theme_bw())+
  labs(x= "Inventory Cycle")+
  labs(y= "Num Trees/Plot")+
  theme(axis.title.y = element_text(size=15))+
  theme(strip.text=element_text(size=10))


# To identify fire:
####
###########

fcCOND<- read.csv("AllStates_COND_RAW.csv",  row.names = 1)
# 1. get variables I want and create SUCP for annual
fcCOND2<- fcCOND[, c(1:8,38:43)]
summary(fcCOND2)

# Create SUCP
SUCP<- c('STATECD', 'UNITCD', 'COUNTYCD', 'PLOT')
fcCOND2$SUCP<- apply(fcCOND2[,SUCP], 1, paste, collapse="_")
fcCOND2$SUCP<- factor(fcCOND2$SUCP)
length(unique(fcCOND2$SUCP)) #82862 PLOTS
summary(fcCOND2)


# need plot list by cycle/year
summary(BAandTPApipo2)
plotlist<- BAandTPApipo2[, c(1:3)]
plotlist2<- plotlist %>% distinct()
summary(plotlist2)

fcCOND3<- merge(plotlist2, fcCOND2, all.x = TRUE)
summary(fcCOND3)
length(unique(fcCOND3$SUCP))   #1474           

# 2. More cleaning of variables I want
fcCOND4<- fcCOND3[, c(1:3,10,11)]
summary(fcCOND4)

# 3. Drop NAs
fcCOND5<- fcCOND4 %>% drop_na(DSTRBCD1, DSTRBYR1)
library(dplyr)
summary(fcCOND5)

# 4. Filter for fire between cycle 2 and 3 
#   For AZ(4)  fire year greater than 1999 and less than 2015
#  For NM (35) fire year greater than 1999 and less than 2011 
#  DSTRBCD 30, 31, and 32

fcCOND6<- fcCOND5 %>% filter(DSTRBCD1==30| DSTRBCD1==31| DSTRBCD1==32)

fcCOND7<- filter(fcCOND6, (STATECD==4 & DSTRBYR1> 2001 & DSTRBYR1<2011 |
                             STATECD==35 & DSTRBYR1>2008 & DSTRBYR1<2014 |
                             STATECD==8 & DSTRBYR1>2002 & DSTRBYR1<2012 |
                             STATECD==49 & DSTRBYR1>2000 & DSTRBYR1 <2010))

# but this excludes fires outside of these boundaries.... 

summary(fcCOND7)

# 5. Lump all fire DSTRBCD to 1 
fcCOND7$DSTRBCD1 [fcCOND7$DSTRBCD1 >29| fcCOND7$DSTRBCD1<33] <- "1"
fcCOND7$DSTRBCD1<- factor(fcCOND7$DSTRBCD1)
summary(fcCOND7)
duplicated(fcCOND7)
fcCOND8<- fcCOND7 %>% distinct()
summary(fcCOND8)
fcCOND9<- fcCOND8[, c(1,2,4,5)]
summary(fcCOND9)
fcCOND10<- fcCOND9 %>% distinct()
summary(fcCOND10)

#### 6. Merge with plot list
summary(plotlist2)

fcCOND11<- merge(plotlist2, fcCOND10, all.x = TRUE)
summary(fcCOND11)
fcCOND12<- fcCOND11 %>% distinct()
summary(fcCOND12)

length(unique(fcCOND8$SUCP))
#check
firetest<- fcCOND8 %>% filter(DSTRBCD1==1)
length(unique(firetest$SUCP)) #109 burned plots


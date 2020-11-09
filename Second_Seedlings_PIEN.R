
# Seedlings and Fire 



###  Annual Seedlings

summary(BAandTPApipo)

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
allseed11<- merge(allseed10, deadseed4, all = TRUE)
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

BAandTPApipo3<- BAandTPApipo2[, c(1:10,12)]
summary(BAandTPApipo3)

# save final df
write.csv(BAandTPApipo3, "PIEN.csv")




# Change names for graphs

levels(BAandTPApipo2$SizeClass)
levels(BAandTPApipo2$SizeClass)<- c("Seedling", "Sapling", "Small Tree: 5-10in", 
                                 "Medium Tree: 10-15in", "Large Tree: >15")


levels(BAandTPApipo2$STATUSCD)
levels(BAandTPApipo2$STATUSCD)<- c("Live", "Dead")
summary(BAandTPApipo2)
length(unique(BAandTPApipo2$SUCP))



ggplot(BAandTPApipo2, aes(x=cycle_aw, y=Treecount, linetype=STATUSCD))+
  facet_grid(~SizeClass)+
  geom_smooth(aes(group=STATUSCD), method = "glm", size=1)+
  theme_set(theme_bw())+
  labs(x= "Inventory Cycle")+
  labs(y= "Number Trees/Plot")+
  theme(axis.title.y = element_text(size=15))+
  theme(strip.text=element_text(size=10))+
  theme(legend.title = element_blank())

ggplot(BAandTPApipo2, aes(x=cycle_aw, y=TPA, linetype=STATUSCD))+
  facet_grid(~SizeClass)+
  geom_smooth(aes(group=STATUSCD), method = "glm", size=1)+
  theme_set(theme_bw())+
  labs(x= "Inventory Cycle")+
  labs(y= "Trees Per Acre")+
  theme(axis.title.y = element_text(size=15))+
  theme(strip.text=element_text(size=10))+
  theme(legend.title = element_blank())

ggplot(BAandTPApipo2, aes(x=cycle_aw, y=BAtotal_AcreFt2, linetype=STATUSCD))+
  facet_grid(~SizeClass)+
  geom_smooth(aes(group=STATUSCD), method = "glm", size=1)+
  theme_set(theme_bw())+
  labs(x= "Inventory Cycle")+
  labs(y= "Basal Area (ft2/acre")+
  theme(axis.title.y = element_text(size=15))+
  theme(strip.text=element_text(size=10))+
  theme(legend.title = element_blank())









### Add Fire

AZcond<- read.csv("AZ_COND.csv") 
COcond<- read.csv("CO_COND.csv")
UTcond<- read.csv("UT_COND.csv")
NMcond<- read.csv("NM_COND.csv")
fcCOND<- rbind(UTcond, AZcond, COcond, NMcond)

write.csv(fcCOND, "AllStates_COND_RAW.csv")

# Start with only post 2000 (for SUCP)
# then do 1999 with PLT_CN

fcCOND<- read.csv("AllStates_COND_RAW.csv", row.names = 1)
summary(fcCOND)

# get variables I want
fcCOND2<- fcCOND[, c(3:7,38:43,79)]
summary(fcCOND2)

# Create SUCP
SUCP<- c('STATECD', 'UNITCD', 'COUNTYCD', 'PLOT')
fcCOND2$SUCP<- apply(fcCOND2[,SUCP], 1, paste, collapse="_")
fcCOND2$SUCP<- factor(fcCOND2$SUCP)
length(unique(fcCOND2$SUCP)) #1258 PLOTS
summary(fcCOND2)

# create cycle_aw
fcCOND3<- fcCOND2 %>% mutate(cycle_aw = case_when(STATECD==49 & CYCLE==2 ~ '2', 
                                                    STATECD==49 & CYCLE==3 ~ '3',
                                                    STATECD==8 & CYCLE==2 ~ '2',
                                                    STATECD==8 & CYCLE==3 ~ '3',
                                                    STATECD==4 & CYCLE==3 ~ '2',
                                                    STATECD==4 & CYCLE==4 ~ '3',
                                                    STATECD==35 & CYCLE==3 ~ '2',
                                                    STATECD==35 & CYCLE==4 ~ '3'))
fcCOND3$cycle_aw<- factor(fcCOND3$cycle_aw)
summary(fcCOND3)


# Filter for fire disturbances
summary(fcCOND3)
fcCOND4<- fcCOND3 %>% filter(DSTRBCD1==30| DSTRBCD1==31| DSTRBCD1==32|
                               DSTRBCD2==30| DSTRBCD2==31| DSTRBCD2==32|
                               DSTRBCD3==30| DSTRBCD3==31| DSTRBCD3==32)
summary(fcCOND4)
fcCOND5<- fcCOND4[, c(13,14,1,2,6:11)]
summary(fcCOND5)

#only need plot list by cycle/year
summary(all4plotlist2)
all4plotlist3<- all4plotlist2[, c(1:5)]
summary(all4plotlist3)

# Merge with plot list
fcCOND6<- merge(all4plotlist3, fcCOND5, all.x = TRUE)
summary(fcCOND6)
length(unique(fcCOND6$SUCP)) # 1108 plots

# Remove NA's for now
fcCOND7<- fcCOND6 %>% drop_na(DSTRBCD1, DSTRBYR1)
length(unique(fcCOND6$SUCP)) # 39 plots burned 

# Need the matching plot of a plot that burned to contain the year

#### Identify plots that burned from cycle 1-2?
# First identify plots in cycle 1 fire year > measured year
cycle2<- fcCOND6 %>% filter(cycle_aw==2 & DSTRBYR1>MEASYEAR | cycle_aw==2 & DSTRBYR2>MEASYEAR | cycle_aw==2 & DSTRBYR3>MEASYEAR)

cycle3<- fcCOND6 %>% filter(cycle_aw==3 & DSTRBYR1<MEASYEAR | cycle_aw==3 & DSTRBYR2<MEASYEAR | cycle_aw==3 & DSTRBYR3<MEASYEAR)

cycle23burn<- merge(cycle2, cycle3, all=TRUE)


# change code 30-32 to 1
cycle23burn$Fire [cycle23burn$DSTRBCD1 >29| cycle23burn$DSTRBCD1<33|
                        cycle23burn$DSTRBCD2 >29| cycle23burn$DSTRBCD2<33|
                        cycle23burn$DSTRBCD3 >29| cycle23burn$DSTRBCD3<33] <- "1"

# Remove any rows with 9999 as disturbance year
cycle23burn2<- cycle23burn[!(cycle23burn$Fire=="1" & cycle23burn$DSTRBYR1=="9999"),]
summary(cycle23burn2)

# this df contains plot that burned between cycle 2 and 3. can delete columns we dont need now.
cycle23burn3<- cycle23burn2[, c(1:5,12)]
summary(cycle23burn3)
cycle23burn4<- cycle23burn3 %>% distinct()
summary(cycle23burn4)
length(unique(cycle23burn4$SUCP)) #27 plots burned

cycle23burn4$Fire<- factor(cycle23burn4$Fire)


summary(BAandTPApipo3)

# add fire to summary(BAandTPApipo3)

BAandTPApipo4<- merge(BAandTPApipo3, cycle23burn4, all.x = TRUE)

# Need to match dataframe up with its paired cycle 1
subset<-unique(BAandTPApipo4[BAandTPApipo4$cycle_aw==2 & BAandTPApipo4$Fire==1 |
                               BAandTPApipo4$cycle_aw==3 & BAandTPApipo4$Fire==1,]$SUCP)

fcCOND7<- BAandTPApipo4 %>% filter(cycle_aw==2 & Fire==1 |
                               cycle_aw==3 & Fire==1 | 
                               cycle_aw==2 & SUCP%in%subset |
                               cycle_aw==3 & SUCP%in%subset)

length(unique(fcCOND7$SUCP)) # 27 plots with fire

#### Need to create new column which identifies these SUCP's had a fire occur. 
fcCOND7$RecentFire= ifelse(fcCOND7$INVYR>=0, 1,0)
summary(fcCOND7)
fcCOND8<- fcCOND7[, c(1:11, 13)]

# Now, need to merge this file with the greater file. 

summary(fcCOND8)

BAandTPApipo5<- merge(BAandTPApipo3, fcCOND8, all.x = TRUE)
BAandTPApipo5$RecentFire[is.na(BAandTPApipo5$RecentFire)]<-0
BAandTPApipo5$RecentFire<- factor(BAandTPApipo5$RecentFire)
summary(BAandTPApipo5)


ggplot(BAandTPApipo5, aes(x=cycle_aw, y=Treecount, linetype=STATUSCD))+
  facet_grid(RecentFire~SizeClass)+
  geom_smooth(aes(group=STATUSCD), method = "glm", size=1)+
  theme_set(theme_bw())+
  labs(x= "Inventory Cycle")+
  labs(y= "Number Trees/Plot")+
  theme(axis.title.y = element_text(size=15))+
  theme(strip.text=element_text(size=10))+
  theme(legend.title = element_blank())



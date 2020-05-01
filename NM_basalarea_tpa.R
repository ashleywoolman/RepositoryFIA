
# calculating basal area and trees per acre


## Upload *resampled, nontreated, pipo & associated sp* file
NMpipo<- read.csv("NM_Initialquery.csv")


# Trees >4.9 dbh, live, only PIPO
# >4.9 dbh because this plot size is 1/24 acre, and trees >1 and <5 is 1/300 acre, so there is 
# different expansion factor. As I work through this, I want to create new columns that have 
# appropriate expansion factors in the same dataframe, so i can include all tree sizes. Also, 
# starting with live trees to keep it simple this round. 

query1<- NMpipo %>% filter(DIACALC>4.9, STATUSCD==1, SPCD==122)
length(unique(query1$SUCP))
# 401 unique SUCP
query1$treeexist<- as.logical(query1$DIACALC)
query2<- query1 %>% group_by(SUCP, cycle_aw, SPCD, STATUSCD) %>% summarise(treecount=sum(treeexist))
length(unique(query2$SUCP))
# 401 unique SUCP

query3<- query6 %>% filter(cycle_aw==3)
length(unique(query3$SUCP))
# this says 372 unique SUCP in cycle 3
query4<- query6 %>% filter(cycle_aw==2)
length(unique(query4$SUCP))
# and this says 357 unique SUCP in cycle 2... so i would expect the final dataframe will contain 357

query5<- unique(query2[query2$cycle_aw==3,]$SUCP)
query6<- query2 %>% filter(cycle_aw==3|cycle_aw==2 & SUCP%in%query5)
length(unique(query6$SUCP))
# but this says 372. so, there are still 15 SUCP (more in cycle 3) that need to be removed in order for them to match..
table(query6$SUCP)

# from the table, I copy and pasted the SUCPs that were only listed once and made the query below. But
# theres gotta be an easier way?
query7<- query6[!(query6$SUCP=="35_1_39_83486" | query6$SUCP=="35_1_31_85707" |query6$SUCP=="35_1_39_84911"| query6$SUCP=="35_1_39_92187"| 
                    query6$SUCP=="35_1_45_82657"| query6$SUCP=="35_1_55_88500"| query6$SUCP=="35_1_55_91358"| query6$SUCP=="35_1_6_87111"|
                    query6$SUCP=="35_2_33_92686"| query6$SUCP=="35_2_47_88271"|query6$SUCP=="35_2_47_90798"| query6$SUCP=="35_2_7_81244"| 
                    query6$SUCP=="35_3_17_80533"| query6$SUCP=="35_3_3_87636"| query6$SUCP=="35_4_35_83290"),]
length(unique(query7$SUCP))                  
table(query7$SUCP)

# this matches! 357*2= 714

# create column for plot count
query7$plotexists<- as.logical(query7$treecount)
plotcount<- query7 %>% group_by(cycle_aw, SPCD, STATUSCD) %>% summarise(plotcount=sum(plotexists))
# 357 in both cycle 2 and 3



##################################
# Create trees per acre (TPA) 
#################################

# TPA= #trees * expansion factor. FOr 1/24 acre plots, 6.018046 is expansion factor. 
# I calculated expansion factor with (4*0.04154172) = 6.018046, where 4 is the number
# of subplots in plot, and 0.04154172 is the area of a 1/24acre subplot. 

query7$TPA<- query7$treecount*6.018046


# I can merge the plot count dataframe with totals per cycle to get the average. 
query8<- query7 %>% group_by(cycle_aw, SPCD, STATUSCD)%>% summarise(TPAtotal=sum(TPA))
query9<- merge(query8,plotcount,by=c("cycle_aw", "SPCD", "STATUSCD"))
#divide for average. 
query9$avgTPA<- (query9$TPAtotal/query9$plotcount)


#plot this
library(ggpubr)
library(ggplot2)
query7$cycle_aw<- factor(query7$cycle_aw)

boxplot(TPA~cycle_aw, data=query7)
ggplot(query7, aes(x=TPA, color=cycle_aw, fill=cycle_aw))+ geom_histogram(binwidth = 20)



##################################
# Create basal area
#################################

# Realized I should have created the dataframe of query 7 earlier on so I wouldnt have to do the below steps again
# that still has DBH variable
# Want df similar to query7 with 357 plots

queryforBA<- unique(query1[query1$cycle_aw==3,]$SUCP)
queryforBA2<- query1 %>% filter(cycle_aw==3|cycle_aw==2 & SUCP%in%queryforBA)
length(unique(queryforBA2$SUCP))
# says 372, it is not working again perfectly, so followed same steps above

queryforBA3<- queryforBA2 %>% filter(cycle_aw==3)
length(unique(queryforBA3$SUCP))
# cycle 3 has 372
queryforBA4<- queryforBA2 %>% filter(cycle_aw==2)
length(unique(queryforBA4$SUCP))
# cycle 2 has 357, need to remove those same 15 SUCP

queryforBA5<- queryforBA2[!(queryforBA2$SUCP=="35_1_39_83486" | queryforBA2$SUCP=="35_1_31_85707" |queryforBA2$SUCP=="35_1_39_84911"| queryforBA2$SUCP=="35_1_39_92187"| 
                queryforBA2$SUCP=="35_1_45_82657"| queryforBA2$SUCP=="35_1_55_88500"| queryforBA2$SUCP=="35_1_55_91358"| queryforBA2$SUCP=="35_1_6_87111"|
                queryforBA2$SUCP=="35_2_33_92686"| queryforBA2$SUCP=="35_2_47_88271"|queryforBA2$SUCP=="35_2_47_90798"| queryforBA2$SUCP=="35_2_7_81244"| 
                queryforBA2$SUCP=="35_3_17_80533"| queryforBA2$SUCP=="35_3_3_87636"| queryforBA2$SUCP=="35_4_35_83290"),]
length(unique(queryforBA5$SUCP)) 
# it worked, 357 plots.


### basal area
queryforBA5$dbhsquared<- queryforBA5$DIACALC^2 
queryforBA5$BA_Tree_Sqft<- queryforBA5$dbhsquared*0.005454
queryforBA5$BA_PerAcre_Sqft<- queryforBA5$BA_Tree_Sqft*6.018046

queryforBA6<- queryforBA5 %>% group_by(SUCP, cycle_aw, SPCD, STATUSCD) %>% 
  summarise(BAplotTotal_PerAcre_Sqft=sum(BA_PerAcre_Sqft))
length(unique(queryforBA6$SUCP))

queryforBA6$cycle_aw<- factor(queryforBA6$cycle_aw)
boxplot(BAplotTotal_PerAcre_Sqft~cycle_aw, data=queryforBA6)
ggplot(queryforBA6, aes(x=BAplotTotal_PerAcre_Sqft, color=cycle_aw, fill=cycle_aw))+ geom_histogram(binwidth = 20)


# merge TPA and basal area into 1 dataframe
merge1<- merge(query7, queryforBA6)
BAandTPA<- merge1[ -c(5:6) ]





########################
# some analysis
########################

# paired T test of NM
t.test(TPA~cycle_aw, data=query7, paired=TRUE)
t.test(TPA~cycle_aw, data=query7, var.equal=T)


modNM<-lm(TPA~cycle_aw, data=query7)
anova(modNM)
summary(modNM)
#nothing significant? then add in SUCP
modNM2<-lm(TPA~cycle_aw+SUCP, data=query7)
anova(modNM2)
summary(modNM2)
# identifies plots where a significant change occured. Next include fire in model






#######################
# add in fire
########################

#try to merge NM BAandTPA with cond talbe for disturbances.
# can use "notreatmentcond" from previous script

newfire<- notreatmentcond %>% filter(DSTRBCD1==30| DSTRBCD1==31| DSTRBCD3==32)
newfire2<- newfire[, c(8:9, 20)]
BAandTPA_fire<- merge(BAandTPA, newfire2, all=TRUE)
length(unique(BAandTPA_fire$SUCP))
# gives me 398 SUCP, and 768 total... need to figure this out
table(BAandTPA_fire$SUCP)

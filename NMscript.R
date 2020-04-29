### NM for J.Coop to follow along


#NM csv files were downloaded 4/28 for most up to date plots.

library(tidyverse)
library(dplyr)


nm_tree<-read.csv("TREE/raw/NM_TREE_042820.csv")
nm_cond<-read.csv("COND/raw/NM_COND_042820.csv")

# remove first cycle of inventory (from 1987)
nm_cond1<- nm_cond %>% filter(CYCLE>1)
summary(nm_cond1)

# we have cycle 2, 3, 4 now but going to rename them to match up with other state data to 1, 2, 3
nm_cond2<- nm_cond1 %>% mutate(cycle_aw = case_when(CYCLE==2 ~ '1', CYCLE==3 ~ '2', CYCLE== 4 ~ '3'))
nm_cond2$cycle_aw<- factor(nm_cond2$cycle_aw)
summary(nm_cond2)                       



#
### 1. Identify resampled plots on COND table
#
### create new unique variable that pastes together state, unit, countycd, plot: "SUCP"
cols<- c('STATECD', 'UNITCD', 'COUNTYCD', 'PLOT')
nm_cond2$SUCP<- apply(nm_cond2[,cols], 1, paste, collapse="_")
nm_cond2$SUCP<- factor(nm_cond2$SUCP)


## create subset of plots w cycle 3
cycle3plots<- unique(nm_cond2[nm_cond2$cycle_aw==3,]$SUCP)

## use subset to get all remeasured plots
nm_cond3<- nm_cond2 %>% filter(cycle_aw==3|(cycle_aw==1 & SUCP%in%cycle3plots| cycle_aw==2 & SUCP%in%cycle3plots))
dim(nm_cond3)
summary(nm_cond3)

#
# Jonathan, this yieled 0 plots for cycle 1 (look at cycle_aw). When I asked John Shaw a while back about linking up pre-2000 plots with 
# post-2000, he explained a couple things and I think it relates to this. Plots got renamed after 2000, so while some of the pre-2000 
# may be useful, the public database does not identify this crossover. They internally still track everything using original plot 
# numbers. Also, the location of plots changed post 2000, but in their internal database they have a "collocated" code
# that identifies if a new plot shares the same center with an older plot. Shaw said he can help get me a record that would contain plots
# crossed over, but first I need to nail down what variables and plots from the last 2 cyles of inventory we would want in this special access
# data. This seems like something I can do pretty soon. 
# 

# But for now, I'll show you my process for linking up post-2000 plots. 

nm_cond4<- nm_cond2 %>% filter(cycle_aw==3|(cycle_aw==2 & SUCP%in%cycle3plots))
dim(nm_cond4)
summary(nm_cond4)

# re-order columns
nm_cond5<- nm_cond4[,c(1:7, 38, 37, 8:36)]

#check- 5,239 plots
length(unique(nm_cond5$SUCP))


# check if number of plots for cycle 2 is equal to cycle 3
cycle3plots2chk<- nm_cond5 %>% filter(cycle_aw==3)
length(unique(cycle3plots2$SUCP))

cycle2plotschk<-nm_cond5 %>% filter(cycle_aw==2)
length(unique(cycle2plotschk$SUCP))

#cycle 2 has 2 fewer plots?
length(unique(nm_cond5$SUCP))


# talk about condition thing here.
table(nm_cond4$SUCP)
# cycle 3 has 38 more rows. Is this from extra "conditions"?



#
### 2. Remove plots with treatment on the *Condition dataframe
#
treatment<- nm_cond5 %>% filter(nm_cond5$TRTCD1==10 | TRTCD2==10 | TRTCD3==10 | 
                                  TRTCD1==20 | TRTCD2==20 | TRTCD3==20 |
                                  TRTCD1==30 | TRTCD2==30 | TRTCD3==30 |
                                  TRTCD1==50 | TRTCD2==50 | TRTCD3==50)

treatment10<- nm_cond5 %>% filter(nm_cond5$TRTCD1==10 | TRTCD2==10 | TRTCD3==10)
length(unique(treatment10$SUCP))
treatment20<- nm_cond5 %>% filter(nm_cond5$TRTCD1==20 | TRTCD2==20 | TRTCD3==20)
length(unique(treatment20$SUCP))
treatment30<- nm_cond5 %>% filter(nm_cond5$TRTCD1==30 | TRTCD2==30 | TRTCD3==30)
length(unique(treatment30$SUCP))
treatment50<- nm_cond5 %>% filter(nm_cond5$TRTCD1==50 | TRTCD2==50 | TRTCD3==50)
length(unique(treatment50$SUCP))

# 10: removed 44 plots
# 20: removed 2 plots
# 30: removed 1 plot
# 50: removed 19 plots
length(unique(treatment$SUCP))
# total of 64 plots. I think there were some overlap

notreatmentcond<-nm_cond5[!(nm_cond5$SUCP %in% treatment$SUCP),]
length(unique(notreatmentcond$SUCP))
# 5239- 64= 5175. THis worked.

chk3<- notreatmentcond %>% filter(cycle_aw==3)
length(unique(t$SUCP))
chk2<-notreatmentcond %>% filter(cycle_aw==2)
length(unique(r$SUCP))
# this worked, 5,175 unique SUCP


# In the larger data set with all 4 states, using the same lines of code removed a total of 349 plots. of the 349, 292 were
# from code 10 (timber cutting) with the remaining 57 from other codes.
## This tells us of the 29,848 plots+conditions, 349 had a treatment. This removes 
# plots+conditions from any time (cycle 1 and 2) that contain SUCP in treatment. the new 
## "notreatmentcond" has 29,499.. So, 29848-349= 29499..  I think this is valid.
## So, we can apply the same method to resampled tree data frame below. 



#
## 3. Identify resampled plots for Tree dataframe
# 
nm_tree<-read.csv("TREE/raw/NM_TREE_042820.csv")
nm_tree1<- nm_tree %>% filter(CYCLE>1)
nm_tree1$CYCLE<- factor(nm_tree1$CYCLE)
summary(nm_tree1)
nm_tree2<- nm_tree1 %>% mutate(cycle_aw = case_when(CYCLE==2 ~ '1', CYCLE==3 ~ '2', CYCLE==4 ~ '3'))
nm_tree2$cycle_aw<- factor(nm_tree2$cycle_aw)
summary(nm_tree2)



### create new variable that pastes together state, unit, countycd, plot: "SUCP"
cols<- c('STATECD', 'UNITCD', 'COUNTYCD', 'PLOT')
nm_tree2$SUCP<- apply(nm_tree2[,cols], 1, paste, collapse="_")

## create subset of plots w cycle 3
cycle3plotstree<- unique(nm_tree2[nm_tree2$cycle_aw==3,]$SUCP)

## use subset to get all remeasured plots
nm_tree3<- nm_tree2 %>% filter(cycle_aw==3|(cycle_aw==2 & SUCP%in%cycle3plotstree))

# re-order columns
nm_tree4<- nm_tree3[,c(1:9, 27, 26, 10:25)]

length(unique(nm_tree4$SUCP))
summary(nm_tree4)

### Jonathan, This shows only 1,595 plots were remeasured, a huge decrease in Conditions table of 5,239 resampled. 
# I think maybe this is because there's a lot of plots that have records yet do not have trees? ... I didn't notice
# this huge decrease when I did it on the bigger dataset. 
### Also, again this yeilded 0 for cycle_aw 1 (pre-2000)




#
## 4. Remove treatment plots from the resampled Tree dataframe
# 

# Remove plots that had treatment (from the earlier COND dataframe)

treatmenttree<- nm_cond5 %>% filter(nm_cond5$TRTCD1==10 | TRTCD2==10 | TRTCD3==10 | 
                                  TRTCD1==20 | TRTCD2==20 | TRTCD3==20 |
                                  TRTCD1==30 | TRTCD2==30 | TRTCD3==30 |
                                  TRTCD1==50 | TRTCD2==50 | TRTCD3==50)

nm_tree5<-nm_tree4[!(nm_tree4$SUCP %in% treatmenttree$SUCP),]

#check to see how many plots were removed
length(unique(nm_tree5$SUCP))
# 1595-1533=62. 62 plots were removed. 


## In the larger dataset with 4 states, it has 9,893 plots that were resampled, and after removing the
## treatment plots, it is 9,547 plots. So it removed 346.



#
## 5. Select plots with PIPO and associated species:
#


# what species do we have?
nm_tree5$SPCD<- factor(nm_tree5$SPCD)
summary(nm_tree5)
# how many?
length(unique(nm_tree5$SPCD))
table(nm_tree5$SPCD)
unique(nm_tree5$SPCD)

#subset of PIPO, keeping SUCP
pipoplotsnm<- unique(nm_tree5[nm_tree5$SPCD==122,]$SUCP)

#Create dataframe that contains plots that have had PIPO at any time, and the associated species
# I went through the FIA metadata and made a spreadsheet of what all the SPCD codes listed 
# from "unique(nm_tree5$SPCD)" were. I can send that over to you if you want. There are more than 
# I initially wrote in my proposal because for example there are 5 species of juniper found within 
# the 4 states- redberry, alligator, utah, and rocky mountain juniper. So I think what the code 
# below does is even if PIPO was removed from a plot and other species are present, that plot will 
# not be removed. BUT, I am not sure if all PIPO are removed from a plot, and then there are currenly 
# no other species, if it holds onto that plot. Because there is nothing saying that "there are now
# 0 PIPO in this plot" does that make sense? So maybe I need to find a way to add in rows for 0 if 
# this has happened?

pipoplotsnm<- nm_tree5 %>% filter(SPCD==122|(SPCD==814 & SUCP%in%pipoplots|
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
                                                     SPCD==746 & SUCP%in%pipoplots))
length(unique(pipoplotsnm$SUCP))

# This shows there are 417 resampled, non treatment, plots containing PIPO at a certain time. 
# The larger data set contains 1,520 plots. 





---
title: "Wrangling_Merging preExperiment & postExperiment Data"
author: "Kyle Barrentine"
date: "5/25/2021"
---
#Setting Working Directory and Loading Required Packages
```{r setup, include=FALSE, cache = FALSE}
pacman::p_load(tidyverse,knitr)## setting working directory
opts_knit$set(root.dir = "~/Library/Mobile Documents/com~apple~CloudDocs/Graduate School/ExplorationChatroom/experimentData/chatData")
```
#Pre Experiment Exploration Data Wrangling
```{r}  
PreExploration = read.csv("preExploration.csv")    #Loading in data from Qualtrics
PreExploration = PreExploration[-1:-2,-1:-18] #Indexing Values not needed rows 1&2 and columns 1 through 18
PreExploration = PreExploration[,c(-23,-20)]
PreExploration = as.data.frame(PreExploration)
PreExploration[,-52:-53] =  lapply(PreExploration[,-52:-53],as.numeric) #converting everything to numeric
PreExploration = as.data.frame(PreExploration)#converting it to a data frame again. 

PreExploration = PreExploration %>% mutate(PositiveAffect = Q15_1 + Q15_3 + Q15_5 + Q15_7 + Q15_10 + Q15_12,
                                           NegAffect = Q15_2 + Q15_4 + Q15_6 + Q15_8 + Q15_9 + Q15_11,
                                           Sex = Q7,
                                           MotherEduc = Q8,
                                           FatherEduc = Q9,
                                           FamilyIncome = Q10,
                                           Age = Q11,
                                           Hispanic = Q12,
                                           Race = Q13,
                                           FirstGen = Q14,
                                           Extraversion = (6 - Q13_1) + Q13_6 + Q13_11 + Q13_16 + (6 - Q13_21) + (6- Q13_26),
                                           Agreeable = Q13_2 + (6 - Q13_7) + Q13_12 + (6 - Q13_17) + Q13_22 + (6- Q13_27),
                                           Conscientious =  (6 - Q13_3) + (6 - Q13_8) + Q13_13 + Q13_18 + Q13_23 + (6- Q13_28),
                                           NegEmotionality = Q13_4 + Q13_9 + (6 - Q13_14) + (6 - Q13_19) + (6 - Q13_24) + Q13_29,
                                           OpenMind = Q13_5 + (6 - Q13_10) + Q13_15 + (6 - Q13_20) + Q13_25 + (6 - Q13_30),
                                           ComputingID = Q5,
                                           Group = 1)

```                                          
#Pre Experiment Qualtrics Data Wrangling
```{r}
## Loading in the data from Qualtrics
PreExploitation = read.csv("preExploitation.csv")
PreExploitation = PreExploitation[-1:-2,-1:-17]

PreExploitation = PreExploitation[,c(-1,-51)]
PreExploitation = as.data.frame(PreExploitation)
PreExploitation[,-51:-52] =  lapply(PreExploitation[,-51:-52],as.numeric)
PreExploitation = as.data.frame(PreExploitation)
PreExploitation = PreExploitation %>% mutate(PositiveAffect = Q10_1 + Q10_3 + Q10_5 + Q10_7 + Q10_10 + Q10_12,
                                           NegAffect = Q10_2 + Q10_4 + Q10_6 + Q10_8 + Q10_9 + Q10_11,
                                           Sex = Q1,
                                           MotherEduc = Q2,
                                           FatherEduc = Q3,
                                           FamilyIncome = Q4,
                                           Age = Q5,
                                           Hispanic = Q6,
                                           Race = Q7,
                                           FirstGen = Q8,
                                           Extraversion = (6 - Q9_1) + Q9_6 + Q9_11 + Q9_16 + (6 - Q9_21) + (6- Q9_26),
                                           Agreeable = Q9_2 + (6 - Q9_7) + Q9_12 + (6 - Q9_17) + Q9_22 + (6- Q9_27),
                                           Conscientious =  (6 - Q9_3) + (6 - Q9_8) + Q9_13 + Q9_18 + Q9_23 + (6- Q9_28),
                                           NegEmotionality = Q9_4 + Q9_9 + (6 - Q9_14) + (6 - Q9_19) + (6 - Q9_24) + Q9_29,
                                           OpenMind = Q9_5 + (6 - Q9_10) + Q9_15 + (6 - Q9_20) + Q9_25 + (6 - Q9_30),
                                           ComputingID = Q8.1,
                                           Group = 0)
```

#Merging Pre Experiment (both conditions) and post experiment data
``` {r}
#Creating Dataframes from both explore and exploit with only summary variables
Exploit = as.data.frame(PreExploitation[,52:69])
Explore = as.data.frame(PreExploration[,53:70])

Experimental = rbind(Explore,Exploit)
Experimental$SubID = as.numeric(Experimental$SubID)
## Reading in data from the postexperiment qualtrics
PostEx = read.csv("postExperiment.csv")
PostEx$SubID = as.numeric(PostEx$SubID)

#Merging Data sets together
Full_Data = left_join(PostEx, Experimental, by = "SubID")
Full_Data = Full_Data[-1:-8,-1:-17]
Full_Data[1:63] =  lapply(Full_Data[1:63],as.numeric)
```

##Recoding variables from post experiment data

```{r}
Full_Data$Q68_1 = recode(Full_Data$Q68_1, '34' = 1 , '35' = 2, '36' = 3, '37' = 4, '38' = 5, '39' = 6, '40' = 7)
Full_Data$Q68_2 = recode(Full_Data$Q68_2, '34' = 1 , '35' = 2, '36' = 3, '37' = 4, '38' = 5, '39' = 6, '40' = 7)
Full_Data$Q68_3 = recode(Full_Data$Q68_3, '34' = 1 , '35' = 2, '36' = 3, '37' = 4, '38' = 5, '39' = 6, '40' = 7)
Full_Data$Q68_4 = recode(Full_Data$Q68_4, '34' = 1 , '35' = 2, '36' = 3, '37' = 4, '38' = 5, '39' = 6, '40' = 7)
Full_Data$Q68_5 = recode(Full_Data$Q68_5, '34' = 1 , '35' = 2, '36' = 3, '37' = 4, '38' = 5, '39' = 6, '40' = 7)
Full_Data$Q68_6 = recode(Full_Data$Q68_6, '34' = 1 , '35' = 2, '36' = 3, '37' = 4, '38' = 5, '39' = 6, '40' = 7)
Full_Data$Q68_7 = recode(Full_Data$Q68_7, '34' = 1 , '35' = 2, '36' = 3, '37' = 4, '38' = 5, '39' = 6, '40' = 7)
Full_Data$Q68_8 = recode(Full_Data$Q68_8, '34' = 1 , '35' = 2, '36' = 3, '37' = 4, '38' = 5, '39' = 6, '40' = 7)
Full_Data$Q68_9 = recode(Full_Data$Q68_9, '34' = 1 , '35' = 2, '36' = 3, '37' = 4, '38' = 5, '39' = 6, '40' = 7)
Full_Data$Q68_10 = recode(Full_Data$Q68_10, '34' = 1 , '35' = 2, '36' = 3, '37' = 4, '38' = 5, '39' = 6, '40' = 7)
Full_Data$Q68_11 = recode(Full_Data$Q68_11, '34' = 1 , '35' = 2, '36' = 3, '37' = 4, '38' = 5, '39' = 6, '40' = 7)
Full_Data$Q68_12 = recode(Full_Data$Q68_12, '34' = 1 , '35' = 2, '36' = 3, '37' = 4, '38' = 5, '39' = 6, '40' = 7)
Full_Data$Q68_13 = recode(Full_Data$Q68_13, '34' = 1 , '35' = 2, '36' = 3, '37' = 4, '38' = 5, '39' = 6, '40' = 7)
Full_Data$Q68_14 = recode(Full_Data$Q68_12, '34' = 1 , '35' = 2, '36' = 3, '37' = 4, '38' = 5, '39' = 6, '40' = 7)
Full_Data$Q68_15 = recode(Full_Data$Q68_13, '34' = 1 , '35' = 2, '36' = 3, '37' = 4, '38' = 5, '39' = 6, '40' = 7)


Full_Data$Q65_1 = recode(Full_Data$Q65_1, '72' = 1 , '73' = 2, '74' = 3, '75' = 4, '76' = 5, '77' = 6, '78' = 7)
Full_Data$Q65_2 = recode(Full_Data$Q65_2, '72' = 1 , '73' = 2, '74' = 3, '75' = 4, '76' = 5, '77' = 6, '78' = 7)
Full_Data$Q65_3 = recode(Full_Data$Q65_3, '72' = 1 , '73' = 2, '74' = 3, '75' = 4, '76' = 5, '77' = 6, '78' = 7)
Full_Data$Q65_4 = recode(Full_Data$Q65_4, '72' = 1 , '73' = 2, '74' = 3, '75' = 4, '76' = 5, '77' = 6, '78' = 7)
Full_Data$Q65_5 = recode(Full_Data$Q65_5, '72' = 1 , '73' = 2, '74' = 3, '75' = 4, '76' = 5, '77' = 6, '78' = 7)
Full_Data$Q65_6 = recode(Full_Data$Q65_6, '72' = 1 , '73' = 2, '74' = 3, '75' = 4, '76' = 5, '77' = 6, '78' = 7)
Full_Data$Q65_7 = recode(Full_Data$Q65_7, '72' = 1 , '73' = 2, '74' = 3, '75' = 4, '76' = 5, '77' = 6, '78' = 7)
Full_Data$Q65_8 = recode(Full_Data$Q65_8, '72' = 1 , '73' = 2, '74' = 3, '75' = 4, '76' = 5, '77' = 6, '78' = 7)
Full_Data$Q65_9 = recode(Full_Data$Q65_9, '72' = 1 , '73' = 2, '74' = 3, '75' = 4, '76' = 5, '77' = 6, '78' = 7)

Full_Data$Q69_1 = recode(Full_Data$Q69_1, '71' = 1 , '72' = 2, '73' = 3, '74' = 4, '75' = 5, '76' = 6, '77' = 7)
Full_Data$Q69_2 = recode(Full_Data$Q69_2, '71' = 1 , '72' = 2, '73' = 3, '74' = 4, '75' = 5, '76' = 6, '77' = 7)
Full_Data$Q69_3 = recode(Full_Data$Q69_3, '71' = 1 , '72' = 2, '73' = 3, '74' = 4, '75' = 5, '76' = 6, '77' = 7)
Full_Data$Q69_4 = recode(Full_Data$Q69_4, '71' = 1 , '72' = 2, '73' = 3, '74' = 4, '75' = 5, '76' = 6, '77' = 7)
Full_Data$Q69_5 = recode(Full_Data$Q69_5, '71' = 1 , '72' = 2, '73' = 3, '74' = 4, '75' = 5, '76' = 6, '77' = 7)
Full_Data$Q69_6 = recode(Full_Data$Q69_6, '71' = 1 , '72' = 2, '73' = 3, '74' = 4, '75' = 5, '76' = 6, '77' = 7)
```
#Creating summary variables in full data 
```{r}
Full_Data2= Full_Data %>%  mutate(PosAffect2 = Q67_1 + Q67_3 + Q67_5 + Q67_7 + Q67_10 + Q67_12,
                                  NegAffect2 = Q67_2 + Q67_4 + Q67_6 + Q67_8 + Q67_9 + Q67_11,
                                  RelMobility = Q2_1 + Q2_2 + Q2_3 + (7 - Q2_4) + Q2_5 + (7 - Q2_6) + Q2_7 +(7- Q2_8) + Q2_9 +(7-Q2_10) + (7 - Q2_10),
                                  Exploration = Q65_1 + (7 - Q65_2) + Q65_3 + Q65_4  + (7 - Q65_5) + (7 - Q65_6) + Q65_7 + (7 - Q65_8) + Q65_9,
                                 Richness = Q68_1 + (8 - Q68_2) + Q68_3 + ( 8 - Q68_4) + Q68_5,
                                 Hapiness = Q68_6 + (8 - Q68_7) + Q68_8 + (8 - Q68_9) +Q68_10,
                                 Meaning = Q68_11 + (8- Q68_12) + Q68_13 + (8 - Q68_14) + Q68_15,
                                 Connectedness = Q69_1 + Q69_2 + Q69_3 + Q69_4 + Q69_5 + Q69_6,
                                 ChangeP = PosAffect2 - PositiveAffect,
                                 ChangeN = NegAffect2 - NegAffect)


write.csv(Full_Data2, "prePostMerged.csv")
```

#Wrangling Message Data
```{r}
palette(hcl.colors(100, "viridis"))
dLong <-read.csv("finalExplorationChatData.csv")
dAll <-aggregate(dLong,by=list("Channel.Identifier"=dLong$Channel.Identifier,"Dyad"=dLong$ID,"SubID" = dLong$SubID),FUN=length)
dAll <-dAll[,c("Channel.Identifier","Dyad","Message","SubID")]
dAll <-separate(dAll,col=Dyad,into=c("Ego","Alter"),sep="_")

dAll2 = dAll %>% group_by(SubID) %>% mutate(avgMessage = mean(Message,na.rm = T), 
                                            minMessage = min(Message,na.rm = T),
                                            maxMessage = max(Message,na.rm = T))

x = dAll2[,5:8]

palette(hcl.colors(100, "viridis"))
dLong <-read.csv("finalExploitationChatData.csv")
dAll <-aggregate(dLong,by=list("Channel.Identifier"=dLong$Channel.Identifier,"Dyad"=dLong$ID,"SubID" = dLong$SubID),FUN=length)
dAll <-dAll[,c("Channel.Identifier","Dyad","Message","SubID")]
dAll <-separate(dAll,col=Dyad,into=c("Ego","Alter"),sep="_")

dAll3 = dAll %>% group_by(SubID) %>% mutate(avgMessage = mean(Message,na.rm = T), 
                                            minMessage = min(Message,na.rm = T),
                                            maxMessage = max(Message,na.rm = T))


y = dAll3[,5:8]
z = rbind(x,y)
Z = z %>% distinct()

Z1 = read.csv("~/Library/Mobile Documents/com~apple~CloudDocs/Graduate School/ExplorationChatroom/experimentData/qualtricsData/prePostMerged.csv")
Z3 = merge(Z, Z1, by = "SubID")
write.csv(Z3, "prePostMessage.csv")

Z3[85:92] = misty::center(Z3[85:92], type = c("CGM"))

Z3$OpenMindC = misty::center(Z3$OpenMind, type = c("CGM"))
Z3$ExtraversionC = misty::center(Z3$Extraversion, type = c("CGM"))
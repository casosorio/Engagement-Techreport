# Loading packages
library(tidyverse)

# Importing Data from Qualtrics 
temp <- read.csv("Data/qualtrics_pilot_data.csv", header=FALSE, na.strings="")

# Creating Header Columns
x <- temp[2,] # Takes row 2 and places it into an object
data <- temp[-c(1:3),] # Remove row 1 to 2
colnames(data) <- x # turn row 2 from line 5 into header of dataframe

num <- nrow(data) # Tells us how many participants we have in our dataset

################ Cleaning up data and combining the four condition into one ################

data$Cond1 <- rowSums(is.na(data[18:53])) # counts how many na's occur between columns 18 to 53
data$Cond2 <- rowSums(is.na(data[54:89]))
data$Cond3 <- rowSums(is.na(data[90:125]))
data$Cond4 <- rowSums(is.na(data[126:161]))

# Determing what is the condition, if they have less than 36 NA, then that is there condition
data$Condition[data$Cond1 < 36] <- 1
data$Condition[data$Cond2 < 36] <- 2
data$Condition[data$Cond3 < 36] <- 3
data$Condition[data$Cond4 < 36] <- 4

# Splitting the Conditions
cond1 <- data[ which(data$Condition==1), ]
cond2 <- data[ which(data$Condition==2), ]
cond3 <- data[ which(data$Condition==3), ]
cond4 <- data[ which(data$Condition==4), ]

cond1.red <- cond1[,c(6, 18:53, 162:165, 171)]  ## using Cond1 ordering
cond2.red <- cond2[,c(6, 62:65, 70:73, 82:85, 58:61, 74:77, 86:89, 66:69, 78:81, 54:57, 162:165, 171)]
cond3.red <- cond3[,c(6, 94:97, 106:109, 118:121, 98:101, 110:113, 122:125, 102:105, 114:117, 90:93, 162:165, 171)]
cond4.red <- cond4[,c(6, 138:161, 130:137, 126:129, 162:165, 171)]

## Getting rid of condition markers so rbind will work
names(cond1.red) = gsub(pattern = "C*.* - ", replacement = "", x = names(cond1.red))     
names(cond2.red) = gsub(pattern = "C*.* - ", replacement = "", x = names(cond2.red))  
names(cond3.red) = gsub(pattern = "C*.* - ", replacement = "", x = names(cond3.red))  
names(cond4.red) = gsub(pattern = "C*.* - ", replacement = "", x = names(cond4.red))  

together <- rbind(cond1.red, cond2.red, cond3.red, cond4.red)

i <- c(1:37)
together[ , i] <- apply(together[ , i], 2,            # Specify own function within apply
                        function(x) as.numeric(as.character(x)))

#write.csv(together, "together.csv")

################## Recoding items #########################
together$`Most days, I feel happiest when the workday is soon to be complete.` <- 7 - together$`Most days, I feel happiest when the workday is soon to be complete.`
together$`This job drains my energy.` <- 7 - together$`This job drains my energy.`

## BEHAVIORAL (NONE):

## COGNITIVE: 

together$`Thinking about work saps my energy.` <- 7 - together$`Thinking about work saps my energy.`
together$`I often think about finding another job.` <- 7 - together$`I often think about finding another job.`

Cond1_Model<-cond1.red[,2:37]

Cond1_Model<-Cond1_Model%>%rename(
  Item_1=`Iâ€™m able to concentrate on my work without distractions.`,
  Item_2=`I have a hard time detaching mentally from my work.`,
  Item_3=`Time passes quickly while Iâ€™m working.`,
  Item_4=`I find it difficult to mentally disconnect from work.`,
  Item_5=`I enjoy thinking about work even when Iâ€™m not at work.`,
  Item_6=`Most days, I feel happiest when the workday is soon to be complete.`,
  Item_7=`I am happiest when I am immersed in a project.`,
  Item_8=`I love starting my workday.`,
  Item_9=`I devote more time than is expected of me.`,
  Item_10=`I have to be reminded to take breaks while Iâ€™m at work.`,
  Item_11=`I never miss a work deadline.`,
  Item_12=`I never allow distractions to interfere with my work.`,
  Item_13=`I devote my full attention to my work tasks throughout the day.`,
  Item_14=`Thinking about work saps my energy.`,
  Item_15=`I would rather direct my focus toward a work task than a personal task.`,
  Item_16=`Iâ€™m able to maintain good levels of energy throughout the workday.`,
  Item_17=`I enjoy spending time completing my job tasks.`,
  Item_18=`Most days I feel enthusiastic about starting my work day.`,
  Item_19=`I feel motivated to go beyond what is asked of me.`,
  Item_20=`This job drains my energy.`,
  Item_21=`When work is slow I find ways to be productive.`,
  Item_22=`I express enthusiasm for my job while at work.`,
  Item_23=`I try my best to perform well at work.`,
  Item_24=`If I notice my energy level is low, I take corrective steps to re-energize.`,
  Item_25=`I plan my future with this company.`,
  Item_26=`I believe this company cares about my career goals.`,
  Item_27=`I often think about finding another job.`,
  Item_28=`This organization challenges me to work at my full potential.`,
  Item_29=`I am proud to be a member of this organization.`,
  Item_30=`I feel supported by my supervisor when I fail at a task.`,
  Item_31=`I feel proud of my accomplishments within this organization.`,
  Item_32=`My job makes me feel like Iâ€™m part of something meaningful.`,
  Item_33=`I make valued contributions to the organization.`,
  Item_34=`I embrace challenging situations at work.`,
  Item_35=`I speak positively about this organization to others.`,
  Item_36=`This organization provides the resources necessary for me to successfully perform my job.`
) %>% 
  select(Item_1,Item_2,Item_3,Item_4,Item_5,Item_6,Item_7,Item_8,Item_9,Item_10,Item_11,Item_12,Item_13,Item_14,Item_15,Item_16,Item_17,
         Item_18,Item_19,Item_20,Item_21,Item_22,Item_23,Item_24,Item_25,Item_26,Item_27,Item_28,Item_29,Item_30,Item_31,Item_32,Item_33,
         Item_34,Item_35,Item_36)

######Bifactor model
Bifactor_Model_Cond1<-'
Cognitive=~Item_1+Item_2+Item_3+Item_4+Item_13+Item_14+Item_15+Item_16+Item_25+Item_26+Item_27+Item_28
Affective=~Item_5+Item_6+Item_7+Item_8+Item_17+Item_18+Item_19+Item_20+Item_29+Item_30+Item_31+Item_32
Behavioral=~Item_9+Item_10+Item_11+Item_12+Item_21+Item_22+Item_23+Item_24+Item_33+Item_34+Item_35+Item_36
Absorption=~Item_1+Item_2+Item_3+Item_4+Item_5+Item_6+Item_7+Item_8+Item_9+Item_10+Item_11+Item_12
Vigor=~Item_13+Item_14+Item_15+Item_16+Item_17+Item_18+Item_19+Item_20+Item_21+Item_22+Item_23+Item_24
Dedication=~Item_25+Item_26+Item_27+Item_28+Item_29+Item_30+Item_31+Item_32+Item_33+Item_34+Item_35+Item_36
Absorption ~~ 0*Affective
Absorption ~~ 0*Behavioral
Absorption ~~ 0*Cognitive
Vigor ~~ 0*Affective
Vigor ~~ 0*Behavioral
Vigor ~~ 0*Cognitive
Dedication ~~ 0*Affective
Dedication ~~ 0*Behavioral
Dedication ~~ 0*Cognitive
'

Fit.Bi_Cond1 <- lavaan::cfa(Bifactor_Model_Cond1, data = Cond1_Model) #, likelihood = "wishart")

semPlot::semPaths(Fit.Bi_Cond1, bifactor = c("Cognitive", "Affective", "Behavioral"), "std", layout = "tree3", 
                  rotation = 2, curvePivot=TRUE, style="lisrel", nCharNodes = 0)  ## exoCov=FALSE deletes all covariances
title("Initial pilot bifactor analysis (36 candidate items)")

# Condition2 _________________________________________________________________________________________
Cond2_Model<-cond2.red[,2:37]

Cond2_Model<-Cond2_Model%>%rename(
  Item_1=`Iâ€™m able to concentrate on my work without distractions.`,
  Item_2=`I have a hard time detaching mentally from my work.`,
  Item_3=`Time passes quickly while Iâ€™m working.`,
  Item_4=`I find it difficult to mentally disconnect from work.`,
  Item_5=`I enjoy thinking about work even when Iâ€™m not at work.`,
  Item_6=`Most days, I feel happiest when the workday is soon to be complete.`,
  Item_7=`I am happiest when I am immersed in a project.`,
  Item_8=`I love starting my workday.`,
  Item_9=`I devote more time than is expected of me.`,
  Item_10=`I have to be reminded to take breaks while Iâ€™m at work.`,
  Item_11=`I never miss a work deadline.`,
  Item_12=`I never allow distractions to interfere with my work.`,
  Item_13=`I devote my full attention to my work tasks throughout the day.`,
  Item_14=`Thinking about work saps my energy.`,
  Item_15=`I would rather direct my focus toward a work task than a personal task.`,
  Item_16=`Iâ€™m able to maintain good levels of energy throughout the workday.`,
  Item_17=`I enjoy spending time completing my job tasks.`,
  Item_18=`Most days I feel enthusiastic about starting my work day.`,
  Item_19=`I feel motivated to go beyond what is asked of me.`,
  Item_20=`This job drains my energy.`,
  Item_21=`When work is slow I find ways to be productive.`,
  Item_22=`I express enthusiasm for my job while at work.`,
  Item_23=`I try my best to perform well at work.`,
  Item_24=`If I notice my energy level is low, I take corrective steps to re-energize.`,
  Item_25=`I plan my future with this company.`,
  Item_26=`I believe this company cares about my career goals.`,
  Item_27=`I often think about finding another job.`,
  Item_28=`This organization challenges me to work at my full potential.`,
  Item_29=`I am proud to be a member of this organization.`,
  Item_30=`I feel supported by my supervisor when I fail at a task.`,
  Item_31=`I feel proud of my accomplishments within this organization.`,
  Item_32=`My job makes me feel like Iâ€™m part of something meaningful.`,
  Item_33=`I make valued contributions to the organization.`,
  Item_34=`I embrace challenging situations at work.`,
  Item_35=`I speak positively about this organization to others.`,
  Item_36=`This organization provides the resources necessary for me to successfully perform my job.`
) %>% 
  select(Item_1,Item_2,Item_3,Item_4,Item_5,Item_6,Item_7,Item_8,Item_9,Item_10,Item_11,Item_12,Item_13,Item_14,Item_15,Item_16,Item_17,
         Item_18,Item_19,Item_20,Item_21,Item_22,Item_23,Item_24,Item_25,Item_26,Item_27,Item_28,Item_29,Item_30,Item_31,Item_32,Item_33,
         Item_34,Item_35,Item_36)

######Bifactor model
Bifactor_Model_Cond2<-'
Cognitive=~Item_1+Item_2+Item_3+Item_4+Item_13+Item_14+Item_15+Item_16+Item_25+Item_26+Item_27+Item_28
Affective=~Item_5+Item_6+Item_7+Item_8+Item_17+Item_18+Item_19+Item_20+Item_29+Item_30+Item_31+Item_32
Behavioral=~Item_9+Item_10+Item_11+Item_12+Item_21+Item_22+Item_23+Item_24+Item_33+Item_34+Item_35+Item_36
Absorption=~Item_1+Item_2+Item_3+Item_4+Item_5+Item_6+Item_7+Item_8+Item_9+Item_10+Item_11+Item_12
Vigor=~Item_13+Item_14+Item_15+Item_16+Item_17+Item_18+Item_19+Item_20+Item_21+Item_22+Item_23+Item_24
Dedication=~Item_25+Item_26+Item_27+Item_28+Item_29+Item_30+Item_31+Item_32+Item_33+Item_34+Item_35+Item_36
Absorption ~~ 0*Affective
Absorption ~~ 0*Behavioral
Absorption ~~ 0*Cognitive
Vigor ~~ 0*Affective
Vigor ~~ 0*Behavioral
Vigor ~~ 0*Cognitive
Dedication ~~ 0*Affective
Dedication ~~ 0*Behavioral
Dedication ~~ 0*Cognitive
'

Fit.Bi_Cond2 <- lavaan::cfa(Bifactor_Model_Cond2, data = Cond2_Model) #, likelihood = "wishart")

semPlot::semPaths(Fit.Bi_Cond2, bifactor = c("Cognitive", "Affective", "Behavioral"), "std", layout = "tree3", 
                  rotation = 2, curvePivot=TRUE, style="lisrel", nCharNodes = 0)  ## exoCov=FALSE deletes all covariances
title("Condition2 bifactor analysis (36 candidate items)")

# Condition 3 _________________________________________________________________________________________
Cond3_Model<-cond3.red[,2:37]

Cond3_Model<-Cond3_Model%>%rename(
  Item_1=`Iâ€™m able to concentrate on my work without distractions.`,
  Item_2=`I have a hard time detaching mentally from my work.`,
  Item_3=`Time passes quickly while Iâ€™m working.`,
  Item_4=`I find it difficult to mentally disconnect from work.`,
  Item_5=`I enjoy thinking about work even when Iâ€™m not at work.`,
  Item_6=`Most days, I feel happiest when the workday is soon to be complete.`,
  Item_7=`I am happiest when I am immersed in a project.`,
  Item_8=`I love starting my workday.`,
  Item_9=`I devote more time than is expected of me.`,
  Item_10=`I have to be reminded to take breaks while Iâ€™m at work.`,
  Item_11=`I never miss a work deadline.`,
  Item_12=`I never allow distractions to interfere with my work.`,
  Item_13=`I devote my full attention to my work tasks throughout the day.`,
  Item_14=`Thinking about work saps my energy.`,
  Item_15=`I would rather direct my focus toward a work task than a personal task.`,
  Item_16=`Iâ€™m able to maintain good levels of energy throughout the workday.`,
  Item_17=`I enjoy spending time completing my job tasks.`,
  Item_18=`Most days I feel enthusiastic about starting my work day.`,
  Item_19=`I feel motivated to go beyond what is asked of me.`,
  Item_20=`This job drains my energy.`,
  Item_21=`When work is slow I find ways to be productive.`,
  Item_22=`I express enthusiasm for my job while at work.`,
  Item_23=`I try my best to perform well at work.`,
  Item_24=`If I notice my energy level is low, I take corrective steps to re-energize.`,
  Item_25=`I plan my future with this company.`,
  Item_26=`I believe this company cares about my career goals.`,
  Item_27=`I often think about finding another job.`,
  Item_28=`This organization challenges me to work at my full potential.`,
  Item_29=`I am proud to be a member of this organization.`,
  Item_30=`I feel supported by my supervisor when I fail at a task.`,
  Item_31=`I feel proud of my accomplishments within this organization.`,
  Item_32=`My job makes me feel like Iâ€™m part of something meaningful.`,
  Item_33=`I make valued contributions to the organization.`,
  Item_34=`I embrace challenging situations at work.`,
  Item_35=`I speak positively about this organization to others.`,
  Item_36=`This organization provides the resources necessary for me to successfully perform my job.`
) %>% 
  select(Item_1,Item_2,Item_3,Item_4,Item_5,Item_6,Item_7,Item_8,Item_9,Item_10,Item_11,Item_12,Item_13,Item_14,Item_15,Item_16,Item_17,
         Item_18,Item_19,Item_20,Item_21,Item_22,Item_23,Item_24,Item_25,Item_26,Item_27,Item_28,Item_29,Item_30,Item_31,Item_32,Item_33,
         Item_34,Item_35,Item_36)

######Bifactor model
Bifactor_Model_Cond3<-'
Cognitive=~Item_1+Item_2+Item_3+Item_4+Item_13+Item_14+Item_15+Item_16+Item_25+Item_26+Item_27+Item_28
Affective=~Item_5+Item_6+Item_7+Item_8+Item_17+Item_18+Item_19+Item_20+Item_29+Item_30+Item_31+Item_32
Behavioral=~Item_9+Item_10+Item_11+Item_12+Item_21+Item_22+Item_23+Item_24+Item_33+Item_34+Item_35+Item_36
Absorption=~Item_1+Item_2+Item_3+Item_4+Item_5+Item_6+Item_7+Item_8+Item_9+Item_10+Item_11+Item_12
Vigor=~Item_13+Item_14+Item_15+Item_16+Item_17+Item_18+Item_19+Item_20+Item_21+Item_22+Item_23+Item_24
Dedication=~Item_25+Item_26+Item_27+Item_28+Item_29+Item_30+Item_31+Item_32+Item_33+Item_34+Item_35+Item_36
Absorption ~~ 0*Affective
Absorption ~~ 0*Behavioral
Absorption ~~ 0*Cognitive
Vigor ~~ 0*Affective
Vigor ~~ 0*Behavioral
Vigor ~~ 0*Cognitive
Dedication ~~ 0*Affective
Dedication ~~ 0*Behavioral
Dedication ~~ 0*Cognitive
'

Fit.Bi_Cond3 <- lavaan::cfa(Bifactor_Model_Cond3, data = Cond3_Model) #, likelihood = "wishart")

semPlot::semPaths(Fit.Bi_Cond3, bifactor = c("Cognitive", "Affective", "Behavioral"), "std", layout = "tree3", 
                  rotation = 2, curvePivot=TRUE, style="lisrel", nCharNodes = 0)  ## exoCov=FALSE deletes all covariances
title("Condition2 bifactor analysis (36 candidate items)")

# Condition 3 _________________________________________________________________________________________
Cond4_Model<-cond4.red[,2:37]

Cond4_Model<-Cond4_Model%>%rename(
  Item_1=`Iâ€™m able to concentrate on my work without distractions.`,
  Item_2=`I have a hard time detaching mentally from my work.`,
  Item_3=`Time passes quickly while Iâ€™m working.`,
  Item_4=`I find it difficult to mentally disconnect from work.`,
  Item_5=`I enjoy thinking about work even when Iâ€™m not at work.`,
  Item_6=`Most days, I feel happiest when the workday is soon to be complete.`,
  Item_7=`I am happiest when I am immersed in a project.`,
  Item_8=`I love starting my workday.`,
  Item_9=`I devote more time than is expected of me.`,
  Item_10=`I have to be reminded to take breaks while Iâ€™m at work.`,
  Item_11=`I never miss a work deadline.`,
  Item_12=`I never allow distractions to interfere with my work.`,
  Item_13=`I devote my full attention to my work tasks throughout the day.`,
  Item_14=`Thinking about work saps my energy.`,
  Item_15=`I would rather direct my focus toward a work task than a personal task.`,
  Item_16=`Iâ€™m able to maintain good levels of energy throughout the workday.`,
  Item_17=`I enjoy spending time completing my job tasks.`,
  Item_18=`Most days I feel enthusiastic about starting my work day.`,
  Item_19=`I feel motivated to go beyond what is asked of me.`,
  Item_20=`This job drains my energy.`,
  Item_21=`When work is slow I find ways to be productive.`,
  Item_22=`I express enthusiasm for my job while at work.`,
  Item_23=`I try my best to perform well at work.`,
  Item_24=`If I notice my energy level is low, I take corrective steps to re-energize.`,
  Item_25=`I plan my future with this company.`,
  Item_26=`I believe this company cares about my career goals.`,
  Item_27=`I often think about finding another job.`,
  Item_28=`This organization challenges me to work at my full potential.`,
  Item_29=`I am proud to be a member of this organization.`,
  Item_30=`I feel supported by my supervisor when I fail at a task.`,
  Item_31=`I feel proud of my accomplishments within this organization.`,
  Item_32=`My job makes me feel like Iâ€™m part of something meaningful.`,
  Item_33=`I make valued contributions to the organization.`,
  Item_34=`I embrace challenging situations at work.`,
  Item_35=`I speak positively about this organization to others.`,
  Item_36=`This organization provides the resources necessary for me to successfully perform my job.`
) %>% 
  select(Item_1,Item_2,Item_3,Item_4,Item_5,Item_6,Item_7,Item_8,Item_9,Item_10,Item_11,Item_12,Item_13,Item_14,Item_15,Item_16,Item_17,
         Item_18,Item_19,Item_20,Item_21,Item_22,Item_23,Item_24,Item_25,Item_26,Item_27,Item_28,Item_29,Item_30,Item_31,Item_32,Item_33,
         Item_34,Item_35,Item_36)

######Bifactor model
Bifactor_Model_Cond4<-'
Cognitive=~Item_1+Item_2+Item_3+Item_4+Item_13+Item_14+Item_15+Item_16+Item_25+Item_26+Item_27+Item_28
Affective=~Item_5+Item_6+Item_7+Item_8+Item_17+Item_18+Item_19+Item_20+Item_29+Item_30+Item_31+Item_32
Behavioral=~Item_9+Item_10+Item_11+Item_12+Item_21+Item_22+Item_23+Item_24+Item_33+Item_34+Item_35+Item_36
Absorption=~Item_1+Item_2+Item_3+Item_4+Item_5+Item_6+Item_7+Item_8+Item_9+Item_10+Item_11+Item_12
Vigor=~Item_13+Item_14+Item_15+Item_16+Item_17+Item_18+Item_19+Item_20+Item_21+Item_22+Item_23+Item_24
Dedication=~Item_25+Item_26+Item_27+Item_28+Item_29+Item_30+Item_31+Item_32+Item_33+Item_34+Item_35+Item_36
Absorption ~~ 0*Affective
Absorption ~~ 0*Behavioral
Absorption ~~ 0*Cognitive
Vigor ~~ 0*Affective
Vigor ~~ 0*Behavioral
Vigor ~~ 0*Cognitive
Dedication ~~ 0*Affective
Dedication ~~ 0*Behavioral
Dedication ~~ 0*Cognitive
'

Fit.Bi_Cond4 <- lavaan::cfa(Bifactor_Model_Cond4, data = Cond4_Model) #, likelihood = "wishart")

semPlot::semPaths(Fit.Bi_Cond4, bifactor = c("Cognitive", "Affective", "Behavioral"), "std", layout = "tree3", 
                  rotation = 2, curvePivot=TRUE, style="lisrel", nCharNodes = 0)  ## exoCov=FALSE deletes all covariances
title("Condition2 bifactor analysis (36 candidate items)")
# Loading packages
library(tidyverse)


# Importing Data from Qualtrics 
temp <- read.csv("Data/qualtrics_pilot_data.csv", header=FALSE, na.strings="")

# Creating Header Columns
x <- temp[2,] # Takes row 2 and places it into an object
data <- temp[-c(1:3),] # Remove row 1 to 2
colnames(data) <- x # turn row 2 from line 5 into header of dataframe

num <- nrow(data) # Tells us how many participants we have in our dataset

#############Demographics##############################################


# Creating csv sheets for tenure and comments
write.csv(table(data[165]), "comments.csv") # looks at column 165 and turns into a csv sheet
#write.csv(table(data[164]), "tenure.csv")
table(data[163]) # turns column 163 into a table

# Cleaning up demographic- Specfically how many hours work
Hours<-data$`How many hours do you typically work per week in this job?` # Taking all response & putting it into an object
Hours<-na.omit(Hours) # Removing na's

Hours<-gsub("\\D","",Hours,ignore.case = TRUE, fixed =FALSE) # Removes anything that is not a digit
                                                             # For example, "10-16 hours" will becomes "1016"

hours<-as.data.frame(substr(Hours,1,2)) # takes the first two digits of the string, therefore rounding down response
                                        # For example, "1016" will now become 16"

hours<-hours%>%rename(Hours=`substr(Hours, 1, 2)`) # renames columns
hours$Hours <- as.numeric(as.character(hours$Hours)) # changes character to numeric

ggplot(hours, aes(x = Hours)) +           
  geom_histogram(alpha = 0.5, position = "identity") +
  geom_vline(aes(xintercept=mean(hours, na.rm=T)),   # Ignore NA values for mean
             color="blue", linetype="dotted", size=1) +
  labs(x="Average number of hours worked (per week)")


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



################### Item Drops ###################

# Looking at R Drops to determine what Items to drop. 
## Looking at the cell level which is the combination of attitudinal and substantive 

### Looks at all items in the cell level

aff.abs <- psych::alpha(together[,c(2:5)],check.keys = T)
aff.vig <- psych::alpha(together[6:9],check.keys = T)
aff.ded <- psych::alpha(together[,c(10:13)],check.keys = T)

beh.abs <- psych::alpha(together[,c(14:17)],check.keys = T)
beh.vig <- psych::alpha(together[,c(18:21)],check.keys = T)
beh.ded <- psych::alpha(together[,c(22:25)],check.keys = T)

cog.vig <- psych::alpha(together[,c(26:29)],check.keys = T)
cog.ded <- psych::alpha(together[,c(30:33)],check.keys = T)
cog.abs <- psych::alpha(together[,c(34:37)],check.keys = T)

aff.abs
aff.vig
aff.ded

beh.abs
beh.vig
beh.ded

cog.abs
cog.vig
cog.ded

## CFA Analysis

library(lavaan)
library(semPlot)

CFAdata<-together[,2:37]


CFAdata<-CFAdata%>%rename(
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
)


Sub_Model<-'
Absorption=~Item_1+Item_2+Item_3+Item_4+Item_5+Item_6+Item_7+Item_8+Item_9+Item_10+Item_11+Item_12
Vigor=~Item_13+Item_14+Item_15+Item_16+Item_17+Item_18+Item_19+Item_20+Item_21+Item_22+Item_23+Item_24
Dedication=~Item_25+Item_26+Item_27+Item_28+Item_29+Item_30+Item_31+Item_32+Item_33+Item_34+Item_35+Item_36
'

Fit1.1<-lavaan::cfa(Sub_Model, data = CFAdata)
semPlot::semPaths(Fit1.1, "std")
fit1.1 <- as.data.frame(fitMeasures(Fit1.1))
summary(Fit1.1, fit.measure=TRUE)

#write.csv(fit1.1, "temp.csv")

## Looking at scale correlations because of very large latent covariances

testing <- CFAdata

testing$absorp <- rowMeans(testing[1:12], na.rm=TRUE)
testing$vigor <- rowMeans(testing[13:24], na.rm=TRUE)
testing$dedic <- rowMeans(testing[25:36], na.rm=TRUE)

cor(testing[37:39], use="complete.obs")

## plot(testing$absorp, testing$dedic)

Att_Model<-'
Cognitive=~Item_1+Item_2+Item_3+Item_4+Item_13+Item_14+Item_15+Item_16+Item_25+Item_26+Item_27+Item_28
Affective=~Item_5+Item_6+Item_7+Item_8+Item_17+Item_18+Item_19+Item_20+Item_29+Item_30+Item_31+Item_32
Behavioral=~Item_9+Item_10+Item_11+Item_12+Item_21+Item_22+Item_23+Item_24+Item_33+Item_34+Item_35+Item_36
'
Fit1.2<-lavaan::cfa(Att_Model, data = CFAdata)
semPlot::semPaths(Fit1.2,"std")
lavaan::fitMeasures(Fit1.2)
summary(Fit1.2, fit.measure=TRUE)


######Bi factor model
Bifactor_Model<-'
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


Fit.Bi <- lavaan::cfa(Bifactor_Model, data = CFAdata) #, likelihood = "wishart")

semPlot::semPaths(Fit.Bi, bifactor = c("Cognitive", "Affective", "Behavioral"), "std", layout = "tree3", 
                  rotation = 2, curvePivot=TRUE, style="lisrel", nCharNodes = 0)  ## exoCov=FALSE deletes all covariances
title("Initial pilot bifactor analysis (36 candidate items)")

modindices(Fit.Bi, sort = TRUE, maximum.number = 5)

summary(Fit.Bi)



#############Item Reduction-3 items #######################
## Aff-Abs
psych::alpha(together[,c(2,3,4)],check.keys = T) #alpha-.53, r.drops: .44, .34, .28
psych::alpha(together[,c(2,3,5)],check.keys = T) #alpha-.66, r.drops: .55, .38, .50
psych::alpha(together[,c(3,4,5)],check.keys = T) #alpha-.48, r.drops: .29, .26, .36


## Aff-Vig
psych::alpha(together[,c(6,7,9)],check.keys = T) #alpha-.66, r.drops: .48, .60, .37
psych::alpha(together[,c(6,8,9)],check.keys = T) #alpha-.55, r.drops: .48, .41, .23
psych::alpha(together[,c(7,8,9)],check.keys = T) #alpha-.60, r.drops: .54, .36, .35


## Aff-Ded
psych::alpha(together[,c(10,11,12)],check.keys = T) #alpha-.62, r.drops: .49, .35, .50
psych::alpha(together[,c(10,12,13)],check.keys = T) #alpha-.80, r.drops: .65, .61, .68
psych::alpha(together[,c(11,12,13)],check.keys = T) #alpha-.65, r.drops: .37, .52, .54


## Beh-Abs
psych::alpha(together[,c(14,15,16)],check.keys = T) #alpha-.42, r.drops: .31, .38, .10
psych::alpha(together[,c(14,16,17)],check.keys = T) #alpha-.46, r.drops: .20, .23, .44
psych::alpha(together[,c(15,16,17)],check.keys = T) #alpha-.47, r.drops: .23, .29, .38


## Beh-vig
psych::alpha(together[,c(18,19,20)],check.keys = T) #alpha-.69, r.drops: .50, .53, .54
psych::alpha(together[,c(18,20,21)],check.keys = T) #alpha-.60, r.drops: .45, .45, .37
psych::alpha(together[,c(19,20,21)],check.keys = T) #alpha-.62, r.drops: .48, .49, .37

## Bed-ded
psych::alpha(together[,c(22,23,24)],check.keys = T) #alpha-.60, r.drops: .43, .49, .37
psych::alpha(together[,c(22,24,25)],check.keys = T) #alpha-.56, r.drops: .25, .49, .41
psych::alpha(together[,c(23,24,25)],check.keys = T) #alpha-.61, r.drops: .35, .51, .43


## Cog-Vig
psych::alpha(together[,c(26,27,28)],check.keys = T) #alpha-.41, r.drops: .27, .18, .28
psych::alpha(together[,c(26,27,29)],check.keys = T) #alpha-.62, r.drops: .35, .33, .62
psych::alpha(together[,c(27,28,29)],check.keys = T) #alpha-.55, r.drops: .36, .25, .49

## Cog-def
psych::alpha(together[,c(30,31,32)],check.keys = T) #alpha-.80, r.drops: .67, .61, .65
psych::alpha(together[,c(30,31,33)],check.keys = T) #alpha-.80, r.drops: .59, .70, .66
psych::alpha(together[,c(31,32,33)],check.keys = T) #alpha-.79, r.drops: .69, .56, .66

## Cog-abs
psych::alpha(together[,c(34,35,36)],check.keys = T) #alpha-.39, r.drops: .25, .09, .39
psych::alpha(together[,c(34,36,37)],check.keys = T) #alpha-.33, r.drops: .22, .35, .03
psych::alpha(together[,c(35,36,37)],check.keys = T) #alpha-.68, r.drops: .77, .12, .71

### Looking at alpha all together
#### Substantive
##### Affective
psych::alpha(together[,c(2,3,5,6,7,9,10,12,13)])

##### Behavioral
psych::alpha(together[,c(15,16,17,18,19,20,23,24,25)])

##### Cognitive
psych::alpha(together[,c(26,27,29,30,31,32,35,36,37)]) #alpha -.76
psych::alpha(together[,c(26,27,29,30,31,33,35,36,37)]) #alpha -.77
psych::alpha(together[,c(26,27,29,31,32,33,35,36,37)]) #alpha -.77

#### Attitudinal
##### Absorption
psych::alpha(together[,c(2,3,5,15,16,17,35,36,37)])
##### Dedication
psych::alpha(together[,c(10,12,13,23,24,25,30,31,32)]) #alpha-.88
psych::alpha(together[,c(10,12,13,23,24,25,30,31,33)]) #alpha-.89
psych::alpha(together[,c(10,12,13,23,24,25,31,32,33)]) #alpha-.89
##### Vigor
psych::alpha(together[,c(6,7,9,18,19,20,26,27,29)]) #alpha-.83


##### all together
psych::alpha(together[,c(2,3,5,6,7,9,10,12,13,15,16,17,18,19,20,23,24,25,26,27,29,31,32,33,35,36,37)])

Reduction_3Items<-together[,c(2,3,5,6,7,9,10,12,13,15,16,17,18,19,20,23,24,25,26,27,29,31,32,33,35,36,37)]
### Creating CFA for three item response 
Reduction_3Items<-Reduction_3Items%>%rename(
#  Item_1=`Iâ€™m able to concentrate on my work without distractions.`,
  Item_2=`I have a hard time detaching mentally from my work.`,
  Item_3=`Time passes quickly while Iâ€™m working.`,
  Item_4=`I find it difficult to mentally disconnect from work.`,
  Item_5=`I enjoy thinking about work even when Iâ€™m not at work.`,
  Item_6=`Most days, I feel happiest when the workday is soon to be complete.`,
#  Item_7=`I am happiest when I am immersed in a project.`,
  Item_8=`I love starting my workday.`,
#  Item_9=`I devote more time than is expected of me.`,
  Item_10=`I have to be reminded to take breaks while Iâ€™m at work.`,
  Item_11=`I never miss a work deadline.`,
  Item_12=`I never allow distractions to interfere with my work.`,
  Item_13=`I devote my full attention to my work tasks throughout the day.`,
  Item_14=`Thinking about work saps my energy.`,
#  Item_15=`I would rather direct my focus toward a work task than a personal task.`,
  Item_16=`Iâ€™m able to maintain good levels of energy throughout the workday.`,
  Item_17=`I enjoy spending time completing my job tasks.`,
  Item_18=`Most days I feel enthusiastic about starting my work day.`,
#  Item_19=`I feel motivated to go beyond what is asked of me.`,
  Item_20=`This job drains my energy.`,
  Item_21=`When work is slow I find ways to be productive.`,
  Item_22=`I express enthusiasm for my job while at work.`,
  Item_23=`I try my best to perform well at work.`,
#  Item_24=`If I notice my energy level is low, I take corrective steps to re-energize.`,
#  Item_25=`I plan my future with this company.`,
  Item_26=`I believe this company cares about my career goals.`,
  Item_27=`I often think about finding another job.`,
  Item_28=`This organization challenges me to work at my full potential.`,
  Item_29=`I am proud to be a member of this organization.`,
#  Item_30=`I feel supported by my supervisor when I fail at a task.`,
  Item_31=`I feel proud of my accomplishments within this organization.`,
  Item_32=`My job makes me feel like Iâ€™m part of something meaningful.`,
#  Item_33=`I make valued contributions to the organization.`,
  Item_34=`I embrace challenging situations at work.`,
  Item_35=`I speak positively about this organization to others.`,
  Item_36=`This organization provides the resources necessary for me to successfully perform my job.`
)

Sub_Model_ItemReduction3<-'
Absorption=~Item_2+Item_3+Item_4+Item_5+Item_6+Item_8+Item_10+Item_11+Item_12
Vigor=~Item_13+Item_14+Item_16+Item_17+Item_18+Item_20+Item_21+Item_22+Item_23
Dedication=~+Item_26+Item_27+Item_28+Item_29+Item_31+Item_32+Item_34+Item_35+Item_36
'

Fit1.3<-lavaan::cfa(Sub_Model_ItemReduction3, data = Reduction_3Items)
semPlot::semPaths(Fit1.3,"std")
lavaan::fitMeasures(Fit1.3)
summary(Fit1.3, fit.measure=TRUE)

Att_Model_ItemReduction3<-'
Cognitive=~Item_2+Item_3+Item_4+Item_13+Item_14+Item_16+Item_26+Item_27+Item_28
Affective=~Item_5+Item_6+Item_8+Item_17+Item_18+Item_20+Item_29+Item_31+Item_32
Behavioral=~Item_10+Item_11+Item_12+Item_21+Item_22+Item_23+Item_34+Item_35+Item_36
'
Fit1.4<-lavaan::cfa(Att_Model_ItemReduction3, data = Reduction_3Items)
semPlot::semPaths(Fit1.2,"std")
lavaan::fitMeasures(Fit1.2)
summary(Fit1.2, fit.measure=TRUE)


#### Bifactor model 3 items ########
Bifactor_Mode2<-'
Cognitive=~Item_2+Item_3+Item_4+Item_13+Item_14+Item_16+Item_26+Item_27+Item_28
Affective=~Item_5+Item_6+Item_8+Item_17+Item_18+Item_20+Item_29+Item_31+Item_32
Behavioral=~Item_10+Item_11+Item_12+Item_21+Item_22+Item_23+Item_34+Item_35+Item_36
Absorption=~Item_2+Item_3+Item_4+Item_5+Item_6+Item_8+Item_10+Item_11+Item_12
Vigor=~Item_13+Item_14+Item_16+Item_17+Item_18+Item_20+Item_21+Item_22+Item_23
Dedication=~+Item_26+Item_27+Item_28+Item_29+Item_31+Item_32+Item_34+Item_35+Item_36
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


Fit.Bi2 <- lavaan::cfa(Bifactor_Mode2, data = Reduction_3Items) #, likelihood = "wishart")

semPlot::semPaths(Fit.Bi2, bifactor = c("Cognitive", "Affective", "Behavioral"), "std", layout = "tree3", 
                  rotation = 2, curvePivot=TRUE, style="lisrel", nCharNodes = 0)  ## exoCov=FALSE deletes all covariances
title("Initial pilot bifactor analysis (36 candidate items)")

modindices(Fit.Bi2, sort = TRUE, maximum.number = 5)

summary(Fit.Bi2)




############### Item Reduction- 2 Items #######################

####### Aff-Abs
psych::alpha(together[,c(2,3)],check.keys = T) #alpha -.53, r.drops - .36, .36
psych::alpha(together[,c(2,4)],check.keys = T) #alpha -.44, r.drops - .30, .30
psych::alpha(together[,c(3,4)],check.keys = T) #alpha -.27, r.drops - .16, .16
psych::alpha(together[,c(3,5)],check.keys = T) #alpha -.45, r.drops - .29, .29
psych::alpha(together[,c(4,5)],check.keys = T) #alpha -.39, r.drops - .25, .25
psych::alpha(together[,c(2,5)],check.keys = T) #alpha -.68, r.drops - .57, .57


####### Aff-Vig
psych::alpha(together[,c(6,7)],check.keys = T) #alpha -.70, r.drops -.56, .56
psych::alpha(together[,c(6,8)],check.keys = T) #alpha -.68, r.drops -.53, .53
psych::alpha(together[,c(6,9)],check.keys = T) #alpha -.36, r.drops -.23, .23
psych::alpha(together[,c(7,8)],check.keys = T) #alpha -.60, r.drops -.43, .43
psych::alpha(together[,c(7,9)],check.keys = T) #alpha -.58, r.drops -.41, .41
psych::alpha(together[,c(8,9)],check.keys = T) #alpha -.29, r.drops -.17, .17


####### Aff-Ded
psych::alpha(together[,c(10,11)],check.keys = T) #alpha -.47, r.drops -.31, .31
psych::alpha(together[,c(10,12)],check.keys = T) #alpha -.68, r.drops -.52, .52
psych::alpha(together[,c(10,13)],check.keys = T) #alpha -.77, r.drops -.62, .62
psych::alpha(together[,c(11,12)],check.keys = T) #alpha -.44, r.drops -.44, .44
psych::alpha(together[,c(11,13)],check.keys = T) #alpha -.52, r.drops -.35, .35
psych::alpha(together[,c(12,13)],check.keys = T) #alpha -.71, r.drops -.57, .57


###### Beh-Abs
psych::alpha(together[,c(14,15)],check.keys = T) #alpha -.56, r.drops -.40, .40
psych::alpha(together[,c(14,16)],check.keys = T) #alpha -.04, r.drops -.02, .02
psych::alpha(together[,c(14,17)],check.keys = T) #alpha -.45, r.drops -.29, .29
psych::alpha(together[,c(15,16)],check.keys = T) #alpha -.22, r.drops -.13, .13
psych::alpha(together[,c(15,17)],check.keys = T) #alpha -.39, r.drops -.24, .24
psych::alpha(together[,c(16,17)],check.keys = T) #alpha -.51, r.drops -.34, .34


###### Beh-vig
psych::alpha(together[,c(18,19)],check.keys = T) #alpha -.60, r.drops -.43, .43
psych::alpha(together[,c(18,20)],check.keys = T) #alpha -.56, r.drops -.43, .43
psych::alpha(together[,c(18,21)],check.keys = T) #alpha -.49, r.drops -.33, .33
psych::alpha(together[,c(19,20)],check.keys = T) #alpha -.63, r.drops -.49, .49
psych::alpha(together[,c(19,21)],check.keys = T) #alpha -.50, r.drops -.33, .33
psych::alpha(together[,c(20,21)],check.keys = T) #alpha -.44, r.drops -.31, .31



###### Bed-ded
psych::alpha(together[,c(22,23)],check.keys = T) #alpha -.64, r.drops -.47, .47
psych::alpha(together[,c(22,24)],check.keys = T) #alpha -.39, r.drops -.27, .27
psych::alpha(together[,c(22,25)],check.keys = T) #alpha -.24, r.drops -.15, .15
psych::alpha(together[,c(23,24)],check.keys = T) #alpha -.50, r.drops -.35, .35
psych::alpha(together[,c(23,25)],check.keys = T) #alpha -.37, r.drops -.23, .23
psych::alpha(together[,c(24,25)],check.keys = T) #alpha -.62, r.drops -.45, .45


###### Cog-Vig
psych::alpha(together[,c(26,29)],check.keys = T) #alpha -.65, r.drops -.49, .49
psych::alpha(together[,c(27,29)],check.keys = T) #alpha -.62, r.drops -.45, .45
psych::alpha(together[,c(28,29)],check.keys = T) #alpha -.45, r.drops -.29, .29
psych::alpha(together[,c(27,28)],check.keys = T) #alpha -.26, r.drops -.15, .15
psych::alpha(together[,c(27,29)],check.keys = T) #alpha -.62, r.drops -.45, .45
psych::alpha(together[,c(28,29)],check.keys = T) #alpha -.45, r.drops -.29, .29

###### Cog-def
psych::alpha(together[,c(30,31)],check.keys = T) #alpha -.72, r.drops -.56, .56
psych::alpha(together[,c(30,32)],check.keys = T) #alpha -.76, r.drops -.61, .61
psych::alpha(together[,c(30,33)],check.keys = T) #alpha -.66, r.drops -.51, .51
psych::alpha(together[,c(31,32)],check.keys = T) #alpha -.70, r.drops -.54, .54
psych::alpha(together[,c(31,33)],check.keys = T) #alpha -.80, r.drops -.67, .67
psych::alpha(together[,c(32,33)],check.keys = T) #alpha -.64, r.drops -.48, .48


###### Cog-abs
psych::alpha(together[,c(34,35)],check.keys = T) #alpha -.02, r.drops -.01, .01
psych::alpha(together[,c(34,36)],check.keys = T) #alpha -.58, r.drops -.42, .42, Second best option
psych::alpha(together[,c(34,37)],check.keys = T) #alpha -.04, r.drops -.02, .02
psych::alpha(together[,c(35,36)],check.keys = T) #alpha -.25, r.drops -.15, .15
psych::alpha(together[,c(35,37)],check.keys = T) #alpha -.94, r.drops -.89, .89, Considering that these two items are extremely similar, we are going with the second best option
psych::alpha(together[,c(36,37)],check.keys = T) #alpha -.16, r.drops -.09, .09



### Looking at alpha all together
#### Substantive
##### Affective
psych::alpha(together[,c(2,5,6,7,10,13)])

##### Behavioral
psych::alpha(together[,c(14,15,19,20,22,23)]) #alpha -.71
psych::alpha(together[,c(16,17,19,20,22,23)]) #alpha -.69

##### Cognitive
psych::alpha(together[,c(26,29,31,33,35,37)]) #alpha-.7
psych::alpha(together[,c(26,29,31,33,34,36)]) #alpha-.79

#### Attitudinal
##### Absorption
psych::alpha(together[,c(2,5,14,15,35,37)]) #alpha -.72
psych::alpha(together[,c(2,5,16,17,35,37)]) #alpha -.61
psych::alpha(together[,c(2,5,14,15,34,36)]) #alpha -.69
psych::alpha(together[,c(2,5,16,17,34,36)]) #alpha -.71
##### Dedication
psych::alpha(together[,c(10,13,22,23,31,33)]) 
##### Vigor
psych::alpha(together[,c(6,7,19,20,26,29)]) 


##### all together
psych::alpha(together[,c(2,5,6,7,10,13,14,15,19,20,22,23,26,29,31,33,35,37)]) #alpha-.89
psych::alpha(together[,c(2,5,6,7,10,13,14,15,19,20,22,23,26,29,31,33,34,36)]) #alpha-.90

Reduction_2Items<-together[,c(2,5,6,7,10,13,14,15,19,20,22,23,26,29,31,33,34,36)]

Reduction_2Items<-Reduction_2Items%>%rename(
  Item_1=`Iâ€™m able to concentrate on my work without distractions.`,
 # Item_2=`I have a hard time detaching mentally from my work.`,
  Item_3=`Time passes quickly while Iâ€™m working.`,
 # Item_4=`I find it difficult to mentally disconnect from work.`,
  Item_5=`I enjoy thinking about work even when Iâ€™m not at work.`,
 # Item_6=`Most days, I feel happiest when the workday is soon to be complete.`,
 # Item_7=`I am happiest when I am immersed in a project.`,
  Item_8=`I love starting my workday.`,
  Item_9=`I devote more time than is expected of me.`,
  Item_10=`I have to be reminded to take breaks while Iâ€™m at work.`,
 # Item_11=`I never miss a work deadline.`,
 # Item_12=`I never allow distractions to interfere with my work.`,
  Item_13=`I devote my full attention to my work tasks throughout the day.`,
 # Item_14=`Thinking about work saps my energy.`,
 # Item_15=`I would rather direct my focus toward a work task than a personal task.`,
  Item_16=`Iâ€™m able to maintain good levels of energy throughout the workday.`,
  Item_17=`I enjoy spending time completing my job tasks.`,
  Item_18=`Most days I feel enthusiastic about starting my work day.`,
 # Item_19=`I feel motivated to go beyond what is asked of me.`,
 # Item_20=`This job drains my energy.`,
 # Item_21=`When work is slow I find ways to be productive.`,
  Item_22=`I express enthusiasm for my job while at work.`,
  Item_23=`I try my best to perform well at work.`,
 # Item_24=`If I notice my energy level is low, I take corrective steps to re-energize.`,
 # Item_25=`I plan my future with this company.`,
  Item_26=`I believe this company cares about my career goals.`,
 # Item_27=`I often think about finding another job.`,
  Item_28=`This organization challenges me to work at my full potential.`,
  Item_29=`I am proud to be a member of this organization.`,
 # Item_30=`I feel supported by my supervisor when I fail at a task.`,
 # Item_31=`I feel proud of my accomplishments within this organization.`,
  Item_32=`My job makes me feel like Iâ€™m part of something meaningful.`,
  Item_33=`I make valued contributions to the organization.`,
  Item_34=`I embrace challenging situations at work.`,
 # Item_35=`I speak positively about this organization to others.`,
 # Item_36=`This organization provides the resources necessary for me to successfully perform my job.`
)



Sub_Model_ItemReduction2<-'
Absorption=~Item_1+Item_3+Item_5++Item_8+Item_9+Item_10
Vigor=~Item_13+Item_16+Item_17+Item_18+Item_22+Item_23
Dedication=~Item_26+Item_28+Item_29+Item_32+Item_33+Item_34
'

Fit1.4<-lavaan::cfa(Sub_Model_ItemReduction2, data = Reduction_2Items)
semPlot::semPaths(Fit1.4,"std")
lavaan::fitMeasures(Fit1.4)
summary(Fit1.4, fit.measure=TRUE)

Att_Model_ItemReduction2<-'
Cognitive=~Item_1+Item_3+Item_13+Item_16+Item_26+Item_28
Affective=~Item_5+Item_8+Item_17+Item_18+Item_29+Item_32
Behavioral=~Item_9+Item_10+Item_22+Item_23+Item_33+Item_34
'
Fit1.5<-lavaan::cfa(Att_Model_ItemReduction2, data = Reduction_2Items)
semPlot::semPaths(Fit1.5,"std")
lavaan::fitMeasures(Fit1.5)
summary(Fit1.5, fit.measure=TRUE)


#### Bifactor model 2 items
Bifactor_Model3<-'
Cognitive=~Item_1+Item_3+Item_13+Item_16+Item_26+Item_28
Affective=~Item_5+Item_8+Item_17+Item_18+Item_29+Item_32
Behavioral=~Item_9+Item_10+Item_22+Item_23+Item_33+Item_34
Absorption=~Item_1+Item_3+Item_5++Item_8+Item_9+Item_10
Vigor=~Item_13+Item_16+Item_17+Item_18+Item_22+Item_23
Dedication=~Item_26+Item_28+Item_29+Item_32+Item_33+Item_34
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


Fit.Bi3 <- lavaan::cfa(Bifactor_Model3, data = Reduction_2Items) #, likelihood = "wishart")

semPlot::semPaths(Fit.Bi3, bifactor = c("Cognitive", "Affective", "Behavioral"), "std", layout = "tree3", 
                  rotation = 2, curvePivot=TRUE, style="lisrel", nCharNodes = 0)  ## exoCov=FALSE deletes all covariances
title("Initial pilot bifactor analysis (36 candidate items)")

modindices(Fit.Bi3, sort = TRUE, maximum.number = 5)

summary(Fit.Bi3)

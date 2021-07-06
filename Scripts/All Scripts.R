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
write.csv(table(data[164]), "tenure.csv")
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

write.csv(together, "together.csv")

################### Item Drops ###################

# Looking at R Drops to determine what Items to drop. 
## Looking at the cell level which is the combination of attitudinal and substantive 






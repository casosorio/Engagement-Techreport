# Item Analysis w/ Items

## Reduction 3 Items

## Absorption - Cognitive 
psych::alpha(CFAdata[,c(1,2,3)],check.keys = T) #raw alpha-.39, std.alpha-.42, r.drops: .25, .089, .391
psych::alpha(CFAdata[,c(1,2,4)],check.keys = T) #raw alpha-.59, std.alpha-.56, r.drops: .007, .65, .677
psych::alpha(CFAdata[,c(1,3,4)],check.keys = T) #raw alpha-.33, std.alpha-.37, r.drops: .222, .351, .034
psych::alpha(CFAdata[,c(2,3,4)],check.keys = T) #raw alpha-.68, std.alpha-.65, r.drops: .77, .12, .71 - Best R Drops

#### Items 2 and 4 very similar to each other, maybe it would be best to go with item 1, 2, and 3

## Absorption - Affective 
psych::alpha(CFAdata[,c(5,6,7)],check.keys = T) #raw alpha-.53, std.alpha-.53, r.drops: .44, .34, .28  
psych::alpha(CFAdata[,c(5,6,8)],check.keys = T) #raw alpha-.66, std.alpha-.66, r.drops: .55, .38, .50 # Best R Drops
psych::alpha(CFAdata[,c(5,7,8)],check.keys = T) #raw alpha-.63, std.alpha-.62, r.drops: .53, .32, .51 
psych::alpha(CFAdata[,c(6,7,8)],check.keys = T) #raw alpha-.48, std.alpha-.48, r.drops: .29, .26, .36


## Absoption - Behavioral
psych::alpha(CFAdata[,c(9,10,11)],check.keys = T) #raw alpha-.42, std.alpha-.40.,r.drops: .309, .376, .096 
psych::alpha(CFAdata[,c(9,10,12)],check.keys = T) #raw alpha-.57, std.alpha-.58, r.drops: .44, .40, .32 # Best R Drops
psych::alpha(CFAdata[,c(9,11,12)],check.keys = T) #raw alpha-.46, std.alpha-.46, r.drops: .20, .23, .44  
psych::alpha(CFAdata[,c(10,11,12)],check.keys = T) #raw alpha-.47, std.alpha-.48, r.drops: .23, .29, .38


## Vigor - Cognitive
psych::alpha(CFAdata[,c(13,14,15)],check.keys = T) #raw alpha-.41, std.alpha-.41, r.drops: .27, .18, .28 
psych::alpha(CFAdata[,c(13,14,16)],check.keys = T) #raw alpha-.62, std.alpha-.62, r.drops: .35, .33, .62 
psych::alpha(CFAdata[,c(13,15,16)],check.keys = T) #raw alpha-.61, std.alpha-.62, r.drops: .47, .33, .48  # Best R drops
psych::alpha(CFAdata[,c(14,15,16)],check.keys = T) #raw alpha-.55, std.alpha-.56, r.drops: .36, .25, .49


## Vigor - Affective
psych::alpha(CFAdata[,c(17,18,19)],check.keys = T) #raw alpha-.74, std.alpha-.75, r.drops: .65, .56, .53 # Best R drops
psych::alpha(CFAdata[,c(17,18,20)],check.keys = T) #raw alpha-.66, std.alpha-.67, r.drops: .48, .60, .37
psych::alpha(CFAdata[,c(17,19,20)],check.keys = T) #raw alpha-.55, std.alpha-.57, r.drops: .48, .41, .23
psych::alpha(CFAdata[,c(18,19,20)],check.keys = T) #raw alpha-.60, std.alpha-.60, r.drops: .54, .36, .35

## Vigor - Behavioral
psych::alpha(CFAdata[,c(21,22,23)],check.keys = T) #raw alpha-.69, std.alpha-.71, r.drops: .50, .53, .54 # Best R drops
psych::alpha(CFAdata[,c(21,22,24)],check.keys = T) #raw alpha-.63, std.alpha-.63, r.drops: .46, .47, .39
psych::alpha(CFAdata[,c(21,23,24)],check.keys = T) #raw alpha-.60, std.alpha-.62, r.drops: .45, .45, .37
psych::alpha(CFAdata[,c(22,23,24)],check.keys = T) #raw alpha-.62, std.alpha-.64, r.drops: .48, .49, .37


## Dedication - Cognitive
psych::alpha(CFAdata[,c(25,26,27)],check.keys = T) #raw alpha-.80, std.alpha-.80, r.drops: .67, .61, .65
psych::alpha(CFAdata[,c(25,26,28)],check.keys = T) #raw alpha-.80, std.alpha-.81, r.drops: .59, .70, .66 # Best R drops
psych::alpha(CFAdata[,c(25,27,28)],check.keys = T) #raw alpha-.77, std.alpha-.78, r.drops: .66, .64, .55
psych::alpha(CFAdata[,c(26,27,28)],check.keys = T) #raw alpha-.79, std.alpha-.79, r.drops: .69, .56, .66

# 25, 26, 27 might be another good option since item 27 is reverse coded item

## Dedication - Affective
psych::alpha(CFAdata[,c(29,30,31)],check.keys = T) #raw alpha-.62, std.alpha-.64, r.drops: .49, .35, .50
psych::alpha(CFAdata[,c(29,30,32)],check.keys = T) #raw alpha-.68, std.alpha-.69, r.drops: .56, .36, .59
psych::alpha(CFAdata[,c(29,31,32)],check.keys = T) #raw alpha-.80, std.alpha-.80, r.drops: .65, .61, .68 # Best R Drops
psych::alpha(CFAdata[,c(30,31,32)],check.keys = T) #raw alpha-.65, std.alpha-.67, r.drops: .37, .52, .54

## Dedication - Behavioral
psych::alpha(CFAdata[,c(33,34,35)],check.keys = T) #raw alpha-.60, std.alpha-.63, r.drops: .43, .49, .37 
psych::alpha(CFAdata[,c(33,34,36)],check.keys = T) #raw alpha-.50, std.alpha-.55, r.drops: .36, .42, .23
psych::alpha(CFAdata[,c(33,35,36)],check.keys = T) #raw alpha-.56, std.alpha-.55, r.drops: .25, .49, .41
psych::alpha(CFAdata[,c(34,35,36)],check.keys = T) #raw alpha-.61, std.alpha-.61, r.drops: .35, .51, .43 # Best R Drops

psych::alpha(CFAdata[,c(1,2,3,5,6,8,9,10,12,13,15,16,17,18,19,21,22,23,25,26,28,29,31,32,34,35,36)],check.keys=T) # raw alpha: .92
psych::alpha(CFAdata[,c(2,3,4,5,6,8,9,10,12,13,15,16,17,18,19,21,22,23,25,26,28,29,31,32,34,35,36)],check.keys=T) # raw alpha: .92

psych::alpha(CFAdata[,c(1,2,3,5,6,8,9,10,12,13,15,16,17,18,19,21,22,23,25,26,28,29,31,32,33,34,35)],check.keys=T) # raw alpha: .92
psych::alpha(CFAdata[,c(2,3,4,5,6,8,9,10,12,13,15,16,17,18,19,21,22,23,25,26,28,29,31,32,33,34,35)],check.keys=T) # raw alpha: .92

Sub_Model<-'
Absorption=~Item_1+Item_2+Item_3+Item_5+Item_6+Item_8+Item_9+Item_10+Item_12
Vigor=~Item_13+Item_15+Item_16+Item_17+Item_18+Item_19+Item_21+Item_22+Item_23
Dedication=~Item_25+Item_26+Item_28+Item_29+Item_31+Item_32+Item_34+Item_35+Item_36
'

Fit1.1<-lavaan::cfa(Sub_Model, data = CFAdata)
semPlot::semPaths(Fit1.1, "std")
fit1.1 <- as.data.frame(fitMeasures(Fit1.1))
summary(Fit1.1, fit.measure=TRUE)


Att_Model<-'
Cognitive=~Item_1+Item_2+Item_3+Item_13+Item_15+Item_16+Item_25+Item_26+Item_28
Affective=~Item_5+Item_6+Item_8+Item_17+Item_18+Item_19+Item_29+Item_31+Item_32
Behavioral=~Item_9+Item_10+Item_12+Item_21+Item_22+Item_23+Item_34+Item_35+Item_36
'
Fit1.2<-lavaan::cfa(Att_Model, data = CFAdata)
semPlot::semPaths(Fit1.2,"std")
lavaan::fitMeasures(Fit1.2)
summary(Fit1.2, fit.measure=TRUE)

Bifactor_Model<-'
Absorption=~Item_1+Item_2+Item_3+Item_5+Item_6+Item_7+Item_9+Item_10+Item_12
Vigor=~Item_13+Item_15+Item_16+Item_17+Item_18+Item_19+Item_21+Item_22+Item_23
Dedication=~Item_25+Item_26+Item_28+Item_29+Item_31+Item_32+Item_34+Item_35+Item_36
Cognitive=~Item_1+Item_2+Item_3+Item_13+Item_15+Item_16+Item_25+Item_26+Item_28
Affective=~Item_5+Item_6+Item_7+Item_17+Item_18+Item_19+Item_29+Item_31+Item_32
Behavioral=~Item_9+Item_10+Item_12+Item_21+Item_22+Item_23+Item_34+Item_35+Item_36
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
title("Initial pilot bifactor analysis (27 candidate items)")

modindices(Fit.Bi, sort = TRUE, maximum.number = 5)

summary(Fit.Bi)



## Reduction 2 Items

## Absorption - Cognitive
psych::alpha(CFAdata[,c(1,2)],check.keys = T) #raw alpha-.025, std.alpha-.025, r.drops: .013, .013
psych::alpha(CFAdata[,c(1,3)],check.keys = T) #raw alpha-.58, std.alpha-.59, r.drops: .42, .42
psych::alpha(CFAdata[,c(1,4)],check.keys = T) #raw alpha-.049, std.alpha-.05, r.drops: .026, .026
psych::alpha(CFAdata[,c(2,3)],check.keys = T) #raw alpha-.25, std.alpha-.26, r.drops: .15, .15
psych::alpha(CFAdata[,c(2,4)],check.keys = T) #raw alpha-.94, std.alpha-.94, r.drops: .89, .89 # Best R. Drops, however items are too similiar
psych::alpha(CFAdata[,c(3,4)],check.keys = T) #raw alpha-.16, std.alpha-.17, r.drops: .092, .092

### Use items 1 and 3 for cfa models

## Absorption - Affective

psych::alpha(CFAdata[,c(5,6)],check.keys = T) #raw alpha-.53, std.alpha-.53, r.drops: .36, .36
psych::alpha(CFAdata[,c(5,7)],check.keys = T) #raw alpha-.44, std.alpha-.46, r.drops: .30, .30
psych::alpha(CFAdata[,c(5,8)],check.keys = T) #raw alpha-.68, std.alpha-.69, r.drops: .52, .52 # Best R Drops
psych::alpha(CFAdata[,c(6,7)],check.keys = T) #raw alpha-.27, std.alpha-.27, r.drops: .16, .16
psych::alpha(CFAdata[,c(6,8)],check.keys = T) #raw alpha-.45, std.alpha-.45, r.drops: .29, .29
psych::alpha(CFAdata[,c(7,8)],check.keys = T) #raw alpha-.39, std.alpha-.40, r.drops: .25, .25

## Absorption - Behavioral

psych::alpha(CFAdata[,c(9,10)],check.keys = T) #raw alpha-.56, std.alpha-.57, r.drops: .40, .40 # Best R Drops
psych::alpha(CFAdata[,c(9,11)],check.keys = T) #raw alpha-.04, std.alpha-.04, r.drops: .02, .02
psych::alpha(CFAdata[,c(9,12)],check.keys = T) #raw alpha-.45, std.alpha-.45, r.drops: .29, .29
psych::alpha(CFAdata[,c(10,11)],check.keys = T) #raw alpha-.22, std.alpha-.23, r.drops: .13, .13
psych::alpha(CFAdata[,c(10,12)],check.keys = T) #raw alpha-.39, std.alpha-.39, r.drops: .24, .24
psych::alpha(CFAdata[,c(11,12)],check.keys = T) #raw alpha-.51, std.alpha-.51, r.drops: .34, .34

## Vigor - Cognitive

psych::alpha(CFAdata[,c(13,14)],check.keys = T) #raw alpha-.24, std.alpha-.24, r.drops: .13, .13
psych::alpha(CFAdata[,c(13,15)],check.keys = T) #raw alpha-.44, std.alpha-.44, r.drops: .28, .28
psych::alpha(CFAdata[,c(13,16)],check.keys = T) #raw alpha-.65, std.alpha-.65, r.drops: .49, .49 # Best R Drops
psych::alpha(CFAdata[,c(14,15)],check.keys = T) #raw alpha-.26, std.alpha-.26, r.drops: .15, .15
psych::alpha(CFAdata[,c(14,16)],check.keys = T) #raw alpha-.62, std.alpha-.62, r.drops: .45, .45
psych::alpha(CFAdata[,c(15,16)],check.keys = T) #raw alpha-.45, std.alpha-.45, r.drops: .29, .29

## Vigor - Affective

psych::alpha(CFAdata[,c(17,18)],check.keys = T) #raw alpha-.70, std.alpha-.72, r.drops: .56, .56 # Best R Drops
psych::alpha(CFAdata[,c(17,19)],check.keys = T) #raw alpha-.68, std.alpha-.69, r.drops: .53, .53
psych::alpha(CFAdata[,c(17,20)],check.keys = T) #raw alpha-.36, std.alpha-.37, r.drops: .23, .23
psych::alpha(CFAdata[,c(18,19)],check.keys = T) #raw alpha-.60, std.alpha-.60, r.drops: .43, .43
psych::alpha(CFAdata[,c(18,20)],check.keys = T) #raw alpha-.58, std.alpha-.58, r.drops: .41, .41
psych::alpha(CFAdata[,c(19,20)],check.keys = T) #raw alpha-.29, std.alpha-.29, r.drops: .17, .17

## Vigor - Behavioral

psych::alpha(CFAdata[,c(21,22)],check.keys = T) #raw alpha-.60, std.alpha-.60, r.drops: .43, .43
psych::alpha(CFAdata[,c(21,23)],check.keys = T) #raw alpha-.56, std.alpha-.60, r.drops: .43, .43
psych::alpha(CFAdata[,c(21,24)],check.keys = T) #raw alpha-.49, std.alpha-.49, r.drops: .33, .33
psych::alpha(CFAdata[,c(22,23)],check.keys = T) #raw alpha-.63, std.alpha-.66, r.drops: .49, .49 # Best R Drops
psych::alpha(CFAdata[,c(22,24)],check.keys = T) #raw alpha-.50, std.alpha-.50, r.drops: .33, .33
psych::alpha(CFAdata[,c(23,24)],check.keys = T) #raw alpha-.44, std.alpha-.47, r.drops: .31, .31

## Dedication - Cognitive

psych::alpha(CFAdata[,c(25,26)],check.keys = T) #raw alpha-.72, std.alpha-.72, r.drops: .56, .56
psych::alpha(CFAdata[,c(25,27)],check.keys = T) #raw alpha-.76, std.alpha-.76, r.drops: .61, .61
psych::alpha(CFAdata[,c(25,28)],check.keys = T) #raw alpha-.66, std.alpha-.67, r.drops: .51, .51
psych::alpha(CFAdata[,c(26,27)],check.keys = T) #raw alpha-.70, std.alpha-.70, r.drops: .54, .54
psych::alpha(CFAdata[,c(26,28)],check.keys = T) #raw alpha-.80, std.alpha-.80, r.drops: .67, .67 # Best R Drops
psych::alpha(CFAdata[,c(27,28)],check.keys = T) #raw alpha-.64, std.alpha-.65, r.drops: .48, .48

## Dedication - Affective
psych::alpha(CFAdata[,c(29,30)],check.keys = T) #raw alpha-.47, std.alpha-.47, r.drops: .31, .31
psych::alpha(CFAdata[,c(29,31)],check.keys = T) #raw alpha-.68, std.alpha-.69, r.drops: .52, .52
psych::alpha(CFAdata[,c(29,32)],check.keys = T) #raw alpha-.77, std.alpha-.77, r.drops: .62, .62 # Best R Drops
psych::alpha(CFAdata[,c(30,31)],check.keys = T) #raw alpha-.44, std.alpha-.46, r.drops: .30, .30
psych::alpha(CFAdata[,c(30,32)],check.keys = T) #raw alpha-.52, std.alpha-.52, r.drops: .35, .35
psych::alpha(CFAdata[,c(31,32)],check.keys = T) #raw alpha-.71, std.alpha-.72, r.drops: .57, .57

## Dedication - Behavioral

psych::alpha(CFAdata[,c(33,34)],check.keys = T) #raw alpha-.64, std.alpha-.64, r.drops: .47, .47 # Best R Drops
psych::alpha(CFAdata[,c(33,35)],check.keys = T) #raw alpha-.39, std.alpha-.43, r.drops: .27, .27
psych::alpha(CFAdata[,c(33,36)],check.keys = T) #raw alpha-.24, std.alpha-.26, r.drops: .15, .15
psych::alpha(CFAdata[,c(34,35)],check.keys = T) #raw alpha-.50, std.alpha-.52, r.drops: .35, .35
psych::alpha(CFAdata[,c(34,36)],check.keys = T) #raw alpha-.37, std.alpha-.38, r.drops: .23, .23
psych::alpha(CFAdata[,c(35,36)],check.keys = T) #raw alpha-.62, std.alpha-.62, r.drops: .45, .45

psych::alpha(CFAdata[,c(1,3,5,8,9,10,13,16,17,18,22,23,26,28,29,32,33,34)],check.keys=T) # raw alpha: .92
psych::alpha(CFAdata[,c(1,3,5,8,9,10,13,16,17,18,22,23,26,28,29,32,35,36)],check.keys=T) # raw alpha: .90

Sub_Model2<-'
Absorption=~Item_1+Item_3+Item_5+Item_8+Item_9+Item_10
Vigor=~Item_13+Item_16+Item_17+Item_18+Item_22+Item_23
Dedication=~Item_26+Item_28+Item_29+Item_32+Item_33+Item_34
'

Fit1.3<-lavaan::cfa(Sub_Model2, data = CFAdata)
semPlot::semPaths(Fit1.3, "std")
fit1.3 <- as.data.frame(fitMeasures(Fit1.3))
summary(fit1.3, fit.measure=TRUE)


Att_Model2<-'
Cognitive=~Item_1+Item_3+Item_13+Item_16+Item_26+Item_28
Affective=~Item_5+Item_8+Item_17+Item_18+Item_29+Item_32
Behavioral=~Item_9+Item_10+Item_22+Item_23+Item_33+Item_34
'
Fit1.4<-lavaan::cfa(Att_Model2, data = CFAdata)
semPlot::semPaths(Fit1.4,"std")
fit1.4 <- as.data.frame(fitMeasures(Fit1.3))
summary(fit1.4, fit.measure=TRUE)

Bifactor_Model2<-'
Absorption=~Item_1+Item_3+Item_5+Item_8+Item_9+Item_10
Vigor=~Item_13+Item_16+Item_17+Item_18+Item_22+Item_23
Dedication=~Item_26+Item_28+Item_29+Item_32+Item_33+Item_34
Cognitive=~Item_1+Item_3+Item_13+Item_16+Item_26+Item_28
Affective=~Item_5+Item_8+Item_17+Item_18+Item_29+Item_32
Behavioral=~Item_9+Item_10+Item_22+Item_23+Item_33+Item_34
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

Fit.Bi2 <- lavaan::cfa(Bifactor_Model2, data = CFAdata) #, likelihood = "wishart")

semPlot::semPaths(Fit.Bi2, bifactor = c("Cognitive", "Affective", "Behavioral"), "std", layout = "tree3", 
                  rotation = 2, curvePivot=TRUE, style="lisrel", nCharNodes = 0)  ## exoCov=FALSE deletes all covariances
title("Initial pilot bifactor analysis (18 candidate items)")

modindices(Fit.Bi2, sort = TRUE, maximum.number = 5)

summary(Fit.Bi2)



### Bifactor Model 2nd 18 items - Removing item 26, for item 25___________________________________________________________

## This one looks best as the replacement
psych::alpha(CFAdata[,c(1,3,5,8,9,10,13,16,17,18,22,23,25,28,29,32,33,34)],check.keys=T) # raw alpha: .90

Bifactor_Model3<-'
Absorption=~Item_1+Item_3+Item_5+Item_8+Item_9+Item_10
Vigor=~Item_13+Item_16+Item_17+Item_18+Item_22+Item_23
Dedication=~Item_25+Item_28+Item_29+Item_32+Item_33+Item_34
Cognitive=~Item_1+Item_3+Item_13+Item_16+Item_25+Item_28
Affective=~Item_5+Item_8+Item_17+Item_18+Item_29+Item_32
Behavioral=~Item_9+Item_10+Item_22+Item_23+Item_33+Item_34
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

Fit.Bi3 <- lavaan::cfa(Bifactor_Model3, data = CFAdata) #, likelihood = "wishart")

semPlot::semPaths(Fit.Bi3, bifactor = c("Cognitive", "Affective", "Behavioral"), "std", layout = "tree3", 
                  rotation = 2, curvePivot=TRUE, style="lisrel", nCharNodes = 0)  ## exoCov=FALSE deletes all covariances
title("Initial pilot bifactor analysis (18 candidate items)")

modindices(Fit.Bi3, sort = TRUE, maximum.number = 5)

summary(Fit.Bi3)

### Bifactor Model 3rd 18 items - Removing item 26, for item 27_________________________________________________________
psych::alpha(CFAdata[,c(1,3,5,8,9,10,13,16,17,18,22,23,27,28,29,32,33,34)],check.keys=T) # raw alpha: .90

Bifactor_Model4<-'
Absorption=~Item_1+Item_3+Item_5+Item_8+Item_9+Item_10
Vigor=~Item_13+Item_16+Item_17+Item_18+Item_22+Item_23
Dedication=~Item_27+Item_28+Item_29+Item_32+Item_33+Item_34
Cognitive=~Item_1+Item_3+Item_13+Item_16+Item_27+Item_28
Affective=~Item_5+Item_8+Item_17+Item_18+Item_29+Item_32
Behavioral=~Item_9+Item_10+Item_22+Item_23+Item_33+Item_34
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

Fit.Bi4 <- lavaan::cfa(Bifactor_Model4, data = CFAdata) #, likelihood = "wishart")

semPlot::semPaths(Fit.Bi4, bifactor = c("Cognitive", "Affective", "Behavioral"), "std", layout = "tree3", 
                  rotation = 2, curvePivot=TRUE, style="lisrel", nCharNodes = 0)  ## exoCov=FALSE deletes all covariances
title("Initial pilot bifactor analysis (18 candidate items)")

modindices(Fit.Bi4, sort = TRUE, maximum.number = 5)

summary(Fit.Bi4)

### Bifactor Model 4th 18 items - Removing item 28, for item 25_________________________________________________________
psych::alpha(CFAdata[,c(1,3,5,8,9,10,13,16,17,18,22,23,25,26,29,32,33,34)],check.keys=T) # raw alpha: .90

Bifactor_Model5<-'
Absorption=~Item_1+Item_3+Item_5+Item_8+Item_9+Item_10
Vigor=~Item_13+Item_16+Item_17+Item_18+Item_22+Item_23
Dedication=~Item_25+Item_26+Item_29+Item_32+Item_33+Item_34
Cognitive=~Item_1+Item_3+Item_13+Item_16+Item_25+Item_26
Affective=~Item_5+Item_8+Item_17+Item_18+Item_29+Item_32
Behavioral=~Item_9+Item_10+Item_22+Item_23+Item_33+Item_34
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

Fit.Bi5 <- lavaan::cfa(Bifactor_Model5, data = CFAdata) #, likelihood = "wishart")

semPlot::semPaths(Fit.Bi5, bifactor = c("Cognitive", "Affective", "Behavioral"), "std", layout = "tree3", 
                  rotation = 2, curvePivot=TRUE, style="lisrel", nCharNodes = 0)  ## exoCov=FALSE deletes all covariances
title("Initial pilot bifactor analysis (18 candidate items)")

modindices(Fit.Bi5, sort = TRUE, maximum.number = 5)

summary(Fit.Bi5)

### Bifactor Model 5th 18 items - Removing items 26 and 28, for item 25 and item 27_________________________________________________________
psych::alpha(CFAdata[,c(1,3,5,8,9,10,13,16,17,18,22,23,25,27,29,32,33,34)],check.keys=T) # raw alpha: .90

Bifactor_Model6<-'
Absorption=~Item_1+Item_3+Item_5+Item_8+Item_9+Item_10
Vigor=~Item_13+Item_16+Item_17+Item_18+Item_22+Item_23
Dedication=~Item_25+Item_27+Item_29+Item_32+Item_33+Item_34
Cognitive=~Item_1+Item_3+Item_13+Item_16+Item_25+Item_27
Affective=~Item_5+Item_8+Item_17+Item_18+Item_29+Item_32
Behavioral=~Item_9+Item_10+Item_22+Item_23+Item_33+Item_34
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

Fit.Bi6 <- lavaan::cfa(Bifactor_Model6, data = CFAdata) #, likelihood = "wishart")

semPlot::semPaths(Fit.Bi6, bifactor = c("Cognitive", "Affective", "Behavioral"), "std", layout = "tree3", 
                  rotation = 2, curvePivot=TRUE, style="lisrel", nCharNodes = 0)  ## exoCov=FALSE deletes all covariances
title("Initial pilot bifactor analysis (18 candidate items)")

modindices(Fit.Bi6, sort = TRUE, maximum.number = 5)

summary(Fit.Bi6)

### Bifactor Model 6th 18 items - Removing items 32, for item 30_________________________________________________________
psych::alpha(CFAdata[,c(1,3,5,8,9,10,13,16,17,18,22,23,26,28,29,30,33,34)],check.keys=T) # raw alpha: .89

Bifactor_Model7<-'
Absorption=~Item_1+Item_3+Item_5+Item_8+Item_9+Item_10
Vigor=~Item_13+Item_16+Item_17+Item_18+Item_22+Item_23
Dedication=~Item_26+Item_28+Item_29+Item_30+Item_33+Item_34
Cognitive=~Item_1+Item_3+Item_13+Item_16+Item_26+Item_28
Affective=~Item_5+Item_8+Item_17+Item_18+Item_29+Item_30
Behavioral=~Item_9+Item_10+Item_22+Item_23+Item_33+Item_34
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

Fit.Bi7 <- lavaan::cfa(Bifactor_Model7, data = CFAdata) #, likelihood = "wishart")

semPlot::semPaths(Fit.Bi7, bifactor = c("Cognitive", "Affective", "Behavioral"), "std", layout = "tree3", 
                  rotation = 2, curvePivot=TRUE, style="lisrel", nCharNodes = 0)  ## exoCov=FALSE deletes all covariances
title("Initial pilot bifactor analysis (18 candidate items)")

modindices(Fit.Bi7, sort = TRUE, maximum.number = 5)

summary(Fit.Bi7)

### Bifactor Model 7th 18 items - Removing items 32, for item 31_________________________________________________________
psych::alpha(CFAdata[,c(1,3,5,8,9,10,13,16,17,18,22,23,26,28,29,31,33,34)],check.keys=T) # raw alpha: .90

Bifactor_Model8<-'
Absorption=~Item_1+Item_3+Item_5+Item_8+Item_9+Item_10
Vigor=~Item_13+Item_16+Item_17+Item_18+Item_22+Item_23
Dedication=~Item_26+Item_28+Item_29+Item_31+Item_33+Item_34
Cognitive=~Item_1+Item_3+Item_13+Item_16+Item_26+Item_28
Affective=~Item_5+Item_8+Item_17+Item_18+Item_29+Item_31
Behavioral=~Item_9+Item_10+Item_22+Item_23+Item_33+Item_34
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

Fit.Bi8 <- lavaan::cfa(Bifactor_Model8, data = CFAdata) #, likelihood = "wishart")

semPlot::semPaths(Fit.Bi8, bifactor = c("Cognitive", "Affective", "Behavioral"), "std", layout = "tree3", 
                  rotation = 2, curvePivot=TRUE, style="lisrel", nCharNodes = 0)  ## exoCov=FALSE deletes all covariances
title("Initial pilot bifactor analysis (18 candidate items)")

modindices(Fit.Bi8, sort = TRUE, maximum.number = 5)

summary(Fit.Bi8)

### Bifactor Model 8th 18 items - Removing items 29, for item 30_________________________________________________________
psych::alpha(CFAdata[,c(1,3,5,8,9,10,13,16,17,18,22,23,26,28,30,32,33,34)],check.keys=T) # raw alpha: .90

Bifactor_Model9<-'
Absorption=~Item_1+Item_3+Item_5+Item_8+Item_9+Item_10
Vigor=~Item_13+Item_16+Item_17+Item_18+Item_22+Item_23
Dedication=~Item_26+Item_28+Item_30+Item_32+Item_33+Item_34
Cognitive=~Item_1+Item_3+Item_13+Item_16+Item_26+Item_28
Affective=~Item_5+Item_8+Item_17+Item_18+Item_30+Item_32
Behavioral=~Item_9+Item_10+Item_22+Item_23+Item_33+Item_34
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

Fit.Bi9 <- lavaan::cfa(Bifactor_Model9, data = CFAdata) #, likelihood = "wishart")

semPlot::semPaths(Fit.Bi9, bifactor = c("Cognitive", "Affective", "Behavioral"), "std", layout = "tree3", 
                  rotation = 2, curvePivot=TRUE, style="lisrel", nCharNodes = 0)  ## exoCov=FALSE deletes all covariances
title("Initial pilot bifactor analysis (18 candidate items)")

modindices(Fit.Bi9, sort = TRUE, maximum.number = 5)

summary(Fit.Bi9)

### Bifactor Model 9th 18 items - Removing items 29, for item 31_________________________________________________________
psych::alpha(CFAdata[,c(1,3,5,8,9,10,13,16,17,18,22,23,26,28,31,32,33,34)],check.keys=T) # raw alpha: .90

Bifactor_Model10<-'
Absorption=~Item_1+Item_3+Item_5+Item_8+Item_9+Item_10
Vigor=~Item_13+Item_16+Item_17+Item_18+Item_22+Item_23
Dedication=~Item_26+Item_28+Item_31+Item_32+Item_33+Item_34
Cognitive=~Item_1+Item_3+Item_13+Item_16+Item_26+Item_28
Affective=~Item_5+Item_8+Item_17+Item_18+Item_31+Item_32
Behavioral=~Item_9+Item_10+Item_22+Item_23+Item_33+Item_34
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

Fit.Bi10 <- lavaan::cfa(Bifactor_Model10, data = CFAdata) #, likelihood = "wishart")

semPlot::semPaths(Fit.Bi10, bifactor = c("Cognitive", "Affective", "Behavioral"), "std", layout = "tree3", 
                  rotation = 2, curvePivot=TRUE, style="lisrel", nCharNodes = 0)  ## exoCov=FALSE deletes all covariances
title("Initial pilot bifactor analysis (18 candidate items)")

modindices(Fit.Bi10, sort = TRUE, maximum.number = 5)

summary(Fit.Bi10)

### Bifactor Model 10th 18 items - Removing items 29 and 32, for item 30 and 31_________________________________________________________
psych::alpha(CFAdata[,c(1,3,5,8,9,10,13,16,17,18,22,23,26,28,30,31,33,34)],check.keys=T) # raw alpha: .89

Bifactor_Model11<-'
Absorption=~Item_1+Item_3+Item_5+Item_8+Item_9+Item_10
Vigor=~Item_13+Item_16+Item_17+Item_18+Item_22+Item_23
Dedication=~Item_26+Item_28+Item_30+Item_31+Item_33+Item_34
Cognitive=~Item_1+Item_3+Item_13+Item_16+Item_26+Item_28
Affective=~Item_5+Item_8+Item_17+Item_18+Item_30+Item_31
Behavioral=~Item_9+Item_10+Item_22+Item_23+Item_33+Item_34
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

Fit.Bi11 <- lavaan::cfa(Bifactor_Model11, data = CFAdata) #, likelihood = "wishart")

semPlot::semPaths(Fit.Bi11, bifactor = c("Cognitive", "Affective", "Behavioral"), "std", layout = "tree3", 
                  rotation = 2, curvePivot=TRUE, style="lisrel", nCharNodes = 0)  ## exoCov=FALSE deletes all covariances
title("Initial pilot bifactor analysis (18 candidate items)")

modindices(Fit.Bi11, sort = TRUE, maximum.number = 5)

summary(Fit.Bi11)

### Bifactor Model 11th 18 items - Removing items 32 and 26, for item 31 and 25______________________________________
psych::alpha(CFAdata[,c(1,3,5,8,9,10,13,16,17,18,22,23,25,28,29,31,33,34)],check.keys=T) # raw alpha: .92

Bifactor_Model12<-'
Absorption=~Item_1+Item_3+Item_5+Item_8+Item_9+Item_10
Vigor=~Item_13+Item_16+Item_17+Item_18+Item_22+Item_23
Dedication=~Item_25+Item_28+Item_29+Item_31+Item_33+Item_34
Cognitive=~Item_1+Item_3+Item_13+Item_16+Item_25+Item_28
Affective=~Item_5+Item_8+Item_17+Item_18+Item_29+Item_31
Behavioral=~Item_9+Item_10+Item_22+Item_23+Item_33+Item_34
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

Fit.Bi12 <- lavaan::cfa(Bifactor_Model12, data = CFAdata) #, likelihood = "wishart")

semPlot::semPaths(Fit.Bi12, bifactor = c("Cognitive", "Affective", "Behavioral"), "std", layout = "tree3", 
                  rotation = 2, curvePivot=TRUE, style="lisrel", nCharNodes = 0)  ## exoCov=FALSE deletes all covariances
title("Initial pilot bifactor analysis (18 candidate items)")

modindices(Fit.Bi12, sort = TRUE, maximum.number = 5)

summary(Fit.Bi12)

### Bifactor Model 12th 18 items - Removing items 32 and 26, for item 30 and 25______________________________________
psych::alpha(CFAdata[,c(1,3,5,8,9,10,13,16,17,18,22,23,25,28,29,30,33,34)],check.keys=T) # raw alpha: .89

###########################best model!!!!!!!
Bifactor_Model13<-'
Absorption=~Item_1+Item_3+Item_5+Item_8+Item_9+Item_10
Vigor=~Item_13+Item_16+Item_17+Item_18+Item_22+Item_23
Dedication=~Item_25+Item_28+Item_29+Item_30+Item_33+Item_34
Cognitive=~Item_1+Item_3+Item_13+Item_16+Item_25+Item_28
Affective=~Item_5+Item_8+Item_17+Item_18+Item_29+Item_30
Behavioral=~Item_9+Item_10+Item_22+Item_23+Item_33+Item_34
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

Fit.Bi13 <- lavaan::cfa(Bifactor_Model13, data = CFAdata, missing = "ML") #, likelihood = "wishart")

semPlot::semPaths(Fit.Bi13, bifactor = c("Cognitive", "Affective", "Behavioral"), "std", layout = "tree3", 
                  rotation = 2, curvePivot=TRUE, style="lisrel", nCharNodes = 0)  ## exoCov=FALSE deletes all covariances
title("Initial pilot bifactor analysis (18 candidate items)")

modindices(Fit.Bi13, sort = TRUE, maximum.number = 5)

summary(Fit.Bi13)

### Bifactor Model 13th 18 items - Removing items 29 and 26, for item 30 and 25______________________________________
psych::alpha(CFAdata[,c(1,3,5,8,9,10,13,16,17,18,22,23,25,28,30,32,33,34)],check.keys=T) # raw alpha: .89

Bifactor_Model14<-'
Absorption=~Item_1+Item_3+Item_5+Item_8+Item_9+Item_10
Vigor=~Item_13+Item_16+Item_17+Item_18+Item_22+Item_23
Dedication=~Item_25+Item_28+Item_30+Item_32+Item_33+Item_34
Cognitive=~Item_1+Item_3+Item_13+Item_16+Item_25+Item_28
Affective=~Item_5+Item_8+Item_17+Item_18+Item_30+Item_32
Behavioral=~Item_9+Item_10+Item_22+Item_23+Item_33+Item_34
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

Fit.Bi14 <- lavaan::cfa(Bifactor_Model14, data = CFAdata) #, likelihood = "wishart")

semPlot::semPaths(Fit.Bi14, bifactor = c("Cognitive", "Affective", "Behavioral"), "std", layout = "tree3", 
                  rotation = 2, curvePivot=TRUE, style="lisrel", nCharNodes = 0)  ## exoCov=FALSE deletes all covariances
title("Initial pilot bifactor analysis (18 candidate items)")

modindices(Fit.Bi14, sort = TRUE, maximum.number = 5)

summary(Fit.Bi14)

### Bifactor Model 14th 18 items - Removing items 29 & 32, and 26, for item 31 & 30 and 25______________________________________
psych::alpha(CFAdata[,c(1,3,5,8,9,10,13,16,17,18,22,23,25,28,31,32,33,34)],check.keys=T) # raw alpha: .90

Bifactor_Model15<-'
Absorption=~Item_1+Item_3+Item_5+Item_8+Item_9+Item_10
Vigor=~Item_13+Item_16+Item_17+Item_18+Item_22+Item_23
Dedication=~Item_25+Item_28+Item_31+Item_32+Item_33+Item_34
Cognitive=~Item_1+Item_3+Item_13+Item_16+Item_25+Item_28
Affective=~Item_5+Item_8+Item_17+Item_18+Item_31+Item_32
Behavioral=~Item_9+Item_10+Item_22+Item_23+Item_33+Item_34
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

Fit.Bi15 <- lavaan::cfa(Bifactor_Model15, data = CFAdata) #, likelihood = "wishart")

semPlot::semPaths(Fit.Bi15, bifactor = c("Cognitive", "Affective", "Behavioral"), "std", layout = "tree3", 
                  rotation = 2, curvePivot=TRUE, style="lisrel", nCharNodes = 0)  ## exoCov=FALSE deletes all covariances
title("Initial pilot bifactor analysis (18 candidate items)")

modindices(Fit.Bi15, sort = TRUE, maximum.number = 5)

summary(Fit.Bi15)

### Bifactor Model 15th 18 items - Removing items 29 and 26, for item 31 and 25______________________________________
psych::alpha(CFAdata[,c(1,3,5,8,9,10,13,16,17,18,22,23,25,28,30,31,33,34)],check.keys=T) # raw alpha: .89

Bifactor_Model16<-'
Absorption=~Item_1+Item_3+Item_5+Item_8+Item_9+Item_10
Vigor=~Item_13+Item_16+Item_17+Item_18+Item_22+Item_23
Dedication=~Item_25+Item_28+Item_30+Item_31+Item_33+Item_34
Cognitive=~Item_1+Item_3+Item_13+Item_16+Item_25+Item_28
Affective=~Item_5+Item_8+Item_17+Item_18+Item_30+Item_31
Behavioral=~Item_9+Item_10+Item_22+Item_23+Item_33+Item_34
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

Fit.Bi16 <- lavaan::cfa(Bifactor_Model16, data = CFAdata) #, likelihood = "wishart")

semPlot::semPaths(Fit.Bi16, bifactor = c("Cognitive", "Affective", "Behavioral"), "std", layout = "tree3", 
                  rotation = 2, curvePivot=TRUE, style="lisrel", nCharNodes = 0)  ## exoCov=FALSE deletes all covariances
title("Initial pilot bifactor analysis (18 candidate items)")

modindices(Fit.Bi16, sort = TRUE, maximum.number = 5)

summary(Fit.Bi16)

Bifactor_Model16<-'
Absorption=~Item_+Item_3+Item_5+Item_8+Item_9+Item_10
Vigor=~Item_13+Item_16+Item_17+Item_18+Item_21+Item_2
Dedication=~Item_25+Item_28+Item_30+Item_31+Item_33+Item_34
Cognitive=~Item_1+Item_3+Item_13+Item_16+Item_25+Item_28
Affective=~Item_5+Item_8+Item_17+Item_18+Item_30+Item_31
Behavioral=~Item_9+Item_10+Item_22+Item_23+Item_33+Item_34
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

Fit.Bi16 <- lavaan::cfa(Bifactor_Model16, data = CFAdata) #, likelihood = "wishart")

semPlot::semPaths(Fit.Bi16, bifactor = c("Cognitive", "Affective", "Behavioral"), "std", layout = "tree3", 
                  rotation = 2, curvePivot=TRUE, style="lisrel", nCharNodes = 0)

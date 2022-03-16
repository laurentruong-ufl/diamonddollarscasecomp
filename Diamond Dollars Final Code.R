#create dataset 2021
year2021 <- FanGraphs.Leaderboard.3
#create action index
year2021$actionindex <- (year2021$PA - year2021$HBP - year2021$BB - year2021$SO)/81
year2021$Kperc <- year2021$SO/year2021$PA
year2021$BBperc <- year2021$BB/year2021$PA
year2021$HBPperc <- year2021$HBP/year2021$PA
#get three important percentages
lm2021 <- lm(actionindex ~ BBperc + Kperc + HBPperc, data = year2021)
summary(lm2021)
#run regression, strikeouts significant. others not. because they add a plate appearance
cor(year2021$Kperc, year2021$actionindex)
mean(year2021$actionindex)
#walks add an at-bat, and so do HBP so 1-1 for one trade in action index. strikeouts do not.

#create dataset 2011
year2011 <- FanGraphs.Leaderboard.4
#create action index
year2011$actionindex <- (year2011$PA - year2011$HBP - year2011$BB - year2011$SO)/81
year2011$Kperc <- year2011$SO/year2011$PA
year2011$BBperc <- year2011$BB/year2011$PA
year2011$HBPperc <- year2011$HBP/year2011$PA
#get three important percentages
lm2011 <- lm(actionindex ~ BBperc + Kperc + HBPperc, data = year2011)
summary(lm2011)
#run regression, strikeouts significant. others not. because they add a plate appearance
cor(year2011$Kperc, year2011$actionindex)
mean(year2011$actionindex)
#walks add an at-bat, and so do HBP so 1-1 for one trade in action index. strikeouts do not.

#create dataset 2001
year2001 <- FanGraphs.Leaderboard.5
#create action index
year2001$actionindex <- (year2001$PA - year2001$HBP - year2001$BB - year2001$SO)/81
year2001$Kperc <- year2001$SO/year2001$PA
year2001$BBperc <- year2001$BB/year2001$PA
year2001$HBPperc <- year2001$HBP/year2001$PA
#get three important percentages
lm2001 <- lm(actionindex ~ BBperc + Kperc + HBPperc, data = year2001)
summary(lm2001)
#run regression, strikeouts significant. others not. because they add a plate appearance
cor(year2001$Kperc, year2001$actionindex)
mean(year2001$actionindex)
#walks add an at-bat, and so do HBP so 1-1 for one trade in action index. strikeouts do not.

#create dataset 2001
year1991 <- FanGraphs.Leaderboard.6
#create action index
year1991$actionindex <- (year1991$PA - year1991$HBP - year1991$BB - year1991$SO)/81
year1991$Kperc <- year1991$SO/year1991$PA
year1991$BBperc <- year1991$BB/year1991$PA
year1991$HBPperc <- year1991$HBP/year1991$PA
#get three important percentages
lm1991 <- lm(actionindex ~ BBperc + Kperc + HBPperc, data = year1991)
summary(lm1991)
#run regression, strikeouts significant. others not. because they add a plate appearance
cor(year1991$Kperc, year1991$actionindex)
mean(year1991$actionindex)
#walks add an at-bat, and so do HBP so 1-1 for one trade in action index. strikeouts do not.
#create anova testing dataframes for comparison
dfanova = data.frame(matrix(nrow = 116, ncol = 5))
for(i in 1:30){
  dfanova$X1[i] = "2021"
  dfanova$X2[i] = year2021$actionindex[i]
  dfanova$X3[i] = year2021$BBperc[i]
  dfanova$X4[i] = year2021$Kperc[i]
  dfanova$X5[i] = year2021$HBPperc[i]
}
for(i in 31:60){
  dfanova$X1[i] = "2011"
  dfanova$X2[i] = year2011$actionindex[i-30]
  dfanova$X3[i] = year2011$BBperc[i-30]
  dfanova$X4[i] = year2011$Kperc[i-30]
  dfanova$X5[i] = year2011$HBPperc[i-30]
}
for(i in 61:90){
  dfanova$X1[i] = "2001"
  dfanova$X2[i] = year2001$actionindex[i-60]
  dfanova$X3[i] = year2001$BBperc[i-60]
  dfanova$X4[i] = year2001$Kperc[i-60]
  dfanova$X5[i] = year2001$HBPperc[i-60]
}
for(i in 91:116){
  dfanova$X1[i] = "1991"
  dfanova$X2[i] = year1991$actionindex[i-90]
  dfanova$X3[i] = year1991$BBperc[i-90]
  dfanova$X4[i] = year1991$Kperc[i-90]
  dfanova$X5[i] = year1991$HBPperc[i-90]
}
#action index anova
anova4yearsAI <- aov(X2 ~ X1, data = dfanova)
summary(anova4yearsAI)
TukeyHSD(anova4yearsAI, conf.level = .95)
#walk anova
anova4yearsBB <- aov(X3 ~ X1, data = dfanova)
summary(anova4yearsBB)
TukeyHSD(anova4yearsBB, conf.level = .95)
#so anova
anova4yearsSO <- aov(X4 ~ X1, data = dfanova)
summary(anova4yearsSO)
TukeyHSD(anova4yearsSO, conf.level = .95)
#hbp anova
anova4yearsHBP <- aov(X5 ~ X1, data = dfanova)
summary(anova4yearsHBP)
TukeyHSD(anova4yearsHBP, conf.level = .95)

#between 2015 and 2021 there was a shift in the size of the laces by about 9%, this led to a considerable increase in spin rate across the league
#which means that the spin rate went up. between those years there is a significant difference
#in strikeouts but there is not a significant decrease or increase in walk percentage. this means
#that if we decrease the lace size by the same or higher percentage could revert back to similar strikeout percentages

swstr <- FanGraphs.Leaderboard.7
for(i in 1:30){
  swstr$SwStr.[i] = gsub('.{1}$', '', swstr$SwStr.[i])
  
}
swstr$SwStr. = as.numeric(swstr$SwStr.)
for(i in 1:30){
  swstr$K.[i] = gsub('.{1}$', '', swstr$K.[i])
  
}
swstr$K. = as.numeric(swstr$K.)
lmswstr <- lm(K. ~ SwStr., data = swstr)
#shows that spin rate increases swing and miss percentage which increases strikeout percentage

#so we should decrease the lace size by 9% or higher

cor(year1991$BBperc, year1991$actionindex)
cor(year1991$HBPperc, year1991$actionindex)

cor(year2001$BBperc, year2001$actionindex)
cor(year2001$HBPperc, year2001$actionindex)

cor(year2011$BBperc, year2011$actionindex)
cor(year2011$HBPperc, year2011$actionindex)

cor(year2021$BBperc, year2021$actionindex)
cor(year2021$HBPperc, year2021$actionindex)


TukeyHSD(anova4yearsSO, conf.level = .95)
mean(year2021$Kperc)
perc1 <- (.2318997-.04558353)/(.2318997)
mean(year2001$Kperc)
perc2 <- (.1733801 - .0216)/.1733801
lmtester <- lm(X2 ~ X4, data = dfanova)
summary(lmtester)
check <- mean(year2021$Kperc)*perc1*perc2
checkerdataframe = data.frame(matrix(nrow = 1, ncol = 5))
checkerdataframe$X4[1] = check
pred <- predict(lmtester, checkerdataframe)


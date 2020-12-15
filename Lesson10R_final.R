
library('ggplot2')
library("tidyr")
library('rcompanion')
library("IDPmisc")
library('readxl')
install.packages("gmodels")
library("gmodels")
library("dplyr")
library("car")

#Scenario 1, One proportion z test 
prop.test(x = 28, n = 94, alternative = "less")
#p< .5 which shows the result is significant and not equal to each other. 
#Sample estimates show that 29% of claimants are not disabled, it is way higher that 16% industry reports.



## Scenario 2 Starter, Independent Chi-Square
df2 <- read.csv("antiseptics.csv")

df2.expanded <- df2[rep(row.names(df2), df2$Number.of.applications), 1:2]


CrossTable(df2.expanded$Clinic, df2.expanded$Antiseptic.Type, fisher = TRUE, chisq = TRUE, expected = TRUE, sresid = TRUE, format = "SPSS")
# Expected value is more than 5 for each cell
# p value is greater than .05 so result is not significant.
# Conclusion: Antiseptic type usage does not differ among clinics. Or difference is not significant.



## Scenario 3 , One way anova

df3 <- read.csv("savings.csv")
df3.reformat <- gather(df3, key="Group", value="Price")
str(df3.reformat$Price)


plotNormalHistogram(df3.reformat$Price)
# it looks normal

bartlett.test(Price ~ Group, data = df3.reformat)
#The p value associated with this test is < .05, which means that unfortunately, we have violated the assumption of homogeneity of variance.
#we have more than 20 samples per IV. 

ANOVA <- lm(Price ~ Group, data = df3.reformat)
Anova(ANOVA, Type = "II", white.adjust = TRUE)
#there is significant difference of prices among  the groups.


pairwise.t.test(df3.reformat$Price, df3.reformat$Group, p.adjust = 'none') 
#price for each group significantly different from each other. 
#Conclusion: Saving habits and amounts for each group is different for each group.So it is correct to splitting into different groups as proposed. 


# Scenarion 4, two proportions testing

prop.test(x = c(74, 171), n = c(129, 374), alternative = "two.sided")
# p is .02 and significant, so the proportion of favoring and not favoring with and without school children differs.





setwd("C:/Users/santo/Documents/documents")
library(readxl)
library(car)
df <- read_xlsx("for analysis.xlsx", sheet = "Soil")
#alternatively
df <- read_xlsx(file.choose(), sheet = "Soil")
getwd()

library(lme4)
library(dplyr)
# fit the model with REML, and get F-test:
df1<-lmer(C~Tillage* Depth*Rate + (1|Year/Rep),data=df, REML=T)
summary(df1)


     
df1A <- Anova(df1, test="F") # F-test
joint_tests(df1)
#random effect contribution

summary(df1)$varcor
ranef(df1)$Year
tapply(df$C, df$Year, mean) 

###############3
emmip(df1, Depth~Rate | Tillage)

#mean separation pairwise
library(multcomp)
library(emmeans)

TU<- summary(glht (df1,linfct=mcp(Tillage = "Tukey")))
TU # print all pairwise comparisons

par(mar=c(5,10,3,1), mgp=c(2,.7,0)) # set wide left margin 
plot(TU)

df2 <- emmeans(df1, ~ Tillage* Depth)
contrast(df2, "consec", simple = "each", combine = TRUE, adjust = "mvt")


emm_s.t <- emmeans(df1, pairwise ~ Tillage | Depth)
emm_s.t                   



#interaction with covariates
df5 <- lm(N ~ Tillage*Rate, data= df)

Anova(df5)

df6 <- aov(N ~Tillage+Rate, data=df)
summary(df6)
TU<-TukeyHSD(df6)
TU
tukey.plot.aov<-aov(N ~ Rate, data=df)
tukey.plot.aov
tukey.plot.test<-TukeyHSD(tukey.plot.aov)
plot(tukey.plot.test, las = 1)


anova(df6,df5)
emtrends (df5,pairwise ~ Tillage, var = "Rate")
emmip(df5, Tillage ~ Rate, cov.reduce = range)

#save output using sink
sink(file = "ltr.txt")
(df1A)
summary(df6)
Anova(df5)
(df2)
sink()



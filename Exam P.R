xp <- read.csv(text="
Student,ExamP,GPA
2,No Pass,3.42
3,No Pass,3.93
4,No Pass,3.41
5,No Pass,3.75
6,Passed,3.18
7,No Pass,2.76
8,No Pass,3.68
9,No Pass,3.11
10,Passed,3.77
11,No Pass,3.06
12,No Pass,3.89
13,No Pass,3.28
14,Passed,3.82
15,No Pass,2.8
16,No Pass,3.45
17,No Pass,3.75
18,No Pass,3.7
19,No Pass,2.61
20,No Pass,3.17
21,Passed,3.93
22,No Pass,2.94
23,No Pass,2.49
24,No Pass,3.77
25,No Pass,2.7
26,Passed,3.4
27,No Pass,3.87
28,No Pass,2.55
29,Passed,3.81
30,No Pass,1.92
31,No Pass,2.79
32,No Pass,3.42
33,Passed,3.91
34,No Pass,3.94
35,Passed,3.6
36,Passed,3.89
37,No Pass,3.66
38,No Pass,3.02
39,Passed,3.87
40,No Pass,3.92
41,No Pass,3.27
42,Passed,3.32
43,Passed,3.52
44,No Pass,3.75
45,No Pass,3.87
46,No Pass,2.67
")

tail(xp)
below.3.5 <- subset(xp,xp$GPA<3.5)
above.3.5 <- subset(xp,xp$GPA>=3.5)

tt <-c(mean(below.3.5$ExamP=="Passed"),
mean(above.3.5$ExamP=="Passed"))
names(tt) <- c("Below 3.5 GPA passing","Above 3.5 GPA")
tt
boxplot(GPA~ExamP,data=xp,main="Comparing GPAs",xlab="Exam Outcome",ylab="GPA")

#Conditional Probability
# Exaplanatory variable: GPA
#Response: Exam P (Pass or fail) Two categories(binary). 
# Did student pass examp? yes or no. Y=0 or 1

#response variable
xp$Pass <- ifelse(xp$ExamP=="Passed",1,0)

#in our model gpa will be on the x axis while probability of passing
#will be on the y axis. There are constraints (prob can't be above 1 or below 0)
# curve will be s-shaped/sigmoidal (kind of like a cdf.)
# We don't have dots that'll follow the line here, they are either 1 or 0.

###Logistic Regression####
# model: log(p(pass|gpa)/p(nopass|gpa))= Bo + B1GPA (no "E" or "normality") 
#because we're modeling conditional prob not data
# Note: Instead of modeling PROBABILITY, we model log(ODDS)
log(1)
#odds is prob/complement
#Probability    Odds    log(Odds)
# .5/(.5)         1       0
# .95(/.05)       19      log(19)=2.944
# .2              .25     log(.25) = -1.39

# The model for logodds can go negative and positive with the middle being 0
# Y ~ Bernoulli. Not modelling Y, we model the probability.
# Note: you can show p(pass|gpa) = 1/(1+exp(-(Bo+B1GPA)) p(nopass|gpa)= exp(-(Bo+B1GPA))/(1+exp(-(Bo+B1GPA)))

#Model
#log(P(pass|gpa)/P(notpass|gpa))= beta0 + beta1 GPA  . . .  this is "logistic pass"

out.xp <- glm(Pass~GPA,data=xp, family="binomial") #genralized. in sas it's called proc genmod
summary(out.xp) #d
# For a one unit increase in GPA we estimate the log odds of passing increase by 2.246. Or just say "it's positive"
# For a one unit increase in GPA (for example, moving from 2.5 to 3.5) we estimate the odds of passing
# increase 9.54 times. (exp(2.256)) times because it used to be log but now we multiply

# Does GPA have a statistically significant effect on passing?

#Ho: B1=0. z-test(summary) or LRT Chi^2(anova of reduced+ish) or 95% CI on B1
# z-test. We reject Ho:B1=0 in favor of Ha: B!=0 at å=0.05/OR GPA has a statistically significant effect on odds 
#of passing Exam P.(pvalue=0.0329)

#LRT or X^2 test of Ho:beta1=0
reduced.examp <-  glm(Pass~ +1,data=xp, family="binomial") #+1 is y intercept
anova(reduced.examp,out.xp,test="Chisq") # the only thing that changed in our conclusion was the p-value.
# The p-value was smaller so we should use that one. 
#95% CI on beta1 (logodds)
confint(out.xp)
#95% Confidence interval on odds exp(beta1)
exp(confint(out.xp))[-1,] # this interval needs to be compared to 1 for statistical significance. (odds of 1)

#Predict probability of passing for students with GPA= 3.25 & 3.85
predict(out.xp, newdata = data.frame(GPA=c(3.25,3.85)),type="response") # we want probability so use response here

#graphic to show relationship between GPA and passing
# data
plot(Pass~GPA,data=xp,xlim=c(0,4))
# predicted probability
xstar <- seq(0,4,length.out = 1001)
phat <- predict(out.xp,newdata=data.frame(GPA=xstar),type="response")
lines(xstar,phat,col="tomato3")
# confidence bounds 95% on probabilities
logit.hat <- predict(out.xp,newdata=data.frame(GPA=xstar),type="link",se.fit=TRUE)
loogit.l <- logit.hat$fit-1.96*logit.hat$se.fit
loogit.u <- logit.hat$fit+1.96*logit.hat$se.fit
phat.l <- 1/(1+exp(-loogit.l))
phat.u <- 1/(1+exp(-loogit.u))
lines(xstar,phat.l,lty=2,col="steelblue4")
lines(xstar,phat.u,lty=2,col="steelblue4")
legend("topleft",legend=c("Data","Estimate","95% CI"),col=c("black","Tomato4","steelblue4"),pch=c(1,NA,NA),lty=c(NA,1,2))

#don't worry about the last two parts.

# Research and analysis, etc.
  # The data matches the research task and analysis perfectly. We were able to estimate the effect
# of GPA on the probability of passing exam P.

#weakness: Due to the amount of data provided, the interval for our prediction is very wide, and does not
# include any information about potential students in the lower end of GPA. Thus the prediction in that area
# is most likely flawed. 

#Challenge:
#Using data  on the Titanic from http://biostat.mc.vanderbilt.edu/wiki/pub/Main/DataSets/titanic3.xls
# s investigate the effect of age on whether or not passengers survived the accident.

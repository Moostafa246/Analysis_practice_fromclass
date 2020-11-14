data = read.table("http://www.utstat.utoronto.ca/~brunner/data/legal/LittleStatclassdata.txt")
summary(data)
attach(data)
compavg <- data$CompAve
finalexam <- data$FinalExam
cor(compavg,finalexam)
midavg <- data$MidTerm
quizavg <- data$QuizAve
#b)
colnames(data) = c("QuizAve", "CompAve", "MidTerm", "FinalExam")
mdl <- lm(FinalExam ~ QuizAve+CompAve+MidTerm)
summary(mdl)
#l)
l <- ((14.54)**2)*54
l
#m)
m <- (11416.23)/(1-0.2662)
m

#n) the answer would be yhat = 46.387
#o) Betahat3 = 0.325
#p) 42.218
#r) R^2 = 0.2662

source("http://www.utstat.utoronto.ca/~brunner/Rfunctions/ftest.txt")
new = cbind(0,1,1,0)
ftest(mdl, new)
source("http://www.utstat.utoronto.ca/~brunner/Rfunctions/ftest.txt")
new = cbind(0,1,0,0)
ftest(mdl, new)
confint(mdl)

meanofcomp <- mean(compavg)
meanofcomp
meanofmidavg <- mean(midavg)
meanofmidavg
meanofquiz <- mean(quizavg)
meanofquiz
#newmdl <- lm(FinalExam ~ meanofquiz+meanofcomp+meanofmidavg)
#summary(newmdl)

anova(mdl)

t.test(FinalExam)
#v) people with higher marks on the midterm tend to do better on the exam
#ii) we cant predict anything
#iii) students with higher marks on the quiz tend to do better on the final exam
#x)
new1 = rbind(c(0,1,0,0),c(0,0,1,0))
ftest(mdl, new1)
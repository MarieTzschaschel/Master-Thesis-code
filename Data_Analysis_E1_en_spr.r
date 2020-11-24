```{r include=FALSE, results='hide', echo=FALSE}
library(lme4)

# Hochladen Daten: Tools - General options - Brows
# oder: setwd("C:\\Users\\marie")

# Load the data file

data <- read.table('Final_DataFrame_e1.csv', sep=',')
head(data)

# rename the columns of the data frame
colnames(data) <- c("Ignore","Subject_ID","Sentence_Number","condition","Grammatical_Condition","LogRT","Centered_simVal")
head(data)

# check how the data looks like
xtabs(~Subject_ID+Grammatical_Condition,data)
xtabs(~Subject_ID+Centered_simVal,data)

# data description:

#49 participants
#8 sentences for each (16 sentences summed up). 
#Except for participant 8: 16 sentences each. 32 summed up.

# remove participant 8 (duplicated data)
data <- subset(data,Subject_ID!=8)
xtabs(~Subject_ID+Grammatical_Condition,data)
# everything is fine and subj 8 is removed.

#power.t.test()?

m0<-lmer(LogRT~Grammatical_Condition*Centered_simVal+(1|Subject_ID),data)
summary(m0)


hist(residuals(m0)) # normally distributed
acf(residuals(m0))

# calculate SE and 95% confidence interval

# 95-% confidence interval = mean +/- 2*SE

confint(m0)

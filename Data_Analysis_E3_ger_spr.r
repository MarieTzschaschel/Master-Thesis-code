```{r include=FALSE, results='hide', echo=FALSE}
library(lme4)

# Load the data file

data <- read.table('Final_DataFrame_e3.csv', sep=',')
head(data)

# rename the columns of the data frame
colnames(data) <- c("Ignore","Subject_ID","Sentence_Number","condition","Grammatical_Condition","LogRT","Centered_simVal")
head(data)

# check how the data looks like
xtabs(~Subject_ID+Grammatical_Condition,data)
xtabs(~Subject_ID+Centered_simVal,data)

# data description:

#missing values

# removed participant 115 - missing data point
data <- subset(data,Subject_ID!=115)
xtabs(~Subject_ID+Grammatical_Condition,data)

# 42 participants instead of 39??? strange ID (starts with 5 and ends up with 114??)


m0<-lmer(LogRT~Grammatical_Condition*Centered_simVal+(1|Subject_ID),data)
summary(m0)


hist(residuals(m0)) # normally distributed
acf(residuals(m0))


# 95-% confidence interval = mean +/- 2*SE

confint(m0)

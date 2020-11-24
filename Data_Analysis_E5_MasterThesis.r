```{r include=FALSE, results='hide', echo=FALSE}
library(lme4)

# Load the data file (take other file)

data <- read.table("Final_DataFrame_e5.csv", sep=',')
head(data)

# rename the columns of the data frame
colnames(data) <- c("Ignore","Subject_ID","Sentence_Number","condition","Contrast_Condition","LogRT","Centered_simVal")
head(data)

# check how the data looks like
xtabs(~Subject_ID+Contrast_Condition,data)

# data description:

#50 participants, no missing data

mean(data$Centered_simVal)
min(data$Centered_simVal)
max(data$Centered_simVal)

m0<-lmer(LogRT~Contrast_Condition*Centered_simVal+(1|Subject_ID),data)
summary(m0)

hist(residuals(m0)) # normally distributed
acf(residuals(m0))


# in general: SE and 90% confidence interval
confint(m0)

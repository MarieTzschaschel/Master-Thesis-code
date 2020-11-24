```{r include=FALSE, results='hide', echo=FALSE}
library(lme4)

# Load the data file

data <- read.table("Final_DataFrame_e1a.csv", sep=',')
head(data)

# rename the columns of the data frame
colnames(data) <- c("Ignore","Subject_ID","Sentence_Number","condition","Contrast_Condition","LogRT","Centered_simVal")
head(data)

# check how the data looks like
xtabs(~Subject_ID+Contrast_Condition,data)

# data description:

#46 participants (participant 1 and 44 are missing)
#8 sentences for each (16 sentences summed up). 
#Except for participant 36: 16 sentences each.

# remove participant 36
data <- subset(data,Subject_ID!=36)
xtabs(~Subject_ID+Contrast_Condition,data)
# everything is fine and subj 36 is removed.

mean(data$Centered_simVal)
min(data$Centered_simVal)
max(data$Centered_simVal)

m0<-lmer(LogRT~Contrast_Condition*Centered_simVal+(1|Subject_ID),data)
summary(m0)

hist(residuals(m0)) # normally distributed
acf(residuals(m0))
confint(m0)

lower_bound <- 0.010457 - 2*0.053977

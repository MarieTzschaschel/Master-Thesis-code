```{r include=FALSE, results='hide', echo=FALSE}
library(lme4)

# Load the data file (take other file)

data <- read.table("Final_DataFrame_e6.csv", sep=',')
head(data)

# rename the columns of the data frame
colnames(data) <- c("Ignore","subject","item","level","grammaticality","Comma_condition","LogRT","centered_similarity")
head(data)

xtabs(~subject+grammaticality,data)
data <- subset(data,subject!=20)
data <- subset(data,subject!=21)
data <- subset(data,subject!=22)

data$grammaticality<-as.factor(data$grammaticality)
levels(data$grammaticality)

#varying intercept model
m0 <- lmer(LogRT~1+(1|subject)+ (1|item),data)
summary(m0)

#varying intercept and slopes model (withoug correlation) for the factor of grammaticality
m1 <- lmer(LogRT~1+grammaticality + (1|subject)+(1|item), data)
summary(m1)

m2 <- lmer(LogRT~1+centered_similarity+(1|subject)+(1|item), data)
m2

#maximal varying intercepts and varying slopes model with correlation

m3 <- lmer(LogRT~1+grammaticality+centered_similarity+(1|subject)+(1|item), data)
summary(m3)

m4 <- lmer(LogRT~1+grammaticality*centered_similarity+(1|subject)+(1|item), data)
summary(m4)

anova(m0, m2)

anova(m0, m1, m3, m4) #m1 is the best

hist(residuals(m0)) # normally distributed
acf(residuals(m0))

hist(residuals(m1)) # normally distributed
acf(residuals(m1))

hist(residuals(m2)) # normally distributed
acf(residuals(m2))

hist(residuals(m3)) # normally distributed
acf(residuals(m3))

# 95-% confidence interval = mean +/- 2*SE

confint(m0)

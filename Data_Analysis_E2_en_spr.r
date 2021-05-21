```{r include=FALSE, results='hide', echo=FALSE}
library(lme4)

# Hochladen Daten: Tools - General options - Brows
# oder: setwd("C:\\Users\\marie")

# Load the data file

data <- read.table('Final_DataFrame_e2.csv', sep=',')
head(data)

# rename the columns of the data frame
colnames(data) <- c("Ignore","subject","item","Level","grammaticality","log_TFT","centered_similarity")
head(data)

data$grammaticality<-as.factor(data$grammaticality)
levels(data$grammaticality)

xtabs(~subject+grammaticality,data)

#varying intercept model
m0 <- lmer(log_TFT~1+(1|subject)+ (1|item),data)
summary(m0)

#varying intercept and slopes model (withoug correlation) for the factor of grammaticality
m1 <- lmer(log_TFT~1+grammaticality + (1|subject)+(1|item), data)
summary(m1)

m2 <- lmer(log_TFT~1+centered_similarity+(1|subject)+(1|item), data)
m2

#maximal varying intercepts and varying slopes model with correlation

m3 <- lmer(log_TFT~1+grammaticality + centered_similarity+(1|subject)+(1|item), data)
summary(m3)

m4 <- lmer(log_TFT~1+grammaticality*centered_similarity+(1|subject)+(1|item), data)
summary(m4)

anova(m0, m1, m3, m4)
anova(m3, m4)

#install.packages("effects")
library(effects)
#install.packages("car")
library(car)
#install.packages("ggplot2")
library(ggplot2)

effects <- as.data.frame(Effect(c("centered_similarity", "grammaticality"), m4))

ggplot(effects,aes(x=centered_similarity, y=fit, ymin=lower, ymax=upper)) +
  geom_point(aes(colour=grammaticality), size=2) +
  geom_errorbar(aes(colour=grammaticality, width=0.25)) +
  theme_light(base_size = 15) +
  ggtitle("Interaction plot") +
  ylab("RT in milliseconds") +
  xlab("centered similarity") +
scale_color_discrete(name ="Grammaticality",
                     breaks=c("-1", "1"),
                     labels=c("Ungrammatical", "Grammatical")) +
  scale_y_continuous(breaks=log(seq(150,1100,100)),labels= seq(150,1100,100))


# calculations were carried out on the log scale and transformed to milliseconds for visualisation






anova(m0, m2)

anova(m0, m1, m3) #m1 is the best

hist(residuals(m0)) # normally distributed
acf(residuals(m0))

hist(residuals(m1)) # normally distributed
acf(residuals(m1))

hist(residuals(m2)) # normally distributed
acf(residuals(m2))

hist(residuals(m3)) # normally distributed
acf(residuals(m3))

aggregate(log_TFT ~ grammaticality, FUN=mean, data= data)
aggregate(log_TFT ~ grammaticality + centered_similarity, FUN=mean, data= data)

# 95-% confidence interval = mean +/- 2*SE

confint(m3)

6.27595 + 0.24684
exp(6.52279)

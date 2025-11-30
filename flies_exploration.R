
library(readxl)
flies <- read_excel("taban_Collection_Data.xlsx", sheet = "count_data")

View(flies)

#factorize categorical data
flies$rep <- as.factor(flies$rep)
flies$site <- as.factor(flies$site)
flies$location <- as.factor(flies$location)

summary(flies)


library(tidyverse)

#means and sd when grouped by study site
flies %>% group_by(site) %>%
  summarise(meanC = mean(count),
            sdC = sd(count))

#means and sd when grouped by trap placement location
flies %>% group_by(location) %>%
  summarise(meanC = mean(count),
            sdC = sd(count))

flies %>% ungroup()


#histogram of all observations
hist(flies$count)


#plot of count vs temp predictors----

plot(count ~ AvgGDD, data = flies, pch = 19)
plot(count ~ AvgCDD, data = flies, pch = 19)
plot(count ~ AvgTemp, data = flies, pch = 19)



#boxplot of count vs trap placement location
boxplot(flies$count ~ flies$location)

##test models----

fliesGDD_poisson <- glm(formula = count ~ AvgGDD + location, family = poisson(link = "log"),
                        data = flies)
summary(fliesGDD_poisson)

fliesTemp_poisson <- glm(formula = count ~ AvgTemp + location, family = poisson(link = "log"),
                        data = flies)
summary(fliesTemp_poisson)

fliesCDD_poisson <- glm(formula = count ~ AvgCDD + location, family = poisson(link = "log"),
                         data = flies)
summary(fliesCDD_poisson)

#looks like Avg Temperature gives the best AIC here

##should check dispersion----

library(AER)

deviance(fliesTemp_poisson)/fliesTemp_poisson$df.residual

dispersiontest(fliesTemp_poisson)
dispersiontest(fliesGDD_poisson)
dispersiontest(fliesCDD_poisson)

###better fit a NBin model----

library(MASS)

fliesTemp_negbin <- glm.nb(formula = count ~ AvgTemp + location,
                           data = flies)
summary(fliesTemp_negbin)

###and test for zero-inflation----

library(pscl)

fliesTemp_ZINB <- zeroinfl(count ~ AvgTemp + location, data = flies, dist = "negbin")

AIC(fliesTemp_negbin, fliesTemp_ZINB)

#looks like a regular NB fits better

##fitting a NegBin GLMM----

library(lme4)

##need something that can handle NegBin GLMM----


flies_GLMM <- glmer.nb(count ~ AvgTemp + location + (1|site) + (1|rep), data = flies)
CDD_GLMM <- glmer.nb(count ~ AvgCDD + location + (1|site) + (1|rep), data = flies)
GDD_GLMM <- glmer.nb(count ~ AvgGDD + location + (1|site) + (1|rep), data = flies)

AIC(flies_GLMM, GDD_GLMM, CDD_GLMM)

summary(flies_GLMM)

##plotting predict?----

new_data_GLMM <- data.frame(AvgTemp = seq(9, 25, 0.001))
new_data_GLMM$Predicted_flies_GLMM <- predict(flies_GLMM,
                                                  type = "response",
                                                  newdata = new_data_GLMM,
                                                  re.form = NA)


ggplot(data=flies, mapping = aes(x=AvgTemp, y=count)) +
  geom_point()+theme_classic()+
  geom_line(data=new_data_GLMM,
            aes(x=AvgTemp, y=Predicted_flies_GLMM), linewidth=1)

logflies <- new_data_GLMM
logflies$transform <- log(logflies$Predicted_flies_GLMM)

plot(x = logflies$AvgTemp, y = logflies$transform)

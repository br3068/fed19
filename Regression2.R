## Exploratory

#1.0 load packages

library(tidyverse)
library(plyr)
library(ggplot2)
library(ggpmisc)
setwd("D:/OneDrive/FED19/Social")

#Transformations  
QR <- readRDS("QALLVONFC")
head(QR)

QR %>% 
  summary()

boxplot(QR)

fitl <- QRlog %>% lm()

summary(fitl)

QRL <- gather(QR, attribute, value, -PON)
View(QRL)




QRlog <- readRDS("QALLVONFCL")

QRlog %>% 
  summary()

boxplot(QRlog)

QRlogL <- gather(QRlog, attribute, value, -PON)
View(QRlogL)

my.formula <- y ~ x
ggplot(QRL)+aes(x=value, y=PON)+
  geom_jitter()+
  geom_smooth(method=lm)+
  stat_poly_eq(formula = my.formula,aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")),parse = TRUE) +
  facet_wrap(~attribute)

my.formula <- y ~ x
ggplot(QRlogL)+aes(x=value, y=PON)+
  geom_jitter()+
  geom_smooth(method=lm)+
  stat_poly_eq(formula = my.formula,aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")),parse = TRUE) +
  facet_wrap(~attribute)

ggplot(QRL)+aes(value)+
  geom_histogram()+
  facet_wrap( ~attribute)

ggplot(QRlogL)+aes(value)+
  geom_histogram()+
  facet_wrap( ~attribute)



#MRA Linear1
QRlogA <- QRlog %>%
  select(PON, Ted, Cert3, NEL, AGRI, blu)

fit2 <- QRlogA %>% lm()

summary(fit2)
fit2 %>% confint()
fit2 %>% summary()

round(confint(fit2, level=0.95),3)


# Predict values 
pred <- fit2 %>% predict()

test <- bind_cols(QRlog$PON,pred)

ggplot(test)+aes(x=...1,y=...2)+
  geom_point()+
  geom_smooth(method=lm)




fit2 %>% predict(interval = "prediction")

# Regression diagnostics
fit2 %>% lm.influence()
fit2 %>% influence.measures()

# Diagnostic plots; run command then hit return in Console
# 1: Residuals vs. Fitted
# 2: Normal Q-Q
# 3: Scale-Location
# 4: Residuals vs. Leverage
fit2 %>% plot()


library(lmtest)



influencePlot(fit2)



rm(list = ls())  # Removes all objects from environment
graphics.off()  # Clears plots, closes all graphics devices
cat("\014")  # Mimics ctrl+L

## Survey analysis

library(caret)
library(Rtsne)
library(tidyverse)
library(rworldmap)
library(leaflet)
library(gridExtra)
library(plotly)

unzip("Kiehl's survey.csv.zip")
data_raw <- read.csv("Kiehl's survey.csv", stringsAsFactors = FALSE)

data <- data_raw
names(data) <- c("time.stamp", "person.gender", "person.age", "person.country", "person.residency", "person.profession",
                 "person.education", "person.income", "person.ethnicity", "person.marital", "person.children",
                 "behave.time.online", "behave.online.buy", "behave.phone.buy", "behave.phone.use",
                 "behave.sociality", "behave.social.nets", "behave.sporty", "behave.environment", "behave.trendy",
                 "behave.skincare.care", "behave.skincare.use", "behave.inform.buy", "behave.expert.recommendations", 
                 "behave.friend.recommendations", "behave.celebrity.recommendation", "behave.where.buy",
                 "skincare.spend", "skincare.brand", "skincare.natural", "skincare.price",
                 "skincare.quality", "skincare.mass.premium", "skincare.social.image", "skincare.service", 
                 "skincare.free.samples", "skincare.returns", "skincare.environment.buy", "kiehls.know", 
                 "kiehls.use", "kiehls.satisfaction", "kiehls.service", "kiehls.recommend", "kiehls.product", 
                 "kiehls.associate", "online.service", "online.why", "online.favorite", "online.satisfaction", 
                 "online.frequency", "online.quantity", "online.time", "online.items", "showroom.webroom", 
                 "use.kiehls?")





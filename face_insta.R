library(tidyverse)
library(plotly)
library(gridExtra)


HRV_prihod <- data.frame(
  prihod=c(302330, 313464, 322098, 366818, 473477, 493458, 487272, 527516, 569250),
  dobit=c(23129, 32245, 25283, 30598, 49339, 57899, 47477, 55137, 53611),
  godina=as.factor(c(2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017))
)

HRV_prihod <- HRV_prihod %>%
  mutate(rast=HRV_prihod$dobit/c(HRV_prihod$prihod[1], HRV_prihod$prihod[1:length(HRV_prihod$prihod)-1]))

ggplot(HRV_prihod) +
  geom_col(aes(x=godina, y=prihod), fill="coral") +
  geom_label(aes(x=godina, y=prihod, label=round(rast,3)), col="indianred1")


## Facebook data 
face <- read.csv("facebook.csv")
names(face) <- c("date", "posts.to.page", "page.mentions", "comments.by.fans",
                 "reactions.by.fans", "shares", "total", "posts", "comments.by.page")
face$date <- as.Date(face$date)
face <- face[-nrow(face),c(-3,-7)]
face_long <- gather(face, activity, value, -date)

face_summary <- data.frame(as.list(colSums(face[,-1], na.rm = TRUE)/nrow(face)  ))


ggplotly(
  ggplot(face_long, aes(x=date, y=value, fill=activity)) +
    geom_col() +
    labs(x="Datum", y="Količina aktivnosti", title="Aktivnost Kiehl's Hrvatska facebook stranice")
  )  

colSums(face[,-1], na.rm = TRUE)/nrow(face)  


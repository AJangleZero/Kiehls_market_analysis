## Cleaning the data
data.pca2 <- data[,c(2:40,50:55)]
data.pca2 <- data.pca2[complete.cases(data[,c(2:38)]),c(-3,-5, -8, -9, -28)]
sum(is.na(data.pca2))
data.pca2$person.gender <- as.integer(factor(data.pca2$person.gender, 
                                             levels=c("Male", "Female"), ordered = TRUE))
data.pca2$person.age <- as.integer(factor(data.pca2$person.age, 
                                          levels=c("<20", "20-29", "30-39", "40-49", "50-59", ">60"), ordered = TRUE))
data.pca2$person.residency <- as.integer(factor(data.pca2$person.residency, 
                                                levels=c("Rural area", "Small town", "Big city"), ordered = TRUE))
data.pca2$person.education <- as.integer(factor(data.pca2$person.education, 
                                                levels=c("No schooling completed", "Nursery school to 8th grade",
                                                         "High school graduate", "Bachelor's degree or eqvivalent",
                                                         "Master's degree or eqvivalent", "Doctorate degree"), ordered = TRUE))
data.pca2$person.income <- as.integer(factor(data.pca2$person.income, 
                                             levels=c("<$500", "$500-$799", "$800-$1000", "$1000-$1499", 
                                                      "$1500-$3000", ">$3000"), ordered = TRUE))
data.pca2$person.children <- as.integer(factor(data.pca2$person.children, 
                                               levels=c("0", "1", "2", "3+"), ordered = TRUE))
data.pca2$skincare.spend <- as.integer(factor(data.pca2$skincare.spend, 
                                              levels=c("<$10", "$10-$19", "$20-$49", "$50-$99", "$100-$199", ">$200"), ordered = TRUE))
data.pca2$behave.skincare.use <- as.integer(factor(data.pca2$behave.skincare.use, 
                                                   levels=c("No", "Yes"), ordered = TRUE))
data.pca2$kiehls.use <- as.integer(factor(data.pca2$kiehls.use, 
                                          levels=c("No", "Yes"), ordered = TRUE))
data.pca2 <- data.pca2[, -1+c(-2,-3,-5,-16, -22, -32, -34, -35, -36, -37, -38, -39)]

data.pca2$cluster <- Kmeans$cluster
range01 <- function(x){(x-min(x))/(max(x)-min(x))}

library(ggradar)

new.data <- data.frame(
  age=range01(data.pca2$person.age),
  gender=range01(abs(data.pca2$person.gender-2)),
  income=range01(data.pca2$person.income),
  socially.active=range01(data.pca2$behave.sociality+data.pca2$behave.social.nets+
                 data.pca2$behave.sporty+data.pca2$behave.environment+
                 data.pca2$behave.trendy),
  digitally.active=range01(data.pca2$behave.time.online+data.pca2$behave.phone.use+
                            data.pca2$behave.social.nets),
  skincare=range01(data.pca2$behave.skincare.care),
  brand.service=range01(data.pca2$skincare.service+data.pca2$skincare.returns+data.pca2$skincare.free.samples),
  brand.quality=range01(data.pca2$skincare.natural+data.pca2$skincare.quality),
  brand.price=range01(data.pca2$skincare.price),
  brand.image=range01(data.pca2$skincare.environment.buy+data.pca2$skincare.social.image+
                        data.pca2$behave.environment),
  premium=range01(data.pca2$skincare.mass.premium),
  buy.online=range01(data.pca2$behave.online.buy+data.pca2$behave.phone.buy+data.pca2$behave.where.buy),
  informed=range01(data.pca2$behave.inform.buy+data.pca2$behave.expert.recommendations+
                   data.pca2$behave.friend.recommendations+data.pca2$behave.celebrity.recommendation),
  use.kiehls= range01(data.pca2$kiehls.use),
  cluster=paste("Buyer persona", data.pca2$cluster)
)

new.data <- new.data %>%
  group_by(cluster) %>%
  summarise_all(funs(mean(., na.rm=TRUE)))

names <- c("Dob", "Spol (M)", "Prihod", "Društvena aktivnost", "Digitalna aktivnost", "Briga za zdravlje kože", "Važnost usluge brenda", 
           "Važnost kvalitete i sastava proizvoda", "Važnost cijene proizvoda", "Važnost slike brenda", "Premium", 
           "Online kupnja", "Informiranje prije kupnje","Korištenje Kiehl's")
g_spyder <-ggradar(new.data, axis.label.size = 3, legend.text.size = 10, grid.label.size = 4,
        group.line.width = 1, group.point.size = 4, axis.labels = names)

png("sypder.png", height = 500, width = 800)
g_spyder
dev.off()

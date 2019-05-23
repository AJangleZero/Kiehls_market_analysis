## Cleaning the data
data.pca1 <- data[,c(2:40,50:55)]
data.pca1 <- data.pca1[complete.cases(data[,c(2:38)]),c(-3,-5, -8, -9, -28)]
sum(is.na(data.pca1))
data.pca1$person.gender <- as.integer(factor(data.pca1$person.gender, 
                                            levels=c("Male", "Female"), ordered = TRUE))
data.pca1$person.age <- as.integer(factor(data.pca1$person.age, 
                                         levels=c("<20", "20-29", "30-39", "40-49", "50-59", ">60"), ordered = TRUE))
data.pca1$person.residency <- as.integer(factor(data.pca1$person.residency, 
                                               levels=c("Rural area", "Small town", "Big city"), ordered = TRUE))
data.pca1$person.education <- as.integer(factor(data.pca1$person.education, 
                                               levels=c("No schooling completed", "Nursery school to 8th grade",
                                                        "High school graduate", "Bachelor's degree or eqvivalent",
                                                        "Master's degree or eqvivalent", "Doctorate degree"), ordered = TRUE))
data.pca1$person.income <- as.integer(factor(data.pca1$person.income, 
                                            levels=c("<$500", "$500-$799", "$800-$1000", "$1000-$1499", 
                                                     "$1500-$3000", ">$3000"), ordered = TRUE))
data.pca1$person.ethnicity <- as.integer(factor(data.pca1$person.ethnicity))
data.pca1$person.marital <- as.integer(factor(data.pca1$person.marital))
data.pca1$person.children <- as.integer(factor(data.pca1$person.children, 
                                              levels=c("0", "1", "2", "3+"), ordered = TRUE))
data.pca1$skincare.spend <- as.integer(factor(data.pca1$skincare.spend, 
                                             levels=c("<$10", "$10-$19", "$20-$49", "$50-$99", "$100-$199", ">$200"), ordered = TRUE))
data.pca1$behave.skincare.use <- as.integer(factor(data.pca1$behave.skincare.use, 
                                             levels=c("No", "Yes"), ordered = TRUE))
data.pca1$kiehls.use <- as.integer(factor(data.pca1$kiehls.use, 
                                             levels=c("No", "Yes"), ordered = TRUE))


data.pca1$cluster <- Kmeans$cluster

sum <- data.pca1 %>%
  group_by(cluster) %>%
  summarise_all(funs(mean(., na.rm=TRUE)))

featurePlot(x=data.pca1[,1:40], y=as.factor(data.pca1[,41]), plot="box",
            scales=list(x=list(relation="free"), y=list(relation="free")))

                
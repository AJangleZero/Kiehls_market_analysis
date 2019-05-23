## PCA and Kmeans to find buyer personas

## Cleaning the data
data.pca <- data[,c(2:38)]
data.pca <- data.pca[complete.cases(data.pca),c(-3,-5, -28, -21)]
sum(is.na(data.pca))
data.pca$person.gender <- as.integer(factor(data.pca$person.gender, 
                                            levels=c("Male", "Female"), ordered = TRUE))
data.pca$person.age <- as.integer(factor(data.pca$person.age, 
                                         levels=c("<20", "20-29", "30-39", "40-49", "50-59", ">60"), ordered = TRUE))
data.pca$person.residency <- as.integer(factor(data.pca$person.residency, 
                                               levels=c("Rural area", "Small town", "Big city"), ordered = TRUE))
data.pca$person.education <- as.integer(factor(data.pca$person.education, 
                                               levels=c("No schooling completed", "Nursery school to 8th grade",
                                                        "High school graduate", "Bachelor's degree or eqvivalent",
                                                        "Master's degree or eqvivalent", "Doctorate degree"), ordered = TRUE))
data.pca$person.income <- as.integer(factor(data.pca$person.income, 
                                            levels=c("<$500", "$500-$799", "$800-$1000", "$1000-$1499", 
                                                     "$1500-$3000", ">$3000"), ordered = TRUE))
data.pca$person.ethnicity <- as.integer(factor(data.pca$person.ethnicity))
data.pca$person.marital <- as.integer(factor(data.pca$person.marital))
data.pca$person.children <- as.integer(factor(data.pca$person.children, 
                                              levels=c("0", "1", "2", "3+"), ordered = TRUE))
data.pca$skincare.spend <- as.integer(factor(data.pca$skincare.spend, 
                                             levels=c("<$10", "$10-$19", "$20-$49", "$50-$99", "$100-$199", ">$200"), ordered = TRUE))
data.pca <- data.frame(scale(data.pca))

## PCA and Kmeans
PCA <- predict(preProcess(data.pca, method="pca"), data.pca)
Kmeans <- kmeans(data.pca,4)
ggplot(data.frame(PC1=PCA$PC1, PC2=PCA$PC2, cluster=factor(Kmeans$cluster)), aes(x=PC1, y=PC2, color=cluster)) +
  geom_point(size=2)

e <- NULL
for (i in 1:30) e[i] <- kmeans(data.pca, i)$tot.withinss

plot_ly(data.frame(J=e, diff=c(Inf,round(diff(e),2)), num_clusters=seq(1:30)), x = ~num_clusters,
        y = ~J, text = ~diff) %>%
  add_markers() %>%
  add_text(textfont = list(family = "Arial", 
                           size = 12, 
                           color = "grey"), 
           textposition = "top right") %>%
  layout(title = "Within error vs. broj klastera",
         xaxis = list(title = "Broj klastera", zeroline = TRUE),
         yaxis = list(title = "J", zeroline=TRUE), 
         showlegend = FALSE)

## Plotting the clusters
library(MASS)
library(ggplot2)
require(scales)
PCA$cluster <- Kmeans$cluster
fit <- lda(cluster ~ PC1+PC2, data = PCA)
datPred <- data.frame(cluster=predict(fit)$class,predict(fit)$x)
fit2 <- lda(cluster ~ PC1 + PC2, data=PCA)
ld1lim <- expand_range(c(min(datPred$LD1),max(datPred$LD1)),mul=0.05)
ld2lim <- expand_range(c(min(datPred$LD2),max(datPred$LD2)),mul=0.05)
ld1 <- seq(ld1lim[[1]], ld1lim[[2]], length.out=300)
ld2 <- seq(ld2lim[[1]], ld1lim[[2]], length.out=300)
newdat <- expand.grid(list(PC1=ld1,PC2=ld2))
preds <-predict(fit2,newdata=newdat)
predclass <- preds$class
postprob <- preds$posterior
df <- data.frame(x=newdat$PC1, y=newdat$PC2, class=predclass)
df$classnum <- as.numeric(df$class)
df <- cbind(df,postprob)

colorfun <- function(n,l=65,c=100) { hues = seq(15, 375, length=n+1); hcl(h=hues, l=l, c=c)[1:n] } # default ggplot2 colours
colors <- colorfun(4)
colorslight <- colorfun(4,l=90,c=50)
g_clusters <- ggplot(datPred, aes(x=LD1, y=LD2) ) +
  geom_raster(data=df, aes(x=x, y=y, fill = factor(class)),alpha=0.7,show.legend=FALSE) +
  #geom_contour(data=df, aes(x=x, y=y, z=classnum), colour="red2", alpha=0.5, breaks=c(1.5,2.5)) +
  geom_point(data = datPred, size = 3, aes(colour=cluster)) +
  scale_x_continuous(limits = ld1lim, expand=c(0,0)) +
  scale_y_continuous(limits = ld2lim, expand=c(0,0)) +
  scale_fill_manual(values=colorslight,guide=F) +
  annotate("text", x = -2.5, y = 2.5, label = "Rookies", color=colorfun(4,l=40,c=100)[1], size=10) +
  annotate("text", x = -5, y = -2, label = "Passivists", color=colorfun(4,l=40,c=100)[2], size=10) +
  annotate("text", x = 1, y = -4, label = "Veterans", color=colorfun(4,l=30,c=100)[3], size=10) +
  annotate("text", x = 5, y = -1, label = "Addicts", color=colorfun(4,l=40,c=100)[4], size=10) +
  labs(x = "PC1", y = "PC2") 
  

png("clusters.png", width = 800, height = 500)
g_clusters
dev.off()

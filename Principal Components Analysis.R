# Principal components analysis

sessionInfo()

# Remove all loaded packages to prevent conflicts between packages
detachAllPackages <- function() {
  basic.packages <- c("package:stats","package:graphics","package:grDevices","package:utils","package:datasets","package:methods","package:base")
  package.list <- search()[ifelse(unlist(gregexpr("package:",search()))==1,TRUE,FALSE)]
  package.list <- setdiff(package.list,basic.packages)
  if (length(package.list)>0)  for (package in package.list) detach(package, character.only=TRUE)
}
detachAllPackages()

sessionInfo()

# First always clear R environment
# This prevents using objects created previously
rm(list=ls())

# options(warn=-1) # only for Knit


###############################################################################################
# Dataset
#
###############################################################################################

# Iris Dataset
df <- iris[c(1, 2, 3, 4)]
head(iris)


###############################################################################################
# Principal Components Analysis
#
###############################################################################################

# Principal Components Analysis
pca <- prcomp(df)

# Old School
components <- data.frame(pca$x)
components$PC1
plot(components$PC1, components$PC2)
plot(components$PC3, components$PC4)

# Nice plots with autoplot
library(ggfortify)
autoplot(pca, x = 1, y = 2,
         main="PCA",
         loadings = FALSE, loadings.label = FALSE)

autoplot(pca, x = 3, y = 4,
         main="PCA",
         loadings = FALSE, loadings.label = FALSE)

# Nice plots with ggplot 2
ggplot(components, aes(x=PC1, y=PC2))+
  geom_point(aes(color=iris$Species, shape=iris$Species))

components[,"row_names_vectors"]  <- data.frame(row.names(components))
head(components)
dim(components)

iris[,"row_names_meta"]  <- data.frame(row.names(iris))
head(iris)
dim(iris)

meta_vector <- merge(components, iris, by.x="row_names_vectors", by.y="row_names_meta")
head(meta_vector)


a <- ggplot(meta_vector, aes(x=Species, y=PC1))+
  geom_boxplot(width=0.3, color="blue", fill="blue", alpha=0.7)+
  stat_summary(geom = "crossbar", width=0.65, fatten=0, color="white", 
               fun.data = function(x){ return(c(y=median(x), ymin=median(x), ymax=median(x))) })+
 xlab("")+
    ylab("")+
      theme(axis.line = element_line(colour = "black"),
        panel.background = element_rect(fill = NA, color = NA), 
       # legend.position="none",
        axis.text.x = element_text(colour="grey20",size=8,face="bold"))+
  coord_flip()

b <- ggplot(meta_vector, aes(x=Species, y=PC2))+
  geom_boxplot(width=0.3, color="blue", fill="blue", alpha=0.7)+
  stat_summary(geom = "crossbar", width=0.65, fatten=0, color="white", 
               fun.data = function(x){ return(c(y=median(x), ymin=median(x), ymax=median(x))) })+

  xlab("")+
  ylab("")+
      theme(axis.line = element_line(colour = "black"),
        panel.background = element_rect(fill = NA, color = NA), 
        legend.position="none",
        axis.text.x = element_blank())



p <- ggplot(components, aes(x=PC1, y=PC2))+
  geom_point(aes(color=iris$Species))


c<- ggplot(components, aes(x=PC1, y=PC2))+
theme(line = element_blank(),
        text = element_blank(),
        title = element_blank())

library(cowplot)

upper_row <- plot_grid(b, p,  align = 'h', rel_widths = c(1/4,3/4))
bottom_row <- plot_grid(c, a,  align = 'h', rel_widths = c(0.21,0.79))

#pdf("pimp_my_PCoA_order_extra_pimping.pdf",width =12, height=8,useDingbats=FALSE)                                
plot_grid(upper_row,bottom_row, nrow = 2, ncol = 1, align="h", rel_heights = c(3, 1))
#dev.off()

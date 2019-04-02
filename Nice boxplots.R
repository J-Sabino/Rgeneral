# Nice Boxplots
# Script by Joao Sabino

date()

# Dettach all packages, this prevents conflicts between packages loaded in the session
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


###############################################################################################
# Dataset
#
###############################################################################################

# Iris Dataset
df <- iris[c(1, 2, 3, 4)]
head(iris)


###############################################################################################
# Nice boxplots
#
###############################################################################################
library(ggplot2)

ggplot(iris, aes(x=Species, y=Sepal.Length))+
  geom_boxplot(width=0.3, color="#bcbddc", fill="#bcbddc", outlier.shape = NA)+
  stat_summary(geom = "crossbar", width=0.65, fatten=0, color="white", 
               fun.data = function(x){ return(c(y=median(x), ymin=median(x), ymax=median(x))) })+
  geom_jitter(aes(color=Species), width = 0.1, size=1)+
  xlab("Species")+
  ylab("Septal lenght")+
  scale_color_manual(values = c("#32CD32", "#f03b20","#1E90FF"))+
  theme(axis.line = element_line(colour = "black"),
        panel.background = element_rect(fill = NA, color = "black"),
        axis.text.x = element_text(colour="grey20",size=12,face="bold"),
        axis.text.y = element_text(colour="grey20",size=12,face="bold"),
        axis.title=element_text(size=12,face="bold"),
        legend.text=element_text(size=10),
        legend.title=element_text(size=12,face="bold"))


# Adding the significance bars
library(ggsignif)

ggplot(iris, aes(x=Species, y=Sepal.Length))+
  geom_boxplot(width=0.3, color="#bcbddc", fill="#bcbddc", outlier.shape = NA)+     # Set the colour of the boxplot
  stat_summary(geom = "crossbar", width=0.65, fatten=0, color="white", 
               fun.data = function(x){ return(c(y=median(x), ymin=median(x), ymax=median(x))) })+
  geom_jitter(aes(color=Species), width = 0.1, size=1, height = 0)+      # set the width of jitter, height should be 0 (compare with plot above)
  xlab("Species")+
  ylab("Septal lenght")+
  scale_color_manual(values = c("#32CD32", "#f03b20","#1E90FF"))+  # Colours of the point
  theme(axis.line = element_line(colour = "black"),
        panel.background = element_rect(fill = NA, color ="black"),
        axis.text.x = element_text(colour="grey20",size=12,face="bold"),
        axis.text.y = element_text(colour="grey20",size=12,face="bold"),
        axis.title=element_text(size=12,face="bold"),
        legend.text=element_text(size=10),
        legend.title=element_text(size=12,face="bold"))+
  geom_signif(comparisons = list(c("versicolor", "virginica"),       # set the list of comparisons
                                 c("versicolor", "setosa"),
                                 c("setosa", "virginica")),
              y_position=c(8, 7.5, 8.3),                     # control the height of the bars
              test=c("wilcox.test"),
              map_signif_level=TRUE, tip_length=0)



# If you want a line around the plot

ggplot(iris, aes(x=Species, y=Sepal.Length))+
  geom_boxplot(width=0.3, color="#bcbddc", fill="#bcbddc", outlier.shape = NA)+     # Set the colour of the boxplot
  stat_summary(geom = "crossbar", width=0.65, fatten=0, color="white", 
               fun.data = function(x){ return(c(y=median(x), ymin=median(x), ymax=median(x))) })+
  geom_jitter(aes(color=Species), width = 0.1, size=1, height = 0)+      # set the width of jitter, height should be 0 (compare with plot above)
  xlab("Species")+
  ylab("Septal lenght")+
  scale_color_manual(values = c("#32CD32", "#f03b20","#1E90FF"))+  # Colours of the point
  theme(axis.line = element_line(colour = "black"),
        panel.background = element_rect(fill = NA, color =NA),
        axis.text.x = element_text(colour="grey20",size=12,face="bold"),
        axis.text.y = element_text(colour="grey20",size=12,face="bold"),
        axis.title=element_text(size=12,face="bold"),
        legend.text=element_text(size=10),
        legend.title=element_text(size=12,face="bold"))+
  geom_signif(comparisons = list(c("versicolor", "virginica"),       # set the list of comparisons
                                 c("versicolor", "setosa"),
                                 c("setosa", "virginica")),
              y_position=c(8, 7.5, 8.3),                     # control the height of the bars
              test=c("wilcox.test"),
              map_signif_level=TRUE, tip_length=0)


###############################################################################################
# END 
###############################################################################################

sessionInfo()
Sys.time()
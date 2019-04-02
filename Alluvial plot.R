# Alluvial plots - similar to Sankey plots
# Script by Joao Sabino

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

# Titanic dataset
tit <- as.data.frame(Titanic, stringsAsFactors = FALSE)
head(tit)

library(alluvial)
# Refugees dataset
data(Refugees)
head(Refugees)

###############################################################################################
# Alluvial plot
#
###############################################################################################

# Simple alluvial plot
alluvial(tit[,1:4], freq=tit$Freq,
         col = ifelse(tit$Survived == "Yes", "orange", "grey"),
         border = ifelse(tit$Survived == "Yes", "orange", "grey"),
         hide = tit$Freq == 0,
         cex = 0.7
)


library(magrittr)
library(dplyr)
tit %>% group_by(Sex, Class, Survived) %>%
  summarise(n = sum(Freq)) -> tit3d

# Remove white boxes
alluvial(
  tit3d[,1:3], 
  freq=tit3d$n,
  col = ifelse( tit3d$Sex == "Female", "pink", "lightskyblue"),
  border = "grey",
  alpha = 0.7,
  blocks=FALSE
)


# More complex alluvial plot
set.seed(39) # for nice colours
cols <- hsv(h = sample(1:10/10), s = sample(3:12)/15, v = sample(3:12)/15)

# pdf("Alluvial.pdf",width =15, height=11, useDingbats=FALSE)
alluvial_ts(Refugees, wave = .3, ygap = 5, col = cols, plotdir = 'centred', alpha=.9,
            grid = TRUE, grid.lwd = 5, xmargin = 0.2, lab.cex = .7, xlab = '',
            ylab = '', border = NA, axis.cex = .8, leg.cex = .7,
            leg.col='white', 
            title = "UNHCR-recognised refugees\nTop 10 countries (2003-13)\n")
# dev.off()

###############################################################################################
# END 
###############################################################################################

sessionInfo()
Sys.time()

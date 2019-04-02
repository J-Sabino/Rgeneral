# Circlize
# script by Joao
# Start 30/08/2017

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

# Setting working directory 
setwd("~/Dropbox/Doutoramento_Joao") # Joao


###############################################################################################
# Circlize
#
###############################################################################################

df = read.table(file="circlize.txt", header = T, sep="\t", stringsAsFactors = FALSE)
df[1:5,]

factor = c(structure(df$factor_from, names=df$subfactor_from),
          structure(df$factor_to,names= df$subfactor_to))
factor = factor[!duplicated(names(factor))]
factor = factor[order(factor, names(factor))]
str(df)
factor_color = structure(1:5, names = unique(factor))
subfactor_color = structure(6:19, names = names(factor))

factor
factor_color
subfactor_color

(df[, c(2, 4)])

library(circlize)

gap.degree = do.call("c", lapply(table(factor), function(i) c(rep(2, i-1), 8)))
circos.par(gap.degree = gap.degree)

#pdf(file="Circlize_IBD.pdf", width = 8, height = 9)
chordDiagram(df[, c(2, 4)], order = names(factor), grid.col = subfactor_color,
    directional = 1, annotationTrack = c("grid","name"),preAllocateTracks = list(
        track.height = uh(5, "mm"),
        track.margin = c(uh(5, "mm"), 0)
)
)


for(b in unique(factor)) {
  subfactor = names(factor[factor == b])
  highlight.sector(sector.index = subfactor, track.index = 1, cex = 1, text.col = "white", col = factor_color[b], 
    text = b,  niceFacing = TRUE)
}

#dev.off()


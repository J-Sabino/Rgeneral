# Heatmap with microbiota
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
library(phyloseq)

# enterotype Dataset
data(enterotype)
enterotype

###########################################################################
# Heatmap
# 
###########################################################################
library(pheatmap)
    
physeq <- enterotype # define phyloseq object to use 
# This is usefull when you want to subset data
metadata <- data.frame(sample_data(physeq))
str(metadata)

# Basic heatmap from phyloseq package
#####################################
    plot_heatmap(physeq, sample.label="Nationality",  sample.order = "ClinicalStatus")   
# interesting result, however, to many samples
# try to look only to americans
    
    japanese <- subset_samples(enterotype, Nationality=="japanese")
    japanese
    plot_heatmap(japanese, sample.label="Nationality",  sample.order = "ClinicalStatus")   
    # Less informative

    
# Heatmap with clustering
#####################################
    library(pheatmap)
    metadata <- data.frame(sample_data(physeq))
    df <-  metadata[,c("Nationality", "ClinicalStatus")]
    otu_df <- data.frame(otu_table(physeq))
    pheatmap(t(otu_df), cluster_rows=TRUE, show_rownames=TRUE,
                cluster_cols=TRUE, annotation_row=df)
    
    # This will work better with smaller datasets (less genera)
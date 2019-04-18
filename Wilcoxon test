df1 <- data.frame()
su1 <- data.frame()
mat1 <- matrix(nrow=ncol(Genus_table),ncol=1)
rownames(mat1) <- colnames(Genus_table)

for (i in colnames(Genus_table)) 
{ 
  t=wilcox.test(Genus_table_groupA[,i],Genus_table_groupB[,i], paired=FALSE);
  z<-t$estimate;
  p<-t$p.value;
  df1<-rbind(df1,cbind(i,t,z,p))
  su1 <- rbind(su1, cbind(i,p))
  mat1[i,1] <- p
}

colnames(mat1)[1] <- "pvalue"
su1 <- data.frame(mat1)
adj1 <- p.adjust(su1$pvalue,method="BH", n=ncol(Genus_table))
su11<-cbind(su1,adj1)
significant <- subset(su1, su1$pvalue<0.05)
significant2 <- subset(su11, adj1<0.05)
print(significant)
print(significant2)

# df1 data frame will provide you more information
# significant gives you the significant values BEFORE multiple test correction
# significant2 gives you the significant values AFTER multiple test correction

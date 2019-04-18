wilcox_otu <- function(a,b)
  { 
      otu_a <- data.frame(otu_table(a))
      otu_b <- data.frame(otu_table(b))
      mat1 <- matrix(nrow=ncol(otu_a),ncol=1)
      rownames(mat1) <- colnames(otu_a)
      
      for (i in colnames(otu_a)) 
      { 
        t=wilcox.test(otu_a[,i],otu_b[,i], paired=F);
        p<-t$p.value;
        mat1[i,1] <- p
      }
      
      colnames(mat1)[1] <- "pvalue"
      df1 <- data.frame(mat1)
      adj1 <- p.adjust(df1$pvalue,method="BH", n=ncol(otu_a))
      df2<-cbind(df1,adj1)
      significant <- subset(df1, pvalue<0.05)
      significant2 <- subset(df2, adj1<0.05)
      cat("Significant BEFORE multiple testing correction\n")
      print(significant)
      cat("\nSignificant AFTER multiple testing correction (FDR)\n")
      print(significant2)
  
  }

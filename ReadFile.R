# read csv Expression data for iSeq

csvRead <- function(file){
  exp = read.csv(file, header= T, sep=',', stringsAsFactors = F)
  genes = exp[,1]
  
  if ( length(genes) != length(unique(genes)) ){
    dup_gene = duplicated(genes)
    #exp = exp[!duplicated(genes),]
    dup_gene_name = genes[dup_gene]
    genes = genes[!dup_gene]
    
    for (j in unique(dup_gene_name)){
      tmp = exp[exp[,1] == j,-1]
      tmp = matrix(as.numeric( unlist(tmp)), ncol = ncol(tmp), byrow = F)
      tmp = colSums(tmp)
      exp = exp[exp[,1] != j,]
      exp = rbind(exp, c(j, tmp))
    }
  }
  rownames(exp) = exp[,1]
  exp = exp[,-1]
  exp1 = matrix(as.numeric( unlist(exp)), ncol = ncol(exp), byrow = F)
  exp1 = as.data.frame(exp1)
  rownames(exp1) = rownames(exp)
  colnames(exp1) = colnames(exp)
  return(exp1)
}

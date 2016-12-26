# t-SNE


  output$tSNE_plot <- renderPlotly({
    withProgress(message = 't-SNE:', value = 0, {
    incProgress(1/4, detail = "load data")  
    tmp = EXP()
    Expression = tmp$Expression
    condition = tmp$condition

    incProgress(2/4, detail = "pca") 
    # pca
    library(ggfortify)
    pca = prcomp(t(Expression))
    pca = pca$x

    if(ncol(pca) > 30){
      pca = pca[, 1:30]
    }

     
    library(tsne)
    tsne_p = tsne(pca)
    rownames(tsne_p) = colnames(Expression)

      incProgress(3/4, detail = "iterating, please wait a minute!") 
      
      tsne_p =  merge(as.data.frame(tsne_p), t(condition), by="row.names")
      colnames(tsne_p) = c("Sample", "X", "Y", "Condition")
      p <- ggplot(tsne_p, aes(X, Y)) +
      geom_point(aes(colour=Condition, text=paste0("Sample: ", Sample)), size = 3.5) +
      theme_bw(base_size = 20) +
      ggtitle("t-SNE")+
      theme(panel.border = element_rect(colour = "black", size = 2.5)) 

      incProgress(4/4, detail = "ploting") 
      ggplotly(p)


    })
    })




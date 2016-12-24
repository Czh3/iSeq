# heatmap

#heatmapGenes <- reactive({
#    if(input$Genes != "Paste a gene list, one gene per line..."){
#    Genes = input$Genes
#    Genes = strsplit(Genes, "\\s+")[[1]]
#
#    Genes = intersect(rownames(Expression), Genes)
#
#    if (length(Genes) <= 2){
#      return("Genelist must contains more than 2 genes")
#    }
#
#    ExpressionSelected = Expression[Genes,]
#
#    return(rownames(ExpressionSelected))
#    
#    } else if (exists("DEGs")){
#      return(DEGs)
#      
#    } else {
#      return("Paste Genes Frist .")
#    }
#  })
  

#  output$heatmapGenes = renderPrint({
#		heatmapGenes()
#  })
  
  heatMAP <- function(scale, clust, rowname){
   	withProgress(message = 'Plot Heatmaps ', value = 0, {
    Expression = EXP()$Expression
    DEGs = values$DEGs

	 if(input$Genes != "Paste a gene list, one gene per line..."){
      Genes = input$Genes
      Genes = strsplit(Genes, "\\s+")[[1]]

      Genes = intersect(rownames(Expression), Genes)

      if (length(Genes) <= 2){
        return(NULL)
      }
      
    } else if (!is.null(DEGs)) {
      Genes = rownames(DEGs)
      
    } else {
      return(NULL)
    }
	incProgress(1/2, detail = "Ploting, Please Wait.")
      ExpressionSelected = Expression[Genes,]
      ExpressionSelected = log10(ExpressionSelected + 1)

      if ("column" %in% clust){
        C = TRUE
      } else {
        C = FALSE
      }

      if ("row" %in% clust){
        R = TRUE
      } else {
        R = FALSE
      }

      library(RColorBrewer)
      library(gplots)
      hmcol = colorRampPalette(brewer.pal(11, "RdYlBu"))(100)

      if (rowname){
        heatmap.2(as.matrix(ExpressionSelected), trace="none", col = rev(hmcol), scale = scale, Rowv = R, Colv = C, main="Gene Expression Heatmap")
      } else {
        heatmap.2(as.matrix(ExpressionSelected), trace="none", col = rev(hmcol), scale = scale, Rowv = R, Colv = C, labRow = "", main="Gene Expression Heatmap")
      }
	incProgress(2/2, detail = "Plotting, Please Wait.")
	})
  }
  
  #heatmapPlot <- eventReactive(input$plotButton, {
  #  heatMAP()
  #})

  output$plot.ui <- renderUI({
    plotOutput("heatmapP", width = paste0(input$heatmapWidth, "px"), height = paste0(input$heatmapHeight, "px"))
  })

  output$heatmapP <- renderPlot({

    heatMAP(heatmap_R(), heatmap_G(), heatmap_RN())
  })


  heatmap_R = reactive({
      switch(input$HeatmapRadio,
          "row" = "row",
          "column" = "column",
          "none" = "none"
        )
    })

  heatmap_G = reactive({
      input$HeatmapGroup
    })

  heatmap_RN = reactive({
      switch(input$HeatmapRownames,
          "Show" = TRUE,
          "Hide" = FALSE
        )
    })
  

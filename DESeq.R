# call differentially expresed genes using "DESeq" r package

values <- reactiveValues()

call_DEG = function(Expression, condition, condition1, condition2){
  withProgress(message = 'Calling Differentially Expressed Genes ', value = 0, {

      library( "DESeq" )

      incProgress(1/4, detail = "loading data")
      tmp = EXP()
      Expression = tmp$Expression
      condition = tmp$condition
      condition.vector = as.vector(as.matrix(condition)[1,colnames(Expression)])
      Expression.int = sapply(Expression, as.integer)
      rownames(Expression.int) = rownames(Expression)
      

      incProgress(2/4, detail = "estimateDispersions")
      cds = newCountDataSet( Expression.int, condition.vector )
      #cds = estimateSizeFactors( cds )
      sizeFactors(cds) = seq(1, 1, length.out = length(condition.vector))
      
      # No replications
      if(length(colnames(condition)[condition[1,] == input$Condition1]) == 1 & length(colnames(condition)[condition[1,] == input$Condition2]) == 1 ){
        cds = estimateDispersions( cds, method = "blind", sharingMode="fit-only" )

      } else {
        cds = estimateDispersions( cds, method = "pooled" )

      }
      
      incProgress(3/4, detail = "Testing")
      res = nbinomTest( cds, condition1, condition2 )
      #DESeq::plotMA(res)
      #resSig = res[ res$padj < 0.01 & (res$log2FoldChange > 1 | res$log2FoldChange < -1) & (res$baseMeanA > 1 | res$baseMeanB > 1), ]
      
      incProgress(4/4, detail = "Done")
      resSig = na.omit(res)
      resSig <- resSig[ order(resSig$pval), ]
      return(resSig)
    
  })
}
    
    output$conditionSelecter1 <- renderUI({
      selectInput("Condition1", "Choose condition1:", unique(t(EXP()$condition)[,1]))
    })
    output$conditionSelecter2 <- renderUI({
      condition = EXP()$condition
      selectInput("Condition2", "Choose condition2:", unique(t(condition)[,1]), selected = unique(t(condition)[,1])[2]) 
    })
    
    
    DEseqButtonAct <- eventReactive(input$DEseqButton, { 
      tmp = EXP()
      Expression = tmp$Expression
      condition = tmp$condition
      DESeqOut <- call_DEG(Expression, condition, input$Condition1, input$Condition2)
      padj = filterDEseqP()
      foldChange = filterDEseqF()
      baseMean = filterDEseqB()
      
      DEGs <-DESeqOut[DESeqOut$baseMean >= baseMean & (DESeqOut$foldChange >= foldChange | DESeqOut$foldChange <= 1/foldChange) & DESeqOut$padj <= padj,]

      rownames(DEGs) = DEGs$id
      DEGs <- DEGs[,2:ncol(DEGs)]

      values$DEGs = DEGs

      DESeq_plot(Expression, condition, DESeqOut, DEGs)
      COPY(DEGs)

      DT::datatable(DEGs)
    })
 

    output$DESeqOutput <- DT::renderDataTable({
          DEseqButtonAct()    
    })


    DESeq_plot <- function(Expression, condition, DESeqOut, DEGs){
      id = rownames(DEGs)
      DEGs = cbind(id, DEGs)
      output$DESeq_valcano <- renderPlotly({
        if (exists("DESeqOut")){
            p = ggplot() + geom_point(data = DESeqOut, aes(x = log2FoldChange, y = -log10(pval), color = log10(baseMean), text = paste0("Gene: ", id)), alpha = 0.3) +
            scale_colour_gradient(low="green", high="red") +
            geom_point(data = DEGs, aes(x = log2FoldChange, y = -log10(pval), color = log10(baseMean), text = paste0("Gene: ", id)),  alpha = 0.8) +
            ggtitle("Volcano plot") +
            theme_bw(base_size = 30) +
            theme(panel.border = element_rect(colour = "black", size = 2))
            g = ggplotly(p)
          } else {
            return(NULL)
          }
      })
    }

  
  

    
    

    ## filter
    filterDEseqP = reactive({
        padj = switch(input$DESeqPadjCutoff,
          "0.01" = 0.01,
          "0.05" = 0.05,
          "0.1" = 0.1,
          "none" = 1
          )
        })
    filterDEseqF = reactive({
        foldChange = switch(input$DESeqfoldChangeCutoff,
          "less than 2/3 or greater than 1.5" = 1.5, # 2/3;1.5 FC
		  "less than 0.5 or greater than 2" = 2,     # 0.5;2 FC
          "less than 1/3 or greater than 3" = 3,     # 1/3;3
          "less than 0.25 or greater than 4" = 4,    # 0.25;4
          "none" = 1                                 # all
          )
        })
    filterDEseqB = reactive({
        baseMean = switch(input$DESeqbaseMeanCutoff,
          "1" = 1,
          "10" = 10,
          "20" = 20,
		  "30" = 30,
          "none" = 0
          )
        })

    # download data
    output$downloadDESeqOutput <- downloadHandler(
    filename = function() { 
      paste(input$file1, 'DESeq.iSeq.csv', sep='') 
    },
    content = function(file) {
      write.csv(values$DEGs, file, quote = F)
    }
    )


    








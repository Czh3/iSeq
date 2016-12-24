
### NOT READY !!! ####
# call differential Expression genes using "DESeq" r package

call_DEG = function(condition1, condition2){
      library( "edgeR" )
      condition.vector = as.vector(as.matrix(condition)[1,colnames(Expression)])
      Expression.int = sapply(Expression, as.integer)
      rownames(Expression.int) = rownames(Expression)
      
      dgel = DGEList(counts=Expression.int, group=factor(condition.vector))
      #dgel = calcNormFactors(dgel)
      dgel = estimateCommonDisp(dgel)
      dgel = estimateTagwiseDisp(dgel)

      
      # No replications
      if(length(colnames(condition[1,condition[1,] == input$Condition1])) == 1 & length(colnames(condition[1,condition[1,] == input$Condition2])) == 1){
        cds = estimateDispersions( cds, method = "blind" )
      } else {
        cds = estimateDispersions( cds, method = "pooled" )
      }
      
      res = nbinomTest( cds, condition1, condition2 )
      #DESeq::plotMA(res)
      #resSig = res[ res$padj < 0.01 & (res$log2FoldChange > 1 | res$log2FoldChange < -1) & (res$baseMeanA > 1 | res$baseMeanB > 1), ]
      resSig = na.omit(res)
      resSig <- resSig[ order(resSig$pval), ]
      return(resSig)
    }
    
    output$conditionSelecter1 <- renderUI({
      selectInput("Condition1", "Choose condition1:", unique(t(condition)[,1]))
    })
    output$conditionSelecter2 <- renderUI({
      selectInput("Condition2", "Choose condition2:", unique(t(condition)[,1]), selected = unique(t(condition)[,1])[2]) 
    })
    
    #samplesInC1 = reactive({colnames(condition[1,condition[1,] == input$Condition1])})
    #samplesInC2 = reactive({colnames(condition[1,condition[1,] == input$Condition2])})
    
    DEseqButtonAct <- eventReactive(input$DEseqButton, {
      DESeqOut <- call_DEG(input$Condition1, input$Condition2)
      padj = filterDEseqP()
      foldChange = filterDEseqF()
      baseMean = filterDEseqB()
      DESeqOut = DESeqOut[DESeqOut$baseMean >= baseMean & (DESeqOut$foldChange >= foldChange | DESeqOut$foldChange <= 1/foldChange) & DESeqOut$padj <= padj,]
      #assign('DESeqOut',DESeqOut,envir=.GlobalEnv)
      DT::datatable(DESeqOut)
    })
 

    output$DESeqOutput <- DT::renderDataTable({
          DEseqButtonAct()    
    })

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
          "none" = 0
          )
        })

    # download data
    output$downloadDESeqOutput <- downloadHandler(
    filename = function() { 
      paste(input$file1, 'DESeq.iSeq.csv', sep='') 
    },
    content = function(file) {
      write.csv(DESeqOut, file, quote = F)
    }
    )

    








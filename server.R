############################################################################
# iSeq: A Web-based server for RNA-seq Data Analysis and Visualization
# Copyright (C) 2016  Chao Zhang
#
# Any bugs or suggestion please report to Czh3(zhangchao3@hotmail.com)
############################################################################


options(shiny.maxRequestSize=40*1024^2) 
options(Expressions=500000)
library(ggplot2)
library(reshape2)
library(shinyBS)
library(zeroclipr)
library(plotly)

source("ReadFile.R", local=TRUE)
shinyServer(func = function(input, output, session) {

  ######### upload file #############
  #source("upload.R", local=TRUE)
  # upload file
  EXP <- reactive({
    if(is.null(input$file1)){
      if(input$ExampleData == 0){
        return(NULL)
      } else {
        return(loadExample())
      }
      
    } else {
      return(uploadData())
    }
    })

  uploadData <- reactive({
      inFile1 <- input$file1
      if (is.null(inFile1)){
        return(NULL)
      }
        
      
       Expression <- csvRead(inFile1$datapath)

      inFile2 = input$file2
      if (is.null(inFile2)){
          condition <- data.frame(Condition = colnames(Expression))
          condition <- t(condition)
          colnames(condition) = colnames(Expression)
          condition <- as.data.frame(condition)
        } else {
          condition <- read.csv(inFile2$datapath, header= T, sep=',', row.names = 1, stringsAsFactors = F)
          rownames(condition) = c("Condition")
          
          if (nrow(condition) != 1 ){
            createAlert(session, "alert", "exampleAlert", title = "Oops",
                        content = "The condition file MUST have 2 lines", append = FALSE, dismiss = FALSE)
          }
          
          if(is.null(Expression)){
            createAlert(session, "alert1", "exampleAlert", title = "Upload Expression data",
                        content = "", append = FALSE)
          } else if (ncol(condition) != ncol(Expression)){
            createAlert(session, "alert2", "exampleAlert", title = "Oops",
                        content = "The columns of condition file MUST be the same as Expression file", append = FALSE, dismiss = FALSE)
          } else if (length(unique(t(condition)[,1])) <= 1){
            createAlert(session, "alert3", "exampleAlert", title = "Just one condition ?", append = FALSE, dismiss = FALSE)
          }
      }



      Expression = NORM()

      return(list('Expression' = Expression, 'condition' = condition))
    })

  loadExample <- reactive({
    tmp = Example()
    tmp$Expression = NORM()
    return(tmp)
    })
    
  # load example
  Example <- eventReactive(input$ExampleData,{
		Expression = csvRead("data/Test.Expression.csv")
		condition = read.csv("data/Test.Condition.csv", header= T, sep=',', row.names = 1, stringsAsFactors = F)
    Expression = NORM()
		return(list('Expression' = Expression, 'condition' = condition))
	})

  
  output$fileUploaded <- reactive({
    return(!is.null(EXP()))
  })
  outputOptions(output, 'fileUploaded', suspendWhenHidden=FALSE)
  
  output$fileUploaded1 <- reactive({
    return(is.null(EXP()))
  })
  outputOptions(output, 'fileUploaded1', suspendWhenHidden=FALSE)
  
  
  ########## Upload page #############
  
  # Expression file
  output$contents <- DT::renderDataTable({
    DT::datatable(EXP()$Expression)
  })
  

  # condition file
  output$condition <- renderTable({
    return(EXP()$condition)
  })
  
  ######### down load Expression ####
  output$downloadExpression <- downloadHandler(
    filename = function() { 
      paste(input$file1, '.iSeq.csv', sep='') 
    },
    content = function(file) {
      write.csv(EXP()$Expression, file, quote = F)
    }
  )
  


 
  ########## main plots ##############
  #source("main_plot.R", local=TRUE)
  # main plots

  plots <- function(Expression){
    withProgress(message = 'QC plot:', value = 0, {
      incProgress(1/5, detail = "Summary")
      
      incProgress(2/5, detail = "Density")
      #distribution
      output$distribution <- renderPlotly({
        if (ncol(Expression) > 20){
          return(NULL)
        }
        Expression.melt = melt(log10(Expression + 1))
        colnames(Expression.melt) = c("Samples", "value")
        p = qplot(value, ..density..,  data=Expression.melt, geom="density", fill=Samples, alpha=I(.5), ylab = "Density", xlab = "log10(Expression + 1)") +
          theme_bw(base_size = 30) +
          theme(panel.border = element_rect(colour = "black", size = 2))
      	ggplotly(p)
		})
      
      incProgress(3/5, detail = "MA plot")
      # MA plot
      output$MA_plot <- renderPlot({
        library(affy)
        par(cex = 2)
        exp = log2(Expression[,1:2] + 1)
        ma.plot( rowMeans(exp), (exp[, 1] - exp[, 2]), cex = 1.5, lwd = 2) 
        title(paste("MA plot:", colnames(exp)[1], "vs.", colnames(exp)[2]))
      })
      
      incProgress(4/5, detail = "Heatmap")
      # correlation heatmap 
      output$clustHeatmap <- renderPlot({

        d = 1 - cor(Expression)
        library(RColorBrewer)
        library(gplots)
        hmcol = colorRampPalette(brewer.pal(9, "GnBu"))(100)
        heatmap.2(d, trace="none", col = rev(hmcol), main="Correlation Heatmap")
      })
      
      incProgress(5/5, detail = "hclust")
      # hclust
      output$hclust <- renderPlot({
        par(cex = 2)
        #d = dist(t(log2(Expression+1)))
        d = dist(t(Expression))
        h = hclust(d)
        plot(h, xlab = "Samples")
      })

    })
  }



  ######### normalization ############
  #source("normalization.R", local=TRUE)
  # normalization Expression level  
  NORM <- reactive({
    
    normalizatedMethod <- input$Normalization
    inFile <- input$file1
    if (is.null(inFile)){
      if(input$ExampleData == 0){
          return(NULL)
        } else {
          exp_path = "data/Test.Expression.csv"
        }
      
	  } else {
      exp_path = inFile$datapath
    }
    if(normalizatedMethod == 1){
      # None
       Expression <- csvRead(exp_path)

    } else if(normalizatedMethod == 2){
      # Quantiles
       Expression <- csvRead(exp_path)
      library(preprocessCore)
      Expression.matrix = as.matrix(Expression)
      Expression.matrix = normalize.quantiles(Expression.matrix)
      colnames(Expression.matrix) = colnames(Expression)
      rownames(Expression.matrix) = rownames(Expression)
       Expression <- as.data.frame(Expression.matrix)
    } else if(normalizatedMethod == 3){
      # size factors
       Expression <- csvRead(exp_path)
      library( "DESeq" )
      sizeF = estimateSizeFactorsForMatrix(Expression)
      for (i in 1:length(sizeF)){
        Expression[,i] <-  Expression[,i] / sizeF[i]
      }
    }
    return(Expression)
  })
  


    output$box_plot <- renderPlotly({
      withProgress(message = 'Normalization:', value = 0, {
        incProgress(1/3, detail = "read data")
         Expression <- NORM()
        incProgress(2/3, detail = "normalize")
        
        p <- plots(Expression)

        

        Expression.melt = melt(log10(Expression + 1))
        colnames(Expression.melt) = c("Samples", "value")

        p = qplot(Samples, value, data=Expression.melt, geom="boxplot", fill=Samples, alpha=I(.5), xlab = " ", ylab = "log10(Expression + 1)") +
          theme_bw(base_size = 30) +
          theme(panel.border = element_rect(colour = "black", size = 2),
                legend.key.size = unit(1.2, "cm"),
              axis.text.x = element_text(angle = 45, hjust = 1))

        incProgress(3/3, detail = "plots")
        ggplotly(p)

      })
    })



  ########### DEGs calling ###########
  ### DESeq
  source("DESeq.R", local=TRUE)
   
  ### Fold Change
  source("FoldChange.R", local=TRUE)




  ## setup the copy button.
  COPY <- function(DEGs){

      output$copyAll <- renderUI({

          str <- textConnection("copy_DEGs", open = "w")
          write(rownames(DEGs), str)
          close(str)
          zeroclipButton("clipbtn", "Copy All Differentially Expressioned Genes to Clipboard", paste(copy_DEGs, collapse= "\n"), icon("clipboard"))

      })

      output$copyUp <- renderUI({

          str <- textConnection("copy_DEGs", open = "w")
          write(rownames(DEGs[DEGs$log2FoldChange > 0,]), str)
          close(str)
          zeroclipButton("clipbtn1", "Copy Up-regulated Gene List to Clipboard", paste(copy_DEGs, collapse= "\n"), icon("clipboard"))

      })

      output$copyDown <- renderUI({
          str <- textConnection("copy_DEGs", open = "w")
          write(rownames(DEGs[DEGs$log2FoldChange < 0,]), str)
          close(str)
          zeroclipButton("clipbtn2", "Copy Down-regulated Gene List to Clipboard", paste(copy_DEGs, collapse= "\n"), icon("clipboard"))

      })
  }
  

  ########## Gene Ontology Enrichment ####
  ### David ###
  source("David.R", local=TRUE)

  ### GOseq ###
  source("GOseq.R", local=TRUE)

  #####                       #####
  ############ PLOTs ##############
  #####                       #####

  ########## bar plot #############
  source("Barplot.R", local=TRUE)

  ########## gene heatmap ############
  source("heatmap.R", local=TRUE)
  
  ############# PCA ##################
  source("PCA.R", local=TRUE)

  ############# t-SNE ##################
  source("tSNE.R", local=TRUE)

})

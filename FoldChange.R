# call differentially expressed genes using Fold Change

FC_DEGs = function(Expression, condition, condition1, condition2){
      condition.t = t(condition)

      C1 = rownames(condition.t)[condition.t[,1] == condition1]
      C2 = rownames(condition.t)[condition.t[,1] == condition2]

      if (length(C1) == 1){
        C1_exp = Expression[,C1]
      } else {
        C1_exp = rowMeans(Expression[,C1])
      }
      
      if (length(C2) == 1){
        C2_exp = Expression[,C2]
      } else {
        C2_exp = rowMeans(Expression[,C2])
      }
      
      
      result = data.frame(
          row.names = rownames(Expression),
          mean = rowMeans(Expression[,c(C1, C2)]),
          C1 = C1_exp,
          C2 = C2_exp,
          foldChange = C1_exp/C2_exp,
          log2FoldChange = log2(C1_exp/C2_exp)
        )

      return(result)
    }
    
    output$FCconditionSelecter1 <- renderUI({
      selectInput("Condition1", "Choose condition1:", unique(t(EXP()$condition)[,1]))
    })
    output$FCconditionSelecter2 <- renderUI({
      condition = EXP()$condition
      selectInput("Condition2", "Choose condition2:", unique(t(condition)[,1]), selected = unique(t(condition)[,1])[2]) 
    })

    FCButtonAct <- eventReactive(input$FCButton, {
      Expression = EXP()$Expression
      condition = EXP()$condition
      FCOut <- FC_DEGs(Expression, condition, input$Condition1, input$Condition2)

      foldChange = filterFCF()
      mean = filterFCB()
      FCOut <- FCOut[FCOut$mean >= mean, ]
      DEGs <- FCOut[FCOut$mean >= mean & (FCOut$foldChange >= foldChange | FCOut$foldChange <= 1/foldChange),]

      values$DEGs = DEGs

      FC_plot(Expression, condition, FCOut, DEGs)
      COPY(DEGs)

      DT::datatable(DEGs)
    })
 

    output$FCOutput <- DT::renderDataTable({
          FCButtonAct()    
    })

    FC_plot <- function(Expression, condition, FCOut, DEGs){
      output$FC_valcano <- renderPlotly({
        if (exists("FCOut")){
            id = rownames(FCOut)
            FCOut = cbind(id, FCOut)
            id = rownames(DEGs)
            DEGs = cbind(id, DEGs)
            ggplot() + geom_point(data = FCOut, aes(x = log2FoldChange, y = log2(mean), text = paste0("Gene: ", id)),alpha = 0.5 ) +
            geom_point(data = DEGs, aes(x = log2FoldChange, y = log2(mean), text = paste0("Gene: ", id)), color='red',alpha = 0.4) +
            ggtitle("Volcano plot") +
            theme_bw(base_size = 30) +
            theme(panel.border = element_rect(colour = "black", size = 2))
          } else {
            return(NULL)
          }
      })
    }

    ## filter
    filterFCF = reactive({
        foldChange = switch(input$FCcutoff,
          "less than 0.5 or greater than 2" = 2,     # 0.5;2 FC
          "less than 1/3 or greater than 3" = 3,     # 1/3;3
          "less than 0.25 or greater than 4" = 4,    # 0.25;4
          "none" = 1                                 # all
          )
        })
    filterFCB = reactive({
        baseMean = switch(input$FCmeanCutoff,
          "1" = 1,
          "10" = 10,
          "20" = 20,
          "none" = 0
          )
        })

    # download data
    output$downloadFCOutput <- downloadHandler(
    filename = function() { 
      paste(input$file1, 'foldChange.iSeq.csv', sep='') 
    },
    content = function(file) {
      write.csv(values$DEGs, file, quote = F)
    }
    )

    








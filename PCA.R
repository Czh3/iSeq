# heatmap


  output$PCA_plot <- renderPlot({
    pca_c = PCA_C()
    pca_l = PCA_L()

    tmp = EXP()
    Expression = tmp$Expression
    condition = tmp$condition

    if(is.null(Expression)){
      return(NULL)
    }

    library(ggfortify)

    if (pca_c == "sample"){
      condition.info = data.frame(Sample = colnames(Expression))
      rownames(condition.info) = colnames(Expression)

      autoplot(prcomp(t(Expression)), data = condition.info, colour = "Sample", size=5, alpha = 0.5, label = pca_l, label.vjust = 1.8) +
      theme_bw(base_size = 30) +
      theme(panel.border = element_rect(colour = "black", size = 2),
        legend.position = PCA_G())

    } else {

      condition.info = t(condition)
      colnames(condition.info) = c("Condition")

      autoplot(prcomp(t(Expression)), data = condition.info, colour = "Condition", size=5, alpha = 0.5, label = pca_l, label.vjust = 1.8) +
      ggtitle("PCA") +
      theme_bw(base_size = 30) +
      theme(panel.border = element_rect(colour = "black", size = 2),
        legend.position = PCA_G())
    }
    
    
  })

  output$PCA.ui <- renderUI({
    plotOutput("PCA_plot", width = paste0(input$PCA_Width, "px"), height = paste0(input$PCA_Height, "px"))
  })



  PCA_C = reactive({
      switch(input$PCA_color,
          "sample" = "sample",
          "condition" = "condition"
        )
    })


  PCA_L = reactive({
      switch(input$PCA_label,
          "Show" = TRUE,
          "Hide" = FALSE
        )
    })

  PCA_G = reactive({
      switch(input$PCA_legend,
          "Show" = "right",
          "Hide" = "none"
        )
    })
  

# bar plot
  observe({
    gene = EXP()$Expression
    gene = rownames(gene)
    updateSelectizeInput(session,'Bar_Gene',
                       choices= gene,
                       server=TRUE)
    })

  stderr <- function(x) sqrt(var(x,na.rm=TRUE)/length(na.omit(x)))

  output$Bar_plot <- renderPlot({

    geneName = input$Bar_Gene

    tmp = EXP()
    Expression = tmp$Expression
    condition = tmp$condition

    if (!(geneName %in% rownames(Expression))){
      return(NULL)
    }

    gene = Expression[geneName,]
    d = merge(t(condition),t(gene),by="row.names",all.x=TRUE)

    colnames(d) = c("Sample", "Condition", "Expression")
    d$Sample = as.factor(d$Sample)

    if (Bar_G() == "sample"){
      
      p = ggplot(d, aes(fill=Sample, x = Condition, y =Expression)) +
      geom_bar(position="dodge",stat = "identity") +
      theme_bw(base_size = 30) +
      theme(panel.border = element_rect(colour = "black", size = 2),
        axis.text.x = element_text(hjust = 1, angle = 45),
		legend.position = Bar_L()) +
      ggtitle(geneName)

    } else {

      dd = aggregate(d[, 3], list(d$Condition), mean)
      colnames(dd) = c("Condition", "Expression")

      errbar = aggregate(d[, 3], list(d$Condition), stderr)
      colnames(errbar) = c("Condition", "Err")

      dd = cbind(dd, errbar$Err)
      colnames(dd) = c("Condition", "Expression", "SE")
      limits <- aes(ymax = Expression + SE, ymin=Expression - SE)

      p = ggplot(dd, aes(fill=Condition, x = Condition, y =Expression)) +
      geom_bar(position="dodge",stat = "identity") +
      geom_errorbar(limits, position="dodge", width=0.25) +
      theme_bw(base_size = 30) +
      theme(panel.border = element_rect(colour = "black", size = 2),
        axis.text.x = element_text(hjust = 1, angle = 45),
		legend.position = Bar_L()) +
      ggtitle(geneName)

    }
    p
    
  })

  output$Bar.ui <- renderUI({
    plotOutput("Bar_plot", width = Bar_w(), height = Bar_h())
  })



  Bar_G = reactive({
      switch(input$Bar_group,
          "sample" = "sample",
          "condition" = "condition"
        )
    })

  Bar_L = reactive({
		switch(input$Bar_legend,
		"Show" = "right",
		"Hide" = "none")
  })

  Bar_w = reactive({
    paste0(input$Bar_Width, "px")
    })

  Bar_h = reactive({
     paste0(input$Bar_Height, "px")
    })


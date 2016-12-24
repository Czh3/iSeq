# Gene Ontolgoy Enrichment with GOseq

    GOseq <- function(species, geneID, p, ud){
      withProgress(message = 'GOseq:', value = 0, {

        incProgress(1/5, detail = "Loading data")
        library(goseq)
        if (species == "mm9")
        {
          library(org.Mm.eg.db, lib.loc = "~/tools/")

        } else if (species == "hg19")
        {
          library(org.Hs.eg.db)
        }

        if (is.null(values$DEGs))
        {
          return(as.matrix(c("Error!", "Please call DEGs first!")))
        }

        DEGs = values$DEGs

        if (identical(ud, "Up-regulated")){
          DEG = DEGs[DEGs$log2FoldChange >= 0,]
          DEG = rownames(DEG)
        } else if (identical(ud, "Down-regulated" )){
          DEG = DEGs[DEGs$log2FoldChange < 0,]
          DEG = rownames(DEG)
        } else {
          DEG = rownames(DEGs)
        }

        incProgress(2/5, detail = "Geting data")
        allgenes = rownames(EXP()$Expression)

        genes = as.integer(allgenes %in% DEG)
        names(genes) = allgenes

        incProgress(3/5, detail = "normalize gene length")
        # normalize gene length
        pwf = nullp(genes, species, geneID, plot.fit = FALSE)

        incProgress(4/5, detail = "Runing GOseq")
        # run GOseq
        GO.wall = goseq(pwf, species, geneID)

        # get siginaficant terms
        GO.wall.sig = GO.wall[GO.wall$over_represented_pvalue < p,]

        incProgress(5/5, detail = "Down")
        if (nrow(GO.wall.sig) == 0){
            a = data.frame("There", "is", "0", "siginaficant", "terms")
            colnames(a) = c("There", "is", "0", "siginaficant", "terms")
            a = a[-1,]
            return(a)
          } else  {
            return(GO.wall.sig)
          }
      })  
    }


    GO_ButtonACT <- eventReactive(input$GO_Button, {
          species = GO_s()
          p = GO_p()
          geneID = GO_g()
          GO.result <- GOseq(species, geneID, p, GO_u())

          values$GO.result = GO.result

          #plot
          GO_p_f(GO.result)

          DT::datatable(GO.result)
    })
 

    output$GO_Output <- DT::renderDataTable({
          GO_ButtonACT()
          #DT::datatable(GO.result)

    })

    # opt
    GO_s = reactive({
        GO_species = switch(input$GO_species,
          "Human" = "hg19",     
          "Mouse" = "mm9" 
          )
        })

    GO_p = reactive({
        GO_pval = switch(input$GO_pval,
          "10^-2" = 0.01,
          "10^-3" = 0.001,
          "10^-4" = 0.0001,
          "none" = 1
          )
        })

    GO_g = reactive({
        GO_geneID = switch(input$GO_geneID,
          "GeneSymbol" = "geneSymbol",
          "ENSEMBL GENE ID" = "ensGene",
          "REFSEQ GENE ID" = "refGene"
          )
        })

    GO_u = reactive({
        input$GO_updown
      })

    # download data
    output$downloadGOOutput <- downloadHandler(
    filename = function() { 
      paste(input$file1, '.GOseq.csv', sep='') 
    },
    content = function(file) {
      write.csv(values$GO.result , file, quote = F)
    }
    )

    ## GO plot
    GO_p_f <- function(GO.result){
      output$GO_plot <- renderPlot({
        if (is.null(GO.result)){
          return(NULL)
        }

        if (nrow(GO.result) == 0){
          return(as.matrix(c("No Enrichment Term!")))
        }

        goSize = -log10(GO.result$over_represented_pvalue)
        for(i in 1:length(goSize)){
          if(goSize[i] > 5){
            goSize[i] = 5
          } else if(goSize[i] < 3){
            goSize[i] = 3
          }
        }
        goSize = goSize - 2

        p <- ggplot(GO.result, aes(numDEInCat/numInCat, -log10(over_represented_pvalue)))
        p + geom_point(aes(size = log2(numDEInCat) , shape = ontology, color = ontology), alpha = 0.5) + 
          geom_text(aes(label=category, size=goSize), alpha = 0.4) +
          theme_bw(base_size = 20) +
          theme(panel.border = element_rect(colour = "black", size = 1.5))
        })

      output$GO_bar <- renderPlot({
          if (nrow(GO.result) == 0){
            return(NULL)
          }

          tmp = GO.result
          #tmp$category = paste(tmp$category, tmp$term, sep = "|")
          tmp$category = factor(tmp$category, levels=tmp[order(tmp$over_represented_pvalue,decreasing = T), "category"])
          ggplot(tmp[1:15,], aes(x = category,y = -log10(over_represented_pvalue), fill = term)) +
            geom_bar(stat = "identity") + 
            coord_flip() +
            theme_classic() +
            theme(legend.position = "none",
                  axis.text = element_text(size = rel(1.5)))

        })
    }

    







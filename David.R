

######## David Gene Ontology ########

Run_david <- function(species, geneID, ud) {
	geneID = geneID

	withProgress(message = 'David:', value = 0, {

		if (is.null(values$DEGs))
        {
        	return(as.matrix(c("Error!", "Please call DEGs first!")))
        }

        DEGs = values$DEGs

		if (nrow(DEGs) > 3000){
        	DEGs = DEGs[1:3000,]
        }

		incProgress(1/5, detail = "Loading data")

		if (identical(ud, "Up-regulated")){
          DEGs = DEGs[DEGs$log2FoldChange >= 0,]
          DEG <- rownames(DEGs)
        } else if (identical(ud, "Down-regulated" )){
          DEGs = DEGs[DEGs$log2FoldChange < 0,]
          DEG <- rownames(DEGs)
        } else {
          DEG <- rownames(DEGs)
        }

		if (species == "Human"){

			if (geneID == "GENE_SYMBOL"){
				library("org.Hs.eg.db")
				DEG <- select(org.Hs.eg.db, DEG, "ENTREZID", "SYMBOL")[,2]
				geneID = "ENTREZ_GENE_ID"
			}
		}
		else if (species == "Mouse"){

			if (geneID == "GENE_SYMBOL"){
				library(org.Mm.eg.db)
				DEG <- select(org.Mm.eg.db, DEG, "ENTREZID", "SYMBOL")[,2]
				geneID = "ENTREZ_GENE_ID"
			}
		} 

		
		ID = DEG
		DEGs = cbind(ID, DEGs)




        incProgress(2/5, detail = "Connect David Server")
        library("RDAVIDWebService")
        david = DAVIDWebService(email="zhangchao3@pku.edu.cn", url="https://david.ncifcrf.gov/webservice/services/DAVIDWebService.DAVIDWebServiceHttpSoap12Endpoint/")


        incProgress(2/5, detail = "Submit to David Server")
        result = addList(david, DEG, idType=geneID, listName="iSeq_list", listType="Gene")

        incProgress(3/5, detail = "Annotate Genes")
        setAnnotationCategories(david, c("GOTERM_BP_FAT","GOTERM_MF_FAT", "GOTERM_CC_FAT"))

        GO_Enrich = getFunctionalAnnotationChart(david)

        if(is.null(GO_Enrich)){
        	return("No Enrichment GO Terms")
        }

        if(nrow(GO_Enrich) < 10){
        	return(GO_Enrich[,c(1,2,3,10,5)])
        }
        
        Enrich_Terms = GO_Enrich

        colnames(Enrich_Terms)[12] = "adj_pval"
        

        Enrich_Terms[,1] = sub("GOTERM_CC_FAT", "CC", Enrich_Terms[,1])
        Enrich_Terms[,1] = sub("GOTERM_MF_FAT", "MF", Enrich_Terms[,1])
        Enrich_Terms[,1] = sub("GOTERM_BP_FAT", "BP", Enrich_Terms[,1])



        temp = strsplit(Enrich_Terms[,2], "~")
        a = matrix(unlist(temp), ncol=2, byrow=TRUE)
        colnames(a) = c("ID", "Term")
        Enrich_Terms = cbind(Enrich_Terms[,-2], a)

        #GOprocess = head(Enrich_Terms[Enrich_Terms$Category == "BP", "Term"], n=3L)
        #GOgenes = Enrich_Terms[Enrich_Terms$Term %in% GOprocess, "Genes"]

        

        a = colnames(DEGs)
        colnames(DEGs) = sub("log2FoldChange", "logFC", a)


        #return(DEGs)
		incProgress(4/5, detail = "Plot : GOPlot") 

		if(ncol(DEGs) != 6){

		library(GOplot)
		circ <- circle_dat(as.data.frame(Enrich_Terms), as.data.frame(DEGs))

		output$GOBubble <- renderPlot({
			a = circ[,c(2,7)]
			a.u = unique(a)
			if (nrow(a.u) >= 20){
				cutoff = -log10(sort(a.u[,2])[20])
			}
			if (cutoff < 3){
				cutoff = 3
			}

			GOBubble(circ, labels = cutoff)
			}) 	
		} else {
			# FoldChange does not have a p-value, so skip ploting
		}
		
		


		incProgress(5/5, detail = "Finished")
		#return the enrichment table
		#return(GO_Enrich[,c(1,2,3,4,10,5)])
		return(Enrich_Terms[,c(1,13,14,2,9,4)])
		})
	
}




David_ButtonACT <- eventReactive(input$David_run, {
	species = GO_species()
	if (species == "Other"){
		Go_id = GO_ID()
	} else {
		Go_id = GO_ID1()
	}


	David.result = tryCatch({
			David.result = Run_david(species, Go_id, GO_ud())
			return(David.result)
		}, warning = function(war){
			return(David.result)
		}, error = function(err) {
			return(as.matrix(c("Error!","Select The Right Species and Identifier!")))
		}, finally = {

		}
	)
	

	DT::datatable(David.result)
	})

output$David_GO_output <- DT::renderDataTable({
    David_ButtonACT()
    })



GO_species = reactive({
	input$David_species
	})

GO_ID = reactive({
	input$David_ID
	})

GO_ID1 = reactive({
	input$David_ID1
	})

GO_ud = reactive({
    input$David_updown
    })




######## David Pathway ########


Run_david_pathway <- function(species, geneID, ud) {
	geneID = geneID

	withProgress(message = 'David:', value = 0, {

		if (is.null(values$DEGs))
        {
        	return(as.matrix(c("Error!", "Please call DEGs first!")))
        }

        DEGs = values$DEGs

		if (nrow(DEGs) > 3000){
        	DEGs = DEGs[1:3000,]
        }

		incProgress(1/4, detail = "Loading data")

		if (identical(ud, "Up-regulated")){
          DEGs = DEGs[DEGs$log2FoldChange >= 0,]
          DEG <- rownames(DEGs)
        } else if (identical(ud, "Down-regulated" )){
          DEGs = DEGs[DEGs$log2FoldChange < 0,]
          DEG <- rownames(DEGs)
        } else {
          DEG <- rownames(DEGs)
        }

		if (species == "Human"){

			if (geneID == "GENE_SYMBOL"){
				library("org.Hs.eg.db")
				DEG <- select(org.Hs.eg.db, DEG, "ENTREZID", "SYMBOL")[,2]
				geneID = "ENTREZ_GENE_ID"
			}
		}
		else if (species == "Mouse"){

			if (geneID == "GENE_SYMBOL"){
				library(org.Mm.eg.db)
				DEG <- select(org.Mm.eg.db, DEG, "ENTREZID", "SYMBOL")[,2]
				geneID = "ENTREZ_GENE_ID"
			}
		} 

		ID = DEG
		DEGs = cbind(ID, DEGs)


        incProgress(2/4, detail = "Connect David Server")
        library("RDAVIDWebService")
        david = DAVIDWebService(email="zhangchao3@pku.edu.cn", url="https://david.ncifcrf.gov/webservice/services/DAVIDWebService.DAVIDWebServiceHttpSoap12Endpoint/")


        incProgress(2/4, detail = "Submit to David Server")
        result = addList(david, DEG, idType=geneID, listName="iSeq_list", listType="Gene")

        incProgress(3/4, detail = "Annotate Genes")
        setAnnotationCategories(david, "KEGG_PATHWAY")

        GO_Enrich = getFunctionalAnnotationChart(david)


        if(is.null(GO_Enrich)){
        	return("No Enrichment Pathway Terms")
        }


		incProgress(4/4, detail = "Finished")
		#return the enrichment table
		return(GO_Enrich[,c(1,2,3,10,5)])

		})
	
}



David_Pathway_ButtonACT <- eventReactive(input$David_pathway_run, {
	species = GO_species_P()
	if (species == "Other"){
		Go_id = GO_ID_P()
	} else {
		Go_id = GO_ID_P1()
	}

	David.result = tryCatch({
			David.result = Run_david_pathway(species, Go_id, GO_ud_P())
			return(David.result)
		}, warning = function(war){
			return(David.result)
		}, error = function(err) {
			return(as.matrix(c("Error!","Select The Right Species and Identifier!", "Or, No enrichment term.")))
		}, finally = {

		}
	)

	DT::datatable(David.result)
	})

output$David_Pathway_output <- DT::renderDataTable({
    David_Pathway_ButtonACT()
    })



GO_species_P = reactive({
	input$David_species_pathway
	})

GO_ID_P = reactive({
	input$David_ID_P
	})

GO_ID_P1 = reactive({
	input$David_ID_P1
	})

GO_ud_P = reactive({
    input$David_pathway_updown
    })















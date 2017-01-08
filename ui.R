library(markdown)
library(shinyBS)
library(shiny)
#library(RDAVIDWebService)
library(plotly)

shinyUI(navbarPage("iSeq : A web-based server for RNA-seq Data Analysis and Visualization", 
                   tabPanel("Upload", icon = icon("cloud-upload"),
                            tags$head(tags$link(rel="stylesheet",type="text/css", href="iSeq.css")),
                            sidebarLayout(
                              sidebarPanel(
                                
                                ## upload Expression file
                                HTML('<div class="form-group shiny-input-container">
                                       <label>*Choose An Expression File of Genes(.csv)</label>
                                       <a class="helpInput" href="help.input.Expression.html" target="_blank">
                                       <img src="help.jpg" height="20" width="20"/>
                                       </a>
                                       <input id="file1" name="file1" type="file" accept="text/csv,.csv"/>
                                       <div id="file1_progress" class="progress progress-striped active shiny-file-input-progress">
                                       <div class="progress-bar"></div>
                                       </div>
                                       </div>'),
                                
                                
                                #tags$a(img(src="help.png", height=20, width=20),class="helpInput",href="help.input.Expression.html",target="_blank"),
                                #fileInput('file1', '*Choose a Genes Expression File',
                                #          accept=c('text/csv', 
                                #                   'text/comma-separated-values,text/plain', 
                                #                   '.csv,.tsv,.txt,.xls')),
                                tags$hr(),
                                
                                # upload condition file
                                HTML('<div class="form-group shiny-input-container">
                                       <label>Choose a Condition File of Samples(.csv)</label>
                                     <a class="helpInput" href="help.input.condition.html" target="_blank">
                                     <img src="help.jpg" height="20" width="20"/>
                                     </a>
                                     <input id="file2" name="file2" type="file" accept="text/csv,.csv"/>
                                     <div id="file2_progress" class="progress progress-striped active shiny-file-input-progress">
                                     <div class="progress-bar"></div>
                                     </div>
                                     </div>'),
                                
                                #fileInput('file2', 'Choose A Samples\' Condition File',
                                #          accept=c('text/csv', 
                                #                   'text/comma-separated-values,text/plain', 
                                #                   '.csv,.tsv,.txt,.xls')),
                               
                                #radioButtons('sep', 'File Separator',
                                #             c(Comma=',',
                                #               Semicolon=';',
                                #               Tab='\t'),
                                #               ',')
                                            
								                actionButton("ExampleData", "Example Data")
                                
                              ),
                              mainPanel(
                                
                                bsAlert("alert"),
                                bsAlert("alert1"),
                                bsAlert("alert2"),
                                bsAlert("alert3"),
                                
                                conditionalPanel("output.fileUploaded1",
                                                  tags$h4(tags$b("iSeq ", id = "firstLetter")," is a websever to help you analyse RNA sequencing data and plot publishable figures."),
                                                  tags$h4("Upload your genes Expression data first !"),
                                                  tags$hr(),
                                                  HTML('<img src="examples.png", width="100%"/>'),
                                                  tags$hr(),
                                                  HTML("
                                                      <a href='http://www.pku.edu.cn/'><img src='pku_logo.png', width='15%'/></a> &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; 
                                                      <a href='http://www.cls.edu.cn/'><img src='cls_logo.png', width='15%'/></a> &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;
                                                      <a href='http://forum.cbi.pku.edu.cn/forum.php'><img src='forum_logo.png', width='15%' /></a> &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;
                                                      <a href='http://bioinfocore.cbi.pku.edu.cn/'><img src='Core_logo.png', width='20%' /></a>
                                                    "),
                                                  tags$h5("iSeq @ Bioinformatics core facility, Center for Life Science, Peking University")                          
                                                 ),
                                conditionalPanel("output.fileUploaded",
                                  h3(tags$blockquote("Condition:")),
                                  tableOutput("condition"),
                                  h3(tags$blockquote("Expression:")),
                                  DT::dataTableOutput("contents"),
                                  downloadButton("downloadExpression", "Download the data.")
                                )
                            	)
                            )
                   ),
                   #tabPanel("Quality Check", icon = icon("gears"),
                   #         conditionalPanel("output.fileUploaded1",
                   #                          tags$h1("Upload Your Data First !")
                   #                          ),
                   #         conditionalPanel("output.fileUploaded",
                   #                  tags$h3("Summary:"),
                   #                  verbatimTextOutput("summary"),
                   #                  tags$hr(),
                   #                  tags$h3("Expression Distribution:"),
                   #                  #plotOutput(outputId = "distribution", height = "700px", width = "800px"),
                   #                  plotOutput(outputId = "distribution",height = "700px"),
                   #                  tags$hr(),
                   #                 #tags$h3("PCA:"),
                   #                  #plotOutput(outputId = "PCA_plot", height = "700px", width = "800px"),
                   #                  #plotOutput(outputId = "PCA_plot", height = "700px"),
                   #                  #tags$hr(),
                   #                  tags$h3("hclust"),
                   #                  #plotOutput(outputId = "hclust", height = "600px",width = "800px"),
                   #                  plotOutput(outputId = "hclust", height = "600px"),
                   #                  tags$hr(),
                   #                  tags$h3("Clust heatmap"),
                   #                  #plotOutput(outputId = "clustHeatmap", height = "600px", width = "700px")
                   #                  plotOutput(outputId = "clustHeatmap", height = "600px")
                   #         )
                   #),
					tabPanel("Normalization", icon = icon("server"),
							sidebarLayout(
	                            sidebarPanel(
									            ## normalization methods
	                                selectInput("Normalization", "Normalization", 
	                                            #choices = list("None" = 1, "Quantiles" = 2, "size factor" = 3, "Given Genes" = 4), 
                                              choices = list("None" = 1, "Quantile" = 2, "Size factor" = 3), 
	                                            selected = 1),
	                                
	                                conditionalPanel(condition = "input.Normalization == 4",
                                              tags$hr(),
	                                            textInput("GuideGenes", label = "Genes chosen for normalization", value = "Enter Genes...")
	                            	  ),

                                  actionButton("showAll", "Show All")
                            	),
                            mainPanel(
                            	conditionalPanel("output.fileUploaded1",
                                             tags$h1("Upload Your Data First !")
                                             ),
                            	conditionalPanel("output.fileUploaded", 

                                         tags$blockquote(tags$em("Normalization is a key step in RNA-seq data analysis.")),
                                         tags$br(),
                                         tags$h3("Box plot:"),
                                         #plotOutput(outputId = "box_plot", height = "600px", width = "700px"),
                                         plotlyOutput(outputId = "box_plot", height = "600px"),
                                         
                                         tags$hr(),

                                         tags$h3("hclust"),
                                         plotOutput(outputId = "hclust", height = "600px"),
                                         
                                         
                                         conditionalPanel("input.showAll % 2 == 1",
                                           tags$hr(),
                                           tags$h3("Expression Distribution:"),
                                           plotlyOutput(outputId = "distribution",height = "700px"),
                                           tags$hr(),

                                           tags$h3("Clust heatmap"),
                                           plotOutput(outputId = "clustHeatmap", height = "600px"),

                                           tags$h3("MA plot: (Just using the first 2 samples)"),
                                           #plotOutput(outputId = "MA_plot", height = "700px", width = "700px"),
                                           plotOutput(outputId = "MA_plot", height = "700px")
                                          )
                                       )
                            	)
                            )

						),
                   
                   navbarMenu("DEG calling", icon = icon("align-left"),
                              tabPanel("DESeq", icon = icon("arrow-right"),
                                       sidebarLayout(
                                         position = "left",
                                         sidebarPanel(
                                           conditionalPanel("output.fileUploaded1",
                                              tags$h1("Upload Your Data First !")
                                           ),
                                           conditionalPanel("output.fileUploaded",
                                               uiOutput("conditionSelecter1"),
                                               uiOutput("conditionSelecter2")
                                           ),
                                             
                                             tags$hr(),
                                             tags$h4("Optional filter:"),
                                             selectInput("DESeqPadjCutoff", "Adjusted p-value cutoff", 
                                                         choices = list("0.01", "0.05", "0.1", "none"), 
                                                         selected = "0.05"),
                                             selectInput("DESeqfoldChangeCutoff", "Fold Change cutoff", 
                                                         choices = list("less than 2/3 or greater than 1.5", "less than 0.5 or greater than 2", "less than 1/3 or greater than 3", "less than 0.25 or greater than 4", "none"), 
                                                         selected = "less than 0.5 or greater than 2"),
                                             selectInput("DESeqbaseMeanCutoff", "Mean Expression cutoff", 
                                                         choices = list("1", "10", "20", "30","none"), 
                                                         selected = "1"),
                                             actionButton("DEseqButton", "Run to detect"),
                                             downloadButton('downloadDESeqOutput', 'Download Table')
                                             ),
                                         
                                           mainPanel(
                                             tags$blockquote(tags$em("Detect differentially expressed genes using DESeq. ")),
                                             #tags$br(),
                                             tags$em(tags$h6("Anders S and Huber W (2010). “Differential expression analysis for sequence count data.” Genome Biology, 11, pp. R106")),
                                             DT::dataTableOutput("DESeqOutput"),
                                             tags$br(),tags$br(),tags$br(),
                                             plotlyOutput(outputId = "DESeq_valcano", height = "700px", width="1200px")
                                             
                                           )
                                       )
                              ),
                              
                              tabPanel("Fold Change", icon = icon("arrow-right"),
                                       sidebarLayout(
                                         position = "left",
                                         sidebarPanel(
                                           conditionalPanel("output.fileUploaded1",
                                                            tags$h1("Upload Your Data First !")
                                           ),
                                           conditionalPanel("output.fileUploaded",
                                                            uiOutput("FCconditionSelecter1"),
                                                            uiOutput("FCconditionSelecter2")
                                           ),
                                           
                                           tags$hr(),
                                           selectInput("FCcutoff", "Fold Change cutoff", 
                                                       choices = list("less than 0.5 or greater than 2", "less than 1/3 or greater than 3", "less than 0.25 or greater than 4", "none"), 
                                                       selected = "less than 1/3 or greater than 3"),
                                           selectInput("FCmeanCutoff", "mean Expression cutoff", 
                                                       choices = list("1", "10", "20", "none"), 
                                                       selected = "1"),
                                           actionButton("FCButton", "Run to detect"),
                                           downloadButton('downloadFCOutput', 'Download Table')
                                         ),
                                         
                                         mainPanel(
                                           tags$blockquote(tags$em("Detect differentially expresed genes using expression fold change.")),
                                           DT::dataTableOutput("FCOutput"),
                                           tags$br(),tags$br(),tags$br(),
                                           plotlyOutput(outputId = "FC_valcano", height = "600px", width = "1200px")

                                         )
                                       )
                              )
                              #tabPanel("T test"),
                              #tabPanel("Fold Change")
                   ),
                  navbarMenu("Function", icon = icon("bullseye"),
                      #tabPanel(HTML("<a href='https://david.ncifcrf.gov/tools.jsp', target='_blank'>DAVID</a>")), 
                      tabPanel("Online Servers", 
                          sidebarLayout(
                              position = "left",
                              sidebarPanel(
                          
                                 # Control the styling of the button. This is not necessary but will make the
                                 # button feel more natural, with the flash object on top:
                                 tags$head(tags$style(
                                  '
                                 .zeroclipboard-is-hover { background-color: steelblue; }
                                 .zeroclipboard-is-active { background-color: red; }
                                  '
                                  )),

                                  # The UI placeholder for the copy button
                                  uiOutput("copyAll"),
                                  uiOutput("copyUp"),
                                  uiOutput("copyDown"),
                                  tags$br(),tags$br()
                              ),

                          mainPanel(
                            h3("Several highly recommended online gene functional enrichment tools are list below, you can copy the DEGs to clipboard and submit them to these tools."),
                            tags$blockquote(h2(a(href="https://david.ncifcrf.gov/", target="_blank", "David"))),
                            h4("DAVID provides a comprehensive set of functional annotation tools for investigators to understand biological meaning behind large list of genes."),
                            br(),
                            h6(tags$em("Huang DW, Sherman BT, Lempicki RA. Systematic and integrative analysis of large gene lists using DAVID Bioinformatics Resources. Nature Protoc. 2009;4(1):44-57.")),
                            h6(tags$em("Huang DW, Sherman BT, Lempicki RA. Bioinformatics enrichment tools: paths toward the comprehensive functional analysis of large gene lists. Nucleic Acids Res. 2009;37(1):1-13.")),
                            br(),br(),

                            tags$blockquote(h2(a(href="http://amp.pharm.mssm.edu/Enrichr/",target="_blank","Enrichr"))),
                            h4("Enrichr is an integrative web-based and mobile software application that includes new gene-set libraries, an alternative approach to rank enriched terms, and various interactive visualization approaches to display enrichment results using the JavaScript library, Data Driven Documents (D3). The software can also be embedded into any tool that performs gene list analysis"),
                            br(),
                            h6(tags$em("Chen EY, Tan CM, Kou Y, Duan Q, Wang Z, Meirelles GV, Clark NR, Ma'ayan A. Enrichr: interactive and collaborative HTML5 gene list enrichment analysis tool. BMC Bioinformatics. 2013;128(14).")),
                            h6(tags$em("Kuleshov MV, Jones MR, Rouillard AD, Fernandez NF, Duan Q, Wang Z, Koplev S, Jenkins SL, Jagodnik KM, Lachmann A, McDermott MG, Monteiro CD, Gundersen GW, Ma'ayan A. Enrichr: a comprehensive gene set enrichment analysis web server 2016 update. Nucleic Acids Research. 2016; gkw377."))
                            )

                          )

                      ),

                      tabPanel( "GeneOntology(David)",
                          sidebarLayout(
                               position = "left",
                               sidebarPanel(
                                  selectInput("David_species", "Species", 
                                                         choices = list("Human", "Mouse", "Other"), 
                                                         selected = "Human"),
                                  conditionalPanel(condition = "input.David_species == 'Other'",
                                      selectInput("David_ID", "Select Identifier",
                                              choices = list("ENSEMBL_GENE_ID", "ENTREZ_GENE_ID", "FLYBASE_GENE_ID", "MGI_ID", "REFSEQ_GENOMIC", "RGD_ID", "SGD_ID", "TAIR_ID",
                                                "UCSC_GENE_ID", "UNIGENE", "WORMBASE_GENE_ID","WORMPEP_ID", "ZFIN_ID"),
                                              selected = "ENTREZ_GENE_ID")
                                    ),
                                  conditionalPanel(condition = "input.David_species != 'Other'",
                                      selectInput("David_ID1", "Select Identifier",
                                              choices = list("GENE_SYMBOL", "ENSEMBL_GENE_ID", "ENTREZ_GENE_ID", "FLYBASE_GENE_ID", "MGI_ID", "REFSEQ_GENOMIC", "RGD_ID", "SGD_ID", "TAIR_ID",
                                                "UCSC_GENE_ID", "UNIGENE", "WORMBASE_GENE_ID","WORMPEP_ID", "ZFIN_ID"),
                                              selected = "GENE_SYMBOL")
                                    ),

                                  
                                  checkboxGroupInput("David_updown", label = "DEGs to use:", 
                                                        choices = list("Up-regulated" , "Down-regulated" ),
                                                        selected = c("Up-regulated" , "Down-regulated" )),
                                  actionButton("David_run", "Run !")
                                ),
                               mainPanel(
                                  tags$blockquote(tags$em("Gene Ontology enrichment using DAVID webserver")),
                                  tags$em(tags$h6("Huang DW, Sherman BT, Lempicki RA. Systematic and integrative analysis of large gene lists using DAVID Bioinformatics Resources. Nature Protoc. 2009;4(1):44-57.")),
                                  tags$em(tags$h6("Huang DW, Sherman BT, Lempicki RA. Bioinformatics enrichment tools: paths toward the comprehensive functional analysis of large gene lists. Nucleic Acids Res. 2009;37(1):1-13.")),
                                  DT::dataTableOutput("David_GO_output"),
                                  tags$br(),tags$br(),
                                  plotOutput(outputId = "GOBubble", height = "800px")
                                  #plotOutput(outputId = "GOCir", height = "800px")
                                )
                            )

                        ), 
                        

                        tabPanel( "Pathway(David)",
                          sidebarLayout(
                               position = "left",
                               sidebarPanel(
                                  selectInput("David_species_pathway", "Specise", 
                                                         choices = list("Human", "Mouse", "Other"), 
                                                         selected = "Human"),
                                  conditionalPanel(condition = "input.David_species_pathway == 'Other'",
                                      selectInput("David_ID_P", "Select Identifier",
                                              choices = list("ENSEMBL_GENE_ID", "ENTREZ_GENE_ID", "FLYBASE_GENE_ID", "MGI_ID", "REFSEQ_GENOMIC", "RGD_ID", "SGD_ID", "TAIR_ID",
                                                "UCSC_GENE_ID", "UNIGENE", "WORMBASE_GENE_ID","WORMPEP_ID", "ZFIN_ID"),
                                              selected = "ENTREZ_GENE_ID")
                                    ),
                                  conditionalPanel(condition = "input.David_species_pathway != 'Other'",
                                      selectInput("David_ID_P1", "Select Identifier",
                                              choices = list("GENE_SYMBOL", "ENSEMBL_GENE_ID", "ENTREZ_GENE_ID", "FLYBASE_GENE_ID", "MGI_ID", "REFSEQ_GENOMIC", "RGD_ID", "SGD_ID", "TAIR_ID",
                                                "UCSC_GENE_ID", "UNIGENE", "WORMBASE_GENE_ID","WORMPEP_ID", "ZFIN_ID"),
                                              selected = "GENE_SYMBOL")
                                    ),

                                  
                                  checkboxGroupInput("David_pathway_updown", label = "DEGs to use:", 
                                                        choices = list("Up-regulated" , "Down-regulated" ),
                                                        selected = c("Up-regulated" , "Down-regulated" )),
                                  actionButton("David_pathway_run", "Run !")
                                ),
                               mainPanel(
                                  tags$blockquote(tags$em("KEGG pathway enrichment using DAVID webserver")),
                                  tags$em(tags$h6("Huang DW, Sherman BT, Lempicki RA. Systematic and integrative analysis of large gene lists using DAVID Bioinformatics Resources. Nature Protoc. 2009;4(1):44-57.")),
                                  tags$em(tags$h6("Huang DW, Sherman BT, Lempicki RA. Bioinformatics enrichment tools: paths toward the comprehensive functional analysis of large gene lists. Nucleic Acids Res. 2009;37(1):1-13.")),
                                  
                                  DT::dataTableOutput("David_Pathway_output"),
                                  tags$br(),tags$br()
                                  #plotOutput(outputId = "Pathway", height = "800px")
                                )
                            )

                        ), 
                      tabPanel("GeneOntology(GOseq)", 
                              sidebarLayout(
                                           position = "left",
                                            sidebarPanel(
                                             #tags$h4("Genes chosen for Gene Ontology Enrichment"),
                                             #tags$textarea(id="GO_Genes", rows=10, cols=40, "Paste a gene list, one gene per line..."),
                                             
                                             selectInput("GO_species", "Specise", 
                                                         choices = list("Human", "Mouse"), 
                                                         selected = "Human"),

                                             selectInput("GO_geneID", "Gene Identifier", 
                                                         choices = list("GeneSymbol", "ENSEMBL GENE ID", "REFSEQ GENE ID"), 
                                                         selected = "GeneSymbol"),

                                             #selectInput("GO_pval", "p-value cutoff", 
                                             #            choices = list("10^-2", "10^-3", "10^-4", "none"), 
                                             #            selected = "10^-2"),

                                             checkboxGroupInput("GO_updown", label = "DEGs to use:", 
                                                        choices = list("Up-regulated" , "Down-regulated" ),
                                                        selected = c("Up-regulated" , "Down-regulated" )),

                                             actionButton("GO_Button", "Run !"),
                                             downloadButton('downloadGOOutput', 'Download Table')
                                           ),
                                           
                                           mainPanel(
                                            tags$blockquote(tags$em("Gene Ontology enrichment using GOseq.")),
                                            tags$em(tags$h6("Young MD, Wakefield MJ, Smyth GK and Oshlack A (2010). “Gene ontology analysis for RNA-seq: accounting for selection bias.” Genome Biology, 11, pp. R14")),
                                             DT::dataTableOutput("GO_Output"),
                                             tags$br(),
                                             #tags$hr(),
                                             plotOutput(outputId = "GO_plot", height = "600px"),
                                             tags$br(),
                                             #tags$hr(),
                                             plotOutput(outputId = "GO_bar", height = "600px")
                                           )
                                         )
                        ) 
                        
                    ),

					          navbarMenu("Plots", icon = icon("barcode"),
                      tabPanel("Bar plot", icon = icon("bar-chart"),
                              sidebarLayout(
                                position = "left",
                                sidebarPanel(
                                  tags$h3("Gene Expression bar plot:"),

                                  #tags$textarea(id="Bar_Gene", rows=1, cols=30, "Paste a gene to plot"),
                                  selectizeInput("Bar_Gene",
                                      label="Gene Name:",
                                      choices = NULL,
                                      multiple=FALSE,
                                      options = list(
                                        placeholder = 
                                          'Start typing to search for a gene name'
                                      )
                                  ),

                                  tags$hr(),
                                  radioButtons("Bar_group", label = "Group by:", 
                                    choices = list("sample", "condition"),
                                    selected = "sample"),
								  
								  radioButtons("Bar_legend", label = "Legend:",
                                    choices = list("Hide", "Show"),
                                    selected = "Show"),


                                  sliderInput("Bar_Width", "Width:", min=500, max=1500, value=800),
                                  sliderInput("Bar_Height", "Height:", min=300, max=1200, value=600)
                                ),
                                mainPanel(
                                  conditionalPanel("output.fileUploaded", 
                                                   uiOutput("Bar.ui")
                                                   #plotlyOutput("Bar_plot",height=600)
                                  )
                                )
                              )
                     ),
					           tabPanel("Heatmap", icon = icon("th"),
					                    sidebarLayout(
					                      position = "left",
					                      sidebarPanel(
					                        #textInput("Genes", label = "Genes chosen for plot heatmap", value = "Paste a gene list, one gene per line...")
					                        tags$h4("Gene List (DEGs by default):"),
					                        tags$textarea(id="Genes", rows=10, cols=30, "Paste a gene list, one gene per line..."),
					                        br(),
                                  tags$hr(),
                                  radioButtons("HeatmapRadio", label = "Scale by:",
                                    choices = list("row", "column", "none"), 
                                    selected = "none"),
                                  
                                  checkboxGroupInput("HeatmapGroup", label = "Clust by:", 
                                    choices = list("column", "row"),
                                    selected = c("column", "row")),

                                  radioButtons("HeatmapRownames", label = "Gene name:",
                                    choices = list("Show", "Hide"), 
                                    selected = "Hide"),

					                        sliderInput("heatmapWidth", "Width:", min=400, max=1500, value=800),
					                        sliderInput("heatmapHeight", "Height:", min=400, max=1500, value=700)
					                        #actionButton("plotButton", "Plot!")
					                      ),
					                      mainPanel(
					                        conditionalPanel("output.fileUploaded", 
					                                         tags$h3("Heatmap:"),
					                                         uiOutput("plot.ui")
                                                   #verbatimTextOutput("heatmapGenes")
					                        )
                                )
					                    )
					           ),
                     tabPanel("PCA", icon = icon("th-large"),
                              sidebarLayout(
                                position = "left",
                                sidebarPanel(
                                  tags$h3("Principal Components Analysis:"),
                                  tags$hr(),
                                  radioButtons("PCA_color", label = "Colored by:", 
                                    choices = list("sample", "condition"),
                                    selected = "condition"),

                                  radioButtons("PCA_label", label = "Sample labels:",
                                    choices = list("Hide", "Show"), 
                                    selected = "Hide"),

                                  radioButtons("PCA_legend", label = "Legend:",
                                    choices = list("Hide", "Show"), 
                                    selected = "Show"),

                                  sliderInput("PCA_Width", "Width:", min=600, max=1500, value=800),
                                  sliderInput("PCA_Height", "Height:", min=400, max=1200, value=600)
                                ),
                                mainPanel(
                                  conditionalPanel("output.fileUploaded", 
                                                   uiOutput("PCA.ui")
                                  )
                                )
                              )
                     ),
                      tabPanel("t-SNE", icon = icon("th-large"),
                              sidebarLayout(
                                position = "left",
                                sidebarPanel(
                                  tags$h3("T-Distributed Stochastic Neighbor Embedding(t-SNE)):"),
                                  tags$h4("Warning: t-SNE suits for large number of samples. If your sample number < 10, we highly recommend you using PCA.")
                                  
                                ),
                                mainPanel(
                                  conditionalPanel("output.fileUploaded", 
                                                   plotlyOutput("tSNE_plot", height="700px", width="900px")
                                  )
                                )
                              )
                     ),
                    tabPanel(HTML("<a href='http://bioinformatics.psb.ugent.be/webtools/Venn/', class='fa fa-mars-double' aria-hidden='true', target='_blank'> Venn</a>") )
                     
                    ),
					           
                   tabPanel("About", icon = icon("users"),
                            fluidPage(
                              #titlePanel("About"),
                              
                              fluidRow(
                                column(2),
                                column(7,
                                  h3("About"),
                                  HTML("
                                    <h5>iSeq is a web-based platform for RNA-seq research. If you are new to iSeq read the tutorial (<a href='iSeq_introduction_ChineseVersion.pdf'>Chinese version</a>).</h5><br />
                                    <a href='http://www.pku.edu.cn/'><img src='pku_logo.png', width='40%'/></a> &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; 
                                    <a href='http://www.cls.edu.cn/'><img src='cls_logo.png', width='40%'/></a> &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;
                                    <a href='http://forum.cbi.pku.edu.cn/forum.php'><img src='forum_logo.png', width='40%' /></a> &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;
                                    <a href='http://bioinfocore.cbi.pku.edu.cn/'><img src='Core_logo.png', width='40%' /></a>
                                    "),

                                  br(),br(),br(),
                                  h5("The iSeq is designed and supported by bioinformatics core facility @ Peking University."),
                                  HTML("<h5>This is a free, public, internet accessible resource. Data transfer and data storage are not encrypted. If you worry about the security of data, you are encouraged to setup your own <a href='https://github.com/Czh3/iSeq'>local iSeq</a> on your computer.</h5>"),
								  h5("Github page: https://github.com/Czh3/iSeq"),
                                  br(),br(),br(),
                                  h6("Any bugs or suggestion report to Czh3 (Email: zhangchao3@hotmail.com).")
                                ),
                                column(2)
                                #HTML('
                                #      <div style="width: 400px; height: 400px;">
                                #      <script src="iSeq"></script>
                                #      </div>
                                #       ')
                                )
                              )
                            )
                   
  
))



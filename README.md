# iSeq : A R-Shiny Based webserver for RNA-seq Data Analysis and Visualization

Author: Czh3 <zhangchao3@hotmail.com>

Transcriptome sequencing (RNA-seq) is becoming a standard experimental methodology for genome-wide characterization and quantification of transcripts at single base-pair resolution. However, downstream analysis of massive amount of sequencing data can be prohibitively technical for wet-lab researchers. A functionally integrated and user-friendly platform is required to meet this demand. Here, we present iSeq, an R-based Web server, for RNA-seq data analysis and visualization. iSeq is a streamlined Web-based R application under the Shiny framework, featuring a simple user interface and multiple data analysis modules. Users without programming and statistical skills can analyze their RNA-seq data and construct publication-level graphs through a standardized yet customizable analytical pipeline. iSeq provides five analysis modules dedicated to data quality check, gene expression normalization, detection of differentially expressed genes (DEGs), functional characterization of DEGs, and several common plotting types used in RNA-seq analysis. iSeq is accessible via Web browsers on any operating system.

##Based on R package:
* shiny
* shinyBS
* markdown
* affy
* ggplot2
* gplots
* reshape2
* RColorBrewer
* ggfortify
* DESeq
* goseq
* RDAVIDWebService
* preprocessCore
* plotly
* org.Hs.eg.db
* org.Mm.eg.db
* zeroclipr

##How to install

### install R/Rstudio
This APP is based on R language, download and install R / Rstudio first.
 
### install R package
```bash
# R enveriment:
# The R packages should be installed first!

install.packages(c("shiny", "shinyBS", "markdown", "ggplot2", "gplots", "reshape2",
					"RColorBrewer", "ggfortify", "plotly", "zeroclipr"))


## try http:// if https:// URLs are not supported
source("https://bioconductor.org/biocLite.R")
biocLite(c("affy", "DESeq", "goseq", "RDAVIDWebService", "preprocessCore",
			"org.Hs.eg.db", "org.Mm.eg.db"))

```
### run iSeq
```bash
# run APP 
shiny::runGitHub("iSeq", "Czh3")
```


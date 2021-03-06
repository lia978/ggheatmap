#ggheatmap

##Instructions for making the ggheatmap package from R scripts using RStudio:

### 1.) set up basic R package directory structure:

package.skeleton(...) 
this sets up basic directory structure for making a package, not personally recommended because you have to manually edit some of the files that are automatically created

at minimum you need to create a project folder (e.g. ggheatmap), in that folder create a /R folder and copy over your R scripts to be included in the package

### 2.) obtain package documentation tools and set up RStudio to automatically write documentation files (Rd files) when you build the package

library(devtools)

library(roxygen2)

in RSstudio: file -> new project -> in existing directory -> select ggheatmap

in Rstudio: Build -> configure build tools -> package -> generate documentation with  roxygen -> select all

load_all() # this will create description file if not already made this will give warning if current description file is not valid

edit DESCRIPTION file (see ggheatmap DESCRIPTION file example)

*a note about "Depends" vs "Imports": Depends attachs the package to the main search path: search(), whereas Imports attaches
the package in <imports:packageName>. They say Imports is safer because you gain more control over which function is used 
if you have two functions from different packages that are of the same name. But for now, I put all my dependencies under
"Depends", because my package runs into an error if all dependencies are under Imports. 

load_all() #if no warnings, DESCRIPTION file is good, proceed

In RStudio: Build(a tab right hand side of RStudio) -> check (address the warnings, some are ignorable)


### 3.) add roxygen2 documentation code on top of each of the functions in your R scripts 

add #' @export 

to top of functions in R scripts for the functions you want to export (exclude internal functions)

*you need to do this for at least one function in your package for build and reload to successfully execute

In RStudio: build -> build and reload

this will run package checking and roxygen2, all files created by roxygen2 will start with # Generated by roxygen2 (4.0.2): do not edit by hand. These files are: NAMESPACE and Rd files in /man directory

add more oxygen documentation: e.g. @param and @example (see R/heatmap.ggplot.R) for example

optional: add example datasets in /data directory under the package folder, the easiest way is to save them in .rda files:
e.g. save(eSet1, file = “./data/eSet1.rda")

so after you load your package in R environment you can reference these data files with data()

e.g.

library(ggheatmap)

data(eSet1)

add an additional package documentation R script: see R/ggheatmap.R
this file doesn’t run any R code, but contains more help pages that are additional to ones documented on top of each function. 
For example, in ggheatmap.R I created a general help page for the ggheatmap package and 
separate page pages for each of the datasets associated with the package


### 4.) make source package and share with others 

build -> build and reload 

If this executes successfully, your package is installed! 

reload the package with library() and try about a few of the examples you provide in your documentation and see if they run, then you are good to go

build -> more -> build source package

this will create a .tar.gz file which you can share with others, they can install the package with the command:
R CMD INSTALL package_name.tar.gz

unfortunately, with this local install, you need to install the dependencies first (look up DESCRIPTION file for dependencies) and install them using install.packages()

#### Alternative installation:

if your source code is linked to a github repo, you can install directly from github, this should install (most of) the dependencies correctly

library(devtools)

install_github("lia978/ggheatmap")

some dependencies do not automatically install (e.g. bioconductor packages), so you should install them independently

source("http://bioconductor.org/biocLite.R")

biocLite("Biobase")

install_github("lia978/ggheatmap")

try out some examples:

library(ggheatmap)

?ggheatmap

data(eSet2)

p2<-heatmap.ggplot2(eSet=eSet2, col.clust = TRUE, row.clust = TRUE,
     col.lab = c("HER2_status", "ER_status", "PR_status", "TN_status"), row.lab = "",
     heatmap.y.text = FALSE, heatmap.x.text = FALSE,
     heatmap.colorlegend.name = "RNASeq_expression",
     title.text = "TCGA BRCA log2 RNA-seq expression, z-score row normalized",
     col.legend.name = c("HER2_status", "ER_status", "PR_status", "TN_status"),
     row.legend.name = "",
     row.scaling = "z-score.capped",
     z.norm = FALSE,
     cuttree.col = 4, cuttree.row = 6,
     verbose = FALSE, show = FALSE)
     
print(p2)
     
 





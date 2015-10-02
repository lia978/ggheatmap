require(ggheatmap)
data(eSet1)
eSet1<-eSet1[1:10,1:25]


source("../R/heatmap.ggplot.R")
source("../R/util.R")
p1<-heatmap.ggplot2(eSet=eSet1, col.clust = TRUE, row.clust = TRUE, 
col.clust.hc = NA, row.clust.hc = NA,
col.lab = c("HER2_status", "ER_status", "PR_status", "TN_status"), row.lab = "", 
heatmap.y.text = TRUE, heatmap.x.text = TRUE,
heatmap.colorlegend.name = "RNASeq_expression",
title.text = "TCGA BRCA log2 RNA-seq expression, z-score row normalized",
col.legend.name = c("HER2_status", "ER_status", "PR_status", "TN_status"), 
row.legend.name = "", 
row.scaling = "z-score.capped", 
z.norm = FALSE, 
cuttree.col = 4, cuttree.row = 3,
verbose = FALSE, show = FALSE)

x<-exprs(eSet1)
x<-x[rev(1:nrow(x)),]
hc.row<-hcopt(stats::as.dist(1-cor(t(x))),method="ward.D")
hc.col <- hcopt(stats::dist(t(x)), method="ward.D") 

p2<-heatmap.ggplot2(eSet=eSet1, col.clust = TRUE, row.clust = TRUE, 
col.clust.hc = hc.col, row.clust.hc = hc.row,
col.lab = c("HER2_status", "ER_status", "PR_status", "TN_status"), row.lab = "", 
heatmap.y.text = TRUE, heatmap.x.text = TRUE,
heatmap.colorlegend.name = "RNASeq_expression",
title.text = "TCGA BRCA log2 RNA-seq expression, z-score row normalized",
col.legend.name = c("HER2_status", "ER_status", "PR_status", "TN_status"), 
row.legend.name = "", 
row.scaling = "z-score.capped", 
z.norm = FALSE, 
cuttree.col = 4, cuttree.row = 3,
verbose = FALSE, show = FALSE)

p3<-heatmap.ggplot2(eSet=eSet1, col.clust = TRUE, row.clust = TRUE, 
col.clust.hc = hc.col, row.clust.hc = hc.row,
col.lab = c("HER2_status", "ER_status", "PR_status", "TN_status"), row.lab = "", 
heatmap.y.text = TRUE, heatmap.x.text = TRUE,
heatmap.colorlegend.name = "RNASeq_expression",
title.text = "TCGA BRCA log2 RNA-seq expression, z-score row normalized",
col.legend.name = c("HER2_status", "ER_status", "PR_status", "TN_status"), 
row.legend.name = "", 
row.scaling = "z-score.capped", 
z.norm = FALSE, 
cuttree.col = 4, cuttree.row = 3,
verbose = TRUE, show = FALSE)

meta.c.lab<-levels(unique(p3$meta.c$id))
meta.c.color.string<-c("yellow", "khaki3", "gold", "chocolate", "darkred", "cyan", "white")
meta.c.color<-as.character(sapply(meta.c.color.string, to.hex))

meta.r.lab<-levels(unique(p3$meta.r$id))
meta.r.color.string<-c("pink", "azure", "green")
meta.r.color<-as.character(sapply(meta.r.color.string, to.hex))

p4<-heatmap.ggplot2(eSet=eSet1, 
col.legend.brewer = meta.c.color,
row.legend.brewer = meta.r.color,
col.clust = TRUE, row.clust = TRUE, 
col.clust.hc = hc.col, row.clust.hc = hc.row,
col.lab = c("HER2_status", "ER_status", "PR_status", "TN_status"), row.lab = "cluster.row", 
heatmap.y.text = TRUE, heatmap.x.text = TRUE,
heatmap.colorlegend.name = "RNASeq_expression",
title.text = "TCGA BRCA log2 RNA-seq expression, z-score row normalized",
col.legend.name = c("HER2_status", "ER_status", "PR_status", "TN_status", "cluster.col"), 
row.legend.name = "cluster.row", 
row.scaling = "z-score.capped", 
z.norm = FALSE, 
cuttree.col = 4, cuttree.row = 3,
verbose = FALSE, show = FALSE)

#ggsave(p1, file = "p1.pdf")
#ggsave(p2, file = "p2.pdf")
#ggsave(p3$heatmap, file = "p3.pdf" )
#ggsave(p4, file = "p4.pdf")

hcopt <- function(d, HC=NULL, method = "ward.D", members = NULL){
  require("cba")
  if ( is.null(HC) ) {
    HC <- hclust(d,method=method,members=members)
  }
  #optimal leaf ordering
  ORD <- order.optimal(d,merge=HC$merge)
  HC$merge <- ORD$merge
  HC$order <- ORD$order
  HC
}

hclust.custom<-function(x, type = c("col", "row"), dist.method, clust.method){

	dist.method.choice<-c("euclidean", "maximum", "manhattan", "canberra", "binary", "minkowski", "pearson")

	clust.method.choice<-c("ward.D", "ward.D2",
          "single", "complete", "average", "mcquitty", "median", "centroid")

	if (type == "col"){
		x <-t(x)
	}

	if (!(dist.method %in% dist.method.choice)){
		stop(paste("distance method must be one of ", paste(dist.method.choice, collapse = ", "), "\n",
		"see ?stats::dist for additional details", "\n", sep = ""))
	}

	if (!(clust.method %in% clust.method.choice)){
		stop(paste("clustering method must be one of ", paste(clust.method.choice, collapse = ", "), "\n",
		"see ?stats::hclust for additional details", "\n", sep = ""))
	}

	if (dist.method.choice != "pearson"){
		hc<-hcopt(stats::dist(x, method = dist.method), method = clust.method)
	} else {
		hc <- hcopt(stats::as.dist(1-cor(t(x))),method=clust.method)	
	}
	return(hc)
}



#performs hierarchical clustering on 2D matrix

hclust.wrapper<-function(x,
	col.clust = TRUE,
	row.clust = TRUE, 
	col.clust.method = "ward.D", 
	row.clust.method = "ward.D", 
	row.dist = "euclidean", #any string in stats::dist method: "euclidean", "maximum", "manhattan", "canberra", "binary" or "minkowski"
		#additional option: "pearson's correlation"
	col.dist = "euclidean"	
	){

	require(stats)

  	if (col.clust == TRUE){ 
  		#column distance: euclidean, clustering method: ward
  		if (col.dist != pearson){
  			dist.col <- stats::dist(t(x), method = col.dist)
  		} else {
  			dist.col <- as.dist(1 - cor(t))
  		}
  		hc.col <- hcopt(d = dist.col, method=col.clust.method) 
      	dd.col <- as.dendrogram(hc.col)
      	col.ord <- order.dendrogram(dd.col)
	 # 	data_col <- dendro_data(dd.col, draw = F)
	  #	HC <- ggplot(segment(data_col)) + 
	  #	geom_segment(aes(x=x, y=y, xend=xend, yend=yend)) +
	  #	scale_x_continuous( expand = c(0,0)) + 
	  #	scale_y_continuous(expand = c(0.01,0.01))+
	  #	theme_none 

	  #	if (cuttree.col > 1){
	  #		clusMember.col<-cutree(hc01.col, cuttree.col)
	  #	}
  	}


  	if (row.clust == TRUE){ 
  		#column distance: euclidean, clustering method: ward
  		if (row.dist != pearson){
  			dist.row<-stats::dist(x, method = row.dist)
  		} else {
  			dist.row<-as.dist(1 - cor(t(x)))
  		}
  		hc.row <- hcopt(d = dist.row, method=row.clust.method) 
      	dd.row<-as.dendrogram(hc.row)
      	row.ord<-order.dendrogram(dd.row)
	 # 	data_col <- dendro_data(dd.col, draw = F)
	  #	HC <- ggplot(segment(data_col)) + 
	  #	geom_segment(aes(x=x, y=y, xend=xend, yend=yend)) +
	  #	scale_x_continuous( expand = c(0,0)) + 
	  #	scale_y_continuous(expand = c(0.01,0.01))+
	  #	theme_none 

	  #	if (cuttree.col > 1){
	  #		clusMember.col<-cutree(hc01.col, cuttree.col)
	  #	}
  	}

	return(list(col.ord = col.ord, row.ord =row.ord))
}



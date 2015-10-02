
#####
#This function performs hclust with optimal leaf ordering from S. Monti / D. Gusenleitner
#####

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

#####
#This function converts a color string from colors() to hexidecimal string
#####
to.hex<-function(x){
	cols<-col2rgb(x)
	red<-cols[1]
	green<-cols[2]
	blue<-cols[3]
	return(rgb(red, green, blue, maxColorValue = 255))
}
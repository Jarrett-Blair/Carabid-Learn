setwd("C:/Users/blair/Downloads/CLBJ_042.20160914")

rastlist <- list.files(path = "C:/Users/blair/Downloads/CLBJ_042.20160914", pattern='*.tif', 
                       all.files=TRUE, full.names=FALSE)
allrasters = lapply(rastlist, raster)
rows = as.data.frame(lapply(allrasters, nrow))
col = as.data.frame(lapply(allrasters, ncol))
maxrow = max(rows)
maxcol = max(col)
extendr = function(raster){
  result = extend(raster, c((maxrow - nrow(raster))/2, (maxcol - ncol(raster))/2), value = 0)
  return(result)
}

padsters = lapply(allrasters, extendr)
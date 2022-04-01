
#' Theil-sen regression for a raster time series, 

require("rkt")
require("modifiedmk")

eco.theilsen2 <- function(stacked, dates, 
                          adjust = "none",
                          method = c("mk", "mk_corrected"),
                          my_modified = modifiedmk::mmkh3lag) {
  
  adjust <- match.arg(adjust)
  method <- match.arg(method)
  if(method == "mk")
    cat("Using uncorrected MK test\n")
  else
    cat("Using corrected MK test\n")
  
  
  cat("Starting...", "\n")
  
  # pre allocate memory
  cellnumber <- raster::ncell(stacked)
  
  estimates <- pvalues <- rep(0, cellnumber)
  # compute slope and p value in series
  for(i in seq_len(cellnumber)) {
    temp <- stacked[i]
    if(sum(!is.na(temp)) < 4) {
      estimates[i] <- NA
      pvalues[i] <- NA
    } else {
      if(method == "mk") {
        this_result <- rkt::rkt(dates, temp)
        estimates[i] <- this_result[[3]]
        pvalues[i] <- this_result[[1]]
      } else if(method == "mk_corrected") {
        this_result <- my_modified(as.vector(temp))
        estimates[i] <- this_result[[7]]
        pvalues[i] <- this_result[[2]]
      }
    }
    cat ("\r", ceiling(100 * i / cellnumber), "% ", "completed", sep = "")
  }
  cat("\n")
  
  ts <- pval <- raster(nrow=nrow(stacked), ncol =ncol(stacked), crs=raster::crs(stacked))
  raster::extent(ts) <- raster::extent(pval) <- raster::extent(stacked) 
  
  if(adjust != "none") {
    cat(paste("Adjusting p values with ", adjust, " method"), "\n")
    pvalues <- p.adjust(pvalues, method = adjust)
  }
  
  
  ts[] <- estimates
  pval[] <- pvalues
  
  # write output
  cat("Writing slope image into workspace...", "\n")
  raster::writeRaster(ts, "slope.tif", overwrite = T)
  cat("Writing P-value image into workspace...", "\n")
  raster::writeRaster(pval, "pvalue.tif", overwrite = T)
  
}

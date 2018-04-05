#'@export
#'@import ggplot2
#'@title plot intensity distribution of an ion in different ROIs
#'@description displays intensty data for a given m/z in different user-defined ROIs. If ROIs are absent, will display the distribution of the signal in the entire data set
#'@param cardinaldata MSImageSet containing pixels and intensity data
#'@param mzs single mz value for plotting
#'@param roi_names ROIs saved in the MSImageSet pixelData using selectROI
#'@return ggplot2 object and display of plot

mzIntensPlot <- function(cardinaldata, mz = 885.55, roi_names = c('myrois')){

  mz_idx = features(cardinaldata, mz = mz)

  if(!any(roi_names %in% names(pixelData(cardinaldata)@data)) == T){

    intensity_plot_data = data.frame(ROI = deparse(substitute(cardinaldata)), intensity = iData(cardinaldata)[mz_idx,], row.names = NULL)

  } else {

    intensity_dfs <- list()

    for(roi in roi_names){
      intensity_dfs[[roi]] = tryCatch(data.frame(ROI = roi, intensity = iData(cardinaldata)[mz_idx,pData(cardinaldata)[roi][,1]], row.names = NULL), silent = TRUE, error= function(e){
        NULL
      })

    }
    intensity_plot_data = do.call(rbind, intensity_dfs)
  }

  min.mean.sd.max <- function(x) {
    r <- c(min(x), mean(x) - sd(x), mean(x), mean(x) + sd(x), max(x))
    names(r) <- c("ymin", "lower", "middle", "upper", "ymax")
    r
  }



  p <- ggplot(intensity_plot_data, aes(x=ROI , y=intensity)) +
    geom_jitter(aes(colour = ROI, x = ROI),
                position = position_jitter(width = .05), alpha = 0.2) +
    geom_violin(aes(colour = ROI, fill=ROI), position = position_dodge(width=0.9), alpha = 0.5, width=1)

  p <- p + ggtitle(paste0('m/z ', mz))

  p

  return(p)
}



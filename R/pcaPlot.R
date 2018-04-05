#' @export
#' @title plot 2D PCA scores in R from Cardinal
#' @description uses ggplot2 to plot PC scores with optional ROI color coding
#' @param pca_data an object of class ResultSet
#' @param components integer vector indicating which PCs to plot, will only plot the first two supplied values
#'
#' @return ggplot of the PCA scores
#' @import ggplot2


pcaPlot <- function(pca_data, components = c(1,2), roi_names = c('myrois')){

  if(length(components) < 2){
    stop('at least two components are needed')
  }

  pca_plot_comps = data.frame(pca_data$scores[[1]])

  ROIs <- as.character(pixelData(pca_data)$sample)

  if(!any(roi_names %in% names(pixelData(pca_data)@data)) == F){

    for(roi_name in roi_names){
      pix_idx = which(pData(pca_data)[roi_name][,1] == T)
      ROIs[pix_idx] =  roi_name
    }
  }

  pca_plot_comps$ROIs <- as.factor(ROIs)

  ggplot(pca_plot_comps,
         aes_string(x=paste0('PC',components[1]),
                    y=paste0('PC',components[2]),
                    colour = 'ROIs')) +

    geom_point(size=0.75, shape=1) +

    ggtitle(paste0('PCA plot of components ',components[1],' & ', components[2]))


}


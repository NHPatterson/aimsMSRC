#'@export
#'@title Plot mean spectra for ROIs
#'@description see title
#'@param cardinaldata MSImageSet
#'@param roi_names vector of roi_names, if none are found in the pixelData, only the overall mean will be plotted
#'@param ... arguments passed to image()
#'@return R plot of the mean spectra

plotMeanSpecs <- function(cardinaldata, roi_names = c('myrois'),...){

  ROIs <- rep('overall_mean', ncol(cardinaldata))

  if(!any(roi_names %in% names(pixelData(cardinaldata)@data)) == F){

    for(roi_name in roi_names){
      pix_idx = which(pData(cardinaldata)[roi_name][,1] == T)
      ROIs[pix_idx] =  roi_name
    }
  }

  mycol <- rainbow(nlevels(as.factor(ROIs)))

  plot(cardinaldata, pixel=1:ncol(cardinaldata), pixel.groups=ROIs, superpose=TRUE, key=TRUE, col = mycol, main="Mean spectra for the selected ROIs",...)

}

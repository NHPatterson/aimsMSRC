#'@export
#'@title Output representative image for registration IMS to microscopy registration
#'@description Output representative image for IMS to microscopy registration
#'@param cardinaldata a cardinal MSImageSet or ResultSet
#'@param filename output file name, will be extended by '.tif'
#'@param ... arguments passed to Cardinal's image function mz selection, PCA component selection, etc.
#'@return writes tif of IMS data in working directory for registration
#'@import Cardinal


outputIMSrepImg <- function(cardinaldata, filename = 'myIMSregImage',...){

  tiff(filename = paste0(filename, '.tif'),
       width = max(coord(cardinaldata)$x),
       height = max(coord(cardinaldata)$y),
       units = "px", pointsize = 1,
       compression = 'none',
       bg = "black")

  par(mar=c(0, 0, 0, 0), xaxs='i', yaxs='i')

  image(cardinaldata, strip=F, colorkey=F, key=F,...)

  dev.off()

}

#'@export
#'@title Output microscopy fusion files
#'@description Outputs a comma seperated .txt file in UNIX format for the fusion prototype tool
#'@param micro_image_array an image array [x,y,channel] from the RGB microscopy data loaded into R through the tiff, jpeg, or png library
#'@param filename output file name without extension. a '_micro_data.txt' and '_micro_info.xml' will be tagged onto files approrpiately
#'@param data_label a description of the data(i.e. rat brain, 1 um / pixel)
#'@param micro_spatial_res microscopy image spatial resolution in microns / pixel
#'@return writes outputs in working directory
#'@import imager
#'@import plyr

outputMicroFusion <- function(micro_image_array, filename="myfusionmicrodata",
                              mask_image_array = NULL,
                              data_label = 'microscopy data',
                              micro_spatial_res = 1){

  micro_image_df = as.cimg(micro_image_array)
  micro_image_df = as.data.frame(micro_image_df, wide= "c")

  if(!is.null(mask_image_array)){
    mask_image_df = as.cimg(mask_image_array)
    mask_image_df = as.data.frame(mask_image_df, wide= "c")

    if(nrow(mask_image_df) != nrow(micro_image_df)){
      stop("mask dimensions do match image dimensions", call. = TRUE, domain = NULL)
    }
    positive_idx = which(mask_image_df[,3] > 0)
    micro_image_df = micro_image_df[positive_idx,]
  }
  colnames(micro_image_df) <- c('row', 'col', "Red","Green","Blue")
  micro_image_df <- arrange(micro_image_df, micro_image_df$col, micro_image_df$row)

  dest <- file(paste0(filename, '_micro_data.txt'), open="wb")
  write.table(micro_image_df, file=dest, quote=FALSE, sep=",", eol=",\n",row.names=F, col.names=T)
  close(dest)

  outputFusionMicroInfo(micro_image_array,
                        filename = filename,
                        data_label = data_label,
                        micro_spatial_res = micro_spatial_res,
                        data_points = nrow(micro_image_df))


}


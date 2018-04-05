#'@export
#'@title Output imaging mass spectrometry fusion files
#'@description Outputs a comma seperated .txt file in UNIX format and an XML of metadata for the fusion prototype tool
#'@param cardinaldata a Cardinal MSImageSet of full profile data
#'@param filename output file name without extension. a '_ims_data' and '_ims_info' will be tagged onto files approrpiately
#'@param data_label a description of the data(i.e. rat brain, 1 um / pixel)
#'@param type continus or peak-picked, continous is full profile, peak-picked is peak picked data but does not currently work (TODO)
#'@return writes outputs in working directory
#'@import Cardinal
#'@import plyr

outputIMSfusion <- function(cardinaldata, filename = 'myfusiondata',data_label = 'this is my fusion data', type= c('continuous', 'peak-picked'), IMS_spatial_res = 100){


  if(type != 'continuous'){

    stop("Unrecognized data type argument, please use \'continuous\'")

  }

  if(type == 'continuous'){

    #cardinaldata <- reduceXYcardinal(cardinaldata)

    intensity_data <- data.frame(row = cardinaldata$y,
                                 col = cardinaldata$x,
                                 t(iData(cardinaldata)))

    colnames(intensity_data)[3:ncol(intensity_data)] <- round(Cardinal::mz(cardinaldata),4)

    intensity_data <- arrange(intensity_data, intensity_data$col, intensity_data$row)

    dest <- file(paste0(filename, '_ims_data.txt'), open="wb")
    write.table(intensity_data, file=dest, quote=FALSE, sep=",", eol=",\n",row.names=F)
    close(dest)

    outputFusionIMSinfo(cardinaldata,
                        filename = paste0(filename,'_ims_info'),
                        data_label = data_label,
                        IMS_spatial_res = IMS_spatial_res)

  }

  # if(type == 'peak-picked'){
  #
  #   cardinaldata <- reduceXYcardinal(cardinaldata)
  #
  #   outputIMSforFusion(cardinaldata, filename = paste0(filename,'_info'), data_label = 'my fusion data', IMS_spatial_res = IMS_spatial_res)
  #
  #
  # }



}


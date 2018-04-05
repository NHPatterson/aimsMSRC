#'@export
#'@title Export csv intensity table
#'@description exports a data table including x,y, mz... and intensity data for imaging dataset for use in other analysis softwares
#'@param cardinaldata MSImageSet containing pixels and intensity data
#'@param mz_range range of mz_values to export
#'@param metadata boolean, include all metadata or just x,y?
#'@param filename filename for output, will be appended with '.csv'
#'@return writes csv of intensity data matrix to disk
#'

exportIntensMatrix <- function(cardinaldata, mz_range = c(0,1000), metadata = TRUE, filename = "myintensitydata"){

  #mz indexing
  begin_feature = features(cardinaldata, mz=min(mz_range))
  end_feature = features(cardinaldata, mz=max(mz_range))

  if(metadata == TRUE){
    intensity_df = data.frame(pData(cardinaldata), t(iData(cardinaldata))[,begin_feature:end_feature])
    colnames(intensity_df) <- c(colnames(pData(cardinaldata)),
                                round(mz(cardinaldata)[begin_feature:end_feature],3))

    } else {
    intensity_df = data.frame(pData(cardinaldata)[,1:2], t(iData(cardinaldata))[,begin_feature:end_feature])
    colnames(intensity_df) <- c(colnames(pData(cardinaldata))[,1:2],
                                round(mz(cardinaldata)[begin_feature:end_feature],3))

  }

  write.csv(intensity_df, file=paste0(filename, '.csv'))


}

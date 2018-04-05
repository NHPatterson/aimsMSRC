#'@export
#'@title Output IMS fusion metadata xml
#'@description Outputs an eXtensible Markup Language file based on the IMS input
#'@param cardinaldata a Cardinal MSImageSet of full profile data
#'@param filename output file name without extension
#'@param data_label a description of the data (i.e. rat brain, 10 um / pixel)
#'@param IMS_spatial_res IMS image spatial resolution in microns / pixel
#'@return writes xml in working directory
#'@import XML
#'@import Cardinal

outputFusionIMSinfo <- function(cardinaldata, filename = 'mydata', data_label = 'my fusion data', IMS_spatial_res = 10){

  modality = 'msi'

  #cols
  spatial_grid_size_x = max(cardinaldata$x)

  #rows
  spatial_grid_size_y = max(cardinaldata$y)

  nr_spatial_grid_elems = spatial_grid_size_x * spatial_grid_size_y

  nr_vars = nrow(cardinaldata)[1]

  nr_obs = ncol(cardinaldata)[1]

  doc <- newXMLDoc()
  root = newXMLNode("data_source", doc=doc)
  newXMLNode("modality", modality, parent=root)
  newXMLNode("data_label", data_label, parent=root)
  newXMLNode("nr_spatial_dims", 2, parent=root)

  #rows x cols
  newXMLNode("spatial_grid_size",
             paste(spatial_grid_size_y, spatial_grid_size_x, sep=" "),
             parent=root)

  newXMLNode("nr_spatial_grid_elems", nr_spatial_grid_elems, parent=root)
  newXMLNode("spatial_resolution_um", IMS_spatial_res, parent=root)
  newXMLNode("nr_obs", nr_obs, parent=root)
  newXMLNode("nr_vars", nr_vars, parent=root)

  cat(saveXML(doc,
              indent = TRUE,
              prefix = "<?xml version=\"1.0\" encoding=\"utf-8\"?>\n"),
      file=paste0(filename, '.xml'))


}

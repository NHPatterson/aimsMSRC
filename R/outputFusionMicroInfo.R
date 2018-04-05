#'@export
#'@title Output microscopy fusion metadata xml
#'@description Outputs an eXtensible Markup Language file based on the microscopy input
#'@param micro_image_array an image array [x,y,channel] from the RGB microscopy data loaded into R through the tiff, jpeg, or png library
#'@param filename output file name
#'@param data_label a description of the data (i.e. rat brain, 1 um / pixel)
#'@param micro_spatial_res microscopy image spatial resolution in microns / pixel
#'@return writes xml in working directory
#'@import XML


outputFusionMicroInfo <- function(micro_image_array,
                                 filename = 'myfusiondata',
                                 data_label = 'fusion label',
                                 micro_spatial_res = 1,
                                 data_points = NULL){


  modality = 'microscopy'

  #cols
  spatial_grid_size_x = dim(micro_image_array)[1]

  #rows
  spatial_grid_size_y = dim(micro_image_array)[2]

  nr_spatial_grid_elems = spatial_grid_size_x * spatial_grid_size_y

  nr_vars = dim(micro_image_array)[3]

  if(!is.null(data_points)){
    nr_obs = data_points
  } else {
    nr_obs = nr_spatial_grid_elems
  }

  doc <- newXMLDoc()
  root = newXMLNode("data_source", doc=doc)
  newXMLNode("modality", modality, parent=root)
  newXMLNode("data_label", data_label, parent=root)
  newXMLNode("nr_spatial_dims", 2, parent=root)

  #rows x cols
  newXMLNode("spatial_grid_size",
             paste(spatial_grid_size_x, spatial_grid_size_y, sep=" "),
             parent=root)

  newXMLNode("nr_spatial_grid_elems", nr_spatial_grid_elems, parent=root)
  newXMLNode("spatial_resolution_um", micro_spatial_res, parent=root)
  newXMLNode("nr_obs", nr_obs, parent=root)
  newXMLNode("nr_vars", nr_vars, parent=root)

  cat(saveXML(doc,
              indent = TRUE,
              prefix = "<?xml version=\"1.0\" encoding=\"utf-8\"?>\n"),
      file=paste0(filename, '_micro_info.xml'))


}

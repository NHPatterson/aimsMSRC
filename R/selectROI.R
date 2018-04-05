#' @export
#' @title Select ROI using Cardinal wrapper
#' @description Select ROI using Cardinal and return it as a boolean vector in the pixelData for downstream analysis
#' @param cardinaldata an object of class MSImageSet
#' @param mz the mz value used for plot and ROI selection
#' @return MSImageSet with selection boolean
#'


selectROI <- function(cardinaldata, mz=885.55){


  if(class(cardinaldata)[1] != 'MSImageSet'){
    stop(paste0(deparse(substitute(cardinaldata)),' is not Cardinal MSImageSet'))
  }

  #add layout to make sure selection can be made on any of the samples in the MSImageSet
  if(nlevels(cardinaldata$sample) > 1){
    print('multi samples in dataset, select the ROI on one then the next if present')
    multisample_roi_selection = c()

    for(sample in levels(cardinaldata$sample)){

      roi_selection =  select(cardinaldata[,cardinaldata$sample %in% sample], mz= mz, layout=c(1,1))

      if(length(roi_selection) == 0){
        multisample_roi_selection =c(multisample_roi_selection, rep(FALSE,ncol(cardinaldata[,cardinaldata$sample %in% sample])))

      } else{

        multisample_roi_selection =c(multisample_roi_selection, roi_selection)

      }
      }

    if(any(multisample_roi_selection ==  TRUE)){
      roi_name <- readline(prompt="Name of the ROI: ")

      #add ROI to pixel data as boolean
      pData(cardinaldata)[roi_name] = FALSE
      pData(cardinaldata)[roi_name][multisample_roi_selection,] = TRUE

      image(cardinaldata, pData(cardinaldata)[roi_name][,1] ~ x*y, strip=F, main = paste(roi_name, ': selection in red'))

      return(cardinaldata)
    }else{

      stop('No ROI was selected')

    }

    } else {
      #user cardinal's internal selection tool to get ROI
      roi_selection <- select(cardinaldata, mz=mz,layout=c(1,1))

      if(length(roi_selection) == 0){
        stop('No ROI was selected')
      }

      #prompt user for input at console
      roi_name <- readline(prompt="Name of the ROI: ")

      #add ROI to pixel data as boolean
      pData(cardinaldata)[roi_name] = FALSE
      pData(cardinaldata)[roi_name][roi_selection,] = TRUE

      #plot selected ROI
      image(cardinaldata, pData(cardinaldata)[roi_name][,1] ~ x*y, strip=F, main = paste(roi_name, ': selection in red'))

      #return appended MSImageSet
      return(cardinaldata)

    }
}


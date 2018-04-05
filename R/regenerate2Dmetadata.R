regenerate2Dmetadata <- function(cardinaldata){
  varMetadata(cardinaldata)[c("x","y","sample"),"labelType"] <- "dim"

  protocolData(cardinaldata) <- AnnotatedDataFrame(data=data.frame(row.names=sampleNames(cardinaldata)))

  cardinaldata <- regeneratePositions(cardinaldata)

  print(validObject(cardinaldata))
  return(cardinaldata)
}

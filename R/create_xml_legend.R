create_xml_legend <- function(unique_values, destination, name_file){
  #' Create .tif.aux.xml legend for soilgrids layer
  #'
  #' @description
  #' internal function.
  #'
  #' @param unique_values int vector. with unique values in soilgrids layer.
  #' @param destination character. absolute path where create .tif.aux.xml file.
  #' @param name_file character. Name of the file to create, `gecevar` or `environ`.
  #' @import rjson
  #' @export

  json <- fromJSON(file = "https://files.isric.org/soilgrids/latest/data/wrb/MostProbable.rat.json")
  df <- as.data.frame(json)[, -1]

  header <- '<PAMDataset>
  <PAMRasterBand band="6">
    <GDALRasterAttributeTable tableType="thematic">
      <FieldDefn index="0">
        <Name>VALUE</Name>
        <Type>1</Type>
        <Usage>5</Usage>
      </FieldDefn>
      <FieldDefn index="1">
        <Name>SoilType</Name>
        <Type>2</Type>
        <Usage>2</Usage>
      </FieldDefn>'

  footer <- '    </GDALRasterAttributeTable>
  </PAMRasterBand>
</PAMDataset>'


  for (i in 1:length(unique_values)){
    row <- glue('      <Row index="{i - 1}">
        <F>{unique_values[i]}</F>
        <F>{df[i + 1]}</F>
      </Row>')
    header <- paste(header, row, sep = '\n')
  }
  header <- paste(header, footer, sep = '\n')
  writeLines(header, paste(destination, paste0(name_file, ".tif.aux.xml"), sep = "/"))
}

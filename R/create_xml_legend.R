#' Create .tif.aux.xml legend for soilgrids layer
#'
#' @param unique_values int vector. with unique values in soilgrids layer.
#' @param output_dir character. Output directory
#' @param file_name character. Name of the output file without .tif.aux.xml extension.
#' @import rjson
#' @keywords internal
#' 
create_xml_legend <- function(unique_values, output_dir, file_name) {

  json <- fromJSON(file = "https://files.isric.org/soilgrids/latest/data/wrb/MostProbable.rat.json")
  soil <- unlist(json)[-1]

  header <- '<PAMDataset>
  <PAMRasterBand band="6">
    <GDALRasterAttributeTable tableType="thematic">
      <FieldDefn index="0">
        <Name>VALUE</Name>
        <Type>1</Type>
        <Usage>5</Usage>
      </FieldDefn>
      <FieldDefn index="1">
        <Name>soil_type</Name>
        <Type>2</Type>
        <Usage>2</Usage>
      </FieldDefn>'

  footer <- '    </GDALRasterAttributeTable>
  </PAMRasterBand>
</PAMDataset>'


  for (i in 1:length(unique_values)) {
    row <- glue('      <Row index="{i - 1}">
        <F>{unique_values[i]}</F>
        <F>{soil[i + 1]}</F>
      </Row>')
    header <- paste(header, row, sep = '\n')
  }
  header <- paste(header, footer, sep = '\n')
  writeLines(header, paste(output_dir, paste0(file_name, ".tif.aux.xml"), sep = "/"))
}

# End

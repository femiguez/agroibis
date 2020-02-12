#'
#' Edit an Agro-IBIS parameter file
#' 
#' @title Edit an Agro-IBIS parameter file
#' @name edit_parms
#' @param crop.parms.lst list wth components as produced by 'read_crop_parms'
#' @param table.name name of the table to edit
#' @param col name (or index) of column to edit
#' @param rwo name (or index) of row to edit
#' @param value value for replacement
#' @return list structure with replaced values
#' @export
#' @examples 
#' \donttest{
#' extd.dir <- system.file("extdata", package = "agroibis")
#' cpp <- read_crop_parms(file = "params_text.crp", src.dir = extd.dir)
#' }
#' 
edit_parms <- function(crop.parms.lst, 
                       table.name = c("crop_growth_physiology_properties",
                                      "crop_growth_leafarea_c_allocation",
                                      "climatic_manage_planting_control",
                                      "gdd_phenology_control",
                                      "wheat_growth_control",
                                      "misc_crop_control",
                                      "sugarcane_control",
                                      "crop_residue_control"),
                       col = NULL,
                       row = NULL,
                       value = 0){
  
  table.name <- match.arg(table.name)
  
  ## The distinction between numeric and character 
  ## seems unnecesary but it might catch some errors
  ## maybe...
  if(inherits(col,"numeric") && inherits(row, "numeric")){
    crp.par.lst[[table.name]][row, col] <- value
  }
  
  if(inherits(col,"character") && inherits(row, "character")){
    tbl <- crp.par.lst[[table.name]]
    i.row <- which(rownames(tbl) == row)
    i.col <- which(colnames(tbl) == col)
    crp.par.lst[[table.name]][i.row, i.col] <- value
  }
  
  return(crp.par.lst)
}
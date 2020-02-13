#'
#' Edit an Agro-IBIS parameter file
#' 
#' @title Edit an Agro-IBIS parameter file
#' @name edit_parms
#' @param crop.parms.lst list wth components as produced by 'read_crop_parms'
#' @param table.name name of the table to edit
#' @param col name (or index) of column to edit
#' @param row name (or index) of row to edit
#' @param value value for replacement
#' @return list structure with replaced values
#' @export
#' @examples 
#' \donttest{
#' extd.dir <- system.file("extdata", package = "agroibis")
#' cpp <- read_crop_parms(file = "params_text.crp", src.dir = extd.dir)
#' cpp[["wheat_growth_control"]]
#' cpp2 <- edit_parms(cpp, table.name = "wheat_growth_control",
#'                    col = 2, row = 1, value = 0.812)
#' cpp2[["wheat_growth_control"]]
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
  
  if(missing(col)) stop("need to provide a column either by index or name")
  if(missing(row)) stop("need to provide a row either by index or name")
  
  ## The distinction between numeric and character 
  ## seems unnecesary but it might catch some errors
  ## maybe...
  if(inherits(col,"numeric") && inherits(row, "numeric")){
    crop.parms.lst[[table.name]][row, col] <- value
  }
  
  if(inherits(col,"character") && inherits(row, "character")){
    tbl <- crop.parms.lst[[table.name]]
    i.row <- which(rownames(tbl) == row)
    i.col <- which(colnames(tbl) == col)
    crop.parms.lst[[table.name]][i.row, i.col] <- value
  }else{
    ## It might still work if you mix numeric and character...
    crop.parms.lst[[table.name]][row, col] <- value
  }
  
  return(crop.parms.lst)
}
#'
#' Read an Agro-IBIS crop parameter file
#' 
#' @title Read an Agro-IBIS crop parameter file
#' @name read_crop_parms
#' @param file file name, should end in .crp
#' @param src.dir source directory
#' @param verbose whether to print progress messages (default = TRUE)
#' @return list structure with matrices for different sections of the parameter file
#' @export
#' @examples 
#' \donttest{
#' extd.dir <- system.file("extdata", package = "agroibis")
#' cpp <- read_crop_parms(file = "params_text.crp", src.dir = extd.dir)
#' }
#' 
read_crop_parms <- function(file, src.dir = ".", verbose = TRUE){
  
  ## Will separate file name from path
  file.pth <- file.path(src.dir, file)
  
  parms <- readLines(con = file.pth)
  
  j <- 1
  cgpp <- FALSE
  ## Crop growth physiology properties parameters start
  cgpp.start <- "###<crop_growth_physiology_properties_start>###"
  cgpp.end <- "###<crop_growth_physiology_properties_end>###"
  cgpp.var.names <- NULL
  cgpp.values <- NULL
  cgpp.pfts <- NULL
  
  ## Crop growth leaf area expansion parameter start
  ## laca: stands for leaf area and carbon allocation
  laca <- FALSE
  laca.start <- "###<crop_growth_leafarea_c_allocation_start>###"
  laca.end <- "###<crop_growth_leafarea_c_allocation_end>###"
  laca.var.names <- NULL
  laca.values <- NULL
  laca.pfts <- NULL
  
  ## Climatic and managed planting control
  clmp <- FALSE
  clmp.start <- "###<climatic_managed_planting_control_start>###"
  clmp.end <- "###<climatic_managed_planting_control_end>###"
  clmp.var.names <- NULL
  clmp.values <- NULL
  clmp.pfts <- NULL
  
  ## GDD and phenology control
  gdd <- FALSE
  gdd.start <- "###<gdd_phenology_control_start>###"
  gdd.end <- "###<gdd_phenology_control_end>###"
  gdd.var.names <- NULL
  gdd.values <- NULL
  gdd.pfts <- NULL
  
  ## wheat growth
  wheat <- FALSE
  wheat.start <- "###<wheat_growth_start>###"
  wheat.end <- "###<wheat_growth_end>###"
  wheat.var.names <- NULL
  wheat.values <- NULL
  wheat.pfts <- NULL
  
  ## miscellaneous crop
  misc <- FALSE
  misc.start <- "###<misc_crop_control_start>###"
  misc.end <- "###<misc_crop_control_end>###"
  misc.var.names <- NULL
  misc.values <- NULL
  misc.names <- NULL ## There are no PFTs for misc
  
  ## sugarcane 
  sugar <- FALSE
  sugar.start <- "###<sugarcane_control_start>###"
  sugar.end <- "###<sugarcane_control_end>###"
  sugar.var.names <- NULL
  sugar.values <- NULL
  sugar.names <- NULL ## There are no PFTs for sugarcane
  
  ## crop residue
  residue <- FALSE
  residue.start <- "###<crop_residue_control_start>###"
  residue.end <- "###<crop_residue_control_end>###"
  residue.var.names <- NULL
  residue.values <- NULL
  residue.pfts <- NULL
  
  ## Create a list for storing results
  crop.parms.lst <- vector("list", length = 10)
  ## Keep track of index list
  
  for(i in seq_along(parms)){
    
    ## First process crop growth physiology parameters
    if(grepl(cgpp.start, parms[i], fixed = TRUE)){
      cgpp <- TRUE
      cgpp.i.1 <- i
    } 
    
    while(cgpp && j < 100){
      ## The counter above is to prevent wild things from happening
      ## If it is an empty character we break the while loop
      if(parms[i] == "") break
      
      ## If it has one # and then letters it is a parameter name
      if(grepl("^#[a-z]",parms[i])){
        ## Create vector of such name
        cgpp.var.names <- c(cgpp.var.names, sub("#", "\\1", parms[i]))
      }else{
        ## We need to make sure that we skip non-parameter lines
        if(!grepl("^#", parms[i])){
          cgpp.values <- c(cgpp.values, as.numeric(strsplit(parms[i], "!")[[1]][1]))
          cgpp.pfts <- c(cgpp.pfts, strsplit(parms[i], "!")[[1]][2])
        }
      }
      
      ## When the end of the section has been found, create the matrix
      if(grepl(cgpp.end, parms[i], fixed = TRUE)){
        cgpp.i.2 <- i
        if(verbose) cat("Done with crop growth physiology properties \n")

        cgpp.mat <- matrix(cgpp.values, 
                           ncol = length(cgpp.var.names), 
                           nrow = length(unique(cgpp.pfts)),
                           dimnames = list(unique(as.character(cgpp.pfts)), cgpp.var.names))
        cgpp <- FALSE
        j <- 1
        crop.parms.lst[[1]] <- cgpp.mat
        break
      }else{
        j <- j + 1
        break
      }
    }
    
    ## Second process leaf area expansion and carbon allocation
    if(grepl(laca.start, parms[i], fixed = TRUE)){
      laca <- TRUE
      laca.i.1 <- i
    } 
    
    while(laca && j < 100){
      ## The counter above is to prevent wild things from happening
      ## If it is an empty character we break the while loop
      if(parms[i] == "") break
      
      ## If it has one # and then letters it is a parameter name
      if(grepl("^#[a-z]",parms[i])){
        ## Create vector of such name
        laca.var.names <- c(laca.var.names, sub("#", "\\1", parms[i]))
      }else{
        ## We need to make sure that we skip non-parameter lines
        if(!grepl("^#", parms[i])){
          laca.values <- c(laca.values, as.numeric(strsplit(parms[i], "!")[[1]][1]))
          laca.pfts <- c(laca.pfts, strsplit(parms[i], "!")[[1]][2])
        }
      }
      
      ## When the end of the section has been found, create the matrix
      if(grepl(laca.end, parms[i], fixed = TRUE)){
        laca.i.2 <- i
        if(verbose) cat("Done with leaf area expansion and carbon allocation \n")
        
        laca.mat <- matrix(laca.values, 
                           ncol = length(laca.var.names), 
                           nrow = length(unique(laca.pfts)),
                           dimnames = list(unique(as.character(laca.pfts)), laca.var.names))
        laca <- FALSE
        j <- 1
        crop.parms.lst[[2]] <- laca.mat
        break
      }else{
        j <- j + 1
        break
      }
    }
    
    ## Third process climatic and managed planting control
    if(grepl(clmp.start, parms[i], fixed = TRUE)){
      clmp <- TRUE
      clmp.i.1 <- i
    } 
    
    while(clmp && j < 100){
      ## The counter above is to prevent wild things from happening
      ## If it is an empty character we break the while loop
      if(parms[i] == "") break
      
      ## If it has one # and then letters it is a parameter name
      if(grepl("^#[a-z]",parms[i])){
        ## Create vector of such name
        clmp.var.names <- c(clmp.var.names, sub("#", "\\1", parms[i]))
      }else{
        ## We need to make sure that we skip non-parameter lines
        if(!grepl("^#", parms[i])){
          clmp.values <- c(clmp.values, as.numeric(strsplit(parms[i], "!")[[1]][1]))
          clmp.pfts <- c(clmp.pfts, strsplit(parms[i], "!")[[1]][2])
        }
      }
      
      ## When the end of the section has been found, create the matrix
      if(grepl(clmp.end, parms[i], fixed = TRUE)){
        clmp.i.2 <- i
        if(verbose) cat("Done with climatic and managed planting control \n")
        
        clmp.mat <- matrix(clmp.values, 
                           ncol = length(clmp.var.names), 
                           nrow = length(unique(clmp.pfts)),
                           dimnames = list(unique(as.character(clmp.pfts)), clmp.var.names))
        clmp <- FALSE
        j <- 1
        crop.parms.lst[[3]] <- clmp.mat
        break
      }else{
        j <- j + 1
        break
      }
    }
    
    ## Fourth process GDD and phenology control
    if(grepl(gdd.start, parms[i], fixed = TRUE)){
      gdd <- TRUE
      gdd.i.1 <- i
    } 
    
    while(gdd && j < 100){
      ## The counter above is to prevent wild things from happening
      ## If it is an empty character we break the while loop
      if(parms[i] == "") break
      
      ## If it has one # and then letters it is a parameter name
      if(grepl("^#[a-z]",parms[i])){
        ## Create vector of such name
        gdd.var.names <- c(gdd.var.names, sub("#", "\\1", parms[i]))
      }else{
        ## We need to make sure that we skip non-parameter lines
        if(!grepl("^#", parms[i])){
          gdd.values <- c(gdd.values, as.numeric(strsplit(parms[i], "!")[[1]][1]))
          gdd.pfts <- c(gdd.pfts, strsplit(parms[i], "!")[[1]][2])
        }
      }
      
      ## When the end of the section has been found, create the matrix
      if(grepl(gdd.end, parms[i], fixed = TRUE)){
        gdd.i.2 <- i
        if(verbose) cat("Done with gdd and phenology control \n")
        
        gdd.mat <- matrix(gdd.values, 
                           ncol = length(gdd.var.names), 
                           nrow = length(unique(gdd.pfts)),
                           dimnames = list(unique(as.character(gdd.pfts)), gdd.var.names))
        gdd <- FALSE
        j <- 1
        crop.parms.lst[[4]] <- gdd.mat
        break
      }else{
        j <- j + 1
        break
      }
    }
    
    ## Fifth process wheat growth control
    if(grepl(wheat.start, parms[i], fixed = TRUE)){
      wheat <- TRUE
      wheat.i.1 <- i
    } 
    
    while(wheat && j < 150){
      ## The counter above is to prevent wild things from happening
      ## If it is an empty character we break the while loop
      if(parms[i] == "") break
      
      ## If it has one # and then letters it is a parameter name
      if(grepl("^#[a-z]",parms[i])){
        ## Create vector of such name
        wheat.var.names <- c(wheat.var.names, sub("#", "\\1", parms[i]))
      }else{
        ## We need to make sure that we skip non-parameter lines
        if(!grepl("^#", parms[i])){
          wheat.values <- c(wheat.values, as.numeric(strsplit(parms[i], "!")[[1]][1]))
          wheat.pfts <- c(wheat.pfts, strsplit(parms[i], "!")[[1]][2])
        }
      }
      
      ## When the end of the section has been found, create the matrix
      if(grepl(wheat.end, parms[i], fixed = TRUE)){
        wheat.i.2 <- i
        if(verbose) cat("Done with wheat growth control \n")
        
        wheat.mat <- matrix(wheat.values, 
                          ncol = length(wheat.var.names), 
                          nrow = length(unique(wheat.pfts)),
                          dimnames = list(unique(as.character(wheat.pfts)), wheat.var.names))
        wheat <- FALSE
        j <- 1
        crop.parms.lst[[5]] <- wheat.mat
        break
      }else{
        j <- j + 1
        break
      }
    }
    
    ## Sixth process wheat growth control
    if(grepl(misc.start, parms[i], fixed = TRUE)){
      misc <- TRUE
      misc.i.1 <- i
    } 
    
    while(misc && j < 150){
      ## The counter above is to prevent wild things from happening
      ## If it is an empty character we break the while loop
      if(parms[i] == "") break
      
      ## If it has one # and then letters it is a parameter name
      if(grepl("^#[a-z]",parms[i])){
        ## Create vector of such name
        ## For misc crop this does not exist
        misc.var.names <- c(misc.var.names, sub("#", "\\1", parms[i]))
      }else{
        ## We need to make sure that we skip non-parameter lines
        if(!grepl("^#", parms[i])){
          misc.values <- c(misc.values, as.numeric(strsplit(parms[i], "!")[[1]][1]))
          misc.names <- c(misc.names, strsplit(parms[i], "!")[[1]][2])
        }
      }
      
      ## When the end of the section has been found, create the matrix
      if(grepl(misc.end, parms[i], fixed = TRUE)){
        misc.i.2 <- i
        if(verbose) cat("Done with miscellaneous crop control \n")
        
        misc.mat <- matrix(misc.values, 
                            ncol = 1, ## At this point this is just one 
                            nrow = length(misc.names),
                            dimnames = list(as.character(misc.names), "parameter"))
        misc <- FALSE
        j <- 1
        crop.parms.lst[[6]] <- misc.mat
        break
      }else{
        j <- j + 1
        break
      }
    }
    
    ## Seventh process wheat growth control
    if(grepl(sugar.start, parms[i], fixed = TRUE)){
      sugar <- TRUE
      sugar.i.1 <- i
    } 
    
    while(sugar && j < 150){
      ## The counter above is to prevent wild things from happening
      ## If it is an empty character we break the while loop
      if(parms[i] == "") break
      
      ## If it has one # and then letters it is a parameter name
      if(grepl("^#[a-z]",parms[i])){
        ## Create vector of such name
        ## For sugar crop this does not exist
        sugar.var.names <- c(sugar.var.names, sub("#", "\\1", parms[i]))
      }else{
        ## We need to make sure that we skip non-parameter lines
        if(!grepl("^#", parms[i])){
          sugar.values <- c(sugar.values, as.numeric(strsplit(parms[i], "!")[[1]][1]))
          sugar.names <- c(sugar.names, strsplit(parms[i], "!")[[1]][2])
        }
      }
      
      ## When the end of the section has been found, create the matrix
      if(grepl(sugar.end, parms[i], fixed = TRUE)){
        sugar.i.2 <- i
        if(verbose) cat("Done with sugarcane control \n")
        
        sugar.mat <- matrix(sugar.values, 
                           ncol = 1, ## At this point this is just one 
                           nrow = length(sugar.names),
                           dimnames = list(as.character(sugar.names), "parameter"))
        sugar <- FALSE
        j <- 1
        crop.parms.lst[[7]] <- sugar.mat
        break
      }else{
        j <- j + 1
        break
      }
    }
    
    ## Eight process residue control
    if(grepl(residue.start, parms[i], fixed = TRUE)){
      residue <- TRUE
      residue.i.1 <- i
    } 
    
    while(residue && j < 150){
      ## The counter above is to prevent wild things from happening
      ## If it is an empty character we break the while loop
      if(parms[i] == "") break
      
      ## If it has one # and then letters it is a parameter name
      if(grepl("^#[a-z]",parms[i])){
        ## Create vector of such name
        ## For sugar crop this does not exist
        residue.var.names <- c(residue.var.names, sub("#", "\\1", parms[i]))
      }else{
        ## We need to make sure that we skip non-parameter lines
        if(!grepl("^#", parms[i])){
          residue.values <- c(residue.values, as.numeric(strsplit(parms[i], "!")[[1]][1]))
          residue.pfts <- c(residue.pfts, strsplit(parms[i], "!")[[1]][2])
        }
      }
      
      ## When the end of the section has been found, create the matrix
      if(grepl(residue.end, parms[i], fixed = TRUE)){
        residue.i.2 <- i
        if(verbose) cat("Done with crop residue control \n")
        
        residue.mat <- matrix(residue.values, 
                            ncol = length(residue.var.names), 
                            nrow = length(unique(residue.pfts)),
                            dimnames = list(unique(as.character(residue.pfts)), residue.var.names))
        residue <- FALSE
        j <- 1
        crop.parms.lst[[8]] <- residue.mat
        break
      }else{
        j <- j + 1
        break
      }
    }
  }
  
  indexes <- c(cgpp.i.1, cgpp.i.2,
               laca.i.1, laca.i.2,
               clmp.i.1, clmp.i.2,
               gdd.i.1, gdd.i.2,
               wheat.i.1, wheat.i.2,
               misc.i.1, misc.i.2,
               sugar.i.1, sugar.i.2,
               residue.i.1, residue.i.2)
  
  crop.parms.lst[[9]] <- parms
  crop.parms.lst[[10]] <- indexes
  ## Return a list
  names(crop.parms.lst) <- c("crop_growth_physiology_properties",
                             "crop_growth_leafarea_c_allocation",
                             "climatic_manage_planting_control",
                             "gdd_phenology_control",
                             "wheat_growth_control",
                             "misc_crop_control",
                             "sugarcane_control",
                             "crop_residue_control",
                             "parms", "indexes")
  return(crop.parms.lst)
}

extd.dir <- "../inst/extdata"
file <- "params_text.crp"

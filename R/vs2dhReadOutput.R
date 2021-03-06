#' Read VS2dh model output file with fluxes at boundary faces 
#' 
#' @param model.path full path to folder containing model output files 
#' @param fileName  name of file containing fluxes at boundary faces
#' (default: "boundaryFluxes.out")
#' @param dbg if true text output 
#' @return read model results of boundary faces are imported in a R object 
#' @examples
#' ### Location of example vs2dh model contained in "kwb.vs2dh package"
#' model.path <- system.file("extdata", "vs2dh_example/tutorial2", package = "kwb.vs2dh")
#' vs2dh.readBoundaryFluxes(model.path)

vs2dh.readBoundaryFluxes <- function(
  model.path, fileName = "boundaryFluxes.out", dbg = TRUE
)
{
  boundaryFluxes <- readLines(file.path(model.path, fileName))
  
  if (length(boundaryFluxes) == 0) {
    
    return(NULL)
  }
  
  header <- boundaryFluxes[1:3]
  
  header[3] <- gsub("TIME STEP", "TIMESTEP", boundaryFluxes[3])
  
  headerSplit <- strsplit(header, split = "\\s+")
  
  header1 <- c("TIME", "bID")
  
  header2 <- paste0(
    headerSplit[[1]][4:9], "_", 
    headerSplit[[2]][3:8], "_", 
    headerSplit[[3]][2:7]
  )
  
  header <- c(header1, header2)
  
  body <- boundaryFluxes[4:length(boundaryFluxes)]
  
  body <- convertStringVectorToMatrix(
    values = body, splitSep = "\\s+", toNumeric = FALSE
  )
  
  stats::setNames(as.data.frame(body[, c(2:ncol(body))]), nm = header)
}

#' Read VS2dh model output file with observation point time series
#' 
#' @param model.path full path to folder containing model output files 
#' @param fileName  name of file containing observation point results 
#' (default: "obsPoints.out")
#' @param dbg if true text output 
#' @return read model results at observation points aare imported in a R object 
#' @examples
#' ### Location of example vs2dh model contained in "kwb.vs2dh package"
#' model.path <- system.file("extdata", "vs2dh_example/tutorial2", package = "kwb.vs2dh")
#' vs2dh.readObsPoints(model.path)
vs2dh.readObsPoints <- function(
  model.path, fileName = "obsPoints.out", dbg = TRUE
)
{
  obsPoints <- readLines(file.path(model.path, fileName))
  
  if (length(obsPoints) == 0) {
    
    return(NULL)
  }
  
  header <- obsPoints[3]
  
  header <- kwb.utils::multiSubstitute(header, list(
    ",\\s+" = "_",
    "\\s+" = ","
  ))
  
  header <- sub(pattern = ",", replacement = "", x = header)
  
  header <- strsplit(x = header,split = ",")[[1]]
  
  body <- obsPoints[4:length(obsPoints)]
  body <- gsub(pattern = "[[:space:]]+", replacement = ",", x =  body)
  body <- sub(pattern = ",", replacement = "", x = body) 
  body <- strsplit(x = body, split = ",")
  
  stats::setNames(nm = header, as.data.frame(matrix(
    as.numeric(unlist(body)), 
    nrow = length(body), 
    byrow = TRUE
  )))
}

#' Helper function: splits header of output file with (energy, fluid) mass 
#' balance time series (balance.out) 
#' 
#' @param row index of row to be split (valid values: 1-3)
#' @param header first three lines of balance.out containing header
#' @return splitted header 

splitHeader <- function (row, header)
{
  tmp <- as.vector(strsplit(x = header[[row]], split = "\\s+"))[[1]]
  
  tmp[seq(from = ifelse(row == 1, 2, 1), to = length(tmp))]
}

#' Read VS2dh model output file with (energy, fluid) mass balance time series 
#' 
#' @param model.path full path to folder containing model output files 
#' @param engine model engine 'vs2dh' (for flow & heat modelling) or 'vs2dt' (for 
#' flow & solute transport) (Default: no default)
#' @param fileName  name of file containing (energy and fluid) balance results 
#' (default: "balance.out")
#' @param dbg if true text output 
#' @return balance model results are imported in a R object 
#' @examples
#' ### Location of kwb.vs2dh package on your computer
#' wDir <- system.file(package = "kwb.vs2dh")
#' model.path <- file.path(wDir, "extdata/vs2dh_example/tutorial2")
#' res <- vs2di.run(model.path = model.path) 
#' vs2di.readBalance(model.path = model.path, engine = "vs2dh")
#' 
vs2di.readBalance <- function(
  model.path, engine, fileName = "balance.out", dbg = TRUE
)
{
  if (engine != "vs2dh") {
    
    return("Import function only working with 'vs2dh' model currently!")
  }
  
  balance <- readLines(file.path(model.path, fileName))
  
  if (length(balance) == 0) {
    
    return(NULL)
  }
  
  header <- balance[1:3]
  
  header[3] <- gsub("TIME STEP", "TIMESTEP", header[3])
  
  header[2] <- kwb.utils::multiSubstitute(header[2], replacements = list(
    "VOL " = "VOL_",
    "SP\\s*" = "SP_",
    "ENERGY " = "ENERGY_",
    "FLOW " = "FLOW_"
  ))
  
  header[1] <- kwb.utils::multiSubstitute(header[1], replacements = list(
    "FLOW " = "FLOW_", 
    "ENERGY IN" = "ENERGY_IN_", 
    "ENERGY OUT" = "ENERGY_OUT_", 
    "TRANS\\s*-" = "TRANSPIRATION", 
    "EVAP\\s*-" = "EVAPORATION", 
    "EVAP\\s*\\+" = "EVAP_plus_TRANS"
  ))
  
  header1 <- splitHeader(row = 1, header = header)
  header2 <- splitHeader(row = 2, header = header)
  header3 <- splitHeader(row = 3, header = header)
  
  header2 <- gsub("PIRATION|ORATION", "", header2)
  
  headerLabel <- paste(header1, header2, header3, sep = "__")
  
  headerLabel[1] <- "TIME"
  
  body <- balance[-(1:3)]
  body <- gsub("\\s+", ",", body)
  body <- sub(",", "", body) 
  body <- strsplit(body, split = ",")
  
  stats::setNames(nm = headerLabel, as.data.frame(matrix(
    as.numeric(unlist(body)), nrow = length(body), byrow = TRUE)
  ))
}

#' Read VS2dh model output file with time series of variables pressure head and
#' temperature  
#' 
#' @param model.path full path to folder containing model output files 
#' @param fileName  name of file containing variable results 
#' (default: "variables.out")
#' @param dbg if true text output 
#' @return variable model results are imported in a R object 
#' @examples
#' ### Location of example vs2dh model contained in "kwb.vs2dh package"
#' model.path <- system.file("extdata", "vs2dh_example/tutorial2", package = "kwb.vs2dh")
#' res <- vs2di.run(model.path = model.path)  
#' vs2dh.readVariables(model.path)

vs2dh.readVariables <- function(
  model.path, fileName = "variables.out", dbg = TRUE
)
{
  variables <- readLines(file.path(model.path,fileName))
  
  if (length(variables) == 0) {
    
    return(NULL)
  }
  
  out <- list()
  
  timeRows <- grep(pattern = "TIME", variables)
  
  out$TIME <- as.numeric(stringr::str_sub(variables[timeRows], 12, 24))
  
  end <- stringr::str_length(variables[timeRows]) - 1
  
  out$Unit <- stringr::str_sub(variables[timeRows], 25, end)
  
  nTimes <- length(timeRows)
  
  ### data points for each time step
  dataPerTime <- (length(variables) - 1 - nTimes * 2 - nTimes + 1) / nTimes
  
  ### Backcalculated NLY parameter (could be directly read from vs2dh.dat!) 
  ### division with two because two parameters "pressure head" & "temp" are 
  ### saved at each time step
  nly <- dataPerTime / 2 
  
  var <- data.frame(ID = c(0, 1), name = c("PressureHead", "Temp"))
  
  for (row in seq_len(nrow(var))) { 
    
    counter <- 0 
    
    for (time in timeRows) {
      
      counter <- counter + 1
      start <- time + 2 + var$ID[row] * nly
      end <- start + nly - 1  
      
      resSplit <- strsplit(sub("\\s+", "", variables[start:end]), split = "\\s+")
      resSplit <- matrix(as.numeric(unlist(resSplit)), nrow = nly, byrow = TRUE)
      
      out[[as.character(var$name[row])]][[counter]] <- resSplit
    }
  }
  
  out
}

#' Helper function: selects pattern contained in different lines of a string vector 
#' (e.g. imported using readLines()) 
#' @param pattern index of row to be split (valid values: 1-3)
#' @param data  vector with strings 
#' @return data.frame with columns lines (indicating the lines where pattern was 
#' found and txt (string contained in that lines)

patternSelect <- function(pattern, data)
{
  lines <- grep(pattern = pattern, x = data)
  
  if (length(lines)) {
    
    data.frame(lines = lines, txt = data[lines])  
    
  } else {
    
    NULL
  } 
}

#' Read main VS2dh model output file "vs2dh.out"   
#'
#' @param model.path full path to folder containing model output files 
#' @param engine model engine 'vs2dh' (for flow & heat modelling) or 'vs2dt' (for 
#' flow & solute transport) (Default: "vs2dh")
#' @return main model results are imported in a R object 
#' @examples
#' ### Location of example vs2dh model contained in "kwb.vs2dh package"
#' model.path <- system.file("extdata", "vs2dh_example/tutorial2", package = "kwb.vs2dh")
#' res <- vs2di.run(model.path = model.path)  
#' vs2di.readMain(model.path = model.path, engine = "vs2dh")
vs2di.readMain <- function(model.path, engine = "vs2dh")
{
  main <- readLines(file.path(model.path, sprintf("%s.out", engine)))
  
  if (length(main) == 0) {
    
    return(NULL)
  }
  
  warn <- patternSelect(
    pattern = "DATA FOR RECHARGE PERIOD|WARNING|EXCEEDED", 
    data = main
  )
  
  warn <- warn[order(warn$lines), ]
  
  list(
    warnings = sprintf("Line: %6d ::: %s\n", as.numeric(warn$lines), warn$txt)
  )
} 

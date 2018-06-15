# library(lattice)
# library(colorRamps)

#' Plot vs2dh.plotMatrix results   
#' 
#' @param data matrix as returned by vs2di.run(model.path, returnOutput=TRUE)
#' @param nodes if TRUE node numbers are used as x-/y-axis; if false spatial extent
#' @param ... further parameters passed to \code{levelplot()}
#' is derived returned in model units (currently not implemented!) 
#' @param maxCol maximum number of colors to be used for colorramp
#' @param  ignoreZeroValues if TRUE 0 values will be set to NA (Default: FALSE)
#' @return Plot matrix values 
#' @examples
#' ### Location of example vs2dh model contained in "kwb.vs2dh package"
#' model.path <- system.file("extdata", "vs2dh_example/tutorial2", package = "kwb.vs2dh")
#' res <- vs2di.run(model.path = model.path)  
#' vs2dh.plotMatrix(data=res$variables$PressureHead[[1]])  

vs2dh.plotMatrix <- function(data,
                             nodes = TRUE, 
                             maxCol = 100,
                             ignoreZeroValues = FALSE,
                             ...
)
{
  
  transMatrixData <- t(data)[,nrow(data):1, drop = FALSE]
  
  if (ignoreZeroValues) transMatrixData[transMatrixData == 0] <- NA
  
  if (nodes == TRUE)
  {
    lattice::levelplot(transMatrixData,
                       col.regions = colorRamps::blue2red(n = maxCol),
                       ...
    )
  } else {
    cat("Spatial extent in x,y still needs to be implemented")
    #   #### Spatial extent in x,y 
    #   DELZ <-  vs2dh.readGridSpacing(model.path, para="DELZ")
    #   DXR <- vs2dh.readGridSpacing(model.path, para="DXR")
    #   
    #   image(x=cumsum(DXR), y=cumsum(DELZ), z=transMatrixData)
    #   
  }
  
}

#' Plot vs2dh.plotVariables matrix time series
#' 
#' @param para either "Temp" (for temperature) or "PressureHead" 
#' @param data as returned by vs2di.run() in sublist "variables"
#' @param ignoreBoundary should boundary nodes (first/last column/row) be ignored?
#' Default: TRUE
#' @param  ignoreZeroValues if TRUE 0 values will be set to NA (Default: TRUE)
#' @param paraLabel optional parameter for changing the label text of the parameter
#' (if not used the value in "para" is used for labelling)
#' @param maxCol maximum number of colors to be used for colorramp
#' @param main Additional text for main label (e.g. model name)
#' @param nodes if TRUE node numbers are used as x-/y-axis; if false spatial extent
#' is derived returned in model units (currently not implemented!) 
#' @param ... further parameters passed to \code{levelplot()}
#' @return Time series matrix plot
#' @examples
#' ### Location of example vs2dh model contained in "kwb.vs2dh package"
#' modelName <- "tutorial2" 
#' model.path <- system.file("extdata", "vs2dh_example", modelName, package = "kwb.vs2dh")
#' res <- vs2di.run(model.path = model.path)  
#' vs2dh.plotVariables ( para = "Temp",
#'                      data = res$variables,                    
#'                      paraLabel = "Temperature (\u00B0\ C)",
#'                      main = modelName)  
#' vs2dh.plotVariables (para = "PressureHead", 
#'                      data = res$variables, 
#'                      paraLabel = "Pressure head (m)",
#'                      main = modelName
#'                     )  



vs2dh.plotVariables <- function(para="Temp",
                                  data,
                                  ignoreBoundary = TRUE,
                                  ignoreZeroValues = TRUE,
                                  paraLabel=NULL,
                                  maxCol = 100,
                                  main = "",
                                  nodes = TRUE,
                                  ...
)
{
  if (is.null(paraLabel)) paraLabel <- para
  
  time <- data$TIME
  unit <- data$Unit[1]
  
  for (selTime in 1:length(time))
  {
    mainLabel <- sprintf("%s %s distribution after %4.2f (%s)",
                         sprintf("%s:", main),
                         paraLabel, 
                         time[selTime],
                         unit
    ) 
    
    selData <- data[[para]][[selTime]]
    
    if (ignoreBoundary == TRUE) {
      selData <- selData[2:(nrow(selData) - 1),
                         2:(ncol(selData) - 1),drop = FALSE]
    }
    
    print(vs2dh.plotMatrix( selData,
                            nodes = nodes, 
                            maxCol = maxCol,
                            main = mainLabel,
                            ignoreZeroValues = ignoreZeroValues,
                            xlab = "column number",
                            ylab = "row number",
                            ...
    ))
    
  }
}

#' Spatial plotting of observation point time series
#' 
#' @param paras vector with at least one of the following parameters: H_m (total head),
#' P_m (pressure head), THETA (moisture content), SAT (saturation), TEMP 
#' (for temperature), VX (velocity in x-direction), VZ (velocity in z-direction), 
#' ET (evapotransporation)
#' @param data as returned by vs2di.run() in sublist "obsPoint"
#' @param paraLabel optional parameter for changing the label text of the parameters
#' defined in \code{paras} (if not used the value in "paras" is used for labelling)
#' @param maxCol maximum number of colors to be used for colorramp
#' @param ... further parameters passed to \code{levelplot()}
#' @return Spatial plots of observation point time series
#' @examples
#' ### Location of example vs2dh model contained in "kwb.vs2dh package"
#' model.path <- system.file("extdata", "vs2dh_example/tutorial2", package = "kwb.vs2dh")
#' res <- vs2di.run(model.path = model.path)  
#' vs2dh.plotObservationPoints (paras="TEMP",
#'                              data=res$obsPoints,
#'                              paraLabel="Temperature (\u00B0\ C)")  


vs2dh.plotObservationPoints <- function(paras="Temp",
                                         data,
                                         paraLabel=NULL,
                                         maxCol = 100,
                                         ...
)
{
  if (is.null(paraLabel)) paraLabel <- paras
  
  data$TIME_label <- as.factor(sprintf("%s days", data$TIME_day))
  selPara <- "TEMP"
  for ( para in paras)
  {
    index <- which(paras == para)
    mainLabel <- sprintf("%s distribution for observation points",paraLabel[index]) 
    myFormula <- stats::as.formula(sprintf("%s ~ XR_m + (-Z_m) | TIME_label", para))
    print(lattice::levelplot(
      myFormula,
      data = data, 
      main = mainLabel,
      layout = c(1, 1), 
      col.regions = colorRamps::blue2red(n = maxCol), 
      ...
    ))
  }
}

#' Plotting of mass balance time series
#' 
#' @param paras vector with at least one or multiple parameters in model results, 
#' for checking available paras run (see example: validParas). The parameter "TIME"
#' is not allowed!
#' @param paraUnit TIMESTEP RATE or 
#' @param data as returned by vs2di.run()$balance
#' @param mainLabel a text to be written above the plot
#' @param ... further parameters passed to \code{xyplot()}
#' @return Temporal time series plot of mass balance variables 
#' @examples
#' ### Location of example vs2dh model contained in "kwb.vs2dh package"
#' model.path <- system.file("extdata", "vs2dh_example/tutorial2", package = "kwb.vs2dh")
#' res <- vs2di.run(model.path = model.path)  
#' #### Checking available parameter names for "paras":
#' validParas <- gsub(pattern="__TOTAL|__RATE|__TIMESTEP|TIME", 
#'                    replacement = "", 
#'                    colnames(res$balance))
#' validParas 
#' ### Flow mass balance components (inflow, outflow, storage):
#' vs2dh.plotMassBalance(data=res$balance)  
#' ### Only resulting flow mass balance error (per timestep):
#' vs2dh.plotMassBalance(paras="FLUID__VOL_BAL", 
#'                        data = res$balance, 
#'                      mainLabel = "Flow mass balance error")  
#' ### Energy mass balance components (inflow, outflow, storage):
#' vs2dh.plotMassBalance(paras=c("TOTAL__ENERGY_IN",
#'                               "TOTAL__ENERGY_OUT",
#'                               "ENERGY__STORAGE"),
#'                               data=res$balance,
#'                               mainLabel="Energy mass balance") 
#' ### Only resulting energy mass balance error (per timestep):
#' vs2dh.plotMassBalance(paras="ENERGY__BALANCE",
#'                        data = res$balance,
#'                     mainLabel = "Energy mass balance error")   


vs2dh.plotMassBalance <- function(paras=c("TOTAL__FLOW_IN",
                                            "TOTAL__FLOW_OUT",
                                            "FLUID__STORAGE"),
                                    paraUnit = "TIMESTEP",
                                    data,
                                    mainLabel="Flow mass balance",
                                    ...
)
{
  
  available <- data.frame(paraUnit = c("TOTAL", "TIMESTEP", "RATE"),
                          paraUnitLabel = c("total", "per time step", "rate"))
  
  index <- which(available$paraUnit == paraUnit)
  
  mainLabel <- sprintf("%s ( %s )", mainLabel,  as.character(available$paraUnitLabel[index]))
  
  parasWithUnit <- sprintf("%s__%s", paras, paraUnit)
  
  myFormula <- stats::as.formula(sprintf(
    "%s ~ TIME", paste(parasWithUnit, collapse = " + ")
  ))
  
  print(lattice::xyplot(
    myFormula, 
    auto.key = list(points = FALSE, lines = TRUE), 
    data = data,
    type = "l",
    ylab = sprintf("Values ( %s )", available$paraUnitLabel[index]), 
    xlab = "Time (days)",
    main = mainLabel,
    ...
  ))
}



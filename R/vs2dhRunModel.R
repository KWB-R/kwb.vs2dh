#' Create vs2dh.fil or vs2dt.fil file  
#' 
#' @param engine model engine either 'vs2dh' or 'vs2dt'  
#' @param model.path full path to folder containing vs2dh.dat 
#' @param dbg if true text output 
#' @return Saves/Overwrites vs2dh.fil in folder \code{model.path} 
#' @examples
#' ### Location of example vs2dh model contained in "kwb.vs2dh package"
#' model.path <- system.file("extdata", "vs2dh_example/tutorial2", package = "kwb.vs2dh")
#' vs2di.createFileDat(engine = "vs2dh", 
#'                     model.path = model.path)

vs2di.createFileDat <- function(engine,
                                model.path,
                                dbg=TRUE) {
 
  fileNames = list(
    boundaryFluxes = "boundaryFluxes.out", 
    variables = "variables.out", 
    balance = "balance.out",
    obsPoints = "obsPoints.out")
  
  allFileNames <- c(
    sprintf("%s.dat", engine), 
    sprintf("%s.out", engine), 
    fileNames$boundaryFluxes, 
    fileNames$variables,
    fileNames$balance,
    fileNames$obsPoints,
    sprintf("#%s3.3", engine) 
  )
  
  txt <- paste(allFileNames, collapse = "\n")
  
  if (dbg == TRUE) 
  {
    cat(sprintf("Write/Overwrite %s.fil in folder:\n%s\n", 
                engine,
                model.path))
  }
  
  write(x = txt, file = file.path(model.path, 
                                  sprintf("%s.fil", engine)))
  
  if (dbg == TRUE)
  {
    cat("Done!\n\n")
    cat("Output files:\n")
    cat(sprintf("Variables: %s\nMass balance: %s\nObservation points: %s\n\n", 
                fileNames$variables,
                fileNames$balance,
                fileNames$obsPoints))
  }
}

#' Helper function: converts string to windows path 
#' 
#' @param path path string to be converted to windows path 
#' @param sep  seperator which default: "/", which is used on unix and for R) 
#' @examples
#' unixPath <- "/usr/model/vs2dh" 
#' winPath <- convertToWindowsPath(path=unixPath)
#' winPath

convertToWindowsPath <- function(path, 
                                 sep = "\\/"
)
{
  windowsPath <- gsub(pattern = sep, 
                      replacement = "\\\\", 
                      x = path)
  return(windowsPath)
}

#' Helper function: checks whether operating system is windows or linux 
#' 
#' @examples
#' os <- checkOperatingSystem() ### if function shell() is not available -> linux"
#' os ### return operating system

checkOperatingSystem <- function()
  if (exists("shell")) 
  {
    os <- "windows"
  } else {
    os <- "linux"
    
    return(os)
  }

#' Run VS2dh model 
#' 
#' @param engine model engine 'vs2dh' (for flow & heat modelling) or 'vs2dt' (for 
#' flow & solute transport) (Default: "vs2dh")
#' @param engineDirectoryWin default directory on Windows OS containing vs2dh3_3.exe and 
#' vs2dt3_3.exe (Default: system.file("extdata/engine/win", package = "kwb.vs2dh"))
#' @param engineDirectoryLinux default directory on Linux OS containing vs2dh3_3.exe and 
#' vs2dt3_3.exe (Default: system.file("extdata/engine/linux", package = "kwb.vs2dh"))
#' @param model.path full path to folder containing vs2dh.dat (default: 
#' system.file("extdata", "vs2dh_example/tutorial2", package = "kwb.vs2dh"))
#' @param returnOutput if TRUE model output files variables.out, balance.out and
#' obsPoints.out are imported into R completely; vs2dh.out (only warnings imported) 
#' @param showWarnings if TRUE print warning messages during simulation on screen 
#' (default: TRUE)
#' @param openTargetDir If TRUE path containing model files will be opened in 
#' explorer (Default: FALSE)
#' @param dbg if true text output on screen on model run progress
#' @return Run VS2dh model and saves model output files in folder \code{model.path}.
#' If \code{returnOutput} is TRUE the output is imported in R object
#' @examples
#' ### Running model with default model.path and engine.path: 
#' model.path <- system.file("extdata", "vs2dh_example/tutorial2", package = "kwb.vs2dh")
#' res <- vs2di.run(model.path = model.path)

vs2di.run <- function(engine = "vs2dh", 
                      engineDirectoryWin = system.file("extdata/engine/win", 
                                                       package = "kwb.vs2dh"), #"C:/Program Files (x86)/USGS/VS2DI_1.3/bin",
                      engineDirectoryLinux = system.file("extdata/engine/linux", 
                                                         package = "kwb.vs2dh"),
                      model.path = system.file("extdata", "vs2dh_example/tutorial2", 
                                               package = "kwb.vs2dh"),
                      returnOutput = TRUE, 
                      showWarnings = TRUE,
                      openTargetDir = FALSE, 
                      dbg=TRUE)
  
{ 
  #### Compilte Fortran code
  ### gfortran vs2dh.f -o vs2dh.exe -I/home/micha/RPackages/kwb.vs2dh/src 
  ###engine.path <- "/home/micha/RPackages/kwb.vs2dh/inst/extdata/vs2dh.exe"
  
  if (dbg == TRUE) cat(sprintf("1. Step: Create %s.fil...", engine))
  vs2di.createFileDat(engine = engine, 
                      model.path = model.path, 
                      dbg = FALSE)
  if (dbg == TRUE) cat("Done!\n\n")
  
  engineExecutable <- sprintf("%s3_3.exe", engine)
  cmd <- sprintf('cd %s & "%s"', model.path, file.path(engineDirectoryWin, engineExecutable))
  if (dbg == TRUE) cat("\n2. Step: Run model...")
  
  os <- checkOperatingSystem()
  if (os == "windows") 
  {
    cmd <- convertToWindowsPath(cmd)
    runTime <- system.time(shell(cmd))
  } else 
  {
    ### OS is Linux 
    runTime <- system.time(system(cmd))
  }
  
  if (dbg == TRUE) cat(sprintf("finished after %4.1f seconds\n", runTime[3]))
  
  if (dbg == TRUE & returnOutput == TRUE) cat("\n4. Step: Import model results...")
  if (returnOutput == TRUE)
  {
    output <- list(
      main = vs2di.readMain(model.path = model.path,
                            engine = engine),
      boundaryFluxes = vs2dh.readBoundaryFluxes(model.path),
      variables = vs2dh.readVariables(model.path), 
      balance = vs2di.readBalance(model.path = model.path,
                                  engine = engine),
      obsPoints = vs2dh.readObsPoints(model.path)
    )
  }
  if (dbg == TRUE & returnOutput == TRUE) cat("Done!\n")
  if (showWarnings == TRUE) 
  {
    
    warnings <- vs2di.readMain(model.path, engine)$warnings
    anyWarnings <- any(grepl(pattern = "WARNING|EXCEEDED PERMITTED", warnings) == TRUE)
    if (anyWarnings) 
    {
      cat("\nPrint warnings during simulation (vs2dh.out):\n")
      cat(warnings)
    } else {
      cat("\nNo warnings during simulation (vs2dh.out)\n")
    }
  }
  if (openTargetDir == TRUE) 
  {
    kwb.utils::hsOpenWindowsExplorer(model.path)
  }
  
  if (returnOutput == TRUE) return(output)
}

#' Run configuration with VS2dh model  
#' 
#' @param conf as retrieved by vs2dh.ReadConfig (i.e. importing a working VS2DH 
#' configuration from an vs2h.dat file) or completley in R using function 
#' vs2dh.Configure() (ATTENTION: DEFAULT PARAMETERISATION FOR vs2dh.Configure() 
#' CURRENTLY YIELDS NO RUNNING VS2DH MODEL CONFIGURATION)
#' @param engine model engine 'vs2dh' (for flow & heat modelling) or 'vs2dt' (for 
#' flow & solute transport) (Default: "vs2dh")
#' @param tDir target directory where vs2dh model input/output files should be 
#' stored. (Default: tempdir())
#' @param returnOutput if TRUE model output files variables.out, balance.out and 
#' obsPoints.out are imported into R completely; vs2dh.out (only warnings imported) 
#' (Default: TRUE)
#' @param openTargetDir If TRUE path containing model files will be opened in 
#' explorer (Default: FALSE)
#' @param showWarnings if TRUE print warning messages during simulation on 
#' screen (default: TRUE)
#' @param dbgRun if true text output on screen on model run progress
#' @param dbg if true text output on screen on additional model run progress
#' 

#' @return Import & write VS2dh model results in R object 
#' @examples 
#' \dontrun{
#' ### Testing import and writing functions with 
#' ### Folder which contains the subfolders with the different models to test: 
#' model.main.path <- system.file("extdata", "vs2dh_example", package = "kwb.vs2dh")
#' #### Using vs2dh.dat file contained in subfolders example1, example2, tutorial1
#' #### for testing (i.e. ignore example3 due to long run time, but this also works!)
#' exampleModels <- c(paste0("example", 1:2), "tutorial1")
#' ### Create result list in R for storing model outputs for each model in a separate
#' ### sublist 
#' res <- list()
#' #### Loop through all example model input files & plot temperature distribution
#' for (testModel in exampleModels)
#' {
#'  model.path <- file.path(model.main.path, testModel)
#'  cat(sprintf("Testing model in %s\n", model.path ))
#'  cat("1.Step: importing configuration to R.....")
#'  conf <- vs2dh.ReadConfig(model.path = model.path)
#'  cat("Done!")
#'  cat("2.Step: Run vs2di.runConfig():")
#'  tDir <- file.path(tempdir(), testModel)
#'  res[[testModel]] <- vs2di.runConfig(conf = conf, tDir = tDir)
#'  cat("3.Step: Plot temperature distribution...")
#'  vs2dh.plotVariables(para = "Temp", 
#'                      data = res[[testModel]]$variables,
#'                      main = testModel  ### nice label by using model folder name
#'                     )
#'   cat("Done!")
#' } 
#' }
vs2di.runConfig <- function(
  conf, 
  engine = "vs2dh", 
  tDir = tempdir(), 
  returnOutput = TRUE, 
  openTargetDir = TRUE,
  showWarnings = TRUE,
  dbgRun = TRUE, 
  dbg = TRUE
)
{
  if (dbg == TRUE) cat("1.Step: creating target directory or delete all files in that folder ...")
  dir.create(tDir)
  unlink( file.path(tDir, "*"), recursive = FALSE)
  tPath <- file.path(tDir, sprintf("%s.dat", engine) )
  if (dbg == TRUE) 
  {
    cat("Done!\n")
    cat("2.Step: convert R configuration to FORTRAN style...")
  }
  inpDat <- vs2dh.writeConfig(conf)  
  if (dbg == TRUE) 
  {
    cat("Done!\n")
    cat(sprintf("3.Step: writing configuration to '%s'....", tPath))
  }
  write(inpDat, tPath)
  
  
  if (dbg == TRUE)  
  {
    cat("Done!\n")
    cat(sprintf("3.Step: Running vs2dh model in '%s'....", tPath))
  }
  res <- vs2di.run(engine = engine, 
                   model.path = tDir,
                   returnOutput = returnOutput,
                   openTargetDir = openTargetDir,
                   showWarnings = showWarnings, 
                   dbg = dbgRun)
  
  if (dbg == TRUE) cat("Done!\n")
  return(res)
}

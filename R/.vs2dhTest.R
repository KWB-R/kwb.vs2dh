###library(R.utils)
rPath<- "C:/Users/mrustl/Documents/WC_Server/R_Development/trunk/RPackages/kwb.vs2dh/R"
sourceDirectory(path=rPath,pattern="^[^.].*$")

# mainPath <- "C:/Users/mrustl/Desktop/vs2dhi/examples/energy/"
# model.path <- file.path(mainPath, "tutorial1")
# 
# model.path <- system.file("extdata", "vs2dh_example/tutorial2", package = "kwb.vs2dh")
 system.file("extdata",  "InputDescription.xls", package = "kwb.vs2dh")
model.path <- "C:/Users/mrustl/Documents/WC_Server/R_Development/trunk/RPackages/kwb.vs2dh/inst/extdata/vs2dh_example/tutorial2"
mdb.path <- "C:/Users/mrustl/Documents/WC_Server/R_Development/trunk/RPackages/kwb.vs2dh/inst/extdata/InputDescription.xls"

conf <- vs2dh.ReadConfig(model.path=model.path)
vs2di.runConfig(conf )
#model.path <- "C:/Users/mrustl/Desktop/vs2dhi/examples/energy"

# model.path <- "C:/Users/mrustl/Documents/WC_AWS_Rrocks/RDevelopment/Packages/kwb.vs2dh/inst/extdata/vs2dh_example/tutorial2"
# mdb.path <- "C:/Users/mrustl/Documents/WC_AWS_Rrocks/RDevelopment/Packages/kwb.vs2dh/inst/extdata/InputDescription.xls"

preparedData <- prepareImport(model.path=model.path,mdb.path = mdb.path)
conf <- vs2dh.ReadConfig(model.path)
single <- importSingleLineParas(prepData=preparedData)

single$basic$tiread
# 
# foodweb(prune = "importSingleLineParas")
# 
# 
# prepData <- preparedData

### Recharge period selection

vs2di.runConfig <- function (conf, tDir = tempdir(), dbg=TRUE)
{
  if (dbg==TRUE) cat("1.Step: creating target directory or delete all files in that folder ...")
  dir.create(tDir)
  unlink( file.path(tDir, "*"), recursive = FALSE)
  tPath <- file.path(tDir, "vs2dh.dat" )
  if (dbg==TRUE) 
  {
    cat("Done!\n")
    cat("2.Step: convert R configuration to FORTRAN style...")
  }
  inpDat <- vs2dh.writeConfig(conf)  
  if (dbg==TRUE) 
  {
    cat("Done!\n")
    cat(sprintf("3.Step: writing configuration to '%s'....", tPath))
  }
  write(inpDat, tPath)
  if (dbg==TRUE)  
  {
    cat("Done!\n")
    cat(sprintf("3.Step: Running vs2dh model in '%s'....", tPath))
  }
  res <- vs2di.run(tDir)
  if (dbg==TRUE) cat("Done!\n")
  kwb.utils::hsOpenWindowsExplorer(tDir)
  return(res)
}

exampleModels <- c(paste0("example", 1:3), "tutorial1")
fixed.path <- "C:/Users/mrustl/Desktop/vs2dhi/examples/energy"
res <- list()
for (testModel in exampleModels)
{
  model.path <- file.path(fixed.path, testModel)
  cat(sprintf("Testing model in %s\n", model.path ))
  cat("1.Step: importing configuration to R.....")
  conf <- vs2dh.ReadConfig(model.path=model.path, mdb.path=mdb.path)
  cat("Done!\n")
  cat("2.Step: Run vs2di.runConfig():\n")
  res[[testModel]] <- vs2di.runConfig(conf, tDir = file.path(tempdir(), testModel))
  cat("3.Step: Plot temperature distribution:\n")
  vs2dh.plotVariables(para = "Temp", data = res[[testModel]]$variables)
} 
                           
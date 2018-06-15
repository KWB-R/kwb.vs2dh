#' Helper function: Cut string by pattern
#' 
#' @param pattern pattern to be searched for in string  \code{txt}; if pattern 
#' is a vector of two the string is cut between pattern[1] and pattern[2]
#' @param endString if TRUE the string after the search pattern is returned, if 
#' FALSE from the beginning until the search pattern (only evaluated if pattern 
#' \code{pattern} is vector of length 1!)
#' @param txt string or vector of strings to be searched for in string 
#' @return Returns only partial string(s) depending on the cut pattern and until 
#' the end of each string 
#' @examples
#' ### Path to your vs2dh model directory
#' string = "A B C D"
#' pattern = "B "
#' cutStringByPattern(pattern=pattern, txt=string)

cutStringByPattern <- function (pattern,
                                endString=TRUE,
                                txt)
{
  strings <- data.frame(txt=txt,
                        Line = 1:length(txt)
  )
  
  if(length(pattern) %in% c(1,2))
  {
    if(length(pattern)==1)
    {
      search  <-  regexpr(pattern = pattern , 
                          text = strings$txt)
      
      condition <- search!=-1
      
      strings <- strings[condition,] 
      
      if (endString==TRUE)
      {
        start <- as.numeric(search[condition]) + attr(x = search, which = "match.length")[condition]
        end <- stringr::str_length(strings$txt)
      } else {
        start <- 1
        end <- as.numeric(search[condition])-1
      }} else {
        
        search1  <-  regexpr(pattern = pattern[1] , 
                             text = strings$txt)
        search2  <-  regexpr(pattern = pattern[2] , 
                             text = strings$txt)
        
        condition <- search1!=-1 & search2!=-1
        
        strings <- strings[condition,] 
        
        start <- as.numeric(search1) + attr(search1,"match.length")
        end <- as.numeric(search2[condition]) 
      }
    strings$parNames <- stringr::str_sub(string = strings$txt,
                                         start = start,
                                         end = end)
    attr(x = strings, which = "indexes") <- which(condition==TRUE)
    return(strings)
    
  } else {
    cat(sprintf("Pattern vector of length %d not possible.\nPlease specificy pattern 
                 vector with length 1 or 2", length(pattern)))
  }
}

#' Helper function: convert string vector to numeric matrix 
#' 
#' @param values character vector to be converted to matrix 
#' @param splitSep seperator used as split parameter in function strsplit (Default: " ") 
#' @param byrow If TRUE matrix is filled by rows first. Default: TRUE
#' @param rows  for constructing matrix: Default: length(values)
#' @param toNumeric If true values will be converted to numeric, if FALSE not
#' @return Returns matrix with nrows equals to length of values input vector

convertStringVectorToMatrix <- function(values, 
                                        splitSep=" ", 
                                        byrow=TRUE,
                                        rows = length(values),
                                        toNumeric=TRUE)
{
  valuesVector <- unlist(strsplit(values,split = splitSep))
  
  if (toNumeric==TRUE) valuesVector <- as.numeric( valuesVector )
  
  x <- matrix(data = valuesVector,byrow = byrow,
              nrow = rows)
  
  return(x)
}



################################################################################

#' Helper function: filtering lines 
#' 
#' @param pattern pattern which lines are searched
#' @param patternNameCol  for pattern like "/B25" use: "txt", for para names 
#' use "paraNames", (Default: "txt") 
#' @param data as retrieved by prepareImport()$res
#' @param startLineOffset offset of starting line: 0 (if parName is in same row 
#' as parVal, 1 if parVal is one row after parName)
#' @param lineCol Default "Line"
#' @param input as retrieved by prepareImport()$input

filterLines <- function (pattern, 
                         patternNameCol="txt",#### for referencing by /B25
                         data,
                         startLineOffset=0,
                         input,
                         lineCol="Line")
{ 
  # data <- data[order(data[,lineCol]),]
  #  data$LineDiff <- c(diff(data[,lineCol]),0)
  
  row <- grep(pattern = pattern, data[, patternNameCol])
  
  tmp <- data[row,]
  if(nrow(tmp)>0)
  {
    startLine <- tmp[,lineCol] +  startLineOffset
    endLine <- tmp[,lineCol] + tmp$LineDiff - 1
    x <- input[startLine:endLine]
    return(x)
  } else {
    warning(sprintf("No parameter with pattern '%s' was found in input file)",pattern ))
  }
  
  
}
###############################################################################

#' Helper function: multipleLineValues
#' 
#' @description  Writes one vector for values in multiple lines  
#' that beginn in FORTRAN "in the next line" (e.g. DELZ, DXR)
#' @param parName e.g. "PLTIM", "DELZ" or "DXR"
#' @param data as retrieved by  prepareImport()
#' @param patternNameCol  for pattern like "/B25" use: "txt", for para names 
#' use "paraNames", (Default: "txt") -> used by function: filterLines()
#' @param startLineOffset offset of starting line: 0 (if parName is in same row 
#' as parVal, 1 if parVal is one row after parName), -> used by function: filterLines()
#' @param toNumeric If TRUE result is converted to numeric (Default: TRUE)
#' @return List with one vector of multiple lines values for each parameter


multipleLineValues <- function (parName="DELZ", 
                                data, 
                                patternNameCol="parNames",
                                startLineOffset=1,
                                toNumeric=TRUE)
{ 
  values <- filterLines(pattern = parName, 
                        data =  data$res, 
                        patternNameCol=patternNameCol,
                        startLineOffset = startLineOffset,
                        input = data$input)
  x <- list()
  
  values <- unlist(strsplit(values,split = "\\s+"))
  if (toNumeric) values <- as.numeric(values)
  
  x[[tolower(parName)]] <- values
  return(x)
}



#' Helper function: prepareImport (imports and prepares input file)
#' 
#' @param model.path full path to folder containing vs2dh.dat 
#' @param mdb.path full path to the folder containing the InputDescription.xls
#' which is used as lookup table for converting FORTRAN to R parameter names 
#' (Default: system.file("extdata", "InputDescription.xls", package = "kwb.vs2dh"))
#' @param dbg if TRUE text output is printed on the screen 
#' @return Read vs2dhi.dat and does some preparations steps which are stores in a list
#' which contains the sublists "input" (raw imported vs2dh.dat file), "inpDescription" 
#' (lookup table defined in file "mdb.path"), "res" (prepared table with parameters names 
#' and values) and  "rechargePeriods" (summary of start/endlines of file "input" for 
#' each recharge period)
#' @examples
#' \dontrun{
#' model.path <- system.file("extdata", "vs2dh_example/tutorial2", package = "kwb.vs2dh")
#' prepareImport(model.path)
#' }
#' 
prepareImport   <- function(
  model.path ,
  mdb.path = system.file(
    "extdata", "InputDescription.xls", package = "kwb.vs2dh"
  ),
  dbg = TRUE
)
{
  filePath <- file.path(model.path,"vs2dh.dat")
  
  
  if(dbg == TRUE) cat(sprintf("Importing parameter description from file '%s'....\n\n", mdb.path))
  paras <- kwb.db::hsGetTable(mdb = mdb.path,tbl = "paras")
  comment <- kwb.db::hsGetTable(mdb = mdb.path,tbl = "comment")
  meta <- kwb.db::hsGetTable(mdb = mdb.path,tbl = "meta")
  inpDescription <- list()
  inpDescription$paras <- merge(merge(paras, comment, all.x=TRUE),meta,all.x=TRUE)
  inpDescription$massBalance <- kwb.db::hsGetTable(mdb = mdb.path,tbl = "massBalance")
  if(dbg == TRUE) cat("\n......Done!!!\n\n")
  
  input <- readLines(filePath)
  
  ### 2) All single line input parameters
  res <- cutStringByPattern(pattern="[A-C]\\d+A? -- ",
                            txt=input
  )
  res$LineLabel <- cutStringByPattern(pattern = c("/"," --"),txt = res$txt)$parNames
  
  res2 <- cutStringByPattern(pattern="begins next line: |to begin next line: ",
                             txt=input
  )  
  res2$LineLabel <- cutStringByPattern(pattern = c("/"," --"),txt = res2$txt)$parNames
  res2$txt <- ""
  res2$Line <- res2$Line + 1
  res <- rbind(res,res2)
  res$LineLabel <- gsub(pattern = "\\s+",replacement = "", res$LineLabel)
  ind <- which(res$txt!="")
  res$parVals <- ""
  res$parVals[ind] <- cutStringByPattern(pattern = "/", 
                                         endString = FALSE, 
                                         txt = res$txt[ind])$parNames
  
  
  selHK <- res[res$parNames=="HK",]
  selHK$row <- which(rownames(res) %in% rownames(selHK))
  res$parVals[selHK$row] <- input[selHK$Line]
  
  #### Identify lines where recharge periods start/end
  startLine <- res[grep("Recharge Period", res$parNames), "Line"]
  endLine <- res[grep("End of data for recharge period", res$parNames),"Line"] 
  
  rechargePeriods <- data.frame(number=1:length(startLine),
                                startLine=startLine,
                                endLine=endLine)
  
  
  sel <- cutStringByPattern(pattern = "\\.", 
                            endString = FALSE, 
                            txt = res$parNames)
  
  sel$rowsInRes <- attr(x = sel, which = "indexes") 
  
  res[sel$rowsInRes, "parNames"] <- sel$parNames
  
  res$parNames <- gsub(pattern="\\(NTC,\\s+CF\\)",replacement = "NTC, CF", res$parNames)
  res$parNames <- gsub(pattern="\\(not used\\)", replacement = "", res$parNames)
  res$parNames <- gsub(pattern="\\s+\\(Recharge Period\\s+\\d+\\)",replacement = "", res$parNames)
  res <- res[order(res$Line),]
  res$LineDiff <- c(diff(res$Line),0)
  x <- list(input=input,
            inpDescription = inpDescription,
            res=res,
            rechargePeriods=rechargePeriods)
  return(x)
}


#' Helper function: imports single line parameters 
#' 
#' @description FORTRAN parameters with always correspond to only one value  
#' @param prepData object as retrieved by prepareImport()
#' @param dbg prints debug information on the screen
#' @return Single line values with naming parameter names in R style (before 
#' part C, i.e. recharge period 1 starts)

importSingleLineParas <- function(prepData,
                                  dbg = TRUE)
{
  
  #  grepl(pattern = "IU",x = prepData$res$parNames))  
  prepData$res$nVals <- unlist(lapply(strsplit(prepData$res$parVals, split = "\\s+"),FUN = length))
  prepData$res$nParNames <- unlist(lapply(strsplit(prepData$res$parNames, split = "\\s+"),FUN = length))
  prepData$res$diffVals <- prepData$res$nVals - prepData$res$nParNames
  oneLinerRows <- which((prepData$res$LineDiff==1  | prepData$res$parNames=="NOBS" | 
                           grepl(pattern = "IU",x = prepData$res$parNames)) & 
                          !grepl(pattern = "ITEX|IDBF|NUMCELLS",x = prepData$res$parNames) &
                          prepData$res$Line < prepData$rechargePeriods$startLine[1])
  
  oneLineParas <- prepData$res[oneLinerRows,]
  
  ##############################################################################
  #### 1) Parameters with one value per parameter
  ##############################################################################
  oneLineParasWithOneValue <- oneLineParas[oneLineParas$diffVals == 0,]
  
  #### Use lookup table to automatically change FORTRAN names to Rnames:
  
  
  parNamesSplit <- gsub("\\s+", "", unlist(strsplit(oneLineParasWithOneValue$parNames, split = ",\\s+")))
  
  duplicateParas <- c("IREAD", "FACTOR")
  for (id in  1:length(duplicateParas))
  {
    index <- grep(pattern = duplicateParas[id], parNamesSplit)
    
    parNamesSplit[index[1]] <-  sprintf("H%s", parNamesSplit[index[1]])
    if (parNamesSplit[index[1]+2] %in% c("IU", "IFMT"))
    {
      for (i in 1:2)
      {
        parNamesSplit[index[1]+i] <-  sprintf("H%s", parNamesSplit[index[1]+i])
      }
    }
    
    if (length(index) == 2)
    {
      parNamesSplit[index[2]] <-  sprintf("T%s", parNamesSplit[index[2]]) 
      if (parNamesSplit[index[2]+2] %in% c("IU", "IFMT"))
      {
        for (i in 1:2)
        {
          parNamesSplit[index[2]+1+i] <-  sprintf("T%s", parNamesSplit[index[2]+1+i])
        }
      }
      
    }
  }
  
  parValsSplit <- unlist(strsplit(oneLineParasWithOneValue$parVals, split = "\\s+"))
  parValsSplit <- gsub("F", "FALSE", parValsSplit)
  parValsSplit <- gsub("T", "TRUE", parValsSplit)
  parValsSplit <- gsub(pattern="'", "", parValsSplit)
  
  lookupTable <- prepData$inpDescription$paras[,c("Line", "Variable","Rname")]
  AB_part <- grep(pattern = "A|B", x = lookupTable$Line) 
  lookupTable <- lookupTable[AB_part,]
  lookupTable$Line <- as.character(lookupTable$Line)
  
  
  tmp <- data.frame(parNames = parNamesSplit,
                    parVals = parValsSplit)
  
  
  ### Ignore ITEX para: 
  tmp <- tmp[!(tmp$parNames %in% c("ITEX", "adsorptionType")), ]
  
  tmp <- merge(tmp, lookupTable, by.x = "parNames", by.y="Variable", all.x=TRUE)
  
  ##### List with single parameter values
  #x <- list()
  x <- lapply( X = as.character(tmp$parVals), FUN=c)
  names(x) <- as.character(tmp$Rname)
  
  title <- list(titl=prepData$input[1])
  x <- c(x, title)
  
  ###############################################################################
  #### 2) Parameters with one value per parameter 
  ###############################################################################
  
  oneLineParasWithMultipleValues <- oneLineParas[oneLineParas$diffVals != 0,]
  
  #### Line /A18 -- MB9: definition of multiple output paras for "balance.out"
  
  pars <- c("MB9", "HK", "HT")
  props <- list()
  for(selPar in pars)
  {
    tmp <- oneLineParasWithMultipleValues[oneLineParasWithMultipleValues$parNames==selPar,]
    if (selPar=="MB9")
    {
      x[[tolower(selPar)]] <- as.numeric(unlist(strsplit(tmp$parVals, split = "\\s+")))
    } else if (selPar %in% c("HK", "HT"))
    {
      itex <- 0 
      
      for (i in 1:nrow(tmp))
      {
        itex <- itex + 1
        id <- sprintf("itex%d",itex)
        values <- as.numeric(unlist(strsplit(tmp$parVals[i], split = "\\s+")))
        props[[id]][[tolower(selPar)]] <- values
      } 
    } else {
      if(dbg==TRUE) 
      {
        msg <- sprintf("Data import not define for FORTRAn parameter: %s \n\n", selPar)
        cat(msg)
      }
    }}
  
  x <- list(basic=x,soil=list(props=props))
  return(x )
}

#' Helper function: imports matrix values of vs2dh.dat
#' 
#' @param prepData object as retrieved by prepareImport()
#' @param pattern as used in filterLines()
#' @param patternNameCol as used in filterLines()
#' @param startLineOffset as used in filterLines()
#' @param msg message which parameter is imported. used if DBG == TRUE
#' @param nrow number of rows of matrix: Default: nly
#' @param dbg If TRUE prints debug information on the screen (Default: TRUE)
#' @return Matrix values (always: soil, if available: initial pressure head &
#' temperature distribution)

importMatrix <- function(prepData,
                         pattern="JTEX",
                         patternNameCol="parNames",
                         startLineOffset = 0,
                         msg="B9: Importing soil matrix...",
                         nrow = NULL,
                         dbg=TRUE)
{
  if (is.null(nrow))
  {
    nrow <- as.numeric(importSingleLineParas(prepData)$basic$nly)
  }
  
  values <- filterLines(pattern=pattern,
                        patternNameCol=patternNameCol,
                        data=prepData$res,
                        startLineOffset = startLineOffset,
                        input = prepData$input)
  
  
  
  if (length(values) > 1) 
  {
    if(dbg==TRUE) cat(paste(msg, "DONE!\n\n"))
    valuesVector <- as.numeric(unlist(strsplit(x = values, split = "\\s+")))
    x <- matrix(data = valuesVector,byrow = TRUE,
                nrow = nrow, ncol=length(valuesVector)/nrow)
    
    
    return(x)
  } else {
    if(dbg==TRUE) cat(paste(msg, "NOT AVAILABLE!\n\n"))  
  }}

#' Helper function: imports matrices (jtex, hvalues, tvalues)
#' 
#' @param prepData object as retrieved by prepareImport()
#' @param dbg prints debug information on the screen
#' @return Matrix values (always: soil, if available: initial pressure head &
#' temperature distribution)
#' model.path <- system.file("extdata", "vs2dh_example/tutorial2", package = "kwb.vs2dh")
#' inp <- prepareImport(model.path)
#' grid <- importMatrices(inp) 
#' #### Soil properties matrix
#' vs2dh.plotMatrix(data = grid$jtex)
#' #### Initial temperature distribution matrix
#' vs2dh.plotMatrix(data = grid$tvalues)

importMatrices <- function(prepData,
                           dbg = TRUE)
{
  ################################################################################
  ##### Soil matrix JTEX
  ###############################################################################
  jtex <- importMatrix(prepData,
                       pattern="JTEX",
                       patternNameCol="parNames",
                       startLineOffset = 0,
                       msg="B9: Importing soil matrix...",
                       dbg=dbg)
  
  ###############################################################################
  ### Initial conditions
  ###############################################################################
  ### Pressure head or moisture content B13 (if available)
  hvalues <- importMatrix(prepData,
                          pattern="/B13",
                          patternNameCol="txt",
                          startLineOffset = 1,
                          msg="B13: Importing initial pressure head or moisture content...",
                          dbg=dbg)
  
  ### Initial head distribution B25 (if available)
  tvalues <- importMatrix(prepData,
                          pattern="/B25",
                          patternNameCol="txt",
                          startLineOffset = 1,
                          msg="B25: Importing initial temperatures...",
                          dbg=dbg)
  
  x <- list(jtex = jtex,
            hvalues = hvalues,
            tvalues = tvalues)
  
  return(x)
}


#' Helper function: imports recharge periods
#' 
#' @description xxxxxx
#' @param prepData object as retrieved by prepareImport()
#' @return Recharge periods (i.e. PART C) with naming parameter names in R style


importRechargePeriods <- function (prepData)
  
{
  tmp <- list(input = prepData$input, res=list())
  numberOfRechargePeriods <- nrow(prepData$rechargePeriods)
  
  x <- list()  
  
  for (id in 1:numberOfRechargePeriods)
  {
    periodlabel <- sprintf("id%d", id)  
    
    selPeriod <- prepData$rechargePeriods[id, ]
    cond <- which(prepData$res$Line >= selPeriod$startLine & prepData$res$Line < selPeriod$endLine)
    tmp$res <-prepData$res[cond,]
    
    ####1. Step: one line parameters for this recharge period
    
    oneLineParas <- tmp$res[tmp$res$LineDiff==1,]
    parNamesSplit <- gsub("\\s+", "", unlist(strsplit(oneLineParas$parNames, split = ",\\s+")))
    parNamesSplit <- tolower(parNamesSplit)
    parNamesSplit <- gsub("bcit", "rbcit",x = parNamesSplit)
    parNamesSplit <- gsub("etsim", "retsim",x = parNamesSplit)  
    parValsSplit <- unlist(strsplit(oneLineParas$parVals, split = "\\s+"))
    parValsSplit <- gsub("F", "FALSE", parValsSplit)
    parValsSplit <- gsub("T", "TRUE", parValsSplit)
    
    isSeep <- parNamesSplit %in% c("seep", "nfcs","jj", "jlast")
    
    noSeep <-  isSeep==FALSE
    
    single <- lapply( X = as.character(parValsSplit[noSeep]), FUN=c)
    names(single) <- parNamesSplit[noSeep]
    x[[periodlabel]] <- single
    x[[periodlabel]]$seepage$seep <- as.logical(parValsSplit[parNamesSplit=="seep"])
    ####2 Step: Seepage faces?
    
    if (x[[periodlabel]]$seepage$seep ==TRUE)
    {
      ### 1) Single line parameters: 
      
      numberOfSeepageFaces <- as.numeric(parValsSplit[parNamesSplit=="nfcs"])
      
      x[[periodlabel]]$seepage$nfcs <-  numberOfSeepageFaces 
      
      jjIndex <- which(parNamesSplit %in% c("jj"))
      jlastIndex <- which(parNamesSplit %in% c("jlast"))
      jn <- tmp$res[tmp$res$parNames=="J, N",]
      for (selSeepFace in 1:numberOfSeepageFaces)
      {
        seepFaceLabel <- sprintf("id%d", selSeepFace)  
        
        ### 2) Single line parameters 
        x[[periodlabel]]$seepage$seepFaces[[seepFaceLabel]]$jj <- as.numeric(parValsSplit[jjIndex][selSeepFace])
        x[[periodlabel]]$seepage$seepFaces[[seepFaceLabel]]$jlast <- as.numeric(parValsSplit[jlastIndex][selSeepFace])
        
        
        ### 3) Multi line parameters: 
        jnDat <- jn[selSeepFace,]
        
        startLine <- jnDat$Line 
        endLine <-  jnDat$Line + jnDat$LineDiff - 1
        jnValues <- tmp$input[startLine:endLine]
        nodes <- as.data.frame(convertStringVectorToMatrix(values = jnValues))
        names(nodes) <- c("j", "n")
        
        x[[periodlabel]]$seepage$seepFaces[[seepFaceLabel]]$nodes <- nodes
        
      }
      
    } 
    
    ####3. Step: Boundary condition for this recharge period
    boundaryNames <- "JJ, NN, NTX, PFDUM, NTC, CF"
    
    boundary <- multipleLineValues(parName = boundaryNames,startLineOffset = 0,
                                   data = tmp)
    boundaryR <- as.data.frame(matrix(data = boundary[[1]],ncol = 6,byrow = TRUE))
    names(boundaryR) <- tolower(unlist(strsplit(x = boundaryNames,split = ",\\s+")))
    x[[periodlabel]]$boundary <- boundaryR
  }
  
  return(x)
}


#' vs2dh.ReadConfig()
#' 
#' @description Imports the configuration from an existing vs2dh.dat file 
#' and stores it in an R list data structure as retrieved by vs2dh.Configure()
#' @param model.path Full path to the folder containing the vs2dh.dat
#' @param dbg prints debug information on the screen
#' @return Imported vs2dh configuration
#' model.path <- system.file("extdata", "vs2dh_example/tutorial2", package = "kwb.vs2dh")
#' vs2dh.ReadConfig(model.path)

vs2dh.ReadConfig <- function(model.path,
                             dbg = TRUE)
{
  prepData <- prepareImport(model.path = model.path, 
                            dbg = dbg)
  
  ##########################################
  ### Section A & B
  ##########################################
  
  single <- importSingleLineParas(prepData, dbg)
  
  basic <- c(single$basic, multipleLineValues(parName = "DXR",startLineOffset = 0,data = prepData),
             multipleLineValues(parName = "DELZ",startLineOffset = 0,data = prepData),
             multipleLineValues(parName = "PLTIM",startLineOffset = 0,data = prepData))
  
  if(as.logical(basic$f11p)==TRUE) 
  {
    nrows <- abs(x = as.numeric(basic$nobs))  
    obs_jn <- as.data.frame(importMatrix(prepData = prepData, 
                                         pattern = "NOBS", 
                                         startLineOffset = 1,
                                         nrow = nrows,
                                         msg = "A16: Importing observation points..."))
    names(obs_jn) <- c("obs_j", "obs_n")
    obs_jn <- list(obs_jn=obs_jn, obs_j=as.vector(obs_jn[,"obs_j"]), obs_n=as.vector(obs_jn[,"obs_n"]))
    basic <- c(basic, obs_jn)
  }
  
  # Boundary fluxes import 
  if(as.logical(basic$f7p)==TRUE) 
  {
    numbf <- as.numeric(basic$numbf)
    maxcells <- as.numeric(basic$maxcells)
    
    condition <- grep(pattern="NUMCELLS", prepData$res$parNames)
    b27 <-  prepData$res$parVals[condition]
    b27 <- as.numeric(unlist(strsplit(x = b27, split = "\\s+")))
    b27 <- as.data.frame(matrix(data=b27,nrow = length(b27)/2, byrow = TRUE))
    names(b27) <- c("idbf", "numcells")
    
    line <- data.frame(line=prepData$res$Line[condition])
    
    b27 <- cbind(b27, line)   
    
    bf_nodes <- data.frame()
    if (dbg==TRUE) cat("B27: Importing boundary flux faces...")
    for (id in 1:numbf)
    {
      idbf <- b27$idbf[id]
      numcells <- b27$numcells[id]
      startLine <- b27$line[id] + 1
      endLine <-  b27$line[id] + b27$numcells[id] 
      nodeString <- paste(prepData$input[startLine:endLine], collapse=" ")
      bf_jn <- as.data.frame(convertStringVectorToMatrix(nodeString, rows = numcells))
      names(bf_jn) <- c("bf_j", "bf_n")
      nodes <- data.frame(idbf = rep(idbf,numcells), 
                          bf_j = as.vector(bf_jn[,"bf_j"]), 
                          bf_n = as.vector(bf_jn[,"bf_n"]))
      bf_nodes <- rbind(bf_nodes, nodes)
    }
    
    basic <- c(basic, list(bf_nodes = bf_nodes))
    if (dbg==TRUE) cat("Done!!!\n")
  }
  
  #### Recharge
  recharge <- list(periods=importRechargePeriods(prepData),
                   nrech = as.numeric(basic$nrech))
  
  grid <-  importMatrices(prepData = prepData, dbg)
  
  flow <- basic[which(names(basic) %in% c("phrd", "hiread", "hfactor", "hiu", "hifmt", "dwtx", "hmin"))]
  if (!is.null(grid$tvalues))
  {
    flow$hvalues <- grid$hvalues
  }
  temp <- basic[which(names(basic) %in% c("tiread", "tfactor", "tiu", "tifmt"))] 
  if (!is.null(grid$tvalues))
  {
    temp$tvalues <- grid$tvalues
  }
  initial <- list(flow=flow, temp=temp)
  
  soils <- single$soil
  
  soils$base <- basic[which(names(basic) %in% c("wus", "hft", "nprop", "nprop1", "ntex"))]
  
  #### soils$base$itex: probably error-prone approach in case that ITEX is not 
  #### always from 1:ntex!!!!! should be optimised in a later version 
  soils$base$itex <- 1:length(soils$props) 
  soils$base$grid = list(irow = basic$irow, jtex=grid$jtex)
  
  x <- list(basic=basic, 
            soils=soils, 
            initial=initial,
            recharge=recharge)
  
  return(x)  
}



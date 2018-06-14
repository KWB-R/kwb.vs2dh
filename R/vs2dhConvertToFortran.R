#' Helper function: converts matrix to one string 
#' 
#' @param matr matrix with character data
#' @param colSep seperator used for merging data from all columns for each row 
#' (default: " ")
#' @param rowSep seperator used for separating the data of different rows 
#' (default: "newline")
#' @return One string with each row separated with rowSep

convMatrixByRowToString <- function (matr, colSep=" ", rowSep="\n")
{
  rows <- apply(X = matr, 1,FUN = function (x) paste(x, collapse=colSep)) 
  oneString <-  paste(rows, collapse=rowSep)
  return(oneString)
}


#' Helper function: names of all sublists of a list returns the names
#'of all sublists of \emph{x} in the "$"-notation, e.g.
#' list$sublist$subsublist$subsubsublist
#' 
#' @param x R list.
#' @param values name to be used as prefix for all names found. default: ""
#' @return leafs of sublists into one dimensional list

leafValues <- function (
  x, 
  values = list()
)
{
  if (!is.list(x)) {
    return(x) 
  }
  
  for (elementName in names(x)) {
    
    child <- x[[elementName]]
    
    if (is.list(child)) {
      values <- c(values, leafValues(child))
    }
    else {
      i <- length(values) + 1
      values[[i]] <- child
      names(values)[i] <- elementName
    }
  }
  
  values
}

#' Helper function: convert basic config to  FORTRAN style  
#' 
#' @param baseConf with basic parameterisation, i.e. conf$basic
#' @param dbg If TRUE debug messages are printed on the screen (default: TRUE)
#' @return Basic configuration in FORTRAN style

convBasic <- function (
  baseConf,
  dbg=TRUE
)
{
  out <- leafValues(baseConf) 
  ### Line A1 ###############################################################
  nTitle <- stringr::str_length(string = out$titl) 
  
  if (nTitle > 80) 
  {
    if (dbg==TRUE)
    {
      warnMsg <- sprintf("'titl' has %2d characters. Only the first 80 can be used!\n",
                         nTitle)
      warning(warnMsg)
    }
    out$titl <- substr(out$titl, 1, 80)
  }
  ### Line A2 ###############################################################
  if (as.numeric(out$ang) < -90 | as.numeric(out$ang) > 90)
  {
    if (dbg==TRUE)
    {
      warnMsg <- "'ang' is not >= -90 or <= 90 degrees. Set to 0!\n"
      warning(warnMsg)
    }
    out$ang <- 0
  }
  ### Line A3 ##################################################################
  unitNames <- c("zunit","tunit", "cunx")
  for (name in unitNames)
  {
    out[[name]] <- sprintf("%-4s", out[[name]])
  }
  ### Lines A10,A12,A14,A18 #######################################################
  vectorNames <- c("dxr", "delz", "pltim", "mb9") 
  for (name in vectorNames)
  {
    out[[name]] <- paste(out[[name]], collapse=" ")
  }
  ### Lines A16 ################################################################
  if (as.logical(out$f11p) == TRUE)
  {
    out$obs_jn <- paste(out$obs_j, out$obs_n, "\n", collapse="") 
  }
  ### Lines B28 ################################################################
  if (as.logical(out$f7p) == TRUE)
  {

  bndFaces <- ""
  for (idbf in unique(out$idbf))
    {
      selFace <- list()
      selFace$idbf <- idbf
      
      indices <- which(out$idbf == idbf)
      selFace$numcells <- length(indices)
      
      selFace$bf_jn <- paste(out$bf_j[indices], out$bf_n[indices ], "\n", collapse="") 
     
      grammarBndFace <- list(
        B27 = "<idbf> <numcells>    /B27 -- IDBF, NUMCELLS. B-28 begins next line: J, N\n<bf_jn>"
      )
      grammarBndFace <- c(grammarBndFace,selFace)
      bndFaces <- paste(bndFaces, kwb.utils::hsResolve("B27", grammarBndFace))
   }  
  out$bndFaces <- bndFaces    
  
  grammarBoundaryFluxes <- list(
    B_BoundaryFluxes = "<B26>\n<B27>",
    B26 = "<numbf> <maxcells>    /B26 -- NUMBF, MAXCELLS",
    B27 = "bndFaces"
  )  
  grammarBoundaryFluxes<- c( grammarBoundaryFluxes, out)
      
  out$B_BoundaryFluxes <- kwb.utils::hsResolve("B_BoundaryFluxes",  grammarBoundaryFluxes)    
  } 
  
  return(out)
}



#' Helper function: converting soils structure to FORTRAN style  
#' 
#' @param conf config with R model parameterisation, i.e. conf
#' @param dbg If TRUE debug messages are printed on the screen (default: TRUE)
#' @return Soil configuration in FORTRAN style

convSoils <- function (
  conf,
  dbg = TRUE
)
{
  soilConf <- conf$soils
  baseConf <- leafValues(conf$basic)
  
  out <- leafValues(soilConf$base)
  
  out$jtex <- convMatrixByRowToString(matr = out$jtex)
  
  simulateTransport <- as.logical(baseConf$trans)
  
  soilPara <- ""
  for (itex in out$itex)
  {
    label <- sprintf("itex%d", itex)
    flowDat <- paste(as.numeric(unlist(soilConf$props[[label]]$hk)), collapse=" ")
    flow <- sprintf("%-6d/B6 -- ITEX. B7 to begin next line: HK\n%s\n", itex, flowDat )
    soilPara <- paste0(soilPara, flow)
    if(simulateTransport)
    {
      transDat <- paste(as.numeric(unlist(soilConf$props[[label]]$ht)), collapse=" ") 
      trans <- sprintf("%s      /B7A -- HT\n", transDat)
      soilPara <- paste0(soilPara, trans)
    }
  }
  out$soils <- soilPara
  return(out)
}

#' Helper function: converting initial conditions to FORTRAN style  
#' 
#' @param iniConf config with soils parameterisation, i.e. conf$initial
#' @param dbg If TRUE debug messages are printed on the screen (default: TRUE)
#' @return Soil configuration in FORTRAN style

convInitial <- function (
  iniConf,
  dbg = TRUE
)
{
  
  out <- leafValues(iniConf)
  
  
  B12 <- ""
  B13 <- ""
  B25 <- ""
  
  
  if (any(names(out)=="hiread"))
  {
    out$hiread <- as.numeric(out$hiread)
    
    if (out$hiread==1) 
    {
      out$hvalues <- convMatrixByRowToString(out$hvalues)
      B13 <- "<hiu> '<hifmt>'     /B13 -- IU, IFMT. Initial values to follow.\n<hvalues>"
    } else if (out$hiread==2) {
      B12 <-  "<dwtx> <hmin>     /B12 -- DWTX, HMIN"
    } 
  }
  if (any(names(out)=="tiread") &  as.numeric(out$tiread) != 3)
  {
    out$tiread <- as.numeric(out$tiread)
    
    if(out$tiread==1) 
    { out$tvalues <- convMatrixByRowToString(out$tvalues)
      B25 <- "<tiu> '<tifmt>'     /B25 -- IU, IFMT. Initial values to follow.\n<tvalues>\n"
    }
  }
  
  grammarInitial <- list(
    B_InitialFlow = "<B11>\n<B12><B13>",
    B11 = "<hiread> <hfactor>     /B11 -- IREAD, FACTOR",
    B12 = B12,
    B13 = B13,
    B_InitialTemp = "<B24>\n<B25>",
    B24 = "<tiread> <tfactor>     /B24 -- IREAD, FACTOR",
    B25 = B25
  )
  grammarInital <- append( grammarInitial, out)
  x <- list(B_InitialFlow  =kwb.utils::hsResolve("B_InitialFlow",  grammarInital),
            B_InitialTemp = kwb.utils::hsResolve("B_InitialTemp",  grammarInital),
            phrd = out$phrd)
  return(x)
}


#' Helper function: converting recharge periods to FORTRAN style  
#' 
#' @param rechConf config with recharge period parameterisation, i.e. conf$recharge
#' @param dbg If TRUE debug messages are printed on the screen (default: TRUE)
#' @return Recharge periods configuration in FORTRAN style

convRecharge <- function (
  rechConf,
  dbg = TRUE
)
{
  periods <- 1:as.numeric(rechConf$nrech)
  
  rechargePara <- ""
  for (rechargePeriod in  periods)
  {
    selPeriod <- rechConf$periods[[rechargePeriod]]
    selPeriod$boundary <- convMatrixByRowToString(selPeriod$boundary)
    
    selPeriod$seep <- as.logical(selPeriod$seepage$seep)
    seepagePara <- ""
    #### If seepage faces are simulated for the recharge period 
    if ( selPeriod$seep  == TRUE)
    {
      selPeriod$nfcs <- as.numeric(selPeriod$seepage$nfcs)
      
      for (selFace in c(1:selPeriod$nfcs))
      {
        C7 <- ""
        selSeepFace <- selPeriod$seepage$seepFaces[[selFace]]
        selSeepFace$nodes <- convMatrixByRowToString(selSeepFace$nodes)
        if (selFace == 1) C7 <- "<nfcs>     /C7 -- NFCS\n"
        grammarSeep <- list(
          C789 = "<C7><C8>\n<C9>\n",
          C7 =  C7,
          C8 = "<jj> <jlast>     /C8 -- JJ, JLAST. C-9 begins next line: J, N",
          C9 = "<nodes>"
        )
        grammarSeep <- append(grammarSeep, selSeepFace)
        
        seepagePara <- paste(seepagePara, kwb.utils::hsResolve("C789", grammarSeep))
        
      }
    }
    selPeriod$seepagePara <- seepagePara
    
    grammar <- list(
      C = "<C_fix1><C_opt1><C_fix2>",
      C_fix1 = "<C1>\n<C2>\n<C3>\n<C4>\n<C5>\n<C6>\n",
      C_opt1 = "<seepagePara>",
      C_fix2 = "<C10>\n<C11>\n<C13>\n",
      C1 = sprintf("<tper> <delt>     /C1 -- TPER, DELT (Recharge Period %s)", 
                   rechargePeriod), 
      C2 = "<tmlt> <dltmx> <dltmin> <tred>     /C2 -- TMLT, DLTMX, DLTMIN, TRED",
      C3 = "<dsmax> <sterr>     /C3 -- DSMAX, STERR", 
      C4 = "<pond>     /C4 -- POND",
      C5 = "<prnt>     /C5 -- PRNT",
      C6 = "<rbcit> <retsim> <seep>     /C6 -- BCIT, ETSIM, SEEP",
      C10 = "<ibc>     /C10 -- IBC. C11 begins next line: JJ, NN, NTX, PFDUM, (NTC, CF)",
      C11 = "<boundary>",
      C13 = sprintf("-999999 / C13 -- End of data for recharge period %d", 
                    rechargePeriod)                
    )
    grammar <- append(grammar, selPeriod)
    
    rechargePara <- paste(rechargePara, kwb.utils::hsResolve("C", grammar))
  }
  x <- list(rechargePara=rechargePara,
            nrech = rechConf$nrech)
  return(x)
}


#' Helper function: formatting R elments to FORTRAN style
#' 
#' @param conf as retrieved by vs2dhConfigure()
#' @return config with prepared FORTRAN formatting style (only TRUE/FALSE values
#' will be replaced during vs2dh.writeConfig()

fortranFormat <- function (conf)
{
  
  out <- c( convBasic(conf$basic),
            convSoils(conf), 
            convInitial(conf$initial),
            rechargePara = convRecharge(conf$recharge)$rechargePara,
            nrech = convRecharge(conf$recharge)$nrech
  )
  
  return(out)
}

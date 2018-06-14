#' Write configuration
#' 
#' @param conf model config as retrieved by vs2dhConfigure()
#' @return Write vs2dh.dat in folder \code{model.path} 
#' @examples
#' conf <- vs2dh.Configure()
#' inpDat <- vs2dh.writeConfig(conf)  
#' write(inpDat, file.path(getwd(), "inp.dat"))

vs2dh.writeConfig <- function ( conf
)
{
  fConf <- fortranFormat(conf)
  fConf$A_option1_txt <- ""
  fConf$A_option2_txt <- ""
  fConf$B_option1 <- ""
  fConf$B_option2 <- ""
  
  #### Set optional input strings by default to ""
  options <- c(paste("A_option", 1:5, sep=""))
  for (option in options)
  {
    assign(x = option, value = "")
  }
  

  if (fConf$ifac %in% c(0,2))
  {
    A_option1 <- "<dxr>\n"
    fConf$A_option2_txt <- ". A10 begins next line: DXR"
  }
  
  if (fConf$jfac %in% c(0,2))
  {
    A_option2 <- "<delz>\n"
    fConf$A_option2_txt <- ". A12 begins next line: DELZ"
  }
  
  if (as.logical(fConf$variables)==TRUE) 
  {
    A_option3 <- "<A13>\n<A14>\n"
  }
    ### Check which optional strings should be written:
  if (as.logical(fConf$variables)==TRUE) 
  {
    A_option3 <- "<A13>\n<A14>\n"
  }
  if (as.logical(fConf$f11p)==TRUE) 
  {
    A_option4 <- "<A15>\n<A16>"
  }
  if (as.logical(fConf$f9p)==TRUE) 
  {
    A_option5 = "<A17>\n<A18>\n"
  }
  
  if (as.logical(fConf$trans) == TRUE) 
  {
    fConf$B_option1 <- fConf$B_InitialTemp
  }
  
  if (as.logical(fConf$f7p) == TRUE) 
  {
    fConf$B_option2 <- fConf$B_BoundaryFluxes
  }
  
  
  grammar <- list(
    input_file = "<A><B><C>",
    A = "<A_fixBlock><A10><A11>\n<A12><A1314><A1516><A1718>", 
    A_fixBlock = "<A1>\n<A2>\n<A3>\n<A4>\n<A5>\n<A6>\n<A6A>\n<A7>\n<A8>\n<A9>\n",
    B = "<B_fixBlock1><B_InitialTemp><B_BoundaryFluxes>", 
    B_fixBlock1 = "<B1>\n<B3>\n<B4>\n<B5>\n<B5A>\n<B67><B8>\n<B9>\n<B_Flow>\n<B14>\n",
    A1 = "<titl>",
    A2 = "<tmax> <stim> <ang>   /A2 -- TMAX, STIM, ANG",
    A3 = "<zunit><tunit><cunx>   /A3 -- ZUNIT, TUNIT, CUNX",
    A4 = "<nxr> <nly>   /A4 -- NXR, NLY",
    A5 = "<nrech> <numt>   /A5 -- NRECH, NUMT",
    A6 = "<rad> <itstop> <trans>     /A6 -- RAD, ITSTOP, TRANS",
    A6A = "<cis> <cit>      /A6A -- CIS, CIT",
    A7 = "<f11p> <f7p> <variables> <f9p> <f6p>    /A7 -- F11P, F7P, F8P, F9P, F6P",
    A8 = "<thpt> <spnt> <ppnt> <hpnt> <vpnt>   /A8 -- THPT, SPNT, PPNT, HPNT, VPNT",
    A9 = "<ifac> <facx>      /A9 -- IFAC, FACX<A_option1_txt>",
    A10 = A_option1,
    A11 = "<jfac> <facz>     /A11 -- JFAC, FACZ<A_option2_txt>",
    A12 = A_option2,
    A1314 = A_option3,
    A13 =  "<nplt> /A13 -- NPLT. A14 begins next line: PLTIM",
    A14 =  "<pltim>",
    A1516 = A_option4,
    A15 =  "<nobs> /A15 -- NOBS. A16 begines next line: J, N",
    A16 =  "<obs_jn>",
    A1718 = A_option5,
    A17 = "<nmb9>    /A17 -- NMB9",
    A18 = "<mb9>  /A18 -- MB9",
    B1 = "<eps> <hmax> <wus> <eps1> <eps2>     /B1 -- EPS, HMAX, WUS, EPS1, EPS2",
    B3 = "<minit> <itmax>     /B3 -- MINIT, ITMAX",
    B4 = "<phrd>     /B4 -- PHRD",
    B5 = "<ntex> <nprop> <nprop1>     /B5 -- NTEX, NPROP, NPROP1",
    B5A = "<hft> 1     /B5A -- hydraulicFunctionType, adsorptionType (not used)",
    B67 = "<soils>", 
    B8 = "<irow>     /B8 -- IROW. B9 begins next line: JTEX",
    B9 = "<jtex>",
    B_Flow = "<B_InitialFlow>",
    B14 = "<bcit> <etsim>      /B14 -- BCIT, ETSIM",
    B_InitialTemp = "<B_option1>",
    B_BoundaryFluxes =  "<B_option2>",
    C = "<rechargePara><endOfFile>",
    endOfFile = "-999999 / End of input data file"
  )
  
  grammar <- append(grammar, fConf)
  
  inpTxt <- kwb.utils::hsResolve("input_file", grammar)
  
  inpTxt <- gsub(pattern = "TRUE", replacement = "T", inpTxt)
  inpTxt <- gsub(pattern = "FALSE", replacement = "F", inpTxt)
  return(inpTxt)
}


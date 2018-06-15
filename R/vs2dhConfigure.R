#' Helper function: replace a list index with a name
#' 
#' @param inpLst  input list
#' @param labelTxt user defined txt used for naming list 
#' @return Labeled list


labeledList <- function (inpLst, labelTxt="id")
{
  listLength <- length(inpLst)
  
  if (listLength > 0) {
    names(inpLst) <- sprintf("%s%d", labelTxt, seq_len(listLength)-1)
  } 
  
  return(inpLst)
}



###############################################################################
##### Configure Time
###############################################################################
###############################################################################

#' Configure time parameters
#' 
#' @param stim  Initial time (usually set to 0). (default: 0)
#' @param tmax  Maximum simulation time. (default: 10.1)
#' @return Time parameters

vs2dh.ConfigureBasicTime <- function(stim = 0,
                                     tmax = 10.1
)
{
  x <- list(stim = stim, 
            tmax = tmax
  )
  return(x)
}

###############################################################################
##### Configure Units
###############################################################################
###############################################################################

#' Configure unit parameters
#' 
#' @param zunit  Units used for length, "m" for meters. (default: "m")
#' @param tunit  Units used for time, "sec" for seconds, "day" for day 
#' (default: "day")
#' @param cunx  Units used for heat, "J" for Joules. (default: "J")
#' @return Model units

vs2dh.ConfigureBasicUnits <- function(zunit = "m",
                                      tunit = "day",
                                      cunx = "J")
{
  x <- list(zunit = zunit, 
            tunit = tunit, 
            cunx = cunx)
  return(x)
}

###############################################################################
##### Configure Output
###############################################################################
###############################################################################

#' Configure model output times
#' 
#' @param pltim Elapsed times at which pressure heads and temperatures are to be
#' written to file "variables.out", and heads, temperatures, saturations, 
#' velocities, and/or moisture contents to file "vs2dt.out", T. (default: seq(0,10,0.5))
#' @param numt Maximum number of time steps. (default: 1000), will be set 
#' automatically if called from higher level function vs2dh.ConfigureBasic()
#' @param enhancedPrecision if TRUE enhanced results are written with enhanced 
#' precision to files6 (vs2dh.out) & file9 (variables.out), (default: FALSE)
#' @param dbg if TRUE additional information printed on screen (default: TRUE)
#' @return Model output times

vs2dh.ConfigureBasicOutputTimes <- function( pltim = seq(0,10,0.5),
                                             numt = 1000,
                                             enhancedPrecision = FALSE,
                                             dbg = TRUE)
{
  if(enhancedPrecision == TRUE)  {
    if(dbg==TRUE) {
      cat("Enhanced precision selected for writting files6 (vs2dh.out) & file9 (variables.out)\n")
    }
    numt <-  numt * (-1) 
  }
  
  nplt <- length(pltim)
  
  x <- list(pltim = pltim,
            numt = numt,
            nplt = nplt
  )
  
  return(x)
  
}

#' Configure main output file ("vs2dh.out")
#' 
#' @param f6p If TRUE  mass balance is to be written to file "vs2dt.out" 
#' for each time step; if FALSE mass balance is to be written to file "vs2dt.out" 
#' only at observation times and ends of recharge periods (default: TRUE)
#' @param thpt If TRUE volumetric moisture contents are to be written to 
#' file "vs2dt.out", otherwise: FALSE (default: TRUE)
#' @param spnt  If TRUE saturation is written to file "vs2dt.out", 
#' otherwise: FALSE (default: TRUE)
#' @param ppnt If TRUE pressure heads are written to file "vs2dt.out", 
#' otherwise: FALSE (default: TRUE)
#' @param hpnt If TRUE total heads are written to file "vs2dt.out", 
#' otherwise: FALSE (default: TRUE)
#' @param vpnt If TRUE velocities are written to file "vs2dt.out", 
#' otherwise: FALSE (default: TRUE)
#' @return Output config of vs2dh.out file

vs2dh.ConfigureBasicOutputMain <- function (f6p = TRUE,
                                            thpt = TRUE,
                                            spnt = TRUE,
                                            ppnt = TRUE,
                                            hpnt = TRUE, 
                                            vpnt = TRUE)
{
  ###/A8 -- THPT, SPNT, PPNT, HPNT, VPNT
  x <- list(f6p = f6p,
            thpt = thpt, 
            spnt = spnt,
            ppnt = ppnt, 
            hpnt = hpnt, 
            vpnt = vpnt 
  )
  
  return(x)
}

#' Configure balance output file ("balance.out")
#' 
#' @param mb9 The index number of each mass balance component to be written to 
#' file "balance.out".(See table 7, from p. 66, in Healy (1990) 
#' @param outputEachTimeStep  FALSE if output to file 9 ("balance.out") is desired 
#' only at selected output times rather than at each time step (default: TRUE)
#' @return Selected components and temporal resolution for "balance.out" file

vs2dh.ConfigureBalance <- function (mb9=c(1:72), 
                                    outputEachTimeStep = TRUE)
{
  f9p <- FALSE
  
  ### Number of different mass balance components
  n <- length(mb9)
  if (n > 0) f9p <- TRUE
  
  ### If output 
  if (outputEachTimeStep==FALSE) n <- n*(-1)
  
  x <- list(nmb9=n, ### /A17 -- NMB9
            mb9=mb9, ### /A18 -- MB9
            f9p=f9p ### /A7 -- F9P
  )
  
  return(x)
}

###############################################################################
##### Optional outout: Configure Observation points
###############################################################################
###############################################################################

#' Configure Observation points
#' 
#' @param obs_n grid column "n" (if NULL no observation points will be written to file
#' obsPoints.out)
#' @param obs_j grid row "j" (if NULL no observation points will be written to file
#' obsPoints.out)
#' @param outputEachTimeStep If FALSE output to obsPoints.out is desired only at 
#' selected output times rather than at each time step (default: TRUE)
#' @return Observation point parameterisation

vs2dh.ConfigureObsPoints <- function (obs_n=NULL, ### column "n"
                                      obs_j=NULL, ### row "j"
                                      outputEachTimeStep = TRUE
)
{
  n_obs_n <- length(obs_n)
  n_obs_j <- length(obs_j)
  
  if(n_obs_n==n_obs_j &n_obs_n > 0)
  {
    obsNodes <- data.frame(obs_j=obs_j, obs_n=obs_n)
  } else {
    commonLength <- min(n_obs_n, n_obs_j)
    commonIndices <- 1:commonLength
    
    obsNodes <- data.frame(obs_j=obs_j[commonIndices], 
                           obs_n=obs_n[commonIndices])
    
    if (commonLength==0) 
    {
      warning("No observation points (n & j == 0) are defined! 
               No file 'obsPoints.out' will be written in a vs2dh run")
      x <- list(f11p = FALSE) 
      return(x)
    } else {
      warnMsg <- sprintf("n (n=%d) and j (n=%d) have different length! Only the first %d 
                         elements will be used as observation points", obs_n,  obs_j, commonLength)
      warning(warnMsg)  
    }
    
  } 
  nobs <- nrow(obsNodes)
  if (outputEachTimeStep==FALSE) nobs <- nobs * (-1) 
  ###/A15 -- NOBS. A16 begines next line: J, N
  x <- list(f11p = TRUE,
            nobs = nobs,
            obs_jn = obsNodes
  )
  
  return(x)
} 

#' Configure Boundary fluxes 
#' 
#' @param nodes data.frame with columns "idbf" (id of boundary face), "bf_j" (grid row)
#' and "bf_n" (grid column)
#' @return Boundary fluxes parameterisation
#' @examples
#' nodes <- data.frame(idbf = c(rep(1,5), rep(2,6)), 
#'                     bf_j = 1:11, 
#'                     bf_n = rep(1,11))
#' vs2dh.ConfigureBoundaryFluxes(nodes = nodes) 
vs2dh.ConfigureBoundaryFluxes <- function (nodes = NULL) {
  
  x <- list()
  
  if (is.data.frame(nodes)) {
    if(nrow(nodes) > 0)
    { 
      bf <- stats::aggregate(bf_j ~ idbf, nodes,length)
      names(bf)[names(bf) == "bf_j"] <- "numcells"
      
      numbf <- nrow(bf)
      maxcells <- max(bf$numcells)
      
      x <- list(f7p = TRUE,
                numbf = numbf,
                maxcells = maxcells,
                bf_nodes = nodes )
    }} else {
      x <- list(f7p = FALSE)
    }
  
  return(x)
}

#' Configure model output 
#' 
#' @param times as retrieved by \code{vs2dh.ConfigureBasicOutputTimes()}
#' @param main as retrieved by \code{vs2dh.ConfigureBasicOutputMain()}
#' @param variables If TRUE output of pressure heads (and temperatures if TRANS = T)
#' to file "variables.out" is desired at selected observation times (default: TRUE)
#' @param balance  IF TRUE one-line mass balance summary for each time step is 
#' written to file "balance.out"; (default: TRUE) 
#' @param obsPoints as retrieved by  vs2dh.ConfigureObsPoints(), default: no output
#' of observation points to file "obsPoints.out"
#' @param boundaryFluxes as retrieved by vs2dh.ConfigureBoundaryFluxes(), default:
#' no output of boundary fluxes to file "boundaryFluxes.out"
#' @param enhancedPrecision If TRUE results are written with enhanced precision 
#' to files9 ("variables.out") and file11 ("obsPoints.out"), (default: FALSE)
#' @return Configuration of model output

vs2dh.ConfigureBasicOutput <- function (times = vs2dh.ConfigureBasicOutputTimes(),
                                        main = vs2dh.ConfigureBasicOutputMain(),
                                        variables = TRUE,
                                        balance = vs2dh.ConfigureBalance(),
                                        obsPoints = vs2dh.ConfigureObsPoints(),
                                        boundaryFluxes = vs2dh.ConfigureBoundaryFluxes(),
                                        enhancedPrecision = FALSE 
)
{
  ### /A7 -- F11P, F7P, F8P, F9P, F6P
  x <- list(times = times, 
            main = main, 
            variables = variables, 
            balance = balance,
            obsPoints = obsPoints,
            boundaryFluxes = boundaryFluxes
  )
  
  return(x)
}

###############################################################################
##### Configure basic solver
###############################################################################
###############################################################################

#' Basic solver configuration
#' 
#' @param cis If TRUE spatial discretisation is realised by centered-in-space 
#' differencing; if FALSE backward-in-space differencing is to be used for 
#' transport equation. (default: TRUE)
#' @param cit If TRUE temporal discretisation is realised by centered-in-time 
#' differencing; if FALSE backward-in-time or fully implicit differencing is to 
#' be used. (default: TRUE)
#' @param numt  Maximum number of time steps.(default: 1000). (NOTE: if enhanced 
#' precision in print out to file "balance.out" and file 11 "obsPoints.out", is 
#' desired set NUMT equal to a negative number. That is, multiply actual maximum 
#' number of time steps by -1)1
#' @param minit Minimum number of iterations per time step. (default: 2)
#' @param itmax Maximum number of iterations per time step. (default: 80)
#' @param eps Head closure criterion for iterative solution of flow equation, L.
#' (default: 0.0001)
#' @param eps1 Temperature closure criterion for iterative solution of transport 
#' equation, C. (default: 0.001)
#' @param eps2 Velocity closure criterion for outer iteration loop at each time 
#' step, L/T. (default: 0.001)
#' @param hmax Relaxation parameter for iterative solution. See discussion in 
#' Lappala and others (1987) for more detail. Value is generally in the range of
#' 0.4 to 1.2.  (default: 0.7)
#' @param itstop If TRUE simulation is terminated after ITMAX iterations in one 
#' time step; otherwise = F. (default: TRUE)
#' @return Configuration of basic solver

vs2dh.ConfigureBasicSolver <- function (cis = TRUE, 
                                        cit = TRUE,
                                        numt = 1000,
                                        minit = 2,
                                        itmax = 80,
                                        eps = 0.0001,
                                        eps1 = 0.001,
                                        eps2 = 0.001,
                                        hmax = 0.7,
                                        itstop = TRUE)
{
  
  x <- list(cis = cis, 
            cit = cit, 
            numt = numt,
            minit = minit,
            itmax = itmax,
            eps = eps,
            eps1 = eps1,
            eps2 = eps2,
            hmax = hmax,
            itstop = itstop
  )
  return(x)
}

###############################################################################
##### Configure Grid
###############################################################################
###############################################################################

#' Configure Grid 
#' 
#' @param nxr  Number of cells in horizontal or radial direction. (default: 46)
#' @param nly  Number of cells in vertical direction. (default: 34)
#' @param ang  Angle by which grid is to be tilted (Must be between -90 and +90
#' degrees, ang = 0 for no tilting, see Healy (1990) for further discussion), degrees.
#' (default: 0)
#' @param rad  Logical variable. TRUE if radial coordinates are used; otherwise = FALSE
#' (default: FALSE)
#' @param dx  Constant value for grid spacing in horizontal or radial direction.
#' (default: 0.5)
#' @param dy  Constant value for grid spacing in vertical direction. (default: 0.5)
#' @return Grid parameterisation 

vs2dh.ConfigureBasicGrid <- function(nxr = 46, 
                                     nly = 34,
                                     ang = 0,
                                     rad = FALSE,
                                     dx = 0.5,
                                     dy = 0.5
)
{
  
  matrix <- list(nxr = nxr, 
                 nly = nly,
                 ang = ang,
                 rad = rad
  )
  
  if (length(dx)==1) {
    ifac <- 1
    facx <- dx
    dxr <- NA
  } else if (length(dx)==nxr){
    ifac <- 0
    facx <- 1
    dxr <- dx
  } else {
    stop("dx neither a scalar nor a vector of length equal to nxr")
  }
  
  if (length(dy)==1) {
    jfac <- 1
    facz <- dy
    delz <- NA
  } else if (length(dy)==nly){
    jfac <- 0
    facz <- 1
    delz <- dy
  } else {
    stop("dy neither a scalar nor a vector of length equal to nly")
  }
  
  spacing <- list(dxr = dxr, 
                  delz = delz,
                  facx = facx,
                  facz = facz,
                  ifac = ifac, ### only 0 and 1 are implemented 
                  jfac = jfac  ### only 0 and 1 are implemented 
  )
  
  x <- list(matrix = matrix,
            spacing = spacing
  )
  return(x)
}

###############################################################################
##### Configure soil
###############################################################################
###############################################################################

#' Configure Genuchten flow parameters  
#' 
#' @param ratioKzKh  Ratio of hydraulic conductivity in the z-coordinate direction
#'  to that in the x-coordinate direction (Default:1)
#' @param satKh  Saturated hydraulic conductivity (K) at 20 C in the x-coordinate 
#' direction for class ITEX, L/T. (Default: 750 m/d)
#' @param ss  Specific storage (Ss), L^-1. (Default: 0)
#' @param porosity Porosity (f), (Default: 0.39)
#' @param alpha  van Genuchten alpha. NOTE: alpha is as defined by van Genuchten 
#' (1980) and is the negative reciprocal of alpha' used in earlier versions (prior 
#' to version 3.0) of VS2DT, L., (Default: 2.3/m)
#' @param rmc	Residual moisture content,  (Default: 0.05)
#' @param beta van Genuchten parameter, beta' in Healy (1990) and Lappala and 
#' others (1987), (Default:5.8)
#' @return Genuchten flow parameters
#' @seealso \url{http://pubs.usgs.gov/circ/2003/circ1260/pdf/Circ1260.pdf} p.87 for default parameterisation

vs2dh.ConfigureGenuchten <- function (ratioKzKh = 1, 
                                      satKh = 750, 
                                      ss = 0, 
                                      porosity = 0.39,
                                      alpha = 2.3,
                                      rmc = 0.05, 
                                      beta =5.8){
  x <- list(ratioKzKh = ratioKzKh, 
            satKh = satKh, 
            ss = ss, 
            porosity = porosity,
            alpha = alpha,
            rmc = rmc, 
            beta = beta
  )
  return(x)
}

#' Configure soil transport parameters  
#' 
#' @param alphaL  Longitudinal dispersivity, L. (Default: 1 m)
#' @param alphaT  Transverse dispersivity, L. (Default: 0.1 m)
#' @param cs  Heat capacity of dry solids (Cs), Q/L3 C. (Default: 2180000.0 J/m3C)
#' @param ktRmc Thermal conductivity of water sediment at residual moisture content, Q/LTC.
#' (default: 129600.0)
#' @param ktSat	Thermal conductivity of water sediments at full saturation, Q/LC.
#' (Default: 155520.0)
#' @param cw	Heat capacity of water (Cw), which is the product of density times 
#' specific heat of water, Q/L3 C. (default: 4180000.0)
#' @return Soil transport parameters
#' @seealso \url{http://pubs.usgs.gov/circ/2003/circ1260/pdf/Circ1260.pdf} p.87 for default parameterisation

vs2dh.ConfigureTrans <- function (alphaL = 1, 
                                  alphaT = 0.1, 
                                  cs =2180000.0,
                                  ktRmc = 129600.0,
                                  ktSat = 155520.0,
                                  cw = 4180000.0)
{
  x <- list(alphaL = alphaL, 
            alphaT = alphaT, 
            cs = cs,
            ktRmc = ktRmc,
            ktSat = ktSat,
            cw = cw
  )
  return(x)
}

#' Configure soil parameters  
#' 
#' @param hk  flow properties \code{vs2dh.ConfigureGenuchten()}
#' @param ht  transport properties \code{vs2dh.ConfigureTrans()}
#' @return (Genuchten) flow & transport parameters of soil 

vs2dh.ConfigureSoil <- function (hk=vs2dh.ConfigureGenuchten(),
                                 ht=vs2dh.ConfigureTrans()
)
{
  
  x <- list(hk=hk, 
            ht = ht
  )
  return(x)
}

#' Configure soil grid   
#' 
#' @param ntex number of different textual classes (default: 2)
#' @param grid grid matrix as retrieved by \code{vs2dh.ConfigureBasicGrid()}
#' @param irow Textural classes are read for each row (irow=0). This option is preferable if many 
### rows differ from the others. irow=1 (FUNCTIONALITY NOT IMPLEMENTED YET!!!):
### Textual classes are read in by blocks of rows, each block consisting of all the rows 
### in sequence consisting of uniform properties or uniform properties separated 
### by vertical interface. (default: 0)
#' @return Grid with soil indexes  

vs2dh.ConfigureSoilGrid <- function (ntex=2, 
                                     grid = vs2dh.ConfigureBasicGrid(), 
                                     irow = 0 
)
{
  nodes <- grid$matrix$nly * grid$matrix$nxr
  itex <- 1:ntex
  values <- vector()
  for (i in 1:ntex)
  {
    values <- c(values, rep(itex[i], times = nodes/ntex))
  }
  
  jtex <- matrix(data=values, 
                 byrow = TRUE, 
                 nrow = grid$matrix$nly, 
                 ncol = grid$matrix$nxr) 
  
  ### Soil boundary (ITEX=0)
  jtex[c(1,nrow(jtex)),] <- 0
  jtex[,c(1,ncol(jtex))] <- 0
  
  x <- list(irow = irow, 
            jtex=jtex
  )
  
  return(x) 
}

#' Configure Soils
#' 
#' @param props Soil (flow & transport) properties as retrieved by \code{vs2dh.ConfigureSoil()} 
#' ordered by itex number, i.e. first list element = itex1, second = itex2
#' @param grid grid matrix as retrieved by \code{vs2dh.ConfigureSoilGrid}
#' @param wus Weighting option for intercell relative hydraulic conductivity: 1 
#' (for full upstream weighting), 0.5 (for arithmetic mean) and 0.0 (for geometric 
#' mean), (default: 0.5)
#' @return Configuration of soils for flow and energy transport (only hydraulic
#' function type 1 vanGenuchten model is implemented!)

vs2dh.ConfigureSoils <- function 
(
  props=list(
    vs2dh.ConfigureSoil(),
    vs2dh.ConfigureSoil(hk = vs2dh.ConfigureGenuchten(ratioKzKh = 100))
  ),
  grid = vs2dh.ConfigureSoilGrid(ntex = length(props)),
  wus = 0.5
)
{
  if (!is.list(props[[1]]) || length(props[[1]]) != 2) {
    stop("Check data structure of 'props':\n",
         "Should be a list in the form of: ",
         "list(vs2dh.ConfigureSoil(),list(vs2dh.ConfigureSoil(ratioKzKh=100))\n\n")  
  } else {
    
    #### ITEX 0 applied to all boundary cells of grid (i.e. first/last row/column)
    soilBoundary <- vs2dh.ConfigureSoil(hk = vs2dh.ConfigureGenuchten(ratioKzKh = 1, 
                                                      satKh = 0, 
                                                      ss = 0, 
                                                      porosity = 0, 
                                                      alpha = 0, 
                                                      rmc = 0, 
                                                      beta = 0), 
                                        ht = vs2dh.ConfigureTrans(alphaL = 0, 
                                                                  alphaT = 0, 
                                                                  cs = 0, 
                                                                  ktRmc = 0, 
                                                                  ktSat = 0, 
                                                                  cw = 0)
                                        
                                                     )
    
    ntex <- length(props) + 1 ### +1 because of additional zero boundary soil!
    itex <- 0:(ntex-1) ## starting from zero (0 = boundary soil!)
    
    if (!is.null(ntex) & ntex > 0)
    {
      props1 <- labeledList(inpLst = c(list(soilBoundary), props), 
                           labelTxt = "itex")
    } else {
      props1 <- NULL
    }
    
    base <- list(itex = itex, 
                 ntex = ntex, ### ntex: total number of different "itex" classes     
                 grid = grid,
                 wus = wus,
                 hft = 1, ### hydraulic function type: van Genuchten
                 ### nprop: Number of flow properties to be read in for each textural class. 
                 ###6 when using Brooks and Corey, van Genuchten or Rossi-Nimmo functions 
                 ###8 when using Haverkamp functions. 
                 ###6 plus number of data points in table.  When using tabulated data 
                 ### [For example, if the number of pressure heads in the table is equal 
                 ### to N1, then setNPROP=3*(N1+1)+3]
                 nprop = 6, ### set to 6 because only vanGenuchten model is implemented!
                 nprop1 = 6
    )
    x <- list(base = base, 
              props = props1)
    return(x)
  }
}

###############################################################################
##### Initial conditions
###############################################################################
###############################################################################

#' Configure initial head/soil moisture distribution
#' 
#' @param values If NA values are supplied (default): initial conditions are defined 
#' in terms of pressure head, and an equilibrium profile is specified above a free-water 
#' surface at a depth of DWTX until a pressure head of HMIN is reached. All 
#' pressure heads above this are set to HMIN. If one value is supplied: all 
#' initial conditions in terms of pressure head (if asPressureHead=TRUE) or
#' moisture(if asPressureHead=FALSE) are set equal to value of "hvalues". 
#' @param dwtx  Depth to free-water surface above which an equilibrium profile 
#' is computed, L. All All pressure heads above DWTX this are set to HMIN. 
#' (default: 3.7291667) 
#' @param hmin Minimum pressure head to limit height of equilibrium profile, L. 
#' Must be negative. (default: -3.98)
#' matrix (with columns equal to NXR and rows equals to NLY) needs to be supplied   
#' @param asPressureHeads If TRUE, initial flow conditions are pressure heads, else: 
#' moisture content (default: TRUE)
#' @return Initial flow conditions

vs2dh.ConfigureInitialFlow <- function (values = NA,
                                        dwtx = 3.7291667,
                                        hmin = -3.98,
                                        asPressureHeads = TRUE
)
{
  phrd <- FALSE
  hiu <- NA
  hifmt <- NA 
  hvalues <- NA 
  
  if (asPressureHeads | (length(dwtx)==1 & length(hmin)==1)) {
    phrd <- TRUE
  }
  if (!is.matrix(values))
  {
    if (is.na(values))
    {
      hiread <- 2
      hfactor <- 1
      dwtx <- dwtx
      hmin <- hmin
    } else if (length(values)==1)
    {
      hiread <- 0
      hfactor <- values
      dwtx <- NA
      hmin <- NA
    }}
  else if  (is.matrix(values)) {
    hiread <- 1
    hfactor <- 1
    hiu <- 5
    hifmt <- "free"
    hvalues <- values
    dwtx <- NA
    hmin <- NA
  } else {
    stop("\nCheck parameter 'values'. It should be either:
1) NA (i.e.initial conditions are defined in terms of pressure head, and an equilibrium profile) 
2) a constant (i.e. constant initial head/saturation for whole model domain or 
3) a matrix with number of rows (equals to nxr) and columns (equals to nly)\n\n")
  }
  x <- list(phrd = phrd, ### TRUE, i.e. initial conditions are read in as pressure heads
            hiread = hiread, ### only B11-Iread=2 implemented, no other options allowed!
            hfactor = hfactor, ### constant value for B11-Iread=2
            hiu = hiu,
            hifmt = hifmt,
            hvalues = hvalues,
            dwtx = dwtx, 
            hmin = hmin
  )
  return(x)
}


#' Configure initial temperature distribution:  
#' 
#' @param values either constant (for constant initial temperature for whole model
#' domain, or matrix with number of rows (equals to nxr) and columns (equals to nly), 
#' with user defined initial temperature distribution (default: 10 degree C, 
#' constant temperature)
#' @return Initial temperature distribution parameterisation

vs2dh.ConfigureInitialTemp <- function (values=10
                                        #values=matrix(data=2,nrow = 34, ncol = 46)
)
  
{
  if (!is.matrix(values) & length(values)==1) 
  { 
    tiread  <- 0
    tfactor <- values
    tiu <- NA
    tifmt <- NA 
    tvalues <- NA
  } else if (is.matrix(values)) {
    tiread  <- 1
    tfactor <- 1 
    tiu <- 5 ### i.e. IU=5, i.e. from "vs2dh.dat"  
    tifmt <- "free" 
    tvalues <- values
  } else {
    stop("\nCheck parameter 'values'. It should be either:
1) a constant (i.e. constant initial temperature for whole model domain or 
2) a matrix with number of rows (equals to nxr) and columns (equals to nly)\n\n")
  }
  
  x <- list(tiread = tiread,
            tfactor = tfactor, 
            tiu = tiu,   
            tifmt = tifmt,
            tvalues = tvalues
  )
  
  return(x)
}


#' Configure initial conditions
#' 
#' @param flow  initial flow conditions (i.e. pressure head or saturation distribution
#' for model domains, as retrieved by vs2dh.ConfigureInitialFlow()
#' @param temp  initial temperature distribution as retrieved by
#' vs2dh.ConfigureInitialTemp()
#' @return Initial model conditions

vs2dh.ConfigureInitial <- function (flow = vs2dh.ConfigureInitialFlow(),
                                    temp = vs2dh.ConfigureInitialTemp()
)
{
  x <- list(flow = flow,
            temp = temp
  )
  return(x)
}


###############################################################################
##### Boundary conditions
###############################################################################
###############################################################################

#' Configure boundary conditions
#' 
#' @param jj vector with row numbers of nodes
#' @param nn vector with column numbers of nodes
#' @param ntx vector with node type identifier for boundary conditions. 
#'0 (for no specified boundary (needed for resetting some nodes after initial 
#'recharge period); 
#'1 (for specified pressure head); 
#'2 (for specified flux per unit horizontal surface area in units of L/T); 
#'3 (for possible seepage face);  
#'4 (for specified total head); 
#'5 (for evaporation, Note: is not implemented yet!); 
#'6 (for specified volumetric flow in units of L3/T). 
#'7 (for gravity drain). (The gravity drain boundary condition allows gravity driven vertical fow out of the domain assuming a unit vertical hydraulic gradient. Flow into the domain cannot occur.)"
#' @param pfdum	vector with specified head for NTX = 1 or 4 or specified flux for NTX = 2 or 
#' 6. If codes 0, 3, 5, or 7 are specified, the line should contain a dummy value
#' for PFDUM or should be terminated after NTX by a blank and a slash (/).
#' @param ntc  vector with node type identifier for transport boundary conditions. 
#' 0 (for no specified boundary); 
#' 1 (for specified temperatures)"
#' @param  cf	vector with specified temperature for NTC = 1 or NTX = 1, 2, 4, 6, or 7. 
#' Present only if TRANS = T.
#' @param  importData Optionally a data.frame with the colnames 
#' (jj,nn,ntx,pfdum,ntc,cf) can be used. In this case the input for the function
#' parameters (jj,nn,ntx,pfdum,ntc,cf) will be ignored! 
#' @param ibc Code for reading in boundary conditions: 0 (by individual node),
#' 1 (by row/column). (default: 0), Note: currently only option 0 is implemented 

#' @return Boundary conditions

vs2dh.ConfigureBoundaryCondition <- function ( jj = 5,
                                               nn = 17:26, 
                                               ntx = 2,
                                               pfdum = 0.009778035, 
                                               ntc = 0,
                                               cf = 10.0,
                                               importData = NULL,
                                               ibc = 0)
  
{
  
  if (is.data.frame(importData))
  {
    boundary <- importData
  } else {
    
    boundary <- data.frame(jj,
                           nn, 
                           ntx, 
                           pfdum,
                           ntc,
                           cf)
  }
  x <- list(ibc = ibc, 
            boundary = boundary
  )
  
  
  return(x)
}


###############################################################################
##### Configure recharge periods
###############################################################################
###############################################################################
#' Helper function: node pairs
#' 
#' @param j Row of each cell 
#' @param n Column of each cell 
#' @return data.frame with node pairs 

nodePairs <- function(j,n)
{
  nodes <- data.frame()
  
  n_n <- length(n)
  n_j <- length(j)
  if (n_n==n_j) 
  {
    nodes <- data.frame(j=j, n=n)
  } else if (n_j==1)
  {
    nodes <- data.frame(j=rep(j,times=n_n), n=n)
  } else if (n_n==1)
  {
    nodes <- data.frame(j=j, n= rep(n,times=n_j))
  } else {
    stop("j & n must either have the same length or one has length=1")
  }
  return(nodes)
}



#' Configure seepage face
#' 
#' @param j Row and column of each cell on possible seepage face, 
#' in order from the lowest to the highest elevation; JJ pairs of 
#' values are required.
#' @param n Column of each cell on possible seepage face, 
#' in order from the lowest to the highest elevation; JJ pairs of 
#' values are required.
#' @param jlast Number of the node which initially represents the highest node 
#' of the seep; value can range from 0 (bottom of the face) up to JJ (top of the 
#' face). (default: 0)
#' @return Seepage face

vs2dh.ConfigureSeepageFace <- function (j = c(30:20),
                                        n = 2,
                                        jlast=0
)                                                                                         
{
  
  ###Number of nodes on the possible seepage face
  nodes <- nodePairs(j = j,
                     n = n)
  
  
  x <- list(nodes = nodes,
            jj = nrow(nodes),
            jlast=jlast
  )
  return(x)
}


#' Configure seepage
#' 
#' @param seepFaces list of seepage faces as retrieved by vs2dh.ConfigureSeepageFace()
#' @return Seepage config
#' 

vs2dh.ConfigureSeepage <- function (seepFaces=list(vs2dh.ConfigureSeepageFace(),
                                                   vs2dh.ConfigureSeepageFace(j = 30:20,
                                                                              n = 45)
)      
)                                                                                         
{
  nfcs <- length(seepFaces)
  
  if (!is.null(nfcs) && nfcs > 0) 
  {
    seepFaces <- labeledList(inpLst=seepFaces)
    x <- list(seep=TRUE,   
              nfcs = nfcs,
              seepFaces = seepFaces
    )
  } else {
    x <- list(seep = FALSE)
  }
  
  return(x)
}

#' Configure recharge period options
#' 
#' @param prnt If TRUE, = T if heads, temperature, moisture contents, and/or 
#' saturations are to be printed to file "vs2dt.out" after each time step; If 
#' FALSE they are to be written to file "vs2dt.out" only at observation times and
#' ends of recharge periods. (default: FALSE)
#' @param rbcit NOT IMPLEMENTED YET!!!!!!! If TRUE evaporation is to be simulated 
#' for this recharge period;if FALSE no evaporation is simulated for this recharge 
#' period (default: FALSE)
#' @param retsim NOT IMPLEMENTED YET!!!!!!! If TRUE evapotanspiration (plant-root 
#' extraction) is to be simulated for this recharge period; if FALSE no evaporation is simulated for 
#' this recharge period (default: FALSE). 
#' @param seepage If TRUE are to be simulated for this recharge period; seepage face
#' not simulated for this recharge period. (default: vs2dh.ConfigureSeepage())
#' @return Recharge period

vs2dh.ConfigureRechargePeriodOptions <- function (prnt = FALSE,
                                                  rbcit = FALSE,
                                                  retsim = FALSE,
                                                  seepage = vs2dh.ConfigureSeepage()                                                  
)
{
  x <- list(prnt = prnt,
            rbcit = rbcit,
            retsim = retsim,
            seepage = seepage
  )
  return(x)
}


#' Configure recharge period solver
#' 
#' @param delt  	Length of initial time step for this period, T. (default: 1.0E-4)
#' @param tmlt  	Multiplier for time step length. (default: 1.2)
#' @param dltmx  	Maximum allowed length of time step, T. (default: 1)
#' @param dltmin	Minimum allowed length of time step, T. (default: 1.0E-4)
#' @param tred    Factor by which time-step length is reduced if convergence 
#' is not obtained in ITMAX iterations. Values usually should be in the range 0.1
#' to 0.5. If no reduction of time-step length is desired, input a value of 0.0.
#' (default: 0.1)
#' @param dsmax  	Maximum allowed change in head per time step for this period, L.
#' (default: 10)
#' @param sterr	Steady-state head criterion; when the maximum change in head 
#' between successive time steps is less than STERR, the program assumes that 
#' steady state has been reached for this period and advances to next recharge 
#' period, L. (default: 0)
#' @param pond Maximum allowed height of ponded water for constant flux nodes. 
#' See Lappala and other (1987) for detailed discussion of POND, L. (default: 0)
#' @return Recharge period times

vs2dh.ConfigureRechargePeriodSolver <- function (
  delt = 1.0E-4,
  tmlt = 1.2,
  dltmx = 1,
  dltmin = 1.0E-4,
  tred = 0.1, 
  dsmax = 10, 
  sterr = 0,
  pond = 0
)                                                  
  
{
  x <- list(delt = delt,
            tmlt = tmlt,
            dltmx = dltmx,
            dltmin = dltmin,
            tred = tred, 
            dsmax = dsmax, 
            sterr = sterr,
            pond = pond
  )
  return(x)
}


#' Configure recharge period
#' 
#' @param tper    Length of this recharge period, T. (default: 0.1)
#' @param options as retrieved by vs2dh.ConfigureRechargePeriodOptions()
#' @param solver as retrieved by vs2dh.ConfigureRechargePeriodSolver()
#' @param boundary as retrieved by vs2dh.ConfigureBoundaryCondition()
#' @return Recharge period

vs2dh.ConfigureRechargePeriod <- function 
(
  tper = 0.1,
  options = vs2dh.ConfigureRechargePeriodOptions(),
  solver = vs2dh.ConfigureRechargePeriodSolver(),
  boundary = vs2dh.ConfigureBoundaryCondition()
)
  
{
  x <- c(list(tper=tper),
         options,
         solver,
         boundary
  ) 
  
  return(x)
}



#' Configure recharge periods 
#' @param periods list with one or multiple recharge periods with structure as retrieved by 
#' vs2dh.ConfigureRechargePeriod()
#' @return List with parameterisation of recharge periods

vs2dh.ConfigureRechargePeriods <- function (periods = list(vs2dh.ConfigureRechargePeriod(),
                                                           vs2dh.ConfigureRechargePeriod(tper = 10)))
  
{
  ### nrech: Number of recharge periods. (NOTE: set NRECH to a negative number (-1 
  ### times actual number of recharge periods) to output binary values of head 
  ### and concentration at selected allows the simulation to be restarted at 
  ### observation times to file fort.12. Selecting this option allows the simulation 
  ### to be restarted at any observation time; however, it may require a large 
  ### amount of disk storage space.) -> neg. number not implemented here!
  
  nrech <- length(periods) 
  
  if (!is.null(nrech) && nrech > 0) 
  {
    periods <- labeledList(inpLst=periods)
    
    x <- list(nrech = nrech,
              periods = periods
    )
    
  } else {
    x <- list(nrech = nrech)
  }
  
  return(x)
}

#' Configure basic simulation options: energy transport, evaporation or 
#' evapotranspiration should be simulated? (FUNCTIONALITY NOT IMPLEMENTED YET! 
#' DO NOT CHANGE default PARAMETERISATION) 
#' 
#' @param trans Should energy transport be simulated? (default: TRUE), Option
#' FALSE currently NOT IMPLEMENTED YET!!!!!!! 
#' @param bcit NOT IMPLEMENTED YET!!!!!!! If TRUE evaporation is to be simulated 
#' for this simulation;if FALSE no evaporation is simulated for this simulation
#' (default: FALSE)
#' @param etsim NOT IMPLEMENTED YET!!!!!!! If TRUE evapotanspiration (plant-root 
#' extraction) is to be simulated for this simulation; if FALSE no evaporation is simulated for 
#' this simulation (default: FALSE).
#' @return   Return basic simulation options 
vs2dh.ConfigureBasicOptions <- function (trans=TRUE,
                                         bcit = FALSE,
                                         etsim = FALSE)
  
{
  x <- list(trans = trans,
            bcit = bcit,
            etsim = etsim)
  return(x)
}


###############################################################################
##### Group different parts
###############################################################################
#' Configure basic model parameters 
#' 
#' @param titl 80-character problem description (formatted read, 20A4) 
#' @param units as retrieved by  vs2dh.ConfigureBasicUnits()
#' @param time as retrieved by  vs2dh.ConfigureBasicTime()
#' @param output as retrieved by vs2dh.ConfigureBasicTime()
#' @param solver retrieved by vs2dh.ConfigureBasicSolver()
#' @param grid retrieved by vs2dh.ConfigureBasicGrid()
#' @param options as simulation options as retrieved by vs2dh.ConfigureBasicOptions
#' (default: TRUE)
#' @return Basic configuration

vs2dh.ConfigureBasic <- function (titl = "My title",
                                  units = vs2dh.ConfigureBasicUnits(),
                                  time = vs2dh.ConfigureBasicTime(),
                                  output = vs2dh.ConfigureBasicOutput(),
                                  solver = vs2dh.ConfigureBasicSolver(), 
                                  grid = vs2dh.ConfigureBasicGrid(),
                                  options = vs2dh.ConfigureBasicOptions())
{
  #### Set automatically
  output$times$numt <- solver$numt
  
  x <- list(titl = titl,
            units = units,
            time = time,
            output = output,
            solver = solver,
            grid = grid,
            options = options)  
  
  return(x)
}

#' Configure complete vs2dh parameterisation
#' 
#' @param basic parameterisation (title, units, time, output, solver, grid, options), 
#' as retrieved by  vs2dh.ConfigureBasic(),
#' @param soils retrieved by vs2dh.ConfigureSoils()
#' @param initial initial conditions for flow (pressure head or moisture contents and transport 
#' (temperatures), as retrieved by  vs2dh.ConfigureInitial()
#' @param recharge recharge periods as retrieved by vs2dh.ConfigureRechargePeriods()
#' @return Complete vs2dh parameterisation in R style

vs2dh.Configure <- function (basic = vs2dh.ConfigureBasic(),
                             soils = vs2dh.ConfigureSoils(),
                             initial = vs2dh.ConfigureInitial(),
                             recharge = vs2dh.ConfigureRechargePeriods()
)
{
  x <- list(basic = basic,
            soils = soils,
            initial = initial,
            recharge = recharge
  )
  return(x)
}



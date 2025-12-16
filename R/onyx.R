cacheEnv <- new.env()

#' onyx
#' 
#' Starts Onyx executable to display a path diagram from either an OpenMx or
#' lavaan model representation.
#'
#' @param model An OpenMx or lavaan model. Defaults to NULL. If omitted, Onyx is started with an empty desktop, otherwise
#' the specified model is rendered as path diagram
#' @param onyyfile path to Onyx executable (onyx-***.jar). Defaults to NULL. If NULL, Onyx searches local directors, otherwise downloads Onyx from official repository.
#' @param mode batch operation mode (experimental)
#'
#' @examples 
#' 
#' require(OpenMx)
#' data(demoOneFactor)
#' manifests <- names(demoOneFactor)
#' latents <- c("G")
#' factorModel <- mxModel("One Factor",
#'                        type="RAM",
#'                        manifestVars = manifests,
#'                        latentVars = latents,
#'                        mxPath(from=latents, to=manifests),
#'                        mxPath(from=manifests, arrows=2),
#'                        mxPath(from=latents, arrows=2,
#'                               free=FALSE, values=1.0),
#'                        mxData(cov(demoOneFactor), type="cov",
#'                               numObs=500))
#' fit <- mxRun(factorModel)
#' \dontrun{
#' onyx(fit)
#' }
#' 
#' @export

onyx<-function(model=NULL, onyxfile=NULL, batch=NULL, java_path="")
{
  
  if (isFALSE(nzchar(Sys.which("java")))) {
    warning("It seems that java is not on your file path. Consider setting the java_path argument when calling onyR().")
  }
  
  if (java_path != "") {
    java_path = add_trailing_sep(java_path) 
  }
 
  # attempt to retrieve the onyxfile from the package's cache environment
  # return NULL if nothing stored in cacheEnv
  if (is.null(onyxfile)) {
    onyxfile <- get0("onyxfile", envir=cacheEnv, ifnotfound=NULL)
  }
  
  # is there a local onyx file?
  if (is.null(onyxfile)) {
  files <- list.files()
  hasonyx <- sapply(files, function(x){length(grep("onyx.*jar",x,value=FALSE))>0},simplify = TRUE)
  if (sum(hasonyx) > 0) {
    if (sum(hasonyx)>2) {
      warning("Found several local onyx executables!")
    }
    idx <- which(hasonyx)[1]
    onyxfile <- names(hasonyx)[idx]
  }
  }
  
  # the following part tries to find onyx.jar from command line arguments.
  # May be removed in future
  #if (is.null(onyxfile)) {
  #  options <- commandArgs(trailingOnly = F)
  #  filearg <- "--file="
# 
#    if (length(grep(filearg, options))!=0) {
 #     onyxfile <- sub(filearg, "", options[grep(filearg, options)])
  #  }

  #}
  
  # if there is no Onyx jar, download it from official repository
  if (is.null(onyxfile)) {
    warning("Could not find a local Onyx version. Trying to download Onyx from the official repository.")
    onyxfile <- tempfile(pattern="onyx_",
                         fileext=".jar")
    download.file(url="https://onyx-sem.com/wp-content/uploads/2021/08/onyx-stable.jar",
                  destfile = onyxfile, mode="wb")
    if (!file.exists(onyxfile)) {
      stop("Download failed!")
    }
    # store the file reference in the package environment
    assign("onyxfile",onyxfile, envir=cacheEnv)
  }
  
  # catches if onyx.jar is not available

  if (is.null(onyxfile)) {
    stop("Could not find a valid Onyx executable (e.g., onyx.jar)!"+
           "Please put Onyx in the local directory, specify a path to the file,"+
           " or download Onyx from http:\\\\onyx.brandmaier.de.")
  }
    
  if (!file.exists(onyxfile)) {
    stop(paste("Onyx executable does not exist: ",onyxfile))  
  }
  

  
  # stores onyx or lavaan parsed model in temp file and calls onyx with this file, or empty call. 
  if (!is.null(model)) {

	if (inherits(model,"MxModel") || inherits(model,"MxRAMModel")) {
  		rep <- parser.OpenMx(model,"onyxR")
	} else if (inherits(model,"lavaan")) {	
  	  rep <- parser.lavaan(model,"onyxR")
	} else {
	  	rep <- parser.lavaan(model,"onyxR", string.representations=TRUE)
	}
    
    xmlhead <- "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"no\"?>"
    rep <- paste(xmlhead, rep)
    
  	fn <- tempfile()
  	cat(rep, file=fn)
  	
  	if (is.null(batch)) {
    	cmd <- paste(java_path,"java","-cp",onyxfile,"Master","--input-file ",fn)
  	} else {
  	  outf <- tempfile(fileext = ".png")
  	  cmd <- paste(java_path,"java -cp",onyxfile," Master --batch --output-filetype png --input-file",fn," --output-file",outf)

  	}
  	
  } else {
    cmd <- paste(java_path,"java","-cp",onyxfile,"Master")
  }
  
  # system call depends on operating system
  sysname <- Sys.info()[['sysname']]
  if (sysname=="Windows") {
    # OS that need to spawn a new shell explicitly should go here:
    system("cmd.exe", input = cmd, wait = FALSE)
  } else {  
    # OS that spawn a new shell for each subprocess should go here
	  system(cmd,wait=FALSE)
  }
  
  if (!is.null(batch)) {
    rpng <- png::readPNG(outf, native=TRUE)
    plot(c(0,100),c(0,100),type="n",xlab="",ylab="",axes = FALSE)
    rasterImage(rpng,0,0,100,100)
  }
  
}

add_trailing_sep <- function(path) {
  sep <- .Platform$file.sep
  if (!grepl(paste0(sep, "$"), path)) {
    path <- paste0(path, sep)
  }
  path
}

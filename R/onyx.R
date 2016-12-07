onyx<-function(model=NULL, onyxfile=NULL)
{
  
  # the following part tries to find onyx.jar from command line arguments.
  # May be removed in future
  if (is.null(onyxfile)) {
    options <- commandArgs(trailingOnly = F)
    filearg <- "--file="
 
    if (length(grep(filearg, options))!=0) {
      onyxfile <- sub(filearg, "", options[grep(filearg, options)])
    }

  }
  
  # if there is no Onyx jar, download it from official repository
  if (is.null(onyxfile)) {
    warning("Could not find a local Onyx version. Trying to download Onyx from the official repository.")
    onyxfile <- tempfile()
    download.file("http://onyx.brandmaier.de/onyx-1.0-937.jar",destfile = onyxfile)
    if (!file.exists(onyxfile)) {
      stop("Download failed!")
    }
  }
  
  # catches if onyx.jar is not available
  
  if (!file.exists(onyxfile)) {
    stop("Could not find Onyx executable (e.g., onyx.jar)! Please specify a path to the file or download Onyx from http:\\\\onyx.brandmaier.de.")
  }
  
  # stores onyx or lavaan parsed model in temp file and calls onyx with this file, or empty call. 
  if (!is.null(model)) {

	if (inherits(model,"MxModel") || inherits(model,"MxRAMModel")) {
  		rep <- parser.OpenMx(model,"onyxR")
	} else {
	  	rep <- parser.lavaan(model,"onyxR")
	}
  	fn <- tempfile()
  	cat(rep, file=fn)
  	cmd <- paste("java","-cp",onyxfile,"Master","--input-file ",fn)
  } else {
    cmd <- paste("java","-cp",onyxfile,"Master")
  }
  
	system(cmd,wait=F)
}

onyx<-function(model=NULL, path="")
{
  # the following part tries to find onyx.jar in the path of the script and the home directory.
  if (nchar(path)==0) {
    options <- commandArgs(trailingOnly = F)
    filearg <- "--file="
    scriptname <- sub(filearg, "", options[grep(filearg, options)])
    path <- dirname(scriptname)
    onyxpath <- paste(path,"onyx.jar",sep="")
    if (!file.exists(onyxpath)) path <- "" 
  }
  
  # catches if onyx.jar is not available
  onyxpath <- paste(path,"onyx.jar",sep="")
  if (!file.exists(onyxpath)) {
    stop("file onyx.jar not found. You can download it at http:\\\\onyx.brandmaier.de.")
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
  	cmd <- paste("java","-cp",onyxpath,"Master","--input-file ",fn)
  } else {
    cmd <- paste("java","-cp",onyxpath,"Master")
  }
  
	system(cmd,wait=F)
}

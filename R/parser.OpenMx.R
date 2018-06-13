
#
# converter of OpenMx model into Onyxml
#

parser.OpenMx <- function(model, name="unnamed", submodel=FALSE, id.offset=0)
{

  # generate Onyx file header
  if (!submodel) {
    xml <- paste( "<model name=\"",name,"\" specificationType=\"Onyx\" specificationVersion=\"1.0\">\n<graph>\n",sep="");
  } else {
    xml <- ""
  }
  
  has.submodels <- length(model@submodels)!=0
  
  # check for submodels
  if (has.submodels) {
    
    for (i in 1:length(model@submodels)) {
      smodel <- model@submodels[[i]]
      xml <- paste(xml, parser.OpenMx(smodel, paste0(name,"_",i),submodel=TRUE, id.offset=id.offset))
      id.offset <- id.offset + length(smodel@manifestVars)+length(smodel@latentVars)+1
    }
    
  } else {
    

  
  # check for RAM matrices
  checkRAM = !is.null(model$matrices$A) & !is.null(model$matrices$S) & !is.null(model$matrices$F)
  if (!checkRAM & !has.submodels) {
    stop("Only RAM models supported!")
  }
  

  
  # extract latents and manifests
	manifests <- model@manifestVars
	latents <- model@latentVars
	variables <- c(manifests,latents)

	# mean structure ?
	triangle <- FALSE;
	triangleId <- length(variables)	+ id.offset
	triangleXml <- paste("<Node caption=\"one\" id=\"",triangleId,"\" constant=\"true\"/>\n",sep="");


	  
	xml <- paste (xml, "<!-- manifest variables -->\n",sep="")

	# construct all manifest nodes
	if (length(manifests) > 0) {
	for (i in 1:length(manifests)) {
		xml <- paste(xml,"<Node caption=\"", manifests[i] ,  "\" latent=\"false\" id=\"",id.offset+(i-1),"\" />\n",sep="");
	}	
	}
	xml <- paste (xml, "<!-- latent variables -->\n",sep="")
	
	# construct all latent nodes
	if (length(latents) > 0) {
	for (i in 1:length(latents)) {
		xml <- paste(xml,"<Node caption=\"", latents[i] ,  "\" latent=\"true\" id=\"", id.offset+length(manifests)+(i-1),"\" />\n",sep="");
	}}
	
	
		xml <- paste (xml, "<!-- directed edges -->\n",sep="")

	# construct all directed edges
	A <- model@matrices$A
	lenA <- dim(A)[1]
	for (i in 1:lenA)
	{
		for (j in 1:lenA)
		{
			dnames <- dimnames(model$A@labels)[[1]]
			sourceNodeId = id.offset + which(variables==dnames[j])-1;
			targetNodeId = id.offset + which(variables==dnames[i])-1;
			
			fixed <- !A@free[i,j];
			value <- A@values[i,j];
			
			if ((value==0) && (fixed)) next;

			
			label <- A@labels[i,j];
			
			#definitionVariable <- is.character(value) && !is.na(label);
			definitionVariable <-  !is.na(label) && !(label %in% manifests)
			definitionVariable <- FALSE
			
			pString <- "";
			if (!is.na(label)) {
				pString <- paste("parameterName=\"",label,"\"",sep="");
			
			} 
			
			dString <- "";
			if (definitionVariable) {
				dString <- "definitionVariable=\"true\"";
			}
			
			xml <- paste(xml,"<Edge sourceNodeId=\"",sourceNodeId,"\"  targetNodeId=\"",targetNodeId,"\" doubleHeaded=\"false\" fixed=\"",fixed,"\" ",pString," ",dString, " value=\"",value,"\" />\n", sep="")
		}
	}
	
		xml <- paste (xml, "<!-- undirected edges -->\n",sep="")

	
	# construct all undirected edges
	S<- model@matrices$S
	lenS <- dim(S)[1]
	for (i in 1:lenS)
	{
		for (j in i:lenS)
		{
			
			dnames <- dimnames(model$S@labels)[[1]]
			sourceNodeId = id.offset + which(variables==dnames[i])-1;
			targetNodeId = id.offset + which(variables==dnames[j])-1;
			
			fixed <- !S@free[i,j];
			value <- S@values[i,j];
			label <- S@labels[i,j];
						
			
			if ((value==0) && (fixed)) next;
			
			pString <- "";
			if (!is.na(label)) {
				pString <- paste("parameterName=\"",label,"\"",sep="");
			} 
			
			
			xml <- paste(xml,"<Edge sourceNodeId=\"",sourceNodeId,"\"  targetNodeId=\"",targetNodeId,"\" doubleHeaded=\"true\" fixed=\"",fixed,"\" ",pString," value=\"",value,"\" />\n", sep="")
		}
	}	
	
		xml <- paste (xml, "<!-- directed edges from means -->\n",sep="")

	
	if (!is.null(model$M)) {
	# construct mean edges (really mean...)
	M <- model@matrices$M
	lenM <- dim(M)[2]
	
		for (j in 1:lenM)
		{
			dnames <- dimnames(model$M@labels)[[2]]
			sourceNodeId = triangleId;
			targetNodeId = id.offset + which(variables==dnames[j])-1;
			
			fixed <- !M@free[1,j];
			value <- M@values[1,j];
			
			#cat(j, fixed, value, label, "\n")
			
			if ((value==0) && (fixed)) next;

			
			if (!triangle) {
				xml <- paste(xml, triangleXml);
				triangle <- TRUE;
			}
			
			label <- M@labels[1,j];
			
			parampart <- ""
			if (!is.na(label)) {
				parampart <- paste("\" parameterName=\"",label,sep="");
			}
			
			xml <- paste(xml,"<Edge sourceNodeId=\"",sourceNodeId,"\"  targetNodeId=\"",targetNodeId,"\" doubleHeadeded=\"false\" fixed=\"",fixed,parampart,"\" value=\"",value,"\" />\n", sep="")
		}
	}
	
		
  }

  if (!submodel)
    xml <- paste(xml, "\n</graph>\n</model>\n" ,sep="");		

	return(xml);
}


#variables <- ls();
#for (i in 1:length(variables))
#{
#	if (class(eval(parse(text=variables[i]))) == "MxRAMModel"){#
#		cat( parser(eval(parse(text=variables[i])), variables[i])  );
#		cat("\n")
#	}
#}#
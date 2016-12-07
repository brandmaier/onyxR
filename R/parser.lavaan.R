parser.lavaan <- function(model, name="") {

lstr <- lavaanify(model, auto.var=TRUE)

xml <- paste( "<model name=\"",name,"\" specificationType=\"Onyx\" specificationVersion=\"1.0-500\">\n<graph>\n",sep="");

known <- list()

isknown <- function(key)
{
  return(key %in% names(known))
}

idx <- 0
for (i in 1:dim(lstr)[1]) {
  left <- lstr$lhs[i]
  op <- lstr$op[i]
  right <- lstr$rhs[i]
  free <- lstr$free[i]
  ustart <- lstr$ustart[i]
  latentleft <- TRUE
  latentright <- TRUE;
  if (op == "=~") { latentleft <- TRUE; latentright <- FALSE}
  
  if (isknown(left)) {
    lid <- known[[left]]
  } else {
    xml <- paste(xml,"<node caption=\"", left ,  "\" latent=\"",latentleft,"\" id=\"",
                 (idx),"\" />\n",sep="");
    known[[left]] <- idx
    lid <- idx
    idx <- idx + 1
  }
  
  if (isknown(right)) {
      rid <- known[[right]]
  } else {
    xml <- paste(xml,"<Node caption=\"", right ,  "\" latent=\"",latentright,"\" id=\"",
                 (idx),"\" />\n",sep="");
    known[[right]] <- idx
    rid <- idx
    idx <- idx + 1
  }
  
  if (op == "~~")
    doubleheaded <- "true" 
  else
    doubleheaded <- "false"
  
  if (free == 0) {
    fixed <- "true"
    pString <- ""
    }
  else {
    fixed <- "false"
    pname <- lstr$plabel[i]
    pString <- paste("parameterName=\"",pname,"\"",sep="")
  }
  
  
  
  aString <- "arrowHead=\"1\" "#definitionVariable=\"false\""
  
  value <- 1
  if (!is.na(ustart)) value <- ustart
  
  # some postprocessing. Onyx does not like fixed path with zero values
  if ((op == "~~")) {
    if (value==0 && free==0) {
      value <- 1
    }
  }
  
  xml <- paste(xml,"<edge sourceNodeId=\"",lid,"\"  targetNodeId=\"",
    rid,"\" doubleHeaded=\"",doubleheaded,"\" fixed=\"",fixed,"\" ",
    pString," ",aString, " value=\"",value,"\" />\n", sep="")
} 
  
 
  
xml <- paste(xml, "\n</graph>\n</model>\n" ,sep="");
  
return(xml)

}
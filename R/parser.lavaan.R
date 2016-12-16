parser.lavaan <- function(model, name="", string.representations=FALSE) {

if (string.representations) {
  lstr <- lavaanify(model, auto.var=TRUE)
} else {
  lstr <- lavaan::parameterTable(model)
}
xml <- paste( "<model name=\"",name,"\" specificationType=\"Onyx\" specificationVersion=\"1.0-500\">\n<graph>\n",sep="");

multigroup <- length(unique(lstr$group))>1

known <- list()

mean.idx <- NULL

isknown <- function(key)
{
  return(key %in% names(known))
}

setknown <- function(key, value)
{
  known[[key]] <- value
}

idx <- 0
for (i in 1:dim(lstr)[1]) {
  left <- lstr$lhs[i]
  op <- lstr$op[i]
  right <- lstr$rhs[i]
  free <- lstr$free[i]
  ustart <- lstr$ustart[i]
  latentleft <- TRUE
  latentright <- TRUE
  grp <- lstr$group[i]
  
  if ("est" %in% names(lstr)) {
    est <- lstr$est[i]
  } else {
    est <- NULL
  }
  
  if (op == "=~") { latentleft <- TRUE; latentright <- FALSE}
  
  meanpath <- FALSE
  if (op == "~1") {meanpath <- TRUE}
  
  if (isknown(left)) {
    lid <- known[[left]]
  } else {
    
    mg <- ""
    if (multigroup) {
      mg <- paste("groupValue=\"",grp,"\"",sep="")
    }
    
    xml <- paste(xml,"<node caption=\"", left ,  "\" latent=\"",latentleft,"\" id=\"",
                 (idx),"\" ",mg," />\n",sep="");
    known[[left]] <- idx
    lid <- idx
    idx <- idx + 1
  }
  
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
  
  if (!is.null(est)) {
    value <- est
  }
  
  
  if (meanpath) {
    
    if (is.null(mean.idx)) {
        mean.idx <- idx
        triangleXml <- paste("<Node caption=\"one\" id=\"",idx,"\" constant=\"true\"/>\n",sep="");
        xml <- paste(xml, triangleXml)
        idx <- idx + 1
    }
    
    xml <- paste(xml,"<edge sourceNodeId=\"",mean.idx,"\"  targetNodeId=\"",
                 lid,"\" doubleHeaded=\"false\" fixed=\"",fixed,"\" ",
                 pString," ",aString, " value=\"",value,"\" />\n", sep="")
    
    
  } else {
    
  
  if (isknown(right)) {
      rid <- known[[right]]
  } else {
    
    mg <- ""
    if (multigroup) {
      mg <- paste("groupValue=\"",grp,"\"",sep="")
    }
    
    xml <- paste(xml,"<Node caption=\"", right ,  "\" latent=\"",latentright,"\" id=\"",
                 (idx),"\" ",mg," />\n",sep="");
    known[[right]] <- idx
    rid <- idx
    idx <- idx + 1
  }
    

  
  if (op == "~~")
    doubleheaded <- "true" 
  else
    doubleheaded <- "false"
  

  
  
  


  xml <- paste(xml,"<edge sourceNodeId=\"",lid,"\"  targetNodeId=\"",
    rid,"\" doubleHeaded=\"",doubleheaded,"\" fixed=\"",fixed,"\" ",
    pString," ",aString, " value=\"",value,"\" />\n", sep="")
  
  }
} 
  
 
  
xml <- paste(xml, "\n</graph>\n</model>\n" ,sep="");
  
return(xml)

}
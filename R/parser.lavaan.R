parser.lavaan <- function(model, name="") {

lstr <- lavaanify(model)

xml <- paste( "<model name=\"",name,"\" specificationType=\"Onyx\" specificationVersion=\"1.0\">\n<graph>\n",sep="");

known <- list()

isknown <- function(key)
{
  return(key %in% names(known))
}

idx <- 1
for (i in 1:dim(lstr)[1]) {
  left <- lstr$lhs[i]
  op <- lstr$op[i]
  right <- lstr$rhs[i]
  free <- lstr$free[i]
  ustart <- lstr$ustart[i]
  latentleft <- TRUE
  latentright <- TRUE;
  if (op == "~=") { latentleft <- TRUE; latentright <- FALSE}
#  if (op == "~~")
  
  if (isknown(left)) {
    lid <- known[[left]]
  } else {
    xml <- paste(xml,"<Node caption=\"", left ,  "\" latent=\"",latentleft,"\" id=\"",
                 (i),"\" />\n",sep="");
    known[[left]] <- idx
    lid <- idx
    idx <- idx + 1
  }
  
  if (isknown(right)) {
      rid <- known[[right]]
  } else {
    xml <- paste(xml,"<Node caption=\"", right ,  "\" latent=\"",latentright,"\" id=\"",
                 (i),"\" />\n",sep="");
    known[[right]] <- idx
    rid <- idx
    idx <- idx + 1
  }
  
  if (op == "~~")
    doubleheaded <- TRUE 
  else
    doubleheaded <- FALSE
  
  if (free == 0)
    fixed <- TRUE
  else 
    fixed <- FALSE
  
  pString <- ""
  dString <- "" # no def. vars.
  
  value <- 0
  if (!is.na(ustart)) value <- ustart
  
  xml <- paste(xml,"<Edge sourceNodeId=\"",lid,"\"  targetNodeId=\"",
    rid,"\" doubleHeaded=\"",doubleheaded,"\" fixed=\"",fixed,"\" ",pString," ",dString, " value=\"",value,"\" />\n", sep="")
} 
  
 
  
xml <- paste(xml, "\n</graph>\n</model>\n" ,sep="");
  
return(xml)

}
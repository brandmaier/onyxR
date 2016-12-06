parser.lavaan <- function(model, title="") {

lstr <- lavaanify(model)

xml <- paste( "<model name=\"",title,"\" specificationType=\"Onyx\" specificationVersion=\"1.0\">\n<graph>\n",sep="");

known <- list()

isknown <- function(key)
{
  return(key %in% names(known))
}

idx <- 1
for (i in 1:dim(lstr)[1]) {
  left <- lstr[i,2]
  op <- lstr[i,3]
  right <- lstr[i,4]
  
  latentleft <- TRUE
  latentright <- TRUE;
  if (op == "~=") { latentleft <- TRUE; latentright <- FALSE}
#  if (op == "~~")
  
  if (!isknown(left)) {
    lid <- known[[latentleft]]
  } else {
    xml <- paste(xml,"<Node caption=\"", left ,  "\" latent=\"",latentleft,"\" id=\"",
                 (i),"\" />\n",sep="");
    known[[latentleft]] <- idx
    lid <- idx
    idx <- idx + 1
  }
  
  if (!isknown(right)) {
      rid <- known[[latentright]]
  } else {
    xml <- paste(xml,"<Node caption=\"", right ,  "\" latent=\"",latentright,"\" id=\"",
                 (i),"\" />\n",sep="");
    known[[latentright]] <- idx
    rid <- idx
    idx <- idx + 1
  }
  
  
  
  xml <- paste(xml,"<Edge sourceNodeId=\"",lid,"\"  targetNodeId=\"",
    rid,"\" doubleHeaded=\"",doubleheaded,"\" fixed=\"",fixed,"\" ",pString," ",dString, " value=\"",value,"\" />\n", sep="")
  
  
  return(xml);
}
  
xml <- paste(xml, "\n</graph>\n</model>\n" ,sep="");
  
return(xml)

}
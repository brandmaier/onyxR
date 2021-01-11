
isknown <- function(known, key, group = FALSE)
{
  if ((!group))
    return(key %in% names(known))
  else
    return (paste(key, "_", group, sep = "") %in% names(known))
}

setknown <- function(known, key, value, group = FALSE)
{
  if ((!group))
    known[[key]] <- value
  else
    known[[paste(key, "_", group, sep = "")]] <- value
  
  return(known)
}

getknown <- function(known, key, group = FALSE)
{
  if ((!group))
    return(known[[key]])
  else
    return(known[[paste(key, "_", group, sep = "")]])
}

parser.lavaan <-
  function(model,
           name = "",
           string.representations = FALSE) {
    
    if (string.representations) {
      lstr <- lavaan::lavaanify(model, auto.var = TRUE)
    } else {
      lstr <- lavaan::parameterTable(model)
    }
    xml <-
      paste(
        "<model name=\"",
        name,
        "\" specificationType=\"Onyx\" specificationVersion=\"1.0-500\">\n<graph>\n",
        sep = ""
      )
    
    
    num.groups <- length(unique(lstr$group))
    if (0 %in% unique(lstr$group)) {
      num.groups <- num.groups - 1
    }
    
    multigroup <- num.groups > 1
    
    # reorder parameter table by operator
    if (sum(lstr$op == "=~") > 0) {
      reorder.ids <- c(which(lstr$op == "=="), 
                       which(lstr$op == "=~"), 
                       which(lstr$op != "=~" && lstr$op != "==") )
      lstr <- lstr[reorder.ids,]
    }
    
    known <- list()
    
    mean.idx <- NULL
    
    fixed_parms <- list()

    
    idx <- 0
    for (i in 1:dim(lstr)[1]) {
      left <- lstr$lhs[i]
      op <- lstr$op[i]
      right <- lstr$rhs[i]
      free <- lstr$free[i]
      ustart <- lstr$ustart[i]
      latentleft <- FALSE
      latentright <- FALSE
      grp <- lstr$group[i]
      plabel <- lstr$plabel[i]
      
      if ("est" %in% names(lstr)) {
        est <- lstr$est[i]
      } else {
        est <- NULL
      }
      
      if (!is.null(fixed_parms[[plabel]])) {
        free <- 0
        ustart <- fixed_parms[[plabel]]        
      }
      
      if (op == "=~") {
        latentleft <- TRUE
        latentright <- FALSE
      }
      
      if (op == ":=")
        next
      
      if (op == "==") {
        suppressWarnings({
          numeric_right <- as.numeric(right)
        })
        if ((is.na(numeric_right)) || (right != as.character(numeric_right))) {
          next
        }
        # if this is a numeric equality constraint, save it for later
        fixed_parms[[left]] <- numeric_right
        
        next
      }
      
      meanpath <- FALSE
      if (op == "~1") {
        meanpath <- TRUE
      }
      
      if (isknown(known,
                  key = left,
                  group = ifelse(multigroup, grp, FALSE))) {
        lid <-
          getknown(known, left, ifelse(multigroup, grp, FALSE))#known[[left]]
        
      } else {
        mg <- ""
        if (multigroup) {
          mg <- paste("groupValue=\"", grp, "\"", sep = "")
        }
        
        xml <-
          paste(
            xml,
            "<node caption=\"",
            left ,
            "\" latent=\"",
            latentleft,
            "\" id=\"",
            (idx),
            "\" ",
            mg,
            " />\n",
            sep = ""
          )
        
        #known[[left]] <- idx
        known <-
          setknown(
            known,
            key = left,
            value = idx,
            group = ifelse(multigroup, grp, FALSE)
          )
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
        if (lstr$label[i] != "") {
          pname <- lstr$label[i]
        }
        pString <- paste("parameterName=\"", pname, "\"", sep = "")
      }
      
      aString <- "arrowHead=\"1\" "#definitionVariable=\"false\""
      
      value <- 1
      if (!is.na(ustart))
        value <- ustart
      
      # some postprocessing. Onyx does not like fixed path with zero values
      if ((op == "~~")) {
        if (value == 0 && free == 0) {
          #      value <- 1
          next
          # skip (co)variances fixed to zero
        }
      }
      
      if (!is.null(est)) {
        value <- est
      }
      
      
      if (meanpath) {
        if (is.null(mean.idx)) {
          mean.idx <- idx
          triangleXml <-
            paste("<Node caption=\"one\" id=\"",
                  idx,
                  "\" constant=\"true\"/>\n",
                  sep = "")
          
          xml <- paste(xml, triangleXml)
          idx <- idx + 1
        }
        
        xml <-
          paste(
            xml,
            "<edge sourceNodeId=\"",
            mean.idx,
            "\"  targetNodeId=\"",
            lid,
            "\" doubleHeaded=\"false\" fixed=\"",
            fixed,
            "\" ",
            pString,
            " ",
            aString,
            " value=\"",
            value,
            "\" />\n",
            sep = ""
          )
        
        
      } else {
        if (isknown(known,
                    key = right,
                    group = ifelse(multigroup, grp, FALSE))) {
          rid <-
            getknown(known,
                     key = right,
                     group = ifelse(multigroup, grp, FALSE)) #known[[right]]
        } else {
          mg <- ""
          if (multigroup) {
            mg <- paste("groupValue=\"", grp, "\"", sep = "")
          }
          
          xml <-
            paste(
              xml,
              "<Node caption=\"",
              right ,
              "\" latent=\"",
              latentright,
              "\" id=\"",
              (idx),
              "\" ",
              mg,
              " />\n",
              sep = ""
            )
          
          #    known[[right]] <- idx
          known <-
            setknown(
              known,
              key = right,
              value = idx,
              group = ifelse(multigroup, grp, FALSE)
            )
          rid <- idx
          idx <- idx + 1
        }
        
        
        
        if (op == "~~")
          doubleheaded <- "true"
        else
          doubleheaded <- "false"
        
        
        if (op == "~") {
          temp <- lid
          lid <- rid
          rid <- temp
        }
        
        
        
        
        xml <-
          paste(
            xml,
            "<edge sourceNodeId=\"",
            lid,
            "\"  targetNodeId=\"",
            rid,
            "\" doubleHeaded=\"",
            doubleheaded,
            "\" fixed=\"",
            fixed,
            "\" ",
            pString,
            " ",
            aString,
            " value=\"",
            value,
            "\" />\n",
            sep = ""
          )
        
      }
    }
    
    # collect all information on variables
    
    
    
    xml <- paste(xml, "\n</graph>\n</model>\n" , sep = "")
    
    
    return(xml)
    
  }
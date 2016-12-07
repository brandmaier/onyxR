# onyxR

Calling Onyx GUI from R when using OpenMx or lavaan packages for SEM.

## Install

To install the onyxR package directly from GitHub, copy the following line into R:
```{r, eval=FALSE}
source('https://raw.githubusercontent.com/brandmaier/onyxR/master/tools/install.R')
```

## Demo

In a simple demo, we define the HS model with three independent factors in lavaan and pass this to Onyx to display the path diagram.
````{r, eval=FALSE}
require(onyxR)
demo(lavaanHS)
```


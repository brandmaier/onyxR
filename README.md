# onyxR

Calling Onyx GUI from R when using OpenMx or lavaan packages for SEM.

## Install

To install the onyxR package directly from GitHub, copy the following line into R:
```{r, eval=FALSE}
source('https://raw.githubusercontent.com/brandmaier/onyxR/master/tools/install.R')
```

## Usage

Using onyxR is as simple as that. In lavaan

````{r, eval=FALSE}
model <-  'visual =~ x1 + 33*x2 + x3
textual =~ x4 + x5 + x6
speed =~ x7 + x8 + 2*x9

visual ~~ textual'

onyx(model)
```


## Demo

In a simple demo, we have wrapped the above example.  We define the HS model with three independent factors in lavaan and pass this to Onyx to display the path diagram.
````{r, eval=FALSE}
require(onyxR)
demo(lavaanHS)
```



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
HS.model <- ' visual  =~ x1 + x2 + x3 
              textual =~ x4 + x5 + x6
              speed   =~ x7 + x8 + x9 '

fit <- cfa(HS.model, data = HolzingerSwineford1939)

summary(fit, fit.measures = TRUE)

onyx(fit)
```

Alternatively, we can use onyxR to generate a path diagram for an OpenMx RAM-type model:

````{r, eval=FALSE}
data(demoOneFactor)
manifests <- names(demoOneFactor)
latents <- c("G")
factorModel <- mxModel("One Factor",
                       type="RAM",
                       manifestVars = manifests,
                       latentVars = latents,
                       mxPath(from=latents, to=manifests),
                       mxPath(from=manifests, arrows=2),
                       mxPath(from=latents, arrows=2,
                              free=FALSE, values=1.0),
                       mxData(cov(demoOneFactor), type="cov",
                              numObs=500))
fit <- mxRun(factorModel)

onyx(fit)
```


## Demo

In a simple demo, we have wrapped the above example.  We define the HS model with three independent factors in lavaan and pass this to Onyx to display the path diagram.
````{r, eval=FALSE}
require(onyxR)
demo(lavaanHS)
```



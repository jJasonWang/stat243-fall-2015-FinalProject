# stat243-fall-2015-FinalProject

* [Google Document](https://docs.google.com/document/d/10-o1opcEjyoi8Jxu8148s400xHnZr7uUb3ieG7SNWak/edit#heading=h.h5d6i1osekck)  
* [Latex Document](https://www.overleaf.com/3858778rmbdwj)
* [Codeshare](https://codeshare.io/n6mD3)

# Install the package ars

### Way1: devtools

Go to your directory which contain the folder `ars`. Then run the following code.

```r
setwd('to your directory contain ars...')
library(devtools)

#make the .tar.gz
build()

#install the package in your R library path
install()
```
After the above steps, you can run `library(ars)` and start to use the package.

### Way2: devtools + load_all


```r
setwd('to your directory contain ars...')
library(devtools)

#load and package
load_all()
```

Then, use `library(ars)` and start to use the package. Note that this method will not install `ars` into your library path, so whenever you want to use the package, you have to navigate to `ars` folder and run the above command.

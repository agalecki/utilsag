
TracedFunction1 <- function(x){
    funNm <- "TF1"       # Function label
   .traceRinit <- attr(options()$traceR, "init")
   .traceR <-   if (is.null(.traceRinit))
      function(...){} else .traceRinit(funNm) 
      
   .traceR(1,"TracedFunction1 STARTS here", funNm, msg = TRUE)
   .traceR(2, x, funNm)
   .traceR(3, c(1,x), funNm)
   .traceR(1,"TracedFunction1 ENDS here", funNm, msg = TRUE )
   x
}

TracedFunction2 <- function(x){
    funNm <- "TF2"       # Function label
   .traceRinit <- attr(options()$traceR, "init")
   .traceR <-   if (is.null(.traceRinit))
      function(...){} else .traceRinit(funNm) 

   .traceR(1, "TracedFunction2 STARTS here", funNm, msg = TRUE)
    mtx <- diag(x,2)
   .traceR(2, mtx, funNm)
   .traceR(3, list(x, mtx), funNm)
   .traceR(4, str(mtx), funNm, lbl="6a")
   .traceR(5, capture.output(str(mtx)), funNm, lbl="6b")  # Preferred
   
    envx <- new.env()
    assign("Mtx", 2*mtx, env = envx)
   .traceR(70, as.list(envx), funNm, "Environment envx as list")
    z <- NULL
   .traceR(80, z, funNm, lbl= "80a")
   .traceR(80, capture.output(z), funNm, lbl = "80b")
   .traceR(1, "TracedFunction2 ENDS here", funNm, msg = TRUE)
   x
}

library(utilsag)
## Ex1: By default: No tracing
options(traceR = NULL)                           #  Just in case reset traceR options to NULL
TracedFunction1(1)

## Ex2: Uniform trace for all functions
##      selected ids only

library(utilsag)
traceR <- list()
attr(traceR, "init") <-  utraceRinit     #  Always needed for traceR
attr(traceR, "default") <- utraceRprint  #  traceRprint, traceList
attr(traceR, "id") <- 1                  #  Numeric with selected ids
attr(traceR, "default")
options (traceR = traceR) 
TracedFunction1(1)
TracedFunction2(2)


## Ex3: Print trace for TracedFunction1. No trace for other functions.
traceR <- list("TF1" = utraceRprint)
attr(traceR, "init") <-  utraceRinit
options (traceR = traceR) 
## options()$traceR
TracedFunction1(31)
TracedFunction2(32)


## Ex4: traceRprint for TracedFunction1. 
##      traceRlist for TracedFunction2 for selected ids

traceR <- list(
    "TF1" = utraceRprint,   # for TracedFunction1
    "TF2" = utraceRlist     # for TracedFunction2
)


idL <- list("TF2" = 1:2)              # Selected ids for TF2

attr(traceR, "id")   <-  idL  
attr(traceR, "init") <-  utraceRinit    # Always needed
                   
options (traceR = traceR) 
TracedFunction1(41)

.traceRlist <- list()
TracedFunction2(42)
names(.traceRlist)






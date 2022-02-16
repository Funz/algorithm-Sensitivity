#title: Morris
#help: Morris's Elementary Effects Screening Method
#type: sensitivity
#authors: Gilles Pujol, with contributions from Frank Weber (2016)
#references: M. D. Morris, 1991, Factorial sampling plans for preliminary computational experiments, Technometrics, 33, 161-174.
#require: sensitivity; xtable; jsonlite
#options: r='10';levels='4';seed='1'
#options.help: r=integer giving the number of repetitions of the design; levels=integer specifying the number of levels of the design; seed=random seed

Morris <- function(options) {
    library(sensitivity)
    options$r <- as.integer(options$r)
    options$levels <- as.integer(options$levels)
    options$seed <- as.integer(options$seed)

    morris = new.env()
    lapply(names(options), function(x) assign(x, options[[x]], 
        morris))
    return(morris)
}

getInitialDesign <- function(algorithm, input, output) {
    algorithm$input <- input
    algorithm$output <- output
    d = length(input)
    set.seed(algorithm$seed)
    algorithm$m <- morris(model = NULL, factors = d, r = algorithm$r, 
        design = list(type = "oat", levels = algorithm$levels))
    names(algorithm$m$X) <- names(input)
    return(from01(algorithm$m$X,input))
}

getNextDesign <- function(algorithm, X, Y) {
    return()
}

displayResults <- function(algorithm, X, Y) {
    algorithm$Yi = Y[, 1]
    eval(expr = parse(text = "tell(m,Yi)"), envir = algorithm)
    
    algorithm$files = "plot.png"
    png(file = algorithm$files, bg = "transparent", height = 600, width = 600)
    try(plot(algorithm$m))
    dev.off()
    
    mu = colMeans(algorithm$m$ee)
    mu.star = colMeans(abs(algorithm$m$ee))
    sig = sqrt((colSums(algorithm$m$ee^2) - mu^2 * 4)/(algorithm$r - 1))
    effect = rep("Weak effect", length(mu))
    for (i in 1:length(mu)) if (mu.star[i] > 0.2 * max(mu.star)) 
        if (sig[i] > 0.2 * max(sig)) 
            effect[i] = "Non linear or interactive effect"
        else effect[i] = "Linear effect"
        
    html = paste0("<HTML name='sensitivity'>", paste0(collapse = "\n", 
        sub("<table border=1", replacement = "<table border='1'", capture.output(print(xtable::xtable(cbind(mu, 
            mu.star, sig, effect)), type = "html")))), "<br/><img src='", 
        algorithm$files, "'/></HTML>")
        
    return(paste0(html,"<mustar>",jsonlite::toJSON(mu.star),"</mustar>","<sig>",jsonlite::toJSON(sig),"</sig>"))
}

displayResultsTmp <- function(algorithm, X, Y) {}

from01 = function(X, inp) {
  nX = names(X)
  for (i in 1:ncol(X)) {
    namei = nX[i]
    X[,i] = X[,i] * (inp[[ namei ]]$max-inp[[ namei ]]$min) + inp[[ namei ]]$min
  }
  return(X)
}

to01 = function(X, inp) {
  nX = names(X)
  for (i in 1:ncol(X)) {
    namei = nX[i]
    X[,i] = (X[,i] - inp[[ namei ]]$min) / (inp[[ namei ]]$max-inp[[ namei ]]$min)
  }
  return(X)
}
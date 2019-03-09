#Quick function to save space by repeating the code to make a JAGS object for given variables
makeJagsData <- function(df, ivs, dv, addConstant = FALSE){
    x <- df[,ivs]
    if(addConstant == TRUE) x <- cbind(c = rep(1, nrow(df)), x)
    y <- df[,dv]
    jagsData <- list(y = y, x = x,
                     N = nrow(x),
                     J = ncol(x))
    return(jagsData)
}
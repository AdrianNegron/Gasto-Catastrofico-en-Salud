# Integration Out the random effect - Princeton University Code ================
gauher <- function(n, eps = 3e-14, maxit = 10) {
    # abscissas and weights for Gauss-Hermite quadrature
    # We use Gauss-Hermite code from Recipes in C p.154
    # and then change x to x*sqrt(2) and w to w/sqrt(pi)
    pim4 <- 0.751125544464942
    m <- floor((n + 1)/2)
    x <- w <- matrix(0, n, 1)
    for(i in 1:m) {
        if (i==1) {
            z <- sqrt(2 * n + 1) - 1.85575 * (2 * n + 1)^(-0.16667)
        }
        else if (i==2) {
            z <- z - (1.14 * n^0.426)/z	
        }
        else if (i==3) {
            z <- 1.86 * z - 0.86 * x[1]
        }
        else if (i==4) {
            z <- 1.91 * z - 0.91 * x[2]
        }
        else {
            z <- 2 * z - x[i-2]
        }
        its <- 0
        done <- FALSE
        while(!done & its < maxit) {
            p1 <- pim4
            p2 <- 0
            for(j in 1:n) {
                p3 <- p2
                p2 <- p1
                p1 <- z * sqrt(2/j) * p2 - sqrt((j-1)/j) * p3
            }
            pp <- sqrt(2 * n) * p2
            z1 <- z
            z <- z1 - p1/pp
            its <- its + 1
            done <- abs(z - z1) <= eps
        }
        if (its > maxit) {
            stop("gauher has not converged")
        }
        x[i] <- z
        x [n + 1 - i] <- - z
        w[i] <- 2/(pp * pp)
        w[n + 1 - i] <- w[i]
    }
    if (2 * m > n) {
        x[m] <- 0
    }
    data.frame(z = x * sqrt(2), w = w/sqrt(pi))	
}


# Calculando la correlación intra clase (clase latente y mediana) ==============
re_intra_class_corr <- function(model){
    require(lme4)
    # Intra class correlation in la escala latente
    v <- unlist(VarCorr(model))
    v <- v / (v + pi ^ 2 / 3)
    
    # Intra class correlation in median
    X <- model.matrix(model)
    # Multiplicación matricila en tre la matriz del modelo y los coef.
    xb <- X %*% fixef(model)
    md <- median(xb)
    
    ghi <- function(f, gq = gauher(12)) {
        sum(f(gq$z) * gq$w)
    }
    
    # Magin
    m1 <- ghi(function(z) plogis(md + sqrt(v) * z))
    
    # Joint
    m11 <- ghi(function(z) plogis(md + sqrt(v) * z) ^ 2)
    
    M <- matrix(c(m11, m1 - m11, m1 - m11, 1 - 2*m1 + m11), 2, 2)
    
    # Odd ratio
    or <- M[1,1] * M[2,2] / (M[1,2] * M[2,1])
    
    # Person's r
    r <- (m11 - m1 ^ 2) / (m1 = (1 - m1))
    
    # Yule's Q
    Q <- (or - 1) / (or + 1)
    
    
    re_it_corr <- data.frame("Medida" = c("Corr Intra Clase (latente) :",
                                         "Corr Intra Clase (en Mediana):",
                                         "Prob. Mariginal :",
                                         "Prob. Conjunta :",
                                         "Odds Ratio :",
                                         "Person's R :",
                                         "Yule's Q :"),
                             "Valor" = c(v, NA , m1, m11, or, r, Q))
    
    return(re_it_corr)
}

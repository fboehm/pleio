
load("~/Box Sync/attie/research-notebook/derived_data/probs.RData")
# Function for input to apply(, MARGIN = 3)
#ie, how do we treat a matrix of genotype probabilities from a single locus?
hk_residuals <- function(pheno, genomat){
  pheno - genomat %*% solve(t(genomat) %*% genomat) %*% t(genomat) %*% pheno
}



# function to simulate phenotypes from genotype data
sim_pheno <- function(genomat, beta = 1:8 / 8, sd = 0.05){
  pheno_length <- nrow(genomat)
  noise <- rnorm(n = pheno_length, mean = 0, sd = sd)
  pheno <- genomat %*% beta + noise
  return(pheno)
}

# simulate two phenotypes

y1 <- sim_pheno(genomat = probs$probs$`19`[, ,100])
y2 <- sim_pheno(genomat = probs$probs$`19`[, ,100])
yy <- cbind(y1, y2)


# multivariate regression; pleiotropy
nmar <- dim(probs$probs$`19`)[3]
res_pleio <- array(NA, c(nrow(yy), ncol(yy), nmar))
for (k in 1:nmar){
  res_pleio[, , k] <- hk_residuals(yy, probs$probs$`19`[, , k])
}

# in the below code, where we populate res_linkage,
# we get singular matrix errors. Why?
# One option: could two loci have identical (294 x 8) matrices?
#             what would happen if they did?
#
# we actually want to use duplicated() here!!
!duplicated(probs$probs$`19`, MARGIN = 3)


#res_linkage <- array(NA, c(nrow(yy), ncol(yy), nmar * (nmar - 1) / 2))
res_linkage <- list()
m <- 1
for (k in 2:200){
  for (l in 1:(k-1)){
    res_linkage[[m]] <- hk_residuals(c(y1, y2),
                                        rbind( probs$probs$`19`[, 1:8, l],
                                              probs$probs$`19`[, 1:8, k]))
    m <- m + 1
  }
}


# make a matrix of determinants of cross prods
deter_pleio <- matrix(nrow = nmar, ncol = nmar)
deter_linkage <- matrix(nrow = nmar, ncol = nmar)
for (i in 1:nmar){
  for (j in 1:i){
    deter_pleio[i, j] <- det(t(res_pleio[, , i]) %*% res_pleio[, , j])
    deter_linkage[i, j] <- det(t(res_linkage[, , i]) %*% res_linkage[, , j])
  }
}

range(deter_pleio, na.rm = TRUE)
range(deter_linkage, na.rm = TRUE)

###############
###############

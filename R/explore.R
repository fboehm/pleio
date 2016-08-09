
load("~/Box Sync/attie/research-notebook/derived_data/probs.RData")
genomat <- probs$probs$`19`

# Function for input to apply(, MARGIN = 3)
#ie, how do we treat a matrix of genotype probabilities from a single locus?
hk_residuals <- function(pheno, genomat){
  xmat <- cbind(1, genomat[, 2:8])
  lm1 <- lm(pheno ~ xmat)
  return(residuals(lm1))
}



# function to simulate phenotypes from genotype data
sim_pheno <- function(genomat, beta = 1:8 / 8, sd = 0.1){
  pheno_length <- nrow(genomat)
  noise <- rnorm(n = pheno_length, mean = 0, sd = sd)
  pheno <- genomat %*% beta + noise
  return(pheno)
}

# simulate two phenotypes

y1 <- sim_pheno(genomat = probs$probs$`19`[, ,100])
y2 <- sim_pheno(genomat = probs$probs$`19`[, ,100])

yy <- cbind(y1, y2)


res_mat <- apply(FUN = function(x)hk_residuals(pheno = yy, genomat = x), X = probs$probs$`19`, MARGIN = 3)

out <- list()
for (k in 1:3311){
  out[[k]] <- hk_residuals(yy, probs$probs$`19`[, , k])
}



# make a matrix of determinants of cross prods
M <- dim(probs$probs$`19`)[3]
deter_mat <- matrix(nrow = M, ncol = M)
for (i in 1:M){
  for (j in 1:i){
    deter_mat[i, j] <- det(t(out[[i]]) %*% out[[j]])
  }
}

range(deter_mat, na.rm = TRUE)

###############
###############

out2a <- list()
out2b <- list()
for (k in 1:3311){
  out2a[[k]] <- hk_residuals(y1, probs$probs$`19`[, , k])
  out2b[[k]] <- hk_residuals(y2, probs$probs$`19`[, , k])
}

deter_mat2 <- matrix(nrow = M, ncol = M)
out2 <- list()
for (i in 1:M){
  out2[[i]] <- cbind(c(out2a[[i]], rep(0, 294)), c(rep(0, 294), out2b[[i]]))
  for (j in 1:i){
    deter_mat2[i, j] <- det(t(out2[[i]]) %*% out2[[j]])
  }
}

reshape2::melt(deter_mat2)
deter_mat2 - deter_mat -> diff
range(diff, na.rm = TRUE)

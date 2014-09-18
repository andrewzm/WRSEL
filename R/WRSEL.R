#'@param X samples from the posterior, size \code{n} by \code{N}
#'@param weights the weight function, must be a vector of length 1 or length \code{n}
#'@param num_nodes the number of nodes to use on multi-core architecture (needs parallel package)
#'@details Requires the Matrix package. Returns a function with which we can apply any weight
WRSEL <- function(X, num_nodes = NULL) {
  
  stopifnot(is(X,"matrix") | is(X,"Matrix"))
  N <- ncol(X)
  n <- nrow(X)
  Xorder <- apply(X,2,rank)
  
  if (is.null(num_nodes)) {# single-threaded version
    P_lambda <- E_lambda <- matrix(0,n,n)
    for(i in 1:n) { # for each rank
      this_rank <- which(Xorder == i,arr.ind=T)
      mask <- sparseMatrix(i=this_rank[,1], j = this_rank[,2], x = 1,dims = c(n,N))
      X_sub <- sparseMatrix(i=this_rank[,1], j = this_rank[,2], x = X[this_rank],dims = c(n,N))
      
      E_lambda[i,] <- rowSums(X_sub)/rowSums(mask)
      P_lambda[i,] <- rowSums(mask)/N
    }
  } else {
    stopifnot(is.numeric(num_nodes))
    E_op <- function(i) {
      this_rank <- which(Xorder == i,arr.ind=T)
      mask <- sparseMatrix(i=this_rank[,1], j = this_rank[,2], x = 1,dims = c(n,N))
      X_sub <- sparseMatrix(i=this_rank[,1], j = this_rank[,2], x = X[this_rank],dims = c(n,N))
      return(rowSums(X_sub)/rowSums(mask))
    }
    
    P_op <- function(i) {
      this_rank <- which(Xorder == i,arr.ind=T)
      mask <- sparseMatrix(i=this_rank[,1], j = this_rank[,2], x = 1,dims = c(n,N))
      X_sub <- sparseMatrix(i=this_rank[,1], j = this_rank[,2], x = X[this_rank],dims = c(n,N))
      return(rowSums(mask)/N)
    }
    cl <- makeCluster(num_nodes, methods = FALSE)
    clusterEvalQ(cl = cl, library(Matrix))
    clusterExport(cl, c("n","Xorder","N","X"),envir = environment())
    E_lambda <- t(parSapply(cl,1:n,E_op))
    P_lambda <- t(parSapply(cl,1:n,P_op))
    stopCluster(cl)
  }
  E_lambda[is.nan(E_lambda)] <- 0
  
  function(weights){
    stopifnot(is.numeric(weights))
    weights_mat <- matrix(weights,n,n)
    colSums(E_lambda * P_lambda * weights_mat) / colSums(P_lambda * weights_mat) 
  }
}


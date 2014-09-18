#' @title Weighted Rank Squared Error loss
#' 
#' @description Given a set of samples of an n-dimensional random variable, \code{WRSEL} returns a function which, when supplied with a weight vector, returns
#' optimal estimates under a rank-weighted squared error loss, see references for details.
#' @param X samples from the posterior, size \code{n} by \code{N}
#' @param weights the weight function, must be a vector of length 1 or length \code{n}
#' @param num_nodes the number of nodes to use on multi-core architecture (needs parallel package)
#' @return function
#' @keywords weighted rank squared error loss, exceedances
#' @export
#' @references Wright, Deanne L., Hal S. Stern, and Noel Cressie. "Loss functions for estimation of extrema with an application to disease mapping." Canadian Journal of Statistics 31.3 (2003): 251-266.
#' @examples
#' ### Simulate a three variable system, where the three variables are Gaussian with sd = 1 and means c(-1,0,1)
#' n <- 3
#' N <- 100000
#' X <- matrix(rnorm(n*N),n,N)
#' X <- X + c(-1,0,1)
#' ### Create the WRSEL function which we can then supply with weight vectors to obtain the optimal estimates
#' WRSEL_fun <- WRSEL(X = X,num_nodes = NULL)
#' ### Create a weight vector
#' weights <- c(1,0,0)
#' ### Find and print the optimal estimates
#' lambda_hat <- WRSEL_fun(weights)
#' print(lambda_hat)
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


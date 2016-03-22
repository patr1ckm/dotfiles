################################################################################
################################################################################
################################################################################
##
##  title: boostMDMR.R
##
##  Authors:  Daniel McArtor (dmcartor@nd.edu) 
##            Patrick Miller (pmille13@nd.edu)
##
##  Date: 02/06/16
##
##  Purpose:  Function to conduct boosting on MDMR
##
##  Version history
##    v0.0 02/06/16. 
##      (DBM) First draft sent to Patrick after just playing around with it some
################################################################################
################################################################################
################################################################################


################################################################################
# *** Quick usage info: *** #
#
# --- INPUT ---
#
# X: matrix of main effects (all variables treated as at least ordinal)
#
# D: distance matrix or dist() object
# lambda: shrinkage rage
#
# ntrees: total number of trees
#
# nsplit: number of splits that we'll consider for each predictor variable
#
# verbose.ind: after the (verbose.ind)^th tree has been fitted, an update will 
#              print to the console just to give you an idea of how long it's
#              going to take, for testing purposes
#
# --- OUTPUT ---
# just the residual G matrix and pseudo R-square accounted for by the full model
################################################################################



mdmr.boost <- function(X, D, lambda = 0.01, ntrees = 100, nsplit = 5,
                       verbose.ind = 25){
  p <- ncol(X)
  ncores <- min(p, 7)
  
  ##############################################################################
  ## Convert distance matrix to gower's centered matrix
  ##############################################################################
  # Convert distance object to matrix form
  D <- as.matrix(D)
  
  # Dimensionality of distance matrix
  n <- nrow(D)
  
  # Create Gower's symmetry matrix (Gower, 1966)
  A <- -0.5 * D^2
  
  # Subtract column means As = (I - 1/n * 11')A
  As <- A - rep(colMeans(A), rep.int(n, n))
  
  # Subtract row means G = As(I- 1/n * 11')
  # the transpose is just to deal with how "matrix minus vector" operations work
  # in R
  G <- G.resid <- t(As) - rep(rowMeans(As), rep.int(n, n))
  
  ##############################################################################
  ## Inverse loss function: pseudo R-square
  ##    input: design matrix, vectorized G matrix, trace of G
  ##############################################################################
  pr2 <- function(x, vg, trG){
    # Initialize (X'X)^-1 as NULL and pr2=0 becuase if X'X is singular, we don't 
    # want to select this tree because it's unusable
    xx.inv <- NULL
    pr2 <- 0
    
    # Try to compute (X'X)^-1
    try(xx.inv <- solve(crossprod(x)), silent = T)
    
    # Compute pr2 if possible
    if(!is.null(xx.inv)){
      h <- tcrossprod(tcrossprod(x, xx.inv), x)
      vh <- matrix(h, ncol = 1)
      pr2 <- as.numeric(crossprod(vh, vg) / trG)
    }
    
    return(pr2)
  }
  
  ##############################################################################
  ## Create split-points
  ##############################################################################
  # Note: this needs to get recoded as a list to be more efficient so that we
  # can have different numbers of splits for each variable, especially in the
  # case of combining categorical and continuous predictors
  
  splits <- 
    apply(X, 2, quantile, seq(0, 1, length = nsplit + 2))[-c(1, nsplit + 2),]
  # Check to see if any splits are the maximum level of the variable - that will
  # not work (minimum will because I use <= and >)
  for(j in 1:ncol(splits)){
    splits[which(splits[,j] == max(X[,j])),j] <- 
      max(X[-which(X[,j] == max(X[,j])),j])
  }
  
  
  
  ##############################################################################
  ## FOR I IN 1:NTREES - BOOSTING ALGORITHM
  ##############################################################################
  
  for(b in 1:ntrees){
    # ==========================================================================
    # Initialize G objects for this iteration
    # ==========================================================================
    vg <- matrix(G.resid, ncol = 1) 
    trG <- sum(diag(G.resid))
    
    # ==========================================================================
    ## Find optimal splitting variable and splitting point for first level
    # ==========================================================================
    
    # --- Iterate over variables --- #
    pr2.level1 <- parallel::mclapply(1:p, mc.cores = ncores, FUN = function(j){
      
      # Get continuous single predictor that we'll split in the next step, and
      # vectorize it to optimize speed
      X.hold <- X[,j, drop = T]
      
      # --- Iterate over splits --- #
      split.res <- lapply(1:nsplit, FUN = function(i){
        
        # Create a working variable
        X.dicho <- X.hold
        
        # Apply the splitting rule
        X.dicho[X[,j] <= splits[i,j]] <- 0
        X.dicho[X[,j] > splits[i,j]] <- 1
        
        # Scale the predictor and add an intercept term
        X.dicho <- cbind(1, scale(X.dicho))
        
        # Compute the pseudo R-square for this predictor-split combination
        return(pr2(X.dicho, vg, trG))
        
      })
      
      # Clean up split-wise output
      split.res <- unlist(split.res)
      
      return(split.res)
    })
    
    # Clean up result: rows = splits, columns = variables
    pr2.level1 <- matrix(unlist(pr2.level1), ncol = p, nrow = nsplit, byrow = F)
    
    # Choose best variable/split combination
    best.level1 <- which(pr2.level1 == max(pr2.level1), arr.ind = T)
    best.var <- best.level1[2]
    best.split <- best.level1[1]
    
    # --- Create foundation of tree's design matrix based on this result --- #
    X.stump <- X[,best.var]
    X.stump[X[,best.var] <= splits[best.split, best.var]] <- 0
    X.stump[X[,best.var] > splits[best.split, best.var]] <- 1
    rm(best.var, best.split)
    
    # ==========================================================================
    # Find optimal splitting variables and points for second level.
    #     This wil require a four-level loop because the left and right nodes
    #     need to be simultaneously optimized for both variable choice and split
    #     point
    # ==========================================================================
    
    # --- 1. Iterate over variables that can comprise the left node --- #
    res1 <- parallel::mclapply(1:p, mc.cores = ncores, FUN = function(j1){
      
      # Get continuous single predictor that we'll split later, and
      # vectorize it to optimize speed
      X1 <- X[,j1]
      
      # --- 2. Iterate over variables that can comprise the right node --- #
      res2 <- lapply(1:p, FUN = function(j2){
        
        # Get continuous single predictor that we'll split later, and
        # vectorize it to optimize speed
        X2 <- X[,j2]
        
        # --- 3. Iterate over splits on the first node --- #
        res3 <- sapply(1:nsplit, FUN = function(i1){
          
          # Create a working variable
          X.node1 <- X1
          
          # Apply the splitting rule
          X.node1[X[,j1] <= splits[i1,j1]] <- 0
          X.node1[X[,j1] > splits[i1,j1]] <- 1
          
          # --- 4. Iterate over split points on the second node --- #
          res4 <- sapply(1:nsplit, FUN = function(i2){
            
            # Create a working variable
            X.node2 <- X2
            
            # Apply the splitting rule
            X.node2[X[,j2] <= splits[i2,j2]] <- 0
            X.node2[X[,j2] > splits[i2,j2]] <- 1
            
            # ==================================================================
            # Compute pseudo R-square with this combination of variables and
            # splits
            # ==================================================================
            
            # Bind together the three predictors (level 1 and two level-2)
            X.tree <- cbind(1, X.stump, X.node1, X.node2, NA, NA)
            
            # Center prior to appending interaction terms
            X.tree[,2:4] <- scale(X.tree[,2:4], scale = F, center = T)
            
            # Append inteactions of each child node with the parent node
            X.tree[,5] <- X.tree[,2] * X.tree[,3]
            X.tree[,6] <- X.tree[,2] * X.tree[,4]
            
            # Compute pseudo R-square for this tree
            pr2(X.tree, vg = vg, trG = trG)
          })
        })
        
        # Name the entries of res3 to track what generated each 
        # pseudo R-square
        dimnames(res3) <- list(split.node2 = 1:nsplit, split.node1 = 1:nsplit)
        
        # Return the results from all possible splits with this combination of
        # child node variables
        return(res3)
      })
      
      # Convert the list to an array
      res2 <- simplify2array(res2)
      dimnames(res2) <- list(split.node2 = 1:nsplit, split.node1 = 1:nsplit,
                             x.node2 = 1:p)
      return(res2)
    })
    # Convert the list to an array
    res1 <- simplify2array(res1)
    dimnames(res1) <- list(split.node2 = 1:nsplit, split.node1 = 1:nsplit,
                           x.node2 = 1:p, x.node1 = 1:p)
    
    # -----------------------------------------------------
    # Find the optimal tree and create its design matrix
    # -----------------------------------------------------
    best.inds <- as.matrix(which(res1 == max(res1), arr.ind = T))[1,]
    j1 <- best.inds[4]
    j2 <- best.inds[3]
    i1 <- best.inds[2]
    i2 <- best.inds[1]
    
    # Create splitting variables
    X.node1 <- X[,j1]
    X.node1[X[,j1] <= splits[i1, j1]] <- 0
    X.node1[X[,j1] > splits[i1, j1]] <- 1
    
    X.node2 <- X[,j2]
    X.node2[X[,j2] <= splits[i2, j2]] <- 0
    X.node2[X[,j2] > splits[i2, j2]] <- 1
    
    # Bind together the three predictors (level 1 and two level-2)
    X.tree <- cbind(1, X.stump, X.node1, X.node2, NA, NA)
    
    # Center prior to appending interaction terms
    X.tree[,2:4] <- scale(X.tree[,2:4], scale = F, center = T)
    
    # Append inteactions of each child node with the parent node
    X.tree[,5] <- X.tree[,2] * X.tree[,3]
    X.tree[,6] <- X.tree[,2] * X.tree[,4]
    
    # Clean up
    rm(X.node1, X.node2, X.stump, res1)
    
    # ==========================================================================
    # Get fitted G, shrink it, and use it to update G.resid
    # ==========================================================================
    H <- tcrossprod(tcrossprod(X.tree, solve(crossprod(X.tree))), X.tree)
    HG <- H %*% G.resid
    G.resid <- G.resid - lambda * HG
    rm(H, HG)
    
    # Update estimate of pseudo r-square
    pr2.res <- 1 - sum(diag(G.resid)) / sum(diag(G))
    
    # Track progress cause this is really slow right now
    if(b %% verbose.ind == 0){
      cat('rep', b, '   pr2 =', pr2.res, fill = T)
    }
  }
  
  return(list(G.resid = G.resid, pr2 = pr2.res))
  
}

### logistic regression ###

initialize_sparse_weight <- function(D) {
  w = runif(D+1, min=-1, max=1)            # runif = random uniform(균일분포)
  return(w)
}

# D+1개 만큼 균일분포 값을 생성하는데 최소값은 -1, 최대값은 1
initialize_sparse_weight(5)

## 로지스틱 회귀식
logistic_regression <- function(X, W){
  X_bias <- cbind(rep(1, nrow(X)), X)      # 절편 (b) 추가
  z <- X_bias%*%W
  p <- sigmoid(z)
  return(p)
}

## 시그모이드 함수
sigmoid <- function(z){
  p <- 1/(1+exp(-z))
  return(p)
}

##
gradient_descent <- function(W, X, Y, D, eps=0.0001, max_iter=100, lr=0.01, k=0){
  start <- Sys.time()
  
  loss = 0
  rep_iter = 0
  X_bias <- cbind(rep(1, nrow(X)), X)
  
  while(rep_iter < max_iter){
    rep_iter <- rep_iter + 1
    print(paste("iteration: ", rep_iter))
    
    # predict
    P = logistic_regression(X, W)     # predict 참고
    
    # Calculate cost
    loss_new <- loss_function(P, Y)   # calculate cost ckarh
    
    if(abs(loss_new - loss) < eps){
      break
    }
    
    print(paste("loss: ", loss_new, "   eps: ", (loss - loss_new)))
    loss <- loss_new
    
    # decay learning rate
    lr <- lr*(1/(1+k*rep_iter))
    
    # Update Weights
    W <- W-lr*t(X_bias) %*% (P-Y)
  }
  
  print(paste("Time taken: ", as.numeric((Sys.time() - start))))
  return(W)
}


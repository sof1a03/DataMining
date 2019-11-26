
tree.grow <- function(x,y,nmin, minleaf,nfeat){
  tree <- data.frame (1, length(y[y==0]), sum(y[y==1]),list(1),-1,-1,-1,-1,1)
  names(tree) <- c("node.id", "class.0", "class.1", "observations", "left.id", "right.id", "split.attribute", "split.value", "is.splittable")
  tree$observations <- list(c(1:length(y)))
  x<- cbind(x, c(1:nrow(x)))
  while (sum(tree$is.splittable) > 0){
    splittable.rows <- which(tree$is.splittable == 1)
    for(row in splittable.rows){
        i <- tree[row,]
        if (length(i$observations[[1]]) < nmin){
          tree[tree$node.id == i$node.id,]$is.splittable <- 0
        }
        else{
          x.filter = x[as.vector(unlist(i$observations)),]
          y.filter = y[as.vector(unlist(i$observations))]
          attributes.numbers <- sample(1:(ncol(x.filter)-1), nfeat, replace=FALSE)
          splitpoints <- sapply(attributes.numbers, function(i){bestsplit(x.filter[,i],y.filter, minleaf)})
          splitpoints <- t(splitpoints)
          splitpoints <- cbind(splitpoints,attributes.numbers)
          rows = complete.cases(splitpoints)
          splitpoints <- splitpoints[rows, ]
          if (length(splitpoints) == 0){
            tree[tree$node.id == i$node.id,]$is.splittable <- 0
          }
          else{
            if(is.vector(splitpoints)){
              split.value <- splitpoints[ 1]
              split.attribute <- splitpoints[ 3]
            }
            else{
              splitpoints <- splitpoints[order(splitpoints[,2], decreasing = TRUE), ]
              split.value <- splitpoints[1 , 1]
              split.attribute <- splitpoints[1, 3]
            }
            
            left.obs <- x.filter[x.filter[,split.attribute] > split.value, ncol(x.filter)]
            right.obs <- x.filter[x.filter[,split.attribute] <= split.value, ncol(x.filter)]
        
            left.obs.1 <- sum(y[left.obs])
            left.obs.0 <- length(left.obs) - left.obs.1
            right.obs.1 <- sum(y[right.obs])
            right.obs.0 <- length(right.obs) - right.obs.1
            
            left.id <- max(tree$node.id) +1
            right.id <- left.id +1
            
            left.child <- data.frame (left.id,left.obs.0,left.obs.1,-1,-1,-1,-1,-1,1)
            names(left.child) <- c("node.id", "class.0", "class.1", "observations", "left.id", "right.id", "split.attribute", "split.value", "is.splittable")
            left.child$observations <- list(left.obs)
            
            right.child <- data.frame (right.id,right.obs.0,right.obs.1,-1,-1,-1,-1,-1,1)
            names(right.child) <- c("node.id", "class.0", "class.1", "observations", "left.id", "right.id", "split.attribute", "split.value", "is.splittable")
            right.child$observations <- list(right.obs)
            
            tree[tree$node.id == i$node.id,]$left.id <- left.id
            tree[tree$node.id == i$node.id,]$right.id <- right.id
            tree[tree$node.id == i$node.id,]$is.splittable <- 0
            tree[tree$node.id == i$node.id,]$split.attribute <- split.attribute
            tree[tree$node.id == i$node.id,]$split.value <- split.value
            tree <- rbind(tree, left.child)
            tree <- rbind(tree, right.child)
          }
        }
    }
  }
  return (tree)
}


tree.classify <- function(x, tr){
  result <- apply(x,1, function(i){
    current.node <- tr[1, ]
    while(current.node$split.attribute != -1){
      if (i[current.node$split.attribute] > current.node$split.value){
        current.node <- tr[current.node$left.id, ]
      }
      else{
        current.node <- tr[current.node$right.id, ]
      }
    }
    if (current.node$class.0 > current.node$class.1){
      return (0) 
    } 
    else return (1)
  })

}
tree.grow.bag <- function(x,y,nmin, minleaf,nfeat,m){
  list.trees <- list()
  list.trees <- lapply(c(1:m), function(i){
    bootstrap.samples <- sample(nrow(x), size=nrow(x), replace = TRUE)
    x.bootstrap = x[bootstrap.samples,]
    y.bootstrap = y[bootstrap.samples]
    tree <- tree.grow(x.bootstrap,y.bootstrap,nmin,minleaf,nfeat)
    return (tree)
  })
  return (list.trees)
}


tree.classify.bag <- function(x, list.trees){
  classification_matrix <- sapply(list.trees, function(tree){
    labels <- tree.classify(x, tree)
    return (labels)
  })
  classification_matrix <- apply(classification_matrix, 1, function(row){
    if (length(row[row == 0]) >= length(row[row == 1])){
      return (0)
    }
      else return (1)
    })
  return (classification_matrix)
}

impurity <- function(y){
  if(length(y)== 0){
    return (0)
  }
  x <- sum(y)/ length(y)
  result <- x*(1-x)
}

bestsplit <- function(x,y,minleaf){
  my_matrix <- cbind(x,y)[order(x,y),]
  splitpoints <- sapply(c(2:nrow(my_matrix)-1), function(i){
    if(my_matrix[i,1] != my_matrix[i+1,1]){
      return ((my_matrix[i,1]+ my_matrix[i+1,1])/2)
    }
    else {
      return (NA)
    }
  })
  
  splitpoints <- splitpoints[!is.na(splitpoints)]
  splitpoints <- sapply(splitpoints, function(i){
    if (length(x[x<=i]) < minleaf || length(x[x > i])< minleaf){
      return (NA)
    }
    else {
      return (i)
    }
  })
  
  splitpoints <- splitpoints[!is.na(splitpoints)]
  if (length(splitpoints)==0){
    empty.matrix <- matrix(nrow=1, ncol=2, byrow = FALSE, dimnames = list(c(),c("splitpoints", "quality")))
    return (empty.matrix)
  }
  
  impurities <- sapply(splitpoints, function(i) {
    less_mat <- my_matrix[which(my_matrix[,1] <=i), 2]
    great_mat <- my_matrix[which(my_matrix[,1] > i), 2]
    length(less_mat)*impurity(less_mat)/length(y) + length(great_mat)*impurity(great_mat)/length(y)
  })
  
  quality <- impurity(y) - impurities
  quality_matrix <- cbind(splitpoints, quality)
  quality_matrix <- quality_matrix[which.max(quality_matrix[,2]), ]
  if (quality_matrix[2] > 0){
    return (quality_matrix)
  }
  else {
    empty.matrix <- matrix(nrow=1, ncol=2, byrow = FALSE, dimnames = list(c(),c("splitpoints", "quality")))
    return (empty.matrix)
  }
}








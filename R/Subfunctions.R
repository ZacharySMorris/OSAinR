### Simple subfunctions ###

## Make adjacent pairs
adjacent.pairs <- function(x){
  X <- length(x)
  adj.pairs <- cbind(seq(X-1), seq(2,X))
  out <- adj.pairs
}
##

## Make row pairs
row.pairs <- function(x){
  X <- length(x)
  row.pairs <- cbind(seq(1,X,2), seq(2,X,2))
  out <- row.pairs
}
##

## Resolve and update the matrix and edge list
resolve.MxMn <- function(x,y,primary,secondary){
  # x is a matrix of event scores with new MxMn hypothetical nodes
  # y is a matrix of event scores for the dataset prior to comparison
  x <- MxMn.matrix
  y <- tmp_matrix

  updated_P
  updated_S

  n_pairs <- nrow(x)
  dup_pairs <- duplicated(Combined_MxMn_Real_Matrix) #this creates a logical vector for whether the min_pair has already been included
  dup_index <- grep("TRUE",dup_pairs)

  orig_match <- list()
  orig_match <- matrix(nrow=n_pairs,ncol=2,dimnames = list(seq(n_pairs),c("MxMn","Match")))
  for (i in seq(n_pairs)){
    tmp_MxMn <- x[i,]
    tmp_MxMn_name <- rownames(x)[i]
    tmp_match <- apply(y,1,function(x) all(tmp_MxMn == x)) #creates a logical vector for which earlier node matches the current new Mx of Mn node
    if (any(tmp_match)){ #if there are any matches
      tmp_names <- rownames(y)[tmp_match] #extract the name of the original node
      orig_match[i,] <- c(tmp_MxMn_name,tmp_names)
    }else{
      orig_match[[i]] <- c(updated_P,updated_S)[pair_order][i]
      # orig_match[[i]] <- rownames(MxMn.matrix)[i]
    }

  }

  ### This works perfectly to identify which existing nodes match the new hypothesized nodes
  ### Next check whether the distance between points being compared is >1 step
      ### If yes, then place hypothetical nodes and edges to the points being compared rather than the points themselves
      ### If no, then place edges between points being compared


  ### Need to rewrite the function which adds edges
  ### Currently it adds edges based on the nodes compared
  ### Should instead add edges to the new hypothetical nodes and each of the nodes compared
  ### Unless the hypothetical node is equal to an existing node
      ### If so, then check for whether an edge has already been placed between the matching node and the comparison node
        ### If yes, do nothing
        ### If no, add edge between original nodes



  c(primary,secondary)[pair_order]

  ##make a list of all comparisons
  tmp_comp <- rbind(cbind(primary,Mx_list),  cbind(primary,Mn_list),cbind(secondary,Mx_list),cbind(secondary,Mn_list))

  orig_match[,2]
  tmp_comp[grep(orig_match[1,1],tmp_comp)] <- orig_match[1,2]

  orig_match[tmp_comp %in% orig_match[1,1]]
  orig_match[1,1] %in% tmp_comp

  rownames(MxMn.matrix)

  orig_match[ROW_PAIRS]
  updated_edges <- unlist(orig_match)
  updated_edges <- primary
  updated_S <- secondary
  c(updated_P,updated_S)[pair_order] <- match_names

  updated_P[primary %in% match_names]
  match_names[match_names %in% primary]

  updated_P[match_names %in% primary] <- match_names[primary %in% match_names]
  updated_S[secondary == orig_match[dup_index]] <- match_names[orig_match[dup_index] == secondary]



  new_edges <-

  new_edges <- c(primary,secondary)[pair_order]

out <- list(updated_matrix = updated_matrix, updated_edges = updated_edges)
}



apply(Combined_MxMn_Real_Matrix[dup_index,],1,function(x) x == tmp_matrix)

rownames(tmp_matrix)[apply(tmp_matrix,1,function(x) all(Combined_MxMn_Real_Matrix[dup_index,][1,] == x))]


updated_matrix <- Combined_MxMn_Real_Matrix[!dup_pairs,]


sapply(Combined_MxMn_Real_Matrix[dup_pairs,])


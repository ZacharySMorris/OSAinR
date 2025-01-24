###Logic of algorithm###

#Take set of semaphoronts
Color_Ontogeny.df$Matrix
Color_Ontogeny.df$Semaphoronts

#Calculate Maturity Score#
Color_Ontogeny.df$MaturityScore <- apply(Color_Ontogeny.df$Matrix, 1, sum)

##Create semaphoront weights list (initially all set to 1)
make_weights <- function(X=OSA_DataFrame){
  temp_weights <- rep(1,length(X$Semaphoronts))
  names(temp_weights) <- X$Semaphoronts

out <- temp_weights
}

Color_Ontogeny.df$weights <- make_weights(Color_Ontogeny.df)
##

##Create list to keep track of already completed comparisons
Color_Ontogeny.df$ComparisonsList <- NULL
##

##Create vector of node pairs for each edge in the network or the sequences reconstructed by OSA
Color_Ontogeny.df$RecSeqlist <- NULL
##

##Calculate pairwise differences among semaphoronts
test_obj <- OSA_PairComp(Color_Ontogeny.df)

#Construct network based on raw distance matrix

#Find delminoronts (closest pair of semaphoronts)

next_nearest <- function(X=OSA_DataFrame, Y=OSA_DistanceMatrix){

  for (i in seq_along(unique(Y$Distance))){
    min_dist <- sort(unique(Y$Distance))[[i]] #calculate minimum pair distance
    max_dist <- max(unique(Y$Distance)) #calculate maximum pair distance
    min_pairs <- Y$Distance.Matrix[grep(min_dist, Y$Distance.Matrix[,3]),] #subset to only delminoronts

    #check if first comparison or not
    if(is.null(X$ComparisonsList)){
      updated_pairs <- min_pairs
      break
    }
    else {
      #remove comparisons among only 0 weight semaphoronts (i.e., inferred hypothetical intermediates)
      comp_weight<-c()
      for (i in seq(nrow(min_pairs))){
        w1 <- X$weights[[match(min_pairs[i,"From"],names(X$weights))]]
        w2 <- X$weights[[match(min_pairs[i,"To"],names(X$weights))]]
        comp_weight <- append(comp_weight, w1 + w2)
      }
      good_pairs <- min_pairs[comp_weight>0,]

      #check for repeated comparisons
      dup_pairs <- duplicated.data.frame(rbind(X$ComparisonsList,good_pairs))[-c(1:nrow(X$ComparisonsList))] #this creates a logical vector for whether the min_pair has already been included

      #if all min_pairs match, then move to next min_dist
      if (all(dup_pairs)){
        if(min_dist == max_dist){
          updated_pairs <- c("END")
        }else{
          next
        }
      }else{
        updated_pairs <- good_pairs[!dup_pairs,] #subsamples the min_pairs to only those that haven't been done yet
        break
        }
      }
    }
    #output
    output = list(min_dist = min_dist, minimum_pairs = updated_pairs)
}

nn_test4 <- next_nearest(OSA_list[[27]],obj_list[[28]])
obj_list[[28]]$Distance.Matrix[grep(8, obj_list[[28]]$Distance.Matrix[,3]),]
OSA_list[[27]]$ComparisonsList
min_pairs <- Y$Distance.Matrix[grep(min_dist, Y$Distance.Matrix[,3]),]

  sort(unique(test_obj$Distance))[match(min_dist,sort(unique(test_obj$Distance))) + 1]


for (i in seq_along(unique(test3_obj$Distance))){

  test2_obj$Distance.Matrix %in% test_obj$Distance.Matrix[1,]
  sr_test$ComparisonsList

  sapply()

  tmp_OSA <- Color_Ontogeny.df
  tmp_OSA <- OSA_list[[4]]

  OSA_list[[33]]$RecSeqlist

obj_list <- list()
nn_list <- list()
OSA_list <- list()

success <- FALSE
i=1
while (!success){
    tmp_obj <- OSA_PairComp(tmp_OSA)
    tmp_nn <- next_nearest(tmp_OSA,tmp_obj)
    tmp_out <- resolve.comp.OSA(X=tmp_OSA,Y=tmp_nn,Z=tmp_obj)
    tmp_OSA <- tmp_out
    # if(!is.null(tmp_out$RecSeqlist)){
    #   tmp_match <- match(unique(tmp_out$RecSeqlist),tmp_out$Semaphoronts)
    #   tmp_out$colors <- tmp_out$colors[tmp_match]
    #   tmp_out$size <- tmp_out$size[tmp_match]
    # }

    suppressWarnings(if(tmp_nn$minimum_pairs == "END"){
      success <- TRUE
    }else{
      obj_list[[i]] <- tmp_obj
      nn_list[[i]] <- tmp_nn
      OSA_list[[i]] <- tmp_out
      i=i+1
    })
  }

next_nearest(tmp_OSA,tmp_obj)

min_pairs <- tmp_obj$Distance.Matrix[grep(min(unique(tmp_obj$Distance)), tmp_obj$Distance.Matrix[,3]),] #subset to only delminoronts


comp_weight<-c()
i=2
for (i in seq(nrow(min_pairs))){
  w1 <- tmp_OSA$weights[[match(min_pairs[i,"From"],names(tmp_OSA$weights))]]
  w2 <- tmp_OSA$weights[[match(min_pairs[i,"To"],names(tmp_OSA$weights))]]
  comp_weight <- append(comp_weight, w1 + w2)
}
nn_list[[5]]$minimum_pairs[comp_weight>0,]

grep(paste(nn_list[[5]]$minimum_pairs[,"From"],collapse = "|"),names(OSA_list[[4]]$weights))

paste(names(OSA_list[[4]]$weights),collapse = "|")

grep(paste(names(OSA_list[[4]]$weights),collapse = "|"), nn_list[[5]]$minimum_pairs[,1])

names(OSA_list[[4]]$weights)grep(paste(nn_list[[5]]$minimum_pairs[,1],collapse = "|"), names(OSA_list[[4]]$weights))

names(OSA_list[[4]]$weights) %in% nn_list[[5]]$minimum_pairs[,1]

findMatches(names(OSA_list[[4]]$weights)[3], nn_list[[5]]$minimum_pairs, select=c("all"))

z <- apply(x, 1, function(a) apply(y, 1, function(b) all(a==b)))

apply(nn_list[[5]]$minimum_pairs, MARGIN = 2, FUN = function(x) match(names(OSA_list[[4]]$weights), x))

match(data.frame(t(names(OSA_list[[4]]$weights))), data.frame(nn_list[[5]]$minimum_pairs))



resolve.comp.OSA <- function(X=OSA_DataFrame,Y=minimum_pairs_list,Z=OSA_PairComp){
  temp_diff <- Y$min_dist

  if (temp_diff == 0){
  out <- synonymy.update.OSA(X=X,Y=Y$minimum_pairs)
  }
  else{
  out <- sequence.reconstruction.OSA(X=X,Y=Y$minimum_pairs,Z=Z)
  }
}

###if diff = 0, then

  synonymy.update.OSA <- function(X=OSA_DataFrame, Y=minimum_pairs_list){
    #load in dataframe values
    tmp_weights <- X$weights
    tmp_matrix <- X$Matrix
    tmp_colors <- X$colors
    tmp_size <- X$size
    tmp_comparisons <- X$ComparisonsList
    tmp_maturity <- X$MaturityScore
    tmp_edges <- c(X$RecSeqlist)

    #Update semaphoront weighting
    primary <- Y[[1]]
    clone <- Y[[2]]

    new_name <- paste(primary,clone,sep = "_&_")
    weight_sum <- sum(tmp_weights[grep(paste(c(primary,clone),collapse = "|"), names(tmp_weights))])

    updated_weights <- tmp_weights[-grep(clone, names(tmp_weights))]
    names(updated_weights)[grep(primary, names(updated_weights))] <- new_name
    updated_weights[grep(primary, names(updated_weights))] <- weight_sum

  #Update semaphoront matrix & network (by synonymizing semaphoronts)
    updated_names <- names(updated_weights)

    rownames(tmp_matrix)[grep(primary, names(tmp_weights))] <- new_name
    updated_matrix <- tmp_matrix[-grep(clone, names(tmp_weights)),]

    updated_colors <- tmp_colors[-grep(clone, names(tmp_weights))]

    tmp_size[grep(primary, names(updated_weights))] <- weight_sum*tmp_size[grep(primary, names(updated_tmp))]
    updated_size <- tmp_size[-grep(clone, names(tmp_weights))]

    updated_maturity <- tmp_maturity[-grep(clone, names(tmp_weights))]
    names(updated_maturity)[grep(primary, names(updated_maturity))] <- new_name

  #Update comparison matrix (which removes comparisons already done)
    updated_comparisons <- rbind(tmp_comparisons,Y)

    #output
    out <- list(Matrix = updated_matrix, Semaphoronts = updated_names, Events = X$Events,
                colors = updated_colors, size = updated_size, weights = updated_weights,
                ComparisonsList = updated_comparisons, RecSeqlist = tmp_edges,
                MaturityScore = updated_maturity)

  #start next loop
  }

###if diff > 0, then

  ##Should consider giving a numerical list of hypothetical nodes instead of the long list of comparisons
  ##Perhaps a new variable could contain the list of hypothetical nodes and lists the comparison that generated it?
  ###  c(20,21), c(23,24), c(26,27),

  tmp_obj <- OSA_PairComp(tmp_out)
  tmp_nn <- next_nearest(tmp_out,tmp_obj)
  tmp_out <- resolve.comp.OSA(X=tmp_out,Y=tmp_nn,Z=tmp_obj)

  tmp_OSA$Network <- graph(tmp_OSA$RecSeqlist)
  tmp_OSA$EventChanges <- c()

  sequence.reconstruction.OSA <- function(X=OSA_DataFrame,Y=minimum_pairs_list,Z=OSA_PairComp){
    #load in dataframe values
    tmp_weights <- X$weights
    tmp_matrix <- X$Matrix
    tmp_colors <- X$colors
    tmp_size <- X$size
    tmp_comparisons <- X$ComparisonsList
    tmp_maturity <- X$MaturityScore
    tmp_edges <- c(X$RecSeqlist)
    tmp_net <- X$Network

    if (is.null(tmp_net)){
      tmp_net <- make_empty_graph(n = length(X$Semaphoronts), directed = FALSE) #make empty network
      V(tmp_net)$name <-  X$Semaphoronts
    }

    #tmp_events <- X$EventChanges

    tmp_diffs <- Z$Difference.Matrix
    tmp_dist <- Z$Distance
    tmp_dist.matrix <- Z$Distance.Matrix

  #Calculate Mnoront & Mxoront
    #updated_MxMn <- resolve.MxMn(X,Y)

    primary <- Y[[1]]
    secondary <- Y[[2]]
    pair_order <- order(c(seq_along(primary),seq_along(secondary)))

    ## Reconstruct hypothetical nodes and create updated matrix
    #Create empty matrice and give names for Max and Min hypothetical intermediate semaphoronts for all step distances
    Mx_list <- paste("Mx", primary, "&", secondary, sep = "_")
    Mn_list <- paste("Mn", primary, "&", secondary, sep = "_")
    MxMn_ordered_list <- c(Mx_list,Mn_list)[pair_order]
    MxMn.matrix <- matrix(nrow = length(MxMn_ordered_list),
                                  ncol = ncol(tmp_matrix), ##This calls to either the Ev_num or $Events for the main dataframe
                                  dimnames = list( MxMn_ordered_list, X$Events) ##This calls to either the Ev_num or $Events for the main dataframe
      )

    #Fill matrix for each hypothetical intermediate
    N <- nrow(MxMn.matrix)
    MxMn_pairs <- tmp_matrix[c(primary,secondary)[pair_order],]

    ROW_PAIRS <- row.pairs(rownames(MxMn_pairs))

    inferred_nodes <- NULL

    for (g in 1:(N/2)){
      inferred_nodes <- rbind(inferred_nodes, apply(X = MxMn_pairs[ROW_PAIRS[g,],], 2, max))
      inferred_nodes <- rbind(inferred_nodes, apply(X = MxMn_pairs[ROW_PAIRS[g,],], 2, min))
    }

    MxMn.matrix[1:N,1:Ev_num] <- c(inferred_nodes)

  #Combined_MxMn_Real_Matrix <- tmp_matrix ## This needs to happen at the start of the analysis and be renamed to another global variable
  Combined_MxMn_Real_Matrix <- rbind(tmp_matrix,MxMn.matrix)

  #Checks to ensure that duplicates are removed (indicating a simple connection between two existing nodes)

  ##Currently this just removes duplicates, but this removes some key comparisons
  ##Need to make this a bit more complicated
  ##Should identify the original node being matched
  ##Then swap out the names of the nodes
  ##Remove them if they exist already or connect nodes of the same maturity score

  dup_pairs <- duplicated(Combined_MxMn_Real_Matrix) #this creates a logical vector for whether the min_pair has already been included
  updated_matrix <- Combined_MxMn_Real_Matrix[!dup_pairs,]
  ##

  #Update other OSA dataframe values
  updated_names <- rownames(updated_matrix)

  new_N <- nrow(updated_matrix)-nrow(tmp_matrix) #calculate the number of new nodes

  updated_colors <- c(tmp_colors, rep('black', new_N))
  updated_size <- c(tmp_size, rep(1, new_N))
  updated_weights <- c(tmp_weights, rep(0, new_N))
  names(updated_weights) <- updated_names

  updated_maturity <- apply(updated_matrix, 1, sum) ##is it even necessary to track this across the loops? Can't it just be calculated based on the final matrix??

  if (new_N > 0){
    #if a new intermediate is reconstructed, stop this run and rerun cycle
    #this is to ensure that the minimum distance comparisons are given priority
    updated_comparisons <- tmp_comparisons
    updated_edges <- tmp_edges
    updated_net <- tmp_net + vertex(updated_names[!updated_names %in% V(tmp_net)$name])
    # updated_net <- graph(edges=updated_edges, directed = FALSE)

  }else{
    #Update comparison matrix (which removes comparisons already done)
    updated_comparisons <- rbind(tmp_comparisons,Y)
    #rownames(updated_comparisons) <- c(rownames(updated_comparisons),rep(paste("Comp", )))

    new_edges <- c(primary,secondary)[pair_order]
    new_net <- tmp_net + vertex(new_edges[!new_edges %in% V(tmp_net)$name])

      # old_net <- graph(edges=tmp_edges, isolates = updated_names[!updated_names %in% unique(tmp_edges)], directed = FALSE) #make network from prior edges
      # new_edge_net <- graph(edges=new_edges, directed = FALSE) #make network from new edges
      #find the paths which connect the new node pairs (should create a new index entry for each pair with a list of network paths coded as a sequence of the nodes along the path)
      ## but I have yet to explore an example with multiple paths between a node pair, so I'm not really sure yet
      # old_paths <- suppressWarnings(shortest_paths(old_net,new_edges[ODDS],new_edges[EVENS])$vpath)

      ##There is an issue in the line above
      ## it needs to be an apply command, as otherwise it makes inappropriate comparisons
      # apply(ROW_PAIRS,1,function(x) shortest_paths(old_net,new_edges[x,1],new_edges[x,2]))
      # shortest_paths(old_net,new_edges[ODDS],new_edges[EVENS])$vpath
      # old_dist <- list()
      # decision <- c()
      add_edges <- c()
      # old_nodes <- NULL
      # apply(ROW_PAIRS,1,function(x) shortest_paths(new_net,new_edges[x][1],new_edges[x][2])$vpath[[1]])
      for (g in 1:(N/2)){
        P_node <- new_edges[ROW_PAIRS[g,1]]
        S_node <- new_edges[ROW_PAIRS[g,2]]
        old_path <- suppressWarnings(shortest_paths(new_net,P_node,S_node)$vpath)[[1]]

        if (any(old_path)){
          old_path_pairs <-adjacent.pairs(old_path)

          V(tmp_net)$name == primary[1]

          are.connected(tmp_net, primary[1], rownames(tmp_matrix)[5])
          distances(tmp_net, primary[1], rownames(tmp_matrix)[6])
          all_shortest_paths(tmp_net, primary[1], rownames(tmp_matrix)[5])

          are.connected(tmp_net, V(tmp_net)[V(tmp_net)$name == primary[1]], V(tmp_net)[V(tmp_net)$name == secondary[1]])
          distances(tmp_net, V(tmp_net)[V(tmp_net)$name == primary[1]], rownames(tmp_matrix)[10])



          old_path_dist <- sum(apply(old_path_pairs,1,function(x) distances(new_net,old_path[[x]][1],old_path[[x]][2])))

          # distances(new_net,old_path[1],old_path[2])
          #
          # tmp_dist.matrix$From == old_nodes[1:2] & tmp_dist.matrix$To == old_nodes[1:2]
          #
          # old_path_dist <- sum(apply(old_path_pairs,1,function(x) distances(new_net,old_path[[x]][1],old_path[[x]][2])))
          #
          # tmp_dist.matrix$From == old_nodes & tmp_dist.matrix$To == old_nodes

          # old_path[test]
          # old_node_matches <- tmp_dist.matrix$From %in% old_nodes[test[,1]] & tmp_dist.matrix$To %in% old_nodes[test[,2]]
          # old_nodes <- matrix(ncol=2,old_nodes[test])
          #
          # old_node_matches <- tmp_dist.matrix$From == old_nodes & tmp_dist.matrix$To == old_nodes
          #
          # old_nodes <- V(new_net)$name[old_path]
           # old_node_matches <- tmp_dist.matrix$From %in% old_nodes & tmp_dist.matrix$To %in% old_nodes
           # old_dist <- tmp_dist[old_node_matches]
          # old_paths[[g]] <- tmp_path

          # distances(new_net,old_path[1,1],old_path[1,2])

          # tmp_dist.matrix$From %in% new_edges[ROW_PAIRS[g,1]] & tmp_dist.matrix$To %in% new_edges[ROW_PAIRS[g,2]]

          new_node_matches <- tmp_dist.matrix$From %in% new_edges[ROW_PAIRS[g,1]] & tmp_dist.matrix$To %in% new_edges[ROW_PAIRS[g,2]]    #finds those entries where both From and To columns of the distance matrix matched a node from the new edges
          new_dist <- tmp_dist[new_node_matches]     #grab the distance / length along those edges

          # tmp_decision <- any(new_dist < old_path_dist)

          add_edges <- append(add_edges,c(P_node,S_node)[new_dist < old_dist])

          # new_net <- new_net + edge(new_edges[ROW_PAIRS[g,1]],new_edges[ROW_PAIRS[g,2]])
          # new_edges[decision]
          # updated_edges <- c(tmp_edges,new_edges[new_dist < old_dist])

        }else{
          # decision <- c(decision,TRUE,TRUE)
          add_edges <- append(add_edges,c(P_node,S_node))
          # new_net <- new_net + edge(new_edges[ROW_PAIRS[g,1]],new_edges[ROW_PAIRS[g,2]])
          next
        }
      }

      updated_net <- new_net + edges(add_edges)

        # old_paths <- suppressWarnings(shortest_paths(old_net,new_edges[ODDS[g]],new_edges[EVENS[g]])$vpath)[[1]]
        # if(any(old_paths)){
        #   old_nodes <- V(old_net)$name[old_paths]
        # }
      # }

      ##
      # if(any(unlist(old_dist))){
        ##This version works when there aren't any emptry entries in the old_paths list, but not if it does
        #  old_nodes <- lapply(old_paths, function(x) V(old_net)$name[x]) #find all entries that match the nodes in the old paths
        # node_matches <- lapply(old_nodes, function(x) apply(tmp_dist.matrix[,-3] == x,1,all)) #find where both From and To columns of the distance matrix matched a node from the old edges
        #  old_dist <- lapply(node_matches, function(x) tmp_dist[x])[[1]] #grab the distance / length along that edge

        # new_edge_net <- graph(edges=new_edges, directed = FALSE) #make network from new edges
        #
        # new_node_matches <- tmp_dist.matrix$From %in% new_edges[ODDS] & tmp_dist.matrix$To %in% new_edges[EVENS]    #finds those entries where both From and To columns of the distance matrix matched a node from the new edges
        # new_dist <- tmp_dist[new_node_matches]     #grab the distance / length along those edges

        #compare the distances, reject new if longer than existing edges (if this is not done then the network is full of long, unresolved paths which cannot inform about the order of events)

      #   updated_net <- tmp_net + edges(updated_edges[decision])

        # updated_edges <- c(tmp_edges,new_edges[new_dist < old_dist])
        # updated_net <- graph(edges=updated_edges, directed = FALSE)

        #new_events <- tmp_diffs[new_node_matches,-9]
        #updated_events <- c(tmp_events,as.matrix(new_events)[sapply(new_dist, function(x) all(x<old_dist)),])
      }
  # else{
  #
  #       updated_edges <- c(tmp_edges,new_edges)
  #       updated_net <- tmp_net + edges(updated_edges)
  #       # updated_net <- graph(edges=updated_edges, isolates = updated_names[!updated_names %in% unique(updated_edges)], directed = FALSE)
  #     }
    # }
  # }

  #output
  out <- list(Matrix = updated_matrix, Events = X$Events,
              Semaphoronts = updated_names,
              colors = updated_colors,
              size = updated_size,
              weights = updated_weights,
              ComparisonsList = updated_comparisons, RecSeqlist = updated_edges,
              Network = updated_net, MaturityScore = updated_maturity
              # , EventChanges = updated_events
              )

  #start next loop
  }

#### random old code
  # distances(old_net)
  #
  # length(shortest_paths(old_net,new_edges[1],new_edges[2])$vpath[[1]])
  # length(shortest_paths(new_edge_net,new_edges[1],new_edges[2])$vpath[[1]])

  ###This is it!!! It works###





  # if(all(new_dist > old_dist)){
  #   #if all new edges are longer than existing ones, do not add edges and move to next round
  #   updated_edges <- c(tmp_edges)
  # } else{
  #   #if all new edges are longer than existing ones, do not add edges and move to next round
  #   updated_edges <- c(tmp_edges)
  #
  # }

  # extinct_dist_list <-list()
  #
  # tmp_obj$Distance.Matrix$To[V(old_net)$name[old_paths][2] == tmp_obj$Distance.Matrix$To]
  # tmp_obj$Distance.Matrix$Length[tmp_obj$Distance.Matrix$To[V(old_net)$name[old_paths][1] == tmp_obj$Distance.Matrix$From] == V(old_net)$name[old_paths][2]]
  #
  # grep(V(old_net)$name[old_paths][2],tmp_obj$Distance.Matrix$To) %in% grep(V(old_net)$name[old_paths][1],tmp_obj$Distance.Matrix$From)
  # tmp_obj$Distance.Matrix[match(V(old_net)$name[old_paths][1],tmp_obj$Distance.Matrix$From)[tmp_dist],]
  #
  # apply(tmp_obj$Comparison.Matrix, 2, function(x) x[old_paths[1],old_paths[2]])
  #
  # tmp_obj$Distance.Matrix[secondary,primary]
  # tmp_obj$Comparison.Matrix[1:8,1:8]
  # tmp_obj$Comparison.Matrix[old_paths[1],old_paths[2]]
  # tmp_obj$Comparison.Matrix[old_paths[3],old_paths[2]]
  #
  # combn(3,m=2)[2,2]-combn(3,m=2)[1,2]
  #
  #
  # shortest_paths(old_net,new_edges[1],new_edges[2],output="both")$epath[[1]]
  # old_paths <- shortest_paths(old_net,new_edges[1],new_edges[2],output="both")$vpath[[1]]
  #
  # length(shortest_paths(old_net,new_edges[1],new_edges[2])$vpath[[1]])
  # length(distances(old_net,new_edges[1],new_edges[2])$vpath[[1]])
  #
  # shortest_paths(old_net,new_edges[1],new_edges[2])$vpath[[1]][combn(shortest_paths(old_net,new_edges[1],new_edges[2])$vpath[[1]],m=2)
  # c(1,2,3)[combn(3,m=2)]
  #
  # updated_matrix[new_edges,]

  # old_net <- graph(edges=tmp_edges)
  # new_edge_net <- graph(edges=new_edges)
  # distances(old_net)
  #
  # length(shortest_paths(old_net,new_edges[1],new_edges[2])$vpath[[1]])
  # length(shortest_paths(new_edge_net,new_edges[1],new_edges[2])$vpath[[1]])

  # edge_color_list <- list()
  # i=3
  # for (i in 3:length(OSA_list)){
  #   new_edges <- matrix(ncol=2,data=OSA_list[[i]]$RecSeqlist,byrow = T)
  #   old_edges <- c(1:nrow(matrix(ncol=2,data=OSA_list[[i-1]]$RecSeqlist,byrow = T)))
  #
  #   edge.colors <- rep("black", length(new_edges))
  #   edge.colors[-old_edges] <- "red"
  #
  #   edge_color_list[[i]] <- edge.colors
  # }



  # bad_pairs <- dup_pairs[-seq(nrow(tmp_matrix))]
  # new_edges <- c(primary,secondary)[pair_order]
  # if (length(bad_pairs) == length(new_edges)){
  #   updated_edges <- tmp_edges
  # }else{
  #   good_edges <- new_edges[!bad_pairs]
  #   updated_edges <- c(tmp_edges,good_edges)
  # }

####

##After all comparisons have been made & connections reconstructed

#Create sequence list
#Create event transformation list
#Calculate modal sequence
#Calculate average sequence

##Output variables
#hidden ones for plotting
#Final semaphoront matrix
#Final semaphoront network
#Final sequence list
#Final event list

###Calculate all theoretical network connections###

factorial(length(Color_Ontogeny.df$Events))



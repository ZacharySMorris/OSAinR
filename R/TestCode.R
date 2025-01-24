#Test code#

plot(OSA_Guide_Forward.phy$PAUP_1)
plot(OSA_Guide_Forward.phy$PAUP_2)
plot(OSA_Guide_Forward.phy$PAUP_3)
plot(OSA_Guide_Forward.phy$PAUP_4)

plot(OSA_Guide_Reverse.phy)

plot.OSA(All_Semaphoronts.df,Sm_num,Ev_num)



for(i in seq_along(OSA_list)){
  OSA_list[[i]]$PlottingValues <- OSA_PlottingValues(OSA_list[[i]])
}

i=20
for (i in seq(2,length(OSA_list))){
  plot.OSA(OSA_list[[i]],9,8)
  mtext(paste("Step", i, sep=" "))
}

OSA_list[[i]]$Matrix
OSA_list[[i]]
obj_list[[i]]
nn_list[[i]]$minimum_pairs
OSA_list[[i-1]]$RecSeqlist



OSA_list[[26]]$Network
OSA_list[[27]]$Matrix["Mx_Sm_green_&_Mn_Sm_brown_&_Mx_Sm_purple_&_Mx_Sm_orange_&_Mx_Sm_blue_&_Mx_Sm_green_&_Sm_navy_&_Sm_black",]
nn_list[[27]]$minimum_pairs[[1]],nn_list[[27]]$minimum_pairs[[2]]

new_edges <- c(nn_list[[27]]$minimum_pairs[[1]],nn_list[[27]]$minimum_pairs[[2]])[order(c(seq_along(nn_list[[27]]$minimum_pairs[[1]]),seq_along(nn_list[[27]]$minimum_pairs[[2]])))]

old_net <- graph(edges=OSA_list[[27]]$RecSeqlist, isolates = OSA_list[[27]]$Semaphoronts[!OSA_list[[27]]$Semaphoronts %in% unique(OSA_list[[27]]$RecSeqlist)]) #make network from prior edges

  shortest_paths(old_net,new_edges[3],new_edges[4])$vpath[[1]]

  old_nodes[[2]] <- V(old_net)$name[unlist(shortest_paths(old_net,new_edges[3],new_edges[4])$vpath[[1]])] #find all entries that match the nodes in the old paths
  node_matches <- apply(old_nodes == obj_list[[26]]$Distance.Matrix[,-3],1,all)
  old_dist <- unname(obj_list[[26]]$Distance[node_matches])

  new_edge_net <- graph(edges=new_edges) #make network from new edges
  new_node_matches <- apply(new_edges == obj_list[[26]]$Distance.Matrix[,-3],1,all)    #finds those entries where both From and To columns of the distance matrix matched a node from the new edges
  new_dist <- obj_list[[26]]$Distance[new_node_matches]     #grab the distance / length along those edges

  #compare the distances, reject new if longer than existing edges (if this is not done then the network is full of long, unresolved paths which cannot inform about the order of events)
  updated_edges <- c(tmp_edges,new_edges[new_dist < old_dist])

temp_matrix <- OSA_list[[33]]$Matrix

temp_net <- OSA_list[[33]]$Network

set_vertex_attr(temp_net, "name", index = V(temp_net), apply(unname(temp_matrix[V(temp_net)$name,]),1,function(x) paste(x, collapse = "")))
V(temp_net)$name
vertex_attr(temp_net, "name", index = V(temp_net)) <- apply(unname(temp_matrix[V(temp_net)$name,]),1,function(x) paste(x, collapse = ""))

i=16
temp_X <- OSA_list[[i]]$PlottingValues$X
temp_Y <- OSA_list[[i]]$PlottingValues$Y
temp_Matrix <- OSA_list[[i]]$Matrix
temp_bg <- OSA_list[[i]]$colors
temp_cex <- OSA_list[[i]]$size
temp_names <- OSA_list[[i]]$Semaphoronts
temp_net <- OSA_list[[33]]$Network
V(temp_net)$name
temp_recseq <- c(OSA_list[[i]]$RecSeqlist)

old_recseq <- c(OSA_list[[i-1]]$RecSeqlist)
new_edges <- matrix(ncol=2,data=temp_recseq,byrow = T)
old_edges <- c(1:nrow(matrix(ncol=2,data=old_recseq,byrow = T)))

set_edge_attr(temp_net, "color", index = seq(nrow(new_edges))[-old_edges], value="red")

graph(edge=OSA_list[[i]]$RecSeqlist)[[2]]


length(OSA_list[c(2:33)])

new_recseq <- c(OSA_list[[33]]$RecSeqlist) # get the current iterations edge list

temp_net <- graph(edges=new_recseq, isolates = OSA_list[[33]]$Semaphoronts[!OSA_list[[33]]$Semaphoronts %in% unique(new_recseq)]) #create network with isolated nodes
V(temp_net)$name

old_recseq <- OSA_list[[i-1]]$RecSeqlist
new_edges <- matrix(ncol=2,data=new_recseq,byrow = T)
old_edges <- c(1:nrow(matrix(ncol=2,data=old_recseq,byrow = T)))

edge.colors <- rep("black", nrow(new_edges))
edge.colors[-old_edges] <- "red"

plot.OSA.animation(OSA_list[c(2:33)],9,8)

OSA_list[[33]]$MaturityScore[c(1:8)]
OSA_list[[33]]$PlottingValues$Y[c(1:8)]
OSA_list[[33]]$Semaphoronts[-c(1:8)] <- c(paste("hypo",c(1:15)))
OSA_list[[33]]$Semaphoronts[-c(1:8)] <- OSA_list[[32]]$Semaphoronts[-c(1:8)]

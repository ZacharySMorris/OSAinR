library("igraph")

#set the layout boundaries
l <- cbind(OSA_list[[33]]$PlottingValues$X, OSA_list[[33]]$PlottingValues$Y)

tmp_Ylim<-c(0:Ev_num)
plot.new()
rect(xleft = 0.75,
     ybottom = y_bottom,
     xright = 4.5,
     ytop = y_top,
     density = NULL,
     angle = 45,
     col = "light grey",
     border = NA
)

plot(OSA_list[[33]]$Network,vertex.shape="fcircle", edge.arrow.mode=0, vertex.label=NA, vertex.size=12,
     vertex.color=OSA_list[[33]]$colors, vertex.frame.color="black",
     vertex.frame.width=2,
     layout=l,rescale=FALSE,asp=0,
     ylab="Maturity Score", ylim = c(0,max(tmp_Ylim)+0.5),
     xlim = c(1,max(l[,1])+0.5)
     )
#axis(2, tmp_Ylim, pos=0.75, tick = T, lwd = 5, lwd.ticks = 0, las = 1)
axis(2, tmp_Ylim, pos=1, tick = FALSE, lwd.ticks = 0, las = 1)




g <- make_empty_graph() %>%
  add_vertices(3, color = "red") %>%
  add_vertices(2, color = "green") %>%
  add_edges(c(1,2, 2,3, 3,4, 4,5))

RecSeq_list <- c(1,2, 2,3, 3, 1) #a list of node pairs for each edge in the network or the sequences reconstructed by OSA

plot(OSA_list[[33]]$Network, add = TRUE,
     vertex.shape="fcircle", vertex.label=NA, edge.arrow.mode=0, vertex.size=OSA_list[[33]]$size*10,
            vertex.color=OSA_list[[33]]$colors, vertex.frame.color="black",
            vertex.frame.width=2,
            layout=l,rescale=FALSE,asp=0,
            ylab="Maturity Score", ylim = c(0,max(tmp_Ylim)+0.5),
            xlim = c(1,max(l[,1])+0.5)
            )



net_list <- list()


##making a gif of the OSA network being built

for (i in 1:length(OSA_list)){
  tmp_net <- graph(edges=OSA_list[[i]]$RecSeqlist)
  net_list[[i]] <- tmp_net
}

edge_color_list <- list()
i=3
for (i in 3:length(OSA_list)){
  new_edges <- matrix(ncol=2,data=OSA_list[[i]]$RecSeqlist,byrow = T)
  old_edges <- c(1:nrow(matrix(ncol=2,data=OSA_list[[i-1]]$RecSeqlist,byrow = T)))

  edge.colors <- rep("black", length(new_edges))
  edge.colors[-old_edges] <- "red"

  edge_color_list[[i]] <- edge.colors
}

new_edges[-old_edges,]

i=33
for (i in seq_along(OSA_list)){
  plot(net_list[[i]], vertex.shape="fcircle", edge.arrow.mode=0,
       vertex.size=OSA_list[[i]]$size[match(V(net_list[[i]])$name, OSA_list[[i]]$Semaphoronts)]*10,
       vertex.color=OSA_list[[i]]$colors[match(V(net_list[[i]])$name, OSA_list[[i]]$Semaphoronts)], vertex.frame.color="black",
       vertex.frame.width=2, edge.color=edge_color_list[[i]],
       layout=l,rescale=FALSE,asp=0,
       ylab="Maturity Score", ylim = c(0,max(tmp_Ylim)+0.5),
       xlim = c(1,max(l[,1])+0.5)
  )
  title(paste("Step", i, sep = " "))
}

plot(net_list[[3]], vertex.shape="fcircle", vertex.color=c("green","blue","orange","purple","red","navy"),
     vertex.frame.color="black", vertex.frame.width=2, edge.color = edge.colors)
plot(new_edge_list[[3]], vertex.shape="fcircle", vertex.color=c("green","blue","orange","purple","red","navy"),
     vertex.frame.color="black", vertex.frame.width=2, edge.color="red")

OSA_list[[3]]$colors[match(unique(OSA_list[[3]]$RecSeqlist),OSA_list[[3]]$Semaphoronts)]
OSA_list[[3]]$size[match(unique(OSA_list[[3]]$RecSeqlist),OSA_list[[3]]$Semaphoronts)]

OSA_list[[3]]$Semaphoronts


plot(net_list[[i]], vertex.shape="fcircle", vertex.label=NA, edge.arrow.mode=0, vertex.size=OSA_list[[i]]$size*6,
     vertex.color=OSA_list[[i]]$colors, vertex.frame.color="black",
     vertex.frame.width=2,
     #layout=l,rescale=FALSE,asp=0,
     #ylab="Maturity Score", ylim = c(0,max(tmp_Ylim)+0.5),
     #xlim = c(1,max(l[,1])+0.5)
)

sr_test7$colors
sr_test7$size
sr_test7$weights



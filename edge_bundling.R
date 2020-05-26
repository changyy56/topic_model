#HIERARCHICAL EDGE BUNDLING
#https://www.r-graph-gallery.com/311-add-labels-to-hierarchical-edge-bundling/

#libraries
library(ggraph)
library(igraph)
library(tidyverse)
library(RColorBrewer)
library(dplyr)

## connect (connect_mpt): from, to, value
## edges (edge_mpt): from, to
## vertices: name, value, group, id, angle, hjust

#---------------- preprocessing: construct connect ---------------------
mpt_data <- read.csv("dataframe_ouput1.csv", stringsAsFactors=FALSE) #caution: stringsAsFactors=FALSE (or it'll be treated as factor, which could cause error when using rbind) 
#method-problem
by_mp <- group_by(mpt_data, method, problem)
connect_mp <- data.frame(summarise(by_mp, count = n()))
#method-term
mpt_data_mt <- select(mpt_data, method, paper_id, term)
mpt_data_mt <- mpt_data_mt[!duplicated(mpt_data_mt),] # delete duplicates (1003 remain)
by_mt <- group_by(mpt_data_mt, method, term)
connect_mt <- data.frame(summarise(by_mt, count = n()))
#sum(connect_mt$count)
#problem-term
mpt_data_pt <- select(mpt_data, problem, paper_id, term)
mpt_data_pt <- mpt_data_pt[!duplicated(mpt_data_pt),] # delete duplicates (1002 remian)
by_pt <- group_by(mpt_data_pt, problem, term)
connect_pt <- data.frame(summarise(by_pt, count = n()))

#sapply(connect_mp, class)
names(connect_mp) <- c("from", "to", "value")
names(connect_mt) <- c("from", "to", "value")
names(connect_pt) <- c("from", "to", "value")
connect_mpt <- bind_rows(data.frame(connect_mp), data.frame(connect_mt), data.frame(connect_pt))
connect_mpt <- filter(connect_mpt, value > 4)

#---------------- preprocessing: construct edges ---------------------
list_p <- select(mpt_data, problem)
list_p <- list_p[!duplicated(list_p),] 
edge_p <- data.frame(from = "problem", to = list_p) #(66 problem)
list_m <- select(mpt_data, method)
list_m <- list_m[!duplicated(list_m),] 
edge_m <- data.frame(from = "method", to = list_m) #(39 method)
list_t <- select(mpt_data, term)
list_t <- list_t[!duplicated(list_t),] 
edge_t <- data.frame(from = "topic", to = list_t) #(61 term)

edge_origin <- data.frame(from = "origin", to = c("method", "problem", "topic"))
edge_mpt = rbind(edge_origin, edge_m, edge_p, edge_t)

#---------------- preprocessing: construct vertices ---------------------
#vertices$value = #paper
vertices_p <- select(mpt_data, problem, paper_id)
vertices_p <- vertices_p[!duplicated(vertices_p),]
vertices_p <- data.frame(summarise(group_by(vertices_p, problem), count = n())) #(66 problem)
vertices_m <- select(mpt_data, method, paper_id)
vertices_m <- vertices_m[!duplicated(vertices_m),]
vertices_m <- data.frame(summarise(group_by(vertices_m, method), count = n())) #(39 method)
vertices_t <- select(mpt_data, term, paper_id)
vertices_t <- vertices_t[!duplicated(vertices_t),]
vertices_t <- data.frame(summarise(group_by(vertices_t, term), count = n())) #(61 term, total 696/102 paper)

# #vertices$value = average py 
# vertices_p <- select(mpt_data, problem, paper_id, PY)
# vertices_p <- vertices_p[!duplicated(vertices_p),]
# vertices_p <- data.frame(summarise(group_by(vertices_p, problem), count = mean(PY))) #(66 problem)
# vertices_m <- select(mpt_data, method, paper_id, PY)
# vertices_m <- vertices_m[!duplicated(vertices_m),]
# vertices_m <- data.frame(summarise(group_by(vertices_m, method), count = mean(PY))) #(39 method)
# vertices_t <- select(mpt_data, term, paper_id, PY)
# vertices_t <- vertices_t[!duplicated(vertices_t),]
# vertices_t <- data.frame(summarise(group_by(vertices_t, term), count = mean(PY))) #(61 term, total 696/102 paper)

names(vertices_p) <- c("name", "value")
names(vertices_m) <- c("name", "value")
names(vertices_t) <- c("name", "value")
vertices_mpt <- bind_rows(vertices_m, vertices_p, vertices_t) #166

vertices_origin <- data.frame(name = c("origin", "method", "problem", "topic"), value = c(100, 100, 100, 100))
vertices_mpt = rbind(vertices_origin, vertices_mpt)

# Let's add a column with the group of each name. It will be useful later to color points
vertices_mpt$group = edge_mpt$from[ match( vertices_mpt$name, edge_mpt$to ) ]

#------------------- add label information ---------------------
#calculate the ANGLE of the labels
vertices_mpt$id = NA
myleaves_mpt = which(is.na( match(vertices_mpt$name, edge_mpt$from) ))
nleaves_mpt = length(myleaves_mpt)
vertices_mpt$id[ myleaves_mpt ] = seq(1:nleaves_mpt)
vertices_mpt$angle = 90 - 360 * vertices_mpt$id / nleaves_mpt

# calculate the alignment of labels: right or left
# If I am on the left part of the plot, my labels have currently an angle < -90
vertices_mpt$hjust<-ifelse( vertices_mpt$angle < -90, 1, 0)

# flip angle BY to make them readable
vertices_mpt$angle<-ifelse(vertices_mpt$angle < -90, vertices_mpt$angle+180, vertices_mpt$angle)

#------------------- create graph object ---------------------
# Create a graph object
mygraph_mpt <- graph_from_data_frame( edge_mpt, vertices = vertices_mpt )

# The connection object must refer to the ids of the leaves:
from_mpt = match( connect_mpt$from, vertices_mpt$name)
to_mpt = match( connect_mpt$to, vertices_mpt$name)

# # Basic usual argument
# png("mpt_figure.png", height = 480, width=480)
# ggraph(mygraph_mpt, layout = 'dendrogram', circular = TRUE) + 
#   geom_node_point(aes(filter = leaf, x = x*1.05, y=y*1.05)) +
#   geom_conn_bundle(data = get_con(from = from_mpt, to = to_mpt), alpha=0.2, colour="skyblue", width=0.9) +
#   geom_node_text(aes(x = x*1.1, y=y*1.1, filter = leaf, label=name, angle = angle, hjust=hjust), size=1.5, alpha=1) +
#   theme_void() +
#   theme(
#     legend.position="none",
#     plot.margin=unit(c(0,0,0,0),"cm"),
#   ) +
#   expand_limits(x = c(-1.2, 1.2), y = c(-1.2, 1.2))
# dev.off()

#---------------------------- plot ---------------------------
ggraph(mygraph_mpt, layout = 'dendrogram', circular = TRUE) + 
  geom_conn_bundle(data = get_con(from = from_mpt, to = to_mpt), alpha=0.2, width=1, aes(colour=..index..), tension = 0.5) +
  scale_edge_colour_distiller(palette = "RdPu") +
  #geom_conn_bundle(data = get_con(from = from_mpt, to = to_mpt), aes(colour=value), alpha=0.5, width = 1, tension = 0.5) +
  #scale_edge_color_continuous(low="white", high="orange")+
  
  geom_node_text(aes(x = x*1.15, y=y*1.15, filter = leaf, label=name, angle = angle, hjust=hjust, colour=group), size=2, alpha=1) +
  
  geom_node_point(aes(filter = leaf, x = x*1.07, y=y*1.07, colour=group, size=value, alpha=0.2)) +
  scale_colour_manual(values= rep( brewer.pal(9,"Paired") , 30)) +
  #scale_size_continuous( range = c(0.1,10) ) +
  
  theme_void() +
  theme(
  #  legend.position="none",
    plot.margin=unit(c(0,0,0,0),"cm"),
  ) +
  expand_limits(x = c(-1.3, 1.3), y = c(-1.3, 1.3))

#=================== original code ===================================
# # create a data frame giving the hierarchical structure of your individuals
# d1=data.frame(from="origin", to=paste("group", seq(1,10), sep=""))
# d2=data.frame(from=rep(d1$to, each=10), to=paste("subgroup", seq(1,100), sep="_"))
# edges=rbind(d1, d2)
# 
# # create a dataframe with connection between leaves (individuals)
# all_leaves=paste("subgroup", seq(1,100), sep="_")
# connect=rbind( data.frame( from=sample(all_leaves, 100, replace=T) , to=sample(all_leaves, 100, replace=T)), data.frame( from=sample(head(all_leaves), 30, replace=T) , to=sample( tail(all_leaves), 30, replace=T)), data.frame( from=sample(all_leaves[25:30], 30, replace=T) , to=sample( all_leaves[55:60], 30, replace=T)), data.frame( from=sample(all_leaves[75:80], 30, replace=T) , to=sample( all_leaves[55:60], 30, replace=T)) )
# connect$value=runif(nrow(connect))

# # create a vertices data.frame. One line per object of our hierarchy
# vertices = data.frame(
#   name = unique(c(as.character(edges$from), as.character(edges$to))) , 
#   value = runif(111)
# ) 
# # Let's add a column with the group of each name. It will be useful later to color points
# vertices$group = edges$from[ match( vertices$name, edges$to ) ]

# #------------------------
# #let's add information concerning the label we are going to add: angle, horizontal adjustment and potential flip
# #calculate the ANGLE of the labels
# vertices$id=NA
# myleaves=which(is.na( match(vertices$name, edges$from) ))
# nleaves=length(myleaves)
# vertices$id[ myleaves ] = seq(1:nleaves)
# vertices$angle= 90 - 360 * vertices$id / nleaves
# 
# # calculate the alignment of labels: right or left
# # If I am on the left part of the plot, my labels have currently an angle < -90
# vertices$hjust<-ifelse( vertices$angle < -90, 1, 0)
# 
# # flip angle BY to make them readable
# vertices$angle<-ifelse(vertices$angle < -90, vertices$angle+180, vertices$angle)
# 
# #------------------------
# # Create a graph object
# mygraph <- graph_from_data_frame( edges, vertices=vertices )
# 
# # The connection object must refer to the ids of the leaves:
# from = match( connect$from, vertices$name)
# to = match( connect$to, vertices$name)
# 
# ## connect: from, to, value
# ## edges: from, to
# ## vertices: name, value, group, id, angle, hjust
# 
# # Basic usual argument
# png("my figure.png", height = 480, width=480)
# ggraph(mygraph, layout = 'dendrogram', circular = TRUE) + 
#   geom_node_point(aes(filter = leaf, x = x*1.05, y=y*1.05)) +
#   geom_conn_bundle(data = get_con(from = from, to = to), alpha=0.2, colour="skyblue", width=0.9) +
#   geom_node_text(aes(x = x*1.1, y=y*1.1, filter = leaf, label=name, angle = angle, hjust=hjust), size=1.5, alpha=1) +
#   theme_void() +
#   theme(
#     legend.position="none",
#     plot.margin=unit(c(0,0,0,0),"cm"),
#   ) +
#   expand_limits(x = c(-1.2, 1.2), y = c(-1.2, 1.2))
# dev.off()
# 
# #-----------------------------
# ggraph(mygraph, layout = 'dendrogram', circular = TRUE) + 
#   geom_conn_bundle(data = get_con(from = from, to = to), alpha=0.2, width=0.9, aes(colour=..index..)) +
#   scale_edge_colour_distiller(palette = "RdPu") +
#   
#   geom_node_text(aes(x = x*1.15, y=y*1.15, filter = leaf, label=name, angle = angle, hjust=hjust, colour=group), size=2, alpha=1) +
#   
#   geom_node_point(aes(filter = leaf, x = x*1.07, y=y*1.07, colour=group, size=value, alpha=0.2)) +
#   scale_colour_manual(values= rep( brewer.pal(9,"Paired") , 30)) +
#   scale_size_continuous( range = c(0.1,10) ) +
#   
#   theme_void() +
#   theme(
#     legend.position="none",
#     plot.margin=unit(c(0,0,0,0),"cm"),
#   ) +
#   expand_limits(x = c(-1.3, 1.3), y = c(-1.3, 1.3))


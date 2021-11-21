#PS5 Code 

rm(list=ls())

library(readr)
library(ggplot2)
library(readxl)
library(tidyverse)
library(igraph)
library(ggnetwork)
library(ggraph)
library(graphlayouts)
library(GGally)

#install.packages("DiagrammeR")

edgelist <- read_excel("Desktop/IU/INFO/Complex Systems/PS5/edgelist.xlsx", 
                       sheet = "Sheet1")

nodes<-read_excel("Desktop/IU/INFO/Complex Systems/PS5/edgelist.xlsx", 
                     sheet = "Sheet2")





edge_start<-substring(edgelist$Edges,1,1)
edge_end<-substring(edgelist$Edges,2,2)

edgelist<-cbind(edge_start,edge_end)%>%as.data.frame()

g1=graph_from_data_frame(edgelist, directed=F, vertices=nodes)
plot(g1)

degree(g1)
c1 = cluster_fast_greedy(g1)
member_df=data.frame(Node=c1$names,club=c1$membership)
nodes=nodes%>%inner_join(member_df)


ggraph::create_layout(g1,layout = "auto")
plot(g1)

df1=edgelist%>%
  inner_join(nodes,by=c("edge_start"="Node"))%>%
  rename(x0=x,y0=y)%>%
  inner_join(nodes,by=c("edge_end"="Node"))%>%
  rename(x1=x,y1=y)

ggplot(nodes,aes(x,y)) +
  geom_edges(data=df1,aes(x=x0,y=y0,xend=x1,yend=y1))+
  geom_nodes(aes(color=as.factor(club)),size=4)





cascade_fun=function(edge_list,nodes,initial_infect,q_rule){
  g1=graph_from_data_frame(edgelist, directed=F, vertices=nodes)
  
  
  vertex_list=names(V(g1))
  infect_nodes_index=which(vertex_list %in% initial_infect)
  non_infect=which(vertex_list %in% initial_infect ==F)
  set.vertex.attribute(g1,name="brand",value="B")
  g1<-set.vertex.attribute(g1,"brand",index=infect_nodes_index,value="A")
  g1<-set.vertex.attribute(g1,"brand",index=non_infect,value="B")
  
  convert_number=1
  counter=0
  node_status_df=data.frame(Time=counter,Nodes=vertex_list,brand=V(g1)$brand)
  
  infect_count=length(initial_infect)
  new_infect_c=1
  #browser()
  while(convert_number>0 & counter<100 &infect_count<length(vertex_list) &new_infect_c>0){
    print(new_infect_c)
    counter=counter+1
    temp_nodes=data.frame(Time=counter,Nodes=vertex_list,brand=V(g1)$brand)
    
    period_convert=c()
    for (ind in 1:length(vertex_list)){
      verts<-vertex_list[ind]
      neigh=neighbors(g1,verts)
      neigh_share=mean(V(g1)[neigh]$brand=="A")
      current_status=V(g1)$brand[ind]
      if( neigh_share>=q_rule &  current_status!="A" ){
        period_convert=rbind( period_convert, verts)
      }else{
        next
      }
      
      period_convert<-as.character(period_convert)
      period_convert_index=which(vertex_list %in% period_convert)
      g1<-set.vertex.attribute(g1,"brand",index= period_convert_index,value="A")
    }
    #browser()
    
    
    
    temp_nodes$brand=ifelse(temp_nodes$Nodes %in% period_convert,"A", temp_nodes$brand)
    node_status_df<-rbind( node_status_df,temp_nodes)
    convert_number=sum(vertex_list%in% period_convert)
    infect_count=sum(temp_nodes$brand=="A")
    
    old_count=sum(node_status_df$brand[node_status_df$Time==(counter-1)]=="A")
    
    new_infect_c=infect_count-old_count
    
  }
  node_status_df=node_status_df%>%inner_join(nodes,by=c("Nodes"="Node"))
  
  return( list(node_status_df=node_status_df,net=g1))
  
  
}

res1=cascade_fun(edge_list = edgelist,nodes = nodes,
                 initial_infect = c("e","f"),q_rule = 2/5)

res1$node_status_df%>%
  ggplot(aes(x,y)) +
  geom_edges(data=df1,aes(x=x0,y=y0,xend=x1,yend=y1))+
  geom_nodes(aes(color=brand),size=5)+
  geom_text(aes(label=Nodes),size=4)+
  facet_wrap(~Time)+
  theme_blank()

plot(res1$net)

res_net=res1$net


res_net<-set.vertex.attribute(res_net,"cluster",index=which(V(res_net)$brand=="B"),value="Block S")


test_vert_names=V(res_net)[V(res_net)$brand=="B"]%>%names()

#test_vert_names=names(V(res_net))

nodes_2=data.frame(Node=nodes$Node,neighbors=0,neighbor_share=0)

neighbors(g1,nodes$Node[2])%>%unlist()%>%names()

for(i in 1:nrow(not_S)){
  tn=neighbors(g1,nodes$Node[i])%>%unlist()%>%names()
  nodes_2$neighbors[i]<-sum(tn %in% test_vert_names)
  nodes_2$neighbor_share[i]<-mean(tn %in% test_vert_names)
}


nodes%>%inner_join( nodes_2)%>%
  mutate(status=ifelse(neighbor_share>=3/5,"Not S","S"),
         mesg=paste0(Node,": ",round(neighbor_share,2),""))%>%
  ggplot(aes(x,y)) +
  geom_edges(data=df1,aes(x=x0,y=y0,xend=x1,yend=y1))+
  geom_nodes(aes(color=as.factor(status)),size=4)+
  ggrepel::geom_label_repel(aes(label= mesg))+
  theme_blank()




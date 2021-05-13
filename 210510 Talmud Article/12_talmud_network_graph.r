##################################################################################################################
################################### Make a network and draw the graph
##################################################################################################################
# help from this site: https://kateto.net/network-visualization
# http://www.kateto.net/wp-content/uploads/2019/06/Sunbelt%202019%20R%20Network%20Visualization%20Workshop.pdf
print("Create network graphs")
print("Create network graphs: Part 1")
Rabbi_List = unique(unlist(df_scores8[c("Disputant_1", "Disputant_2")]))
df_rabbi_nodes = data.frame(Rabbi_List)
df_rabbi_nodes$IDNum = seq.int(nrow(df_rabbi_nodes))
df_rabbi_nodes$id = paste("s", df_rabbi_nodes$IDNum, sep="")
df_rabbi_nodes <- df_rabbi_nodes %>% select(Rabbi_List, id)
colnames(df_rabbi_nodes) <- c("RabbiName", "id")
df_rabbi_nodes = left_join(df_rabbi_nodes, df_results_100, by=c("RabbiName"="Name"))

print("Create network graphs: Part 2")
df_rabbi_links = df_scores8[c("WinnerName", "LoserName")]
df_rabbi_links = left_join(df_rabbi_links, df_rabbi_nodes %>% select(RabbiName, id, n.disputes), by=c("WinnerName" = "RabbiName"))
colnames(df_rabbi_links) <- c("WinnerName", "LoserName", "Winnerid", "WinnerMatches")
df_rabbi_links = left_join(df_rabbi_links, df_rabbi_nodes %>% select(RabbiName, id, n.disputes), by=c("LoserName" = "RabbiName"))
colnames(df_rabbi_links) <- c("WinnerName", "LoserName", "Winnerid", "WinnerMatches", "Loserid", "LoserMatches")

print("Create network graphs: Part 3")
df_rabbi_links2 <- df_rabbi_links %>% group_by(WinnerName, LoserName) %>% summarize(wincount=n())
df_rabbi_links3 <- left_join(df_rabbi_links2, df_rabbi_links2, by=c("WinnerName"="LoserName", "LoserName"="WinnerName"))
colnames(df_rabbi_links3) <- c("WinnerName", "LoserName", "wincount", "losecount")
df_rabbi_links3$losecount[is.na(df_rabbi_links3$losecount)] <- 0
df_rabbi_links3$totalcount <- df_rabbi_links3$losecount + df_rabbi_links3$wincount

print("Create network graphs: Part 4")
k = 5
df_rabbi_links_mink <- df_rabbi_links %>% filter(WinnerMatches>=k, LoserMatches>=k)
df_rabbi_links_mink2 <- df_rabbi_links_mink %>% group_by(WinnerName, LoserName) %>% summarize(wincount=n())
df_rabbi_links_mink3 <- left_join(df_rabbi_links_mink2, df_rabbi_links_mink2, by=c("WinnerName"="LoserName", "LoserName"="WinnerName"))
colnames(df_rabbi_links_mink3) <- c("WinnerName", "LoserName", "wincount", "losecount")
df_rabbi_links_mink3$losecount[is.na(df_rabbi_links_mink3$losecount)] <- 0
df_rabbi_links_mink3$totalcount <- df_rabbi_links_mink3$losecount + df_rabbi_links_mink3$wincount
df_rabbi_nodes_mink <- df_rabbi_nodes %>% filter(n.disputes >= 5)

print("Create network graphs: Part 5")
# Make a directed graph where the arrow flows from the loser to a winner
net_rabbi_dir <- graph_from_data_frame(d=df_rabbi_links3, vertices=df_rabbi_nodes, directed=T)

# Make an undirected graph where the connection is based on the number of disputes
df_rabbi_links_undir3 <- df_rabbi_links3 %>%
    group_by(rabbiname_1 = pmin(WinnerName, LoserName), rabbiname_2 = pmax(WinnerName, LoserName)) %>% 
    summarise(count = max(totalcount))
net_rabbi_undir <- graph_from_data_frame(d=df_rabbi_links_undir3, vertices=df_rabbi_nodes, directed=F)

print("Create network graphs: Part 6")
df_relationship_map <- df_rabbi_links_undir3 %>% spread("rabbiname_2","count") %>% replace(., is.na(.), 0)
df_relationship_map$TotalDisputes = rowSums(df_relationship_map[,-1])

FieldList25 = Big25List
FieldList25[26] <- "rabbiname_1"
FieldList25[27] <- "TotalDisputes"
# This used to work, fails with dlypr update
# https://stackoverflow.com/questions/62609723/must-subset-elements-with-a-valid-subscript-vector-during-an-dplyranti-join
# df_relationship_map_top25 <-  df_relationship_map %>% filter(rabbiname_1 %in% Big25List) %>% select(names(.) %in% FieldList25)
# New version
df_relationship_map_top25 <-  df_relationship_map %>% filter(rabbiname_1 %in% Big25List) %>% select(any_of(FieldList25))


print("Create network graphs: Part 7")
# Make an undirected graph where the connection is based on the number of disputes and there is a minimum of k connections
df_rabbi_links_undir_mink3 <- df_rabbi_links_mink3 %>%
    group_by(rabbiname_1 = pmin(WinnerName, LoserName), rabbiname_2 = pmax(WinnerName, LoserName)) %>% 
    summarise(count = max(totalcount))


df_rabbi_nodes_mink$x = log(df_rabbi_nodes_mink$UniqueQuestions)
df_rabbi_nodes_mink$y = df_rabbi_nodes_mink$EloScoreWins
net_rabbi_undir_mink <- graph_from_data_frame(d=df_rabbi_links_undir_mink3, vertices=df_rabbi_nodes_mink, directed=FALSE)

l2b <- layout_in_circle(net_rabbi_undir)
plot(net_rabbi_undir, layout=l2b)

l3b <- layout_in_circle(net_rabbi_undir_mink)
plot(net_rabbi_undir_mink, layout=l3b)


# Make an undirected graph where the connection is based on the number of disputes and the rabbis are in the top 25 list

print("Create network graphs: Part 8")
df_gen000 = read.xlsx(xlsxFile="rabbi list top 25 by n_rabbi_generation.xlsx", sheet="Top 25 Rabbis by Disputes")
df_gen000$RabbiName = df_gen000$English_Name

df_rabbi_nodes_top25 <- df_rabbi_nodes %>% filter((RabbiName %in% Big25List))

df_rabbi_links_top25 <- df_rabbi_links %>% filter((WinnerName %in% Big25List), (LoserName %in% Big25List))


colnames(df_rabbi_links_top25) <- c("WinnerName", "LoserName", "Winnerid", "wincount", "Loserid", "losecount")
df_rabbi_links_top25$losecount[is.na(df_rabbi_links_top25$losecount)] <- 0
df_rabbi_links_top25$totalcount <- df_rabbi_links_top25$losecount + df_rabbi_links_top25$wincount


# df_rabbi_links_undir_mink3 <- df_rabbi_links_mink3 %>%
#     group_by(rabbiname_1 = pmin(WinnerName, LoserName), rabbiname_2 = pmax(WinnerName, LoserName)) %>% 
#     summarise(count = max(totalcount))

print("Create network graphs: Part 9a")
#print("Create network graphs: Part 9a1")
df_rabbi_links_undir_top25 <- df_rabbi_links_top25 %>%
    group_by(rabbiname_1 = pmin(WinnerName, LoserName), rabbiname_2 = pmax(WinnerName, LoserName)) %>% 
    summarise(count = max(totalcount))
#print("Create network graphs: Part 9a2")
df_rabbi_nodes_top25 = left_join(df_rabbi_nodes_top25, df_gen000, by="RabbiName")
#print("Create network graphs: Part 9a3")
df_rabbi_nodes_top25$x = log(df_rabbi_nodes_top25$UniqueQuestions)
#print("Create network graphs: Part 9a4")
df_rabbi_nodes_top25$y = df_rabbi_nodes_top25$Pretty_Generation
#print("Create network graphs: Part 9a5")
net_rabbi_undir_top25 <- graph_from_data_frame(d=df_rabbi_links_undir_top25, vertices=df_rabbi_nodes_top25, directed=FALSE)

# Specify x and Y for graph
# Add X and Y to the Node List / vertices data
# https://stackoverflow.com/questions/5364264/how-to-control-the-igraph-plot-layout-with-fixed-positions/5364376#5364376
print("Create network graphs: Part 9a6")
#plot(net_rabbi_undir_top25, edge.curved = 0.3)
print("Create network graphs: Part 9b")
df_rabbi_nodes_top25b = df_rabbi_nodes_top25
df_rabbi_nodes_top25b$x = 1
df_rabbi_nodes_top25b[(df_rabbi_nodes_top25b$RabbiName %in% Big05List), "x"] = 0
df_rabbi_nodes_top25b[(df_rabbi_nodes_top25b$RabbiName %in% Big25List) & (df_rabbi_nodes_top25b$RabbiName %ni% Big15List), "x"] = -1
df_rabbi_nodes_top25b[df_rabbi_nodes_top25b$RabbiName %in% Big05List, "y"] = (-2:2)*5
df_rabbi_nodes_top25b[df_rabbi_nodes_top25b$RabbiName %ni% Big05List, "y"] = (0:19) * (20/19) - 10

print("Create network graphs: Part 9c")
net_rabbi_undir_top25b <- graph_from_data_frame(d=df_rabbi_links_undir_top25, vertices=df_rabbi_nodes_top25b, directed=FALSE)
# plot(net_rabbi_undir_top25b, edge.curved = 0.3)
E(net_rabbi_undir_top25b)$weight <- (log(edge_attr(net_rabbi_undir_top25b, "count")) - 4) 
# E(net_rabbi_undir_top25b)$weight <- edge_attr(net_rabbi_undir_top25b, "count")

print("Create network graphs: Part 10")
plot(net_rabbi_undir_top25b, edge.curved = 0.3, edge.width = E(net_rabbi_undir_top25b)$weight)

Big4 = c("Chachamim", "Tana Kama", "Implicit opinion", "Stam")
df_rabbi_nodes_top25ex4 <- df_rabbi_nodes_top25 %>% filter(RabbiName %ni% Big4)

df_rabbi_links_top25ex4 <- df_rabbi_links %>% filter((WinnerName %in% Big25List), (WinnerName %ni% Big4), (LoserName %in% Big25List), (LoserName %ni% Big4))
colnames(df_rabbi_links_top25ex4) <- c("WinnerName", "LoserName", "Winnerid", "wincount", "Loserid", "losecount")
df_rabbi_links_top25ex4$losecount[is.na(df_rabbi_links_top25ex4$losecount)] <- 0
df_rabbi_links_top25ex4$totalcount <- df_rabbi_links_top25ex4$losecount + df_rabbi_links_top25ex4$wincount
df_rabbi_links_undir_top25ex4 <- df_rabbi_links_top25ex4 %>%
    group_by(rabbiname_1 = pmin(WinnerName, LoserName), rabbiname_2 = pmax(WinnerName, LoserName)) %>% 
    summarise(count = max(totalcount))
net_rabbi_undir_top25ex4 <- graph_from_data_frame(d=df_rabbi_links_undir_top25ex4, vertices=df_rabbi_nodes_top25ex4, directed=FALSE)

print("Create network graphs: Part 11")
E(net_rabbi_undir_top25ex4)$weight <- (log10(edge_attr(net_rabbi_undir_top25ex4, "count")) - 1.5) * 2 
#layout_on_grid
# layout_nicely
#layout_with_gem

l4 <- layout_with_gem(net_rabbi_undir_top25ex4)

plot(net_rabbi_undir_top25ex4, edge.curved = 0.3, layout=l4, edge.width = E(net_rabbi_undir_top25ex4)$weight)

# net_rabbi_dir_simp <- simplify(net_rabbi_dir, remove.multiple = F, remove.loops = T)
# plot(net_rabbi_dir_simp, edge.arrow.size=.4,vertex.label=NA)

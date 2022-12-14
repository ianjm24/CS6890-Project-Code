#library(igraph)

#mygraph <- read.graph("com-amazon.ungraphRS.txt", format = "ncol", directed = FALSE)
#mygraph <- read.graph("toynet1-dir.txt", format = "ncol", directed = TRUE)
#mygraph <- read.graph("karate-edge.txt", format = "ncol", directed = FALSE)

mygraph <- read.graph("C:\\Users\\ianjm\\School\\2022Fall\\MachineLearningWithGraphs\\Project\\Code\\DensityBasedCommunityDetectionOptimization\\Density-based-Community-Detection-Optimization\\Wiki-Vote.txt", format = "ncol", directed = TRUE)
#mygraph <- read.graph(paste("grafo_",graph.n , ".txt",sep=""), format = "ncol", directed = TRUE)
#mygraph <- read.graph(paste("Wiki_Vote_top-",graph.n ,".txt",sep=""), format = "ncol", directed = TRUE)
#mygraph <- read.graph(graph.n, format = "ncol", directed = TRUE)


comms <- label.propagation.community (mygraph)
n.comms <- max(membership(comms))
new.n.comms <- n.comms
#browser()
#para nos numerados
#df.comms <<- data.frame(node = as.numeric(names(membership(comms))), comm = as.numeric(membership(comms)))

#para nos alfanumericos
df.comms <<- data.frame(node = names(membership(comms)), comm = as.numeric(membership(comms)))
df.comms.old <- df.comms

#write.csv(df.comms, file = "toynet3_res_original.csv", sep = " ")
write.csv(df.comms, file = paste("Wiki_Vote2_res_original_",counter1,".csv",sep=""), sep = " ")

original.comm.density <- c()
optimized.comm.density <- c()

for (current.comm in 1:n.comms){
  
  comm <- which(membership(comms) == current.comm)
  subgraph <- induced.subgraph(graph = mygraph, v = comm, impl = 'copy_and_delete')
  comm.density <- graph.density(subgraph, loops = FALSE)
  original.comm.density <- c(original.comm.density, comm.density)
  ncompcomm <- clusters(subgraph, mode=c("strong"))
  
  if(ncompcomm$no > 1){
    
    densities <- c()
    compnodeslist <- list()
    
    for (comp in 1:ncompcomm$no){
      
      compnodes <- which(membership(ncompcomm) == comp)
      #browser()
      compnodeslist <- c(compnodeslist, list(comm[compnodes]))
      subsubgraph <- induced.subgraph(graph = subgraph, vids = compnodes, impl = "copy_and_delete")
      if(gorder(subsubgraph)==1){
        compdensity <- 0
      }else{
        compdensity <- graph.density(subsubgraph, loops=FALSE) 
      }
      densities <- c(densities, compdensity)
      
    }
    
    if (mean(densities, na.rm = TRUE) > comm.density){
      #create new communities
      new.comms <- new.n.comms + rep(1:ncompcomm$no)
      
      n <<- 0
      
      lapply(compnodeslist, FUN = function(x){
        n <<- n + 1 
        cat("New community for a found component!!! \n\n")
        sapply(x,FUN=function(y){
          df.comms[as.numeric(df.comms$node)==y,2] <<- new.comms[n]
          cat("Changing node ", y, "community label from ", membership(comms)[y] , " to ", new.comms[n], ".\n")
        })
      })
      
      new.n.comms <- new.comms[length(new.comms)]
      optimized.comm.density <- c(optimized.comm.density, densities)
    }
  }else{
    optimized.comm.density <- c(optimized.comm.density, comm.density)
  }
}

#browser()
ori.mean <- mean(original.comm.density, na.rm = TRUE)
comm.mean <- mean(optimized.comm.density, na.rm = TRUE)
ori.mod <- modularity(mygraph, df.comms.old[,2])
mod.new <- modularity(mygraph, df.comms[,2])

cat("\nAverage Original Comm. Density: ", ori.mean)
cat("\nAverage Optimized Comm. Density: ", comm.mean)
cat("\nOld Modularity: ", ori.mod)
cat("\nNew Modularity: ", mod.new)

#write.csv(df.comms, file = "toynet3_res_opt.csv", sep = " ")
write.csv(df.comms, file = paste("Wiki_Vote2_res_opt_",counter1,".csv",sep=""), sep = " ")
Density.df.results <<- rbind(Density.df.results, c(graph.n,ori.mean,comm.mean, ori.mod, mod.new))
names(Density.df.results) <- c("graph.n","ori.mean","comm.mean", "ori.mod", "mod.new")


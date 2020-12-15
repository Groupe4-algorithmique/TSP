compute_distance <- function(G,cities,start_city){
  n = length(cities)
  distance = G[start_city,cities[1]]
  for (i in 1:(n-1)){
    distance = distance + G[cities[i],cities[i+1]]
  }
  return(distance + G[cities[n],start_city])
}

naive_method <- function(G,cities,start_city){
  n = length(cities)
  c = rep(1,n)
  i = 1
  dist_optimal = compute_distance(G,cities,start_city)
  path_optimal = cities
  while (i < n+1){
    if (c[i] < i){
        if (i %% 2 == 1 ){
            tmp = cities[1]
            cities[1] = cities[i]
            cities[i] = tmp
        }
        else{
            tmp = cities[c[i]]
            cities[c[i]] = cities[i]
            cities[i] = tmp
        }
        if ( compute_distance(G,cities,start_city) < dist_optimal){

            path_optimal = cities
            dist_optimal = compute_distance(G,path_optimal,start_city)
        }
        c[i] = c[i] + 1
        i = 1
    }
    else {
        c[i] = 1
        i = i+1
    }
  }
  return(list(path_optimal=path_optimal,dist_optimal=dist_optimal))
}


sget_subsets <- function(set,p){
  subsets = list()
  for (i in 1:(2^length(set))){
    t = i
    tmp = c()
    for (j in 1:length(set)){
      if (bitwAnd(t, 1)){
        tmp = c(tmp,set[[j]])
      }
      t =t %/% 2
    }
    if (length(tmp)==p){
      subsets[[length(subsets)+1]]=tmp
    }
  }
  return(subsets)
}

held_karp <- function(G,n){
  C = matrix(Inf,nrow=0,ncol=n)
  C = data.frame(C)
  pr = matrix(Inf,nrow=0,ncol=n)
  pr = data.frame(pr)
  for (k in 2:n){
    C[nrow(C) + 1,] = rep(Inf,n)
    C[nrow(C),k] = G[1,k]
    rownames(C)[nrow(C)] = paste( unlist(k), collapse='')
  }
  
  for (s in 2:(n-1)){
    #subSets = combn(2:n, s, simplify = FALSE)
    subSets = get_subsets(2:n,s)
    for (S in subSets){
      C[nrow(C) + 1,] = rep(Inf,n)
      rownames(C)[nrow(C)]=paste(unlist(S), collapse='')
      pr[nrow(pr) + 1,] = rep(Inf,n)
      rownames(pr)[nrow(pr)]=paste(unlist(S), collapse='')
      for (k in S){
        C_S_k = construct_C_S_k(C,S,k,G)
        tmp = search_min_C_S_k(C_S_k)
        C[nrow(C),k] = tmp$c_s_k
        pr[nrow(pr),k] = tmp$m
      }
    }
  }
  C_S_1 = construct_C_S_k(C,c(2:n),1,G)
  min_tmp = search_min_C_S_k(C_S_1)
  dist_opt = min_tmp$c_s_k
  path_optimal = rep(0,(n-1))
  path_optimal[1] = min_tmp$m
  subset_opt = c(2:n)
  for (k in 2:(n-1)){
    row_S_k = paste(unlist(subset_opt), collapse='')
    path_optimal[k] = pr[row_S_k,path_optimal[k-1]]
    subset_opt = subset_opt[subset_opt!=path_optimal[k-1]]
  }
  return(list(dist_opt=dist_opt,path_optimal=rev(path_optimal)))
}

construct_C_S_k <- function(C,Subset,k,G){
  C_S_k = list()
  S_k  = Subset[Subset!=k]
  row_S_k = paste(unlist(S_k), collapse='')
  for (m in Subset){
    if (m!=k){
      C_S_k[as.character(m)] = C[row_S_k,m] + G[m,k]
    }
  }
  return(C_S_k)
}

search_min_C_S_k <- function(C_S_k){
  m_0 = names(C_S_k)[1]
  for (m in names(C_S_k)){
    if (C_S_k[[m]]  < C_S_k[[m_0]]){
      m_0=m
    }
  }
  return(list(c_s_k=C_S_k[[m_0]],m=as.integer(m_0)))
}

test_compare <- function(n){
  #tmp = sample.int(5, n*n, replace = TRUE)
  tmp =  runif(n*n,1,10)
  G = matrix(tmp,nrow=n,ncol=n)
  G = G %*% t(G)
  #G = matrix(c(0,1,15,6,2,0,7,3,9,6,0,12,10,4,8,0),nrow=4,ncol=4)
  diag(G) = 0
  print(G)
  print('-----------------')
  print('held_karp')
  ptm <- proc.time()
  hp = held_karp(G,n)
  time_hp = proc.time()-ptm
  print(paste('Temps pour finir Held-karp',time_hp['elapsed']))
  print(hp$path_optimal)
  print('-----------------')
  print('All permutations test')
  
  ptm <- proc.time()
  nm = naive_method(G,c(2:n),1)
  time_nv = proc.time()-ptm
  
  print(paste('Temps pour tester toutes les permutations',time_nv['elapsed']))#end_time1-start_time1))
  print(nm$path_optimal)
  print('-----------------')
  print(paste('distance optimal avec Held_karp ',compute_distance(G,hp$path_optimal,1)))
  print(paste('distance optimal avec tout les tests',compute_distance(G,nm$path_optimal,1)))
}


#' @name compute_distance
#' @title calcul de distance selon un ordre de villes.
#' @param G : la matrice des distances.
#' @param cities : vecteur des villes à visiter.
#' @param start_city : la ville de début du voyage.
#' @usage compute_distance(G,cities,start_city)
#' @return La distance totale parcourue en commençant par 'start_city' et visitant 'cities' et terminant en 'start_city'.
#' @examples 
#' G = matrix(runif(4*4,min=10,max=50),nrow=4,ncol=4)
#' G = G %*% t(G) # rendre G symétrique.
#' diag(G) = 0 # annuler la diagonale de G.
#' ## la symétrie de G n'est pas nécessaire.
#' start_city = 1 # la ville de Départ.
#' cities = c(2,3,4)  # un ordre de ville à visiter.
#' distance = compute_distance(G,cities,start_city) # calcul de distance du tour.
compute_distance <- function(G,cities,start_city){
  n = length(cities)
  distance = G[start_city,cities[1]]
  for (i in 1:(n-1)){
    distance = distance + G[cities[i],cities[i+1]]
  }
  return(distance + G[cities[n],start_city])
}

#' @name  naive_method
#' @title Test de toutes les permutations possibles.
#' @param G : la matrice des distances.
#' @param cities : vecteur des villes à visiter.
#' @param start_city : la ville de début du voyage.
#' @usage naive_method(G,cities,start_city)
#' @return La séquence des villes qui donnent la moindre en distance.
#' @examples 
#' G = matrix(runif(4*4 , min = 10, max = 50),nrow=4)
#' G = G %*% t(G) # rendre G symétrique (n'est pas nécessaire.).
#' diag(G) = 0 # annuler la diagonale de G.
#' start_city = 1 # la ville de Départ.
#' cities = c(2,3,4)  # un ordre de ville à visiter.
#' results = naive_method(G,cities,start_city)
#' results['path_optimal'] # l'ordre des villes optimales.
#' results['dist_optimal'] # la distance obtenue avec l'ordre des villes optimales.
naive_method <- function(G,cities,start_city){
  if (start_city %in% cities){
    stop(showNonASCII(paste("'start_city' :",start_city," ne doit pas \u00EAtre dans 'cities' :",toString(cities))))
  }
  if ( (!is.matrix(G)) || dim(G)[1] != dim(G)[2]){
   stop(showNonASCII(paste("'G' :",toString(G)," doit \u00EAtre une matrice carr\u00E9e.")))
  }
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
        # On compare la nouvelle permutation avec la précédente.
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

##########################################################################

#' @name  .get_subsets 
#' @title (Fonction cachée) Génère toutes les sous partitions de taille p d'un groupe de taille n.
#' @param set : le groupe initial.
#' @param p : la taille des partitions qu'on veut extraire de 'set'.
#' @return L'ensemble des partitions de taille p du groupe 'set'.
.get_subsets <- function(set,p){
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

#' @name .construct_C_S_k 
#' @title (Fonction cachée) Construire le vecteur (la liste [C(S{k}, m) + G[m,k]] pour tout m dans 'Subset')
#' @param C : la matrice tel que C[S,k] le coût min du chemin à partir de 1 et se termine au 
#' #          sommet k, passant les sommets de l'ensemble S exactement une fois.
#' @param Subset : Un sous groupe du groupe complet des villes.
#' @param k : La ville pour laquelle on veut calculer C[S-k,m] pour tout m dans Subset.
#' @param G : la matrice des distances.
#' @usage .construct_C_S_k(C,Subset,k,G)
#' @return (la liste [C(S{k}, m) + G[m,k]] pour tout m dans 'Subset')
.construct_C_S_k <- function(C,Subset,k,G){
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

#' @name  .search_min_C_S_k 
#' @title (Fonction cachée) recherche du min dans un vecteur et son index.
#' @param C_S_k : Une liste construit par la fonction '.construct_C_S_k'.
#' @return Le minimum du vecteur et l'index du minimum.
.search_min_C_S_k <- function(C_S_k){
  m_0 = names(C_S_k)[1]
  for (m in names(C_S_k)){
    if (C_S_k[[m]]  < C_S_k[[m_0]]){
      m_0=m
    }
  }
  return(list(c_s_k=C_S_k[[m_0]],m=as.integer(m_0)))
}

#' @name  held_karp
#' @title L'algorithme de Held_karp.
#' @param G : la matrice des distances.
#' @param n : le nombre de villes.
#' @usage held_karp(G,n)
#' @return La séquence des villes qui donnent la moindre en distance et la distance optimale.
#' @examples 
#' G = matrix(runif(4*4 , min = 10, max = 50),nrow=4)
#' G = G %*% t(G) # rendre G symétrique (n'est pas nécessaire.).
#' diag(G) = 0 # annuler la diagonale de G.
#' n = 4 # nombre de villes.
#' results = held_karp(G,n)
#' results['path_opt'] # l'ordre optimale des villes.
#' results['dist_opt'] # la distance totale optimale.
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
    subSets = .get_subsets(2:n,s)
    for (S in subSets){
      C[nrow(C) + 1,] = rep(Inf,n)
      rownames(C)[nrow(C)]=paste(unlist(S), collapse='')
      pr[nrow(pr) + 1,] = rep(Inf,n)
      rownames(pr)[nrow(pr)]=paste(unlist(S), collapse='')
      for (k in S){
        C_S_k = .construct_C_S_k(C,S,k,G)
        tmp = .search_min_C_S_k(C_S_k)
        C[nrow(C),k] = tmp$c_s_k
        pr[nrow(pr),k] = tmp$m
      }
    }
  }
  C_S_1    = .construct_C_S_k(C,c(2:n),1,G)
  min_tmp  = .search_min_C_S_k(C_S_1)
  dist_opt = min_tmp$c_s_k
  path_opt = rep(0,(n-1))
  path_opt[1] = min_tmp$m
  subset_opt = c(2:n)
  for (k in 2:(n-1)){
    row_S_k = paste(unlist(subset_opt), collapse='')
    path_opt[k] = pr[row_S_k,path_opt[k-1]]
    subset_opt = subset_opt[subset_opt!=path_opt[k-1]]
  }
  return(list(dist_opt=dist_opt,path_opt=path_opt))
}


#' @name  .generateDistances
#' @title  (Fonction cachée) Génère une matrice de distance aléatoirement.
#' @param n : le nombre des villes.
#' @param minDistance : le min des distance à ne pas dépasser.
#' @param maxDistance : le max des distance à ne pas dépasser.
#' @return Une matrice des distances.
.generateDistances <- function(n,minDistance,maxDistance){
  G = matrix( rep( 0, len=n*n), nrow = n)
  for(i in 1:(n-1)){
    for(j in (i+1):n){
      G[i,j]= sample(minDistance:maxDistance, 1)
      G[j,i]= G[i,j]
    }
  }
  return(G)
}

#' @name  test_compare 
#' @title Comparaison entre la méthode naïve et Held-Karp.
#'        Affiche les chemins optimaux obtenus et les distances associées.  
#' @param n : le nombre des villes.
#' @param minDistance : le min des distance à ne pas dépasser.
#' @param maxDistance : le max des distance à ne pas dépasser.
#' @usage test_compare(n,minDistance,maxDistance)
#' @return Affichage des résultats.
test_compare <- function(n,minDistance,maxDistance){
  G = .generateDistances(n,minDistance,maxDistance)
  print('-----------------')
  print('held_karp')
  hp = held_karp(G,n)
  print(hp$path_opt)
  print('-----------------')
  print('Test de toutes les permutations ')
  nm = naive_method(G,c(2:n),1)
  
  print(nm$path_optimal)
  print('-----------------')
  print(paste('distance optimal avec Held_karp ',compute_distance(G,hp$path_opt,1)))
  print(paste('distance optimal avec tous les tests ',compute_distance(G,nm$path_opt,1)))
}


#' @name  .generate_x_y_G
#' @title  (Fonction cachée) Génère une matrice des distance avec  et les coordonnées des villes aléatoirement.
#' @param n : le nombre des villes.
#' @return Une matrice des distances et les coordonnées des villes.
.generate_x_y_G <- function(n){
  x = runif(n,min = 10, max = 100)
  y = runif(n,min = 10, max = 100)
  cities = list()
  for (i in 1:n){
    cities[[i]] = c(x[i],y[i])
  }
  G = matrix(0,nrow=n,ncol=n)
  for (i in 1:n){
    for (j in 1:n){
      dist_x = (x[i]-x[j])^2
      dist_y = (y[i]-y[j])^2
      G[i,j]= sqrt(dist_x + dist_y)
    }
  }
  return(list(cities=cities,G=G))
}

#' @name  test_compare_plot 
#' @title Comparaison entre la méthode naïve et Held-Karp.
#' #      Affiche les chemins optimaux obtenus et les distances associées.  
#' @param n : le nombre des villes.
#' @usage test_compare_plot(n)
#' @return Affichage des résultats et un plot des villes et 
#' #       retourne les données utilisée (matrice G et les coordonnées des villes).  
test_compare_plot <- function(n){
  data = .generate_x_y_G(n)
  G = data$G
  cities = data$cities
  df = as.data.frame(matrix(unlist(cities),nrow=length(cities),byrow=T))
  print('-----------------')
  print('held_karp')
  hp = held_karp(G,n)
  print(hp$path_opt)
  print('-----------------')
  print('Test de toutes les permutations ')
  nm = naive_method(G,c(2:n),1)
  print(nm$path_optimal)
  print('-----------------')
  print(paste('distance optimal avec Held_karp ',compute_distance(G,hp$path_opt,1)))
  print(paste('distance optimal avec tous les tests ',compute_distance(G,nm$path_opt,1)))
  
  df_tmp = df[c(1,hp$path_opt,1),] # réordonner les coordonnées des villes.
  plot(df_tmp$V1,df_tmp$V2,type='c',main="L'enchainement optimale des villes",
       xlab="X", ylab="Y", 
       xlim=c(10, 100), ylim=c(10, 100))
  points(df$V1,df$V2 , cex=2)
  text(df$V1,df$V2, labels = rownames(df),col='red' ,cex = 1)
}


#' @name  time_compare 
#' @title Comparaison entre la méthode naïve et Held-Karp.
#' #      Affiche un plot du temps pris par les algorithmes en fonction de nombre des villes.  
#' @param min_cities : le nombre minimale des villes à ne pas dépasser.
#' @param max_cities : le nombre maximale des villes à ne pas dépasser.
#' @usage time_compare(min_cities,max_cities)
#' @return Affichage des résultats et un plot du temps pris par les algorithmes en fonction de nombre des villes et
#' #       retourne les données utilisées (matrice G et les coordonnées des villes).  
time_compare <- function(min_cities,max_cities){
  n = c(min_cities:max_cities)
  start_city = 1
  held_karp_time = c()
  naive_method_time = c()
  for (nb_cities in n){
    cities = c(2:nb_cities)
    G = .generateDistances(nb_cities,10,100)
    ptm <- proc.time()
    hp = held_karp(G,nb_cities)
    time_hp = proc.time()-ptm
    held_karp_time = c(held_karp_time,time_hp['elapsed'])
    
    ptm <- proc.time()
    nm = naive_method(G,cities,start_city)
    time_nv = proc.time()-ptm
    naive_method_time = c(naive_method_time,time_nv['elapsed'])
  }
  df = as.data.frame(cbind(held_karp_time,naive_method_time,n))
  p <- ggplot2::ggplot(df) +
    ggplot2::geom_line(ggplot2::aes(n,naive_method_time , colour="green"))+
    ggplot2::geom_line(ggplot2::aes(n,held_karp_time, colour="red"))+  
    ggplot2::scale_color_discrete(name = "Algorithmes", labels = c("Naive", "Held-Karp")) + 
    ggplot2::xlab("Nombre de villes") + ggplot2::ylab("Temps en (seconds)")
  print(p)
  
  return(list(naive_method = naive_method_time , held_karp= held_karp_time))
}


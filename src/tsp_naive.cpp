#include <Rcpp.h>
using namespace Rcpp;

//' @name compute_distance_Rcpp
//' @title calcul de distance selon un ordre de villes.
//' @param G : la matrice des distances.
//' @param cities : vecteur des villes à visiter.
//' @param start_city : la ville de début du voyage.
//' @usage compute_distance_Rcpp(G,cities,start_city)
//' @return la distance totale parcourue
//' @examples 
//' G = matrix(runif(4*4,min=10,max=50),nrow=4,ncol=4)
//' G = G %*% t(G) # rendre G symétrique
//' diag(G) = 0 # annuler la diagonale de G
//' ## la symétrie de G n'est pas nécessaire.
//' start_city = 1 # la ville de Départ.
//' cities = c(2,3,4) # un ordre de ville à visiter.
// [[Rcpp::export]] //mandatory to export the function
double compute_distance_Rcpp(NumericMatrix G,
                             NumericVector cities,
                             unsigned int start_city)
{
  
  int n = cities.size();
  double distance = G(start_city,cities[0]);
  for ( int i =0 ; i<n-1 ; i = i+1){
    distance = distance + G(cities[i],cities[i+1]);
  }
  return(distance + G(cities[n-1],start_city));
}

//' @name  naive_method_Rcpp
//' @title Test de toutes les permutations possibles.
//' @param G : la matrice des distances.
//' @param cities : vecteur des villes à visiter.
//' @param start_city : la ville de début du voyage.
//' @usage naive_method_Rcpp(G,cities,start_city)
//' @return la séquence des villes qui donnent la moindre en distance.
//' @examples 
//' G = matrix(runif(4*4 , min = 10, max = 50),nrow=4)
//' G = G %*% t(G) # rendre G symétrique (n'est pas nécessaire.)
//' diag(G) = 0 # annuler la diagonale de G
//' ## la symétrie de G n'est pas nécessaire.
//' start_city = 1 # la ville de Départ.
//' cities = c(2,3,4)  # un ordre de ville à visiter.
//' results = naive_method(G,cities,start_city)
//' results['path_optimal'] # l'ordre des villes optimales
//' results['dist_optimal'] # la distance obtenue avec l'ordre des villes optimales.
// [[Rcpp::export]] //mandatory to export the function
List naive_method_Rcpp(NumericMatrix G,
                  NumericVector cities,
                  unsigned int start_city){
  int n = cities.size();
  NumericVector c(n);
  int i = 0;
  double dist_optimal = compute_distance_Rcpp(G,cities,start_city);
  NumericVector path_optimal = cities;
  while( i < n){
    if (c[i] < i){
      if (i %2 == 0){
        int tmp = cities[0];
        cities[0] = cities[i];
        cities[i] = tmp;
      }
      else {
        int tmp = cities[c[i]];
        cities[c[i]] = cities[i];
        cities[i] = tmp;
      }
      double tmp_dist = compute_distance_Rcpp(G,cities,start_city);
      if ( tmp_dist < dist_optimal){
        path_optimal = cities;
        dist_optimal = compute_distance_Rcpp(G,cities,start_city);
      }
      c[i] = c[i] + 1 ;
      i = 0;
    }
    else {
      c[i] = 0;
      i = i+1;
    }
  }
  List L = List::create(Named("path_optimal")=path_optimal,Named("dist_optimal")=dist_optimal);
  return(L);
}


//' @name get_subsets_Rcpp
//' @title  (Fonction cachée) Génère toutes les sous partitions de taille p d'un groupe de taille n.
//' @param NumericVector set : le groupe initial.
//' @param int p : la taille des partitions qu'on veut extraire de 'set'.
//' @return List : L'ensemble des partitions de taille p du groupe 'set'.
//' @usage compute_distance_Rcpp(G,cities,start_city)
//' @examples 
//' set = c(1,2,3,4,5,6,7)
//' p = 2
//' subsets = get_subsets_Rcpp(set,p) # on obtient tous les sous-groupes possibles de taille 2 du vecteur c(1,2,3,4,5,6,7)
List get_subsets_Rcpp(NumericVector set, int p){
  List subsets;
  int n = set.size();
  for (int i= 0; i < pow(2,n); i++ ){
    int t = i;
    NumericVector tmp ;
    for (int j=0; j < n;j++ ){
      if ( t & 1){
        tmp.push_back(set[j]);
      }
      t >>= 1;
    }
    if (tmp.size()==p){
      subsets.push_back(tmp);
    }
  }
  return(subsets);
}

//' @name to_String
//' @title (Fonction cachée) Convertir un vecteur en une chaîne de caractères.
//' @param NumericVector v: le vecteur à convertir.
//' @usage to_String(v)
//' @return la chaîne de caractères associée au vecteur.
//' @examples 
//' v = c(1,2,3,4,5,6,7)
//' v_string = to_String(v)
//' print(v_string) # '1234567'
String to_String(NumericVector v){
  String s("");
  int n = v.size();
  for (int i=0;i<n;i=i+1){
    s.push_back(v[i]);
  }
  return(s);
}


//' @name construct_C_S_k_Rcpp
//' @title (Fonction cachée) Construire le vecteur (la liste \[C(S\{k\}, m) + G\[m,k\]\] pour tout m dans 'Subset')
//' @param List C : la matrice tel que C\[S,k\] le coût min du chemin à partir de 1 et se termine au 
//' #          sommet k, passant les sommets de l'ensemble S exactement une fois.
//' @param NumericVector Subset : Un sous groupe du groupe complet des villes.
//' @param int k : La ville pour laquelle on veut calculer C\[S-k,m\] pour tout m dans Subset.
//' @param NumericMatrix G : la matrice des distances.
//' @usage construct_C_S_k_Rcpp(C,Subset,k,G)
//' @return (la liste \[C(S\{k\}, m) + G\[m,k\]\] pour tout m dans 'Subset')
List construct_C_S_k_Rcpp(List C, NumericVector Subset,int k,NumericMatrix G){
  List C_S_k ;
  NumericVector S_k ;
  int n =Subset.size();
  for(int i=0; i < n ; i++){
    if( Subset[i] != k){
      S_k.push_back(Subset[i]);
    }
  }
  String row_S_k = to_String(S_k);
  for (int i=0; i < n ; i++){
    if ( Subset[i] != k){
      NumericVector tmp = C[row_S_k];
      NumericVector v(1);
      v.fill(Subset[i]);
      C_S_k.push_back( tmp[Subset[i]] + G(Subset[i],k),to_String(v));
    }
  }
  return(C_S_k);
}


//' @name  search_min_C_S_k_Rcpp
//' @title (Fonction cachée) recherche du min dans une liste et son index.
//' @param List C_s_k : la liste construite par la fonction construct_C_S_k_Rcpp.
//' @usage search_min_C_S_k_Rcpp(C_s_k)
//' @return Le minimum de la liste et l'index du minimum.
List search_min_C_S_k_Rcpp(List C_S_k){
  CharacterVector names = C_S_k.names();
  String m_0 = names[0];
  int n = C_S_k.size();
  for(int i = 0 ;i < n; i =i+1){
    String m = names[i];
    double x = C_S_k[m];
    double y = C_S_k[m_0];
    if ( x < y ){
      m_0=m;
    }
  }
  List L = List::create(Named("c_s_k") = C_S_k[m_0], _["m"] = m_0);
  return(L);
}

//' @name  element_index
//' @title (Fonction cachée) l'index d'un element dans un vecteur. 
//' @param NumericVector vec : un vecteur.
//' @param int e : un element.
//' @usage element_index(vec,e)
//' @return Le minimum de la liste et l'index du minimum.
//' @examples 
//' v = c(11,2,31,4,25,6,71)
//' e = 4
//' index = element_index(v,e)
//' print(index) # 3
int element_index(NumericVector vec,int e){
  int n = vec.size();
  NumericVector results(n-1);
  for(int i=0;i<n ;i=i+1){
    if (vec[i] == e){
      return(i);
    }
  }
  return(-1);
}


//' @name  held_karp_Rcpp
//' @title L'algorithme de Held_karp.
//' @param NumericMatrix G : la matrice des distances.
//' @param int n : le nombre de villes.
//' @usage held_karp_Rcpp(G,n)
//' @return La séquence des villes qui donnent la moindre en distance et la distance optimale.
//' @examples 
//' G = matrix(runif(4*4 , min = 10, max = 50),nrow=4)
//' G = G %*% t(G) # rendre G symétrique (n'est pas nécessaire.).
//' diag(G) = 0 # annuler la diagonale de G.
//' n = 4 # nombre de villes.
//' results = held_karp(G,n)
//' results['path_opt'] # l'ordre optimale des villes.
//' results['dist_opt'] # la distance totale optimale.
// [[Rcpp::export]]
List held_karp_Rcpp(NumericMatrix G, int n){
  int a = std::numeric_limits<int>::max();
  List C ;
  List pr;
  NumericVector cities(n-1);
  for (int p =0;p <n-1;p =p+1){
    cities[p] = p+1 ;
  }
  for (int k=1; k<n; k++){
    NumericVector tmp(n);
    tmp.fill(R_PosInf);
    tmp[k] = G(0,k);
    NumericVector v(1);
    v[0] = k;
    String s = to_String(v);
    C.push_back(tmp ,s);
  }
  for(int s=2 ; s < n ; s=s+1){
    List subSets = get_subsets_Rcpp(cities,s);
    int n_subsets = subSets.size();
    for(int y = 0; y < n_subsets; y=y+1){
      NumericVector S = subSets[y];
      String name_S = to_String(S);
      NumericVector tmp_1(n);
      tmp_1.fill(R_PosInf);
      NumericVector tmp_2(n);
      tmp_2.fill(R_PosInf);
      int n_s = S.size();
      for (int k=0 ; k<n_s ; k=k+1){
        double k_S= S[k];
        List C_S_k = construct_C_S_k_Rcpp(C,S,k_S,G);
        List tmp = search_min_C_S_k_Rcpp(C_S_k);
        tmp_1[k_S] = tmp["c_s_k"];
        char c = tmp["m"];
        tmp_2[k_S] = c - '0';
      }
      C.push_back(tmp_1 , name_S);
      pr.push_back(tmp_2 , name_S);
    }
  }
  List C_S_1  = construct_C_S_k_Rcpp(C,cities,0,G);
  List min_tmp  = search_min_C_S_k_Rcpp(C_S_1);
  char c = min_tmp["m"];
  double dist_opt = min_tmp["c_s_k"];
  NumericVector path_opt(n-1);
  path_opt[0]= c - '0';
  NumericVector subset_opt = cities;
  for (int k=1;k<n-1;k=k+1){
    String row_S_k = to_String(subset_opt);
    NumericVector tmp_pr = pr[row_S_k];
    path_opt[k] = tmp_pr[path_opt[k-1]] ;
    int i = element_index(subset_opt,path_opt[k-1]);
    subset_opt.erase(i);
  }
  List L = List::create(Named("path_opt") = path_opt, _["dist_opt"] = dist_opt);
  return(L);
}




/*** R
n = 7
a = matrix(runif(n*n,10,100),nrow=n,ncol=n)
a = a %*% t(a)
diag(a)=0
held_karp(a,n) # R 
print('-------------------------------------')
held_karp_Rcpp(a,n) # Rcpp
print('-------------------------------------')
naive_method(a,c(2:n),1) # R
*/


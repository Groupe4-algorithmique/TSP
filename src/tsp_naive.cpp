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
//' distance = compute_distance_Rcpp(G,cities,start_city)
// [[Rcpp::export]] //mandatory to export the function
double compute_distance_Rcpp(NumericMatrix G,
                             NumericVector cities,
                             unsigned int start_city)
{
  
  int n = cities.size(); // Nombre des villes.
  double distance = G(start_city,cities[0]);// la distance entre la ville de début et la ville qui suit.
  for ( int i =0 ; i<n-1 ; i = i+1){
    // somme des distances des villes successifs.
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
//' results = naive_method_Rcpp(G,cities,start_city)
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

/*
//' @name get_subsets_Rcpp
//' @title Créer les sous-groupes d'un groupe.
//' @param set : la matrice des distances.
//' @param p : vecteur des villes à visiter.
//' @usage get_subsets_Rcpp(set,p)
//' @return la liste des sous-groupes de taille p dans le groupe initial 'set'.
 */
// [[Rcpp::export(.get_subsets_Rcpp)]] //Fonction cachée
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


/*
 //' @name to_String
 //' @title Convertir un vecteur numérique vers une chaîne de caractères.
 //' @param p : Le vecteur à convertir.
 //' @usage to_String(v)
 //' @return la chaîne de caractères associée au vecteur d'entrée.
 */
// [[Rcpp::export(.to_String)]] //Fonction cachée
String to_String(NumericVector v){
  String s("");
  int n = v.size();
  for (int i=0;i<n;i=i+1){
    s.push_back(v[i]);
  }
  return(s);
}

/*
 //' @name construct_C_S_k 
 //' @title Construire le vecteur (la liste [C(S{k}, m) + G[m,k]] pour tout m dans 'Subset')
 //' @param C : la matrice tel que C[S,k] le coût min du chemin à partir de 1 et se termine au 
 //' #          sommet k, passant les sommets de l'ensemble S exactement une fois.
 //' @param Subset : Un sous groupe du groupe complet des villes.
 //' @param k : La ville pour laquelle on veut calculer C[S-k,m] pour tout m dans Subset.
 //' @param G : la matrice des distances.
 //' @usage construct_C_S_k_Rcpp(C,Subset,k,G)
 //' @return (la liste [C(S{k}, m) + G[m,k]] pour tout m dans 'Subset').
 */
// [[Rcpp::export(.construct_C_S_k_Rcpp)]] //Fonction cachée
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

/*
  //' @name  search_min_C_S_k_Rcpp 
  //' @title recherche du min dans un vecteur et son index.
  //' @param C_S_k : Une liste construit par la fonction 'construct_C_S_k_Rcpp'.
  //' @usage search_min_C_S_k_Rcpp(C_S_k)
  //' @return Le minimum du vecteur et l'index du minimum.
*/
// [[Rcpp::export(.search_min_C_S_k_Rcpp)]] //Fonction cachée
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

/*
 //' @name  delete_element 
 //' @title supprimer un élément dans un vecteur
 //' @param vec : Vecteur numérique.
 //' @param e : un élément de même type que les éléments de 'vec'.
 //' @usage delete_element(vec,e)
 //' @return Un nouveau vecteur sans l'élément 'el'.
 */
// [[Rcpp::export(delete_element)]] //Fonction cachée
NumericVector delete_element(NumericVector vec,int el){
  int n = vec.size();
  NumericVector results(0);
  for(int i=0;i<n ;i=i+1){
    if (vec[i] != el){
      results.push_back(vec[i]);
    }
  }
  return(results);
}


/*
 //' @name  str_to_int 
 //' @title recherche du min dans un vecteur et son index.
 //' @param numStr : Une chaîne de caractères (String).
 //' @usage str_to_int(numStr)
 //' @return L'entier équivalent au String. 
 */
// [[Rcpp::export(.str_to_int)]] //Fonction cachée
int str_to_int(String numStr){
  std::string s = numStr.get_cstring(); 
  int nlen = s.size();
  int results = 0;
  for(int i=0 ; i<nlen ; i = i +1){
    int sm = s[i] -'0';
    results = results + sm*pow(10,nlen-i-1);
  }
  return results;
}

//' @name  held_karp_Rcpp
//' @title L'algorithme de Held_karp.
//' @param G : la matrice des distances.
//' @param n : le nombre de villes.
//' @usage held_karp_Rcpp(G,n)
//' @return La séquence des villes qui donnent la moindre en distance et la distance optimale.
//' @examples 
//' G = matrix(runif(4*4 , min = 10, max = 50),nrow=4)
//' G = G %*% t(G) # rendre G symétrique (n'est pas nécessaire.).
//' diag(G) = 0 # annuler la diagonale de G.
//' n = 4 # nombre de villes.
//' results = held_karp_Rcpp(G,n)
//' results['path_opt'] # l'ordre optimale des villes.
//' results['dist_opt'] # la distance totale optimale.
// [[Rcpp::export]]
List held_karp_Rcpp(NumericMatrix G, int n){
  List C ;
  List pr;
  NumericVector cities(n-1);
  for (int p  = 0; p < n-1 ; p =p+1){
    cities[p] = p+1 ;
  }
  NumericVector v(1);
  for (int k=1; k < n ; k++){
    NumericVector tmp(n , R_PosInf);
    tmp[k] = G(0,k);
    v[0] = k;
    String s = to_String(v);
    C.push_back(tmp ,s);
  }
  for(int s = 2 ; s < n ; s=s+1){
    List subSets = get_subsets_Rcpp(cities,s);
    for(int y = 0; y < subSets.size(); y=y+1){
      NumericVector S = subSets[y];
      String name_S = to_String(subSets[y]);
      NumericVector tmp_1(n , R_PosInf);
      NumericVector tmp_2(n , R_PosInf);
      for (int k=0 ; k < S.size() ; k=k+1){
        int k_S= S[k];
        List C_S_k = construct_C_S_k_Rcpp(C,S,k_S,G);
        List tmp = search_min_C_S_k_Rcpp(C_S_k);
        //Rcpp::print(tmp);
        tmp_1[k_S] =  tmp["c_s_k"];
        String ss = tmp["m"];
        tmp_2[k_S] = str_to_int(ss);
      }
      C.push_back(tmp_1 , name_S);
      pr.push_back(tmp_2 , name_S);
    }
  }
  List C_S_1  = construct_C_S_k_Rcpp(C,cities,0,G);
  List min_tmp  = search_min_C_S_k_Rcpp(C_S_1);
  double dist_opt = min_tmp["c_s_k"];
  NumericVector path_opt(n-1);
  path_opt[0] = str_to_int(min_tmp["m"]); //
  for (int k=1; k<n-1; k=k+1){
    String row_S_k = to_String(cities);
    NumericVector tmp_pr = pr[row_S_k];
    path_opt[k] = tmp_pr[path_opt[k-1]];
    cities = delete_element(cities,path_opt[k-1]);
  }
  List L = List::create(Named("path_opt") = path_opt, _["dist_opt"] = dist_opt);
  return(L);
}




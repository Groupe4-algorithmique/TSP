# Installer le package avec : 
```R
devtools::install_github("Groupe4-algorithmique/TSP")
```
# Importer le package avec :
```R
library("tspp")
```

# Problème du voyageur de commerce :
#### Le dossier R, contient Un script R pour la méthode naïve et la méthode performante pour l'algorithme Held-Karp.
#### 
#### La méthode naïve consiste à tester toutes les permutations possibles des villes et garder la permutation qui donne le chemin le moins chère pour le voyageur, cette méthode à une complexité de O(n!).
#### Cette méthode consiste à commencer par une première permutation initiale aléatoirement et on génère une nouvelle permutation des villes qu'on utilise pour calculer la distance parcourue avec elle et on la compare avec la distance fournie par la permutation précédente, et on garde la permutation qui donne la distance minimale.
#### On refait le processus jusqu'à l'exploration de toutes les permutations.
#### function Naive_Methode(G, n):
    Permutation_0 = une permutation initiale.
    Distance_0 = Distance obtenue pour Permutation_0. 
    for new_permutation in permutations do :
        new_Distance = Distance obtenue pour new_permutation.
        if ( new_Distance < Distance_0) do :
            Distance_0 := new_Distance
            Permutation_0 := new_permutation
        end if
    end for
    return(Permutation_0,Distance_0)  
#### end function 

La méthode de Held-Karp est décrite sur https://en.wikipedia.org/wiki/Held%E2%80%93Karp_algorithm, sa complexité est de<img src="https://render.githubusercontent.com/render/math?math= O(2^n n^2)">.

On note les villes 1, 2,. . .,n et supposons que nous commençons à la ville 1, et on note G la matrice des distances entre les villes, Alors la distance entre la ville i et la ville j est <img src="https://render.githubusercontent.com/render/math?math= G_{i,j}"> .

Considérons les sous-groupes <img src="https://render.githubusercontent.com/render/math?math= S \subseteq \{2,..,n}"> , et pour k ∈ S, soit C(S,k) la distance minimale commençant à la ville 1 en visitant toutes les villes de S et se terminant à la ville k . 

##### Première phase: Si S = {k}, alors C(S,k) = G[1,k]. 
#####                 Sinon: C(S,k)= min x∈{S-k} ( C( S-k,x) + G[x,k] ). (S-k i.e l'ensemble S sauf la ville k)

##### Deuxième phase: la distance minimale pour un tour complet de toutes les villes est M = min k∈{2,..., N}( C({2,...,n},k) + G[k,1] ). 

#### function Held_Karp(G, n):
    for k := 2 to n do 
      C({k}, k) := G[1, k] 
    end for 
    for s := 2 to n−1 do 
        Subsets := get_subsets({2, . . . , n},s) /*recuperer tous les sous-groupes de taille s du groupe {2,. .,n} */
        for S in Subsets do 
          for k in S do 
              C(S, k) := min_{m≠k,m∈S} [C(S\{k}, m) + G[m,k] ]
          end for 
        end for 
    end for 
    opt := min_{k≠1} [C({2, 3, . . . , n}, k) + G[k, 1] ] 
    return (opt) 
#### end function 

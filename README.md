# Installer le package avec : 
```R
devtools::install_github("Groupe4-algorithmique/TSP")
```
# Importer le package avec :
```R
library("tspp")
```


![image](https://drive.google.com/uc?export=view&id=1AWaFDHf5BXQOk7-zvoPaM-5ikvCYf0pg)
```R
function Naive_Methode(G, n):
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
end function 
```

![image](https://drive.google.com/uc?export=view&id=1XmNh7Sz7Jf0X7x19JKtBGu_j6UVtAI5G)

```R
function Held_Karp(G, n):
    for k := 2 to n do 
      C({k}, k) := G[1, k] 
    end for 
    for s := 2 to n−1 do 
        Subsets := get_subsets({2, . . . , n},s) /*récupérer tous les sous-groupes de taille s du groupe {2,. .,n} */
        for S in Subsets do 
          for k in S do 
              C(S, k) := min_{m≠k,m∈S} [C(S\{k}, m) + G[m,k] ]
          end for 
        end for 
    end for 
    opt := min_{k≠1} [C({2, 3, . . . , n}, k) + G[k, 1] ] 
    return (opt) 
 end function 
```
![image](https://drive.google.com/uc?export=view&id=146mzXEs1PokHImDgDCpSOnZVqiqi9eoO)


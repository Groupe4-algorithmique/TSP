# Problème du voyageur de commerce :
#### Dossier R, contient Un script R pour la méthode naïve et la méthode performante pour l'algorithme Held-Karp.
#### La méthode naïve consiste à tester toutes les permutations possibles des villes et garder la permutation qui donne le chemin le moins chère pour le voyageur, cette méthode à une complexité de O(n!).

#### La méthode de Held-Karp est décrite sur https://en.wikipedia.org/wiki/Held%E2%80%93Karp_algorithm, sa complexité est de O(2^n n^2).

## function algorithm TSP (G, n):
    for k := 2 to n do 
      C({k}, k) := d1,k 
    end for 
    for s := 2 to n−1 do 
        for all S ⊆ {2, . . . , n}, |S| = s do 
          for all k ∈ S do 
              C(S, k) := minm≠k,m∈S [C(S\{k}, m) + dm,k] 
          end for 
        end for 
    end for 
    opt := min_{k≠1} [C({2, 3, . . . , n}, k) + dk, 1] 
    return (opt) 
## end function 

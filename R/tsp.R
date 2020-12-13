compute_distance <- function(G,cities,start_city){
  n = length(cities)
  distance = G[start_city,cities[1]]
  for (i in 1:(n-1)){
    distance = distance + G[cities[i],cities[i+1]]
  }
  return(distance + G[cities[n],start_city])
}

permutations <- function(G,cities,start_city){
  n = length(cities)
  c = rep(1,n)
  i = 1
  dist_optimal = compute_distance(G,cities,start_city)
  cities_optimal = cities
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

            cities_optimal = cities
            dist_optimal = compute_distance(G,cities_optimal,start_city)
        }
        c[i] = c[i] + 1
        i = 1
    }
    else {
        c[i] = 1
        i = i+1
    }
  }
  return(list(cities_optimal=cities_optimal,dist_optimal=dist_optimal))
}
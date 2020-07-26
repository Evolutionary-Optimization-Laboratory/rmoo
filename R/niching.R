

niching <- function(pop, n_remaining, niche_count, niche_of_individuals, dist_to_niche){
  survivors <- c()
  #Repetir poblacion boolean array of elements that are considered for each iteration
  mask <- rep(TRUE, nrow(pop))
  #Numero de individuos a seleccionar en la iteración
  while (length(survivors)<n_remaining) {

    n_select <- n_remaining - length(survivors)

    next_niches_list <- unique(niche_of_individuals[mask])
    next_niche_count <- niche_count[next_niches_list]


    min_niche_count <- min(next_niche_count)

    #Traemos todos los nichos con el recuento minimo
    next_niches <- next_niches_list[which(next_niche_count == min_niche_count)]
    next_niches <- next_niches[sample(length(next_niches))[n_select]]

    for (i in next_niches) {
      next_ind <- which(((niche_of_individuals == next_niches[i]) == mask))

      if (length(next_ind)>1) {
          next_ind <- sample(next_ind)
      }

      if (niche_count[next_niche] == 0) {
          next_ind <- next_ind[np.argmin(dist_to_niche[next_ind])]
      }else{
        #Ya randomizado
        next_ind = next_ind[0]
      }
      mask[next_ind] = F

      survivors <- c(survivors,next_ind)

      niche_count[i] = niche_count[i]+1

    }

  }
  return(survivors)

}

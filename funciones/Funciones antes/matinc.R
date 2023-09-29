matinc <- function(pol, tol=5){
  aux <- pol
  aux1 <- st_intersection(aux,aux) %>% 
    filter(man!=man.1) %>% 
    mutate(largo=as.numeric(st_length(.))) %>% 
    filter(largo > tol)
  
  o <- table(select(as.data.frame(aux1),man,man.1))
  q <- (o > 0) + 0
  return(q)
}
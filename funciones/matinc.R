matinc <- function(pol, tol=5){
  aux <- pol
  aux1 <- st_intersection(aux,aux) %>%
    group_by(man) %>% 
    mutate(n = n()) %>% 
    ungroup() %>% 
    filter(!(man==man.1 & n > 1)) %>% 
    mutate(largo=as.numeric(st_length(.)),
           frecuencia = 1) %>% 
    filter(largo > 5 | largo == 0) %>%
    as.data.frame() %>% 
    select(man, man.1, frecuencia)
  
  aux1=aux1[!duplicated(aux1), ]
  
  o <- aux1 %>% 
    arrange(man.1) %>% 
    pivot_wider(names_from = man.1, values_from = frecuencia) %>% 
    arrange(man) %>% 
    select(man, .$man) %>% 
    as.data.frame()
  
  q <- data.matrix(select(o, -man)) %>% 
    replace(is.na(.), 0)
  
  rownames(q) <- colnames(q)
  
  q <- q[pol$man, pol$man]
  
  
  
  return(q)
}
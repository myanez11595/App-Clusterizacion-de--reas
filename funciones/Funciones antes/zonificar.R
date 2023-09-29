zonificar <- function(conglomerados,l){
  # Número de veces a aplicar kerlin
  m1=100
  ##########################################
  conglomerados_centroides <- conglomerados %>% 
    st_centroid(.)
  
  x <- as.numeric(st_coordinates(conglomerados_centroides)[,1])
  y <- as.numeric(st_coordinates(conglomerados_centroides)[,2])
  
  bdd <- as.data.frame(cbind(id=conglomerados_centroides$cong,x,y)) %>% 
    mutate(x=as.numeric(x),
           y=as.numeric(y))
  
  k <- ceiling(dim(bdd)[1]/l)
  
  C <- matdis(bdd$x,bdd$y)
  n <- dim(bdd)[1]
  
  li <- rep(list(n),m1)
  s <- lapply(li, solale,k)
  sr <- lapply(s, ref,C)
  o <- sapply(sr,funobj,C)
  m <- min((1:m1)[o==min(o)])
  sr <- sr[[m]]
  apoyo <- data.frame(man=as.vector(t(sr)),
                      equipo=as.character(sort(rep(1:k,dim(sr)[2])))) %>% 
    filter(man!=0) %>% 
    arrange(man) %>% 
    select(-man) %>% 
    cbind(bdd)
}
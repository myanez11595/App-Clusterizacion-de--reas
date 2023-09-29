extpol <- function(pol,per){
  source("rutinas/funciones/ExtPol/caja.R")
  #Extraemos los puntos del shape
  quntos <- pol %>% 
    st_as_sf() %>% 
    st_simplify(dTolerance = 0.9) %>% 
    mutate(area = as.numeric(st_area(.))) %>% 
    filter(area > 0) %>% 
    st_cast("MULTIPOINT") %>% 
    st_cast("POINT")
  puntos <- st_union(quntos)
  st_crs(puntos) <- st_crs(pol)
  st_crs(quntos) <- st_crs(pol)
  #Calculamos los polígonos de voronoi
  caj <- caja(puntos)
  voronoi <- st_voronoi(puntos,caj) %>% 
    st_cast() %>% 
    st_as_sf()
  voronoi <- voronoi %>% 
    # data.frame(geometry = .) %>% 
    # st_sf(.) %>% 
    st_join(.,quntos, join = st_contains) %>% 
    #calculo de área
    mutate(area = as.numeric(st_area(.))) 
  #Corrección voronoi
  # for (i in 1:(dim(voronoi)[1])){
  #   if(is.na(voronoi$man[i])){
  #     voronoi$man[i] <- voronoi$man[i-1]
  #   }
  # }
  vorpun <- voronoi
  # disolver por manzana
  disolver <- voronoi %>%
    st_make_valid() %>%
    group_by(man) %>% 
    summarise(np=n()) %>% 
    st_cast() %>% 
    filter(!is.na(man))
  st_crs(disolver) <- st_crs(pol)
  # se intersecta con el perfil
  polext <- st_intersection(disolver,perfil) %>% 
    group_by(man) %>% 
    summarise(partes=mean(np)) %>% 
    st_cast()
  return(polext)
}
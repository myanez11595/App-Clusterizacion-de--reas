insumos <- function(sec_man,ca_04,rio,sede,psf_1,pif_1){
 #SEDES_A=  read_sf("C:/Users/HP/Documents/Marlon/Litoral-20220713T200542Z-001/Litoral/sede.shp")
  #st_geometry(SEDES_A)="geometry"
  #SECTOR_MANZANA_A=  read_sf("C:/Users/HP/Documents/Marlon/Litoral-20220713T200542Z-001/Litoral/manzana_Sector.shp")
  #st_geometry(SECTOR_MANZANA_A)="geometry"
  #RIOS_A=  read_sf("C:/Users/HP/Documents/Marlon/Litoral-20220713T200542Z-001/Litoral/rio_a.shp")
  #st_geometry(RIOS_A)="geometry"
  #Ejes=  read_sf("D:/zona centro prueba/ejes.shp")
  #st_geometry(Ejes)="geometry"
  #CA04_A=  read_sf("C:/Users/HP/Documents/Marlon/Litoral-20220713T200542Z-001/Litoral/ca04_a.shp")
  #st_geometry(CA04_A)="geometry"
  #ADICIONAL_L=  read_sf("D:/zona centro prueba/rios_l.shp")
  #st_geometry(ADICIONAL_L)="geometry"
  #sec_man$priorizado[sec_man$priorizado!="alto"]=NA
  
  #sec_man=SECTOR_MANZANA_A
  #ca_04=CA04_A
  #rio=RIOS_A
  #sede=SEDES_A
    #ca_04_2 <- aggregate(n_viv ~ man, ca_04, sum) 
    #sec_man =left_join(sec_man,ca_04_2, by="man")

   # colnames(sec_man)[19]="V_PART2022"
  #sec_man$priorizado="baja"
   # st_write(sec_man,"D:/Entregable/1/MANZANA_SECTOR.gpkg")
  source("funciones/matinc.R") #Calcula matriz de incidencia
  source("funciones/extpol/extpol.R") #Extiente polígonos disjuntos
  
  #Ingrese el código de 7 dígitos de la sede
  
  cs <- "1701023"
  sec_man$priorizado[sec_man$priorizado!="alto"]=NA
  sec_man$priorizado[sec_man$priorizado=="alto"]="alta"
  #Cargamos el shape sector_manzana
  mansec <- sec_man %>% 
    rename_all(tolower) %>% 
    select(man,v_particul = v_part2022, sec, priorizado) %>% 
    mutate(priorizado = ifelse(is.na(priorizado), "no", tolower(priorizado)))
  
  #Cargamos el shape ca04
  ca04 <- ca_04 %>% 
    st_as_sf() %>% 
    st_intersection(mansec %>% st_buffer(-0.2)) %>% 
    st_join(mansec %>% select(man), join = st_covered_by) %>% 
    select(man = man.y)
  
  #Cargamos el shape de rios
  if(nrow(rio)>0){
    rios_a <- rio %>% 
      rename_all(tolower) %>% 
      summarise()
  }else{rios_a=rio}
    
  
  #Cargamos el shape de sede
  sede <- sede%>% 
    rename_all(tolower) %>% 
    select(sede = n_so)
  
  #Filtramos por manzanas
  manzanas <- mansec %>% 
    filter(!is.na(man))
  
  #Filtramos por sectores para determinar el área dispersa
if(nrow(manzanas)!=nrow(mansec)){
  sede_dispersa <- mansec %>% 
    filter(is.na(man)) %>% 
    summarise()
  
}else{
  
}
  
  #Eliminamos el área dispersa para extender las manzanas
  if(exists("sede_dispersa")){
    perfil <- sede %>% 
      st_difference(sede_dispersa %>% st_buffer(0.1)) %>% 
      select(sede)
  }else{perfil=sede}
 
  
  #Se realiza la extensión de manzanas
  manext <- extpol(ca04,perfil) %>% 
    select(-partes) %>% 
    st_as_sf()
 
  #Se realiza el corte de las manzanas extendidas por el shape de rios
  apoyo <- 
    #Diferencia simétrica de manext con rios
      if(nrow(rios_a)>0){st_difference(manext,rios_a)}else{manext} %>% 
    #Validación de geometría
    st_make_valid() %>% 
    #Trasnformación a multipoligono
    st_cast("MULTIPOLYGON") %>% 
    #Transformación a poligono
    st_cast("POLYGON") %>% 
    #Emparejamiento geográfico para identicar pedazos de manzanas a utilizar
    st_join(manzanas) %>% 
    filter(man.x == man.y) %>% 
    #Disolver a nivel de manzana
    group_by(man = man.x) %>% 
    summarise() %>% 
    st_make_valid() %>% 
    st_cast("MULTIPOLYGON")
  
  st_geometry(apoyo)="geom"
  st_geometry(mansec)="geom"
  #names(apoyo)
  #Unión de manzanas extendidas y sectores
  poligonos <- mansec %>% 
    filter(is.na(man)) %>%
    select(man = sec) %>%
    group_by(man) %>% 
    summarise() %>% 
    rbind(apoyo) %>% 
    arrange(man) %>% 
    st_make_valid() %>% 
    st_cast("MULTIPOLYGON")
  
  ##Cálculo de matriz de incidencia
  A <- matinc(poligonos)
  
  #Calculamos las coordenadas de los centroides de las manzanas y sectores
  coordenadas <- mansec %>% 
    st_centroid() %>% 
    st_coordinates() %>% 
    as.data.frame() %>% 
    cbind(man = mansec$man, sector = mansec$sec) %>% 
    mutate(man = ifelse(is.na(man), as.character(sector), as.character(man))) %>% 
    select(man, x_utm = X, y_utm = Y) %>% 
    mutate(x_mean = mean(x_utm), y_mean = mean(y_utm),
           distancia = (x_mean - x_utm)^2 + (y_mean - y_utm)^2,
           x_dir = x_utm[distancia == max(distancia)],
           y_dir = y_utm[distancia == max(distancia)]) %>% 
    select(-x_mean, -y_mean, -distancia) %>% 
    mutate(distancia = (x_dir - x_utm)^2 + (y_dir - y_utm)^2)
  
  
  #Cálculo de vector de pessos
  bdd <- data.frame(man = rownames(A)) %>% 
    left_join(mansec %>% 
                mutate(man=ifelse(is.na(man), sec, man)) %>% 
                as.data.frame() %>% 
                group_by(man, priorizado) %>% 
                summarise(viv = sum(v_particul)) %>% 
                ungroup() %>% 
                mutate(viv = ifelse(substr(man, 7, 9) == "999", ceiling(1.25*viv),viv),
                       masivas = ifelse(viv >= 180, viv, 0.5)), 
              by = "man") %>% 
    left_join(coordenadas, by = "man") %>% 
    arrange(desc(masivas), desc(distancia)) %>% 
    mutate(orden = row_number()) %>% 
    arrange(man) %>% 
    select(man, priorizado, viv, orden)
  
  identical(bdd$man, rownames(A))
  
  source("funciones/sumiderog.R")
  n=bdd$man
  
  n2=rownames(A)
  cs <- "1701023"
  pi <- pif_1
  ps <- psf_1
  
  #Tratamiento de priorizadoad
  prio <- bdd$man[bdd$priorizado == "alta"]
  bdd$viv[is.na(bdd$viv)] <- 0
  peso <- bdd$viv
  
  
  #Se exlcluyen las incidencias entre sectores y manzanas
  A[substr(rownames(A),7,9)=="999", ] <- 0
  A[, substr(colnames(A),7,9)=="999"] <- 0
  
  #Se excluyen las incidencias de las manzanas con priorizadoad 
  A[prio, ] <- 0
  A[, prio] <- 0
  
  #Se quita los nombres de filas y columnas de la matriz de incidencia
  row.names(A) <- NULL
  colnames(A) <- NULL
  
  #Calculamos el número de encuestadores
  
  enc <- ceiling(sum(bdd[, 'viv'], na.rm = TRUE)/15/36)
  class(bdd$viv)
  #Calcula de dias sectores peligrosos 
  
  dias_pe <- round(sum(bdd$viv[(bdd$man %in% prio)])/enc/15)
  
  dias_ce <- 36-dias_pe
  
  #Se fijan los límites de las áreas de empadronamiento
  
  sl <-  ceiling(pi*dias_ce*15) #540*0.945
  su <-  ceiling(ps*dias_ce*15) #540*1.05
  
  #Abrimos la base de pesos de los polígonos y se reescala los pesos de los
  #sectores dispersos
  
  #Inicializamos el vector de áreas de empadronamiento
  #peso[is.na(peso)] <- 0
  ae <- rep(0,length(peso))
  
  lol <- bdd$orden
  
  while(sum(A)>0){
    #Se selecciona el atractor como el que tiene menor número de incidencias
    i <- min((1:length(peso))[lol==min(lol)])
    #Aplicamos la función sumidero
    clus <- sumiderog(i,A,peso,su,sl)
    #Se actualiza el vector ae en función al resultado de la función sumidero
    ae[clus] <- i
    #Se actualiza los valores de la matriz de incidencia aislando los nodos que
    #son parte de alguna ae
    A[clus,] <- 0
    A[,clus] <- 0
    
    lol[clus] <- 10000
    print(sum(A))
  }
  #Añadimos el vector ae a la bdd 
  bdd_01 <- cbind(bdd,ae,num=1:length(ae)) %>%
    #Se calcula el número de viviendas de cada ae formada
    group_by(ae) %>% 
    mutate(viv_ae=sum(viv)) %>% 
    ungroup() %>% 
    #Redefinimos las ae formadas en función del número de viviendas
    mutate(
      # ae = ifelse(viv_ae < sl | viv_ae > su ,num,ae),
      #      #Redefinimos el número de viviendas de cada parte
      #      viv_ae = ifelse(viv_ae < sl | viv_ae > su, viv,viv_ae),
      #Identificamos al nodo sumidero
      sumidero=ifelse(num==ae,1,0),
      #      #calculamos el área como disperso o amanzanado
      #      area = ifelse(substr(man,7,9)=="999",2,1),
      #creamos el identificador de ae
      id_ae = paste0(str_pad(ae,4,"left","0")),
      id_ae = ifelse(priorizado == "alta", "priorizado",
                     ifelse(viv_ae < sl | viv_ae > su, "no_ae", id_ae)))
  
  cat(" Manzanas priorizadas:", n_distinct(bdd_01$man[bdd_01$id_ae=="priorizado"]),"\n",
      "Viviendas priorizadas:", sum(bdd_01$viv[bdd_01$id_ae=="priorizado"]),"\n",
      "Manzanas aisladas:", n_distinct(bdd_01$man[bdd_01$id_ae=="no_ae"]),"\n",
      "Viviendas aisladas:", sum(bdd_01$viv[bdd_01$id_ae=="no_ae"]),"\n",
      "Número de AE formadas:", n_distinct(bdd_01$id_ae[!bdd_01$id_ae %in% c("priorizado", "no_ae")]),"\n",
      "Viviendas en AE:", sum(bdd_01$viv[!bdd_01$id_ae %in% c("priorizado", "no_ae")]))
  
  
  bdd_01 %>% 
    filter(id_ae == "no_ae") %>% 
    select(man, viv) %>% 
    summary()
  
  ######Juntar aislados######
  source("funciones/matinc.R")
  source("funciones/sumiderog.R")
  
  cs <- "1701023"
  psf <- psf_1
  pif <- pif_1
  
  sl <- round((sl/pi)*pif)
  su <- round((su/ps)*psf)
  
  
  prueba <- bdd_01 %>%
    select(man, id_ae, viv, priorizado)
  
  poligonos <- poligonos %>% 
    full_join(prueba, by = "man")
  
  auxiliar <- poligonos %>% 
    mutate(poligono = ifelse(id_ae %in% c("no_ae", "priorizado"), man, id_ae)) %>% 
    group_by(man = poligono, priorizado) %>% 
    summarise(viv = sum(viv)) %>% 
    ungroup() %>% 
    mutate(atractor = ifelse(viv>=sl | substr(man, 7, 9) == "999", viv, 10000)) %>% 
    arrange(atractor) %>% 
    mutate(orden = row_number(),
           orden = ifelse(viv < sl & substr(man, 7, 9) != "999", 10000, orden)) %>% 
    arrange(man)
  
  A <- matinc(auxiliar)
  
  #Se excluyen las incidencias de las manzanas con priorizadoad 
  prio <- bdd_01$man[bdd_01$priorizado == "alta"]
  A[prio, ] <- 0
  A[, prio] <- 0
  
  peso <- auxiliar$viv
  
  identical(colnames(A),auxiliar$man)
  
  row.names(A) <- NULL
  colnames(A) <- NULL
  
  #Inicializamos el vector de Ã¡reas de empadronamiento
  ae <- rep(0,length(peso))
  
  lol <- auxiliar$orden
  
  while(sum(A)>0 & min(lol)<10000){
    #Se selecciona el atractor como el que tiene menor nÃºmero de incidencias
    i <- min((1:length(peso))[lol==min(lol)])
    #Aplicamos la funciÃ³n sumidero
    clus <- sumiderog(i,A,peso,su,sl)
    #Se actualiza el vector ae en funciÃ³n al resultado de la funciÃ³n sumidero
    ae[clus] <- i
    #Se actualiza los valores de la matriz de incidencia aislando los nodos que
    #son parte de alguna ae
    A[clus,] <- 0
    A[,clus] <- 0
    
    lol[clus] <- 10000
    print(sum(A))
  }
  
  bdd_02 <- cbind(auxiliar,ae,num=1:length(ae)) %>%
    #Se calcula el nÃºmero de viviendas de cada ae formada
    group_by(ae) %>% 
    mutate(viv_ae=sum(viv)) %>% 
    ungroup() %>% 
    #Redefinimos las ae formadas en funciÃ³n del nÃºmero de viviendas
    mutate(
      # ae = ifelse(viv_ae < sl | viv_ae > su ,num,ae),
      #      #Redefinimos el nÃºmero de viviendas de cada parte
      #      viv_ae = ifelse(viv_ae < sl | viv_ae > su, viv,viv_ae),
      #Identificamos al nodo sumidero
      sumidero=ifelse(num==ae,1,0),
      #      #calculamos el Ã¡rea como disperso o amanzanado
      #      area = ifelse(substr(man,7,9)=="999",2,1),
      #creamos el identificador de ae
      id_ae = paste0(str_pad(ae,4,"left","0")),
      id_ae = ifelse(priorizado == "alta", "priorizado",
                     ifelse(viv_ae < sl | viv_ae > su, "no_ae", id_ae)), 
      viv_ae = ifelse(id_ae %in% c("no_ae", "priorizado"), viv, viv_ae))
  valores=
 
 text=  paste(" Manzanas priorizadas:", n_distinct(bdd_02$man[bdd_02$id_ae=="priorizado"]),'<br/>',
             "Viviendas priorizadas:", sum(bdd_02$viv[bdd_02$id_ae=="priorizado"]),'<br/>',
             "Manzanas aisladas:", n_distinct(bdd_02$man[bdd_02$id_ae=="no_ae"]),'<br/>',
              "Viviendas aisladas:", sum(bdd_02$viv[bdd_02$id_ae=="no_ae"]),'<br/>',
             "Numero de AE formadas:", n_distinct(bdd_02$id_ae[!bdd_02$id_ae %in% c("priorizado", "no_ae")]),'<br/>',
             "Viviendas en AE:", sum(bdd_02$viv[!bdd_02$id_ae %in% c("priorizado", "no_ae")]),'<br/>',
             "Dias mixtos y amanzanados:", dias_ce,'<br/>',
             "Dias peligrosos:", dias_pe,'<br/>',
             "# minimo de viviendas", min(bdd_02$viv_ae[!bdd_02$id_ae %in% c("priorizado", "no_ae")]),'<br/>',
             "# maximo de viviendas:", max(bdd_02$viv_ae),'<br/>',
             "# encuestadores priorizados:", sum(bdd_02$viv[bdd_02$id_ae=="priorizado"])/(dias_pe*15),'<br/>',
             "# encuestadores amanzanados:", sum(bdd_02$viv[bdd_02$id_ae!="priorizado"])/(dias_ce*15))
 bdd_03=bdd_02
 bdd_03 %>% 
    filter(id_ae == "no_ae") %>% 
    as.data.frame() %>% 
    select(man, viv) %>% 
    summary()
  
  poligonos_01 <- poligonos %>% 
    select(-id_ae, -viv) %>% 
    st_join(bdd_03 %>% 
              select(id_ae, viv_ae, viv), st_covered_by) %>% 
    as.data.frame() %>% 
    select(-geom)
  
  mansec <- sec_man %>% 
    rename_all(tolower) %>% 
    mutate(man = ifelse(is.na(man), sec, man)) %>% 
    select(man,viv_ori = v_part2022)
  
  prueba_01 <- mansec %>% 
    full_join(poligonos_01, by = "man") %>% 
    select(id = man, priorizado, viv_ori, viv, viv_ae, id_ae)
  prueba_01$carga=prueba_01$viv_ae/dias_ce

  return(list(prueba_01=prueba_01,text=text))
}

conglomerar <- function(A,peso,sl){
  #Guardamos la informacion de la matriz de incidencia en B para no alterar la
  #original
  B <- A
  s <- peso
  #Calculamos el numero de nodos
  n <- dim(s)[1]
  #Inicializamos el vector H de resultados
  H <- rep(0,n)
  #Paso 1. Identificamos los nodos que son cluster por si solo
  a <- (1:n)[s$viv>=sl]
  H[a] <- a
  #Aislamos los nodos que son cluster
  B[a,] <- 0
  B[,a] <- 0
  
  while(sum(B)>=1){
    #Paso 2. Identificamos los nodos que tienen menor numero de incidencias
    #y no son aislados
    d <- rowSums(B)
    d[d==0] <- n
    b <- (1:n)[d==min(d)]
    #Identificamos los nodos con los que se pueden juntar los nodos idenficados
    #anteriormente
    e <- B[,b]*(1:n)
    e <- e[e>0]
    unir <- cbind(nodo1=rep(b,each=min(d)),nodo2=e) %>% 
      as.data.frame() %>% 
      mutate(peso1=s[nodo1,2],
             peso2=s[nodo2,2],
             ninc1=d[nodo1]+peso1/1e4+sample(1:1e6,length(e))/1e11,
             ninc2=d[nodo2]+peso2/1e4+sample(1:1e6,length(e))/1e11) %>% 
      group_by(nodo1) %>% 
      filter(ninc2==min(ninc2)) %>% 
      ungroup() %>% 
      group_by(nodo2) %>% 
      filter(ninc1==min(ninc1)) %>% 
      ungroup() %>% 
      mutate(sumninc=ninc1+ninc2,
             eliminar="no",
             peso=peso1+peso2)
    
    repetidos <- unir$nodo1[unir$nodo1 %in% unir$nodo2]
    
    if(length(repetidos!=0)){
      unir_01 <- unir %>%
        filter(nodo1 %in% repetidos | nodo2 %in% repetidos)
      
      unir_02 <- unir %>%
        filter(!(nodo1 %in% repetidos | nodo2 %in% repetidos))
      
      for (i in 1:length(repetidos)){
        apoyo <- unir_01 %>% 
          filter(nodo1==repetidos[i] | nodo2==repetidos[i]) %>% 
          filter(sumninc==min(sumninc))
        if(i==1){
          unir_03 <- apoyo
        }
        else{
          unir_03 <- rbind(unir_03,apoyo)
        }
      }
      unir_03 <- unir_03 %>% 
        filter(!duplicated(.))
      
      unir <- rbind(unir_02,unir_03)
    }
    
    unir <- unir %>% 
      mutate(nodo=ifelse(nodo1 %in% H,nodo1,nodo2),
             nodob=ifelse(nodo1==nodo,nodo2,nodo1),
             previo=ifelse(nodob %in% H,1,0)) 
    
    H[unir$nodo]=unir$nodo
    H[unir$nodob]=unir$nodo
    
    if(sum(unir$previo)>0){
      index1 <- unir$nodob[unir$previo==1]
      for(i in 1:length(index1)){
        H[H==index1[i]] <- unir$nodo[unir$nodob==index1[i]]
      }
    }
    
    
    
    
    
    
    
    B[unir$nodo,] <- B[unir$nodo1,]+B[unir$nodo2,]
    B[,unir$nodo] <- B[,unir$nodo1]+B[,unir$nodo2]
    B[unir$nodob,] <- 0
    B[,unir$nodob] <- 0
    
    s[unir$nodo,2] <- s[unir$nodo1,2]+s[unir$nodo2,2]
    s[unir$nodob,2] <- 0
    
    cong <- unir$nodo[s[unir$nodo,2]>=sl]
    B[cong,] <- 0
    B[,cong] <- 0
    B[B>=2] <- 1
    diag(B) <- 0
    print(sum(B))
    
  }
  
  r1 <- peso %>% 
    cbind(cong=H) %>% 
    group_by(cong) %>% 
    summarise(vivcong=sum(viv),
              nma=n()) 
  
  aislado <- peso %>% 
    cbind(cong=H) %>% 
    group_by(cong) %>% 
    mutate(vivcong=sum(viv),
           nma=n()) %>% 
    ungroup() %>% 
    mutate(congf=ifelse(vivcong<sl,0,cong),
           npol=1:n)
  
  B <- A
  
  B[aislado$congf==0,aislado$congf==0] <- 0
  
  index <- unique(aislado$congf[aislado$congf>0])
  
  for(i in 1:length(index)){
    if(sum(aislado$congf==index[i])>1){
      B[index[i],] <- colSums(B[aislado$congf==index[i],])
      B[,index[i]] <- rowSums(B[,aislado$congf==index[i]])
      B[aislado$congf==index[i] & aislado$npol!=index[i],] <- 0
      B[,aislado$congf==index[i] & aislado$npol!=index[i]] <- 0
    }
  }
  B[B>1] <- 1
  diag(B) <- 0
  
  
  sn <- cbind(congf=aislado$npol[aislado$congf==0],viv=aislado$viv[aislado$congf==0]) %>% 
    as.data.frame() %>% 
    arrange(desc(viv)) %>% 
    mutate(nman=1,
           asignacion=viv+nman/1000)
  
  sc <- aislado %>% 
    group_by(congf) %>% 
    summarise(viv=mean(vivcong),
              nman=n()) %>% 
    group_by(congf) %>% 
    mutate(asignacion=viv+nman/1000+sample(1:1e6,1)/1e10) %>% 
    ungroup()
  
  congff <- rep(0,dim(sn)[1])
  
  sc_o <- sc
  
  for(i in 1:dim(sn)[1]){
    inc <- B[sn$congf[i],]*(1:n)
    inc <- inc[inc>0]
    asi <- sc$asignacion[sc$congf %in% inc]
    congff[i] <- inc[asi==min(asi)]
    B[congff[i],] <- B[congff[i],] + A[sn$congf[i],]
    B[,congff[i]] <- B[,congff[i]] + A[,sn$congf[i]]
    B[sn$congf[i],] <- 0
    B[,sn$congf[i]] <- 0
    diag(B) <- 0
    B[B>1] <- 1
    
    sc <- sc %>% 
      mutate(viv=ifelse(congf==congff[i],viv+sn$viv[i],viv),
             asignacion=ifelse(congf==congff[i],asignacion+sn$asignacion[i],asignacion),
             nman=ifelse(congf==congff[i],nman+1,nman))
    print(i)
  }
  
  H[sn$congf] <- congff
  
  peso_01 <- peso %>% 
    cbind(cong=H) %>% 
    group_by(cong) %>% 
    summarise(viv=sum(viv),
              nman=n()) %>% 
    mutate(control=ifelse(viv<100 | viv>170 ,"R",
                          ifelse((viv>=100 & viv<110) | (viv>160 & viv<=170),"A","V")))
  
  prop.table(table(peso_01$control,useNA = "ifany"))*100
  
  hist(peso_01$viv,breaks = 10)
  #######################################################################################
  cong <- peso %>% 
    cbind(cong=H)
}
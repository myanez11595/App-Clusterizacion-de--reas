priorizados <- function(manzana_p,ca04_p){
library(sf)
library(sp)
library(rgdal)  
library(stats)
library(reshape2)
library(data.table)
library(magrittr)
library(dplyr)

manzanas=manzana_p %>% st_as_sf
manzanas$priorizado[manzanas$priorizado!="alto"]=  NA
manzanas$priorizado[manzanas$priorizado=="alto"]="ALTA"
#st_read("D:/Entregable/APP completa ejercicios/Aplicacion_AES/Aplicacion_AES/Ejercicio Condado/SECTOR_MANZANA_A.shp") %>% st_as_sf()
ca04=ca04_p %>% st_as_sf
#st_read("D:/Entregable/APP completa ejercicios/Aplicacion_AES/Aplicacion_AES/Ejercicio Condado/CA04_A.shp") %>% st_as_sf()
ca04_post=ca04[ca04$n_viv>=17,]
ca04=ca04[ca04$n_viv<17,]
#source("D:/MODELO PRIORIZADOS/st_centroid_within_poly.r")
manzanas$priorizado = ifelse(is.na(manzanas$priorizado) == TRUE, 0, manzanas$priorizado)
man_pri=manzanas[manzanas$priorizado=="ALTA",]
CA04_PRI=st_intersection(ca04,man_pri)
CA04_PRI_POST=st_intersection(ca04_post,man_pri)
#CA04_PRI=CA04_PRI[1:200,]
CA04_PRI=CA04_PRI[,c(1:14,28)]
CA04_PRI_POST=CA04_PRI_POST[,c(1:14,28)]
CA04_PRI=CA04_PRI[order(CA04_PRI$n_edif),]
#CA04_PRI$n_viv[1]=20
cent_ca04=st_centroid_within_poly(CA04_PRI)
cent_ca04_1=cent_ca04 %>% as_Spatial()
cent_ca04_1$X <-  coordinates(cent_ca04_1)[,1]
cent_ca04_1$Y <-  coordinates(cent_ca04_1)[,2]
cent_ca04_1=cent_ca04_1 %>% as.data.frame()
cent_ca04_1=cent_ca04_1[,c(1,16,17)]
#cent_ca04=merge(cent_ca04,cent_ca04_1,by.x="pk",by.y="pk")
cent_ca04_1=cent_ca04_1[,c(2,3)]
mat=dist(cent_ca04_1)
x=as.matrix(mat)
colnames(x) <- cent_ca04$pk
rownames(x) <- cent_ca04$pk
x=melt(as.matrix(x), varnames = c("row", "col"))
x=x[x$value!=0,]
CA04_PRI_PRE=CA04_PRI
i=nrow(CA04_PRI[CA04_PRI_PRE$n_viv<=17,])
count=0
CA04_PRI_PRE=CA04_PRI_PRE[CA04_PRI_PRE$n_viv<=17,]
CA04_PRI_PRE$index=1:nrow(CA04_PRI_PRE)
CA04_PRI_POST$index=1:nrow(CA04_PRI_POST)
CA04_PRI=CA04_PRI_PRE
i=nrow(CA04_PRI_PRE)


while (i>0) {
  y=0
  y_aux=0
  edif=""
  viv=y
  aux=y_aux
  while(viv<14 || viv<=17){
    if(is.null(CA04_PRI_PRE$id_ae)){
      i=i
      subs=x[x$row == CA04_PRI$pk[i],]}else{
        i=CA04_PRI_PRE$index[CA04_PRI_PRE$pk==z]
        subs=x[x$row == z,]}
    z=subs$col[subs$value==min(subs$value)]


      viv=y+CA04_PRI_PRE$n_viv[i]+CA04_PRI_PRE$n_viv[CA04_PRI_PRE$pk==z]
      y=viv-CA04_PRI_PRE$n_viv[CA04_PRI_PRE$pk==z]

  
    #edif=c(edif,CA04_PRI$pk[i],z)
    #edif=edif[edif!=""]
    #edif=edif[!is.na(edif)]
    if(is.na(CA04_PRI_PRE$id_ae[CA04_PRI_PRE$pk==z]) || is.null(CA04_PRI_PRE$id_ae)){
      CA04_PRI_PRE$id_ae[i]=1+count
      x=x[x$col!=CA04_PRI_PRE$pk[i],]
      CA04_PRI=CA04_PRI[CA04_PRI$pk!=CA04_PRI_PRE$pk[i],]
    } else{
      CA04_PRI_PRE$id_ae[i]=1+count
      x=x[x$col!=CA04_PRI_PRE$pk[i],]
      CA04_PRI=CA04_PRI[CA04_PRI$pk!=CA04_PRI_PRE$pk[i],]
    }

      i=i-1
  }
  
  
  count=CA04_PRI_PRE$id_ae[i+1]
  i=nrow(CA04_PRI_PRE[is.na(CA04_PRI_PRE$id_ae),])
}


dt <- data.table(CA04_PRI_PRE)
viviendas=dt[,list(viv = sum(n_viv)), by = c("id_ae")]
CA04_PRI_PRE=right_join(CA04_PRI_PRE,viviendas,by="id_ae")


i_g=nrow(CA04_PRI_POST)
while(i_g>0){
  CA04_PRI_POST$id_ae[i_g]=paste(i_g,"g")
  CA04_PRI_POST$viv[i_g]=CA04_PRI_POST$n_viv[i_g]
  i_g=i_g-1
}
CA04_FIN=rbind(CA04_PRI_PRE,CA04_PRI_POST)

return(CA04_PRI_PRE)
}

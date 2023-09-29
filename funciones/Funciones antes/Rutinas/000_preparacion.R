preparacion <- function(sec_man,eje,adicio){
SECTOR_MANZANA_A=sec_man
Ejes=eje
adicionales=adicio
#Convertir a linea ca04 Y SECTOR MANZANA A


SECTOR_MANZANA_A_l <- st_cast(SECTOR_MANZANA_A, "MULTILINESTRING")%>% 
  st_cast("LINESTRING") %>% 
  group_by() %>% 
  summarise() %>% 
  st_as_sf()

#Perfil del area del esqueleto de voronoi
buffer = st_union(SECTOR_MANZANA_A_l,Ejes,adicionales)%>% 
  st_as_sf() %>% 
  st_simplify() %>% 
  st_buffer(6)
buffer=st_segmentize(buffer, 5) 
buffer_per = st_as_sf(buffer) %>% 
  st_cast("MULTILINESTRING") %>% 
  st_cast("LINESTRING") %>% 
  group_by() %>% 
  summarise() 
union = buffer %>% 
  st_cast("MULTIPOINT") %>% 
  st_cast("POINT")
puntos <- st_union(union)

#GENERACION DE MANZANAS A PARTIR DE DATOS DE EJES, ADICIONALES Y MANZANAS CON VORONOI

caj <- caja(union)
puntos = st_voronoi(puntos,caj) %>% 
  st_cast()

#CREACION DE LOS POLIGONOS
vorpun <- puntos %>% 
  st_cast("LINESTRING") %>% 
  st_as_sf() %>% 
  group_by() %>% 
  summarise() 
buffer=buffer %>% st_simplify(dTolerance = 5)
vorline = st_intersection(vorpun, buffer)
manzanas_= st_polygonize(vorline)
MANZANAS=st_collection_extract(manzanas_, "POLYGON") %>% 
  st_as_sf()%>% 
  st_cast("MULTIPOLYGON")
#st_write(MANZANAS,"MANZANAS.GPKG",driver="GPKG")
#MANZANAS1=readOGR(dsn="MANZANAS.gpkg",layer="MANZANAS") %>% 
#st_as_sf()
#GENERACION DE CENTROIDE DE MANZANAS
source("funciones/Preparacion/st_centroid_within_poly.R")
man_cent=st_centroid_within_poly(manzanas_aes)%>% 
  st_as_sf()%>% 
  st_cast("MULTIPOINT")
#st_write(man_cent,"man_cent.GPKG",driver="GPKG")
#man_cent1=readOGR(dsn="man_cent.gpkg",layer="man_cent") %>% 
#st_as_sf()
source("funciones/dissolve.R")
new_man= st_join(MANZANAS,man_cent, largest = TRUE) %>% filter(!is.na(N_SEDE))
new_man= st_dissolve(new_man,id_ae,viv_ae)

return(new_man)
}

#SECTOR_MANZANA_A=st_read("D:/Aplicacion AES/condado/SECTOR_MANZANA_A.gpkg")
#SECTOR_MANZANA_A$sec=""
#st_write(SECTOR_MANZANA_A,"D:/Aplicacion AES/condado/SECTOR_MANZANA_A_1.gpkg")

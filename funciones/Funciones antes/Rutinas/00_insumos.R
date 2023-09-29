insumos <- function(sec_man,ca_04,rio,sede){
source("funciones/matinc.R") #Calcula matriz de incidencia
source("funciones/ExtPol/extpol.R") #Extiente polígonos disjuntos

#Cargamos la mansec original para tener la variable de peligrosidad
mansec_old <- sec_man %>% 
  rename_all(tolower) %>%
  mutate(peligrosid = ifelse(is.na(peligrosid), "Disperso", peligrosid)) %>% 
  select(peligrosid)

#Cargamos el shape sector_manzana
mansec <- sec_man %>% 
  rename_all(tolower) %>%
  st_join(mansec_old, join = st_equals)

#Cargamos el shape ca04
ca04 <- ca_04 %>% 
  st_as_sf() %>% 
  st_intersection(mansec %>% st_buffer(-0.3)) %>% 
  st_join(mansec %>% select(man), join = st_covered_by) %>% 
  select(man = man.y)

#Cargamos el shape de rios
rios_a <- rio %>% 
  rename_all(tolower) %>% 
  summarise()

#Cargamos el shape de sede
sede <- sede %>% 
  rename_all(tolower) %>% 
  select(sede = cod_sede)

#Filtramos por manzanas
manzanas <- mansec %>% 
  filter(!is.na(man))

#Filtramos por sectores para determinar el área dispersa
sede_dispersa <- mansec %>% 
  filter(is.na(man)) %>% 
  summarise(sede = first(c_sede))

#Eliminamos el área dispersa para extender las manzanas
perfil <- sede %>% 
  st_difference(sede_dispersa) %>% 
  select(sede)

#Se realiza la extensión de manzanas
manext <- extpol(ca04,perfil) %>% 
  select(-partes) %>% 
  st_as_sf()

#Se realiza el corte de las manzanas extendidas por el shape de rios
apoyo <- manext %>% 
  #Diferencia simétrica de manext con rios
  st_difference(rios_a) %>% 
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




return(poligonos)
}

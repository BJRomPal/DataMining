#Feature Engineering
#creo nuevas variables dentro del mismo mes
#Condimentar a gusto con nuevas variables

#limpio la memoria
rm( list=ls() )
gc()

require( "data.table" )


#para poder usarlo en la PC y en la nube
switch ( Sys.info()[['sysname']],
         Windows = { directory.root   <-  "C:\\" },   #Microsoft Windows
         Darwin  = { directory.root   <-  "~/dm/" },  #Apple MAC
         Linux   = { directory.root   <-  "~/buckets/b1/" }  #Entorno Google Cloud
       )

#defino la carpeta donde trabajo
setwd("/Users/Mariano Miró/ITBA/DataMining/")


CorregirDataset <- function( dataset , arch_destino )
{
  columnas_originales <-  copy(colnames( dataset ))

  #INICIO de la seccion donde se deben hacer cambios con variables nuevas
  #se crean los nuevos campos para MasterCard  y Visa, teniendo en cuenta los NA's
  #varias formas de combinar Visa_status y Master_status
  dataset[ , mv_status01       := pmax( Master_status,  Visa_status, na.rm = TRUE) ]
  #dataset[ , mv_status02       := Master_status +  Visa_status ]
  dataset[ , mv_status03       := pmax( ifelse( is.na(Master_status), 9, Master_status) , ifelse( is.na(Visa_status), 9, Visa_status) ) ]
  dataset[ , mv_status04       := ifelse( is.na(Master_status), 9, Master_status)  +  ifelse( is.na(Visa_status), 9, Visa_status)  ]
  #dataset[ , mv_status05       := ifelse( is.na(Master_status), 9, Master_status)  +  10*ifelse( is.na(Visa_status), 9, Visa_status)  ]
  
  #dataset[ , mv_status06       := ifelse( is.na(Visa_status), 
  #                                        ifelse( is.na(Master_status), 9, Master_status), 
  #                                        Visa_status)  ]
  
  #dataset[ , mv_status07       := ifelse( is.na(Master_status), 
  #                                        ifelse( is.na(Visa_status), 9, Visa_status), 
  #                                        Master_status)  ]
  dataset[ , mv_status08 := ifelse(is.na(Master_status) & is.na(Visa_status), 9, 
                                   ifelse(is.na(Master_status) | is.na(Visa_status), 2, 
                                          ifelse(Master_status == 1 | Visa_status == 1, 1, 2)))]

  #combino MasterCard y Visa
  dataset[ , mv_mfinanciacion_limite := rowSums( cbind( Master_mfinanciacion_limite,  Visa_mfinanciacion_limite) , na.rm=TRUE ) ]

  dataset[ , mv_Fvencimiento         := pmin( Master_Fvencimiento, Visa_Fvencimiento, na.rm = TRUE) ]
  dataset[ , mv_Finiciomora          := pmin( Master_Finiciomora, Visa_Finiciomora, na.rm = TRUE) ]
  dataset[ , mv_msaldototal          := rowSums( cbind( Master_msaldototal,  Visa_msaldototal) , na.rm=TRUE ) ]
  dataset[ , mv_msaldopesos          := rowSums( cbind( Master_msaldopesos,  Visa_msaldopesos) , na.rm=TRUE ) ]
  dataset[ , mv_msaldodolares        := rowSums( cbind( Master_msaldodolares,  Visa_msaldodolares) , na.rm=TRUE ) ]
  dataset[ , mv_mconsumospesos       := rowSums( cbind( Master_mconsumospesos,  Visa_mconsumospesos) , na.rm=TRUE ) ]
  dataset[ , mv_mconsumosdolares     := rowSums( cbind( Master_mconsumosdolares,  Visa_mconsumosdolares) , na.rm=TRUE ) ]
  dataset[ , mv_mlimitecompra        := rowSums( cbind( Master_mlimitecompra,  Visa_mlimitecompra) , na.rm=TRUE ) ]
  dataset[ , mv_madelantopesos       := rowSums( cbind( Master_madelantopesos,  Visa_madelantopesos) , na.rm=TRUE ) ]
  dataset[ , mv_madelantodolares     := rowSums( cbind( Master_madelantodolares,  Visa_madelantodolares) , na.rm=TRUE ) ]
  dataset[ , mv_fultimo_cierre       := pmax( Master_fultimo_cierre, Visa_fultimo_cierre, na.rm = TRUE) ]
  dataset[ , mv_mpagado              := rowSums( cbind( Master_mpagado,  Visa_mpagado) , na.rm=TRUE ) ]
  dataset[ , mv_mpagospesos          := rowSums( cbind( Master_mpagospesos,  Visa_mpagospesos) , na.rm=TRUE ) ]
  dataset[ , mv_mpagosdolares        := rowSums( cbind( Master_mpagosdolares,  Visa_mpagosdolares) , na.rm=TRUE ) ]
  dataset[ , mv_fechaalta            := pmax( Master_fechaalta, Visa_fechaalta, na.rm = TRUE) ]
  dataset[ , mv_mconsumototal        := rowSums( cbind( Master_mconsumototal,  Visa_mconsumototal) , na.rm=TRUE ) ]
  dataset[ , mv_cconsumos            := rowSums( cbind( Master_cconsumos,  Visa_cconsumos) , na.rm=TRUE ) ]
  dataset[ , mv_cadelantosefectivo   := rowSums( cbind( Master_cadelantosefectivo,  Visa_cadelantosefectivo) , na.rm=TRUE ) ]
  dataset[ , mv_mpagominimo          := rowSums( cbind( Master_mpagominimo,  Visa_mpagominimo) , na.rm=TRUE ) ]

  #a partir de aqui juego con la suma de Mastercard y Visa
  dataset[ , mvr_Master_mlimitecompra:= Master_mlimitecompra / mv_mlimitecompra ]
  dataset[ , mvr_Visa_mlimitecompra  := Visa_mlimitecompra / mv_mlimitecompra ]
  dataset[ , mvr_msaldototal         := mv_msaldototal / mv_mlimitecompra ]
  dataset[ , mvr_msaldopesos         := mv_msaldopesos / mv_mlimitecompra ]
  dataset[ , mvr_msaldopesos2        := mv_msaldopesos / mv_msaldototal ]
  dataset[ , mvr_msaldodolares       := mv_msaldodolares / mv_mlimitecompra ]
  dataset[ , mvr_msaldodolares2      := mv_msaldodolares / mv_msaldototal ]
  dataset[ , mvr_mconsumospesos      := mv_mconsumospesos / mv_mlimitecompra ]
  dataset[ , mvr_mconsumosdolares    := mv_mconsumosdolares / mv_mlimitecompra ]
  dataset[ , mvr_madelantopesos      := mv_madelantopesos / mv_mlimitecompra ]
  dataset[ , mvr_madelantodolares    := mv_madelantodolares / mv_mlimitecompra ]
  dataset[ , mvr_mpagado             := mv_mpagado / mv_mlimitecompra ]
  dataset[ , mvr_mpagospesos         := mv_mpagospesos / mv_mlimitecompra ]
  dataset[ , mvr_mpagosdolares       := mv_mpagosdolares / mv_mlimitecompra ]
  dataset[ , mvr_mconsumototal       := mv_mconsumototal  / mv_mlimitecompra ]
  dataset[ , mvr_mpagominimo         := mv_mpagominimo  / mv_mlimitecompra ]
  
  #Modificaciones Mariano
  dataset[ , cheques_total_emitidos       := mcheques_emitidos / ccheques_emitidos]
  dataset[ , cheques_total_recibidos       := mcheques_depositados / ccheques_depositados]
  dataset[ , pagos_totales := mpagodeservicios + mpagomiscuentas ]
  dataset[ , ctrx_mvstatus := ctrx_quarter / mv_status01]
  dataset[ , ctrx_mdeliquency := ctrx_quarter * Master_delinquency]
  dataset[ , ctrx_vdeliquency := ctrx_quarter * Visa_delinquency]
  dataset[ , ctrx_plazosFijos := mplazo_fijo_pesos / ctrx_quarter]
  dataset[ , ctrx_msaldos := mcuentas_saldo / ctrx_quarter]
  dataset[ , ctrx_mcaja_ahorro := mcaja_ahorro / ctrx_quarter]
  dataset[ , ctrx_mcuenta_corriente := mcuenta_corriente / ctrx_quarter]
  dataset[ , ctrx_ccuenta_corriente := ccuenta_corriente * ctrx_quarter]
  dataset[ , ctrx_ccuenta_corriente := ccaja_ahorro * ctrx_quarter]
  dataset[ , ctrx01 := ifelse(ctrx_quarter <= 57, 1, 0 )]
  dataset[ , ctrx02 := ifelse(ctrx_quarter > 57 & ctrx_quarter <= 100, 1, 0 )]
  dataset[ , ctrx03 := ifelse(ctrx_quarter > 100 & ctrx_quarter <= 157, 1, 0 )]
  dataset[ , ctrx04 := ifelse(ctrx_quarter > 157, 1, 0 )]
  dataset[ , m_c_prestamos := mprestamos_personales / cprestamos_personales]
  dataset[ , m_c_prestamos_hip := mprestamos_hipotecarios / cprestamos_hipotecarios]
  dataset[ , m_c_prestamos_pren := mprestamos_prendarios / cprestamos_prendarios]
  
  

  #valvula de seguridad para evitar valores infinitos
  #paso los infinitos a NULOS
  infinitos      <- lapply(names(dataset),function(.name) dataset[ , sum(is.infinite(get(.name)))])
  infinitos_qty  <- sum( unlist( infinitos) )
  if( infinitos_qty > 0 )
  {
    cat( "ATENCION, hay", infinitos_qty, "valores infinitos en tu dataset. Seran pasados a NA\n" )
    dataset[mapply(is.infinite, dataset)] <- NA
  }

  #FIN de la seccion donde se deben hacer cambios con variables nuevas

  columnas_extendidas <-  copy( setdiff(  colnames(dataset), columnas_originales ) )

  #grabo con nombre extendido
  fwrite( dataset,
          file=arch_destino,
          sep= "," )
}
#------------------------------------------------------------------------------

#dir.create( "./datasets/" )


#Primera parte, corrijo los datos de 201905
#dataset  <- fread("./datasetsOri/paquete_premium_201905.csv")

#me quedo con los campos correctos y sin la clase
#campos_buenos <- setdiff(colnames(dataset) , 
#                         c("mrentabilidad","mrentabilidad_annual",
#                           "mcomisiones",
#                           "mpasivos_margen","mactivos_margen",
#                           "ccomisiones_otras","mcomisiones_otras",
#                           "ctransferencias_recibidas","mtransferencias_recibidas" ) )

#fwrite(  dataset[ , campos_buenos, with=FALSE ],
#         file= "./datasets/corregido_201905.csv",
#         sep="\t" )


#Segunda Parte, agrego variables nuevas

#lectura rapida del dataset  usando fread  de la libreria  data.table
dataset1  <- fread("./datasets/corregido_201905.csv")
dataset2  <- fread("./datasetsOri/paquete_premium_201907.csv")

CorregirDataset( dataset1, "./datasets/corregido_201905_ext.csv" )
CorregirDataset( dataset2, "./datasets/corregido_201907_ext.csv" )

#quit( save="no")

-- PRÁCTICA 1.2. METODOLOGÍA DE LA PROGRAMACIÓN --

-- Diego Díaz Alvarellos

module Prac2 where
   --****************************************************--

   -- a) Definición de tipos :
type Tipo_Fecha = (Int, Int, Int)
type Tipo_Dominio = [Char]
type Tipo_Nombre_Maquina = [Char]
type Tipo_Nombre_Usuario = [Char]
type Tipo_Texto_Mensaje = [Char]
type Tipo_Mensaje = (Tipo_Nombre_Usuario, Tipo_Nombre_Usuario, Tipo_Texto_Mensaje, Tipo_Fecha)
type Tipo_Coleccion_Mensajes = ([Tipo_Mensaje])


   -----------------------------------------------------------
   -----------------------------------------------------------

   -- b)Implementacion de funciones

   -----------------------------------------------------------
   -----------------------------------------------------------

      -- Funciones auxiliares :
      -----------------------------------------------------------

fFecha_Correcta :: Tipo_Fecha -> Bool
-- Pre : cierto
-- Post : "establece los rangos permitidos para dia, mes y agno"
fFecha_Correcta (dia, mes, agno) =
   (rango_dia && rango_mes && rango_agno)
   where rango_dia  = (dia > 0) && (dia < 32)
         rango_mes  = (mes > 0) && (mes < 13)
         rango_agno = (agno > 0)
      -----------------------------------------------------------
fFecha_Anterior :: (Tipo_Fecha, Tipo_Fecha) -> Bool
-- PRE: cierto.
-- POST: "compara las fechas y devuelve cierto si la fecha del mensaje recibido
--       es anterior o igual una fecha pedida"
fFecha_Anterior ((dia_m, mes_m, agno_m), (dia, mes, agno)) =
                  if (agno_m < agno)
                     || ((agno_m == agno) && (mes_m < mes))
                     || ((agno_m == agno) && (mes_m == mes) && (dia_m < dia))
                     || ((agno_m == agno) && (mes_m == mes) && (dia_m == dia))
                  then True
                  else False
      -----------------------------------------------------------
fFecha_Posterior :: (Tipo_Fecha, Tipo_Fecha) -> Bool
-- PRE: cierto.
-- POST: "compara las fechas y devuelve cierto si la fecha del mensaje recibido 
--        es posterior a o igual a una fecha pedida
fFecha_Posterior ((dia_m, mes_m, agno_m), (dia, mes, agno)) =
                  if (agno_m > agno)
                     || ((agno_m == agno) && (mes_m > mes))
                     || ((agno_m == agno) && (mes_m == mes) && (dia_m > dia))
                     || ((agno_m == agno) && (mes_m == mes) && (dia_m == dia))
                  then True
                  else False
      -----------------------------------------------------------



   -- Fucines principales :
   -----------------------------------------------------------------
fHay_Mensajes :: (Tipo_Coleccion_Mensajes, Tipo_Nombre_Usuario, Tipo_Nombre_Usuario, Tipo_Fecha) -> Bool
-- PRE : cierto
-- POST : "Devuelve cierto si hay algún mensaje que ha recibido el
--       usuario Propietario del usuario Emisor antes de Una_Fecha (no se inlcuye)
--       entre los mensajes de Mensajes."
fHay_Mensajes ([], propietario, emisor, una_fecha) = False
fHay_Mensajes ((prop_1, emisor_1, texto_mensaje_1, fecha_1) : resto, propietario, emisor, una_fecha) =
                if (prop_1==propietario) &&
                   (emisor_1==emisor) &&
                   (fFecha_Correcta (fecha_1)) &&
                   (fFecha_Correcta (una_fecha)) &&
                   (fFecha_Anterior (fecha_1, una_fecha))
                   || fHay_Mensajes (resto, propietario, emisor, una_fecha)
                then True
                else False
     -----------------------------------------------------------
     -----------------------------------------------------------
fCuantos_Mensajes_Entre :: (Tipo_Coleccion_Mensajes, Tipo_Nombre_Usuario, Tipo_Fecha, Tipo_Fecha) -> Int
-- Pre : Una_Fecha <= Otra_Fecha
-- Post : "Devuelve el numero de mensajes recibidos por Un_Usuario entre
--       Una_Fecha y Otra_Fecha que hay en Mensajes."
fCuantos_Mensajes_Entre ([], un_usuario, una_fecha, otra_fecha) = 0
fCuantos_Mensajes_Entre ((prop_1, emisor_1, texto_mensaje_1, fecha_1) : resto, un_usuario, una_fecha, otra_fecha) =
                          if (prop_1==un_usuario) &&
                             (fFecha_Correcta (una_fecha)) &&
                             (fFecha_Correcta (fecha_1)) &&
                             (fFecha_Correcta (otra_fecha)) &&
                             (fFecha_Anterior (fecha_1, otra_fecha)) &&
                             (fFecha_Posterior (fecha_1, una_fecha))
                          then 1 + fCuantos_Mensajes_Entre (resto, un_usuario, una_fecha, otra_fecha)
                          else fCuantos_Mensajes_Entre (resto, un_usuario, una_fecha, otra_fecha)

     --*******************************************************--

-- PRUEBA PRÁCTICA 
-----------------------------------
diego :: Tipo_Nombre_Usuario
diego = "diego"
zapete :: Tipo_Nombre_Usuario
zapete = "zapete"
obelix :: Tipo_Nombre_Usuario
obelix = "obelix"
goliat :: Tipo_Nombre_Usuario
goliat = "goliat"
-----------------------------------
una_fecha :: Tipo_Fecha
una_fecha = (1, 1, 2002)
primera_fecha :: Tipo_Fecha
primera_fecha = (1, 1, 2001)
segunda_fecha :: Tipo_Fecha
segunda_fecha = (31, 12, 2001)
-----------------------------------
mensaje1 :: Tipo_Mensaje
mensaje1 = (diego, zapete, "con el primer..", (2, 1, 2001))
mensaje2 :: Tipo_Mensaje
mensaje2 = (diego, obelix, "estos romanos..", (12, 7, 2001))
mensaje3 :: Tipo_Mensaje
mensaje3 = (diego, goliat, "creo que al final..", (12, 6, 2001))
-----------------------------------
mensajes :: Tipo_Coleccion_Mensajes
mensajes = ([mensaje1, mensaje2, mensaje3])
-----------------------------------
prueba1 :: Bool
prueba1 = fHay_Mensajes (mensajes, diego, zapete, una_fecha)

prueba2 :: Int
prueba2 = fCuantos_Mensajes_Entre (mensajes, diego, primera_fecha, segunda_fecha)

prueba3 :: Bool
prueba3 = fCuantos_Mensajes_Entre (mensajes, diego, primera_fecha, segunda_fecha) == 3

prueba4 :: Bool
prueba4 = not (fHay_Mensajes (mensajes, zapete, diego, una_fecha))

nota :: String
nota = if (prueba1 && prueba3 && prueba4) then
       "Apto"
     else "No apto"
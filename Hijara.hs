------------------------------------------------------------HIJARA---------------------------------------------------------------------------

-- Este juego fue pensado para una fría y lluviosa tarde de sábado :)

module Hijara where

import Data.Maybe (fromJust)
import Data.List (elemIndex)
import System.Random
import Data.Char
import Data.Matrix(Matrix,toLists,fromLists)

-----------------------------------------------------------------Tipos-----------------------------------------------------------------------

data HijaraPlayer = BluePlayer | YellowPlayer deriving (Eq, Show, Enum, Bounded)

data HijaraGame = HijaraGame (Matrix String) deriving (Eq) --Matriz de String
instance Show HijaraGame where 
   show(HijaraGame matriz) = show (mostrarJuego matriz)

mostrarJuego :: Matrix String -> Matrix String
mostrarJuego matriz = fromLists nuevoTablero
   where filas = toLists matriz
         posicionesNumeradas = map (zip [0..3]) filas
         cuadrantesNumerados = zip posicionesNumeradas [1,5..17]
         filasNuevas = map(\x -> (map(\y -> (((snd x)+(fst y)), snd y)) (fst x))) cuadrantesNumerados
         nuevoTablero = map (\fila -> [(show x) ++ " - " ++ y | (x,y) <- fila]) filasNuevas

data HijaraAction = HijaraAction(Int, Int) deriving (Eq, Read) --(Seccion, Posición)
instance Show HijaraAction where 
   show(HijaraAction tupla) = show seccion ++ " " ++ show posicion ++ ","
      where (seccion, posicion) = tupla

-----------------------------------------------------------Funciones pedidas---------------------------------------------------------------------------------

beginning :: HijaraGame
beginning = HijaraGame(fromLists [[map intToDigit [1..4] | columnas <- [1..4]] | filas <- [1..4]])

activePlayer :: HijaraGame -> Maybe HijaraPlayer --Devuelve un Maybe HijaraPlayer
activePlayer tablero
   |fichas == 64 = Nothing --Juego terminado
   |odd fichas = Just YellowPlayer
   |even fichas = Just BluePlayer  
   where fichas = cantidadDeFichas tablero

actions :: HijaraGame -> [(HijaraPlayer, [HijaraAction])]
actions juego
   |jugadorActual == Nothing = [(YellowPlayer,[]),(BluePlayer,[])]
   |otherwise = [(fromJust(jugadorActual), seccionesNoLlenos), (contrario (fromJust jugadorActual), [])] --fromJust
   where jugadorActual = activePlayer juego
         posicionesDisponibles = map ((+1).length.takeWhile (`elem` "BY")) (secciones juego)
         posicionesDisponiblesPorSeccion = zip [1..16] posicionesDisponibles
         seccionesNoLlenos = [HijaraAction(x,y)|(x,y)<-posicionesDisponiblesPorSeccion, y<= 4]

next :: HijaraGame -> (HijaraPlayer, HijaraAction) -> HijaraGame
next juego (jugador,(HijaraAction(seccion, posicion)))
   |cantidadDeFichas juego == 64 = juego --Juego terminado
   |accionPosiblesSeccion seccionCambio movimiento = HijaraGame (fromLists (seccionesH tableroResultante))
   where movimiento = HijaraAction(seccion, posicion)  
         seccionesJuego = secciones juego
         seccionesPrevias = take (seccion-1) seccionesJuego
         seccionCambio = seccionesJuego!!(seccion-1)
         seccionModificada = cambiarUnaSeccion (seccionesJuego!!(seccion-1)) (fichaJugador jugador) posicion   
         seccionesLuego = drop seccion seccionesJuego
         tableroResultante = seccionesPrevias ++ [seccionModificada] ++ seccionesLuego

result :: HijaraGame -> [(HijaraPlayer, Int)]
result juego 
   |cantidadDeFichas juego /= 64 = []
   |puntosAmarillo == puntosAzul = [(BluePlayer,0),(YellowPlayer,0)]
   |puntosAzul > puntosAmarillo = [(BluePlayer,1),(YellowPlayer,-1)]
   |otherwise = [(BluePlayer,-1),(YellowPlayer,1)]
   where puntosAmarillo = puntosTotales YellowPlayer juego
         puntosAzul =  puntosTotales BluePlayer juego
         
score :: HijaraGame -> [(HijaraPlayer, Int)]
score juego = [(BluePlayer, puntosAzul),(YellowPlayer,puntosAmarillo)]
   where puntosAzul = puntosTotales BluePlayer juego
         puntosAmarillo = puntosTotales YellowPlayer juego

showGame :: HijaraGame -> String
showGame tablero = show tablero

showAction :: HijaraAction -> String
showAction accion = show accion

readAction :: String -> HijaraAction
readAction accion@[a,' ', b] = HijaraAction(read seccion ::Int, read posicion::Int)
   where espacio = fromJust (elemIndex ' ' accion) --fromJust
         (seccion,posicion) = splitAt espacio accion
readAction accion@[a,b, ' ', c] = HijaraAction(read seccion ::Int, read posicion::Int)
   where espacio = fromJust (elemIndex ' ' accion) --fromJust
         (seccion,posicion) = splitAt espacio accion
readAction _ = HijaraAction ((-1),(-1))

-----------------------------------------------------Funciones auxiliares---------------------------------------------------------------------------------:

--Genéricas: 

cantidadDeFichas :: HijaraGame -> Int
cantidadDeFichas juego = length [casilla | casilla <- (valores juego), elem casilla "BY"] 

contrario :: HijaraPlayer -> HijaraPlayer
contrario YellowPlayer = BluePlayer
contrario BluePlayer = YellowPlayer          

fichaJugador :: HijaraPlayer -> Char
fichaJugador YellowPlayer = 'Y'
fichaJugador BluePlayer = 'B'

filas :: HijaraGame -> [[String]] --Lista de listas de listas
filas (HijaraGame tablero) = toLists tablero

secciones :: HijaraGame -> [String] --Lista de listas
secciones juego = concat (filas juego)

valores :: HijaraGame -> String --Lista
valores tablero = concat (secciones tablero)

seccionesH :: [a] -> [[a]]--Secciones Horizontales
seccionesH [a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p] = [[a,b,c,d],[e,f,g,h],[i,j,k,l],[m,n,o,p]]

seccionesV :: [a] -> [[a]]--Secciones verticales
seccionesV [a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p] = [[a,e,i,m],[b,f,j,n],[c,g,k,o],[d,h,l,p]]

seccionesD :: [a] -> [[a]]--Secciones diagonales
seccionesD [a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p] = [[a,f,k,p],[m,j,g,d]]

seccionesHVD :: [[a]] -> [[[a]]] --Secciones horizontales, verticales y diagonales
seccionesHVD secciones = (seccionesH secciones) ++ (seccionesV secciones) ++ (seccionesD secciones)


--APLICAR CON SECCIONES
seccionesEsquina :: [a] -> [a]--Secciones esquina
seccionesEsquina [a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p] = [a,d,m,p]

mismaPosicionEnSecciones :: [[a]]->[[a]] --Mismas posiciones en 4 secciones
mismaPosicionEnSecciones [a,b,c,d] = [[a!!x, b!!x, c!!x, d!!x]|x<-[0..3]]

escalerasEnSecciones :: [[a]]->[[a]] --Escaleras en 4 secciones
escalerasEnSecciones [a,b,c,d] = [[a!!0, b!!1, c!!2, d!!3],[a!!3, b!!2, c!!1, d!!0]]

--De next:

accionPosiblesSeccion :: [Char] -> HijaraAction -> Bool
accionPosiblesSeccion unaSeccion (HijaraAction (seccion,posicion))
   |not (movimientoValido movimiento) = error ("Movimiento no válido")
   |length posicionesLlenas == 4 = error ("seccion lleno")
   |length posicionesLlenas == posicion = error ("Lugar ocupado")
   |length posicionesLlenas /= (posicion - 1) = error ("Posición no válida") --No es la mínima posición dentro del seccion
   |otherwise = True
   where movimiento = HijaraAction (seccion,posicion)
         posicionesLlenas = filter (`elem` "BY") unaSeccion

cambiarUnaSeccion :: String-> Char -> Int -> String
cambiarUnaSeccion unaSeccion ficha posicion = posicionesPrevias ++ [ficha] ++ posicionesLuego
   where posicionesPrevias = take (posicion-1) unaSeccion
         posicionesLuego = drop posicion unaSeccion

movimientoValido :: HijaraAction -> Bool
movimientoValido (HijaraAction (seccion, posicion))
   |seccion > 16 || seccion < 1 || posicion > 4 || posicion < 1 = False
   |otherwise = True

--De score:

puntosTotales :: HijaraPlayer->HijaraGame->Int
puntosTotales jugador juego = puntosSeccion ficha juego + puntosMismaPosicion ficha juego + puntosEscalera ficha juego
   where ficha = fichaJugador jugador

puntosSeccion :: Char -> HijaraGame -> Int
puntosSeccion ficha juego = foldl1 (+) (map (puntuarFichaEnValores 20 ficha) (secciones juego))
    
puntosMismaPosicion :: Char -> HijaraGame -> Int
puntosMismaPosicion ficha juego = foldl1 (+) (map (puntuarFichaEnValores 10 ficha) lugaresIguales)
   where lugaresIguales = concat (map mismaPosicionEnSecciones (seccionesHVD (secciones juego)))

puntosEscalera :: Char -> HijaraGame -> Int
puntosEscalera ficha juego = foldl1 (+) (map (puntuarFichaEnValores 15 ficha) lugaresEscalera)
   where lugaresEscalera = concat (map escalerasEnSecciones (seccionesHVD (secciones juego)))

puntuarFichaEnValores :: Int -> Char -> String -> Int
puntuarFichaEnValores puntaje ficha valores 
   |[ficha,ficha,ficha,ficha] == valores = puntaje
   |otherwise = 0

-----------------------------------------------------------------Main---------------------------------------------------------------------------------

{-- Match controller -------------------------------------------------------------------------------
Código de prueba. Incluye una función para correr las partidas y dos agentes: consola y aleatorio.-}

type HijaraAgent = HijaraGame -> IO (Maybe HijaraAction)

{- La función ´runMatch´ corre la partida completa a partir del estado de juego dado, usando los dos 
agentes dados. Retorna una tupla con los puntajes (score) finales del juego.-}

players :: [HijaraPlayer]
players = [minBound..maxBound]

runMatch :: (HijaraAgent, HijaraAgent) -> HijaraGame -> IO [(HijaraPlayer, Int)]
runMatch ags@(ag1, ag2) g = do
   putStrLn (showGame g)
   case (activePlayer g) of
      Nothing -> return $ (result g ++ score g)
      Just p -> do
         let ag = [ag1, ag2] !! (fromJust $ elemIndex p players)
         move <- ag g
         runMatch ags (Hijara.next g (p, fromJust move))

{- La función ´runOnConsole´ ejecuta toda la partida a partir del estado inicial usando dos agentes
de consola.-}

correrJugando :: IO [(HijaraPlayer, Int)]
correrJugando = do
   runMatch (consoleAgent BluePlayer, consoleAgent YellowPlayer) beginning

correrRandom :: IO [(HijaraPlayer, Int)]
correrRandom = do
   runMatch (randomAgent BluePlayer, randomAgent YellowPlayer) beginning

correrMixto :: IO [(HijaraPlayer, Int)]
correrMixto = do
   runMatch (consoleAgent BluePlayer, randomAgent YellowPlayer) beginning

{- El agente de consola ´consoleAgent´ muestra el estado de juego y los movimientos disponibles por
consola, y espera una acción por entrada de texto.-}

consoleAgent :: HijaraPlayer -> HijaraAgent
consoleAgent player state = do
   let moves = fromJust $ lookup player (actions state)
   if null moves then do
      putStrLn "No moves!"
      getLine
      return Nothing
   else do
      putStrLn ("Select one move " ++ [(fichaJugador player)] ++ " :"++ concat [" " ++ showAction m | m <- moves])
      line <- getLine
      let input = readAction line
      if elem input moves then return (Just input) else do 
         putStrLn "Invalid move!"
         consoleAgent player state

randomAgent :: HijaraPlayer -> HijaraAgent
randomAgent player state = do
    let moves = fromJust $ lookup player (actions state)
    if null moves then do
       putStrLn "No moves!"
       return Nothing
    else do
       i <- randomRIO (0, (length moves) - 1)
       return (Just (moves !! i))

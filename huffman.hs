-- Huffman Código en Haskell
-- Autor: Henry Guzmán
-- Fecha: 29 de mayo de 2026
-- Curso: Algoritmos y Estructuras de Datos

-- Creación de clase principal e importación de librerías a usar
module Main where

import qualified Data.Map.Strict as Map
import Data.List (sortBy)
import Data.Ord (comparing)


-- Definición del árbol de Huffman
data ArbolHuffman
    = Hoja Char Int
    | Nodo Int ArbolHuffman ArbolHuffman
    deriving (Show, Eq)


-- Función para obtener la frecuencia de un nodo
frecuencia :: ArbolHuffman -> Int
frecuencia (Hoja _ f) = f
frecuencia (Nodo f _ _) = f


-- Contar las frecuencias de todos los caracteres
mapaFrecuencias :: String -> Map.Map Char Int
mapaFrecuencias =
    foldr (\c -> Map.insertWith (+) c 1) Map.empty


-- Función para construir las hojas iniciales
construirHojas :: String -> [ArbolHuffman]
construirHojas texto =
    map (\(c,f) -> Hoja c f)
        (Map.toList (mapaFrecuencias texto))


-- Insertar nodos manteniendo el orden por frecuencia (versión optimizada)
insertarOrdenado :: ArbolHuffman -> [ArbolHuffman] -> [ArbolHuffman]
insertarOrdenado arbol [] = [arbol]
insertarOrdenado arbol (a:as)
    | frecuencia arbol <= frecuencia a = arbol : a : as
    | otherwise = a : insertarOrdenado arbol as


-- Construcción del árbol de Huffman
construirArbol :: String -> Maybe ArbolHuffman
construirArbol "" = Nothing
construirArbol texto =
    Just $
    construir $
    sortBy (comparing frecuencia) (construirHojas texto)
  where
    construir [arbol] = arbol
    construir (x:y:resto) =
        let fusionado =
                Nodo
                    (frecuencia x + frecuencia y)
                    x
                    y
        in construir (insertarOrdenado fusionado resto)
    construir _ =
        error "Error inesperado construyendo árbol"


-- Generación de los códigos Huffman (versión optimizada sin Map.union)
generarCodigos :: ArbolHuffman -> Map.Map Char String
generarCodigos arbol =
    generar arbol "" Map.empty
  where
    generar (Hoja c _) codigo acumulador =
        Map.insert c (if null codigo then "0" else codigo) acumulador
    generar (Nodo _ izquierda derecha) codigo acumulador =
        let acumulador' = generar izquierda (codigo ++ "0") acumulador
        in generar derecha (codigo ++ "1") acumulador'


-- Codificación de los caracteres
codificar :: String -> Maybe (String, ArbolHuffman)
codificar "" = Nothing
codificar texto =
    case construirArbol texto of
        Nothing -> Nothing
        Just arbol ->
            let codigos = generarCodigos arbol
                codificado = concatMap (\c -> codigos Map.! c) texto
            in Just (codificado, arbol)


-- Decodificación de los caracteres con manejo de errores mejorado
decodificar :: ArbolHuffman -> String -> Either String String
decodificar arbol bits = 
    case caminar arbol bits of
        (resultado, "") -> Right resultado
        (_, resto) -> Left $ "Bits sobrantes o incompletos: " ++ show resto
  where
    caminar _ [] = ("", "")
    caminar raiz xs =
        case recorrerArbol raiz xs of
            Left err -> (err, "")
            Right (caracter, restante) ->
                let (resultadoRestante, finalRestante) = caminar raiz restante
                in (caracter : resultadoRestante, finalRestante)
    
    recorrerArbol (Hoja c _) resto = Right (c, resto)
    recorrerArbol (Nodo _ izquierda derecha) (b:bs)
        | b == '0' = recorrerArbol izquierda bs
        | b == '1' = recorrerArbol derecha bs
        | otherwise = Left "Error: Bit inválido - solo se permiten '0' y '1'"
    recorrerArbol _ [] = Left "Error: Secuencia de bits incompleta"


-- Decodificación simple (sin manejo de errores para compatibilidad)
decodificarSimple :: ArbolHuffman -> String -> String
decodificarSimple arbol bits =
    case decodificar arbol bits of
        Right resultado -> resultado
        Left err -> error err


-- Cálculo de tamaños
bitsOriginales :: String -> Int
bitsOriginales texto =
    length texto * 8

bitsComprimidos :: String -> Int
bitsComprimidos =
    length

ratioCompresion :: String -> String -> Double
ratioCompresion original comprimido =
    (1 - fromIntegral (length comprimido)
        / fromIntegral (length original * 8))
        * 100


-- Mostrar códigos de Huffman de forma ordenada
mostrarCodigos :: ArbolHuffman -> IO ()
mostrarCodigos arbol = do
    putStrLn "\nCódigos Huffman:"
    let codigos = Map.toList (generarCodigos arbol)
    let codigosOrdenados = sortBy (comparing fst) codigos  -- Ordenar por carácter
    mapM_ mostrarCodigo codigosOrdenados
  where
    mostrarCodigo (c, codigo) =
        putStrLn $ show c ++ " -> " ++ codigo


-- Verificar que la codificación/decodificación es correcta
verificarCodificacion :: String -> IO ()
verificarCodificacion entrada = do
    case codificar entrada of
        Nothing -> do
            putStrLn "Cadena vacía - no hay nada que verificar"
        Just (codificado, arbol) -> do
            let decodificado = decodificarSimple arbol codificado
            if entrada == decodificado
                then do
                    putStrLn "✓ Verificación exitosa: codificación/decodificación correcta"
                else do
                    putStrLn "✗ Error: la decodificación no coincide con el original"
                    putStrLn $ "Original:     " ++ show entrada
                    putStrLn $ "Decodificado: " ++ show decodificado


-- Ejecutar las pruebas mejorada
ejecutarPrueba :: String -> IO ()
ejecutarPrueba entrada = do
    putStrLn "\n====================================="
    putStrLn $ "Entrada: " ++ show entrada
    putStrLn $ "Longitud: " ++ show (length entrada) ++ " caracteres"
    
    case codificar entrada of
        Nothing ->
            putStrLn "No hay datos para comprimir."
        Just (codificado, arbol) -> do
            mostrarCodigos arbol
            
            putStrLn "\nTexto codificado:"
            if length codificado > 100
                then putStrLn $ take 100 codificado ++ "... (truncado)"
                else putStrLn codificado
            
            putStrLn "\nTexto decodificado:"
            let decodificado = decodificarSimple arbol codificado
            putStrLn decodificado
            
            putStrLn "\n=== ESTADÍSTICAS ==="
            putStrLn $ "Bits originales:   " ++ show (bitsOriginales entrada)
            putStrLn $ "Bits comprimidos:   " ++ show (bitsComprimidos codificado)
            putStrLn $ "Ahorro:             " ++ show (bitsOriginales entrada - bitsComprimidos codificado) ++ " bits"
            putStrLn $ "Compresión:         " ++ show (ratioCompresion entrada codificado) ++ "%"
            putStrLn $ "Tasa de compresión: " ++ show (fromIntegral (bitsComprimidos codificado) / fromIntegral (bitsOriginales entrada) :: Double)
            
            -- Verificar integridad
            verificarCodificacion entrada


-- Menú interactivo
main :: IO ()
main = do
    putStrLn " HUFFMAN CODING v2.0 "
    putStrLn " Algoritmo de compresión de datos "
    
    -- Caso de prueba 1: Texto normal
    ejecutarPrueba "hola mundo"
    
    -- Caso de prueba 2: Caracteres repetidos
    ejecutarPrueba "aaaaaaaaaa"
    
    -- Caso de prueba 3: Pangrama
    ejecutarPrueba "El veloz murciélago hindú comía feliz cardillo y kiwi"
    
    -- Caso de prueba 4: Caracteres especiales
    ejecutarPrueba "¡Hola! ¿Cómo estás?"
    
    -- Caso borde: Cadena vacía
    ejecutarPrueba ""
    
    -- Caso de prueba 5: Con mayúsculas/minúsculas
    ejecutarPrueba "ABABABababab"
    
    -- Caso de prueba 6: Texto largo
    let textoLargo = replicate 500 'a' ++ replicate 500 'b' ++ replicate 100 'c'
    ejecutarPrueba textoLargo
    
    putStrLn " FIN DE LAS PRUEBAS "
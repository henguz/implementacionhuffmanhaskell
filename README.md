# ImplementacionHuffmanHaskell
# Huffman Coding en Haskell
## Descripción
Implementación funcional del algoritmo de Huffman para compresión sin pérdida de datos.

El programa:
* Calcula frecuencias de caracteres.
* Construye el árbol de Huffman.
* Genera códigos binarios óptimos.
* Codifica texto.
* Decodifica texto.
* Calcula estadísticas de compresión.
* Maneja casos borde.

## Requisitos
* GHC 9.0 o superior

## Compilación del programa
* Abrir una terminal en la carpeta donde se encuentra el archivo Main.hs.
* Ejecutar el siguiente comando: ghc Main.hs -o huffman
* Si la compilación es exitosa, se generará un archivo ejecutable llamado: Linux/macOS: huffman -- Windows: huffman.exe

## Ejecución del programa
Linux o macOS
Ejecutar: /huffman

Windows
Ejecutar: huffman.exe

## Ejecución paso a paso
1. Abrir una terminal.
2. Navegar hasta la carpeta del proyecto.
3. Compilar el código fuente: ghc Main.hs -o huffman
4. Ejecutar el programa: Linux/macOS: ./huffman -- Windows: huffman.exe
5. Observar los resultados generados para cada caso de prueba.

## Casos de prueba incluidos
1. hello world
2. aaaaaaaaaa
3. The quick brown fox jumps over the lazy dog

## Caso especial
* Cadena vacía

## Salida esperada

* Tabla de códigos Huffman.
* Texto codificado.
* Texto decodificado.
* Tamaño original.
* Tamaño comprimido.
* Porcentaje de compresión.


Tu Nombre

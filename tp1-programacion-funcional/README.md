# TP1 - Calculadora Incierta

**Programación Funcional en Haskell**

## Descripción

Una calculadora que opera con incertidumbre estadística, trabajando con rangos numéricos además de números concretos. Los rangos representan números con un 95% de confianza y distribución normal.

### Ejemplo de uso
```
incierticalc> 10000 * 0.7∼0.8 + 2000∼5000
12580.79 - +inf |
12293.05 - 12580.79 |
...
9415.63 - 9703.37 |
-inf - 9415.63 |
```

## Estructura del Proyecto

```
├── app/
│   └── Main.hs              # Punto de entrada de la aplicación
├── src/
│   ├── App.hs              # Aplicación interactiva (completo)
│   ├── Expr/
│   │   └── Parser.hs       # Parser String → Expr (completo)
│   ├── Generador.hs        # Generador de números aleatorios (completo)
│   ├── Util.hs            # Funciones auxiliares (ejercicios 1-2)
│   ├── Histograma.hs      # Tipo abstracto Histograma (ejercicios 3-6)
│   └── Expr.hs            # Expresiones y evaluación (ejercicios 7-11)
├── test/
│   └── Main.hs            # Tests unitarios
├── .hlint.yaml            # Configuración de linter hlint
├── incierticalc.cabal     # Configuración de Cabal
├── Makefile               # Automatización de tareas
├── .gitignore             
└── README.md             # Este archivo
```

## Ejercicios Implementados

### Módulo Util
- [X] **Ejercicio 1**: `alinearDerecha` - Alineación de strings
- [X] **Ejercicio 2**: `actualizarElem` - Actualización de elementos en listas

### Módulo Histograma
- [X] **Ejercicio 3**: `vacio` - Inicialización de histograma vacío
- [X] **Ejercicio 4**: `agregar` - Agregar valores al histograma
- [X] **Ejercicio 5**: `histograma` - Construcción desde lista de valores
- [X] **Ejercicio 6**: `casilleros` - Observer para el tipo abstracto

### Módulo Expr
- [X] **Ejercicio 7**: `recrExpr` y `foldExpr` - Esquemas de recursión
- [] **Ejercicio 8**: `eval` - Evaluación de expresiones con generadores
- [] **Ejercicio 9**: `armarHistograma` - Construcción de histogramas desde generadores
- [] **Ejercicio 10**: `evalHistograma` - Evaluación completa con histograma
- [] **Ejercicio 11**: `mostrar` - Pretty printing de expresiones

### Demostración Formal
- [] **Ejercicio 12**: Demostración por inducción estructural

## Comandos Disponibles

```bash
# Compilar y ejecutar tests
make test

# Iniciar GHCi con módulos cargados  
make repl

# Modo watch - ejecuta tests en cada cambio
make watch

# Ejecutar la aplicación interactiva
make run
```

## Uso de la Aplicación

```bash
make run
```

Comandos disponibles en la aplicación:
- Expresiones matemáticas con rangos: `10 * 0.7∼0.8 + 2000∼5000`
- Salir: `:q`

## Tipos de Datos Principales

### Expr
```haskell
data Expr = Const Float
          | Rango Float Float  
          | Suma Expr Expr
          | Resta Expr Expr
          | Mult Expr Expr
          | Div Expr Expr
```

### Histograma (Abstracto)
```haskell
data Histograma = Histograma Float Float [Int]
-- inicio_segunda_casilla tamaño_intervalo cuentas_por_casillero
```

## Características Técnicas

- **Sin recursión explícita**: Se usan esquemas de recursión y funciones de alto orden
- **Funciones puras**: Excepto generación de números aleatorios
- **Tipos abstractos**: Histograma encapsula su representación interna
- **Testing**: Casos de prueba con HUnit

## Testing

Los tests están organizados por módulo y ejercicio. Ejecutar con:

```bash
make test
```

Cada función implementada incluye casos de prueba que verifican:
- Casos típicos de uso
- Casos borde
- Propiedades esperadas

## Notas de Implementación

- Se utilizan funciones de alto orden (`map`, `filter`, `foldr`, etc.)
- Currificación aplicada donde es apropiado
- Código declarativo sin repetición innecesaria
- Comentarios explicativos para decisiones de diseño no obvias

## Estado del Proyecto

- [] Ejercicios 1-11 implementados
- [] Demostración formal completada  
- [] Tests unitarios agregados
- [] Aplicación funcional


**Fecha de entrega**: 16 de septiembre

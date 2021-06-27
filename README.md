# FLP

* rojbar
* Matomaral
* thiagoc789

## Talleres

### Evaluador de talleres
El evaluador de talleres es una macro que permite realizar tests de las funciones pedidas para determinado taller.

### Taller 1
#### Puntos

> |1| |2| |3| |4| |5| |6| |7| |8| |9| |10| |11| |12| |13| |14| |15| |16| |17|

### Taller 2
#### Puntos 

> |1| |2| |3| |4| |5|

## Proyecto B-SAT
> B-SAT.rkt
### Lenguaje BSAT
#### Tipos de datos
##### Enteros
```
var x = 2 in x
```
##### Flotantes
```
var x = 2.012 in x
```
##### Caracter
```
var x = 'A' in x
```
##### Cadena
```
var x = `Hola mundo` in x
```
##### Booleano
```
var x = true in x
var y = false in x
```
##### Hexadecimal
```
var x = x16(15) in x
```
#### Identificadores
Cada identificador debe empezar por una letra
```
var soyUnaVariable = x16(15) in x
```
#### Datos predfinidos
##### Listas
```
var x =  list(1,2,3,4) in x
```
##### Vectores
```
var x =  vector(1,2,3,4) in x
```
##### Diccionarios
Cada clave debe ser accedida como una cadena
```
var x =  dictionary(a -> 2; b ->4) in x
```
##### SAT
```
var x =  FNC 2 (1 and -1) in x
```
Para resolver la instanca SAT y obtener su resultado debemos correr
```
var x =  FNC 2 (1 and -1) in $x.solve()
```
#### Primitivas
Para ejecutar una primitiva se realiza 
```
var x =  [<primitiva> <primer-operando>,...] in x
```
##### Primitivas disponibles
| Tipos de datos soportados| Simbolo |
| ------------- | ------------- |
| valores numericos    | + |
| valores numericos    | - |
| valores numericos    | * |
| valores numericos    | / |
| valores numericos    | % |
| valores numericos    | add1 |
| valores numericos    | sub1 |
| cadenas   | concat|
| cadenas  | length|
| listas | create-list |
| listas  | append |
| listas   | empty? |
| listas   | list?  |
| listas  | head |
| listas   | tail |
| listas  | empty|
| diccionarios |create-dictionary|
| diccionarios  | dictionary? |
| diccionarios | set-dictionary  |
| diccionarios | ref-dictionary |
| vector  | ref-vector  |
| vector | set-vector |
| vector  | create-vector  |
| vector  | vector? |

#### Definicion de variables
##### Variables mutables Var
```
var x =  2 in x
```
##### Variables inmutables cons
```
cons x =  2 in x
```
##### Variables que apuntan a procedimiento recursivos
```
recursive x(a,b,c) =  [+ a,b]; y(a,b,c) =  [+ b,c] in x
```                 
#### Asignacion de variables
```
var x = 2 in set x = 3
``` 
#### Estructuras de control
##### Begin
```
begin
[+2,3];
[+2,9]
end
``` 
##### if
```
if true then true else false 
``` 
##### while
```
var x = 3 in
  while [> x,0] do 
    begin
     set x = [- x,1];
     x
    end 
``` 
##### For
```
for(i = 0; <to/downto> 9) do 
     begin
     set i = [- x,1];
     i
    end 
``` 
#### Procedimientos
##### Definicion
```
procedure(a,b ){
    [+ a,b]
}
``` 
##### Invocacion
```
var x = procedure(a,b ){
    [+ a,b]
} in invoke x(2,3)
```                       
#### Expresiones booleanas
```
(<primitiva-booleana> expression,expression)
```   
##### Primitvas booleanas
| Tipos de datos soportados| Simbolo |
| ------------- | ------------- |
| valores numericos    | < |
| valores numericos    | > |
| valores numericos    | <= |
| valores numericos    | >= |
| any    | == |
| any   | <> |
##### Expresions unarias
```
(not true)
```   
##### Expresions binarias
```
(&& true,false)
```         
#### Paso por referencia
```
&<identificador>
```
#### Clases
La clase base es object
```
class hola extends object 
field atributo1
method metodo1 (a,b,c) [+ a,b];
``` 
##### Creacion de objetos
``` 
var x = new hola(2,3,4) in x
``` 
##### Llamado de objetos
``` 
send x (a,b,c)
``` 
##### LLamado de super
``` 
super <identificador> (a,b)
``` 
#### Comentarios
``` 
//soy un comentario
``` 

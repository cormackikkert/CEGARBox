# CEGARBox

## Installation
Install Haskell. Then issue: 
```
cabal install
```

## Usage
CEGARBox can determine the satisfiability of formulae in the logics K, KT, and S4. Usage for each logic is as follows:
```
CEGARBox formula_file
CEGARBox --reflexive formula_file
CEGARBox --reflexive --transitive formula_file
```

## Input
CEGARBox takes as input formula defined by the grammar:

```
Fml : '(' Fml ')'         
    | '~' Fml            
    | '<>' Fml       
    | dia Fml            
    | '<' int '>' Fml    
    | '[]' Fml        
    | box Fml            
    | '[' int ']' Fml    
    | Fml '&' Fml        
    | Fml '|' Fml        
    | Fml '=>' Fml     
    | Fml '->' Fml     
    | Fml '<=>' Fml 
    | Fml '<->' Fml 
    | String                 
    | true                
    | false               
```

where a String is alphanumeric.

So for example, when running CEGARBox on an intohylo file (say formula.intohylo) you need to first:
1. remove the "begin" line
2. remove the "end" line
3. replace [r1] with []
4. replace <r1> with <>

then call:
```
CEGARBox formula.intohylo
```

## Benchmarks
The LWB generator we created is here:
https://github.com/cormackikkert/LWB-benchmark-generator

The MQBF and 3CNF benchmarks we used can be found here:
http://www.cril.univ-artois.fr/~montmirail/mosaic/#

# Type-System

## Exemplos para disciplina de Sistemas de Tipos para Linguagens de Programação

### __Lambda__ - Inferência de tipos: cálculo Lambda simplesmente tipado.

### __LambdaInterpreter__ - Interpretador de expressões lambda (não tipado).

A função _main_ irá solicitar uma expressão lambda com a seguinte sintáxe:
```
E := \x.E  (Abstração Lambda)  
     | E E (Aplicação)  
     | x   (Variável, sendo que x representa um nome de variável)  
     | (E) 
```


# DPC: Assignment 2

## Group 5
Name | Student ID
--- | ---
Rowan Cole | 1411131
Joseph Groocock | 1467414
Christopher Lane | 1435876
David Marlow | 1416386

## Compile
```bash
erlc src/tarry.erl
```
OR
```bash
make
```

## Run
```bash
erl -run tarry -s init stop -noshell
```
OR
```
make run < inputfile
```

## Sample Output
```text
p q s t r t u t s q p
```

## Accomplished
Assignment fully completed and correct.

## Discretionary Mark Notes
We've included a makefile that runs input from input.txt,
Added randomness to selecting which path to take,
Well-commented code.

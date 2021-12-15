# dice-mc
```
    _/_/_/    _/_/_/   _/_/_/  _/_/_/            _/      _/   _/_/_/   
   _/    _/    _/   _/        _/                _/_/  _/_/   _/        
  _/    _/    _/   _/        _/_/_/   _/_/_/   _/  _/  _/   _/         
 _/    _/    _/   _/        _/                _/      _/   _/          
_/_/_/_/  _/_/_/   _/_/_/  _/_/_/            _/      _/     _/_/_/     
```

Rolls dice in the form of "1d20 + 1d4 - 2d6 x3". Works up to a "dice character" width of 5 characters: 1d100, 20d99, 100d2.
Sensitive to white space between the dice and operators.

The above example rolls 1d20, adds 1d4, subtracts 2d6, and then repeats that 3 times, reporting the results in a gloriously unformatted manner:


```
 Input >
1d20 + 1d4 - 2d6 x3
 1d20           13 total=          13
 1d4             3 total=           3
 2d6             4           6 total=          10
 ----------------------------------------
 Total for roll           1 is           6
 ----------------------------------------
 1d20            1 total=           1
 1d4             2 total=           2
 2d6             6           5 total=          11
 ----------------------------------------
 Total for roll           2 is          -8
 ----------------------------------------
 1d20           19 total=          19
 1d4             1 total=           1
 2d6             3           6 total=           9
 ----------------------------------------
 Total for roll           3 is          11
 ----------------------------------------

 Total of all           3 rolls is           9
```

Calling this a monte carlo dice roller is probably a stretch, but it does use random numbers (shocking, I know).

Compile with gfortran. 
Help text available with input of `h`, `H`, or `help`.
Quit by unplugging your desktop, with `ctrl-c`, or with input of `q` or `Q`

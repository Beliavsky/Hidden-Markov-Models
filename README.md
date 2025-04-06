### Hidden Markov Models

Sample output:

```
#obs: 1000000

State parameters:
                 mean          sd
State 1        3.0000      1.0000
State 2        0.0000      3.0000
State 3       -2.0000     10.0000

State frequencies:
            empirical theoretical        diff
State 1        0.4627      0.4634     -0.0007
State 2        0.2195      0.2195      0.0000
State 3        0.3178      0.3171      0.0007

Theoretical transition probabilities:
From state 1:    0.7000  0.1000  0.2000
From state 2:    0.2000  0.5000  0.3000
From state 3:    0.3000  0.2000  0.5000

Empirical transition probabilities:
From state 1:    0.6989  0.1002  0.2009
From state 2:    0.1988  0.5001  0.3010
From state 3:    0.3011  0.1994  0.4995

max absolute deviation:   0.0012

observation stats
      mean        sd      skew      kurt       min       max
    0.7450    6.2582   -1.0858    4.2518  -47.4013   43.9155
```

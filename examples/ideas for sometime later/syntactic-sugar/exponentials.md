
Suppose that we have defined processes `proc q(x:A){...};;` and `proc r(x:?A){...};;`. The general understanding of an exponential session `x:?A` is that it can be copied (contraction), instantiated as a linear session (dereliction) and discarded (weakening). Therefore, the following programs should all type check: 

 ```
    proc p1(x:?A){ par{r(x) || r(x)} };;        // contraction 
    proc p2(x:?A){ q(x) };;                     // dereliciton 
    proc p3(x:?A){ () };;                       // weakening 
 ```
In the current implementation p1 and p2 do not type check, whereas p3 does.  

Both of them can be translated to processes in the core language (with dyadic contexts): 

```
    proc p1(x:?A){ ?x; par{ cut{fwd! y x |y:?A| r(y)} || 
                            cut{fwd! z x |z:?A| r(z)} }  };; 

    proc p2(x:?A){ ?x; call x(y); q(y) };; 

    proc p3(x:?A){ ?x;() };;                      
```

These translations introduce some runtime costs: they involve the introduction of the why-not operation `?x` which 
corresponds to server activation and blocks waiting for a server to be ready to be called. Sometimes the server is not even used (in p3). Furthermore, to deal with contraction we have to duplicate the server which with a 
forwarder construct `fwd!` which then needs to be activated again in the body of the processes `r(y), r(z)`. 

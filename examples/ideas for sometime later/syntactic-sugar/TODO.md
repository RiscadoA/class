
1.  Add alternative syntax  `y =<-x; P` for `recv x(y);P`.

2.  Add alternative syntax `y <- x; P` for `send x(y);P`.

3.  Add alternative syntax `y <- x.P; Q` for `send x(y.P);Q`. 

4.  Add alternative syntax `y <- #l;P` for ` #l y; P`. 

5.  Add support for variadic send-and-choice `y <- A1 ... An; P`
    where  `n >= 1` and `A1 ... An` are sessions names / labels. 
    The translation is done as follows: 
        `y <- A1 ... An; P = y <- A1; P` if `n = 1`; 
        `y <- A1 ... An;P = y <- A1; y <- A2 ... An; P`, otherwise. 

6.  Add support for variadic receive `x1 ... xn =<- y;P` 
    where `n >= 1` and `x1 ... xn` are session names. 
    The translation is done as follows: 
        `x1 ... xn =<- y; P = x1 =<- y1;P` if `n = 1`; 
        `x1 ... xn =<- y;P = x1 =<- y; x2 .. xn =<- y; P`, otherwise. 

7.  Add support for nested case 
    ```
        case x of {
            |#A11 A12 ... A1n_1: P1, 
            |#A21 A22 ... A2n_2: P2, 
            ...
            |#An1 An2 ... Akn_n: Pn 
        }
    ```
    where `Aij`are labels. 

    The translation is done as follows.
    If `n_1 = n_2 = n_n = 1`, we are done. 
    
    Suppose `n_1 > 1` (we can always rearrange rows).
            
    Rewrite the nested `case` in the form 
     ```
        case x of {
            |#A11 A12 ... A1n_1: P1, 
            |#B11 B12 ... B1m_1: Q1, 
            |#B21 B22 ... B2m_2: Q2, 
            ...
            |#Bm1 Br2 ... Bmm_m; Qm, 
            |#C11 C12 ... C1s_1: R1, 
            |#C21 C22 ... C2s_2: R2, 
            ...
            |#Ck1 Ck2 ... Ckk_k: Rk
        }
    ```
    such that `B11 = B21 = ... = Bm1 = A11` and
    `C11 != A11, C21 != A11 ... Ck1 != A11`. 

    The translation aborts and flags an error if 
    either `m_1 = 1` or `m_2 = 1` or ... `mm_m = 1`.            
    
    The nested case is translated as 
    ```
        case x of {
            |#A11: 
                case x of {
                    |#A12 ... A1n_1: P1, 
                    |#B12 ... B1m_1: Q1, 
                    ...
                    |#Bm2 ... Bmm_m: Qm
                }
            |#C11 C12 ... C1s_1: R1, 
            |#C21 C22 ... C2s_2: R2, 
                ...
            |#Ck1 Ck2 ... Ckk_k: Rk
        }
    ```

    Apply the tranlation to the remaining nested cases (the order does not matter).
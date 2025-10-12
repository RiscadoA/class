# CLASS: Syntactic Sugar 

Here we describe some syntact sugar constructs that will make programs in CLASS more readable / less verbose. 

## 1 Inference of `close` and `wait`  

There's already support for inferring coweakening of coaffine resources and weakening of exponentials. Why do we not offer support for inference of close operations, which would allows us to write 

    proc p(x: send lint; close){
        send x(2);
        ()
    };; 

instead of 

    proc p(x:send lint; close){
        send x(2);
        close x 
    };; 

Similarly, we could have automatic inference of wait, so we could write 

    proc q(x:recv colint; wait){
        recv x(n);
        println("Got " + n);
        ()
    };; 

instead of 

    proc q(x:recv colint; wait){
        recv x(n);
        println("Got " + n);
        wait x;
        ()
    };; 

Actually, this inference could be applied to types as well, so that we would write 

    send lint 
instead of 

    send lint; close 

## 2 Inference of `()` (inaction)

So that we could write 

    proc p(x: wait){
        wait x
    };; 

instead of 

    proc p(x: wait){
        wait x;
        ()
    };; 

## 3 Send, Receive, Choice and Case syntax 

* Use `y =<- x; P` for `recv x(y);P`. 
* Use `x <-y; P` for `send x(y);P`. 
* Use `x <- y.P; Q` for `send x(y.P);Q`. 
* Use `x <- #l;P` for `#l x;P`. 
* Use `case <-x{|#l1;P1 |#l2;P2}`for `case x of {|#l1;P1 |#l2;P2}`. 

## 4 Variadic send/choice and receive

We can write `l <- #Cons a tail` instead of 
```
l <- #Cons; 
l <- a
l <- tail 
```

Similarly, we can write `n1 n2 n3 =<- x` instead of 
```
n1 =<- x;
n2 =<- x;
n3 =<- x
```

## 5 Exponentials 

* A session `s` of type `!A` is introduced by binding a process definition to a session
    ```
    proc s <- p(s,s1,...,sn);
    q
    ```
    where `p` is a previously defined process `proc p(s:A, s1:A1,...,sn:An){...};;`. 

    It is syntactic sugar for 

    ```
    cut{
        !s(c);
        p(c,s1,...,sn)
        |s:?~A| 
        q
    };;
    ```

* There is no separation of linear and unrestricted zones in the superfical language, nor process constructs `!x(y);P`, `call x(y);P`, `?x;P`.  

* Sessions of type `?A` can be freely duplicated and discared. Duplication can be either `sequential` or `parallel`. For example 
    ```
        proc p1(x:?send Int){//discarded 
            println("sent nothing, did not use x") 
        };;

        proc p2(x:?send Int){ //parallel duplication 
            fork p1(x); p1(x)
        };;

        proc p3(x:?send Int){ //sequential duplication 
            x <- 5; //after, x types with ?send Int again 
            x <- 7;
            x <- 10
        }
    ```

    With the current language syntax we can write p1, p2 and p3 as follows: 
    ```
        proc p1(x:?send lint; close){ //there is already support for inference of ?x
            println("sent nothing, did not use x"); () 
        };;

        /*
            currently, the only way of duplicating a session is by moving it to the 
            unrestricted context wiht ?x. 

            but p1 depends on a session of type ? in the linear part of the context, 
            so we have to use the exponential forwarder to bring the session back. 

            a lot of work just to compose two simple processes... 
        */
        proc p2(x:?send lint; close){
            par {
                cut{
                    fwd! y x 
                    |y:?send lint; close|
                    p1(y)
                }
                ||
                cut{
                    fwd! y x
                    |y:?send lint; close|
                    p1(y)
                }
            }
        };; 

        proc p3(x:?send lint; close){
            call x(x0);
            send x0(5);
            call x(x1);
            send x1(7);
            call x(x2);
            send x2(10);
            par{close x0 || close x1 || close x2}
        };;
    ``` 

## 6 Basic types should be exponential by default 

Basic types like integers, booleans, strings should be exponential by default. 

A lot of the examples use !lint. So I propose havign a single Int exponential type which can 
be freely copied and discarded. 

## 7 Disposable types 

* A type `A` is disposable if it has an associated method `dispose(x:A)`.

* Exponential types `?A` are disposable, they have a default method `dispose(x:?A){ () };;` that simply does not use the session. 

* If, after type checking a process, the type checker sees that a session `x:A`was not consumed it checks that `A` is disposable, triggering an error if it's not, and completing the process by adding a call `dispose(x:)`. 

* When type checking a reference cell `cell c(P)`, the type checker checks that if a session `x` occurs free in `P` with type `A`, then `~A`is disposable. At runtime, the interpreter disposes a reference cell by invoking the corresponding `dipsose` method on all diposable sessions on which the session depends. This makes cell types diposable as well. 

* With disposable types we can implement the discarding mechanism currently offered with the `affine / coaffine` sessions. An affine process of the form `affine x;P` is just asserting that all the sessions on which it depends are disposable. Disposable types give us more flexibility since we can define richer disposable behaviours than the current approach which blindly and mechanically gets rid of coaffine sessions. Furthermore, it alleviates us of the burden of the affine annotations both at the process and the type levels. 

* The units close and wait are diposable: 
    ```
        dispose(x:close){
            close x 
        };; 

        dipose(x:wait){
            wait x 
        };; 
    ```
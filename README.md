# Safe Session-Based Concurrency with Shared Linear State (Artifact)

This document describes the companion artifact for the paper `Safe 
Session-Based Concurrency with Shared Linear State`. The artifact 
consists of a type checker and an interpreter for language CLASS - 
a session-typed, higher-order, core language that supports concurrent 
computation with shared linear state, and which is described in 
detail in the paper.

The artifact is distributed as a Docker image that bundles the source
code, all its dependencies, all the examples from the paper and several
other examples that showcase the expressiveness of language CLASS, 
as well as several test suites. 

Authors: Pedro Rocha and Luís Caires


[![CC BY 4.0][cc-by-shield]][cc-by]

This work is licensed under a
[Creative Commons Attribution 4.0 International License][cc-by].

[![CC BY 4.0][cc-by-image]][cc-by]

[cc-by]: http://creativecommons.org/licenses/by/4.0/
[cc-by-image]: https://i.creativecommons.org/l/by/4.0/88x31.png
[cc-by-shield]: https://img.shields.io/badge/License-CC%20BY%204.0-lightgrey.svg


## Getting Started Guide 

1. After uncompressing the supplied archive file, you should obtain a 
directory with the following contents: 

* `Dockerfile` -- Configuration file and setup script for the docker image.
* `README.md`  --  The readme file with instructions for getting started and step-by-step 
                   instructions to use the implementation. 
* `LICENSE.txt` -- Creative Commons Atrribution 4.0 International License. 
* `bin/` -- Folder containing the compiled Java .class files. 
* `examples/pure` -- Folder containing all the examples involving only pure computations,  
                     without shared state.  
* `examples/state` -- Folder containing the examples involving shared state.  
* `lib/` -- Folder containing initial definitions loaded by the artifact. 
* `src/` -- Folder containing the Java source files. 
* `CLLSj` -- Executable file to start the interactive REPL. 

2. You should have a Docker runtime installed, installation instructions are 
   available in https://www.docker.com. Ensure that the Docker daemon is running. 

3. Run the command `docker build --tag=class .` to build a docker image tagged `class`.

4. To run the docker image tagged `class`, execute the command `docker run -it class`. 
   This will start the image and its bash shell in a directory identical to the root of
   the artifact archive.

5. To run the artifact as an interactive REPL (inside the running docker image),
   execute `./CLLSj`. You can try the basic flag example 
   described in the introduction of the paper (pp.4) by inputting the REPL 
   with the command `include "examples/state/flag.clls";;` which will kind and 
   type check all the type and process definitions included in the file. Then, you can
   type in the REPL `main();;`, which will execute process `main()`.
   The REPL will output either the log 
   
   > 1: wins.  
   >  2: looses. 

   or the log 
   
   > 2: wins.  
   > 1: looses.

   depending on which process wins to turn the flag on. 

   For convenience, we have provided files "examples/pure/run-all.clls" and 
   "examples/state/run-all.clls" that type check and run all the pure and stateful
   examples, respectively. For example, to try all the stateful examples, you can execute 
   `include "examples/state/run-all.clls";;` in the REPL, after which you should 
   obtain a log in which all the examples are successfully defined and run. 
   
6. To exit the REPL execute `quit;;`. To exit the docker image, execute `exit`.
    
## Step-by-Step Instructions 

The artifact consists of type checker and interpreter for language CLASS, 
written in Java. It demonstrates the feasibility of our design and, pragmatically, 
validates and guides the development of many complex programs:
we code inductive datatypes using either primitive recursion or using System-F style 
encodings, linked data structures (e.g. linked lists, binary search trees),
shareable concurrent mutable ADTs (e.g.  stacks, counters,  
imperative queues and buffered channels), resource synchronisation methods
(e.g. fork-join barriers, dining philosophers, and Hoare-style monitors) 
and several test suites.

In particular, all the examples presented in the paper are validated by the 
implementation. In the following list, we map all the examples in the paper 
to the corresponding files in the artifact: 
* flag, presented in pp. 4, is in examples/state/flag.clls. 
* toggle, presented in pp. 17-18, is in examples/state/toggle.clls. 
* linked list, presented in pp. 18-19, is in examples/state/linked-lists.clls. 
* buffered channel, presented in pp. 19-20 is in directory 
  examples/state/buffered-channel. This directory contains three files, namely:
  server.clls, in which we define ADT operations on the server side; client.clls, 
  in which we define the operations on the client side; and tests.clls, that defines
  a series of tests. This is the organisation used for the remaining shared mutable
  ADTs. 
* dining philosophers, presented in pp.20-21, is in examples/state/dining-philosophers.clls. 
* barrier, presented in pp. 21-22, is in examples/state/barrier.clls. 
* Hoare-style monitor, presented in pp.22-23, is in examples/state/hoare-monitor.clls. 

When presenting the examples in the paper we did some simplifications in the code for
the purpose of presentation. 

We will now present the commands to interact with the REPL and the concrete syntax
of the implementation language. We conclude with some final remarks. 

### Interacting with the REPL (read-eval print loop)

The REPL expects the following top-level
commands:
	
	1. type id(X1,...,Xn){ A };;
	2. type rec id(X1,...,Xn){ A };;
	3. type corec id(X1,...,Xn){ A };;
	4. proc id<X1,..., Xn>(x1:A1, ... xm:Am ; y1:B1, ..., yk:Bk){ P };; 
	5. proc rec id<X1,..., Xn>(x1:A1, ... xm:Am ; y1:B1, ..., yk:Bk){ P };; 
	6. proc unsafe_rec id<X1,..., Xn>(x1:A1, ... xm:Am ; y1:B1, ..., yk:Bk){ P };; 
	7. P;; 
	8. trace P;; 
	9. include "filename";; 
	10. quit;; 
	
Command (1): top level type definition.

Defines (if it kindchecks) the (possibly) parametric type A
with name id and parameters  X1, ... Xn ( n >= 0).

Example: when executing 
`type Fun(a,b) { recv ~ a; b };;` we obtain  

>Type Fun: defined.

Command (2) and (3) are similar to (1) but allows inductive (mu X. A) and 
coinductive (nu A. A) type definitions, respectively.  

For example, 
```
 type rec Nat{  
        choice of {  
            |#Z: close  
            |#S: Nat  
        }  
    };;
    
 proc zero(n:Nat){
    #Z n; close n
 };; 
```  

defines the inductive session type of the natural numbers and
process `zero(n)` that encodes the natural zero on session `n`. 

(4) Defines (if it typechecks) the (possibly) parametric process 
with name id  with linear  parameters  x1:A1,..., xm:Am (m >0) and
unrestricted (exponential) parameters y1:B1,..., yk:Bk (k>=0).

The process is typechecked as defined by the typing judgment

P |- x1:A1,..., xm:Am ; y1:B1,..., yk:Bk

Example: if we type in the REPL
```
	proc id<a>(f:Fun(a,a))  
	{  
		recv f (x:~a);fwd x f  
	}  
	;; 
```
we obtain 

> Process id: defined.  


Command (5) is same as (4) but allows corecursive process definitions, e.g.: 

```
    proc rec double(n:~Nat, m:Nat)
    {
        case n of {
            |#Z: wait n; zero(m)
            |#S: cut{ double(n,k) |k:~Nat| #S m; #S m; fwd m k }
        }
    };;
```
defines a corecursive process that doubles natural `n` and outputs the answer on `m`.
The type checker ensures each that corecursive call is done in a 
session that hereditarily descends from the corecursion parameter.
 
Command (6) allows to define an unsafe corecursion definition, 
in which the hereditary check of (5) is turned off.

Commands (7) and (8) type check process P in the current
environment given by prior definitions and execute it, (8) with tracing enabled.

Example: if we input in the REPL
```
	cut {   id<lint>(f) 
	        |f:~Fun(lint,lint)|
            send f (10); 
            println "here is "+f; 
            () 
        };;
```
we obtain the log:

> here is 10


Command (9) includes and processes all the commands included
in the specified file. Filename are given as strings.

Example: `include "examples/state/stack/tests.clls";;`. 

Command (10) quits the REPL. 
 
By convention, we use the filename extension `.clls` for files written in the 
implementation language.

Single-line comments are written as follows

```
//this is a single-line comment
``` 

whereas multiple-line comments are written as
```
/*
this is 
a multiple-line
comment
*/
```


proc test0() {

  println (2); ()

};;

proc test2() {

   cut {

   close n    

   | n : wait |

   wait n; println ("done!"); ()

   }

};;



proc test2() {

   ccut {

   send n (p. close p); close n    

   | n : recv wait; wait |

   recv n(x:wait); wait x; wait n; ()

   }

};;
 


### Concrete Syntax

The concrete syntax of types is given by:: 

	A,B ::=

	lint |

	colint |

	lbool |

	colbool |

	lstring |

	colstring | 

	close |

	wait | 

	send A; B |  ( A (x) B ) 

	recv A; B |  ( A (p) B ) 

	choice of {|#l1:A1 | ... |#ln: An} | ( (+){|#l1:A1 | ... |#ln: An} ) 

	offer of {|#l1:A1 | ... | #ln:An} | ( &{|#l1:A1 | ... |#ln: An} ) 

	!A |

	?A | 

	sendty X; A  | ( Exists X. A )

	recvty X; A |  ( Forall X. A ) 
	
	affine A | (/\ A) 
	
	coaffine A| (\/ A) 
	
	state A | (Sf A) 
	
	usage A | (Uf A) 
	
	statel A | (Se A)

	usagel A | (Ue A) 

    { A } |

	id<T1,...,Tk>(A1,...,An) 

The correspondence to the types of the paper are written in parenthesis. 
	         
Except for the basic type constants, concrete types correspond to the types 
described in the paper. We have basic type constants for linear integers 
`lint`, linear booleans `lbool`and linear strings `lstring`, as well as their 
dual `colint`, `colbool` and `colstring`.  Unrestricted versions of
these types may be defined using ! and ? type constructors.

Choice label identifiers must start by a hash character`#`.

The concrete syntax of process terms is given by:: 
	
    P, Q ::=

	print M; P |

	println M; P |
	
	if M { P }{ Q } |

	let x M |

	let! x M |
	
	id<A1, ... , An>(x1, ..., xm) |
	
	() |

	par {P || Q } |

	share x { P || Q } |

	fwd x y |

	cut { P |x:A| Q } | 

	close x |

	wait x; Q |
	
	case x of {|#l1:P1 | ... | #ln:Pn} | #li;Pi |
    
	send x(y:A. P); Q |

	recv x(y:A);Q |
	
	!x(y:A);P |

	call x(y:A);P | ?x; P |	        

	sendty x(A);P | recvty x(X);P | 
	
	send x(M);P |
	
	affine x; P |
	
	use x; P | 
	
	discard x | 
	
	take x(y:A);P | 
	
	put x(y:A. P); Q | 
	
	put x(M);P |

	cell x(y:A. P) |
	
	cell x(M) |

	release x | 

Construct `id<A1,..., An>(x1,.. ; .,xm)` spawns the 
defined process `id` with type arguments "A1,..., An
and channel name arguments `x1,..; .,xm`. The `;` in the
argument list separates the linear parameters from the unrestricted
parameters, according to the definition of id.

Except for the first five lines, which refer to expressions
for basic datatypes, described in more detail below, all the constructs of the
concrete process syntax are the fundamental ones introduced in our
paper. 

Access to affine (via use process construct) and exponential 
names (via why-not ? process construct) is inferred 
by our type checker, so it does not need to be explicitly indicated.  

Process `print M ; P` prints message `M` and continues as `P`. Process 
`println M;P` prints message `M`, starts a newline, and continues as `P`. 
Process `if M { P } { Q }` evaluates boolean expression `M`. If the result 
is `true`, it continues as `P`. Otherwise, the result is `false` and it 
continues as `Q`. We support ML-style let-bindings. Construct `let x M`
binds expression `M` to channel `x` to be used linearly (type as either 
`lint`, `lbool` or `lstring`). Construct `let! x M` binds expression `M` 
to channel `x` to be used persistently (typed as either `!lint`, `!lbool` or 
`!lstring`).

The concrete syntax of basic value expressions is given by:: 

	M :: =   n | true | false | str | x | (M) |
		    -M | M + M | M - M | M * M | M / M |
		    !M | M and M | M or M | 
		    M == M |  M != M | M < M | M > M  

where `n` is any integer, `true` and `false` denote the boolean constants, 
`str` is any string and `x` is an identifier. We support the usual arithmetic operations 
of negation, addition, subtraction, multiplication and division. We support 
the boolean constructs of logical negation `!`, logical `and` and logical `or`. 
We can compare two expressions for value equality `==` and non-equality `!=`. 
We can compare two integers with the usual relational operators "<" and ">".

The operator of addition `+` is overloaded: we can concatenate two string
expressions, obtaining a string. We can also concatenate a string with an 
integer expression, in which case, the integer is first converted to a string.

We also provide  support for  expressions in the constructs send, cell and put: 
    	
    send x(M);P     cell x(M)      put x(M);P

The elaboration phase of our interpreter expands this in the basic
language before typechecking,.

For convenience, our concrete syntax supports multi-ary par and
multi-ary cut (which are abbreviations of the expected associative
composition of par and cut).

`par {P1 || ... || Pn}`
`cut { P1 |x1:A1| ... |xn:An| Pn+1}`

Multi-ary cut associates (conventionally) to the right.  


### Some Remarks

In our concrete process syntax terms are written with bound 
names type-annotated, which from a pragmatic point of view guides the
type checking algorithm and eases the task of writing programs.
Currently, inference of annotations is provided, so that
only  parameters in process definitions and cut bound names need to 
be explicitly type annotated.

The supported language also includes pragmatic extensions
(native basic datatypes int, boolean and string), that may be anyway
encoded in our the fundamental linear logic based language using 
recursive/corecursive session types. 

The type checker deals with the multiplicatives by lazy context splitting.
The interpreter uses java.util.concurrent.* , using primitives such as
fine-grained locks and condition variables to emulate the synchronous interactions
of CLASS sessions and a cached thread pool to manage the life cycle of short-lived 
threads. Cell deallocation is implemented by reference counting, incremented on 
each share and decremented on each release. Forwarding redirects the clients of a 
shared cell through a chain of forwarding pointers.
  




# The Linear Session Abstract Machine (Artifact)

This document describes the artifact for the Linear Session Abstrtact Machine
as presented in papers "The Session Abstract Machine" (ESOP24)), and "The Linear Session Abstract Machine" (journal submission).
The artifact consists of a type checker and a proof-of-concept implementation for 
the Linear Session Abstract Machine (SAM), as an alternative backend to the CLASS
language prototype. The artifact is currently distributed as a zip archive that bundles the jvm code, the examples from the papers, and many other examples that showcase the abstract machine. 

Authors: Luís Caires and Bernardo Toninho

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

* `README.md`  --  The readme file with instructions for getting started and step-by-step 
                   instructions to use the implementation. 
* `LICENSE.txt` -- Creative Commons Atrribution 4.0 International License. 
* `bin/` -- Folder containing the compiled Java .class files. 
* `examples/` -- Folder containing many examples.
* `examples/sam` -- Folder containing examples specific to exercise the SAM
* `lib/` -- Folder containing initial definitions loaded by the CLASS interpreter.
* `LSAM` -- Executable file to start the interactive REPL. 

2. Assuming you have a Java runtime installed and the path to the `java` executable appropriately configured, you 
   may run the artifact as an interactive REPL use the `./LSAM` command.
   For convenience, we have provided the file `examples/sam/sam-all.clls` 
   that type check and runs all the examples. 
   For example, to run all the examples defined in the journal paper you can type 
   `include "examples/sam/paper-examples-sam.clls";;` in the REPL. 
   Note that some examples produce little visual output since they do not use the `print` or `println` statements.
   
6. To exit the REPL execute `quit;;`. 
    
## Step-by-Step Instructions 

The artifact consists of type checker and interpreter for the SAM, 
written in Java. The artifact is developed on top of the CLASS
interpreter and typechecker by Rocha and Caires (available [here](https://luiscaires.org/software/)).
The artifact supports the main language in the paper, with a few convenience extensions such as type and process definitions, primitive *linear* integers, strings and booleans. 
Note that these primitive values are typed linearly and so must be used exactly once 
(a common idiom is to use them in print statements).
The interpreter supports execution in both reduction style and using the execution rules of the SAM.
By default, processes are executed in reduction style. To execute a process in the SAM, use the command `sam P;;` where `P` is a process.

In particular, all the examples presented in the papers are validated by the 
implementation. In the following list, we provide details on the examples 
provided in the artifact: 
* `examples/sam/sam-all.clls` - Runs all examples.
* `examples/sam/paper-examples-sam.clls` - Simple examples from the journal submission, augmented with `print` statements, includin for mixed sequential/concurrent cuts.
* `examples/sam/sam-exponentials.clls` - Examples exercising the linear logic exponentials.
* `examples/sam/sam-forwarders.clls` - Examples exercising the forwarder construct.
* `examples/sam/sam-mult.clls` - Examples exercising the linear logic multiplicative (types tensor and par) connectives.
* `examples/sam/sam-sums.clls` - Examples exercising the linear logic additive (types or and with) connectives.
* `examples/sam/sam-units.clls` - Examples exercising the units (types 1 and bottom).
* `examples/sam/arithmetic-server-sam.clls` - A simple replicated server offering arithmetic operations, interacting with two clients. The implementation of the clients and the server is given in `examples/pure/arithmetic-server.clls` and then executed in the SAM via the `sam` command.
* `examples/sam/exponential-iso-sam.clls` - Processes that witness the isomorphisms `! (A & B) == !A x !B  and ?(A + B) == !A par ?B`
* `examples/sam/recursion-for-free-sam.clls` - Encoding of inductive and coinductive types via session polymorphism (cf. Wadler's "recursion for free" in System F).
* `examples/sam/naturals-sam.clls` - Example with  church numerals as recursive processes using primitive recursive types.
* `examples/sam/ackermann-sam.clls` - some intensive computation.
* `examples/sam/bit-counter-sam.clls` - Infinite precision boolean counter, using a process network.
`examples/sam/tree-sam.clls` -  Binary search trees.
`examples/sam/toplas.clls` -  Some general tests.
`examples/sam/sam-affine.clls` -  Some general tests for affine types (unpublihsed extension of the LSAM with affine types)
`examples/sam/sam-sieve-sum.clls` -  Sieve of Eratosthenes using a process network and co-recursion and choice to represent an infinite lazy stream)
`examples/sam/sam-sieve-affc.clls` -  Sieve of Eratosthenes using a process network and co-recursion and affinity to represent an discardable infinite lazy stream)
`examples/sam/ccut.clls` -  Some simple tests for mixed sequential/concurrent cuts.

==


We will now present the commands to interact with the REPL and the concrete syntax
of the implementation language. We conclude with some final remarks. 

### Interacting with the REPL (read-eval print loop)

The examples below assume the same REPL session.

The REPL expects the following top-level commands:
	
	1. `type id{ A };;`
	2. `proc id(x1:A1, ... xm:Am ; y1:B1, ..., yk:Bk){ P };;`
	3. `P;;`
	4. `sam P;;`
	5. `trace L;;`  
	6. `include "filename";;`
	7. `quit;;` 
	
Command (1): top level type definition.

Defines (if it kindchecks) the type A with name id.

Example: when executing 
`type tmenu {
    offer of {
        |#Neg:  recv ~lint; send lint; close 
        |#Add:  recv ~lint; recv ~lint; send lint; close
    }
};;` we obtain  

>Type tmenu: defined.

Notice that the CLASS system also suports recursive and corecursive type definitions, including mutual definitions using rec / corec annotations (see examples, e.g. naturals-sam, and others).

(2) Defines (if it typechecks) the process 
with name id  with linear  parameters  x1:A1,..., xm:Am (m >0) and
unrestricted (exponential) parameters y1:B1,..., yk:Bk (k>=0).

The process is typechecked as defined by the typing judgment

P |- x1:A1,..., xm:Am ; y1:B1,..., yk:Bk

Example: if we type in the REPL
```
	proc client1( ; s:~tmenu){
    call s(c);
    #Neg c;
    send c (v:lint. let v 2);
    recv c(m);
    wait c;
	println("CLIENT1 GOT NEG = " + m);
    ()
};;
```
we obtain 

> Process client1: defined.  

Notice that the CLASS system also suports recursive process definitions, including mutual definitions, using rec annotations.

We illustrate with Church numerals (many more examples in the distribution):

```
type rec Nat{
    choice of {
        |#Z: close      //zero
        |#S: Nat        //successor
    }
};;

proc rec add(n:~Nat, m:~Nat, r:Nat)
{
   case n of {
    |#Z: wait n;
         fwd m r
    |#S: #S r;
         add(n,m,r)
   }
};;
```

Commands (3) and (4) type check process P in the current
environment given by prior definitions and execute it.

The  main focus of this artifact is (4),  which executes the process P
using the Linear Session Abstract Machine semantics.

Command (3) executes the process using the multithreaded reduction
semantics previously developed for CLL.

Example: if we input in the REPL (usar o primeiro exemplo do paper ??)
```
	proc example1() {
	cut {
		send a(y. close y); println ("sent on a") ; close a 
		| a: ~ send close ; close |
		recv a(x); wait x; wait a; println ("Done with waiting");()
	    }
    };;
    sam example1();;

```
we obtain the log:

> sent on a
  Done with waiting

Command (5) enables tracing of the SAM execution with `trace 1` and disables it with `trace 0`. 
For instance, running `sam example1()` from above with `trace 1` active produces the output:

```
cut-op a SessionRecord@3a883ce7 size=2
send-op a SessionRecord@3a883ce7 @ 0
sent on a
clos-op a SessionRecord@3a883ce7 @ 1
recv-op a SessionRecord@3a883ce7 @ 0
clos-op y SessionRecord@4973813a @ 0
wait-op x SessionRecord@4973813a @ 0
wait-op a SessionRecord@3a883ce7 @ 1
Done with waiting
empty-op
```

Each line indicates the SAM execution rule selected at each step. In this example, following the rule
for cut, the send on a is executed, followed by the close on a. Each line also mentions the index into
the message buffer on the session record.


Command (6) includes and processes all the commands included
in the specified file. Filenames are given as strings.

Example: `include "examples/sam/sam-all.clls";;`. 

Command (7) quits the REPL. 
 
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

### Concrete Syntax


#### Types 

The concrete syntax of types is given by: 

	A,B ::=

	lint |

	colint |

	lbool |

	colbool |

	lstring |

	colstring | 

	close |   ( 1 )

	wait |    ( bottom )

	send A; B |  ( A (x) B ) 

	recv A; B |  ( A (p) B ) 

	choice of {|#l1:A1 | ... |#ln: An} | ( (+){|#l1:A1 | ... |#ln: An} ) 

	offer of {|#l1:A1 | ... | #ln:An} | ( &{|#l1:A1 | ... |#ln: An} ) 

	!A |

	?A |
 
    ~A |  (negation = duality)

	
The correspondence to the types of the paper are written in parenthesis when not immediate. 
	         
Except for the basic type constants, concrete types correspond to those 
described in the paper. We have basic type constants for linear integers 
`lint`, linear booleans `lbool`and linear strings `lstring`, as well as their 
dual `colint`, `colbool` and `colstring`.  Unrestricted versions of
these types may be defined using ! and ? type constructors.

Choice label identifiers must start by a hash character`#`.

#### Processes

The concrete syntax of process terms is given by: 
	
    P, Q ::=

	print M; P |

	println M; P |
	
	if M { P }{ Q } |

	let x M |

	let! x M |
	
	id(x1, ..., xm) |
	
	() |

	par {P || Q } |    (P || Q)

	fwd x y |

	cut { P |x:A| Q } | 

	close x |

	wait x; Q |
	
	case x of {|#l1:P1 | ... | #ln:Pn} 
	
	| #li;Pi 
    
	send x(y:A. P); Q |

	recv x(y:A);Q |
	
	!x(y:A);P |

	call x(y:A);P | ?x; P |	        

	send x(M);P |
 
    unfold x;P 
	
The correspondence to the processes of the paper are written in parenthesis when not immediate.

The first six constructs are basic extensions to the logical language to deal with values of primitive type.
Constructs `println M;P` and `print M;P` respectively print their (evaluated) 
argument expression `M` with or without an added new line, and then procceed as process `P`.
We also support a process-level `if` construct with the expected semantics (where both
the then and else branch processes must have the same typing) and auxiliary linear (`let x M`)
and unrestricted (`let! x M`) bindings of values to names (which must be accessed via a `cut`).
Such values must be either integers, booleans or strings (in their linear or unrestricted forms, respectively).

The concrete syntax of basic value expressions is given by:

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

Construct `id(x1,.. ; .,xm)` spawns the 
defined process `id` with channel name arguments `x1,..; .,xm`. The `;` in the
argument list separates the linear parameters from the unrestricted
parameters, according to the definition of `id`.
 
For convenience, our concrete syntax supports multi-ary independent parallel composition and multi-ary cut (which are abbreviations of the expected associative
composition of parallel and cut). 

`par {P1 || ... || Pn}`
`cut { P1 |x1:A1| ... |xn:An| Pn+1}`

Multi-ary cut associates (conventionally) to the right.  
Access to exponential names is inferred by our type checker and 
so we omit the `cut!` construct from the paper.

### Some Remarks

In our concrete process syntax terms are written with bound 
names type-annotated, which from a pragmatic point of view guides the
type checking algorithm and eases the task of writing programs.
Currently, inference of annotations is provided, so that
only  parameters in process definitions and cut bound names need to 
be explicitly type annotated.

We also note that since our syntax is that of CLL and so `cut` introduces only one 
name, shared by both processes. The type `A` in `cut { P |x:A| Q }` refers to the 
usage of channel `x` according to process `Q` (and so `P` uses `x` according to type `~A`).
This is flipped from the convention in the paper.

Our SAM proof-of-concept implementation is build using the JDK, on top
of the CLASS infrastructure  (see website linked above). 

The implementation follows the structure of a virtual machine
interpreter that adheres to the
environment-based semantics described in Section 3 of the paper. The top
execution loop starts in `SAM.java`, which calls the `samL()` method for
each
instruction: given a machine state, it executes the next instruction, and
returns the next machine state (essentially a pair (continuation,
env1, env2), where env1 is is the (linear) runtime environment (a spaghetti
stack) and env2 is a global enviroment storing process definitions. 

The implementation of the virtual machine covers all the (sequential) constructs in
the paper, and also basic polymorphic constructs (not discussed in the paper, 
but exemplified e.g. in the recursion-for-free-sam.clls example).

To establish a tighter correspondence with compilation techniques that
we plan to develop in future work, we  represent each session queue 
with a low level array-based imperative structure, akin to a stack frame in a funcional
language, allowing linear values to  passed in sessions. Thus, in our
case, session records behave as a kind of co-routine frames,
paving the way for very efficient execution strategies. The size of
such so-called SessionRecords is statically computed from the session
type. We are currently extending the SAM to handle recursive types,
concurrent constructs and mutable state (cf. [58,59]).

To inspect the sequence of instructions executed by the SAM whle
running a program we may switch tracing mode on using the 
following REPL command.

> trace 1;;

Tracing may be switched of with

> trace 0;;

Each line of a trace starts by the current instruction (as in the
code component of the SAM), we also indicate the channel name, the
respective SessionRecordid (as a JVM object) and the session record slot
being written or read.
The instructions tagged by [S-] correspond to the [S-] execution
of the SAM (see Figure 5) in the paper. 







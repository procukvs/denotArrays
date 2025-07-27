    Denotational semantics of one-dimensional arrays


An imperative programming language is considered that has integer scalar data 
and one-dimensional arrays. Every language program can input integer values, process them, and 
output integer values. Each variable a in this language is either an integer scalar variable a 
or an integer one-dimensional array a[k], where k>0 is a positive integer 
(the dimension of the array). Such an array has k elements, which are sequentially arranged 
and numbered as a[0], a[1], ..., a[k-1]. A program is a separate statement, usually a block 
with a description of local integer variables (scalar or arrays) and a list of statements – 
the body of the block. The main data processing is performed by the 
assignment operators v := e, input read v, output write e, conditional if (e) s, 
loop while (e) s, where v is a variable, e is an expression, s is an operator. 
All expressions e in the operators calculate a scalar integer value. Work with an array is 
carried out only element by element. If a[k] is an entire one-dimensional array, 
then access to an individual element of the array is performed by an indexing operation - 
a record of the form a[e]. The value of the integer expression e - must be an integer from 0 to k-1.
This is a specification of this programming language, which uses the 
extended Backus-Naur form to describe a specific syntax, and the tools of the functional 
language Haskell to describe other parts. Abstract syntax is defined by the types Program, 
Stmt, VarDef, Expr, Var, and Op, which describe, respectively, the syntactic constructs program, 
operator, variable declaration, expression, variable, and operation. Contextual conditions are 
predicates that impose additional context-dependent rules on abstract syntax objects.
Most contextual conditions are related to the correct use of variables in a program.
The leading predicate, which checks the contextual conditions of the program pr, 
is iswfProgram pr. 	The task of semantics is to introduce the denotations
("meanings") of the basic constructions of the language and the semantic functions that 
build the denotations of complex syntactic constructions from the denotations of their 
components, including the program.
         type Work = ([Integer], [Maybe Integer], [Integer]) 
 To describe the semantics of the language, a state is used - a data type of type Work. 
This is a tuple of three elements (inp, stg, out) that models the three sets of values 
with which the program works: the list of integers [Integer] inp contains the input data 
(input file), the list of values ​[Maybe Integer] stg stores the current values ​​of all program 
variables (memory), and the list of integers [Integer] out contains the resulting data (output file).
The value of a scalar program variable is an element of the list stg, and the value of 
an array of dimension k is k consecutive elements of the list stg.
If the current value of a variable is an integer v, then in the 
stg list its current value is displayed as Just v, and if the variable has not yet 
been assigned any value, then the current value is displayed as Nothing.
The presence of blocks means that in specific programs one name can refer to different 
values ​​(description of a scalar variable in the inner and outer blocks). In addition,
one array name is associated with several values ​​(elements with different indices).
To associate variable names with their denotations (numbers of elements of the stg list,
where the current value of the variable is stored), an environment is used - a value of type Env.
	                 type Env = [(String,(Maybe Int, Int))]
If int a is a scalar variable with which the element (a,(Nothing,3)) is bound in env, then stg[3] 
contains the current value of a. If int b[4] is an array with which the element (b,(Just 4,7))
is bound in env, then stg[7] contains the current value of element b[0], stg[6] contains
the value of b[1], stg[5] the value of b[2], stg[4] the value of b[3], respectively.
    The semantic functions getLocation, eLocation and extEnv perform work with the 
enotation of a variable - a given type (Maybe Int, Int). The function getLocation s env 
by the name of the variable s in the environment env generates information about the 
part of the list stg, allocated for storing the value of the variable s, 
returning the pair (n,k): n - the size of the section (for the scalar variable 1) and k 
the addresses of its beginning. The function eLocation (s, mei) env by the name of 
the variable s and the index mei calculates 
the address of the memory element where the value is stored in the environment env.
If s is a scalar variable, then mei == Nothing and the address of its location 
is the value getLocation s env. Otherwise, mei == Just ei, the value of index i 
is calculated from the expression ei. If this value satisfies the 
dimensionality of the array, the address is calculated, otherwise an “Index” error is generated.
The extEnv vs b env function is used to form the block denotation (Block vs sts). 
The function allocates memory for the values ​​of the local variables vs declared 
in the block and, based on the current size b of the stg list, 
forms the denotations of the local variables vs, expanding the environment env.
	   The denotations of the expressions Expr and the operators Stmt of the language are, 
respectively, state change functions of the type Work -> Integer and Work -> Work, 
and the denotation of the program Program is a function of the type [Integer] -> [Integer].
These denotations build the semantic functions eExpr, iStmt and iProgram, 
respectively. Most of the semantic functions for accessing the denotations 
of variables use the environment as an additional parameter.
    	All functions: context conditions, denotations and semantic functions are pure functions. Using 
Haskell tools, a function called parsePLL is built that implements syntactic analysis, which connects 
concrete and abstract syntax. It is shown how by combining the functions parseProgram, iswfProgram and iProgram 
you can get a procedural language interpreter - a pure function with the name interpret.


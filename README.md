# SchemeLoopUntangling
UntanglingScheme is a program I wrote in Spring 2015 in the Scheme programming language.  

The program takes in the trip code of a tangled loop and tells the user whether or not the tangled loop can be untangled.  
Untangling is accomplished by first checking the legality of the trip code, searching and removing any type 1 and type 2 
Reidemeister moves from the trip code, and then reporting the algorithm results back to the user.

The first step to untangling a given loop is to check and see whether the loop’s trip code is actually legal.  
This is accomplished by checking the number of times each crossing name appears in the trip code and by checking that each 
crossing type is either under or over.  If a crossing name does not appear exactly two times in the trip code then the trip 
code is not legal.  Similarly, if the crossing types are any except under or over then the trip code is not legal.

The next step to untangling the loop is to remove all type 1 and type 2 Reidemeister moves from the loop.  A type 1 Reidemeister 
move is found when a given crossing pair shares the same crossing name but different crossing types.  Type 1 moves signify that 
the loop can be “untwisted” at that crossing point thus reducing the tangle’s trip code.  Finding type 2 moves is a little more 
difficult.  A potential type 2 Reidemeister move is found when a given crossing pair doesn’t share the same crossing name but 
does share the same crossing type.  If a potential type 2 move is found then the algorithm will search the trip code for a 
matching type 2 pair with opposite crossing types.  If a type 2 move is found then it signifies that the four crossings can 
be “pulled apart” at those crossing points thus reducing the tangle’s trip code.

The final step is to report back to the user the results of the untangling algorithm.  If after running the trip code through 
the algorithm it becomes empty then it means that the tangle can be untangled.  However, if the trip code does not contain any 
type 1 or type 2 Reidemeister moves but still contains crossings after being put through the algorithm then the tangle is a 
knot and cannot be untangled without cutting the loop.

I tried to make using UntanglingScheme as simple and as straightforward as possible.  
After launching the program the user must call the startProgram function and pass in the trip code to start the algorithm.  
This can be accomplished by simply typing (startProgram ‘(<trip code goes here>)) and pressing the enter key.  

An example execution of the program might look like:
> (startProgram ‘((a o)(a u)(b o)(c o)(c u)(b u)(d o)(e o)(d u)(e u)))
> The knot can be untangled!

Or alternatively:
> (define tripcode ‘((a o)(a u)(b o)(c o)(c u)(b u)(d o)(e o)(d u)(e u)))
> (startProgram tripcode)
> The knot can be untangled!



Language Design - version 1

Specify by example.  Not formal but enough to get you started.

Idea:   
  - numeric values are described by a series of digit-words
  - basic math operators
  - basic control flow 

1) Explicit variable declaration
2) Types are integer & fraction
3) Declare variable with initial value

```
define integer w as negative nine
define integer r as one seven  
define fraction  f as  two three over five 
define integer k```


4)  Var must have value before reading from


5) the math operations & assignment statements

put two into k

```
add three and eight into x
subtract four from nine into y
add two four and x into x
multiply one one by three into y
max of x and y and one two into m```

add negative five and nine into w

6) allow embedded expressions to nest computation

add  [add one and two]  [multiply two by five]  into x


7) define fraction operations by different names

fadd, fsubtract, fmultiply  
   - produces float result
   - operands can be int or float


8) simple output
```
printI r
printF f
printS "hello world"
printNL```

Do we want to allow multiple integers at once with ands?

9) control flow
```
if
while (  cond ) 
  stmts
block```


relational operators:  greater equal less  

10) sample of the loop

```
set x to one five
while ( x greater zero )
  printI x
  subtract five from x into x
block```


11) Wrap a complete program
    begin
    end

12) this should be legal?

```
begin
define integer w as three two
define integer great 
multiply w by one six into great 
printI great 
printNL 
end```



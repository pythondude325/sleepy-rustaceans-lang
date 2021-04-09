G4 Key:

g4a.good:
    Line 2: Check that multi-letter variable names work
    Line 4: Check embedded expressions work
    Line 5: Print an integer works
    Line 10-12: If() block functions correctly
    Line 16: print string works
    And overall basic math functions work

g4b.good:
    Many similar goals as g4a.good, line 10 features an even more embedded expression,
    lines 11-15 feature a while loop to check.

g4a.bad:
    Line 4: Can't store into an undeclared variable
    Line 5: Can't add two undeclared variables
    Line 6: Can't print undeclared integer

g4b.bad:
    Line 6: All integers are declared, however r is uninitialized so we can't take max.
    Line 7: Can't print integer x using a PrintS statement.

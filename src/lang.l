%%
\-?[0-9]*\.[0-9]+ "FLOAT"
[0-9]+ "INT"
\+ "+"
\* "*"
\- "-"
/ "/"
\[ "["
\] "]"
(zero|one|two|three|four|five|six|seven|eight|nine) "DIGIT"
[\t ]+ ;
. "UNMATCHED"
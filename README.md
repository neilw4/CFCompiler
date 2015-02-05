# Compiler for the CF language by Kevin Hammond

#Design Decisions
- I decided to interpret while (condition) don't {dostuff} as while (not condition) do {dostuff}.
- I decided to implement the comefrom statement by making each label a goto statement and each comefrom a label. This requires that every label has exactly one comefrom statement and vice versa. The alternative is to perform an additional pass over the AST to determine which labels are unneccessary.

#Features
- I have introduced an extra AST node type - the Line class - to represent a line of code. If a node within the line fails to compile, the error message can include the line number and context.
- If a stat fails to compile within a Stats node, the compiler will log the failure and attempt to continue compiling. This allows it to detect multiple compilation failures while compiling.
lines
- One problem with the comefrom statement is that the stack may be different at the label to what is at the comefrom. To stop this, I decided that label-comefrom pairs must be within the same stats list, where the stack will always be the same.
- Comefrom statements may also allow a variable declaration to be skipped (e.g. l;new x=5;comefrom l;print x would cause problems). The Stats compiler class checks for this possibility and logs an error if it happens.

# Future Work
- Tiling - boolean operations (among others) such as the not operation would benefit from a tiling algorithm to remove useless code.
- More control flow in the language, such as if statements.
- Integration of a lexer.

#Build Instructions
- To compile: sbt package
- To test: sbt test
- To run code coverage while testing: sbt clean scoverage:test

# Lexical Analyzer & Parser
Ada program that performs a scan and parse of a code file, checking its adherence to syntax rules<br/>
- Uses recursive descent method
- Outputs any errors found, along with a description and line number
## Parser Design
First, the Lex package will scan the input file, then store the results.
The parser will then be used to check the lexemes against the rules of the mini-grammar.
It will check for incorrect syntax or identifiers that are out of place using the
recursive descent method. The parser gets the current lexeme and checks it with the
expected or required symbol according to the mini-grammar. The next lexeme is also
checked and if they do not match the required symobls or identifiers then this will prompt
an error. A counter is then updated to track the number of errors found and is then
printed on the screen. An appropriate error message and the line number is also printed in
the error report.
## Symbol Table Design
The symbol table is stored using a vector which holds the lexeme ID's and line numbers.
A counter is also used to track the total number of lexemes that are currently held in
the vector. The current lexeme in the parser is tracked using a pointer in the vector
as well. This data can be manipulated using the functions in the lexdata files.
For example, the current lexeme can be returned or one can be added to the vector.
The symbol table provides the tools needed for the parser to check the rules of the
mini-grammar.
## Build & Usage
This project can be built by using the GNAT compiler.<br>
**Usage:**
```gnatmake <filename>```<br>
 <br>
For this project, the 'parse.adb' file will be used to link the other files.<br>
**Enter the following:**
```gnatmake parse.adb```<br>
This will create an executable file named 'parse' that can be ran with './'<br>
**Enter the following to run the executable:**
```./parse```<br>
This will then prompt you to enter the input program file, then the output file
<br>
Command line switches can be used by entering /E, /L, and /S with the file names.<br>
**Echo file as it is processed:**<br>
```./parse <InputFile> <OutputFile> /E```<br>
**Output lexemes to file that follows the /L**<br>
```./parse <InputFile> <OutputFile> /L <OutputLexFile>```<br>
**Output symbol table to file that follows the /S**<br>
```./parse <InputFile> <OutputFile> /S <OutputSymFile>```<br>

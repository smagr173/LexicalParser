--<****D* Lex
-- NAME
--  lex.ads - Lexeme proccessor that takes an input and puts proccessed lexemes in
--  output
-- AUTHOR
--  Stephen Magrowski
-- DESCRIPTION
--  Scans an input file that represents a program that follows a
--  simplistic programming language grammar, producing an output file 
--  that is an ordered list of the symbol types found in the input file. 
-->****

with Text_IO;                  use Text_IO;
with OpenFile;		       use OpenFile;
with Ada.Command_Line;         use Ada.Command_Line;
with Ada.Strings.Unbounded;    use Ada.Strings.Unbounded;
with Ada.Text_IO.Unbounded_Io; use Ada.Text_IO.Unbounded_IO;

--<****c* Lex/Lex
-- SYNOPSIS
--  Lexicographical analysis of an input file and outputs to another file
-->****

--<****h* Lex/Lex/Lex.Header
-- SYNOPSIS
--  Proccesses an input file and sends lexemes to an output file
-->****
package lex is

    --<****t* Lex/Lex/Char_Type
    -- DESCRIPTION 
    --  Enumerated type declaration for the symbols used in the grammar
    -->****
    type Char_Type is (Beginsym, Progsym, ProgName, Endsym, Decsym, Colon, Semicolon,
                     Comma, Typesym, Readsym, Writesym, Operator, EqualSign,
                     MinusSign, PlusSign, MultiSign, DivisionSign,
                     Lparen, Rparen, Id, Number, Nullsym, InvalidSym);

    --<****t* Lex/Lex/Class_IOEnumType
    -- DESCRIPTION
    --  Allows Enumeration types to be declared. Used for proccessing lexemes.
    -->****
    package Class_IO is new Ada.Text_IO.Enumeration_IO(Char_Type);
    use Class_IO;
    
    --<****g* Lex/Lex/File_Type
    -- DESCRIPTION
    --  File_Type operations
    -->****

    --<****f* Lex/Lex/File_Type.WriteFile
    -- DESCRIPTION
    --  Takes Enumerated token and outputs it to the file
    -- MEMBER FUNCTION TYPE
    --  Mutator
    -- PARAMETERS
    --  File_Type  OutFile: Output file - Input Output
    --  Char_Type  Class: Enumerated token - Input
    -->****
    procedure WriteFile(OutFile: in out File_Type; Class: in Char_Type);

    --<****f* Lex/Lex/File_Type.ReadFile
    -- DESCRIPTION
    --  Reads input file and sets up output file also handled console commands
    -- MEMBER FUNCTION TYPE
    --  Mutator
    -- PARAMETERS
    --  File_Type  InFile: Input file - Input Output
    --  File_Type  OutFile: Output File - Input Output
    -->****
    procedure ReadFile(InFile: in out File_Type; OutFile: in out File_Type);
    
    --<****g* Lex/Lex/Lex.Functions
    -- DESCRIPTION
    --  Functions for use in lexeme proccessor
    -->****

    --<****f* Lex/Lex/Lex.Functions.Is_Numeric
    -- DESCRIPTION
    --  Takes a string and returns whether it is a number.
    --  This code was adapted from an online resource documented below.
    --  https://rosettacode.org/wiki/Determine_if_a_string_is_numeric#Ada
    -- MEMBER FUNCTION TYPE
    --  Mutator
    -- PARAMETERS
    --  String  Item: Unknown token - Input
    -- RETURNS
    --  True  -  Numeric
    --  False -  Non-Numeric
    -->****
    function Is_Numeric(Item : in String) return Boolean;   

    --<****f* Lex/Lex/Lex.Functions.ConvertToken
    -- DESCRIPTION
    --  Takes a token and converts it into an Enumeration type and passes 
    --  into WriteFile
    -- MEMBER FUNCTION TYPE
    --  Mutator
    -- PARAMETERS
    --  Unbounded_String  Token: Generated token - Input
    --  File_Type         OutFile: Output file - Input Output
    --  Integer           TokenLen: Length of token - input
    -->****
    procedure ConvertToken(Token: in Unbounded_String; OutFile: in out File_Type;
                         TokenLen: in Integer);

    --<****f* Lex/Lex/Lex.Functions.MakeTokenHelper
    -- DESCRIPTION
    --  Sends token to convert token and resets string for next token
    -- MEMBER FUNCTION TYPE
    --  Mutator
    -- PARAMETERS
    --  File_Type         OutFile: Output file - Input Output
    --  Unbounded_String  Str: Token string - Input Output
    --  Integer           StrCount: String length - Input Output
    -->****
    procedure MakeTokenHelper(OutFile: in out File_Type;
                            Str: in out Unbounded_String;
                            StrCount : in out Integer);
			    
    --<****f* Lex/Lex/Lex.Functions.MakeToken
    -- DESCRIPTION
    --  Takes raw file input and converts it into tokens and passes token into
    --  convert token procedure
    -- MEMBER FUNCTION TYPE
    --  Mutator
    -- PARAMETERS
    --  File_Type        InFile: Input file - Input Output
    --  File_Type        OutFile: Output file - Input Output
    -->****
    procedure MakeToken(Infile: in out File_Type; OutFile: in out File_Type);
    
    --<****f* Lex/Lex/Lex.Functions.LexProccessor
    -- DESCRIPTION
    --  Main procedure for the program. The file reads in a program
    --  and performs a lexicographical analysis
    -- MEMBER FUNCTION TYPE
    --  Mutator
    -->****
    procedure LexProccessor;
    
    private
    --<****v* Lex/Lex/Lex.EchoMode
    -- DESCRIPTION
    --  Flag to see if echo mode is enabeld
    -->****
    EchoMode:            Boolean := False;
    
    --<****v* Lex/Lex/Lex.LexemeOutputMode
    -- DESCRIPTION
    --  Flag to see if lexeme output mode is enabeld
    -->****
    LexemeOutputMode:    Boolean := False;
    
    --<****v* Lex/Lex/Lex.SymbolTableOutput
    -- DESCRIPTION
    --  Flag to see if symbol table output mode is enabeld
    -->****
    SymbolTableOutput:   Boolean := False;
    
    --<****v* Lex/Lex/Lex.SpecialOutputFile
    -- DESCRIPTION
    --  Output file in the case any speical mode is needed to output
    -->****
    SpecialOutputFile:   File_Type;
    
end lex;
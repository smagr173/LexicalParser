--<****D* LexData
-- NAME
--  lexdata.ads - Holds all information related to parser and lexemes
-- AUTHOR
--  Stephen Magrowski
-- DESCRIPTION
--  Holds the symbol table, lexemes and their required information
-->****

with Text_IO;                  use Text_IO;
with OpenFile;		       use OpenFile;
with Ada.Command_Line;         use Ada.Command_Line;
with Ada.Strings.Unbounded;    use Ada.Strings.Unbounded;
with Ada.Text_IO.Unbounded_IO; use Ada.Text_IO.Unbounded_IO;
with Ada.Integer_Text_IO;      use Ada.Integer_Text_IO;
with Ada.Containers.Vectors;   use Ada.Containers;
with lex;                      use lex;

--<****c* LexData/LexData
-- SYNOPSIS
--  Lexicographical analysis of an input file and outputs to another file
-->****

--<****h* LexData/LexData/LexData.Header
-- SYNOPSIS
--  The package containing the LexData information and functions/procedures.
--  Holds The symbol table, the lexemes, and their required information
-->****
package LexData is
    --<****v* LexData/LexData/MaxIDConst
    -- DESCRIPTION
    --  This is the default amount of ID's the symbol table is able to hold.
    --  User may change this if more ID's are needed to be stored.
    -->****
    MaxID : constant Integer := 100;

    --<****t* LexData/LexData/Class_IOEnumType
    -- DESCRIPTION
    --  Allows Enumeration types to be declared. Used for proccessing Lexemes.
    -->****
    package Class_IO is new Ada.Text_IO.Enumeration_IO(Char_Type);
    use Class_IO;

    --<****t* LexData/LexData/Lexeme_Vectors
    -- DESCRIPTION
    --  Vector that holds all of the lexemes in the order they were proccesed.
    -->****
    package Lexeme_Vectors is new Vectors(Natural, Char_Type);

    --<****t* LexData/LexData/LexLineNum_Vectors
    -- DESCRIPTION
    --  Vector that hold all line numbers of the Lexemes that were proccessed.
    -->****
    package LexLineNum_Vectors is new Vectors(Natural, Integer);

    --<****g* LexData/LexData/Array.Functions
    -- DESCRIPTION
    --  Array operations
    -->****
    
    --<****f* LexData/LexData/Array.Functions/Array.Add_ID
    -- DESCRIPTION
    --  Attempts to add the Identifier into the ID_holder array
    -- MEMBER FUNCTION TYPE
    --  Mutator
    -- PARAMETERS
    --  Unbounded_String  InputID: The input identifier - Input
    -->****
    procedure Add_ID(InputID: in Unbounded_String);

    --<****f* LexData/LexData/Array.Functions/Array.Find_ID
    -- DESCRIPTION
    --  Tries to find the InputID in the IDHolder array
    -- MEMBER FUNCTION TYPE
    --  Mutator
    -- PARAMETERS
    --  Unbounded_String  InputID: The input identifier - Input
    -- RETURNS
    --  Boolean: True if found, False if not
    -->****
    function Find_ID(InputID: in Unbounded_String) return Boolean;
  
    --<****f* LexData/LexData/Array.Functions/Array.Output_ID
    -- DESCRIPTION
    --  Sends the symbol table into an output file.
    -- MEMBER FUNCTION TYPE
    --  Mutator
    -- PARAMETERS
    --  File_Type  SpecialOutputFile: The output file - Input Output
    -->****
    procedure Output_ID(SpecialOutputFile: in out  File_Type);

    --<****g* LexData/LexData/LexData.Functions
    -- DESCRIPTION
    --  Lexdata operations
    -->****
    
    --<****f* LexData/LexData.Functions/LexData.Set_PastBegin
    -- DESCRIPTION
    --  Sets boolean value to indicate the Lexeme Proccessor is past the Begin.
    -- MEMBER FUNCTION TYPE
    --  Mutator
    -->****
    procedure Set_PastBegin;

    --<****f* LexData/LexData/LexData.Functions/LexData.Get_PastBegin
    -- DESCRIPTION
    --  Returns a boolean value to see if lexeme proccessor is past the Begin.
    -- MEMBER FUNCTION TYPE
    --  Getter
    -- RETURNS
    --  Boolean: True if past, otherwise false
    -->****
    function Get_PastBegin return Boolean;

    --<****f* LexData/LexData/LexData.Functions/LexData.Add_LineNumber
    -- DESCRIPTION
    --  Adds line number into the LexemeLineNum Vector.
    -- MEMBER FUNCTION TYPE
    --  Mutator
    -->****
    procedure Add_LineNumber;

    --<****f* LexData/LexData/LexData.Functions/LexData.Get_LineNumber
    -- DESCRIPTION
    --  Returns a boolean value to see if Lexeme Proccessor is past the Begin. 
    -- MEMBER FUNCTION TYPE
    --  Getter
    -- RETURNS
    --  Boolean: The line number of the lexeme in the symbol table
    -->****
    function Get_LineNumber return Integer;

    --<****f* LexData/LexData/LexData.Functions/LexData.Increase_LineCount
    -- DESCRIPTION
    --  Adds 1 to the LineCount variable
    -- MEMBER FUNCTION TYPE
    --  Mutator
    -->****
    procedure Increase_LineCount;

    --<****g* LexData/LexData/LexemeVector.Functions
    -- DESCRIPTION
    --  Array operations
    -->****
    
    --<****f* LexData/LexData/LexemeVector.Functions/LexemeVector.Output_Lexemes
    -- DESCRIPTION
    --  Sends the proccessed lexemes into an output file.
    -- MEMBER FUNCTION TYPE
    --  Mutator
    -- PARAMETERS
    --  File_Type  SpecialOutputFile: The output File - Input Output
    -->****
    procedure Output_Lexemes(SpecialOutputFile: in out File_Type);

    --<****f* LexData/LexData/LexemeVector.Functions/LexemeVector.Add_Lexeme
    -- DESCRIPTION
    --  Adds a lexeme to the Lexeme Vector as well as the line number.
    -- MEMBER FUNCTION TYPE
    --  Setter
    -- PARAMETERS
    --  Char_Type  Lexeme: The lexeme that needs to be added - Input
    -->****
    procedure Add_Lexeme(Lexeme: in Char_Type);

    --<****f* LexData/LexData/LexemeVector.Functions/LexemeVector.Get_Lexeme
    -- DESCRIPTION
    --  Returns the current lexeme at the CurrentVectorIdx
    -- MEMBER FUNCTION TYPE
    --  Getter
    -- RETURNS
    --  Char_Type  : The lexeme that is at the current index
    -->****
    function Get_Lexeme return Char_Type;

    --<****f* LexData/LexData/LexemeVector.Functions/LexemeVector.CheckNext_Lexeme
    -- DESCRIPTION
    --  Returns the current lexeme at the (CurrentVectorIdx + 1)
    -- MEMBER FUNCTION TYPE
    --  Inspector
    -- RETURNS
    --  Char_Type  : The lexeme that is at the current index + 1
    -->****
    function CheckNext_Lexeme return Char_Type;

    --<****f* LexData/LexData/LexemeVector.Functions/LexemeVector.MoveNext_Lexeme
    -- DESCRIPTION
    --  Moves pointer to the next lexeme in the Vector
    -- MEMBER FUNCTION TYPE
    --  Mutator
    -->****
    procedure MoveNext_Lexeme;
    
    private
    --<****v* LexData/LexData/Array.IDHolder
    -- DESCRIPTION
    --  Holds ID's in the symbol table
    -->****
    IDHolder:          array(1..MaxID) of Ada.Strings.Unbounded.Unbounded_String;
    
    --<****v* LexData/LexData/Array.IDLineNum
    -- DESCRIPTION
    --  Holds ID's line numbers in the symbol table
    -->****
    IDLineNum:         array(1..MaxID) of Integer;
    
    --<****v* LexData/LexData/LexData.LineCount
    -- DESCRIPTION
    --  Keeps track of what line the parser is on
    -->****
    LineCount:         Integer := 0;
    
    --<****v* LexData/LexData/LexData.IDCount
    -- DESCRIPTION
    --  Keeps track of how many ID's are in the symbol table
    -->****
    IDCount:           Integer := 1;
    
    --<****v* LexData/LexData/LexemeVector.LexemeVector
    -- DESCRIPTION
    --  Vector that holds all the Lexemes
    -->****
    LexemeVector:      Lexeme_Vectors.Vector;
    
    --<****v* LexData/LexData/LexemeVector.LexemeLineNum
    -- DESCRIPTION
    --  Vector that holds all the Lexemes line numbers
    -->****
    LexemeLineNum:     LexLineNum_Vectors.Vector;
    
    --<****v* LexData/LexData/LexemeVector.VectorCounter
    -- DESCRIPTION
    --  Keep track of how many Lexemes are in the vector
    -->****
    VectorCounter:     Integer := 0;
    
    --<****v* LexData/LexData/LexemeVector.CurrentVectorIdx
    -- DESCRIPTION
    --  Ponter for what lexeme is being proccessed in the parser
    -->****
    CurrentVectorIdx:  Integer := 0;
    
    --<****v* LexData/LexData/LexData.PastBegin
    -- DESCRIPTION
    --  If the lexeme reader has made it past the begin symbol or not
    -->****
    PastBegin:         Boolean := False;
    
end LexData;

--<****D* Parser
-- NAME
--  parse.adb - Parses a program
-- AUTHOR
--  Stephen Magrowski
-- DESCRIPTION
--  Takes an input file then scans and parses it using recursive descent method.
--  The parser will analyze the lexemes produced by the scanner.
-->****

--<****c* Parser/Parser
-- SYNOPSIS
--  Takes an input file, scans it and parses it using recursive descent method.
--  The parser will analyze the lexemes produced by the scanner
-->****
WITH Text_IO;                  USE Text_IO;
WITH lex;                      USE lex;
WITH LexData;                  USE LexData;
WITH Ada.Integer_Text_IO;      USE Ada.Integer_Text_IO;
WITH Ada.Containers.Vectors;   USE Ada.Containers;
with GNAT.OS_Lib;

--<****h* Parser/Parser/Parser.Header
-- SYNOPSIS
--  Scans and parses a program, checking its adherence to the syntax rules
--  set out in the Minii-Grammar.
-->****
procedure Parse is

    --<****v* Parser/Parser/NumofErrors
    -- DESCRIPTION
    --  Records the number of errors the parser found.
    -->****
    NumofErrors : Integer := 0;

    --<****g* Parser/Parser/Functions
    -- DESCRIPTION
    --  Parser functions for productions
    -->****

    --<****f* Parser/Parser/Functions.ErrorReport
    -- SYNOPSIS
    --  Helper function to report the line number of errors and add to NumofErrors
    -- MEMBER FUNCTION TYPE
    --  Helper
    -->****
    procedure ErrorReport is
    begin
        Put(Item => Get_LineNumber, Width => 2);
        New_Line;
        NumofErrors := NumofErrors + 1;
        if (NumofErrors > 7) then
            Put("Too many errors... Terminating");
            New_Line;
            GNAT.OS_Lib.OS_Exit(0);
        end if;
    end ErrorReport;

   --<****f* Parser/Parser/Productions.ListVar
    -- DESCRIPTION
    --  Handles productions from the <list var> grammar rule
    -- MEMBER FUNCTION TYPE
    --  Helper
    -->****
    procedure ListVar is
    begin
        if (Get_Lexeme = ID) and then (CheckNext_Lexeme = Comma) then
            MoveNext_Lexeme;
            MoveNext_Lexeme;
            ListVar;
        elsif (Get_Lexeme = ID) and then (CheckNext_Lexeme /= Comma) then
            MoveNext_Lexeme;
        elsif (Get_Lexeme /= ID) then
            Put(Item => "Error! Expected ID on line: ");
            ErrorReport;
        end if;
    end ListVar;

    --<****f* Parser/Parser/Productions.DecPart
    -- DESCRIPTION
    --  Handles productions from the <dec part> grammar rule
    -- MEMBER FUNCTION TYPE
    --  Helper
    -->****
    procedure DecPart is
    begin
        ListVar;
        if(Get_Lexeme = Colon) then
            MoveNext_Lexeme;
        else
            Put(Item => "Error! Colon expected on line: ");
            ErrorReport;
        end if;

        if (Get_Lexeme = Typesym) and then (CheckNext_Lexeme = Semicolon) then
            MoveNext_Lexeme;
            if (Get_Lexeme = Semicolon) and then (CheckNext_Lexeme = Beginsym) then
               MoveNext_Lexeme;
               Put(Item => "Error! Expected variable name on line: ");
               ErrorReport;
            else
               MoveNext_Lexeme;
               DecPart;
            end if;
        elsif (Get_Lexeme = Typesym) and then (CheckNext_Lexeme = Beginsym) then
            MoveNext_Lexeme;
        end if;
    end DecPart;

    -- Prototype so factor can access exprssion.
    procedure Expression;
    
    --<****f* Parser/Parser/Productions.Factor
    -- DESCRIPTION
    --  Handles productions from the <factor> grammar rule
    -- MEMBER FUNCTION TYPE
    --  Helper
    -->****
    procedure Factor is
    begin
        if (Get_Lexeme = Id) or (Get_Lexeme = Number) then
            MoveNext_Lexeme;
        else
            Expression;
        end if;
    end Factor;

    --<****f* Parser/Parser/Productions.Term
    -- DESCRIPTION
    --  Handles productions from the <Term> grammar rule
    -- MEMBER FUNCTION TYPE
    --  Helper
    -->****
    procedure Term is
    begin
        Factor;
        if (Get_Lexeme = MultiSign) or (Get_Lexeme = DivisionSign) then
            MoveNext_Lexeme;
            Factor;
        elsif (Get_Lexeme = PlusSign) or (Get_Lexeme = MinusSign) then
            Factor;
        end if;
    end;

    --<****f* Parser/Parser/Productions.Expression
    -- DESCRIPTION
    --  Handles productions from the <expression> grammar rule
    -- MEMBER FUNCTION TYPE
    --  Helper
    -->****
    procedure Expression is
    begin
        Term;
        if (Get_Lexeme = PlusSign) or (Get_Lexeme = MinusSign) then
            MoveNext_Lexeme;
            Term;
        elsif (Get_Lexeme = Endsym) then
            null;
        else
            Put("Error! Expression is not correct on line: ");
            ErrorReport;
        end if;
    end Expression;
    
    procedure Statement;

    --<****f* Parser/Parser/Productions.AssignPart
    -- DESCRIPTION
    --  Handles productions from the <assign> grammar rule
    -- MEMBER FUNCTION TYPE
    --  Helper
    -->****
    procedure AssignPart is
    begin
        if (Get_Lexeme = ID) and then (CheckNext_Lexeme = EqualSign) then
            MoveNext_Lexeme;
            MoveNext_Lexeme;
            Expression;
        elsif (Get_Lexeme = ID) and then (CheckNext_Lexeme /= EqualSign) then
            Put("Error! Missing equals sign in assignment on line: ");
            ErrorReport;
            MoveNext_Lexeme;
            Statement;
        end if;
    end AssignPart;

    --<****f* Parser/Parser/Productions.InputPart
    -- DESCRIPTION
    --  Handles productions from the <Input> grammar rule
    -- MEMBER FUNCTION TYPE
    --  Helper
    -->****
    procedure InputPart is
    begin
        if (Get_Lexeme = Readsym) and then (CheckNext_Lexeme = Lparen) then
            MoveNext_Lexeme;
            MoveNext_Lexeme;
            ListVar;
        elsif (Get_Lexeme = Readsym) and then (CheckNext_Lexeme /= Lparen) then
            Put("Error! No left parenthesis detected on line: ");
            ErrorReport;
        end if;
        if (Get_Lexeme = Rparen) then
            MoveNext_Lexeme;
        else
            Put("Error! No right parenthesis detected on line: ");
            ErrorReport;
        end if;
    end InputPart;

    --<****f* Parser/Parser/Productions.OutputPart
    -- DESCRIPTION
    --  Handles productions from the <output> grammar rule
    -- MEMBER FUNCTION TYPE
    --  Helper
    -->****
    procedure OutputPart is
    begin
        if (Get_Lexeme = Writesym) and then (CheckNext_Lexeme = Lparen) then
            MoveNext_Lexeme;
            MoveNext_Lexeme;
            ListVar;
        elsif (Get_Lexeme = Writesym) and then (CheckNext_Lexeme /= Lparen) then
            Put("Error! No left parenthesis detected on line: ");
            ErrorReport;
        end if;
        if (Get_Lexeme = Rparen) then
            MoveNext_Lexeme;
        else
            Put("Error! No right parenthesis detected on line: ");
            ErrorReport;
        end if;
    end OutputPart;

    --<****f* Parser/Parser/Productions.Statement
    -- DESCRIPTION
    --  Handles productions from the <statement> grammar rule
    -- MEMBER FUNCTION TYPE
    --  Helper
    -->****
    procedure Statement is
    begin
        if (Get_Lexeme = Readsym) then
            InputPart;
        elsif (Get_Lexeme = Writesym) then
            OutputPart;
        elsif (Get_Lexeme = ID) then
            AssignPart;
        else
            Put(Item => "Error in statement on line: ");
            ErrorReport;
            MoveNext_Lexeme;
            Statement;
        end if;
    end Statement;

    --<****f* Parser/Parser/Productions.StatementPart
    -- DESCRIPTION
    --  Handles productions from the <statement part> grammar rule
    -- MEMBER FUNCTION TYPE
    --  Helper
    -->****
    procedure StatementPart is
    begin
        Statement;
        if (Get_Lexeme = Semicolon) and then (CheckNext_Lexeme = Endsym) then
            MoveNext_Lexeme;
            Put("Error! Extra semicolon on line: ");
            ErrorReport;
        elsif (Get_Lexeme = Semicolon) and then (CheckNext_Lexeme /= Endsym) then
            MoveNext_Lexeme;
            StatementPart;
        end if;
    end StatementPart;

    --<****f* Parser/Parser/Productions.Program
    -- DESCRIPTION
    --  Handles the whole parser from start to finish giving subcalls to different
    --  productions.
    -- MEMBER FUNCTION TYPE
    --  Main Function
    -->****
    procedure Program is
    begin
        if (Get_Lexeme = Progsym) then
            MoveNext_Lexeme;
            if (Get_Lexeme = Id) then
                MoveNext_Lexeme;
            else
                Put("Error! No program name on Line: ");
                ErrorReport;
            end if;
        else
            Put(Item => "Error! No program symbol on line:  ");
            ErrorReport;
            Put(Item => "Critical Error... Terminating");
            New_Line;
            GNAT.OS_Lib.OS_Exit(0);
        end if;
        if (Get_Lexeme = Decsym) then
            MoveNext_Lexeme;
            DecPart;
        else
            Put(Item => "Error! No declare symbol on line: ");
            ErrorReport;
            DecPart;
        end if;
        if (Get_Lexeme = Beginsym) then
            MoveNext_Lexeme;
            StatementPart;
        elsif (Get_Lexeme = ID) then
            Put("Error! Misspelled type symbol on line: ");
            ErrorReport;
            MoveNext_Lexeme;
        elsif (Get_Lexeme = Beginsym) then
            MoveNext_Lexeme;
            StatementPart;
        else
            Put("No begin symbol on line: ");
            ErrorReport;
            Put(Item => "Critical Error... Terminating");
            New_Line;
            GNAT.OS_Lib.OS_Exit(0);
        end if;
        if (Get_Lexeme = Endsym) or (CheckNext_Lexeme = Endsym) then
            Put("");
        else
            Put(Item => "Missing end. on line: ");
            ErrorReport;
        end if;
    end Program;

begin
    lex.LexProccessor;
    Program;
    if (NumofErrors >= 1) then
        Put_Line("Program did not compile!");
        Put("Number of Errors: ");
        Put(NumofErrors, Width => 2);
    else
        Put("Successfully compiled");
    end if;
end Parse;

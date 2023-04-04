----------------------------------------------------------------------
--  Author:      Stephen Magrowski                                  --
--  Created on:  May 11, 2020                                       --
--  Due Date:    May 20, 2020                                       --
--  Course:      CSC 310-010                                        --
--  Professor:   Dr. Spiegel                                        --
--  Assignment:  #3                                                 --
--  Filename:    lex.adb                                            --
--  Purpose:     This Ada program performs a lexicographical        --
--               analysis of a program. It takes an input file      --
--               entered by the user, then outputs the result       --
--               of the scan.                                       --
--                                                                  --
----------------------------------------------------------------------

with Text_IO;                  use Text_IO;
with OpenFile;		       use OpenFile;
with Ada.Command_Line;         use Ada.Command_Line;
with Ada.Strings.Unbounded;    use Ada.Strings.Unbounded;
with Ada.Text_IO.Unbounded_Io; use Ada.Text_IO.Unbounded_IO;
with Ada.Integer_Text_IO;      use Ada.Integer_Text_IO;
with lexData;                  use lexData;

package body lex is

  ---------------------------------------------------------------
  --
  -- Function name: WriteFile
  -- Description: Takes enumerated token and outputs it to the file
  --
  ---------------------------------------------------------------
  procedure WriteFile(OutFile: in out File_Type; Class: in Char_Type) is
  begin
    if not (Class = Nullsym) then
       Class_IO.Put(OutFile, Class);
       Text_IO.Put_Line(OutFile, " ");
    end if;
  end WriteFile;

  ---------------------------------------------------------------
  --
  -- Function name: ReadFile
  -- Description: Handles input/output files and the console commands
  --
  ---------------------------------------------------------------
  procedure ReadFile(InFile: in out File_Type; OutFile: in out File_Type) is
  begin
      if Argument_Count = 0 then
         OpenReadFile(InFile);
         OpenWriteFile(OutFile);

      elsif Argument_Count = 2 then
         Text_IO.Open(File => InFile, Mode => Text_IO.In_File, Name => Argument(1));
         Text_IO.Create(File => OutFile, Mode => Text_IO.Out_File, Name => Argument(2));

      elsif Argument_Count = 3 then
         Text_IO.Open(File => InFile, Mode => Text_IO.In_File, Name => Argument(1));
         Text_IO.Create(File => OutFile, Mode => Text_IO.Out_File, Name => Argument(2));
         if (Argument(3) = "/E" or Argument(3) = "/e") then
                EchoMode := True;
         end if;
      elsif Argument_Count = 4 then
         Text_IO.Open(File => InFile, Mode => Text_IO.In_File, Name => Argument(1));
         Text_IO.Create(File => OutFile, Mode => Text_IO.Out_File, Name => Argument(2));
         if (Argument(3) = "/L" or Argument(3) = "/l") then
             LexemeOutputMode := True;
             Text_IO.create(File => SpecialOutputFile,
                            Mode => Text_IO.Out_File, Name => Argument(4));
         end if;
         if (Argument(3) = "/S" or Argument(3) = "/s") then
             SymbolTableOutput := True;
             Text_IO.Create(File => SpecialOutputFile,
                            Mode => Text_IO.Out_File, Name => Argument(4));
         end if;
      end if;
  end ReadFile;

  ---------------------------------------------------------------
  --
  -- Function name: Is_Numeric
  -- Description: Takes a string and returns whether it is a number
  -- This code was adapted from an online resource documented below.
  -- https://rosettacode.org/wiki/Determine_if_a_string_is_numeric#Ada
  --
  ---------------------------------------------------------------
  function Is_Numeric(Item : in String) return Boolean is
       NumVal:   Float;
    begin
       NumVal := Float'Value(Item);
       return True;
    exception
       when others =>
          return False;
  end Is_Numeric;

  ---------------------------------------------------------------
  --
  -- Function name: ConvertToken
  -- Description: Takes a token and converts it to an enumeration type and passes 
  -- into WriteFile
  --
  ---------------------------------------------------------------
  procedure ConvertToken(Token: in Unbounded_String; OutFile: in out File_Type;
                         TokenLen: in Integer) is
     Class:    Char_Type := Nullsym;
     TokenStr: String(1..TokenLen);
  begin
     TokenStr := TO_String(token);

     if (TokenLen > 0)  then
        if (TokenStr = "program") then
           Class := Progsym;
        elsif (TokenStr = "begin") then
           Class := Beginsym;
           LexData.set_PastBegin;
        elsif (TokenStr = "end.") then
           Class := Endsym;
        elsif (TokenStr = "dec") then
           Class := Decsym;
        elsif (TokenStr = "':'") then
           Class := Colon;
        elsif (TokenStr = "';'") then
           Class := SemiColon;
        elsif (TokenStr = "','") then
           Class := Comma;
        elsif (TokenStr = "int") or (TokenStr = "real") then
           Class := Typesym;
        elsif (TokenStr = "Read") then
           Class := Readsym;
        elsif (TokenStr = "Write") then
           Class := Writesym;
        elsif (TokenStr = "'='") then
           Class := EqualSign;
        elsif (TokenStr = "'+'") then
           Class := PlusSign;
        elsif (TokenStr = "'-'") then
           Class := MinusSign;
        elsif (tokenStr = "'*'") then
           Class := MultiSign;
        elsif (TokenStr = "'/'") then
           Class := DivisionSign;
        elsif (TokenStr = "'('" ) then
           Class := Lparen;
        elsif (TokenStr =  "')'" ) then
           Class := Rparen;
        elsif ((TokenStr =  " " ) or (TokenStr = "")) then
           Class := Nullsym;
        else
           if (Is_Numeric(TokenStr)) then
               Class := Number;
           else
               if (Get_PastBegin = False) and then (Find_ID(Token) = False) then
                   Class := id;
                   ADD_ID(token);
               elsif (Get_PastBegin = True) and then (Find_ID(Token) = True) then
                   Class := Id;
               else
                   Class := InvalidSym;
               end if;
           end if;
        end if;
     end if;
     WriteFile(OutFile,Class);
     lexData.Add_Lexeme(Class);
  end ConvertToken;

  ---------------------------------------------------------------
  --
  -- Function name: MakeTokenHelper
  -- Description: Sends token to convert token procedure and resets String for next token
  --
  ---------------------------------------------------------------
  procedure MakeTokenHelper(OutFile: in out File_Type;
                            Str: in out Unbounded_String;
                            StrCount : in out Integer) is
  begin
     if (EchoMode = True) then
          Put(str);
     end if;
     ConvertToken(Str,OutFile,StrCount);
     Delete(Str,1,StrCount);
     StrCount := 0;
  end MakeTokenHelper;

  ---------------------------------------------------------------
  --
  -- Function name: MakeToken
  -- Description: Takes raw file input and converts it into tokens and passes token into
  -- convert token procedure
  --
  ---------------------------------------------------------------
  procedure MakeToken(Infile: in out File_Type; OutFile: in out File_Type) is
     Str:                  Unbounded_String := To_Unbounded_String("");
     Char, TempChar:       Character;
     EOL:                  Boolean;
     StrCount:             Integer := 0;
  begin
     while not Text_Io.End_Of_File(Infile) loop
        while not Text_IO.End_Of_Line(InFile) loop
           look_Ahead(File => InFile, Item => TempChar, End_Of_Line => EOL);
           if not EOL then
              Text_IO.Get(File => InFile, Item => Char);
              case Char is
                 when 'A' .. 'Z' | 'a' .. 'z' =>
                    Append(Str, Char);
                    StrCount := StrCount + 1;

                 when '0' .. '9' =>
                    Append(Str, Char);
                    StrCount := StrCount + 1;

                 when '=' | '*' | '+' | '-' | '/'  =>
                    MakeTokenHelper(OutFile,Str,StrCount);
                    ConvertToken(To_Unbounded_String(Character'Image(Char)),
                                 OutFile,3);
                      if (EchoMode = True) then
                        Put(Char);
                      end if;

                 when ' ' =>
                    MakeTokenHelper(OutFile,Str,StrCount);
                    if (EchoMode = True) then
                        Put(Char);
                    end if;
                 when '.' =>
                    Append(Str, Char);
                    StrCount := StrCount + 1;
                    MakeTokenHelper(OutFile,Str,StrCount);

                 when '(' | ')' | ',' | ':' | ';' =>
                    MakeTokenHelper(OutFile,Str,StrCount);
                    ConvertToken(To_Unbounded_String(Character'Image(Char)),
                                 OutFile,3);
                    if (EchoMode = True) then
                        Put(Char);
                    end if;
                 when others =>
                    MakeTokenHelper(OutFile,Str,StrCount);
              end case;

           else
               MakeTokenHelper(OutFile,Str,StrCount);
           end if;
        end loop;
        lexData.Increase_LineCount;
        Text_IO.Skip_Line (InFile);
        if not Text_Io.End_Of_File (Infile) then
           MakeTokenHelper(OutFile,Str,StrCount);
           if (EchoMode = True) then
               Text_IO.New_Line;
           end if;
        end if;
     end loop;
     if (EchoMode = True) then
         Text_IO.New_Line;
     end if;
  end MakeToken;
  
  ---------------------------------------------------------------
  --
  -- Function name: LexProccessor
  -- Description: Main procedure for the program. The file reads in a program
  -- and performs a lexicographical analysis.
  --
  ---------------------------------------------------------------
  procedure LexProccessor is
  -- Input and output file declartions
  InFile, OutFile: 	File_Type;
  begin
    ReadFile(InFile,OutFile);
    MakeToken(InFile,OutFile);
    Close(InFile);
    Close(OutFile);

    if (LexemeOutputMode = True) then
       Output_Lexemes(SpecialOutputFile);
       Close(SpecialOutputFile);

    elsif (SymbolTableOutput = True) then
       Output_ID(SpecialOutputFile);
       Close(SpecialOutputFile);
    end if;

  end LexProccessor;

end lex;
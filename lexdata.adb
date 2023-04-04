----------------------------------------------------------------------
--  Author:      Stephen Magrowski                                  --
--  Created on:  May 11, 2020                                       --
--  Due Date:    May 20, 2020                                       --
--  Course:      CSC 310-010                                        --
--  Professor:   Dr. Spiegel                                        --
--  Assignment:  #3                                                 --
--  Filename:    lexdata.adb                                        --
--  Purpose:     This file is used to hold the symbol table,        --
--               lexemes and their required information.            --
--                                                                  --
----------------------------------------------------------------------

with Text_IO;                  use Text_IO;
with OpenFile;		       use OpenFile;
with Ada.Command_Line;         use Ada.Command_Line;
with Ada.Strings.Unbounded;    use Ada.Strings.Unbounded;
with Ada.Text_IO.Unbounded_IO; use Ada.Text_IO.Unbounded_IO;
with Ada.Integer_Text_IO;      use Ada.Integer_Text_IO;
with Ada.Containers.Vectors;   use Ada.Containers;

package body LexData is

   ---------------------------------------------------------------
   --
   -- Function name: Add_ID
   -- Description: Attempts to add the identifier into the ID_holder array
   --
   ---------------------------------------------------------------
   procedure Add_ID(InputID: in Unbounded_String) is
   begin
      if (Find_ID(InputID) = False) and then (PastBegin = False) then
         IDHolder(IDCount) := InputID;
         IDLineNum(IDCount) := LineCount + 1;
         IDCount := IDCount + 1;
      end if;
   end Add_ID;

   ---------------------------------------------------------------
   --
   -- Function name: Find_ID
   -- Description: Tries to find the InputID in the IDHolder array
   --
   ---------------------------------------------------------------
   function Find_ID(InputID: in Unbounded_String) return Boolean is
       Idx : Integer := 1;
   begin
   while (Idx < IDCount) loop
       if (InputID = IDHolder(Idx)) then
           return True;
       else
           Idx := Idx + 1;
       end if;
   end loop;
   return False;
   end Find_ID;

   ---------------------------------------------------------------
   --
   -- Function name: Output_ID
   -- Description: Sends the symbol table into an output file
   --
   ---------------------------------------------------------------
   procedure Output_ID(SpecialOutputFile: in out File_type)is
       Idx : Integer := 1;
     begin
     Put(SpecialOutputFile, "   Symbol Table   ");
     New_Line(SpecialOutputFile);

     Put(SpecialOutputFile, "Line 1 indicates program name");
     New_Line(SpecialOutputFile);

     Put(SpecialOutputFile, "ID        LineNumber");
     New_Line(SpecialOutputFile);

     while (Idx < IDCount) loop
         Put(SpecialOutputFile, IDholder(Idx));
         Put(SpecialOutputFile, "    ");
         Set_Col(SpecialOutputFile, 15);
         Put(SpecialOutputFile, IDlineNum(Idx), Width => 1);
         New_Line(SpecialOutputFile);
         Idx := Idx + 1;
     end loop;
   end Output_ID;

   ---------------------------------------------------------------
   --
   -- Function name: Output_Lexemes
   -- Description: Adds a lexeme to the Lexeme Vector as well as the line number
   --
   ---------------------------------------------------------------
   procedure Output_Lexemes(specialOutputfile: in out File_Type) is
         CurrIdx : Integer := 0;
     begin
     while CurrIdx < Integer(LexemeVector.Length) loop
         Put(SpecialOutputFile, LexemeVector.Element(Index => CurrIdx));
         New_Line(SpecialOutputFile);
         CurrIdx := CurrIdx + 1;
     end loop;
   end Output_Lexemes;

   ---------------------------------------------------------------
   --
   -- Function name: Add_LineNumber
   -- Description: Adds line number into the LexemeLineNum Vector.
   --
   ---------------------------------------------------------------
   procedure Add_LineNumber is
   begin
      LexemeLineNum.Append(New_Item => LineCount);
   end Add_LineNumber;
   
   ---------------------------------------------------------------
   --
   -- Function name: Get_LineNumber
   -- Description: Returns a boolean value to see if Lexeme Proccessor is past the Begin. 
   --
   ---------------------------------------------------------------
   function Get_LineNumber return Integer is
   begin
      if (CurrentVectorIdx < Integer(LexemeVector.Length)) then
         return LexemeLineNum.Element(Index => CurrentVectorIdx);
      else 
         return LexemeLineNum.Element(Index => Integer(LexemeVector.Length) - 1);
      end if;
   end Get_LineNumber;

   ---------------------------------------------------------------
   --
   -- Function name: Add_Lexeme
   -- Description: Adds a lexeme to the Lexeme Vector as well as the line number.
   --
   ---------------------------------------------------------------
   procedure Add_Lexeme(Lexeme: in Char_Type) is
   begin
      if not (Lexeme = Nullsym) then
         LexemeVector.Append(New_Item => Lexeme);
         VectorCounter := VectorCounter + 1;
         Add_LineNumber;
      end if;
   end Add_Lexeme;
   
   ---------------------------------------------------------------
   --
   -- Function name: Get_PastBegin
   -- Description: Returns a boolean value to see if lexeme proccessor is past the Begin.
   --
   ---------------------------------------------------------------
   function Get_PastBegin return Boolean is
   begin
       return PastBegin;
   end;
   
   ---------------------------------------------------------------
   --
   -- Function name: Set_PastBegin
   -- Description: Sets boolean value to indicate the Lexeme Proccessor is past the Begin.
   --
   ---------------------------------------------------------------
   procedure Set_PastBegin is
   begin
       PastBegin := True;
   end Set_PastBegin;
 
   ---------------------------------------------------------------
   --
   -- Function name: Increase_LineCount
   -- Description: Adds 1 to the LineCount variable
   --
   ---------------------------------------------------------------
   procedure Increase_LineCount is
   begin
       LineCount := LineCount + 1;
   end Increase_LineCount;
 
   ---------------------------------------------------------------
   --
   -- Function name: Get_Lexeme
   -- Description: Returns the current lexeme at the CurrentVectorIdx
   --
   ---------------------------------------------------------------
   function Get_Lexeme return Char_Type is 
   begin
       if (CurrentVectorIdx < Integer(LexemeVector.Length)) then
           return LexemeVector.Element(Index => CurrentVectorIdx);
       end if;
       return InvalidSym;
   end Get_Lexeme;
 
   ---------------------------------------------------------------
   --
   -- Function name: CheckNext_Lexeme
   -- Description: Returns the current lexeme at the (CurrentVectorIdx + 1)
   --
   ---------------------------------------------------------------
   function CheckNext_Lexeme return Char_Type is
   begin
       if (CurrentVectorIdx + 1 < Integer(LexemeVector.Length)) then
           return LexemeVector.Element(Index => (CurrentVectorIdx + 1));
       end if;
       return InvalidSym;
   end CheckNext_Lexeme;
 
   ---------------------------------------------------------------
   --
   -- Function name: MoveNext_Lexeme
   -- Description: Moves pointer to the next lexeme in the Vector
   --
   ---------------------------------------------------------------
   procedure MoveNext_Lexeme is
   begin
       if (CurrentVectorIdx < Integer(LexemeVector.Length)) then
           CurrentVectorIdx := CurrentVectorIdx + 1;
       end if;
   end MoveNext_Lexeme;
end LexData;

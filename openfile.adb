----------------------------------------------------------------------
--  Author:      Dr. Spiegel                                        --
--  Edited By:   Stephen Magrowski                                  --
--  Created on:  April 20, 2020                                     --
--  Due Date:    May 20, 2020                                       --
--  Course:      CSC 310-010                                        --
--  Professor:   Dr. Spiegel                                        --
--  Assignment:  #3                                                 --
--  Filename:    OpenFile.adb                                       --
--  Purpose:     OpenFile package implementation                    --
--               Procedures for opening files for reading and       --
--               writing. Caller need not know actual file names.   --
--                                                                  --
----------------------------------------------------------------------

package body OpenFile is

   ------------------------------------------------------------------------
   -- OpenReadFile prompts the user for the filename and opens it for input
   ------------------------------------------------------------------------
   procedure OpenReadFile (Datafile : in out File_Type) is
      Fname : String (1..80);
      Len   : Natural;           -- length of the filename
   begin
      Put ("Please enter the name of your data file: ");
      Get_Line (Fname, Len);     -- len is the name of the characters reasd
      Open (File=>Datafile, Mode=>In_File, Name=>Fname(1..Len));
   end OpenReadFile;

   -------------------------------------------------------------------------
   --OpenWriteFile prompts the user for the file name and opens it for input
   -------------------------------------------------------------------------
   procedure OpenWriteFile (Outputfile : in out File_type) is
      Fname : String (1..80);
      Len   : Natural;           -- length of the filename
   begin
      Put ("Please enter the name of your output file: ");
      Get_Line(Fname, Len);      -- len is the number of the characters read
      Create (File=>Outputfile, Mode=>Out_File, Name=>Fname(1..Len));
   end OpenWriteFile;

end OpenFile;
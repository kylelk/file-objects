with Ada.Direct_IO;
with GNAT.SHA1;
with Ada.Numerics.Discrete_Random;
with Ada.Directories;

package body file_sha1 is
   function get_file_sha1 (file_name : String) return Sha1_value is
      File_Size : constant Natural := Natural (Ada.Directories.Size (file_name));
      C         : GNAT.SHA1.Context;
      Seek_Size : constant Positive := 4096;
      subtype File_String is String (1 .. Seek_Size);
      subtype End_String is String (1 .. 1);
      package File_String_IO is new Ada.Direct_IO (File_String);
      package End_File_IO is new Ada.Direct_IO (End_String);

      File     : File_String_IO.File_Type;
      Contents : File_String;
      End_Content : End_String;
      End_File : End_File_IO.File_Type;
      Last_Index : Integer := 1;
   begin

      File_String_IO.Open
        (File,
         Mode => File_String_IO.In_File,
         Name => file_name);

      while not File_String_IO.End_Of_File(File) loop
         Last_Index := Last_Index + Seek_Size;
         File_String_IO.Read (File => File, Item => Contents);
         GNAT.SHA1.Update(C, Contents);
      end loop;
      File_String_IO.Close (File);

      if (File_Size mod Seek_Size) > 0 then
         End_File_IO.Open(End_File, End_File_IO.In_File, file_name);
         End_File_IO.Set_Index(End_File, End_File_IO.Count(Last_Index));
         while not End_File_IO.End_Of_File(End_File) loop
            End_File_IO.Read (End_File, End_Content);
            GNAT.SHA1.Update(C, End_Content);
         end loop;
         End_File_IO.Close(End_File);
      end if;

      return GNAT.SHA1.Digest (C);
   end get_file_sha1;

   function String_Hash (Data : String) return Sha1_value is
      C : GNAT.SHA1.Context;
   begin
      GNAT.SHA1.Update (C, Data);
      return GNAT.SHA1.Digest (C);
   end String_Hash;

   function Rand_Sha1 return Sha1_Value is
      package Guess_Generator is new Ada.Numerics.Discrete_Random(Character);
      Gen : Guess_Generator.Generator;
      Data : Sha1_Value;
   begin
      for I in Data'Range loop
         Guess_Generator.Reset(Gen);
         Data(I) := Guess_Generator.Random(Gen);
      end loop;
      return String_Hash(Data);
   end Rand_Sha1;
end file_sha1;

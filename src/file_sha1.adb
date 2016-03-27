with Ada.Direct_IO;
with GNAT.SHA1;
with Ada.Directories;
with Ada.Numerics.Discrete_Random;

package body file_sha1 is
   function get_file_sha1 (file_name : String) return Sha1_value is
      File_Size : constant Natural := Natural (Ada.Directories.Size (file_name));
      C         : GNAT.SHA1.Context;
      subtype File_String is String (1 .. File_Size);
      package File_String_IO is new Ada.Direct_IO (File_String);
      File     : File_String_IO.File_Type;
      Contents : File_String;
   begin
      File_String_IO.Open
        (File,
         Mode => File_String_IO.In_File,
         Name => file_name);
      File_String_IO.Read (File, Item => Contents);
      File_String_IO.Close (File);
      GNAT.SHA1.Update (C, Contents);
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

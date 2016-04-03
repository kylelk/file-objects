with Ada.Direct_IO;
with GNAT.SHA1;
with Ada.Numerics.Discrete_Random;
with Ada.Text_IO;

package body file_sha1 is
   function get_file_sha1 (file_name : String) return Sha1_value is
      C         : GNAT.SHA1.Context;
      Seek_Size : constant Positive := 1;
      subtype File_String is String (1 .. Seek_Size);
      package File_String_IO is new Ada.Direct_IO (File_String);

      use File_String_IO;
      File     : File_String_IO.File_Type;
      Contents : File_String;
   begin
      File_String_IO.Open
        (File,
         Mode => File_String_IO.In_File,
         Name => file_name);

      while not File_String_IO.End_Of_File(File) loop
         File_String_IO.Read (File => File,
                              Item => Contents);
         Ada.Text_IO.Put(Contents);

         GNAT.SHA1.Update(C, Contents);

      end loop;
      Ada.Text_IO.New_Line;

      File_String_IO.Close (File);
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

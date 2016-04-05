pragma Ada_2012;

with Ada.Text_IO;
with Ada.Integer_Text_IO;
with Ada.Directories;
with Ada.Strings.Fixed;
with Ada.Strings.Unbounded;
with Ada.Characters.Handling;
with GNAT.Directory_Operations;
with Ada.IO_Exceptions;
with Ada.Command_Line;

-- project imports
with config;
with file_operations;
with file_sha1;
with Status;

procedure main is
   use Ada.Directories;
   use Ada.Text_IO;
   use Ada.Characters.Handling;

   package CLI renames Ada.Command_Line;
   package TIO renames Ada.Text_IO;
   package DIR_OPS renames GNAT.Directory_Operations;
   package UBS renames Ada.Strings.Unbounded;

   Project_Status : Status.Status_Map.Map;

   function "+"
     (S : String) return UBS.Unbounded_String is
     (UBS.To_Unbounded_String (S));

   function Integer2Hexa
     (Hex_Int : Integer;
      Width   : Positive := 2) return String
   is
      Hex_Prefix_Length : constant := 3;
      Hexa              : String (1 .. Hex_Prefix_Length + Width + 1);
      Result            : String (1 .. Width);
      Start             : Natural;
   begin
      Ada.Integer_Text_IO.Put (Hexa, Hex_Int, 16);
      Start := Ada.Strings.Fixed.Index (Source => Hexa, Pattern => "#");
      Ada.Strings.Fixed.Move
        (Source  => Hexa (Start + 1 .. Hexa'Last - 1),
         Target  => Result,
         Justify => Ada.Strings.Right,
         Pad     => '0');
      Result := To_Lower (Result);
      return Result;
   end Integer2Hexa;

   procedure Create_New_Dir (Name : String) is
   begin
      if not Ada.Directories.Exists (Name) then
         Create_Directory (Name);
      end if;
   end Create_New_Dir;

   procedure create_directories is
      prefix : String (1 .. 2);
   begin
      Create_New_Dir (config.Project_Dir);
      Create_New_Dir (config.Object_Dir);
      Create_New_Dir (config.Temp_Dir);
      Create_New_Dir (config.Checkout_Dir);

      for I in 0 .. 255 loop
         prefix := Integer2Hexa (I);
         Create_New_Dir
           (DIR_OPS.Format_Pathname (config.Object_Dir & "/") & prefix);
      end loop;
   end create_directories;

   procedure create_files is
   begin
      file_operations.create_empty_file (config.Status_File);
   end create_files;

   procedure clear_temp_dir is
   begin
      file_operations.remake_directory (config.Temp_Dir);
   end clear_temp_dir;

begin

   create_directories;
   create_files;

   TIO.Put_Line("hello world");

   clear_temp_dir;
end main;

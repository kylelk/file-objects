with Ada.Text_IO;
with Ada.Integer_Text_IO;
with Ada.Directories;
with Ada.Direct_IO;
with Ada.Strings.Fixed;
with Ada.Characters.Handling;
with GNAT.SHA1;
with file_sha1;
with GNAT.Directory_Operations;
with Ada.Calendar;
with Ada.Strings.Unbounded;
with Ada.Text_IO.Unbounded_IO;
with Ada.Sequential_IO;
with Ada.IO_Exceptions;

-- project imports
with config;
with file_item;
with album;

procedure main is
   use Ada.Directories;
   use Ada.Text_IO;
   use Ada.Text_IO.Unbounded_IO;
   use Ada.Characters.Handling;
   use GNAT.Directory_Operations;
   use file_item;
   use album;

   Search    : Search_Type;
   Dir_Ent   : Directory_Entry_Type;
   Directory : String := ".";

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

   procedure create_object_directory (name : String) is
      prefix : String (1 .. 2);
   begin
      if not Ada.Directories.Exists (name) then
         Ada.Directories.Create_Directory (name);
      end if;

      for I in 0 .. 255 loop
         prefix := Integer2Hexa (I);
         if not Ada.Directories.Exists
             (Format_Pathname (name & "/") & prefix)
         then
            Ada.Directories.Create_Directory
              (Format_Pathname (name & "/") & prefix);
         end if;
      end loop;
   end create_object_directory;

   procedure create_directories is
   begin
      if not Ada.Directories.Exists (config.project_dir) then
         Create_Directory (config.project_dir);
      end if;

      create_object_directory (config.object_dir);
      create_object_directory (config.properties_dir);

   end create_directories;

   items      : Album_Set.Set;
   album_item : Album_Info;
   set_cursor : Album_Set.Cursor;
begin

   create_directories;
   Ada.Directories.Start_Search (Search, Directory, "");

   while More_Entries (Search) loop
      Ada.Directories.Get_Next_Entry (Search, Dir_Ent);
      if Ada.Directories.Kind (Dir_Ent) =
        Ada.Directories.File_Kind'(Ordinary_File)
      then

         add_file : declare
            item : file_item.file_info;
         begin
            begin
               file_item.create (item, Simple_Name (Dir_Ent));
               Ada.Text_IO.Put_Line (item.sha1 & " " & Simple_Name (Dir_Ent));
            exception
               when Ada.IO_Exceptions.End_Error =>
                  Ada.Text_IO.Put
                    (File => Standard_Error,
                     Item => "IO_Exceptions.End_Error");
            end;
         end add_file;
      end if;
   end loop;

   Ada.Directories.End_Search (Search);

   Create (Album_Item, "2aae6c35c94fcfb415dbe95f408b9ce91ee846ed", "food");
   Album_Set.Insert (Items, Album_Item);

   Create (Album_Item, "2aae6c35c94fcfb415dbe95f408b9ce91ee846ed", "animal");
   -- Create (Child_Item, "2aae6c35c94fcfb415dbe95f408b9ce91ee846ed", "mouse");
   -- Album.Init_Children(album_item);
   -- Album.Append_Child(Album_Item, Child_Item);
   Album_Set.Insert (Items, Album_Item);

   Create (Album_Item, "2aae6c35c94fcfb415dbe95f408b9ce91ee846ed", "movie");
   Album_Set.Insert (Items, Album_Item);
   
  
   
   

   New_Line;

   album.Print_Tree (items);
end main;

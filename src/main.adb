with Ada.Text_IO;
with Ada.Integer_Text_IO;
with Ada.Directories;
with Ada.Strings.Fixed;
with Ada.Characters.Handling;
with GNAT.Directory_Operations;
with Ada.IO_Exceptions;
with Ada.Command_Line;

-- project imports
with config;
with file_item;
with album;
with file_operations;

procedure main is
   use Ada.Directories;
   use Ada.Text_IO;
   use Ada.Characters.Handling;
   --use GNAT.Directory_Operations;
   use file_item;
   use album;
   
   package CLI renames Ada.Command_Line;
   package TIO renames Ada.Text_IO;
   package DIR_OPS renames GNAT.Directory_Operations;
   
   root_album_set : Album_Set.Set;


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
   
   procedure Create_New_Dir(Name : String) is
   begin
      if not Ada.Directories.Exists (Name) then
         Create_Directory (Name);
      end if;
   end Create_New_Dir;
   

   procedure create_directories is
      prefix : String (1 .. 2);
   begin
      Create_New_Dir(Config.project_dir);
      Create_New_Dir(Config.object_dir);
      Create_New_Dir(Config.Temp_Dir);
      
      for I in 0 .. 255 loop
         prefix := Integer2Hexa (I);
         Create_New_Dir(DIR_OPS.Format_Pathname (Config.object_dir & "/") & prefix);
      end loop;
   end create_directories;
   
   
   procedure create_files is
   begin
      file_operations.create_empty_file(config.album_refs_file);
      -- create a blank object
      file_operations.create_empty_file(DIR_OPS.Format_Pathname(config.object_dir & "/da/" & "da39a3ee5e6b4b0d3255bfef95601890afd80709"));
   end create_files;
   
   
   procedure clear_temp_dir is
   begin
      file_operations.remake_directory(config.Temp_Dir);
   end clear_temp_dir;
   
   
   procedure add_files is
      Search    : Search_Type;
      Dir_Ent   : Directory_Entry_Type;
      Directory : constant String := ".";
      begin
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
                   TIO.Put_Line (item.sha1 & " " & Simple_Name (Dir_Ent));
                exception
                   -- file was empty
                   when Ada.IO_Exceptions.End_Error => null;
                end;
             end add_file;
          end if;
       end loop;
       Ada.Directories.End_Search (Search);
   end add_files;
   
   
   procedure test is
      items      : Album_Set.Set;
      album_item : Album_Info;
   begin
      Create (Album_Item, "food");
      Album_Set.Insert (Items, Album_Item);
      
      Create (Album_Item, "animal");
      Album_Set.Insert (Items, Album_Item);
      
      Create (Album_Item, "movie");
      Album_Set.Insert (Items, Album_Item);
      
      Create (Album_Item, "TV show");
      Album_Set.Insert (Items, Album_Item);
      
      album.Print_Tree (items);
      --album.Save_Albums(items, config.album_refs_file);
   end test;
   
   procedure add_new_album_cmd(items : in out Album_Set.Set) is
      temp_album : Album_Info;
   begin
      begin
         album.Create(temp_album, CLI.Argument(2));
         for I in 2..CLI.Argument_Count loop
            TIO.Put_Line("- " & CLI.Argument(I));
         end loop;
         
         -- Album_Set.Insert (Items, temp_album);
         -- album.Save_Albums(items, config.album_refs_file);
         -- TIO.Put_Line("added new album");
      exception when CONSTRAINT_ERROR =>
         TIO.Put_Line(File => Standard_Error, Item => "duplicate album");
      end;
   end add_new_album_cmd;
   
   Album_Namespaces : Album.Namespace_Map.Map;
begin

   create_directories;
   create_files;
   
   Album.Load(Album_Namespaces, Config.album_refs_file);
   Album.Create(Album_Namespaces, Config.Default_Album_Namespace);
   Album.Save(Album_Namespaces, Config.album_refs_file);
   
   
   -- album.Load_Albums(root_album_set, config.album_refs_file);
   
   if CLI.Argument_count < 1 then
      config.display_help;
      CLI.Set_Exit_Status (CLI.Failure);
   else
   
      if CLI.Argument(1) = "help" then
         config.display_help;
      elsif CLI.Argument(1) = "add" then
         add_files;
      elsif CLI.Argument(1) = "test" then
         test;
      elsif CLI.Argument(1) = "tree" then
         album.Print_Tree(root_album_set);
      elsif CLI.Argument(1) = "new" then
        if CLI.Argument_count > 1 then
            add_new_album_cmd(root_album_set);
         else
            TIO.Put_Line("enter a an album tree path");
        end if;
      else
         TIO.Put_Line("invalid command");
         CLI.Set_Exit_Status (CLI.Failure);   
      end if;
   end if;
   
   -- clean up any temporary files or directories
   clear_temp_dir;
end main;

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
with file_sha1;
with Status;

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

   Root_Album_Tree : album.Trees.Tree;
   Project_Status  : Status.Status_Map.Map;

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

      for I in 0 .. 255 loop
         prefix := Integer2Hexa (I);
         Create_New_Dir
           (DIR_OPS.Format_Pathname (config.Object_Dir & "/") & prefix);
      end loop;
   end create_directories;

   procedure create_files is
   begin
      file_operations.create_empty_file (config.Album_Refs_File);
      file_operations.create_empty_file (config.Status_File);
      -- create a blank object
      file_operations.create_empty_file
        (file_item.get_path (file_sha1.Empty_Sha1));
   end create_files;

   procedure clear_temp_dir is
   begin
      file_operations.remake_directory (config.Temp_Dir);
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
                  when Ada.IO_Exceptions.End_Error =>
                     null;
               end;
            end add_file;
         end if;
      end loop;
      Ada.Directories.End_Search (Search);
   end add_files;

   procedure Test
     (T    : in out Trees.Tree;
      Stat : in out Status.Status_Map.Map)
   is
   begin
      Add_Album (T, Stat, (1 => +"animal"));
      Add_Album (T, Stat, (+"animal", +"bird"));
      Add_Album (T, Stat, (+"animal", +"bird", +"duck"));
      Add_Album (T, Stat, (+"animal", +"bird", +"sparrow"));
      Add_Album (T, Stat, (1 => +"food"));
      Add_Album (T, Stat, (+"food", +"bread"));
      Add_Album (T, Stat, (+"food", +"dairy"));
      Add_Album (T, Stat, (+"food", +"dairy", +"milk"));
      Add_Album (T, Stat, (+"food", +"dairy", +"ice cream"));
      Display_Tree (T.Root, 0);
   end Test;

   procedure add_new_album_cmd
     (Tree_Data : in out album.Trees.Tree;
      Stat      : in out Status.Status_Map.Map)
   is
      Path : album.Album_Path (1 .. (CLI.Argument_Count - 1));
   begin
      begin
         for I in 2 .. CLI.Argument_Count loop
            Path (I - 1) := +CLI.Argument (I);
         end loop;
         album.Add_Album (Tree_Data, Stat, Path);
      exception
         when Constraint_Error =>
            TIO.Put_Line (File => Standard_Error, Item => "duplicate album");
      end;
   end add_new_album_cmd;

   procedure Create_Default_Namespace is
      Album_Namespaces : album.Namespace_Map.Map;
   begin
      album.Load (Album_Namespaces, config.Album_Refs_File);
      begin
         album.Create (Album_Namespaces, config.Default_Album_Namespace);
         album.Save (Album_Namespaces, config.Album_Refs_File);
      exception
         when Constraint_Error =>
            null;
      end;

      Status.Set_Default_Value
        (Project_Status,
         "current_namespace",
         config.Default_Album_Namespace);
   end Create_Default_Namespace;

   procedure Add_Namespace
     (Map  : in out album.Namespace_Map.Map;
      Name :        String)
   is
   begin
      begin
         album.Create (Map, Name);
         TIO.Put_Line ("created new namespace: " & Name);
      exception
         when Constraint_Error =>
            TIO.Put_Line
              (File => Standard_Error,
               Item => "namespace already exists");
      end;
   end Add_Namespace;

   procedure Remove_Namespace
     (Map  : in out album.Namespace_Map.Map;
      Name :        String)
   is
   begin
      begin
         if Name /= config.Default_Album_Namespace then
            album.Remove (Map, Name);
            TIO.Put_Line ("removed namespace: " & Name);
         else
            TIO.Put_Line
              (File => Standard_Error,
               Item => "cannot remove default namespace");
         end if;
      exception
         when Constraint_Error =>
            TIO.Put_Line
              (File => Standard_Error,
               Item => "cannot find namespace: " & Name);
      end;
   end Remove_Namespace;

   procedure Change_Namespace (Map : album.Namespace_Map.Map; Name : String) is
   begin
      if album.Contains (Map, Name) then
         Status.Set (Project_Status, "current_namespace", Name);
         TIO.Put_Line ("changed to namespace: " & Name);
      else
         TIO.Put_Line
           (File => Standard_Error,
            Item => "cannot find namespace: " & Name);
      end if;
   end Change_Namespace;

   procedure Edit_Namespace_Cmd (Map : in out album.Namespace_Map.Map) is
   begin
      if CLI.Argument_Count = 2 then
         if CLI.Argument (2) = "list" then
            Display_Namespaces (Map);
         elsif CLI.Argument (2) = "current" then

            TIO.Put_Line (Status.Get (Project_Status, "current_namespace"));
         end if;
      elsif CLI.Argument_Count > 1 then
         if CLI.Argument (2) = "new" then
            Add_Namespace (Map, CLI.Argument (3));
         elsif CLI.Argument (2) = "remove" then
            Remove_Namespace (Map, CLI.Argument (3));
         elsif CLI.Argument (2) = "change" then
            Change_Namespace (Map, CLI.Argument (3));
         end if;
      else
         TIO.Put_Line
           (File => Standard_Error,
            Item => "enter a namespace operation");
      end if;
   end Edit_Namespace_Cmd;

   procedure Save_Current_Album
     (Tree_Data         :        Trees.Tree;
      Namespaces        : in out album.Namespace_Map.Map;
      Current_Namespace :        UBS.Unbounded_String)
   is
      New_Album_Pointer : file_sha1.Sha1_value;
      Temp_Path         : constant String :=
        DIR_OPS.Format_Pathname (config.Temp_Dir & "/album-tree");
      Current_Pointer : file_sha1.Sha1_value;
   begin
      Current_Pointer :=
        album.Namespace_Pointer (Namespaces, Current_Namespace);
      album.Save_Albums (Tree_Data, Temp_Path);
      New_Album_Pointer := file_sha1.get_file_sha1 (Temp_Path);
      if New_Album_Pointer /= Current_Pointer then
         -- move temp file to objects directory
         Ada.Directories.Copy_File
           (Temp_Path,
            file_item.get_path (New_Album_Pointer));
         -- update the album namespace pointer
         album.Update_Namespace
           (Namespaces,
            Current_Namespace,
            New_Album_Pointer);
         -- remove the old album tree
         if Current_Pointer /= file_sha1.Empty_Sha1 and
           Current_Pointer /= New_Album_Pointer
         then
            Ada.Directories.Delete_File (file_item.get_path (Current_Pointer));
         end if;
      end if;
   end Save_Current_Album;

   Album_Namespaces          : album.Namespace_Map.Map;
   Current_Namespace_Pointer : file_sha1.Sha1_value;
   Current_Namespace_Name    : UBS.Unbounded_String;
begin

   create_directories;
   create_files;
   Status.Load (Project_Status, config.Status_File);
   Create_Default_Namespace;
   Status.Set_Default_Value
     (Project_Status,
      "sha1_seed",
      file_sha1.Empty_Sha1);

   album.Load (Album_Namespaces, config.Album_Refs_File);
   Current_Namespace_Name :=
     UBS.To_Unbounded_String
       (Status.Get (Project_Status, "current_namespace"));

   Current_Namespace_Pointer :=
     album.Namespace_Pointer (Album_Namespaces, Current_Namespace_Name);
   album.Load_Albums
     (Root_Album_Tree,
      file_item.get_path (Current_Namespace_Pointer));

   if CLI.Argument_Count < 1 then
      config.display_help;
      CLI.Set_Exit_Status (CLI.Failure);
   else

      if CLI.Argument (1) = "help" then
         config.display_help;

      elsif CLI.Argument (1) = "add" then
         add_files;

      elsif CLI.Argument (1) = "test" then
         Test (Root_Album_Tree, Project_Status);

      elsif CLI.Argument (1) = "tree" then
         album.Display_Tree (Root_Album_Tree.Root, 0);

      elsif CLI.Argument (1) = "new" then
         if CLI.Argument_Count > 1 then
            add_new_album_cmd (Root_Album_Tree, Project_Status);
         else
            TIO.Put_Line ("enter a an album tree path");
         end if;

      elsif CLI.Argument (1) = "namespace" then
         Edit_Namespace_Cmd (Album_Namespaces);
         null;
      else
         TIO.Put_Line ("invalid command");
         CLI.Set_Exit_Status (CLI.Failure);
      end if;
   end if;

   Save_Current_Album
     (Root_Album_Tree,
      Album_Namespaces,
      Current_Namespace_Name);
   Status.Save (Project_Status, config.Status_File);
   album.Save (Album_Namespaces, config.Album_Refs_File);
   clear_temp_dir;
end main;

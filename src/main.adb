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
with SQLite;

-- project imports
with config;
with file_item;
with album;
with file_operations;
with file_sha1;
with Status;
with Data_Source;

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
      -- create a blank object
      file_operations.create_empty_file
        (file_item.get_path (file_sha1.Empty_Sha1));
   end create_files;

   procedure clear_temp_dir is
   begin
      file_operations.remake_directory (config.Temp_Dir);
   end clear_temp_dir;

   procedure Add_Files
     (DB_Conn   : in out SQLite.Data_Base;
      Namespace :        UBS.Unbounded_String;
      Directory :        String)
   is

      Search  : Search_Type;
      Dir_Ent : Directory_Entry_Type;
   begin
      Ada.Directories.Start_Search (Search, Directory, "");
      while More_Entries (Search) loop
         Ada.Directories.Get_Next_Entry (Search, Dir_Ent);
         if Ada.Directories.Kind (Dir_Ent) =
           Ada.Directories.File_Kind'(Ordinary_File)
         then
            add_file : declare
               item : file_item.file_info;
               Path : constant String := DIR_OPS.Format_Pathname(Directory & "/" & Simple_Name(Dir_Ent));
            begin
               begin
                  File_Item.Create (DB_Conn, Item, Path);
                  Album.Add_To_Head(DB_Conn, Namespace, item);
                  if Item.Is_New then
                     TIO.Put_Line (Item.sha1 & " " & path);
                  end if;
               exception
                  -- file was empty
                  when Ada.IO_Exceptions.End_Error => null;
               end;
            end add_file;
         end if;
      end loop;
      Ada.Directories.End_Search (Search);
   end Add_Files;

   procedure add_new_album_cmd
     (DB_Conn   : in out SQLite.Data_Base;
      Namespace :        UBS.Unbounded_String)
   is
      Path : album.Album_Path (1 .. (CLI.Argument_Count - 1));
   begin
      begin
         for I in 2 .. CLI.Argument_Count loop
            Path (I - 1) := +CLI.Argument (I);
         end loop;
         album.Add_Album (DB_Conn, Namespace, Path);
      exception
         when Constraint_Error =>
            TIO.Put_Line (File => Standard_Error, Item => "duplicate album");
      end;
   end add_new_album_cmd;

   procedure Create_Default_Namespace (DB_Conn : in out SQLite.Data_Base) is
   begin
      if not album.Namespace_Exists
          (DB_Conn,
           config.Default_Album_Namespace)
      then
         album.Create_Namespace (DB_Conn, config.Default_Album_Namespace);
      end if;

      Status.Set_Default_Value
        (Project_Status,
         "current_namespace",
         config.Default_Album_Namespace);
   end Create_Default_Namespace;

   procedure Add_Namespace
     (DB_Conn : in out SQLite.Data_Base;
      Name    :        String)
   is
   begin
      if not album.Namespace_Exists (DB_Conn, Name) then
         album.Create_Namespace (DB_Conn, Name);
         TIO.Put_Line ("created new namespace: " & Name);
      else
         TIO.Put_Line
           (File => Standard_Error,
            Item => "namespace already exists");
      end if;
   end Add_Namespace;

   procedure Remove_Namespace
     (DB_Conn : in out SQLite.Data_Base;
      Stat    :        Status.Status_Map.Map;
      Name    :        String)
   is
   begin
      if not album.Namespace_Exists (DB_Conn, Name) then
         TIO.Put_Line
           (File => Standard_Error,
            Item => "cannot find namespace: " & Name);
      elsif Name = Status.Get (Stat, "current_namespace") then
         TIO.Put_Line
           (File => Standard_Error,
            Item => "cannot remove the current namespace");
      elsif Name /= config.Default_Album_Namespace then
         album.Remove_Namespace (DB_Conn, Name);
         TIO.Put_Line ("removed namespace: " & Name);
      else
         TIO.Put_Line
           (File => Standard_Error,
            Item => "cannot remove default namespace");
      end if;
   end Remove_Namespace;

   procedure Change_Namespace
     (DB_Conn : in out SQLite.Data_Base;
      Name    :        String)
   is
   begin
      if album.Namespace_Exists (DB_Conn, Name) then
         Status.Set (Project_Status, "current_namespace", Name);
         TIO.Put_Line ("changed to namespace: " & Name);
      else
         TIO.Put_Line
           (File => Standard_Error,
            Item => "cannot find namespace: " & Name);
      end if;
   end Change_Namespace;

   procedure Edit_Namespace_Cmd (DB_Conn : in out SQLite.Data_Base) is
      Current_Namespace : UBS.Unbounded_String;
   begin
      Current_Namespace :=
        UBS.To_Unbounded_String
          (Status.Get (Project_Status, "current_namespace"));

      if CLI.Argument_Count = 2 then
         if CLI.Argument (2) = "list" then
            Display_Namespaces (DB_Conn, Current_Namespace);
         elsif CLI.Argument (2) = "current" then
            TIO.Put_Line (UBS.To_String (Current_Namespace));
         end if;
      elsif CLI.Argument_Count > 1 then
         if CLI.Argument (2) = "new" then
            Add_Namespace (DB_Conn, CLI.Argument (3));
         elsif CLI.Argument (2) = "remove" then
            Remove_Namespace (DB_Conn, Project_Status, CLI.Argument (3));
         elsif CLI.Argument (2) = "change" then
            Change_Namespace (DB_Conn, CLI.Argument (3));
         end if;
      else
         TIO.Put_Line
           (File => TIO.Standard_Error,
            Item => "enter a namespace operation");
      end if;
   end Edit_Namespace_Cmd;

   procedure Edit_Album_Cmd
     (DB_Conn   : in out SQLite.Data_Base;
      Namespace :        UBS.Unbounded_String)
   is
      Path : album.Album_Path (1 .. (CLI.Argument_Count - 2));
   begin
      if CLI.Argument_Count = 2 then
         if CLI.Argument (2) = "tree" then
            album.Display_Tree (DB_Conn, Namespace);
         end if;
      elsif CLI.Argument_Count > 1 then
         for I in 3 .. CLI.Argument_Count loop
            Path (I - 2) := UBS.To_Unbounded_String (CLI.Argument (I));
         end loop;

         if CLI.Argument (2) = "remove" then
            album.Remove_Album (DB_Conn, Namespace, Path);
         elsif CLI.Argument (2) = "checkout" then
            album.Checkout_Album (DB_Conn, Namespace, Path);
         end if;
      end if;
   end Edit_Album_Cmd;

   Current_Namespace : UBS.Unbounded_String;
   DB_Conn           : SQLite.Data_Base;
begin

   create_directories;
   create_files;
   Status.Load (Project_Status, config.Status_File);
   Data_Source.Load (DB_Conn);
   Create_Default_Namespace (DB_Conn);

   Status.Set_Default_Value (Project_Status, "sha1_seed", file_sha1.Rand_Sha1);

   Current_Namespace :=
     UBS.To_Unbounded_String
       (Status.Get (Project_Status, "current_namespace"));

   if CLI.Argument_Count < 1 then
      config.display_help;
      CLI.Set_Exit_Status (CLI.Failure);
   else

      if CLI.Argument (1) = "help" then
         config.display_help;

      elsif CLI.Argument (1) = "add" then
         if CLI.Argument_Count > 1 then
            Add_Files (DB_Conn, Current_Namespace, CLI.Argument(2));
         else
            -- Add_Files (DB_Conn, Current_Namespace);
            null;
         end if;
         

      elsif CLI.Argument (1) = "album" then
         Edit_Album_Cmd (DB_Conn, Current_Namespace);

      elsif CLI.Argument (1) = "new" then
         if CLI.Argument_Count > 1 then
            add_new_album_cmd (DB_Conn, Current_Namespace);
         else
            TIO.Put_Line ("enter an album tree path");
         end if;

      elsif CLI.Argument (1) = "namespace" then
         Edit_Namespace_Cmd (DB_Conn);
      else
         TIO.Put_Line ("invalid command");
         CLI.Set_Exit_Status (CLI.Failure);
      end if;
   end if;
   
   Status.Save (Project_Status, config.Status_File);
   clear_temp_dir;
end main;

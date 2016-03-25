with GNAT.Directory_Operations;
with Ada.Text_IO;

package config is
   use GNAT.Directory_Operations;
   package TIO renames Ada.Text_IO;

   Project_Dir    : constant String := ".object_dir";

   Object_Dir : constant String := Format_Pathname (project_dir & "/objects");

   Temp_Dir : constant String :=
     Format_Pathname (project_dir & "/temp");

   Status_File : constant String := Format_Pathname (project_dir & "/status");

   Album_Refs_File : constant String :=
     Format_Pathname (project_dir & "/album_refs");

   Default_Album_Namespace : constant String := "default";

   procedure display_help;
end config;

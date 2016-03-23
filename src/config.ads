with GNAT.Directory_Operations;
with Ada.Text_IO;

package config is
   use GNAT.Directory_Operations;
   package TIO renames Ada.Text_IO;

   project_dir    : constant String := ".object_dir";
   
   object_dir : constant String := Format_Pathname (project_dir & "/objects");
   
   Temp_Dir : constant String :=
     Format_Pathname (project_dir & "/temp");
   
   status_file : constant String := Format_Pathname (project_dir & "/status");
   
   album_refs_file : constant String :=
     Format_Pathname (project_dir & "/album_refs");
   
   Default_Album_Namespace : constant String := "default";

   procedure display_help;
end config;

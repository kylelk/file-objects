with GNAT.Directory_Operations;
with Ada.Text_IO;

package config is
   use GNAT.Directory_Operations;
   package TIO renames Ada.Text_IO;

   Project_Dir    : constant String := ".object_dir";

   Object_Dir : constant String := Format_Pathname (project_dir & "/objects");

   Temp_Dir : constant String :=
     Format_Pathname (project_dir & "/temp");

   Checkout_Dir : constant String := "checkout";

   Status_File : constant String := Format_Pathname (project_dir & "/status");

   Database_File : constant String := Format_Pathname(Project_Dir & "/data.sqlite");

   Default_Album_Namespace : constant String := "default";

   procedure display_help;
end config;

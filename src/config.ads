with GNAT.Directory_Operations;

package config is
   use GNAT.Directory_Operations;
   project_dir    : constant String := ".object_dir";
   object_dir : constant String := Format_Pathname (project_dir & "/objects");
   properties_dir : constant String :=
     Format_Pathname (project_dir & "/properties");
   status_file : constant String := Format_Pathname (project_dir & "/status");
   album_refs_file : constant String :=
     Format_Pathname (project_dir & "/album_refs");
end config;

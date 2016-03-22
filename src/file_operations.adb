package body file_operations is
    use Ada.Directories;
    use GNAT.Directory_Operations;
    
   procedure remake_directory(path : String) is 
   begin
     if Ada.Directories.Exists (path) then
        Delete_Tree(path);
        Create_Directory(path);
     end if;
   end remake_directory;
   
   procedure create_empty_file(path : String) is
      File_Handle : STIO.File_Type;
      begin
      if not Ada.Directories.Exists(path) then
         STIO.Create(File_Handle, STIO.Out_File, path);
         STIO.Close (File_Handle);
      end if;
   end create_empty_file;
   
end file_operations;

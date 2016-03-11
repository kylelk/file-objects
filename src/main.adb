with Ada.Text_IO;
with Ada.Directories; 
with Ada.Direct_IO;
with GNAT.SHA1;


procedure main is
    use Ada.Directories;
    use Ada.Text_IO;
    
    Search  : Search_Type;
    Dir_Ent : Directory_Entry_Type;
    Directory : String := ".";
    
    
    function get_file_sha1(file_name : String) return String is 
        File_Size : Natural := Natural (Ada.Directories.Size (File_Name));
        C: GNAT.SHA1.Context;
        subtype File_String  is String (1 .. File_Size);
        package File_String_IO is new Ada.Direct_IO (File_String);
        File     : File_String_IO.File_Type;
        Contents : File_String;
    begin
        File_String_IO.Open  (File, Mode => File_String_IO.In_File, Name => File_Name);
        File_String_IO.Read  (File, Item => Contents);
        File_String_IO.Close (File);
        GNAT.SHA1.Update(C, Contents);
        return GNAT.SHA1.Digest(C);
    end get_file_sha1;
    
    contents_sha1 : String(1..40);
begin
    
    Start_Search (Search, Directory, "");
 
    while More_Entries (Search) loop
      Get_Next_Entry (Search, Dir_Ent);
      if Ada.Directories.Kind(Dir_Ent) = Ada.Directories.File_Kind'(Ordinary_File) then
        contents_sha1 := get_file_sha1(Simple_Name (Dir_Ent));
        Ada.Text_IO.Put_Line (contents_sha1 & " " &  Simple_Name (Dir_Ent));
      end if;
    end loop;
 
  End_Search (Search);
end main;
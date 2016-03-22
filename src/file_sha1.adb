with Ada.Direct_IO;
with GNAT.SHA1;
with Ada.Directories;

package body file_sha1 is
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
end file_sha1;
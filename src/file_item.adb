with Ada.Directories;
with GNAT.Directory_Operations;

package body file_item is
   use GNAT.Directory_Operations;

   function Get_Path (Item : File_Info) return String is
   begin
      return Get_Path(Item.sha1);
   end get_path;


   function Get_Path (Sha1 : File_Sha1.Sha1_Value) return String is
   begin
      return Format_Pathname
          (config.object_dir & "/" & Sha1 (1 .. 2) & "/" & Sha1);
   end get_path;


   procedure create (item : in out file_info; path : String) is
      Sha1 : String (1 .. 40);
   begin
      Sha1              := file_sha1.get_file_sha1 (path);
      item.sha1         := Sha1;
      item.Size         := Integer (Ada.Directories.Size (path));
      item.Extension := To_Unbounded_String (Ada.Directories.Extension (path));
      item.Added_At     := Ada.Calendar.Clock;
      item.value_length := (item'Size) / 8;

      if not Ada.Directories.Exists (get_path (item)) then
         Ada.Directories.Copy_File (path, get_path (item));
      end if;
   end create;


   procedure update (item : in out file_info) is
   begin
      null;
   end update;


   function Exists (Sha1 : File_Sha1.Sha1_value) return Boolean is
      Path : String :=
        Format_Pathname
          (config.object_dir &
           "/" &
           Sha1 (Sha1'First .. Sha1'First + 1) &
           "/" &
           Sha1);
   begin
      return Ada.Directories.Exists (Path);
   end Exists;

--     function get_by_sha1(sha1 : String) return file_info is
--        result : file_info;
--     begin
--        return result;
--     end get_by_sha1;
end file_item;

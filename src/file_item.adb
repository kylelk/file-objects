with Ada.Directories;
with GNAT.Directory_Operations;
with config;

with file_sha1;

package body file_item is
   use GNAT.Directory_Operations;

   function get_path (item : file_info) return String is
   begin
      return Format_Pathname
          (config.object_dir & "/" & item.sha1 (1 .. 2) & "/" & item.sha1);
   end get_path;

   procedure create (item : in out file_info; path : String) is
   begin
      item.sha1      := file_sha1.get_file_sha1 (path);
      item.Size      := Integer (Ada.Directories.Size (path));
      item.Extension := To_Unbounded_String (Ada.Directories.Extension (path));
      item.Added_At  := Ada.Calendar.Clock;
      item.value_length := (item'Size)/8;

      if not Ada.Directories.Exists (get_path (item)) then
         Ada.Directories.Copy_File (path, get_path (item));
      end if;
   end create;

   procedure update (item : in out file_info) is
   begin
      null;
   end update;
   
   function get_by_sha1(sha1 : String) return file_info is
      result : file_info;
   begin
      return result;
   end get_by_sha1;
end file_item;

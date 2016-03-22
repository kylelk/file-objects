with Ada.Directories;
with GNAT.Directory_Operations;
with Ada.Strings.Unbounded;
with config;
with file_sha1;
with Ada.Calendar;

package body file_item is
   use GNAT.Directory_Operations;
   use Ada.Strings.Unbounded;

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

      if not Ada.Directories.Exists (get_path (item)) then
         Ada.Directories.Copy_File (path, get_path (item));
      end if;
   end create;

   procedure update (item : in out file_info) is
   begin
      null;
   end update;
end file_item;

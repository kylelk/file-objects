with Ada.Strings.Unbounded;
with Ada.Calendar;

package file_item is
   use Ada.Strings.Unbounded;
   type file_info is tagged record
      value_length : Integer;
      sha1      : String (1 .. 40);
      Size      : Integer;
      Extension : Unbounded_String;
      Filename  : Unbounded_String;
      Added_At  : Ada.Calendar.Time;
   end record;

   function get_path (item : file_info) return String;
   procedure create (item : in out file_info; path : String);
   procedure update (item : in out file_info);
   
   function get_by_sha1(sha1 : String) return file_info;
end file_item;

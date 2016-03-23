with Ada.Strings.Unbounded;
with Ada.Calendar;
with Ada.Streams.Stream_IO;
with config;

with File_Sha1;

package file_item is
   use Ada.Strings.Unbounded;
   package STIO renames Ada.Streams.Stream_IO;

   type file_info is tagged record
      value_length : Integer;
      sha1      : File_Sha1.Sha1_value;
      Size      : Integer;
      Extension : Unbounded_String;
      Filename  : Unbounded_String;
      Added_At  : Ada.Calendar.Time;
   end record;

   function get_path (item : file_info) return String;
   function get_path (Sha1 : File_Sha1.Sha1_value) return String;
   procedure create (item : in out file_info; path : String);
   procedure update (item : in out file_info);
   function Exists(Sha1 : File_Sha1.Sha1_value) return Boolean;

   -- function get_by_sha1(sha1 : String) return file_info;
end file_item;

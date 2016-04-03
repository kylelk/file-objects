with Ada.Strings.Unbounded;
with Ada.Calendar;
with Ada.Streams.Stream_IO;
with SQLite;

with file_sha1;

package file_item is
   use Ada.Strings.Unbounded;
   package UBS renames Ada.Strings.Unbounded;
   package STIO renames Ada.Streams.Stream_IO;

   type file_info is record
      Id         : Integer;
      sha1       : file_sha1.Sha1_value;
      Extension  : Unbounded_String;
      Created_At : Ada.Calendar.Time;
      File_Size  : Integer;
      Filename   : Unbounded_String;
      Is_New : Boolean := True;
   end record;

   function get_path (item : file_info) return String;
   function get_path (Sha1 : file_sha1.Sha1_value) return String;
   procedure create
     (DB_Conn : in out SQLite.Data_Base;
      item    : in out file_info;
      path    :        String);
   function Object_Exists (Sha1 : file_sha1.Sha1_value) return Boolean;

   function File_Saved
     (DB_Conn : in out SQLite.Data_Base;
      Sha1    :        file_sha1.Sha1_value) return Boolean;

   -- function get_by_sha1(sha1 : String) return file_info;
end file_item;

with Ada.Strings.Unbounded;
with Ada.Calendar;
with Ada.Streams.Stream_IO;
with SQLite;

with file_sha1;

package file_item is
   use Ada.Strings.Unbounded;
   package UBS renames Ada.Strings.Unbounded;
   package STIO renames Ada.Streams.Stream_IO;

   File_Not_Found : exception;

   type File_Info is record
      Id         : Integer := -1;
      sha1       : file_sha1.Sha1_value;
      Extension  : Unbounded_String;
      Created_At : Ada.Calendar.Time;
      File_Size  : Integer := -1;
      Filename   : Unbounded_String;
      Is_New : Boolean := True;
   end record;

   function Get_Path (Item : File_Info) return String;
   function Get_Path (Sha1 : File_Sha1.Sha1_Value) return String;
   procedure Create
     (DB_Conn : in out SQLite.Data_Base;
      item    : in out file_info;
      path    :        String);
   function Object_Exists (Sha1 : file_sha1.Sha1_value) return Boolean;

   function File_Saved
     (DB_Conn : in out SQLite.Data_Base;
      Sha1    :        file_sha1.Sha1_value) return Boolean;

   function Find_By_Sha1
     (DB_Conn : in out SQLite.Data_Base;
      Sha1    :        File_Sha1.Sha1_Value) return File_Info;

   function From_Row(Row : SQLite.Statement) return File_Info;
end file_item;

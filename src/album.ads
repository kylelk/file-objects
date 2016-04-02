with Ada.Containers.Ordered_Maps;
with Ada.Containers.Multiway_Trees;
with Ada.Streams.Stream_IO;
with Ada.Strings.Unbounded;
with Ada.Strings.Fixed;
with SQLite;

with file_sha1;
with Status;

package album is
   package STIO renames Ada.Streams.Stream_IO;
   package UBS renames Ada.Strings.Unbounded;
   package Fixed_Str renames Ada.Strings.Fixed;
   use UBS;

   No_Album_Exception : exception;

   type Album_Path is array (Positive range <>) of UBS.Unbounded_String;

   package Namespace_Map is new Ada.Containers.Ordered_Maps
     (Key_Type     => UBS.Unbounded_String,
      Element_Type => file_sha1.Sha1_value);

   procedure Create_Namespace
     (DB_Conn : in out SQLite.Data_Base;
      Name    :        UBS.Unbounded_String);
   procedure Create_Namespace
     (DB_Conn : in out SQLite.Data_Base;
      Name    :        String);
   procedure Remove_Namespace
     (DB_Conn : in out SQLite.Data_Base;
      Name    :        String);
   procedure Load (Map : out Namespace_Map.Map; Path : String);
   procedure Save (Map : in Namespace_Map.Map; Path : String);
   procedure Display_Namespaces
     (DB_Conn : SQLite.Data_Base;
      Current : UBS.Unbounded_String := UBS.To_Unbounded_String (""));

   function Namespace_Pointer
     (Map  : Namespace_Map.Map;
      Name : UBS.Unbounded_String) return file_sha1.Sha1_value;
   function Contains
     (Map : Namespace_Map.Map;
      Key : String) return Boolean is
     (Namespace_Map.Contains (Map, UBS.To_Unbounded_String (Key)));
   procedure Update_Namespace
     (Map     : in out Namespace_Map.Map;
      Name    :        UBS.Unbounded_String;
      Pointer :        file_sha1.Sha1_value);

   function Namespace_Exists
     (DB_Conn : in out SQLite.Data_Base;
      Name    :        String) return Boolean;

   type Album_Info is record
      Id        : Integer;
      Namespace : UBS.Unbounded_String;
      Depth     : Integer;
      Parent_Id : Integer := -1;
      Name      : UBS.Unbounded_String;
   end record;

   function "<" (a, b : Album_Info) return Boolean;
   function ">" (a, b : Album_Info) return Boolean;

   package Trees is new Ada.Containers.Multiway_Trees (Album_Info);

   function Find_Album
     (DB_Conn   : in out SQLite.Data_Base;
      Namespace :        UBS.Unbounded_String;
      Path      :        Album_Path) return Album_Info;
   procedure Add_Album
     (DB_Conn   : in out SQLite.Data_Base;
      Namespace :        UBS.Unbounded_String;
      Path      :        Album_Path);
   function Album_Exists
     (DB_Conn   : in out SQLite.Data_Base;
      Namespace :        UBS.Unbounded_String;
      Path      :        Album_Path;
      Result    :    out Album_Info) return Boolean;
   procedure Save_Albums (Tree_Data : Trees.Tree; File_Path : String);
   procedure Load_Albums (Tree_Data : out Trees.Tree; File_Path : String);
   procedure Display_Tree
     (DB_Conn   : in out SQLite.Data_Base;
      Namespace :        UBS.Unbounded_String);
   procedure Remove_Album
     (DB_Conn   : in out SQLite.Data_Base;
      Namespace :        UBS.Unbounded_String;
      Path      :        Album_Path);
   procedure Checkout_Album
     (DB_Conn   :    in out SQLite.Data_Base;
      Namespace :        UBS.Unbounded_String;
      Path      :        Album_Path);
   function From_Row (Row : SQLite.Statement) return Album_Info;
   function Get_Head_Id
     (Stat : Status.Status_Map.Map) return file_sha1.Sha1_value;
end album;

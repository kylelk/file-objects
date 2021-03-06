with Ada.Containers.Ordered_Maps;
with Ada.Streams.Stream_IO;
with Ada.Strings.Unbounded;
with Ada.Strings.Fixed;
with Ada.Directories;
with GNAT.Directory_Operations;
with SQLite;

with file_sha1;
with file_item;

package album is
   package STIO renames Ada.Streams.Stream_IO;
   package UBS renames Ada.Strings.Unbounded;
   package Fixed_Str renames Ada.Strings.Fixed;
   package DIRS renames Ada.Directories;
   package DIR_OPS renames GNAT.Directory_Operations;
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
   procedure Display_Tree
     (DB_Conn   : in out SQLite.Data_Base;
      Namespace :        UBS.Unbounded_String);
   procedure Remove_Album
     (DB_Conn   : in out SQLite.Data_Base;
      Namespace :        UBS.Unbounded_String;
      Path      :        Album_Path);
   procedure Checkout_Album
     (DB_Conn   : in out SQLite.Data_Base;
      Namespace :        UBS.Unbounded_String;
      Path      :        Album_Path);
   procedure Checkout (DB_Conn : in out SQLite.Data_Base; Item : Album_Info);
   function From_Row (Row : SQLite.Statement) return Album_Info;
   function Get_Head_Id
     (DB_Conn   : in out SQLite.Data_Base;
      Namespace :        UBS.Unbounded_String) return Integer;

   procedure Add_To_Head
     (DB_Conn   : in out SQLite.Data_Base;
      Namespace :        UBS.Unbounded_String;
      Item      :        file_item.File_Info);

   procedure Add_To_Album
     (DB_Conn  : in out SQLite.Data_Base;
      Album_Id :        Integer;
      Item     :        file_item.File_Info);

   function In_Album
     (DB_Conn  : in out SQLite.Data_Base;
      Album_Id :        Integer;
      Item     :        file_item.File_Info) return Boolean;

   procedure Checkout_Album_Files
     (DB_Conn  : in out SQLite.Data_Base;
      Album_Id :        Integer);
end album;

with Ada.Containers.Ordered_Maps;
with Ada.Containers.Multiway_Trees;
with Ada.Streams.Stream_IO;
with Ada.Strings.Unbounded;
with Ada.Strings.Fixed;

with file_sha1;
with Status;

package album is
   package STIO renames Ada.Streams.Stream_IO;
   package UBS renames Ada.Strings.Unbounded;
   package Fixed_Str renames Ada.Strings.Fixed;
   use UBS;

   type Album_Path is array (Positive range <>) of UBS.Unbounded_String;

   package Namespace_Map is new Ada.Containers.Ordered_Maps
     (Key_Type     => UBS.Unbounded_String,
      Element_Type => file_sha1.Sha1_value);

   procedure Create
     (Map  : in out Namespace_Map.Map;
      Name :        UBS.Unbounded_String);
   procedure Create (Map : in out Namespace_Map.Map; Name : String);
   procedure Remove (Map : in out Namespace_Map.Map; Name : String);
   procedure Load (Map : out Namespace_Map.Map; Path : String);
   procedure Save (Map : in Namespace_Map.Map; Path : String);
   procedure Display_Namespaces (Map : Namespace_Map.Map);

   function Namespace_Pointer
     (Map  : Namespace_Map.Map;
      Name : UBS.Unbounded_String) return file_sha1.Sha1_value;
   function Contains
     (Map : Namespace_Map.Map;
      Key : String) return Boolean is
     (Namespace_Map.Contains (Map, UBS.To_Unbounded_String (Key)));
   procedure Update_Namespace
     (Map     : in out Namespace_Map.Map;
      Name    : UBS.Unbounded_String;
      Pointer : file_sha1.Sha1_value);

   type Album_Info is tagged record
      Unique_Id       : file_sha1.Sha1_value;
      Entries_Pointer : file_sha1.Sha1_value := file_sha1.Empty_Sha1;
      Is_Head : Boolean := False;
      Name            : UBS.Unbounded_String;
   end record;

   function "<" (a, b : Album_Info) return Boolean;
   function ">" (a, b : Album_Info) return Boolean;

   package Trees is new Ada.Containers.Multiway_Trees (Album_Info);

   procedure Add_Album
     (T    : in out Trees.Tree;
      Stat : in out Status.Status_Map.Map;
      Path :        Album_Path);
   procedure Save_Albums (Tree_Data : Trees.Tree; File_Path : String);
   procedure Load_Albums (Tree_Data : out Trees.Tree; File_Path : String);
   procedure Display_Tree (Tree_Cursor : Trees.Cursor; Level : Integer);
   function Find_In_Branch
     (C    : Trees.Cursor;
      Name : UBS.Unbounded_String) return Trees.Cursor;
   procedure Remove_Album(Tree_Data : in out Trees.Tree; Path : Album_Path);
end album;

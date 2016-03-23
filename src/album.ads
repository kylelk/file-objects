with Ada.Containers.Ordered_Sets;
with Ada.Containers.Ordered_Maps;
with Ada.Text_IO;
with Ada.Streams.Stream_IO;
with Ada.Strings.Unbounded;

package album is
   package STIO renames Ada.Streams.Stream_IO;
   package UBS renames Ada.Strings.Unbounded;

   subtype Sha1_value is String(1..40);

   Empty_Sha1 : constant Sha1_Value := "da39a3ee5e6b4b0d3255bfef95601890afd80709";

   use UBS;

   package Namespace_Map is new Ada.Containers.Ordered_Maps(
                                                            Key_Type=>UBS.Unbounded_String,
                                                            Element_Type=>Sha1_Value
                                                           );

   procedure Create(Map : in out Namespace_Map.Map; Name : UBS.Unbounded_String);
   procedure Load(Map : out Namespace_Map.Map; Path : String);
   procedure Save(Map : in Namespace_Map.Map; Path : String);

   type Album_Info is tagged record
      -- SHA-1 of the album entries file
      Entries_Pointer  : Sha1_value := "da39a3ee5e6b4b0d3255bfef95601890afd80709";
      -- SHA-1 of the children album entries file
      Children_Pointer : Sha1_value := "da39a3ee5e6b4b0d3255bfef95601890afd80709";
      Name             : UBS.Unbounded_String; -- := (others=> ' ');
   end record;

   function "<" (a, b : Album_Info) return Boolean;
   function ">" (a, b : Album_Info) return Boolean;

   procedure Create
     (item            : in out Album_Info;
      entries_pointer :        Sha1_value;
      name            :        String);
   procedure Create(item : in out Album_Info; name : String);

   package Album_Set is new Ada.Containers.Ordered_Sets (Album_Info);

   procedure Save_Albums (Album_Items : in Album_Set.Set; path : String);
   procedure Load_Albums(Album_Items : out Album_Set.Set; path : String);
   procedure Print_Tree (Album_Items : Album_Set.Set);

private

   procedure Display_Album_Level
     (Album_Items : Album_Set.Set;
      Level       : Integer);
end album;

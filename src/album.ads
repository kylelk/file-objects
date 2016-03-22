with Ada.Containers.Ordered_Sets;
with Ada.Text_IO;
with Ada.Streams.Stream_IO;

package album is
   package STIO renames Ada.Streams.Stream_IO;

   type Album_Info is tagged record
      -- SHA-1 of the album entries file
      Entries_Pointer  : String (1 .. 40) := "da39a3ee5e6b4b0d3255bfef95601890afd80709";
      --Seperator : Character := ' ';
      -- SHA-1 of the children album entries file
      Children_Pointer : String (1 .. 40) := "da39a3ee5e6b4b0d3255bfef95601890afd80709";
      --Seperator_Name : Character := ' ';
      Name             : String(1..127);-- := (others=> ' ');
      line_ending : Character := ASCII.LF;
   end record;

   function "<" (a, b : Album_Info) return Boolean;
   function ">" (a, b : Album_Info) return Boolean;
   procedure Create
     (item            : in out Album_Info;
      entries_pointer :        String;
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

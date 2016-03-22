with Ada.Containers.Ordered_Sets;
with Ada.Strings.Unbounded;
with Ada.Text_IO.Unbounded_IO;
with Ada.Text_IO;

package album is
   use Ada.Strings.Unbounded;
   type Name_size is new Integer range 1 .. 255;

   type Album_Info is tagged record
      -- SHA-1 of the album entries file
      Entries_Pointer  : String (1 .. 40);
      Name_Length      : Name_size;
      Name             : Unbounded_String;
      -- SHA-1 of the album entries file
      Children_Pointer : String (1 .. 40);
   end record;

   function "<" (a, b : Album_Info) return Boolean;
   function ">" (a, b : Album_Info) return Boolean;
   procedure Create
     (item            : in out Album_Info;
      entries_pointer :        String;
      name            :        String);
   procedure Create
     (item            : in out Album_Info;
      entries_pointer :        String;
      name            :        Unbounded_String);
   procedure Update_Name_Length (Item : in out Album_Info);

   package Album_Set is new Ada.Containers.Ordered_Sets (Album_Info);
   procedure Save_Albums (Album_Items : Album_Set.Set);

   procedure Print_Tree (Album_Items : Album_Set.Set);

private

   procedure Display_Album_Level
     (Album_Items : Album_Set.Set;
      Level       : Integer);
end album;

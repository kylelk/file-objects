with Ada.Containers.Hashed_Maps;
with Ada.Strings.Unbounded;
with Ada.Streams.Stream_IO;
with Ada.Strings.Unbounded.Hash;

package Status is
   package STIO renames Ada.Streams.Stream_IO;
   package UBS renames Ada.Strings.Unbounded;
   use UBS;
   
   function Hash (Key : UBS.Unbounded_String) return Ada.Containers.Hash_Type;
   
   package Status_Map is new Ada.Containers.Hashed_Maps
     (Key_Type     => UBS.Unbounded_String,
      Element_Type => UBS.Unbounded_String,
      Hash => Hash,
      Equivalent_Keys => "=");
      
      
    procedure Load(Map : out Status_Map.Map; Path : String);
    procedure Save(Map : in Status_Map.Map; Path : String);
    
    function Contains (Map : Status_Map.Map; Key : String) return Boolean 
   is (Status_Map.Contains(Map, UBS.To_Unbounded_String(Key)));
   
   procedure Set(Map : in out Status_Map.Map; Key : String; Value : String);
   procedure Set(Map : in out Status_Map.Map; Key : String; Value : UBS.Unbounded_String);
   function Get(Map : Status_Map.Map; Key : String) return String;
   procedure Set_Default_Value(Map : in out Status_Map.Map; Key : String; Value : String);
end status;

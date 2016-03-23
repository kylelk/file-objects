with Ada.Containers.Hashed_Maps;
with Ada.Strings.Unbounded;
with Ada.Streams.Stream_IO;
with Ada.Strings.Unbounded.Hash;

package Status is
   package STIO renames Ada.Streams.Stream_IO;
   package UBS renames Ada.Strings.Unbounded;
   use UBS;
   
   type Entry_Type is (String_Value, Integer_Value, Boolean_Value, Object_Pointer);
   
   type Status_Entry is record
        Value_Type : Entry_Type;
        Value : UBS.Unbounded_String;
   end record;
   
   function Hash (Key : UBS.Unbounded_String) return Ada.Containers.Hash_Type;
   
   package Status_Map is new Ada.Containers.Hashed_Maps
     (Key_Type     => UBS.Unbounded_String,
      Element_Type => Status_Entry,
      Hash => Hash,
      Equivalent_Keys => "=");
      
    procedure Load(Map : out Status_Map.Map; Path : String);
    procedure Save(Map : in Status_Map.Map; Path : String);
    
    procedure Set(Map : in out Status_Map.Map; Key : String; Value : String);
    procedure Set(Map : in out Status_Map.Map; Key : String; Value : UBS.Unbounded_String);
    procedure Set(Map : in out Status_Map.Map; Key : String; Value : Integer);
    procedure Set(Map : in out Status_Map.Map; Key : String; Value : Boolean);
    
    procedure Get(Map : Status_Map.Map; Key : String; Result : out String);
    procedure Get(Map : Status_Map.Map; Key : String; Result : out UBS.Unbounded_String);
    procedure Get(Map : Status_Map.Map; Key : String; Result : out Integer);
    procedure Get(Map : Status_Map.Map; Key : String; Result : out Boolean);
    
    function Contains (Map : Status_Map.Map; Key : String) return Boolean 
    is (Status_Map.Contains(Map, UBS.To_Unbounded_String(Key)));
    
private
    procedure Save_Entry(Map : in out Status_Map.Map; Key : String; Item : Status_Entry);
end status;
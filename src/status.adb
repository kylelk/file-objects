with Ada.IO_Exceptions;

package body Status is
    function Hash (Key : UBS.Unbounded_String) return Ada.Containers.Hash_Type is 
    (UBS.Hash(Key));
    
    
   procedure Load (Map : out Status_Map.Map; Path : String) is
      File_Handle : STIO.File_Type;
      Data_Stream : STIO.Stream_Access;
   begin
      STIO.Open (File_Handle, STIO.In_File, Path);
      Data_Stream := STIO.Stream (File_Handle);

      begin
         Status_Map.Map'Read (Data_Stream, Map);
      exception
         when Ada.IO_Exceptions.End_Error => null;
      end;

      STIO.Close (File_Handle);
   end Load;
    
    
   procedure Save (Map : in Status_Map.Map; Path : String) is
      File_Handle : STIO.File_Type;
      Data_Stream : STIO.Stream_Access;
   begin
      STIO.Open (File_Handle, STIO.Out_File, Path);
      Data_Stream := STIO.Stream (File_Handle);
      STIO.Reset (File_Handle);
      Status_Map.Map'Write (Data_Stream, Map);
      STIO.Close (File_Handle);
   end Save;
   
    
    procedure Set(Map : in out Status_Map.Map; Key : String; Value : String) is
        Item : Status_Entry;
    begin
        Item := (Value_Type => String_Value, Value => UBS.To_Unbounded_String(Value));
        Save_Entry(Map, Key, Item);
    end Set;
    
    
    procedure Set(Map : in out Status_Map.Map; Key : String; Value : UBS.Unbounded_String) is
          Item : Status_Entry;
    begin
        Item := (Value_Type => String_Value, Value => Value);
        Save_Entry(Map, Key, Item);
    end Set;
    
    
    procedure Set(Map : in out Status_Map.Map; Key : String; Value : Integer) is
    begin
        Set(Map, key, Integer'Image(Value));
    end Set;
    
    
    procedure Set(Map : in out Status_Map.Map; Key : String; Value : Boolean) is
    begin
        Set(Map, key, Boolean'Image(Value));
    end Set;
    
    procedure Get(Map : Status_Map.Map; Key : String; Result : out String) is
        item : Status_Entry;
    begin
        Item := Status_Map.Element(Map, UBS.To_Unbounded_String(key));
        Result := UBS.To_String(Item.Value);
    end Get;
    
    
    procedure Get(Map : Status_Map.Map; Key : String; Result : out UBS.Unbounded_String) is
        item : Status_Entry;
    begin
        Item := Status_Map.Element(Map, UBS.To_Unbounded_String(key));
        Result := Item.Value;
    end Get;
    
    
    procedure Get(Map : Status_Map.Map; Key : String; Result : out Integer) is 
        item : Status_Entry;
    begin
        Item := Status_Map.Element(Map, UBS.To_Unbounded_String(key));
        Result := Integer'Value(UBS.To_String(Item.Value));
    end Get;
    
    
    procedure Get(Map : Status_Map.Map; Key : String; Result : out Boolean) is
        item : Status_Entry;
    begin
        Item := Status_Map.Element(Map, UBS.To_Unbounded_String(key));
        Result := Boolean'Value(UBS.To_String(Item.Value));
    end Get;
    
    
    procedure Save_Entry(Map : in out Status_Map.Map; Key : String; Item : Status_Entry) is
        Key_Name : constant UBS.Unbounded_String := UBS.To_Unbounded_String(Key);
        Result_Cursor : Status_Map.Cursor;
    begin
        if Status_Map.Contains(Map, Key_Name) then
            Result_Cursor := Status_Map.Find(Map, Key_Name);
            Status_Map.Replace_Element(Map, Result_Cursor, Item);
        else
            Status_Map.Insert(Map, Key_Name, Item);
        end if;
    end Save_Entry;
end Status;
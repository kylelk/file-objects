with Ada.IO_Exceptions;
with Ada.Streams;

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
   begin
      Set(Map, Key, UBS.To_Unbounded_String(Value));
   end Set;
   
   procedure Set(Map : in out Status_Map.Map; Key : String; Value : UBS.Unbounded_String) is
      Key_Name : constant UBS.Unbounded_String := UBS.To_Unbounded_String(Key);
      Result_Cursor : Status_Map.Cursor;
   begin
       if Status_Map.Contains(Map, Key_Name) then
            Result_Cursor := Status_Map.Find(Map, Key_Name);
            Status_Map.Replace_Element(Map, Result_Cursor, Value);
        else
            Status_Map.Insert(Map, Key_Name, Value);
        end if;
   end Set;
   
   function Get(Map : Status_Map.Map; Key : String) return String is
      Key_Name : constant UBS.Unbounded_String := UBS.To_Unbounded_String(Key);
   begin
      return UBS.To_String(Status_Map.Element(Map, Key_Name));
   end Get;
   
   procedure Set_Default_Value(Map : in out Status_Map.Map; Key : String; Value : String) is
      Key_Name : constant UBS.Unbounded_String := UBS.To_Unbounded_String(Key);
   begin
      if not Status_Map.Contains(Map, Key_Name) then
         Set(Map, Key, Value);
      end if;
   end Set_Default_Value;
end Status;

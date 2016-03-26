with Ada.Text_IO.Unbounded_IO;
with Ada.Strings.Fixed;
with Ada.IO_Exceptions;
with Ada.Text_IO;

package body album is

   function "<" (a, b : Album_Info) return Boolean is
   begin
      return a.Name < b.Name;
   end "<";

   function ">" (a, b : Album_Info) return Boolean is
   begin
      return a.Name < b.Name;
   end ">";

   procedure Create
     (Map  : in out Namespace_Map.Map;
      Name :        UBS.Unbounded_String)
   is
   begin
      Namespace_Map.Insert (Map, Name, file_sha1.Empty_Sha1);
   end Create;

   procedure Create (Map : in out Namespace_Map.Map; Name : String) is
   begin
      Create (Map, UBS.To_Unbounded_String (Name));
   end Create;

   procedure Remove (Map : in out Namespace_Map.Map; Name : String) is
      Result_Cursor : Namespace_Map.Cursor;
   begin
      Result_Cursor :=
        Namespace_Map.Find (Map, UBS.To_Unbounded_String (Name));
      Namespace_Map.Delete (Map, Result_Cursor);
   end Remove;

   procedure Load (Map : out Namespace_Map.Map; Path : String) is
      File_Handle : STIO.File_Type;
      Data_Stream : STIO.Stream_Access;
   begin
      STIO.Open (File_Handle, STIO.In_File, Path);
      Data_Stream := STIO.Stream (File_Handle);

      begin
         Namespace_Map.Map'Read (Data_Stream, Map);
      exception
         when Ada.IO_Exceptions.End_Error =>
            null;
      end;

      STIO.Close (File_Handle);
   end Load;

   procedure Save (Map : in Namespace_Map.Map; Path : String) is
      File_Handle : STIO.File_Type;
      Data_Stream : STIO.Stream_Access;
   begin
      STIO.Open (File_Handle, STIO.Out_File, Path);
      Data_Stream := STIO.Stream (File_Handle);
      STIO.Reset (File_Handle);
      Namespace_Map.Map'Write (Data_Stream, Map);
      STIO.Close (File_Handle);
   end Save;

   procedure Display_Namespaces (Map : Namespace_Map.Map) is
      Map_Cursor : Namespace_Map.Cursor := Namespace_Map.First (Map);
   begin
      for I in 1 .. (Namespace_Map.Length (Map)) loop
         Ada.Text_IO.Unbounded_IO.Put_Line (Namespace_Map.Key (Map_Cursor));
         Namespace_Map.Next (Map_Cursor);
      end loop;
   end Display_Namespaces;

   function Namespace_Pointer
     (Map  : Namespace_Map.Map;
      Name : UBS.Unbounded_String) return file_sha1.Sha1_value
   is
   begin
      return Namespace_Map.Element (Map, Name);
   end Namespace_Pointer;

   procedure Add_Album
     (Table : in out Album_Table;
      Stat  : in out Status.Status_Map.Map;
      Name  :        String)
   is
      Item : Album_Info;
      Unique_Id : File_Sha1.Sha1_value;
   begin
      Unique_Id := File_Sha1.String_Hash(Status.Get(Stat, "sha1_seed"));
      Item.Unique_Id := Unique_Id;
      Item.Name := UBS.To_Unbounded_String (Name);
      Album.Album_Set.Insert(Table.Entries, Item);
      Status.Set(Stat, "sha1_seed", Unique_Id);
   end Add_Album;

   procedure Save_Albums (Album_Items : in Album_Table; path : String) is
      File_Handle : STIO.File_Type;
      Data_Stream : STIO.Stream_Access;
   -- Set_Cursor  : Album_Set.Cursor := Album_Set.First (Album_Items);
   begin
      STIO.Create (File_Handle, STIO.Out_File, path);
      STIO.Reset (File_Handle);
      Data_Stream := STIO.Stream (File_Handle);
      Album_Table'Write (Data_Stream, Album_Items);
      STIO.Close (File_Handle);
   end Save_Albums;

   procedure Load_Albums (Album_Items : out Album_Table; path : String) is
      File_Handle : STIO.File_Type;
      Data_Stream : STIO.Stream_Access;
   begin
      STIO.Open (File_Handle, STIO.In_File, path);
      -- skip the first five bytes
      STIO.Set_Index (File_Handle, 5);
      Data_Stream := STIO.Stream (File_Handle);

      begin
          Album_Table'Read(Data_Stream, Album_Items);
      exception
         when Ada.IO_Exceptions.End_Error => null;
      end;


      STIO.Close (File_Handle);
   end Load_Albums;

   procedure Print_Tree (Album_Items : Album_Table) is
   begin
      Display_Album_Level (Album_Items, 0);
   end Print_Tree;

   procedure Display_Album_Level
     (Album_Items : Album_Table;
      Level       : Integer)
   is
      use Ada.Strings.Fixed;
      Set_Cursor  : Album_Set.Cursor := Album_Set.First (Album_Items.Entries);
      Indentation : constant Integer := 4;
   begin
      for I in 1 .. (Album_Set.Length (Album_Items.Entries)) loop
         Ada.Text_IO.Put ((Level * Indentation) * " ");
         Ada.Text_IO.Put_Line
           (UBS.To_String (Album_Set.Element (Set_Cursor).Name));
         Album_Set.Next (Set_Cursor);
      end loop;
   end Display_Album_Level;

end album;

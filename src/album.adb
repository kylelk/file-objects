with Ada.Text_IO.Unbounded_IO;
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

   procedure Update_Namespace(Map : in out Namespace_Map.Map; Name : UBS.Unbounded_String; Pointer : File_Sha1.Sha1_value) is
   begin
      Namespace_Map.Replace(Map, Name, Pointer);
   end Update_Namespace;

   procedure Load_Albums (Tree_Data : out Trees.Tree; File_Path : String) is
      File_Handle : STIO.File_Type;
      Data_Stream : STIO.Stream_Access;
   begin
      begin
         STIO.Open (File_Handle, STIO.In_File, File_Path);
         Data_Stream := STIO.Stream (File_Handle);
         Trees.Tree'Read (Data_Stream, Tree_Data);
         STIO.Close (File_Handle);
      exception
         when Ada.IO_Exceptions.End_Error =>
            null;
         when Ada.IO_Exceptions.Name_Error =>
            null;
      end;
   end Load_Albums;

   procedure Save_Albums (Tree_Data : Trees.Tree; File_Path : String) is
      File_Handle : STIO.File_Type;
      Data_Stream : STIO.Stream_Access;
   begin
      STIO.Create (File_Handle, STIO.Out_File, File_Path);
      Data_Stream := STIO.Stream (File_Handle);
      STIO.Reset (File_Handle);
      Trees.Tree'Write (Data_Stream, Tree_Data);
      STIO.Close (File_Handle);
   end Save_Albums;

    function Find_In_Branch
     (C    : Trees.Cursor;
      Name : UBS.Unbounded_String) return Trees.Cursor
   is
      use Trees;
      Next_Item : Trees.Cursor;
   begin
      Next_Item := Trees.First_Child (C);
      while Next_Item /= Trees.No_Element loop
         if Trees.Element (Next_Item).Name = Name then
            return Next_Item;
         end if;
         Next_Item := Trees.Next_Sibling (Next_Item);
      end loop;
      return Trees.No_Element;
   end Find_In_Branch;

   procedure Add_Album (T : in out Trees.Tree; Stat : in out Status.Status_Map.Map; Path : Album_Path) is
      use Trees;
      C      : Trees.Cursor := T.Root;
      Result : Trees.Cursor;
      Item : Album_Info;
      Unique_Id : File_Sha1.Sha1_value;
   begin
      for Name of Path loop
         Result := Find_In_Branch (C, Name);
         if Result = Trees.No_Element then
            Unique_Id := File_Sha1.String_Hash(Status.Get(Stat, "sha1_seed"));
            Item.Unique_Id := Unique_Id;
            Item.Name := Name;
            T.Insert_Child (C, Trees.No_Element, Item, Position => C);
            Status.Set(Stat, "sha1_seed", Unique_Id);
         else
            C := Result;
         end if;
      end loop;
   end Add_Album;

   procedure Display_Tree (Tree_Cursor : Trees.Cursor; Level : Integer) is
      use Trees;
      Next_Item : Trees.Cursor;
   begin
      Next_Item := Trees.First_Child (Tree_Cursor);
      while Next_Item /= Trees.No_Element loop
         Ada.Text_IO.Put (Fixed_Str."*" (Level * 4, " "));
         Ada.Text_IO.Unbounded_IO.Put_Line (Trees.Element (Next_Item).Name);
         if not Trees.Is_Leaf (Next_Item) then
            Display_Tree (Next_Item, Level + 1);
         end if;
         Next_Item := Trees.Next_Sibling (Next_Item);
      end loop;
   end Display_Tree;
end album;

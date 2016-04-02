with Ada.Text_IO.Unbounded_IO;
with Ada.IO_Exceptions;
with Ada.Text_IO;
with Interfaces.C;

package body album is

   function "<" (a, b : Album_Info) return Boolean is
   begin
      return a.Name < b.Name;
   end "<";

   function ">" (a, b : Album_Info) return Boolean is
   begin
      return a.Name < b.Name;
   end ">";

   procedure Create_Namespace
     (DB_Conn : in out SQLite.Data_Base;
      Name :        UBS.Unbounded_String)
   is
   begin
      Create_Namespace (DB_Conn, UBS.To_String (Name));
   end Create_Namespace;

   procedure Create_Namespace (DB_Conn : in out SQLite.Data_Base; Name : String) is
      Insert_Statement : SQLite.Statement;
   begin
      Insert_Statement := SQLite.Prepare(DB_Conn, "INSERT INTO namespaces (title) VALUES (?);");
      SQLite.Bind(Insert_Statement, 1, Name);
      SQLite.Step(Insert_Statement);
   end Create_Namespace;

   procedure Remove_Namespace (DB_Conn : in out SQLite.Data_Base; Name : String) is
      Delete_Statement : SQLite.Statement;
   begin
      Delete_Statement := SQLite.Prepare(DB_Conn, "DELETE FROM namespaces WHERE title=?;");
      SQLite.Bind(Delete_Statement, 1, Name);
      SQLite.Step(Delete_Statement);
   end Remove_Namespace;

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

   procedure Display_Namespaces (DB_Conn : SQLite.Data_Base) is
      Query_Statement : SQLite.Statement;
   begin
      Query_Statement := SQLite.Prepare(DB_Conn, "SELECT title FROM namespaces " &
                                          "ORDER BY title;");
      while SQLite.Step(Query_Statement) loop
         Ada.Text_IO.Put_Line(SQLite.Column(Query_Statement, 1));
      end loop;
   end Display_Namespaces;

   function Namespace_Pointer
     (Map  : Namespace_Map.Map;
      Name : UBS.Unbounded_String) return file_sha1.Sha1_value
   is
   begin
      return Namespace_Map.Element (Map, Name);
   end Namespace_Pointer;

   procedure Update_Namespace
     (Map     : in out Namespace_Map.Map;
      Name    :        UBS.Unbounded_String;
      Pointer :        file_sha1.Sha1_value)
   is
   begin
      Namespace_Map.Replace (Map, Name, Pointer);
   end Update_Namespace;

   function Namespace_Exists(DB_Conn : in out SQLite.Data_Base; Name : String) return Boolean is
      Query_Statement : SQLite.Statement;
      use Interfaces.C;
   begin
      Query_Statement := SQLite.Prepare(DB_Conn, "SELECT COUNT(title) FROM namespaces WHERE title=?;");
      SQLite.Bind(Query_Statement, 1, Name);
      SQLite.Step(Query_Statement);
      return SQLite.Column(Query_Statement, 1) = Int(1);
   end Namespace_Exists;


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

   procedure Add_Album
     (T    : in out Trees.Tree;
      Stat : in out Status.Status_Map.Map;
      Path :        Album_Path)
   is
      use Trees;
      C         : Trees.Cursor    := T.Root;
      Result    : Trees.Cursor;
      Item      : Album_Info;
      Unique_Id : file_sha1.Sha1_value;
      Seed      : constant String := Status.Get (Stat, "sha1_seed");
   begin
      for Name of Path loop
         Result := Find_In_Branch (C, Name);
         if Result = Trees.No_Element then
            Unique_Id      := file_sha1.String_Hash (Seed & Seed);
            Item.Unique_Id := Unique_Id;
            Item.Name      := Name;
            T.Insert_Child (C, Trees.No_Element, Item, Position => C);
            Status.Set (Stat, "sha1_seed", Unique_Id);
         else
            C := Result;
         end if;
      end loop;
   end Add_Album;

   procedure Display_Tree (Tree_Cursor : Trees.Cursor; Level : Integer; Stat : Status.Status_Map.Map) is
      use Trees;
      Next_Item     : Trees.Cursor;
      Status_Symbol : Character := ' ';
      Head_Id : constant String := Get_Head_Id(Stat);
   begin
      Next_Item := Trees.First_Child (Tree_Cursor);
      while Next_Item /= Trees.No_Element loop
         if Trees.Element (Next_Item).Unique_Id = Head_Id then
            Status_Symbol := '@';
         end if;
         Ada.Text_IO.Put (Fixed_Str."*" (Level * 4, " "));
         Ada.Text_IO.Put (Status_Symbol & " ");
         Ada.Text_IO.Unbounded_IO.Put_Line (Trees.Element (Next_Item).Name);
         if not Trees.Is_Leaf (Next_Item) then
            Display_Tree (Next_Item, Level + 1, Stat);
         end if;
         Next_Item := Trees.Next_Sibling (Next_Item);
      end loop;
   end Display_Tree;

   procedure Remove_Album (Tree_Data : in out Trees.Tree; Path : Album_Path) is
      use Trees;
      C      : Trees.Cursor := Tree_Data.Root;
      Name   : UBS.Unbounded_String;
      Result : Trees.Cursor;
   begin
      for I in Path'Range loop
         Name   := Path (I);
         Result := Find_In_Branch (C, Name);
         if Result /= Trees.No_Element then
            if I = Path'Last then
               if not Trees.Is_Leaf (Result) then
                  Trees.Delete_Children (Tree_Data, Result);
               end if;
               Trees.Delete_Leaf (Tree_Data, Result);
            end if;
            C := Result;
         else
            Ada.Text_IO.Put_Line
              (Ada.Text_IO.Standard_Error,
               "cannot find album");
            exit;
         end if;
      end loop;
   end Remove_Album;

   procedure Checkout_Album (Tree_Data : Trees.Tree; Path : Album_Path; Stat : in out Status.Status_Map.Map) is
      use Trees;
      C      : Trees.Cursor := Tree_Data.Root;
      Name   : UBS.Unbounded_String;
      Result : Trees.Cursor;
   begin
      for I in Path'Range loop
         Name   := Path (I);
         Result := Find_In_Branch (C, Name);
         if Result /= Trees.No_Element then
            if I = Path'Last then
               Status.Set(Stat, "head_album_id", Trees.Element(Result).Unique_Id);
            end if;
            C := Result;
         else
            Ada.Text_IO.Put_Line
              (Ada.Text_IO.Standard_Error,
               "cannot find album");
            exit;
         end if;
      end loop;
   end Checkout_Album;

   function Get_Head_Id(Stat : Status.Status_Map.Map) return File_Sha1.Sha1_value is
      Result : File_Sha1.Sha1_Value := File_Sha1.Empty_Sha1;
   begin
      begin
         Result := Status.Get(Stat, "head_album_id");
      exception
         when Constraint_Error => null;
      end;
      return Result;
   end Get_Head_Id;
end album;

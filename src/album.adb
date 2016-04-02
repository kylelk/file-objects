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

   function Find_Album
     (DB_Conn : in out SQLite.Data_Base;
      Namespace : UBS.Unbounded_String;
      Path :  Album_Path) return Album_Info is

      use Interfaces.C;
      Result : Album_Info;
      Depth : Integer := 0;
      Name : UBS.Unbounded_String;
      SQL_Query : constant String := "SELECT * FROM albums WHERE " &
        "namespace=? AND title=? AND ";
      Select_Statement : SQLite.Statement;
      Parent_Id : Int := -1;
      Result_Parent_Id : Int;
   begin
      for I in Path'Range loop
         Name := Path(I);
         if Depth = 0 then
            Select_Statement := SQLite.Prepare(DB_Conn, SQL_Query & "parent_id IS NULL;");
         else
            Select_Statement := SQLite.Prepare(DB_Conn, SQL_Query & "parent_id=?;");
         end if;

         SQLite.Bind(Select_Statement, 1, UBS.To_String(Namespace));
         SQLite.Bind(Select_Statement, 2, UBS.To_String(Name));
         if not (Depth = 0) then
            SQLite.Bind(Select_Statement, 3, Parent_Id);
         end if;

         if SQLite.Step(Select_Statement) then
            Parent_Id := SQLite.Column(Select_Statement, 1);
         else
            null;
            raise No_Album_Exception;
         end if;

         Depth := Depth + 1;
      end loop;

      Result_Parent_Id := SQLite.Column(Select_Statement, 4);
      Result.Id := Integer(Parent_Id);
      Result.Namespace := Namespace;
      Result.Parent_Id := Integer(Result_Parent_Id);
      Result.Depth := Depth - 1;
      Result.Name := Name;


      return Result;
   end Find_Album;

   procedure Add_Album
     (DB_Conn : in out SQLite.Data_Base;
      Namespace : UBS.Unbounded_String;
      Path :        Album_Path) is
   begin

   end Add_Album;



   procedure Display_Tree (DB_Conn : in out SQLite.Data_Base; Namespace : UBS.Unbounded_String) is
   begin
      null;
   end Display_Tree;

   procedure Remove_Album (DB_Conn : in out SQLite.Data_Base; Namespace : UBS.Unbounded_String; Path : Album_Path) is
   begin
      null;
   end Remove_Album;

   procedure Checkout_Album
     (DB_Conn : SQLite.Data_Base;
      Namespace : UBS.Unbounded_String;
      Path : Album_Path;
      Stat : in out Status.Status_Map.Map) is
   begin
      null;
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

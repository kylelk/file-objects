with Ada.IO_Exceptions;
with Ada.Text_IO;
with Interfaces.C;
with Color_Text;

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
      Name    :        UBS.Unbounded_String)
   is
   begin
      Create_Namespace (DB_Conn, UBS.To_String (Name));
   end Create_Namespace;

   procedure Create_Namespace
     (DB_Conn : in out SQLite.Data_Base;
      Name    :        String)
   is
      Insert_Statement : SQLite.Statement;
   begin
      Insert_Statement :=
        SQLite.Prepare (DB_Conn, "INSERT INTO namespaces (title) VALUES (?);");
      SQLite.Bind (Insert_Statement, 1, Name);
      SQLite.Step (Insert_Statement);
   end Create_Namespace;

   procedure Remove_Namespace
     (DB_Conn : in out SQLite.Data_Base;
      Name    :        String)
   is
      Delete_Statement : SQLite.Statement;
   begin
      Delete_Statement :=
        SQLite.Prepare (DB_Conn, "DELETE FROM namespaces WHERE title=?;");
      SQLite.Bind (Delete_Statement, 1, Name);
      SQLite.Step (Delete_Statement);
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

   procedure Display_Namespaces
     (DB_Conn : SQLite.Data_Base;
      Current : UBS.Unbounded_String := UBS.To_Unbounded_String (""))
   is
      Query_Statement : SQLite.Statement;
   begin
      Query_Statement :=
        SQLite.Prepare
          (DB_Conn,
           "SELECT title FROM namespaces " & "ORDER BY title;");
      while SQLite.Step (Query_Statement) loop

         if To_Unbounded_String(SQLite.Column (Query_Statement, 1)) = Current then
            Ada.Text_IO.Put("* ");
            Color_Text.Put (SQLite.Column (Query_Statement, 1), Color_Text.Green);
            Ada.Text_IO.New_Line;
         else
            Ada.Text_IO.Put_Line("  " & SQLite.Column (Query_Statement, 1));
         end if;
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

   function Namespace_Exists
     (DB_Conn : in out SQLite.Data_Base;
      Name    :        String) return Boolean
   is
      Query_Statement : SQLite.Statement;
      use Interfaces.C;
   begin
      Query_Statement :=
        SQLite.Prepare
          (DB_Conn,
           "SELECT COUNT(title) FROM namespaces WHERE title=?;");
      SQLite.Bind (Query_Statement, 1, Name);
      SQLite.Step (Query_Statement);
      return SQLite.Column (Query_Statement, 1) = int (1);
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
     (DB_Conn   : in out SQLite.Data_Base;
      Namespace :        UBS.Unbounded_String;
      Path      :        Album_Path) return Album_Info
   is

      use Interfaces.C;
      Result    : Album_Info;
      Depth     : Integer         := 0;
      Name      : UBS.Unbounded_String;
      SQL_Query : constant String :=
        "SELECT * FROM albums WHERE " & "namespace=? AND title=? AND ";
      Select_Statement : SQLite.Statement;
      Parent_Id        : int := -1;
      Result_Parent_Id : int;
   begin
      for I in Path'Range loop
         Name := Path (I);
         if Depth = 0 then
            Select_Statement :=
              SQLite.Prepare (DB_Conn, SQL_Query & "parent_id IS NULL;");
         else
            Select_Statement :=
              SQLite.Prepare (DB_Conn, SQL_Query & "parent_id=?;");
         end if;

         SQLite.Bind (Select_Statement, 1, UBS.To_String (Namespace));
         SQLite.Bind (Select_Statement, 2, UBS.To_String (Name));
         if not (Depth = 0) then
            SQLite.Bind (Select_Statement, 3, Parent_Id);
         end if;

         if SQLite.Step (Select_Statement) then
            Parent_Id := SQLite.Column (Select_Statement, 1);
         else
            null;
            raise No_Album_Exception;
         end if;

         Depth := Depth + 1;
      end loop;

      Result_Parent_Id := SQLite.Column (Select_Statement, 4);
      Result.Id        := Integer (Parent_Id);
      Result.Namespace := Namespace;
      Result.Parent_Id := Integer (Result_Parent_Id);
      Result.Depth     := Depth - 1;
      Result.Name      := Name;

      return Result;
   end Find_Album;

   procedure Add_Album
     (DB_Conn   : in out SQLite.Data_Base;
      Namespace :        UBS.Unbounded_String;
      Path      :        Album_Path)
   is

      use Interfaces.C;
      type Album_Hierarchy is record
         Id    : int;
         Depth : int;
      end record;

      Album_Path_Hierarchy : array (Path'Range) of Album_Hierarchy;
      Depth                : Integer := 0;
      Result               : Album_Info;
      Name                 : UBS.Unbounded_String;
      Parent_Id            : int     := -1;

      Album_Statement : SQLite.Statement;
      Album_SQL       : constant String :=
        "INSERT INTO ALBUMS " &
        "(namespace, depth, parent_id, title) " &
        "VALUES (?, ?, ?, ?);";
      Hierarchy_Statement : SQLite.Statement;
      Hierarchy_SQL       : constant String :=
        "INSERT INTO album_hierarchy " &
        "(album_id, depth, album_parent_id) VALUES (?, ?, ?)";

   begin
      if not Album_Exists (DB_Conn, Namespace, Path, Result) then
         for I in Path'Range loop
            Name := Path (I);
            if not Album_Exists
                (DB_Conn,
                 Namespace,
                 Path (1 .. I),
                 Result)
            then
               Album_Statement := SQLite.Prepare (DB_Conn, Album_SQL);
               SQLite.Bind (Album_Statement, 1, UBS.To_String (Namespace));
               SQLite.Bind (Album_Statement, 2, int (Depth));
               if not (I = Path'First) then
                  SQLite.Bind (Album_Statement, 3, Parent_Id);
               end if;
               SQLite.Bind (Album_Statement, 4, UBS.To_String (Name));
               SQLite.Step (Album_Statement);
               Parent_Id := int (SQLite.Last_Insert_Row (DB_Conn));
            else
               Parent_Id := int (Result.Id);
            end if;
            Album_Path_Hierarchy (I) :=
              (Id => Parent_Id, Depth => int (Depth));
            Depth := Depth + 1;
         end loop;

         for Item of Album_Path_Hierarchy loop
            begin
               Hierarchy_Statement := SQLite.Prepare (DB_Conn, Hierarchy_SQL);
               SQLite.Bind (Hierarchy_Statement, 1, Parent_Id);
               SQLite.Bind (Hierarchy_Statement, 2, Item.Id);
               SQLite.Bind (Hierarchy_Statement, 3, Item.Depth);
               SQLite.Step (Hierarchy_Statement);
            exception
               when Constraint_Error =>
                  null;
            end;
         end loop;
      end if;
   end Add_Album;

   function Album_Exists
     (DB_Conn   : in out SQLite.Data_Base;
      Namespace :        UBS.Unbounded_String;
      Path      :        Album_Path;
      Result    :    out Album_Info) return Boolean
   is
   begin
      begin
         Result := Find_Album (DB_Conn, Namespace, Path);
         return True;
      exception
         when No_Album_Exception =>
            return False;
      end;
   end Album_Exists;

   procedure Display_Tree
     (DB_Conn   : in out SQLite.Data_Base;
      Namespace :        UBS.Unbounded_String)
   is
      use Interfaces.C;

      Result_Limit    : constant Integer := 1000;
      Query_Statement : SQLite.Statement;
      Query_SQL       : constant String  :=
        "SELECT * FROM albums WHERE " & "namespace=? ORDER BY title;";
      Count_Statement : SQLite.Statement;
      Count_Sql       : constant String :=
        "SELECT COUNT(*) FROM albums " & "WHERE namespace=?;";

      Item : Album_Info;
      type Result_Array is array (Integer range <>) of Album_Info;
      Result_Count : Interfaces.C.int;
      Result_Index : Integer := 1;

      procedure Display_Level (Items : Result_Array; Parent_Id : Integer) is
         use Fixed_Str;
         Indentation : constant Integer := 4;
      begin
         for Item of Items loop
            if Item.Parent_Id = Parent_Id then
               Ada.Text_IO.Put ((Indentation * Item.Depth) * " ");
               Ada.Text_IO.Put ("[" & Item.Id'Img & "] ");
               Ada.Text_IO.Put_Line (UBS.To_String (Item.Name));
               Display_Level (Items, Item.Id);
            end if;
         end loop;
      end Display_Level;
   begin
      Query_Statement := SQLite.Prepare (DB_Conn, Query_SQL);
      SQLite.Bind (Query_Statement, 1, UBS.To_String (Namespace));

      Count_Statement := SQLite.Prepare (DB_Conn, Count_Sql);
      Count_Statement.Bind (1, UBS.To_String (Namespace));
      Count_Statement.Step;
      Result_Count := Count_Statement.Column (1);

      declare
         Result_Items : Result_Array (1 .. Integer (Result_Count));
      begin
         while SQLite.Step (Query_Statement) and Result_Index < Result_Limit
         loop
            Item                        := From_Row (Query_Statement);
            Result_Items (Result_Index) := Item;
            Result_Index                := Result_Index + 1;
         end loop;
         -- 0 is used for null values
         Display_Level (Result_Items, 0);
      end;

   end Display_Tree;

   procedure Remove_Album
     (DB_Conn   : in out SQLite.Data_Base;
      Namespace :        UBS.Unbounded_String;
      Path      :        Album_Path)
   is
      use Interfaces.C;
      Result           : Album_Info;
      Delete_Statement : SQLite.Statement;
      Delete_SQL       : constant String := "DELETE FROM albums WHERE id=?;";
   begin
      if Album_Exists (DB_Conn, Namespace, Path, Result) then
         Delete_Statement := SQLite.Prepare (DB_Conn, Delete_SQL);
         Delete_Statement.Bind (1, int (Result.Id));
         Delete_Statement.Step;
      end if;
   end Remove_Album;

   procedure Checkout_Album
     (DB_Conn   :    in out SQLite.Data_Base;
      Namespace :        UBS.Unbounded_String;
      Path      :        Album_Path)
   is
      use Interfaces.C;
      Result : Album_Info;
      Update_Statement : SQLite.Statement;
      Update_SQL : constant String := "UPDATE namespaces " &
        "SET head_album_id=? WHERE title=?;";
   begin
      Result := Find_Album(DB_Conn, Namespace, path);
      Update_Statement := SQLite.Prepare(DB_Conn, Update_SQL);
      Update_Statement.Bind(1, Int(Result.Id));
      Update_Statement.Bind(2, UBS.To_String(Namespace));
      Update_Statement.Step;
   end Checkout_Album;

   function From_Row (Row : SQLite.Statement) return Album_Info is
      use Interfaces.C;
      Result        : Album_Info;
      Id, Parent_Id : int;
   begin
      Id               := SQLite.Column (Row, 1);
      Result.Id        := Integer (Id);
      Parent_Id        := SQLite.Column (Row, 4);
      Result.Namespace := UBS.To_Unbounded_String (SQLite.Column (Row, 2));
      Result.Parent_Id := Integer (Parent_Id);
      Result.Depth     := Integer (int'Value (SQLite.Column (Row, 3)));
      Result.Name      := UBS.To_Unbounded_String (SQLite.Column (Row, 5));
      return Result;
   end From_Row;

   function Get_Head_Id
     (Stat : Status.Status_Map.Map) return file_sha1.Sha1_value
   is
      Result : file_sha1.Sha1_value := file_sha1.Empty_Sha1;
   begin
      begin
         Result := Status.Get (Stat, "head_album_id");
      exception
         when Constraint_Error =>
            null;
      end;
      return Result;
   end Get_Head_Id;
end album;

with Ada.IO_Exceptions;
with Ada.Text_IO;
with Interfaces.C;
with Color_Text;

with config;

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

         if To_Unbounded_String (SQLite.Column (Query_Statement, 1)) =
           Current
         then
            Ada.Text_IO.Put ("* ");
            Color_Text.Put
              (SQLite.Column (Query_Statement, 1),
               Color_Text.Green);
            Ada.Text_IO.New_Line;
         else
            Ada.Text_IO.Put_Line ("  " & SQLite.Column (Query_Statement, 1));
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
      Result_Index : Integer          := 1;
      Head_Id      : constant Integer := Get_Head_Id (DB_Conn, Namespace);

      procedure Display_Level (Items : Result_Array; Parent_Id : Integer) is
         use Fixed_Str;
         Indentation : constant Integer := 4;
      begin
         for Item of Items loop
            if Item.Parent_Id = Parent_Id then
               Ada.Text_IO.Put ((Indentation * Item.Depth) * " ");
               Ada.Text_IO.Put ("[" & Item.Id'Img & "] ");
               if Item.Id = Head_Id then
                  Color_Text.Put (UBS.To_String (Item.Name), Color_Text.Green);
                  Ada.Text_IO.New_Line;
               else
                  Ada.Text_IO.Put_Line (UBS.To_String (Item.Name));
               end if;
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
     (DB_Conn   : in out SQLite.Data_Base;
      Namespace :        UBS.Unbounded_String;
      Path      :        Album_Path)
   is
      Result : Album_Info;
   begin
      Result := Find_Album (DB_Conn, Namespace, Path);
      Checkout (DB_Conn, Result);
   end Checkout_Album;

   procedure Checkout (DB_Conn : in out SQLite.Data_Base; Item : Album_Info) is
      use Interfaces.C;
      Update_Statement : SQLite.Statement;
      Update_SQL       : constant String :=
        "UPDATE namespaces " & "SET head_album_id=? WHERE title=?;";
   begin
      Update_Statement := SQLite.Prepare (DB_Conn, Update_SQL);
      Update_Statement.Bind (1, int (Item.Id));
      Update_Statement.Bind (2, UBS.To_String (Item.Namespace));
      Update_Statement.Step;
      Checkout_Album_Files (DB_Conn, Item.Id);
   end Checkout;

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
     (DB_Conn   : in out SQLite.Data_Base;
      Namespace :        UBS.Unbounded_String) return Integer
   is
      use Interfaces.C;
      Query_Statement : SQLite.Statement;
      -- added is not null check to not return result when head is null
      Query_SQL : constant String :=
        "SELECT head_album_id from namespaces " &
        "WHERE title=? and head_album_id is not NULL";
   begin
      Query_Statement := SQLite.Prepare (DB_Conn, Query_SQL);
      Query_Statement.Bind (1, UBS.To_String (Namespace));
      if Query_Statement.Step then
         return Integer (int'Value (Query_Statement.Column (1)));
      else
         return -1;
      end if;
   end Get_Head_Id;

   procedure Add_To_Head
     (DB_Conn   : in out SQLite.Data_Base;
      Namespace :        UBS.Unbounded_String;
      Item      :        file_item.File_Info)
   is

      Head_Id : constant Integer := Get_Head_Id (DB_Conn, Namespace);
   begin
      if Head_Id > 0 then
         Add_To_Album (DB_Conn, Head_Id, Item);
      end if;
   end Add_To_Head;

   procedure Add_To_Album
     (DB_Conn  : in out SQLite.Data_Base;
      Album_Id :        Integer;
      Item     :        file_item.File_Info)
   is
      use Interfaces.C;
      Insert_Statement : SQLite.Statement;
      Insert_SQL : constant String := "INSERT INTO album_files VALUES(?,?);";

   begin
      if Album_Id > 0 and Item.Id > 0 then
         if not In_Album (DB_Conn, Album_Id, Item) then
            Insert_Statement := DB_Conn.Prepare (Insert_SQL);
            Insert_Statement.Bind (1, int (Album_Id));
            Insert_Statement.Bind (2, int (Item.Id));
            Insert_Statement.Step;
         end if;
      end if;
   end Add_To_Album;

   function In_Album
     (DB_Conn  : in out SQLite.Data_Base;
      Album_Id :        Integer;
      Item     :        file_item.File_Info) return Boolean
   is
      use Interfaces.C;
      Query_Statement : SQLite.Statement;
      Query_SQL       : constant String :=
        "SELECT * FROM album_files WHERE " & "album_id=? AND file_id=?;";
   begin
      Query_Statement := SQLite.Prepare (DB_Conn, Query_SQL);
      Query_Statement.Bind (1, int (Album_Id));
      Query_Statement.Bind (2, int (Item.Id));
      return Query_Statement.Step;
   end In_Album;

   procedure Checkout_Album_Files
     (DB_Conn  : in out SQLite.Data_Base;
      Album_Id :        Integer)
   is
      Query_Statement : SQLite.Statement;
      Query_SQL       : constant String :=
        "WITH RECURSIVE " &
        "under_alice(id, level) AS ( " &
        "VALUES (?, 0) " &
        "UNION ALL " &
        "SELECT " &
        "albums.id, " &
        "under_alice.level + 1 " &
        "FROM Albums " &
        "JOIN under_alice ON albums.parent_id = under_alice.id " &
        "ORDER BY 2) " &
        "SELECT DISTINCT files.* " &
        "FROM under_alice " &
        "JOIN album_files ON album_files.album_id = under_alice.id " &
        "JOIN files ON album_files.file_id = files.id";
      Result         : file_item.File_Info;
      Checkout_Count : Integer := 0;
   begin
      Query_Statement := DB_Conn.Prepare (Query_SQL);
      Query_Statement.Bind (1, Interfaces.C.int (Album_Id));
      while Query_Statement.Step and Checkout_Count < config.Checkout_Limit
      loop
         Result := file_item.From_Row (Query_Statement);

         if UBS.Length (Result.Extension) > 0 then
            DIRS.Copy_File
              (file_item.Get_Path (Result),
               DIR_OPS.Format_Pathname
                 (config.Checkout_Dir &
                  "/" &
                  Result.sha1 &
                  "." &
                  UBS.To_String (Result.Extension)));

         else
            DIRS.Copy_File
              (file_item.Get_Path (Result),
               DIR_OPS.Format_Pathname
                 (config.Checkout_Dir & "/" & Result.sha1));
         end if;
         Checkout_Count := Checkout_Count + 1;
      end loop;
   end Checkout_Album_Files;

end album;

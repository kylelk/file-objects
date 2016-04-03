with Ada.Directories;
with GNAT.Directory_Operations;
with config;
with Interfaces.C;

package body file_item is
   use GNAT.Directory_Operations;
   package DIRS renames Ada.Directories;

   function get_path (item : file_info) return String is
   begin
      return get_path (item.sha1);
   end get_path;

   function get_path (Sha1 : file_sha1.Sha1_value) return String is
   begin
      return Format_Pathname
          (config.Object_Dir & "/" & Sha1 (1 .. 2) & "/" & Sha1);
   end get_path;

   procedure Create
     (DB_Conn : in out SQLite.Data_Base;
      item    : in out file_info;
      path    :        String)
   is

      use Interfaces.C;
      Insert_Statement : SQLite.Statement;
      Insert_SQL       : constant String :=
        "INSERT INTO files " & "(sha1, file_size, filename) VALUES (?,?,?);";
      Is_New : Boolean;
   begin
      item.sha1       := file_sha1.get_file_sha1 (path);
      item.File_Size  := Integer (Ada.Directories.Size (path));
      item.Extension := To_Unbounded_String (Ada.Directories.Extension (path));
      item.Created_At := Ada.Calendar.Clock;
      item.Filename   := UBS.To_Unbounded_String (DIRS.Simple_Name(path));

      if not Object_Exists (item.sha1) then
         DIRS.Copy_File (path, get_path (item));
      end if;

      if not File_Saved (DB_Conn, item.sha1) then
         Insert_Statement := DB_Conn.Prepare (Insert_SQL);
         Insert_Statement.Bind(1, Item.sha1);
         Insert_Statement.Bind(2, Int(Item.File_Size));
         Insert_Statement.Bind(3, UBS.To_String(Item.Filename));
         if Insert_Statement.Step then
            Item.Id := Integer(DB_Conn.Last_Insert_Row);
         end if;

         Is_New := True;
      else
         Is_New := False;
      end if;
      Item := Find_By_Sha1(DB_Conn, Item.Sha1);
      Item.Is_New := Is_New;
   end Create;

   function Object_Exists (Sha1 : file_sha1.Sha1_value) return Boolean is
   begin
      return DIRS.Exists
          (Format_Pathname
             (config.Object_Dir &
              "/" &
              Sha1 (Sha1'First .. Sha1'First + 1) &
              "/" &
              Sha1));
   end Object_Exists;

   function File_Saved
     (DB_Conn : in out SQLite.Data_Base;
      Sha1    :        file_sha1.Sha1_value) return Boolean
   is
      Query_Statement : SQLite.Statement;
      Query_SQL       : constant String := "SELECT * FROM files WHERE sha1=?;";
   begin
      Query_Statement := SQLite.Prepare (DB_Conn, Query_SQL);
      Query_Statement.Bind (1, Sha1);
      return Query_Statement.Step;
   end File_Saved;

   function Find_By_Sha1
     (DB_Conn : in out SQLite.Data_Base;
      Sha1    :        File_Sha1.Sha1_Value) return File_Info is

      use Interfaces.C;
      Result : File_Info;
      Error_Message : constant String := "could not find file object " & Sha1;
      Query_Statement : SQLite.Statement;
      Query_SQL       : constant String := "SELECT * FROM files WHERE sha1=?;";
   begin

      Query_Statement := SQLite.Prepare (DB_Conn, Query_SQL);
      Query_Statement.Bind (1, Sha1);

      if not Object_Exists(Sha1) then
         raise File_Not_Found with Error_Message;
      end if;

      if Query_Statement.Step then
        Result := From_Row(Query_Statement);
      else
         raise File_Not_Found with Error_Message;
      end if;
      return Result;
   end Find_By_Sha1;

   function From_Row(Row : SQLite.Statement) return File_Info is
      use Interfaces.C;
      Result : File_Info;
   begin
      Result.Id := Integer(Int'Value(Row.Column(1)));
      Result.Sha1 := Row.Column(2);
      Result.File_Size := Integer(Int'Value(Row.Column(4)));
      Result.Filename := UBS.To_Unbounded_String(Row.Column(5));
      Result.Extension := UBS.To_Unbounded_String(DIRS.Extension(Row.Column(5)));
      return Result;
   end From_Row;
end file_item;

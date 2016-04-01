with SQLite;
with Ada.Text_IO;
package body Data_Source is

   procedure Load is
      DB_Conn   : SQLite.Data_Base;
      Command : SQLite.Statement;
   begin
      DB_Conn := SQLite.Open ("sqlite.db");

      Command := SQLite.Prepare (DB_Conn, "select sqlite_version();");
      SQLite.Step(Command);
      Ada.Text_IO.Put_Line(SQLite.Column(Command, 1));
   end Load;

   procedure Save is
   begin
      null;
   end Save;

   function Current_Namespace return String is
   begin
      return "";
   end Current_Namespace;
end Data_Source;

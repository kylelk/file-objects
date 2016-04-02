with SQLite;

package Data_Source is
   procedure Load(DB_Conn: in out SQLite.Data_Base);
   procedure Save(DB_Conn: in out SQLite.Data_Base);

   function Current_Namespace return String;
end Data_Source;

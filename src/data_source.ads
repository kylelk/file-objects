with SQLite;

package Data_Source is
   procedure Load(DB_Conn: in out SQLite.Data_Base);
   procedure Save;

   function Current_Namespace return String;
end Data_Source;

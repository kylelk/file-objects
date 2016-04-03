with config;

package body Data_Source is

   procedure Load (DB_Conn : in out SQLite.Data_Base) is
   begin
      DB_Conn := SQLite.Open (config.Database_File);

      SQLite.Exec (DB_Conn, "PRAGMA foreign_keys = ON;");

      SQLite.Exec
        (DB_Conn,
         "CREATE TABLE IF NOT EXISTS namespaces ( " &
         "title VARCHAR(255) PRIMARY KEY NOT NULL, " &
         "head_album_id INTEGER);");

      SQLite.Exec
        (DB_Conn,
         "CREATE UNIQUE INDEX IF NOT EXISTS namespace_index ON " &
         "namespaces(title)");

      SQLite.Exec
        (DB_Conn,
         "CREATE TABLE IF NOT EXISTS files ( " &
         "id INTEGER PRIMARY KEY AUTOINCREMENT, " &
         "sha1 VARCHAR(40) NOT NULL, " &
         "created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP NOT NULL, " &
         "file_size INTEGER, " &
         "filename VARCHAR(255));");

      SQLite.Exec
        (DB_Conn,
         "CREATE UNIQUE INDEX IF NOT EXISTS files_sha1_index ON files (sha1);");

      SQLite.Exec
        (DB_Conn,
         "CREATE TABLE IF NOT EXISTS albums ( " &
         "id INTEGER PRIMARY KEY AUTOINCREMENT, " &
         "namespace VARCHAR(255) NOT NULL, " &
         "depth INTEGER NOT NULL, " &
         "parent_id INTEGER, " &
         "title VARCHAR(255) NOT NULL, " &
         "created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP NOT NULL, " &
         "FOREIGN KEY (parent_id) REFERENCES albums(id) ON DELETE CASCADE, " &
         "FOREIGN KEY (namespace) REFERENCES namespaces(title) ON DELETE CASCADE);");

      SQLite.Exec
        (DB_Conn,
         "CREATE UNIQUE INDEX IF NOT EXISTS album_title_index ON " &
         "albums (namespace, parent_id, title);");

      SQLite.Exec
        (DB_Conn,
         "CREATE TABLE IF NOT EXISTS album_hierarchy ( " &
         "depth INTEGER NOT NULL, " &
         "album_id INTEGER NOT NULL, " &
         "album_parent_id INTEGER, " &
         "FOREIGN KEY(album_id) REFERENCES albums (id) ON DELETE CASCADE);");

      SQLite.Exec
        (DB_Conn,
         "CREATE UNIQUE INDEX IF NOT EXISTS album_hierarchy_index ON " &
         "album_hierarchy (depth, album_id, album_parent_id);");

      SQLite.Exec
        (DB_Conn,
         "CREATE TABLE IF NOT EXISTS album_files( " &
         "album_id INTEGER NOT NULL, " &
         "file_id INTEGER NOT NULL, " &
         "FOREIGN KEY(album_id) REFERENCES albums(id) ON DELETE CASCADE, " &
         "FOREIGN KEY(file_id) REFERENCES files(id) ON DELETE CASCADE);");

      SQLite.Exec
        (DB_Conn,
         "CREATE UNIQUE INDEX IF NOT EXISTS album_files_index ON " &
         "album_files(album_id, file_id);");
   end Load;

   procedure Save (DB_Conn : in out SQLite.Data_Base) is
      pragma Unreferenced (DB_Conn);
   begin
      null;
   end Save;

   function Current_Namespace return String is
   begin
      return "";
   end Current_Namespace;
end Data_Source;

with Ada.Containers.Ordered_Sets;
with Ada.Strings.Unbounded;
with Ada.Text_IO.Unbounded_IO;
with Ada.Text_IO;
with Ada.Strings.Fixed;

package body album is
   function "<" (a, b : album_info) return Boolean is
   begin
      return a.name < b.name;
   end "<";

   function ">" (a, b : album_info) return Boolean is
   begin
      return a.name < b.name;
   end ">";

   procedure create
     (item            : in out album_info;
      entries_pointer :        String;
      name            :        String)
   is
   begin
      item.entries_pointer := entries_pointer;
      item.name            := To_Unbounded_String (name);
   end create;

   procedure create
     (item            : in out album_info;
      entries_pointer :        String;
      name            :        Unbounded_String)
   is
   begin
      item.entries_pointer := entries_pointer;
      item.name            := name;
   end create;

   procedure update_name_length (item : in out album_info) is
   begin
      item.name_length := Name_size (Length (item.name));
   end update_name_length;

   procedure save_albums (Album_Items : in out Album_Set.Set; path : String) is
      File_Handle : STIO.File_Type;
      Data_Stream : STIO.Stream_Access;
      Set_Cursor : Album_Set.Cursor := Album_Set.First(Album_Items);
      temp_item : Album_Info;
   begin
      for I in 1..(Album_Set.Length(Album_Items)) loop
         temp_item := Album_Set.Element(set_cursor);
         update_name_length(temp_item);
         album_set.Replace(Album_Items, temp_item);
         album_set.Next(set_cursor);
      end loop;
   
      STIO.Open(File_Handle, STIO.Out_File, path);
      Data_Stream := STIO.Stream(File_Handle);
      Album_Set.Set'Write(Data_Stream, Album_Items);
      STIO.Close(File_Handle);
   end save_albums;

   procedure Print_Tree(Album_Items : Album_Set.Set) is
   begin
      Display_Album_Level(Album_Items, 0);
   end Print_Tree;

   procedure Display_Album_Level(Album_Items : Album_Set.Set; Level : Integer) is
      use Ada.Strings.Fixed;
      Set_Cursor : Album_Set.Cursor := Album_Set.First(Album_Items);
      Indentation : Integer := 4;
   begin
      for I in 1..(Album_Set.Length(Album_Items)) loop
        Ada.Text_IO.Put((Level * Indentation) * " ");
        Ada.Text_IO.Unbounded_IO.Put_Line(album_set.Element(set_cursor).Name);
        album_set.Next(set_cursor);
    end loop;
   end Display_Album_Level;

end album;

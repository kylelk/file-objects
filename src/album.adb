with Ada.Containers.Ordered_Sets;
with Ada.Strings.Unbounded;
with Ada.Text_IO.Unbounded_IO;
with Ada.Text_IO;
with Ada.Strings.Fixed;
with Ada.IO_Exceptions;

package body album is
   use Ada.Strings.Unbounded;
   function "<" (a, b : album_info) return Boolean is
   begin
      return a.name < b.name;
   end "<";


   function ">" (a, b : album_info) return Boolean is
   begin
      return a.name < b.name;
   end ">";


   procedure create
      (item : in out album_info;
      entries_pointer : String;
      name : String)
   is
   begin
      item.Entries_pointer := Entries_pointer;
      Create(item, name);
   end create;

   procedure Create(item : in out Album_Info; name : String) is
      use Ada.Strings.Fixed;
   begin
      item.Name := UBS.To_Unbounded_String(Name);
   end Create;

   procedure save_albums (Album_Items : in Album_Set.Set; path : String) is
      File_Handle : STIO.File_Type;
      Data_Stream : STIO.Stream_Access;
      Set_Cursor : Album_Set.Cursor := Album_Set.First(Album_Items);
   begin
      STIO.Create(File_Handle, STIO.Out_File, path);
      STIO.Reset(File_Handle);
      Data_Stream := STIO.Stream(File_Handle);
      Album_Set.Set'Write(Data_Stream, Album_Items);
      STIO.Close(File_Handle);
   end save_albums;


   procedure Load_Albums(Album_Items : out Album_Set.Set; path : String) is
      File_Handle : STIO.File_Type;
      Data_Stream : STIO.Stream_Access;
      Temp_Item   : Album_Info;
   begin
      STIO.Open(File_Handle, STIO.In_File, path);
      -- skip the first five bytes
      STIO.Set_Index(File_Handle, 5);
      Data_Stream := STIO.Stream(File_Handle);
      --Ada.Text_IO.Put_Line("created stream");
      --Album_Set.Set'Read(Data_Stream, Album_Items);

      while not STIO.End_Of_File(File_Handle) loop
         begin
            Album_Info'Read(Data_Stream, Temp_Item);

            -- Ada.Text_IO.Put_Line(Temp_Item.Entries_Pointer);
            -- Ada.Text_IO.Put_Line(Temp_Item.Children_Pointer);
            -- Ada.Text_IO.Unbounded_IO.Put_Line(Temp_Item.Name);
            -- Ada.Text_IO.New_Line;

            Album_Set.Insert(Album_Items, Temp_Item);
         exception
            when Ada.IO_Exceptions.End_Error => null;
         end;
      end loop;

      STIO.Close(File_Handle);
   end Load_Albums;


   procedure Print_Tree(Album_Items : Album_Set.Set) is
   begin
      Display_Album_Level(Album_Items, 0);
   end Print_Tree;


   procedure Display_Album_Level(Album_Items : Album_Set.Set; Level : Integer) is
      use Ada.Strings.Fixed;
      Set_Cursor : Album_Set.Cursor := Album_Set.First(Album_Items);
      Indentation : constant Integer := 4;
   begin
      for I in 1..(Album_Set.Length(Album_Items)) loop
        Ada.Text_IO.Put((Level * Indentation) * " ");
        Ada.Text_IO.Put_Line(UBS.To_String(album_set.Element(set_cursor).Name));
        album_set.Next(set_cursor);
    end loop;
   end Display_Album_Level;

end album;

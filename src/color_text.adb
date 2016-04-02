with Ada.Strings.Fixed;

package body Color_Text is
   procedure Put
     (Str : String;
      Str_Color : Color;
      File : Ada.Text_IO.File_Type := ADA.Text_IO.Standard_Output) is
      Color_Code : constant String := Get_Color_Code(Str_Color);
      Reset_Color : constant String := Get_Color_Code(Reset);
   begin
      Ada.Text_IO.Put(Item => Color_Code & Str & Reset_Color, File => File);
   end Put;

   function Get_Color_Code(Str_Color : Color) return String is
      Color_Code : Integer;
      function To_Str(Num : Integer) return String is
      begin
         return Ada.Strings.Fixed.Trim(Integer'Image(Num), Ada.Strings.Left);
      end To_Str;
   begin
      case Str_Color is
      when Normal | Reset => Color_Code := 0;
      when Red => Color_Code := 31;
      when Green => Color_Code := 32;
      when Blue => Color_Code := 34;
      end case;
      return ASCII.ESC & "[" & To_Str(Color_Code) & "m";
   end Get_Color_Code;
end Color_Text;

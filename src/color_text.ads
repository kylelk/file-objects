with Ada.Text_IO;

package Color_Text is
   type Color is (Normal, Reset, Red, Green, Blue);

   procedure Put(Str : String; Str_Color : Color; File : Ada.Text_IO.File_Type := Ada.Text_IO.Standard_Output);
   function Get_Color_Code(Str_Color : Color) return String;
end Color_Text;

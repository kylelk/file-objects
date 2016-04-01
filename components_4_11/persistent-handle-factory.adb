--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Persistent.Handle.Factory                   Luebeck            --
--  Implementation                                 Winter, 2004       --
--                                                                    --
--                                Last revision :  19:53 12 Jan 2008  --
--                                                                    --
--  This  library  is  free software; you can redistribute it and/or  --
--  modify it under the terms of the GNU General Public  License  as  --
--  published by the Free Software Foundation; either version  2  of  --
--  the License, or (at your option) any later version. This library  --
--  is distributed in the hope that it will be useful,  but  WITHOUT  --
--  ANY   WARRANTY;   without   even   the   implied   warranty   of  --
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU  --
--  General  Public  License  for  more  details.  You  should  have  --
--  received  a  copy  of  the GNU General Public License along with  --
--  this library; if not, write to  the  Free  Software  Foundation,  --
--  Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.    --
--                                                                    --
--  As a special exception, if other files instantiate generics from  --
--  this unit, or you link this unit with other files to produce  an  --
--  executable, this unit does not by  itself  cause  the  resulting  --
--  executable to be covered by the GNU General Public License. This  --
--  exception  does not however invalidate any other reasons why the  --
--  executable file might be covered by the GNU Public License.       --
--____________________________________________________________________--

with Strings_Edit.UTF8.Handling;  use Strings_Edit.UTF8.Handling;

package body Persistent.Handle.Factory is
   function Create_ODBC
            (  Server_Name : Wide_String;
               User_Name   : Wide_String;
               Password    : Wide_String;
               Erase       : Boolean := False
            )  return Storage_Handle is
   begin
      return
         Create_ODBC
         (  Server_Name => To_UTF8 (Server_Name),
            User_Name   => To_UTF8 (User_Name),
            Password    => To_UTF8 (Password),
            Erase       => Erase
         );
   end Create_ODBC;

end Persistent.Handle.Factory;

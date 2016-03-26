package file_sha1 is 
   subtype Sha1_value is String(1..40);
   Empty_Sha1 : constant Sha1_Value := "da39a3ee5e6b4b0d3255bfef95601890afd80709";
   
   function get_file_sha1(file_name : String) return Sha1_value;
   function String_Hash(Data : String) return Sha1_value;
end file_sha1;

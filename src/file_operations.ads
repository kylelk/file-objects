with Ada.Directories;
with GNAT.Directory_Operations;
with Ada.Text_IO;
with Ada.Streams.Stream_IO;

package file_operations is
    package STIO renames Ada.Streams.Stream_IO;

    -- removes a directory and it's contents then creates the
    -- deleted directory again
    procedure remake_directory(path : String);
    procedure create_empty_file(path : String);
end file_operations;

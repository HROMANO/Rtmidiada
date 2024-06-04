pragma Ada_2012;

private with Interfaces.C;

------------
-- Rtmidi --
------------

package Rtmidi is

   type Byte is range 0 .. 255;
   for Byte'Size use 8;

   type Message is array (Positive range <>) of Byte;

   type Rtmidi_Api is
     (Unspecified, Macosx_Core, Linux_Alsa, Unix_Jack, Windows_Mm,
      Rtmidi_Dummy, Web_Midi_Api, Windows_Uwp, Android, Num) with
     Convention => C;

   type Rtmidi_Api_Array is array (Positive range <>) of Rtmidi_Api with
     Convention => C;

   type Error_Type is
     (Warning, Debug_Warning, Unspecified, No_Devices_Found, Invalid_Device,
      Memory_Error, Invalid_Parameter, Invalid_Use, Driver_Error, System_Error,
      Thread_Error) with
     Convention => C;

   function Api_Name (Api : Rtmidi_Api) return String;

   function Api_Display_Name (Api : Rtmidi_Api) return String;

   function Compiled_Api_By_Name (Name : String) return Rtmidi_Api;

   function Get_Compiled_Apis return Rtmidi_Api_Array;

   function Get_Version return String;

   --  procedure error (The_Type : Error_Type;
   --                   Msg      : String);

   function To_String (Msg : Message) return String;

private
   --  type RtMidiWrapper is record
   --      ptr  : System.Address;
   --      data : System.Address;
   --      ok   : Boolean;
   --      msg  : Interfaces.C.Strings.chars_ptr;
   --  end record
   --  with Convention => C;

   type RtMidi is limited null record with
     Convention => C;

   type RtMidiPtr is access all RtMidi;

   procedure Open_Port
     (Device : in out RtMidiPtr; Number : Natural; Name : String);

   procedure Open_Virtual_Port (Device : in out RtMidiPtr; Name : String);

   procedure Close_Port (Device : in out RtMidiPtr);

   function Get_Port_Count (Device : RtMidiPtr) return Natural;

   function Get_Port_Name
     (Device : RtMidiPtr; Number : Natural := 0) return String;

   function To_Message
     (Msg : Interfaces.C.char_array; Length : Interfaces.C.size_t)
      return Message;

   function To_Hex (Value : Natural; Pad : Boolean := True) return String;

end Rtmidi;

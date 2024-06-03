pragma Ada_2012;

private with Interfaces.C;

package Rtmidi is

   type Byte is range 0 .. 255;
   for Byte'Size use 8;

   type Message is array (Positive range <>) of Byte;

   type RtMidiApi is
     (RTMIDI_API_UNSPECIFIED, RTMIDI_API_MACOSX_CORE, RTMIDI_API_LINUX_ALSA,
      RTMIDI_API_UNIX_JACK, RTMIDI_API_WINDOWS_MM, RTMIDI_API_RTMIDI_DUMMY,
      RTMIDI_API_WEB_MIDI_API, RTMIDI_API_WINDOWS_UWP, RTMIDI_API_ANDROID,
      RTMIDI_API_NUM) with
     Convention => C;

   type RtMidiApi_Array is array (Positive range <>) of RtMidiApi with
     Convention => C;

   type Rtmidi_Error_Type is
     (RTMIDI_ERROR_WARNING, RTMIDI_ERROR_DEBUG_WARNING,
      RTMIDI_ERROR_UNSPECIFIED, RTMIDI_ERROR_NO_DEVICES_FOUND,
      RTMIDI_ERROR_INVALID_DEVICE, RTMIDI_ERROR_MEMORY_ERROR,
      RTMIDI_ERROR_INVALID_PARAMETER, RTMIDI_ERROR_INVALID_USE,
      RTMIDI_ERROR_DRIVER_ERROR, RTMIDI_ERROR_SYSTEM_ERROR,
      RTMIDI_ERROR_THREAD_ERROR) with
     Convention => C;

   function Api_Name (api : RtMidiApi) return String;

   function Api_Display_Name (api : RtMidiApi) return String;

   function Compiled_Api_By_Name (name : String) return RtMidiApi;

   function Get_Compiled_Apis return RtMidiApi_Array;

   --  procedure error (error_type : RtMidiErrorType;
   --                   msg        : String);

   function To_String (msg : Message) return String;

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
     (device : in out RtMidiPtr; number : Natural; name : String);

   procedure Open_Virtual_Port (device : in out RtMidiPtr; name : String);

   procedure Close_Port (device : in out RtMidiPtr);

   function Get_Port_Count (device : RtMidiPtr) return Natural;

   function Get_Port_Name
     (device : RtMidiPtr; number : Natural := 0) return String;

   function To_Message
     (msg : Interfaces.C.char_array; length : Interfaces.C.size_t)
      return Message;

   function To_Hex (value : Natural; pad : Boolean := True) return String;

end Rtmidi;

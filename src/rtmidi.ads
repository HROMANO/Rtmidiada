pragma Ada_2012;

with Interfaces.C;
with System;
with Interfaces.C.Extensions;
with Interfaces.C.Strings;

package Rtmidi is

    type RtMidiApi is
        (RTMIDI_API_UNSPECIFIED,
         RTMIDI_API_MACOSX_CORE,
         RTMIDI_API_LINUX_ALSA,
         RTMIDI_API_UNIX_JACK,
         RTMIDI_API_WINDOWS_MM,
         RTMIDI_API_RTMIDI_DUMMY,
         RTMIDI_API_NUM)
    with Convention => C;

    type RtMidiApi_Array is Array (Positive range <>) of RtMidiApi
    with Convention => C;

    type RtMidiErrorType is
        (RTMIDI_ERROR_WARNING,
         RTMIDI_ERROR_DEBUG_WARNING,
         RTMIDI_ERROR_UNSPECIFIED,
         RTMIDI_ERROR_NO_DEVICES_FOUND,
         RTMIDI_ERROR_INVALID_DEVICE,
         RTMIDI_ERROR_MEMORY_ERROR,
         RTMIDI_ERROR_INVALID_PARAMETER,
         RTMIDI_ERROR_INVALID_USE,
         RTMIDI_ERROR_DRIVER_ERROR,
         RTMIDI_ERROR_SYSTEM_ERROR,
         RTMIDI_ERROR_THREAD_ERROR)
    with Convention => C;

    function api_name (api : RtMidiApi)
        return String;

    function api_display_name (api : RtMidiApi)
        return String;

    function compiled_api_by_name(name : String)
        return RtMidiApi;

    function get_compiled_apis return RtMidiApi_Array;

    --procedure error (error_type : RtMidiErrorType;
    --                 msg        : String);

private
    -- type RtMidiWrapper is record
    --     ptr  : System.Address;
    --     data : System.Address;
    --     ok   : Boolean;
    --     msg  : Interfaces.C.Strings.chars_ptr;
    -- end record
    -- with Convention => C;

	type RtMidi is limited null record
	with Convention => C;

    type RtMidiPtr is access all RtMidi;

    procedure open_port (device : in out RtMidiPtr;
                         number : Natural;
                         name   : String);

    procedure open_virtual_port (device : in out RtMidiPtr;
                                 name   : String);

    procedure close_port (device : in out RtMidiPtr);

    function port_count (device : RtMidiPtr)
        return Natural;

    function get_port_name (device : RtMidiPtr;
                            number : Natural := 0)
		return String;

end Rtmidi;

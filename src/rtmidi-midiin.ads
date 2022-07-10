package RtMidi.MidiIn is

    type MidiIn is tagged limited private;

    procedure open_port (self   : in out MidiIn;
                         number : Natural := 0;
                         name   : String := "RtMidi Input");

    procedure open_virtual_port (self : in out MidiIn;
                                 name : String := "RtMidi Input");

    procedure close_port (self : in out MidiIn);

    function port_count (self : in out MidiIn) return Natural;

    function port_name (self   : in out MidiIn;
                        number : Natural)
        return String;

    procedure create (self           : in out MidiIn;
                      api            : RtMidiApi := RTMIDI_API_UNSPECIFIED;
                      clientName     : String := "RtMidi Input Client";
                      queueSizeLimit : Positive := 100);

    procedure free (self : in out MidiIn);

    function get_current_api (self : in out MidiIn) return RtMidiApi;

    procedure ignore_types (self      : in out MidiIn;
                            midiSysex : boolean := True;
                            midiTime  : boolean := True;
                            midiSense : boolean := True);

    procedure cancel_callback (self : in out MidiIn);

    generic
		type User_Data_Type is private;
	package Callback is
		use Interfaces.C;
		type User_Data_Access is access all User_Data_Type;
		type Callback_Type is access procedure
		   (deltatime : Float;
			message   : String;
			user_data : access User_Data_Type);
		procedure set_callback (self      : in out MidiIn;
				                callback  : Callback_Type;
				                user_data : access User_Data_Type);
	end Callback;

	function get_message (self : in out MidiIn; deltatime : out Float)
    	return String;

    function get_message (self : in out MidiIn) return String;

    procedure put_message (self : in out MidiIn);

private

    function create_default
        return RtMidiPtr
    with Import        => True,
         Convention    => C,
         External_Name => "rtmidi_in_create_default";

    type MidiIn is tagged limited record
        device : RtMidiPtr := create_default;
    end record;

end RtMidi.MidiIn;

 package body RtMidi.MidiOut is

    ----------------------------------------------------------------------------
    procedure open_port (self   : in out MidiOut;
                         number : Natural := 0;
                         name   : String := "RtMidi Output") is
    begin
        open_port(self.device, number, name);
    end open_port;

    ----------------------------------------------------------------------------
    procedure open_virtual_port (self : in out MidiOut;
                                 name : String := "RtMidi Output") is
    begin
        open_virtual_port(self.device, name);
    end open_virtual_port;

    ----------------------------------------------------------------------------
    procedure close_port (self : in out MidiOut) is
    begin
        close_port(self.device);
    end close_port;

    ----------------------------------------------------------------------------
    function port_count (self : in out MidiOut)
        return Natural is
    begin
        return port_count(self.device);
    end port_count;

    ----------------------------------------------------------------------------
    function port_name (self   : in out MidiOut;
                        number : Natural)
        return String is
    begin
        return get_port_name(self.device, number);
    end port_name;

    ----------------------------------------------------------------------------
    procedure create (self       : in out MidiOut;
                      api        : RtMidiApi := RTMIDI_API_UNSPECIFIED;
                      clientName : String := "RtMidi Output Client") is

        use Interfaces.C.Strings;

		function rtmidi_out_create (api        : RtMidiApi;
		                            clientName : chars_ptr)
			return RtMidiPtr
		with Import        => True,
		     Convention    => C,
		     External_Name => "rtmidi_out_create";

    begin
        self.device := rtmidi_out_create(api, New_String(clientName));
    end create;

    ----------------------------------------------------------------------------
    procedure free (self : in out MidiOut) is

	    procedure rtmidi_out_free (device : RtMidiPtr)
		with Import        => True,
			 Convention    => C,
			 External_Name => "rtmidi_out_free";

    begin
        rtmidi_out_free(self.device);
    end free;

    ----------------------------------------------------------------------------
    function get_current_api (self : in out MidiOut)
        return RtMidiApi is

		function rtmidi_out_get_current_api (device : RtMidiPtr)
		    return RtMidiApi
		with Import        => True,
		     Convention    => C,
		     External_Name => "rtmidi_out_get_current_api";

    begin
        return rtmidi_out_get_current_api(self.device);
    end get_current_api;

    ----------------------------------------------------------------------------
    function send_message (self    : in out MidiOut;
                           message : String)
        return Integer is

		use Interfaces.C;

        function rtmidi_out_send_message (device  : RtMidiPtr;
                                  		  message : char_array;
                                  		  length  : int)
		return int
		with Import        => True,
		     Convention    => C,
		     External_Name => "rtmidi_out_send_message";

    begin
        return Integer(rtmidi_out_send_message(self.device,
                                               To_C(message, False),
                                               message'Length));
    end send_message;

    ----------------------------------------------------------------------------
end RtMidi.MidiOut;

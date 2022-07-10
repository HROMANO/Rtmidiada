package RtMidi.MidiOut is

    type MidiOut is tagged limited private;

    procedure open_port (self   : in out MidiOut;
                         number : Natural := 0;
                         name   : String := "RtMidi Output");

    procedure open_virtual_port (self : in out MidiOut;
                                 name : String := "RtMidi Output");

    procedure close_port (self : in out MidiOut);

    function port_count (self : in out MidiOut) return Natural;

    function port_name (self   : in out MidiOut;
                        number : Natural)
        return String;

    procedure create (self       : in out MidiOut;
                      api        : RtMidiApi := RTMIDI_API_UNSPECIFIED;
                      clientName : String := "RtMidi Output Client");

    procedure free (self : in out MidiOut);

    function get_current_api (self : in out MidiOut) return RtMidiApi;

    function send_message (self    : in out MidiOut;
                           message : String)
        return Integer;

private

    function rtmidi_out_create_default
        return RtMidiPtr
    with Import        => True,
         Convention    => C,
         External_Name => "rtmidi_out_create_default";

    type MidiOut is tagged limited record
        device : RtMidiPtr := rtmidi_out_create_default;
    end record;

end RtMidi.MidiOut;

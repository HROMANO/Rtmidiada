private with Ada.Finalization;

package Rtmidi.MidiOut is

   type MidiOut is tagged limited private;

   procedure open_port
     (self : in out MidiOut; number : Natural := 0;
      name :        String := "RtMidi Output");

   procedure open_virtual_port
     (self : in out MidiOut; name : String := "RtMidi Output");

   procedure close_port (self : in out MidiOut);

   function port_count (self : MidiOut) return Natural;

   function port_name (self : MidiOut; number : Natural) return String;

   procedure create (self : in out MidiOut);

   procedure create
     (self       : in out MidiOut; api : RtMidiApi := RTMIDI_API_UNSPECIFIED;
      clientName :        String := "RtMidi Output Client");

   function get_current_api (self : MidiOut) return RtMidiApi;

   function send_message
     (self : in out MidiOut; message : String) return Integer;

private

   type MidiOut is new Ada.Finalization.Limited_Controlled with record
      device : RtMidiPtr := null;
   end record;

   procedure free (self : in out MidiOut);

end Rtmidi.MidiOut;

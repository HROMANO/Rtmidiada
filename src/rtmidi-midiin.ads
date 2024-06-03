with Ada.Finalization;

package Rtmidi.MidiIn is

   type MidiIn is tagged limited private;

   procedure open_port
     (self : in out MidiIn; number : Natural := 0;
      name :        String := "RtMidi Input");

   procedure open_virtual_port
     (self : in out MidiIn; name : String := "RtMidi Input");

   procedure close_port (self : in out MidiIn);

   function port_count (self : MidiIn) return Natural;

   function port_name (self : MidiIn; number : Natural) return String;

   procedure create (self : in out MidiIn);

   procedure create
     (self : in out MidiIn; api : RtMidiApi := RTMIDI_API_UNSPECIFIED;
      clientName     :        String   := "RtMidi Input Client";
      queueSizeLimit :        Positive := 100);

   function get_current_api (self : MidiIn) return RtMidiApi;

   procedure ignore_types
     (self     : in out MidiIn; midiSysex : Boolean := True;
      midiTime :        Boolean := True; midiSense : Boolean := True);

   procedure cancel_callback (self : in out MidiIn);

   generic
      type User_Data_Type is private;
   package Callback_Factory is
      use Interfaces.C;
      type User_Data_Access is access all User_Data_Type;
      type Callback_Type is
        access procedure
          (deltatime : Float; msg : Message;
           user_data : access User_Data_Type);
      procedure set_callback
        (self      : in out MidiIn; callback : Callback_Type;
         user_data :        access User_Data_Type);
   end Callback_Factory;

   function get_message (self : MidiIn; deltatime : out Float) return Message;

   function get_message (self : MidiIn) return Message;

   procedure put_message (self : MidiIn);

private

   type MidiIn is new Ada.Finalization.Limited_Controlled with record
      device : RtMidiPtr := null;
   end record;

   procedure free (self : in out MidiIn);

   overriding procedure Finalize (self : in out MidiIn);

end Rtmidi.MidiIn;

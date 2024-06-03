private with Ada.Finalization;

package Rtmidi.MidiOut is

   type MidiOut is tagged limited private;

   procedure Open_Port
     (self : in out MidiOut; number : Natural := 0;
      name :        String := "RtMidi Output");

   procedure Open_Virtual_Port
     (self : in out MidiOut; name : String := "RtMidi Output");

   procedure Close_Port (self : in out MidiOut);

   function Get_Port_Count (self : MidiOut) return Natural;

   function Get_Port_Name (self : MidiOut; number : Natural) return String;

   procedure Create (self : in out MidiOut);

   procedure Create
     (self       : in out MidiOut; api : RtMidiApi := RTMIDI_API_UNSPECIFIED;
      clientName :        String := "RtMidi Output Client");

   function Get_Current_Api (self : MidiOut) return RtMidiApi;

   function Send_Message
     (self : in out MidiOut; message : String) return Integer;

private

   type MidiOut is new Ada.Finalization.Limited_Controlled with record
      device : RtMidiPtr := null;
   end record;

   procedure Free (self : in out MidiOut);

end Rtmidi.MidiOut;

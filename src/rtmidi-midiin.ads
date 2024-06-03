private with Ada.Finalization;

package Rtmidi.MidiIn is

   type MidiIn is tagged limited private;

   procedure Open_Port
     (self : in out MidiIn; number : Natural := 0;
      name :        String := "RtMidi Input");

   procedure Open_Virtual_Port
     (self : in out MidiIn; name : String := "RtMidi Input");

   procedure Close_Port (self : in out MidiIn);

   function Get_Port_Count (self : MidiIn) return Natural;

   function Get_Port_Name (self : MidiIn; number : Natural) return String;

   procedure Create (self : in out MidiIn);

   procedure Create
     (self           : in out MidiIn; api : RtMidiApi := RTMIDI_API_UNSPECIFIED;
      clientName     :        String   := "RtMidi Input Client";
      queueSizeLimit :        Positive := 100);

   function Get_Current_Api (self : MidiIn) return RtMidiApi;

   procedure Ignore_Types
     (self     : in out MidiIn; midiSysex : Boolean := True;
      midiTime :        Boolean := True; midiSense : Boolean := True);

   procedure Cancel_Callback (self : in out MidiIn);

   generic
      type User_Data_Type is private;
   package Callback_Factory is
      type User_Data_Access is access all User_Data_Type;
      type Callback_Type is
        access procedure
          (deltatime : Float; msg : Message;
           user_data : access User_Data_Type);
      procedure Set_Callback
        (self      : in out MidiIn; callback : Callback_Type;
         user_data :        access User_Data_Type);
   end Callback_Factory;

   function Get_Message (self : MidiIn; deltatime : out Float) return Message;

   function Get_Message (self : MidiIn) return Message;

   procedure Put_Message (self : MidiIn);

private

   type MidiIn is new Ada.Finalization.Limited_Controlled with record
      device : RtMidiPtr := null;
   end record;

   procedure Free (self : in out MidiIn);

   overriding procedure Finalize (self : in out MidiIn);

end Rtmidi.MidiIn;

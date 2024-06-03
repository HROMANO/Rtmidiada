private with Ada.Finalization;

package Rtmidi.Midi_Out is

   type Midi_Out is tagged limited private;

   procedure Open_Port
     (Self : in out Midi_Out; Number : Natural := 0;
      Name :        String := "RtMidi Output");

   procedure Open_Virtual_Port
     (Self : in out Midi_Out; Name : String := "RtMidi Output");

   procedure Close_Port (Self : in out Midi_Out);

   function Get_Port_Count (Self : Midi_Out) return Natural;

   function Get_Port_Name (Self : Midi_Out; Number : Natural) return String;

   procedure Create (Self : in out Midi_Out);

   procedure Create
     (Self        : in out Midi_Out; Api : Rtmidi_Api := Unspecified;
      Client_Name :        String := "RtMidi Output Client");

   function Get_Current_Api (Self : Midi_Out) return Rtmidi_Api;

   function Send_Message
     (Self : in out Midi_Out; Message : String) return Integer;

private

   type Midi_Out is new Ada.Finalization.Limited_Controlled with record
      Device : RtMidiPtr := null;
   end record;

   procedure Free (Self : in out Midi_Out);

end Rtmidi.Midi_Out;

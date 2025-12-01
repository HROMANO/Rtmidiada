private with Ada.Finalization;

package Rtmidi.Midi_Out is

   type Midi_Out is tagged limited private;

   procedure Open_Port
     (Self   : in out Midi_Out'Class;
      Number : Natural := 0;
      Name   : String := "RtMidi Output")
   with Pre => Self.Valid;

   procedure Open_Virtual_Port
     (Self : in out Midi_Out'Class; Name : String := "RtMidi Output")
   with Pre => Self.Valid;

   procedure Close_Port (Self : in out Midi_Out'Class)
   with Pre => Self.Valid;

   function Get_Port_Count (Self : Midi_Out'Class) return Natural
   with Pre => Self.Valid;

   function Get_Port_Name
     (Self : Midi_Out'Class; Number : Natural) return String
   with Pre => Self.Valid;

   procedure Create (Self : in out Midi_Out'Class);

   procedure Create
     (Self        : in out Midi_Out'Class;
      Api         : Rtmidi_Api := Unspecified;
      Client_Name : String := "RtMidi Output Client");

   function Get_Current_Api (Self : Midi_Out'Class) return Rtmidi_Api
   with Pre => Self.Valid;

   procedure Send_Message (Self : in out Midi_Out'Class; Msg : Message)
   with Pre => Self.Valid;

   function Success (Self : Midi_Out'Class) return Boolean
   with Pre => Self.Valid;

   function Error_Message (Self : Midi_Out'Class) return String
   with Pre => Self.Valid;

   function Valid (Self : Midi_Out'Class) return Boolean;

private

   type Midi_Out is new Ada.Finalization.Limited_Controlled with record
      Device : RtMidiPtr := Null_RtMidiPtr;
   end record;

   procedure Free (Self : in out Midi_Out'Class)
   with Pre => Self.Valid;

   overriding
   procedure Finalize (Self : in out Midi_Out);

end Rtmidi.Midi_Out;

private with Ada.Finalization;

package Rtmidi.Midi_In is

   type Midi_In is tagged limited private;

   procedure Open_Port
     (Self   : in out Midi_In'Class;
      Number : Natural := 0;
      Name   : String := "RtMidi Input")
   with Pre => Self.Valid;

   procedure Open_Virtual_Port
     (Self : in out Midi_In'Class; Name : String := "RtMidi Input")
   with Pre => Self.Valid;

   procedure Close_Port (Self : in out Midi_In'Class)
   with Pre => Self.Valid;

   function Get_Port_Count (Self : Midi_In'Class) return Natural
   with Pre => Self.Valid;

   function Get_Port_Name
     (Self : Midi_In'Class; Number : Natural) return String
   with Pre => Self.Valid;

   procedure Create (Self : in out Midi_In'Class);

   procedure Create
     (Self             : in out Midi_In'Class;
      Api              : Rtmidi_Api := Unspecified;
      Client_Name      : String := "RtMidi Input Client";
      Queue_Size_Limit : Positive := 100);

   function Get_Current_Api (Self : Midi_In'Class) return Rtmidi_Api
   with Pre => Self.Valid;

   procedure Ignore_Types
     (Self       : in out Midi_In'Class;
      Midi_Sysex : Boolean := True;
      Midi_Time  : Boolean := True;
      Midi_Sense : Boolean := True)
   with Pre => Self.Valid;

   procedure Cancel_Callback (Self : in out Midi_In'Class)
   with
     Pre => Self.Valid and then Self.Callback_Already_Set = True,
     Post => Self.Callback_Already_Set = False;

   function Get_Message
     (Self : Midi_In'Class; Delta_Time : out Float) return Message
   with Pre => Self.Valid;

   function Get_Message (Self : Midi_In'Class) return Message
   with Pre => Self.Valid;

   procedure Put_Message (Self : Midi_In'Class)
   with Pre => Self.Valid;

   function Success (Self : Midi_In'Class) return Boolean
   with Pre => Self.Valid;

   function Error_Message (Self : Midi_In'Class) return String
   with Pre => Self.Valid;

   function Callback_Already_Set (Self : Midi_In'Class) return Boolean
   with Pre => Self.Valid;

   function Valid (Self : Midi_In'Class) return Boolean;

private

   type Midi_In is new Ada.Finalization.Limited_Controlled with record
      Device          : RtMidiPtr := Null_RtMidiPtr;
      Callback_Is_Set : Boolean := False;
   end record;

   procedure Free (Self : in out Midi_In'Class)
   with Pre => Self.Valid;

   overriding
   procedure Finalize (Self : in out Midi_In);

end Rtmidi.Midi_In;

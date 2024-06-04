private with Ada.Finalization;

package Rtmidi.Midi_In is

   type Midi_In is tagged limited private;

   procedure Open_Port
     (Self : in out Midi_In; Number : Natural := 0;
      Name :        String := "RtMidi Input");

   procedure Open_Virtual_Port
     (Self : in out Midi_In; Name : String := "RtMidi Input");

   procedure Close_Port (Self : in out Midi_In);

   function Get_Port_Count (Self : Midi_In) return Natural;

   function Get_Port_Name (Self : Midi_In; Number : Natural) return String;

   procedure Create (Self : in out Midi_In);

   procedure Create
     (Self             : in out Midi_In; Api : Rtmidi_Api := Unspecified;
      Client_Name      :        String   := "RtMidi Input Client";
      Queue_Size_Limit :        Positive := 100);

   function Get_Current_Api (Self : Midi_In) return Rtmidi_Api;

   procedure Ignore_Types
     (Self      : in out Midi_In; Midi_Sysex : Boolean := True;
      Midi_Time :        Boolean := True; Midi_Sense : Boolean := True);

   procedure Cancel_Callback (Self : in out Midi_In);

   generic
      type User_Data_Type is private;
   package Callback_Factory is
      type User_Data_Access is access all User_Data_Type;
      type Callback_Type is
        access procedure
          (Delta_Time : Float; Msg : Message;
           User_Data  : access User_Data_Type);
      procedure Set_Callback
        (Self      : in out Midi_In; Callback : Callback_Type;
         User_Data :        access User_Data_Type);
   end Callback_Factory;

   function Get_Message
     (Self : Midi_In; Delta_Time : out Float) return Message;

   function Get_Message (Self : Midi_In) return Message;

   procedure Put_Message (Self : Midi_In);

   function Valid (Self : Midi_In) return Boolean;

   function Error_Message (Self : Midi_In) return String;

private

   type Midi_In is new Ada.Finalization.Limited_Controlled with record
      Device : RtMidiPtr := null;
   end record;

   procedure Free (Self : in out Midi_In);

   overriding procedure Finalize (Self : in out Midi_In);

end Rtmidi.Midi_In;

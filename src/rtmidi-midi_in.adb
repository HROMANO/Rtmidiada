private with Ada.Text_IO;

private with Interfaces.C;
private with Interfaces.C.Strings;

package body Rtmidi.Midi_In is

   package IC renames Interfaces.C;
   package ICS renames Interfaces.C.Strings;

   ----------------------------------------------------------------------------
   procedure Open_Port
     (Self : in out Midi_In'Class; Number : Natural := 0;
      Name :        String := "RtMidi Input")
   is
   begin
      Open_Port (Self.Device, Number, Name);
   end Open_Port;

   ----------------------------------------------------------------------------
   procedure Open_Virtual_Port
     (Self : in out Midi_In'Class; Name : String := "RtMidi Input")
   is
   begin
      Open_Virtual_Port (Self.Device, Name);
   end Open_Virtual_Port;

   ----------------------------------------------------------------------------
   procedure Close_Port (Self : in out Midi_In'Class) is
   begin
      Close_Port (Self.Device);
   end Close_Port;

   ----------------------------------------------------------------------------
   function Get_Port_Count (Self : Midi_In'Class) return Natural is
   begin
      return Get_Port_Count (Self.Device);
   end Get_Port_Count;

   ----------------------------------------------------------------------------
   function Get_Port_Name
     (Self : Midi_In'Class; Number : Natural) return String
   is
   begin
      return Get_Port_Name (Self.Device, Number);
   end Get_Port_Name;

   ----------------------------------------------------------------------------
   procedure Create (Self : in out Midi_In'Class) is

      function Internal return RtMidiPtr with
        Import        => True, Convention => C,
        External_Name => "rtmidi_in_create_default";

   begin
      if Self.Device /= null then
         Self.Free;
      end if;

      Self.Device := Internal;

   end Create;

   ----------------------------------------------------------------------------
   procedure Create
     (Self             : in out Midi_In'Class; Api : Rtmidi_Api := Unspecified;
      Client_Name      :        String   := "RtMidi Input Client";
      Queue_Size_Limit :        Positive := 100)
   is

      function Internal
        (Api              : Rtmidi_Api; Client_Name : IC.char_array;
         Queue_Size_Limit : IC.unsigned) return RtMidiPtr with
        Import => True, Convention => C, External_Name => "rtmidi_in_create";

      Name : IC.char_array := IC.To_C (Client_Name, True);
   begin
      if Self.Device /= null then
         Self.Free;
      end if;

      Self.Device := Internal (Api, Name, IC.unsigned (Queue_Size_Limit));

   end Create;

   ----------------------------------------------------------------------------
   procedure Free (Self : in out Midi_In'Class) is

      procedure Internal (Device : RtMidiPtr) with
        Import => True, Convention => C, External_Name => "rtmidi_in_free";

   begin
      Internal (Self.Device);
      Self.Device := null;
   end Free;

   ----------------------------------------------------------------------------
   function Get_Current_Api (Self : Midi_In'Class) return Rtmidi_Api is

      function Internal (Device : RtMidiPtr) return Rtmidi_Api with
        Import        => True, Convention => C,
        External_Name => "rtmidi_in_get_current_api";

   begin
      return Internal (Self.Device);
   end Get_Current_Api;

   ----------------------------------------------------------------------------
   procedure Ignore_Types
     (Self      : in out Midi_In'Class; Midi_Sysex : Boolean := True;
      Midi_Time :        Boolean := True; Midi_Sense : Boolean := True)
   is

      procedure Internal
        (Device     : RtMidiPtr; Midi_Sysex : IC.C_bool; Midi_Time : IC.C_bool;
         Midi_Sense : IC.C_bool) with
        Import        => True, Convention => C,
        External_Name => "rtmidi_in_ignore_types";

   begin
      Internal
        (Self.Device, IC.C_bool (Midi_Sysex), IC.C_bool (Midi_Time),
         IC.C_bool (Midi_Sense));
   end Ignore_Types;

   ----------------------------------------------------------------------------
   procedure Cancel_Callback (Self : in out Midi_In'Class) is

      procedure Internal (Device : RtMidiPtr) with
        Import        => True, Convention => C,
        External_Name => "rtmidi_in_cancel_callback";

   begin
      Internal (Self.Device);
      Self.Callback_Is_Set := False;
   end Cancel_Callback;

   ----------------------------------------------------------------------------
   function Get_Message
     (Self : Midi_In'Class; Delta_Time : out Float) return Message
   is

      use type Interfaces.C.size_t;
      use type Interfaces.C.double;

      function Internal
        (Device : RtMidiPtr; Msg : out Message; size : out IC.size_t)
         return IC.double with
        Import        => True, Convention => C,
        External_Name => "rtmidi_in_get_message";

      --  SYSEX messages maximum size is 1_024
      Buffer_Length : IC.size_t := 1_024;
      Buffer        : Message (1 .. Integer (Buffer_Length));
      Ret           : IC.double := 0.0;
      Empty         : Message (1 .. 0);

   begin
      Ret := Internal (Self.Device, Buffer, Buffer_Length);

      if Ret <= 0.0 then
         Delta_Time := 0.0;
         return Empty;
      else
         Delta_Time := Float (Ret);
         return Buffer;
      end if;

   end Get_Message;

   ----------------------------------------------------------------------------
   function Get_Message (Self : Midi_In'Class) return Message is
      Delta_Time : Float := 0.0;
   begin
      return Get_Message (Self, Delta_Time);
   end Get_Message;

   ----------------------------------------------------------------------------
   procedure Put_Message (Self : Midi_In'Class) is

      Msg : constant String := To_String (Get_Message (Self));

   begin

      if Msg'First < Msg'Last then
         Ada.Text_IO.Put_Line (Msg);
      end if;

   end Put_Message;

   ----------------------------------------------------------------------------
   function Success (Self : Midi_In'Class) return Boolean is
     (Success (Self.Device));

   ----------------------------------------------------------------------------
   function Error_Message (Self : Midi_In'Class) return String is
     (Error_Message (Self.Device));

   ----------------------------------------------------------------------------
   function Callback_Already_Set (Self : Midi_In'Class) return Boolean is
     (Self.Callback_Is_Set);

   ----------------------------------------------------------------------------
   function Valid (Self : Midi_In'Class) return Boolean is
     (Self.Device /= null);

   ----------------------------------------------------------------------------
   overriding procedure Finalize (Self : in out Midi_In) is
   begin
      if Self.Valid then
         Self.Free;
      end if;
   end Finalize;

end Rtmidi.Midi_In;

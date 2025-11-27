private with Interfaces.C;
private with Interfaces.C.Strings;

package body Rtmidi.Midi_Out is

   package IC renames Interfaces.C;
   package ICS renames Interfaces.C.Strings;

   ----------------------------------------------------------------------------
   procedure Open_Port
     (Self   : in out Midi_Out'Class;
      Number : Natural := 0;
      Name   : String := "RtMidi Output") is
   begin
      Open_Port (Self.Device, Number, Name);
   end Open_Port;

   ----------------------------------------------------------------------------
   procedure Open_Virtual_Port
     (Self : in out Midi_Out'Class; Name : String := "RtMidi Output") is
   begin
      Open_Virtual_Port (Self.Device, Name);
   end Open_Virtual_Port;

   ----------------------------------------------------------------------------
   procedure Close_Port (Self : in out Midi_Out'Class) is
   begin
      Close_Port (Self.Device);
   end Close_Port;

   ----------------------------------------------------------------------------
   function Get_Port_Count (Self : Midi_Out'Class) return Natural is
   begin
      return Get_Port_Count (Self.Device);
   end Get_Port_Count;

   ----------------------------------------------------------------------------
   function Get_Port_Name
     (Self : Midi_Out'Class; Number : Natural) return String
   is (Get_Port_Name (Self.Device, Number));

   ----------------------------------------------------------------------------
   procedure Create (Self : in out Midi_Out'Class) is

      function Internal return RtMidiPtr
      with
        Import => True,
        Convention => C,
        External_Name => "rtmidi_out_create_default";

   begin
      if Self.Device /= null then
         Self.Free;
      end if;

      Self.Device := Internal;

   end Create;

   ----------------------------------------------------------------------------
   procedure Create
     (Self        : in out Midi_Out'Class;
      Api         : Rtmidi_Api := Unspecified;
      Client_Name : String := "RtMidi Output Client")
   is

      function Internal
        (Api : Rtmidi_Api; Client_Name : IC.char_array) return RtMidiPtr
      with
        Import => True,
        Convention => C,
        External_Name => "rtmidi_out_create";

   begin
      if Self.Device /= null then
         Self.Free;
      end if;

      Self.Device := Internal (Api, IC.To_C (Client_Name));

   end Create;

   ----------------------------------------------------------------------------
   procedure Free (Self : in out Midi_Out'Class) is

      procedure Internal (Device : RtMidiPtr)
      with Import => True, Convention => C, External_Name => "rtmidi_out_free";

   begin
      Internal (Self.Device);
      Self.Device := null;
   end Free;

   ----------------------------------------------------------------------------
   function Get_Current_Api (Self : Midi_Out'Class) return Rtmidi_Api is

      function Internal (Device : RtMidiPtr) return Rtmidi_Api
      with
        Import => True,
        Convention => C,
        External_Name => "rtmidi_out_get_current_api";

   begin
      return Internal (Self.Device);
   end Get_Current_Api;

   ----------------------------------------------------------------------------
   procedure Send_Message (Self : in out Midi_Out'Class; Msg : Message) is

      function Internal
        (device : RtMidiPtr; Msg : Message; length : IC.int) return IC.int
      with
        Import => True,
        Convention => C,
        External_Name => "rtmidi_out_send_message";

      Result : IC.int;
   begin
      Result := Internal (Self.Device, Msg, Msg'Length);
   end Send_Message;

   ----------------------------------------------------------------------------
   function Success (Self : Midi_Out'Class) return Boolean
   is (Success (Self.Device));

   ----------------------------------------------------------------------------
   function Error_Message (Self : Midi_Out'Class) return String
   is (Error_Message (Self.Device));

   ----------------------------------------------------------------------------
   function Valid (Self : Midi_Out'Class) return Boolean
   is (Self.Device /= null);

   ----------------------------------------------------------------------------
   overriding
   procedure Finalize (Self : in out Midi_Out) is
   begin
      if Self.Valid then
         Self.Free;
      end if;
   end Finalize;

   ----------------------------------------------------------------------------
end Rtmidi.Midi_Out;

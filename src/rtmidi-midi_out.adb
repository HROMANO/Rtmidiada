private with Interfaces.C;
private with Interfaces.C.Strings;

package body Rtmidi.Midi_Out is

   package IC renames Interfaces.C;
   package ICS renames Interfaces.C.Strings;

   ----------------------------------------------------------------------------
   procedure Open_Port
     (Self : in out Midi_Out'Class; Number : Natural := 0;
      Name :        String := "RtMidi Output")
   is
   begin
      Open_Port (Self.Device, Number, Name);
   end Open_Port;

   ----------------------------------------------------------------------------
   procedure Open_Virtual_Port
     (Self : in out Midi_Out'Class; Name : String := "RtMidi Output")
   is
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
     (Self : Midi_Out'Class; Number : Natural) return String is
     (Get_Port_Name (Self.Device, Number));

   ----------------------------------------------------------------------------
   procedure Create (Self : in out Midi_Out'Class) is

      function Internal return RtMidiPtr with
        Import        => True, Convention => C,
        External_Name => "rtmidi_out_create_default";

   begin
      if Self.Device /= null then
         Self.Free;
      end if;

      Self.Device := Internal;

   end Create;

   ----------------------------------------------------------------------------
   procedure Create
     (Self        : in out Midi_Out'Class; Api : Rtmidi_Api := Unspecified;
      Client_Name :        String := "RtMidi Output Client")
   is

      function Internal
        (Api : Rtmidi_Api; Client_Name : ICS.chars_ptr) return RtMidiPtr with
        Import => True, Convention => C, External_Name => "rtmidi_out_create";

   begin
      if Self.Device /= null then
         Self.Free;
      end if;

      Self.Device := Internal (Api, ICS.New_String (Client_Name));

   end Create;

   ----------------------------------------------------------------------------
   procedure Free (Self : in out Midi_Out'Class) is

      procedure Internal (Device : RtMidiPtr) with
        Import => True, Convention => C, External_Name => "rtmidi_out_free";

   begin
      Internal (Self.Device);
      Self.Device := null;
   end Free;

   ----------------------------------------------------------------------------
   function Get_Current_Api (Self : Midi_Out'Class) return Rtmidi_Api is

      function Internal (Device : RtMidiPtr) return Rtmidi_Api with
        Import        => True, Convention => C,
        External_Name => "rtmidi_out_get_current_api";

   begin
      return Internal (Self.Device);
   end Get_Current_Api;

   ----------------------------------------------------------------------------
   function Send_Message
     (Self : in out Midi_Out'Class; Message : String) return Integer
   is

      function Internal
        (device : RtMidiPtr; Message : IC.char_array; length : IC.int)
         return IC.int with
        Import        => True, Convention => C,
        External_Name => "rtmidi_out_send_message";

   begin
      return
        Integer
          (Internal (Self.Device, IC.To_C (Message, False), Message'Length));
   end Send_Message;

   ----------------------------------------------------------------------------
   function Success (Self : Midi_Out'Class) return Boolean is
     (Success (Self.Device));

   ----------------------------------------------------------------------------
   function Error_Message (Self : Midi_Out'Class) return String is
     (Error_Message (Self.Device));

   ----------------------------------------------------------------------------
   function Valid (Self : Midi_Out'Class) return Boolean is
     (Self.Device /= null);

   ----------------------------------------------------------------------------
   overriding procedure Finalize (Self : in out Midi_Out) is
   begin
      if Self.Valid then
         Self.Free;
      end if;
   end Finalize;

   ----------------------------------------------------------------------------
end Rtmidi.Midi_Out;

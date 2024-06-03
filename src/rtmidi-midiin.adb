private with Ada.Text_IO;
private with Ada.Unchecked_Conversion;

private with Interfaces.C;
private with Interfaces.C.Strings;
private with Interfaces.C.Extensions;

with System;

package body Rtmidi.MidiIn is

   ----------------------------------------------------------------------------
   procedure Open_Port
     (self : in out MidiIn; number : Natural := 0;
      name :        String := "RtMidi Input")
   is
   begin
      Open_Port (self.device, number, name);
   end Open_Port;

   ----------------------------------------------------------------------------
   procedure Open_Virtual_Port
     (self : in out MidiIn; name : String := "RtMidi Input")
   is
   begin
      Open_Virtual_Port (self.device, name);
   end Open_Virtual_Port;

   ----------------------------------------------------------------------------
   procedure Close_Port (self : in out MidiIn) is
   begin
      Close_Port (self.device);
   end Close_Port;

   ----------------------------------------------------------------------------
   function Get_Port_Count (self : MidiIn) return Natural is
   begin
      return Get_Port_Count (self.device);
   end Get_Port_Count;

   ----------------------------------------------------------------------------
   function Get_Port_Name (self : MidiIn; number : Natural) return String is
   begin
      return Get_Port_Name (self.device, number);
   end Get_Port_Name;

   ----------------------------------------------------------------------------
   procedure Create (self : in out MidiIn) is

      function Internal return RtMidiPtr with
        Import        => True, Convention => C,
        External_Name => "rtmidi_in_create_default";

   begin
      if self.device /= null then
         self.Free;
      end if;

      self.device := Internal;

   end Create;

   ----------------------------------------------------------------------------
   procedure Create
     (self : in out MidiIn; api : RtMidiApi := RTMIDI_API_UNSPECIFIED;
      clientName     :        String   := "RtMidi Input Client";
      queueSizeLimit :        Positive := 100)
   is

      use Interfaces.C;
      use Interfaces.C.Strings;

      function Internal
        (api : RtMidiApi; clientName : chars_ptr; queueSizeLimit : unsigned)
         return RtMidiPtr with
        Import => True, Convention => C, External_Name => "rtmidi_in_create";

   begin
      if self.device /= null then
         self.Free;
      end if;

      self.device :=
        Internal (api, New_String (clientName), unsigned (queueSizeLimit));
   end Create;

   ----------------------------------------------------------------------------
   procedure Free (self : in out MidiIn) is

      procedure Internal (device : RtMidiPtr) with
        Import => True, Convention => C, External_Name => "rtmidi_in_free";

   begin
      Internal (self.device);
      self.device := null;
   end Free;

   ----------------------------------------------------------------------------
   function Get_Current_Api (self : MidiIn) return RtMidiApi is

      function Internal (device : RtMidiPtr) return RtMidiApi with
        Import        => True, Convention => C,
        External_Name => "rtmidi_in_get_current_api";

   begin
      return Internal (self.device);
   end Get_Current_Api;

   ----------------------------------------------------------------------------
   procedure Ignore_Types
     (self     : in out MidiIn; midiSysex : Boolean := True;
      midiTime :        Boolean := True; midiSense : Boolean := True)
   is

      use Interfaces.C.Extensions;

      procedure Internal
        (device    : RtMidiPtr; midiSysex : bool; midiTime : bool;
         midiSense : bool) with
        Import        => True, Convention => C,
        External_Name => "rtmidi_in_ignore_types";

   begin
      Internal
        (self.device, bool (midiSysex), bool (midiTime), bool (midiSense));
   end Ignore_Types;

   ----------------------------------------------------------------------------
   procedure Cancel_Callback (self : in out MidiIn) is

      procedure Internal (device : RtMidiPtr) with
        Import        => True, Convention => C,
        External_Name => "rtmidi_in_cancel_callback";

   begin
      Internal (self.device);
   end Cancel_Callback;

   ----------------------------------------------------------------------------
   package body Callback_Factory is

      use Interfaces.C;

      procedure Set_Callback
        (self      : in out MidiIn; callback : Callback_Type;
         user_data :        access User_Data_Type)
      is

         type Proxy is
           access procedure
             (deltatime : double; buffer : char_array; len : size_t;
              user_data : access User_Data_Type) with
           Convention => C;

         procedure Internal
           (device   : RtMidiPtr; callback : Proxy;
            userData : System.Address) with
           Import        => True, Convention => C,
           External_Name => "rtmidi_in_set_callback";

         function Convert_User_Data is new Ada.Unchecked_Conversion
           (User_Data_Access, System.Address);

         procedure wrapper
           (deltatime : double; buffer : char_array; len : size_t;
            user_data : access User_Data_Type) with
           Convention => C;

         procedure wrapper
           (deltatime : double; buffer : char_array; len : size_t;
            user_data : access User_Data_Type)
         is
         begin
            callback (Float (deltatime), To_Message (buffer, len), user_data);
         end wrapper;

      begin
         Internal
           (device   => self.device, callback => wrapper'Access,
            userData => Convert_User_Data (User_Data_Access (user_data)));
      end Set_Callback;
   end Callback_Factory;

   ----------------------------------------------------------------------------
   function Get_Message (self : MidiIn; deltatime : out Float) return Message
   is

      use Interfaces.C;

      function Internal
        (device : RtMidiPtr; message : out char_array; size : out size_t)
         return double with
        Import        => True, Convention => C,
        External_Name => "rtmidi_in_get_message";

      buflen : size_t := 1_024;
      buffer : char_array (0 .. buflen - 1);
      ret    : double := 0.0;
      empty  : Message (1 .. 0);

   begin
      ret := Internal (self.device, buffer, buflen);

      if ret <= 0.0 then
         deltatime := 0.0;
         return empty;
      else
         deltatime := Float (ret);
         --  buflen has been updated to the real length
         return To_Message (buffer, buflen);
      end if;

   end Get_Message;

   ----------------------------------------------------------------------------
   function Get_Message (self : MidiIn) return Message is
      deltatime : Float := 0.0;
   begin
      return Get_Message (self, deltatime);
   end Get_Message;

   ----------------------------------------------------------------------------
   procedure Put_Message (self : MidiIn) is

      use Ada.Text_IO;

      msg : constant String := To_String (Get_Message (self));

   begin

      if msg'First < msg'Last then
         Put_Line (msg);
      end if;

   end Put_Message;

   ----------------------------------------------------------------------------
   overriding procedure Finalize (self : in out MidiIn) is
   begin
      self.Free;
   end Finalize;

end Rtmidi.MidiIn;

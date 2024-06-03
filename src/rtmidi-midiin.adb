private with Ada.Text_IO;
private with Ada.Unchecked_Conversion;

private with Interfaces.C;
private with Interfaces.C.Strings;
private with Interfaces.C.Extensions;

with System;

package body Rtmidi.MidiIn is

   ----------------------------------------------------------------------------
   procedure open_port
     (self : in out MidiIn; number : Natural := 0;
      name :        String := "RtMidi Input")
   is
   begin
      open_port (self.device, number, name);
   end open_port;

   ----------------------------------------------------------------------------
   procedure open_virtual_port
     (self : in out MidiIn; name : String := "RtMidi Input")
   is
   begin
      open_virtual_port (self.device, name);
   end open_virtual_port;

   ----------------------------------------------------------------------------
   procedure close_port (self : in out MidiIn) is
   begin
      close_port (self.device);
   end close_port;

   ----------------------------------------------------------------------------
   function port_count (self : MidiIn) return Natural is
   begin
      return port_count (self.device);
   end port_count;

   ----------------------------------------------------------------------------
   function port_name (self : MidiIn; number : Natural) return String is
   begin
      return get_port_name (self.device, number);
   end port_name;

   ----------------------------------------------------------------------------
   procedure create (self : in out MidiIn) is

      function Internal return RtMidiPtr with
        Import        => True, Convention => C,
        External_Name => "rtmidi_in_create_default";

   begin
      if self.device /= null then
         self.free;
      end if;

      self.device := Internal;

   end create;

   ----------------------------------------------------------------------------
   procedure create
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
         self.free;
      end if;

      self.device :=
        Internal (api, New_String (clientName), unsigned (queueSizeLimit));
   end create;

   ----------------------------------------------------------------------------
   procedure free (self : in out MidiIn) is

      procedure Internal (device : RtMidiPtr) with
        Import => True, Convention => C, External_Name => "rtmidi_in_free";

   begin
      Internal (self.device);
      self.device := null;
   end free;

   ----------------------------------------------------------------------------
   function get_current_api (self : MidiIn) return RtMidiApi is

      function Internal (device : RtMidiPtr) return RtMidiApi with
        Import        => True, Convention => C,
        External_Name => "rtmidi_in_get_current_api";

   begin
      return Internal (self.device);
   end get_current_api;

   ----------------------------------------------------------------------------
   procedure ignore_types
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
   end ignore_types;

   ----------------------------------------------------------------------------
   procedure cancel_callback (self : in out MidiIn) is

      procedure Internal (device : RtMidiPtr) with
        Import        => True, Convention => C,
        External_Name => "rtmidi_in_cancel_callback";

   begin
      Internal (self.device);
   end cancel_callback;

   ----------------------------------------------------------------------------
   package body Callback_Factory is

      use Interfaces.C;

      procedure set_callback
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
            callback (Float (deltatime), to_message (buffer, len), user_data);
         end wrapper;

      begin
         Internal
           (device   => self.device, callback => wrapper'Access,
            userData => Convert_User_Data (User_Data_Access (user_data)));
      end set_callback;
   end Callback_Factory;

   ----------------------------------------------------------------------------
   function get_message (self : MidiIn; deltatime : out Float) return Message
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
         return to_message (buffer, buflen);
      end if;

   end get_message;

   ----------------------------------------------------------------------------
   function get_message (self : MidiIn) return Message is
      deltatime : Float := 0.0;
   begin
      return get_message (self, deltatime);
   end get_message;

   ----------------------------------------------------------------------------
   procedure put_message (self : MidiIn) is

      use Ada.Text_IO;

      msg : constant String := to_string (get_message (self));

   begin

      if msg'First < msg'Last then
         Put_Line (msg);
      end if;

   end put_message;

   ----------------------------------------------------------------------------
   overriding procedure Finalize (self : in out MidiIn) is
   begin
      self.free;
   end Finalize;

end Rtmidi.MidiIn;

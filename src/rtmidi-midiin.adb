with Ada.Text_IO;
with Ada.Unchecked_Conversion;

with Interfaces.C.Strings;
with Interfaces.C.Extensions;

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

      function create_default return RtMidiPtr with
        Import        => True, Convention => C,
        External_Name => "rtmidi_in_create_default";

   begin
      if self.device /= null then
         self.free;
      end if;

      self.device := create_default;

   end create;

   ----------------------------------------------------------------------------
   procedure create
     (self : in out MidiIn; api : RtMidiApi := RTMIDI_API_UNSPECIFIED;
      clientName     :        String   := "RtMidi Input Client";
      queueSizeLimit :        Positive := 100)
   is

      use Interfaces.C;
      use Interfaces.C.Strings;

      function rtmidi_in_create
        (api : RtMidiApi; clientName : chars_ptr; queueSizeLimit : unsigned)
         return RtMidiPtr with
        Import => True, Convention => C, External_Name => "rtmidi_in_create";

   begin
      if self.device /= null then
         self.free;
      end if;

      self.device :=
        rtmidi_in_create
          (api, New_String (clientName), unsigned (queueSizeLimit));
   end create;

   ----------------------------------------------------------------------------
   procedure free (self : in out MidiIn) is

      procedure rtmidi_in_free (device : RtMidiPtr) with
        Import => True, Convention => C, External_Name => "rtmidi_in_free";

   begin
      rtmidi_in_free (self.device);
      self.device := null;
   end free;

   ----------------------------------------------------------------------------
   function get_current_api (self : MidiIn) return RtMidiApi is

      function rtmidi_in_get_current_api
        (device : RtMidiPtr) return RtMidiApi with
        Import        => True, Convention => C,
        External_Name => "rtmidi_in_get_current_api";

   begin
      return rtmidi_in_get_current_api (self.device);
   end get_current_api;

   ----------------------------------------------------------------------------
   procedure ignore_types
     (self     : in out MidiIn; midiSysex : Boolean := True;
      midiTime :        Boolean := True; midiSense : Boolean := True)
   is

      use Interfaces.C.Extensions;

      procedure rtmidi_in_ignore_types
        (device    : RtMidiPtr; midiSysex : bool; midiTime : bool;
         midiSense : bool) with
        Import        => True, Convention => C,
        External_Name => "rtmidi_in_ignore_types";

   begin
      rtmidi_in_ignore_types
        (self.device, bool (midiSysex), bool (midiTime), bool (midiSense));
   end ignore_types;

   ----------------------------------------------------------------------------
   procedure cancel_callback (self : in out MidiIn) is

      procedure rtmidi_in_cancel_callback (device : RtMidiPtr) with
        Import        => True, Convention => C,
        External_Name => "rtmidi_in_cancel_callback";

   begin
      rtmidi_in_cancel_callback (self.device);
   end cancel_callback;

   ----------------------------------------------------------------------------
   package body Callback_Factory is
      procedure set_callback
        (self      : in out MidiIn; callback : Callback_Type;
         user_data :        access User_Data_Type)
      is

         type Proxy is
           access procedure
             (deltatime : double; buffer : char_array; len : size_t;
              user_data : access User_Data_Type) with
           Convention => C;

         procedure rtmidi_in_set_callback
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
         rtmidi_in_set_callback
           (device   => self.device, callback => wrapper'Access,
            userData => Convert_User_Data (User_Data_Access (user_data)));
      end set_callback;
   end Callback_Factory;

   ----------------------------------------------------------------------------
   function get_message (self : MidiIn; deltatime : out Float) return Message
   is

      use Interfaces.C;

      function rtmidi_in_get_message
        (device : RtMidiPtr; message : out char_array; size : out size_t)
         return double with
        Import        => True, Convention => C,
        External_Name => "rtmidi_in_get_message";

      buflen : size_t := 1_024;
      buffer : char_array (0 .. buflen - 1);
      ret    : double := 0.0;
      empty  : Message (1 .. 0);

   begin
      ret := rtmidi_in_get_message (self.device, buffer, buflen);

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

private with Interfaces.C;
private with Interfaces.C.Strings;

package body Rtmidi.MidiOut is

   ----------------------------------------------------------------------------
   procedure Open_Port
     (self : in out MidiOut; number : Natural := 0;
      name :        String := "RtMidi Output")
   is
   begin
      Open_Port (self.device, number, name);
   end Open_Port;

   ----------------------------------------------------------------------------
   procedure Open_Virtual_Port
     (self : in out MidiOut; name : String := "RtMidi Output")
   is
   begin
      Open_Virtual_Port (self.device, name);
   end Open_Virtual_Port;

   ----------------------------------------------------------------------------
   procedure Close_Port (self : in out MidiOut) is
   begin
      Close_Port (self.device);
   end Close_Port;

   ----------------------------------------------------------------------------
   function Get_Port_Count (self : MidiOut) return Natural is
   begin
      return Get_Port_Count (self.device);
   end Get_Port_Count;

   ----------------------------------------------------------------------------
   function Get_Port_Name (self : MidiOut; number : Natural) return String is
   begin
      return Get_Port_Name (self.device, number);
   end Get_Port_Name;

   ----------------------------------------------------------------------------
   procedure Create (self : in out MidiOut) is

      function Internal return RtMidiPtr with
        Import        => True, Convention => C,
        External_Name => "rtmidi_out_create_default";

   begin
      if self.device /= null then
         self.Free;
      end if;

      self.device := Internal;

   end Create;

   ----------------------------------------------------------------------------
   procedure Create
     (self       : in out MidiOut; api : RtMidiApi := RTMIDI_API_UNSPECIFIED;
      clientName :        String := "RtMidi Output Client")
   is

      use Interfaces.C.Strings;

      function Internal
        (api : RtMidiApi; clientName : chars_ptr) return RtMidiPtr with
        Import => True, Convention => C, External_Name => "rtmidi_out_create";

   begin
      if self.device /= null then
         self.Free;
      end if;

      self.device := Internal (api, New_String (clientName));

   end Create;

   ----------------------------------------------------------------------------
   procedure Free (self : in out MidiOut) is

      procedure Internal (device : RtMidiPtr) with
        Import => True, Convention => C, External_Name => "rtmidi_out_free";

   begin
      Internal (self.device);
      self.device := null;
   end Free;

   ----------------------------------------------------------------------------
   function Get_Current_Api (self : MidiOut) return RtMidiApi is

      function Internal (device : RtMidiPtr) return RtMidiApi with
        Import        => True, Convention => C,
        External_Name => "rtmidi_out_get_current_api";

   begin
      return Internal (self.device);
   end Get_Current_Api;

   ----------------------------------------------------------------------------
   function Send_Message
     (self : in out MidiOut; message : String) return Integer
   is

      use Interfaces.C;

      function Internal
        (device : RtMidiPtr; message : char_array; length : int)
         return int with
        Import        => True, Convention => C,
        External_Name => "rtmidi_out_send_message";

   begin
      return
        Integer
          (Internal (self.device, To_C (message, False), message'Length));
   end Send_Message;

   ----------------------------------------------------------------------------
end Rtmidi.MidiOut;

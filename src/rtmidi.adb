with Interfaces.C.Strings;
with Ada.Strings.Unbounded;

package body Rtmidi is

   ----------------------------------------------------------------------------
   function api_name (api : RtMidiApi) return String is

      use Interfaces.C.Strings;

      function Internal (api : RtMidiApi) return chars_ptr with
        Import => True, Convention => C, External_Name => "rtmidi_api_name";

      name : constant String := Value (Internal (api));
   begin
      return name;
   end api_name;

   ----------------------------------------------------------------------------
   function api_display_name (api : RtMidiApi) return String is

      use Interfaces.C.Strings;

      function Internal (api : RtMidiApi) return chars_ptr with
        Import        => True, Convention => C,
        External_Name => "rtmidi_api_display_name";

      name : constant String := Value (Internal (api));
   begin
      return name;
   end api_display_name;

   ----------------------------------------------------------------------------
   function compiled_api_by_name (name : String) return RtMidiApi is

      use Interfaces.C.Strings;

      function Internal (name : chars_ptr) return RtMidiApi with
        Import        => True, Convention => C,
        External_Name => "rtmidi_compiled_api_by_name";

      api : constant RtMidiApi := Internal (New_String (name));
   begin
      return api;
   end compiled_api_by_name;

   ----------------------------------------------------------------------------
   procedure open_port
     (device : in out RtMidiPtr; number : Natural; name : String)
   is

      use Interfaces.C;
      use Interfaces.C.Strings;

      procedure Internal
        (device     : in out RtMidiPtr;
         portNumber :        unsigned; -- if 0, the first available.
         portName   :        chars_ptr) with
        Import => True, Convention => C, External_Name => "rtmidi_open_port";

   begin
      Internal (device, unsigned (number), New_String (name));
   end open_port;

   ----------------------------------------------------------------------------
   procedure open_virtual_port (device : in out RtMidiPtr; name : String) is

      use Interfaces.C.Strings;

      procedure Internal (device : in out RtMidiPtr; portName : chars_ptr) with
        Import        => True, Convention => C,
        External_Name => "rtmidi_open_virtual_port";

   begin
      Internal (device, New_String (name));
   end open_virtual_port;

   ----------------------------------------------------------------------------
   procedure close_port (device : in out RtMidiPtr) is

      procedure Internal (device : in out RtMidiPtr) with
        Import => True, Convention => C, External_Name => "rtmidi_close_port";

   begin
      Internal (device);
   end close_port;

   ----------------------------------------------------------------------------
   function port_count (device : RtMidiPtr) return Natural is

      use Interfaces.C;

      function Internal (device : RtMidiPtr) return unsigned with
        Import        => True, Convention => C,
        External_Name => "rtmidi_get_port_count";

   begin
      return Natural (Internal (device));
   end port_count;

   ----------------------------------------------------------------------------
   function get_port_name
     (device : RtMidiPtr; number : Natural := 0) return String
   is

      use Interfaces.C;
      use Interfaces.C.Strings;

      function Internal
        (device :     RtMidiPtr; portNumber : unsigned; bufOut : chars_ptr;
         bufLen : out int) return int with
        Import        => True, Convention => C,
        External_Name => "rtmidi_get_port_name";

      result : int := 0;
      buflen : int := 0;
      name   : chars_ptr;
   begin
      result := Internal (device, unsigned (number), Null_Ptr, buflen);

      if result < 0 then
         return "";
      end if;

      name   := New_String ((1 .. Integer (buflen) => ' '));
      result := Internal (device, unsigned (number), name, buflen);

      if result <= 0 then
         return "";
      end if;

      return Value (name, size_t (result));

   end get_port_name;

   ----------------------------------------------------------------------------
   function get_compiled_apis return RtMidiApi_Array is

      --  TODO: quite ugly.

      use Interfaces.C;
      use Interfaces.C.Strings;

      function Internal_Get
        (apis : in out RtMidiApi_Array; apis_size : unsigned) return int with
        Import        => True, Convention => C,
        External_Name => "rtmidi_get_compiled_api";

      function Internal_Get_Num
        (apis : chars_ptr; apis_size : unsigned) return int with
        Import        => True, Convention => C,
        External_Name => "rtmidi_get_compiled_api";

      size : unsigned := 0;
      ret  : int      := 0;

   begin
      ret := Internal_Get_Num (Null_Ptr, 0);

      if ret <= 0 then
         declare
            result : RtMidiApi_Array (1 .. 0);
         begin
            return result;
         end;
      else
         size := unsigned (ret);
      end if;

      declare
         result : RtMidiApi_Array (1 .. Integer (size));
      begin
         ret := Internal_Get (result, size);
         if ret <= 0 then
            declare
               result : RtMidiApi_Array (1 .. 0);
            begin
               return result;
            end;
         else
            return result;
         end if;
      end;

   end get_compiled_apis;

   ----------------------------------------------------------------------------
   function to_string (msg : Message) return String is

      --  Message ensures a 2 character only conversion for to_hex.
      result : String (1 .. (msg'Length * 3 - 1));

   begin
      for i in msg'Range loop
         result (i * 3 - 2 .. i * 3) := to_hex (Integer (msg (i))) & " ";
      end loop;

      return result;
   end to_string;

   ----------------------------------------------------------------------------
   function to_message
     (msg : Interfaces.C.char_array; length : Interfaces.C.size_t)
      return Message
   is

      use Interfaces.C;

      result : Message (1 .. Positive (length));

   begin

      for i in 1 .. Positive (length) loop
         --  msg index starts at 0 and not 1, so 'i - 1'
         result (i) := Byte (Character'Pos (To_Ada (msg (size_t (i - 1)))));
      end loop;

      return result;

   end to_message;

   ----------------------------------------------------------------------------
   function to_hex (value : Natural; pad : Boolean := True) return String is

      use Ada.Strings.Unbounded;

      temp1 : Natural;
      temp2 : Natural;

      hexa   : constant String  := "0123456789ABCDEF";
      result : Unbounded_String := Null_Unbounded_String;
   begin

      if value = 0 then
         if pad then
            return "00";
         else
            return "0";
         end if;
      end if;

      temp2 := value;
      while temp2 > 0 loop
         temp1  := temp2 mod 16;
         result := hexa (temp1 + 1) & result;
         temp2  := temp2 / 16;
      end loop;

      if pad and then (Length (result) mod 2) /= 0 then
         result := "0" & result;
      end if;

      return To_String (result);
   end to_hex;

end Rtmidi;

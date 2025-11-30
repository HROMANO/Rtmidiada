private with Ada.Strings.Unbounded;

package body Rtmidi is

   package IC renames Interfaces.C;
   package ICS renames Interfaces.C.Strings;

   ----------------------------------------------------------------------------
   function Api_Name (Api : Rtmidi_Api) return String is

      function Internal (Api : Rtmidi_Api) return ICS.chars_ptr
      with Import => True, Convention => C, External_Name => "rtmidi_api_name";

   begin
      return ICS.Value (Internal (Api));
   end Api_Name;

   ----------------------------------------------------------------------------
   function Api_Display_Name (Api : Rtmidi_Api) return String is

      function Internal (Api : Rtmidi_Api) return ICS.chars_ptr
      with
        Import => True,
        Convention => C,
        External_Name => "rtmidi_api_display_name";

   begin
      return ICS.Value (Internal (Api));
   end Api_Display_Name;

   ----------------------------------------------------------------------------
   function Compiled_Api_By_Name (Name : String) return Rtmidi_Api is

      function Internal (Name : IC.char_array) return Rtmidi_Api
      with
        Import => True,
        Convention => C,
        External_Name => "rtmidi_compiled_api_by_name";

   begin
      return Internal (IC.To_C (Name, True));
   end Compiled_Api_By_Name;

   ----------------------------------------------------------------------------
   procedure Open_Port (Device : RtMidiPtr; Number : Natural; Name : String) is

      procedure Internal
        (Device      : RtMidiPtr;
         Port_Number : IC.unsigned;
         --  if 0, the first available.
         Port_Name   : IC.char_array)
      with
        Import => True,
        Convention => C,
        External_Name => "rtmidi_open_port";

   begin
      Internal (Device, IC.unsigned (Number), IC.To_C (Name));
   end Open_Port;

   ----------------------------------------------------------------------------
   procedure Open_Virtual_Port (Device : RtMidiPtr; Name : String) is

      procedure Internal (Device : RtMidiPtr; Port_Name : IC.char_array)
      with
        Import => True,
        Convention => C,
        External_Name => "rtmidi_open_virtual_port";

   begin
      Internal (Device, IC.To_C (Name));
   end Open_Virtual_Port;

   ----------------------------------------------------------------------------
   procedure Close_Port (Device : RtMidiPtr) is

      procedure Internal (Device : RtMidiPtr)
      with
        Import => True,
        Convention => C,
        External_Name => "rtmidi_close_port";

   begin
      Internal (Device);
   end Close_Port;

   ----------------------------------------------------------------------------
   function Get_Port_Count (Device : RtMidiPtr) return Natural is

      function Internal (Device : RtMidiPtr) return IC.unsigned
      with
        Import => True,
        Convention => C,
        External_Name => "rtmidi_get_port_count";

   begin
      return Natural (Internal (Device));
   end Get_Port_Count;

   ----------------------------------------------------------------------------
   function Get_Port_Name
     (Device : RtMidiPtr; Number : Natural := 0) return String
   is

      use type IC.int;

      function Internal_Get_Length
        (Device      : RtMidiPtr;
         Port_Number : IC.unsigned;
         Buf_Out     : IC.char;
         Buf_Len     : out IC.int) return IC.int
      with
        Import => True,
        Convention => C,
        External_Name => "rtmidi_get_port_name";

      function Internal_Get_Name
        (Device      : RtMidiPtr;
         Port_Number : IC.unsigned;
         Buf_Out     : out IC.char_array;
         Buf_Len     : out IC.int) return IC.int
      with
        Import => True,
        Convention => C,
        External_Name => "rtmidi_get_port_name";

      Result        : IC.int := 0;
      Buffer_Length : IC.int := 0;
   begin
      --  Get needed length of the buffer
      Result :=
        Internal_Get_Length
          (Device, IC.unsigned (Number), IC.nul, Buffer_Length);

      if Result < 0 then
         return "";
      end if;

      --  Get the name
      declare
         Name : IC.char_array (1 .. IC.size_t (Buffer_Length));
      begin

         Result :=
           Internal_Get_Name
             (Device, IC.unsigned (Number), Name, Buffer_Length);

         if Result <= 0 then
            return "";
         end if;

         return IC.To_Ada (Name, True);
      end;

   end Get_Port_Name;

   ----------------------------------------------------------------------------
   function Get_Compiled_Apis return Rtmidi_Api_Array is

      use type IC.int;

      function Internal_Get_Apis
        (Apis : in out Rtmidi_Api_Array; Apis_Size : IC.unsigned) return IC.int
      with
        Import => True,
        Convention => C,
        External_Name => "rtmidi_get_compiled_api";

      function Internal_Get_Number
        (Apis : ICS.chars_ptr; Apis_Size : IC.unsigned) return IC.int
      with
        Import => True,
        Convention => C,
        External_Name => "rtmidi_get_compiled_api";

      Number : IC.int := 0;

   begin
      Number := Internal_Get_Number (ICS.Null_Ptr, 0);

      if Number <= 0 then
         return Empty_Rtmidi_Api_Array;
      end if;

      declare
         Size   : constant IC.unsigned := Interfaces.C.unsigned (Number);
         Result : Rtmidi_Api_Array (1 .. Integer (Size));
      begin
         Number := Internal_Get_Apis (Result, Size);
         if Number <= 0 then
            return Empty_Rtmidi_Api_Array;
         else
            return Result;
         end if;
      end;

   end Get_Compiled_Apis;

   ----------------------------------------------------------------------------
   function Get_Version return String is

      function Internal return ICS.chars_ptr
      with
        Import => True,
        Convention => C,
        External_Name => "rtmidi_get_version";
   begin
      return ICS.Value (Internal);
   end Get_Version;

   ----------------------------------------------------------------------------
   function Success (Device : RtMidiPtr) return Boolean is
   begin
      return Device.Ok;
   end Success;

   ----------------------------------------------------------------------------
   function Error_Message (Device : RtMidiPtr) return String is
   begin
      return ICS.Value (Device.Error_Message);
   end Error_Message;

   ----------------------------------------------------------------------------
   function To_String (Msg : Message) return String is

      --  Message ensures a 2 character only conversion for To_Hex.
      Result : String (1 .. (Msg'Length * 3 - 1));

   begin
      for i in Msg'Range loop
         if i < Msg'Last then
            Result (i * 3 - 2 .. i * 3) := To_Hex (Integer (Msg (i))) & " ";
         else
            Result (i * 3 - 2 .. i * 3 - 1) := To_Hex (Integer (Msg (i)));
         end if;
      end loop;

      return Result;
   end To_String;

   ----------------------------------------------------------------------------
   function To_Message
     (Msg : Interfaces.C.char_array; Length : Interfaces.C.size_t)
      return Message
   is

      Result : Message (1 .. Positive (Length));

   begin

      for Index in 1 .. Positive (Length) loop
         --  Msg index starts at 0 and not 1, so 'i - 1'
         Result (Index) :=
           Byte (Character'Pos (IC.To_Ada (Msg (IC.size_t (Index - 1)))));
      end loop;

      return Result;

   end To_Message;

   ----------------------------------------------------------------------------
   function To_Hex (Value : Natural; Pad : Boolean := True) return String is

      use Ada.Strings.Unbounded;

      Temp1 : Natural;
      Temp2 : Natural;

      Hexa   : constant String := "0123456789ABCDEF";
      Result : Unbounded_String := Null_Unbounded_String;
   begin

      if Value = 0 then
         return (if Pad then "00" else "0");
      end if;

      Temp2 := Value;
      while Temp2 > 0 loop
         Temp1 := Temp2 mod 16;
         Result := Hexa (Temp1 + 1) & Result;
         Temp2 := Temp2 / 16;
      end loop;

      if Pad and then (Length (Result) mod 2) /= 0 then
         Result := "0" & Result;
      end if;

      return To_String (Result);
   end To_Hex;

end Rtmidi;

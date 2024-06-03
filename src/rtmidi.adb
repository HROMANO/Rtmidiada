private with Ada.Strings.Unbounded;

private with Interfaces.C.Strings;

package body Rtmidi is

   package IC renames Interfaces.C;
   package ICS renames Interfaces.C.Strings;

   ----------------------------------------------------------------------------
   function Api_Name (Api : Rtmidi_Api) return String is

      function Internal (Api : Rtmidi_Api) return ICS.chars_ptr with
        Import => True, Convention => C, External_Name => "rtmidi_api_name";

   begin
      return ICS.Value (Internal (Api));
   end Api_Name;

   ----------------------------------------------------------------------------
   function Api_Display_Name (Api : Rtmidi_Api) return String is

      function Internal (Api : Rtmidi_Api) return ICS.chars_ptr with
        Import        => True, Convention => C,
        External_Name => "rtmidi_api_display_name";

   begin
      return ICS.Value (Internal (Api));
   end Api_Display_Name;

   ----------------------------------------------------------------------------
   function Compiled_api_By_Name (Name : String) return Rtmidi_Api is

      function Internal (Name : ICS.chars_ptr) return Rtmidi_Api with
        Import        => True, Convention => C,
        External_Name => "rtmidi_compiled_api_by_name";

   begin
      return Internal (ICS.New_String (Name));
   end Compiled_api_By_Name;

   ----------------------------------------------------------------------------
   procedure Open_Port
     (Device : in out RtMidiPtr; Number : Natural; Name : String)
   is

      procedure Internal
        (Device      : RtMidiPtr;
         Port_Number : IC.unsigned; -- if 0, the first available.
         Port_Name   : ICS.chars_ptr) with
        Import => True, Convention => C, External_Name => "rtmidi_open_port";

   begin
      Internal (Device, IC.unsigned (Number), ICS.New_String (Name));
   end Open_Port;

   ----------------------------------------------------------------------------
   procedure Open_Virtual_Port (Device : in out RtMidiPtr; Name : String) is

      procedure Internal
        (Device : in out RtMidiPtr; Port_Name : ICS.chars_ptr) with
        Import        => True, Convention => C,
        External_Name => "rtmidi_open_virtual_port";

   begin
      Internal (Device, ICS.New_String (Name));
   end Open_Virtual_Port;

   ----------------------------------------------------------------------------
   procedure Close_Port (Device : in out RtMidiPtr) is

      procedure Internal (Device : RtMidiPtr) with
        Import => True, Convention => C, External_Name => "rtmidi_close_port";

   begin
      Internal (Device);
   end Close_Port;

   ----------------------------------------------------------------------------
   function Get_Port_Count (Device : RtMidiPtr) return Natural is

      function Internal (Device : RtMidiPtr) return IC.unsigned with
        Import        => True, Convention => C,
        External_Name => "rtmidi_get_port_count";

   begin
      return Natural (Internal (Device));
   end Get_Port_Count;

   ----------------------------------------------------------------------------
   function Get_Port_Name
     (Device : RtMidiPtr; Number : Natural := 0) return String
   is

      use type IC.int;

      function Internal
        (Device  : RtMidiPtr; Port_Number : IC.unsigned;
         Buf_Out : ICS.chars_ptr; Buf_Len : out IC.int) return IC.int with
        Import        => True, Convention => C,
        External_Name => "rtmidi_get_port_name";

      Result  : IC.int := 0;
      Buf_Len : IC.int := 0;
      Name    : ICS.chars_ptr;
   begin
      Result := Internal (Device, IC.unsigned (Number), ICS.Null_Ptr, Buf_Len);

      if Result < 0 then
         return "";
      end if;

      Name   := ICS.New_String ((1 .. Integer (Buf_Len) => ' '));
      Result := Internal (Device, IC.unsigned (Number), Name, Buf_Len);

      if Result <= 0 then
         return "";
      end if;

      return ICS.Value (Name, IC.size_t (Result));

   end Get_Port_Name;

   ----------------------------------------------------------------------------
   function Get_Compiled_apis return Rtmidi_Api_Array is

      --  TODO: quite ugly.

      use type IC.int;

      function Internal_Get
        (Apis : in out Rtmidi_Api_Array; Apis_Size : IC.unsigned)
         return IC.int with
        Import        => True, Convention => C,
        External_Name => "rtmidi_get_compiled_api";

      function Internal_Get_Num
        (Apis : ICS.chars_ptr; Apis_Size : IC.unsigned) return IC.int with
        Import        => True, Convention => C,
        External_Name => "rtmidi_get_compiled_api";

      Size : IC.unsigned := 0;
      Ret  : IC.int      := 0;

   begin
      Ret := Internal_Get_Num (ICS.Null_Ptr, 0);

      if Ret <= 0 then
         declare
            Result : Rtmidi_Api_Array (1 .. 0);
         begin
            return Result;
         end;
      else
         Size := IC.unsigned (Ret);
      end if;

      declare
         Result : Rtmidi_Api_Array (1 .. Integer (Size));
      begin
         Ret := Internal_Get (Result, Size);
         if Ret <= 0 then
            declare
               Result : Rtmidi_Api_Array (1 .. 0);
            begin
               return Result;
            end;
         else
            return Result;
         end if;
      end;

   end Get_Compiled_apis;

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

      for I in 1 .. Positive (Length) loop
         --  Msg index starts at 0 and not 1, so 'i - 1'
         Result (I) :=
           Byte (Character'Pos (IC.To_Ada (Msg (IC.size_t (I - 1)))));
      end loop;

      return Result;

   end To_Message;

   ----------------------------------------------------------------------------
   function To_Hex (Value : Natural; Pad : Boolean := True) return String is

      use Ada.Strings.Unbounded;

      Temp1 : Natural;
      Temp2 : Natural;

      Hexa   : constant String  := "0123456789ABCDEF";
      Result : Unbounded_String := Null_Unbounded_String;
   begin

      if Value = 0 then
         if Pad then
            return "00";
         else
            return "0";
         end if;
      end if;

      Temp2 := Value;
      while Temp2 > 0 loop
         Temp1  := Temp2 mod 16;
         Result := Hexa (Temp1 + 1) & Result;
         Temp2  := Temp2 / 16;
      end loop;

      if Pad and then (Length (Result) mod 2) /= 0 then
         Result := "0" & Result;
      end if;

      return To_String (Result);
   end To_Hex;

end Rtmidi;

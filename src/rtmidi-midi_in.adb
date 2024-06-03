private with Ada.Text_IO;
private with Ada.Unchecked_Conversion;

private with Interfaces.C;
private with Interfaces.C.Strings;

with System;

package body Rtmidi.Midi_In is

   package IC renames Interfaces.C;
   package ICS renames Interfaces.C.Strings;

   ----------------------------------------------------------------------------
   procedure Open_Port
     (Self : in out Midi_In; Number : Natural := 0;
      Name :        String := "RtMidi Input")
   is
   begin
      Open_Port (Self.Device, Number, Name);
   end Open_Port;

   ----------------------------------------------------------------------------
   procedure Open_Virtual_Port
     (Self : in out Midi_In; Name : String := "RtMidi Input")
   is
   begin
      Open_Virtual_Port (Self.Device, Name);
   end Open_Virtual_Port;

   ----------------------------------------------------------------------------
   procedure Close_Port (Self : in out Midi_In) is
   begin
      Close_Port (Self.Device);
   end Close_Port;

   ----------------------------------------------------------------------------
   function Get_Port_Count (Self : Midi_In) return Natural is
   begin
      return Get_Port_Count (Self.Device);
   end Get_Port_Count;

   ----------------------------------------------------------------------------
   function Get_Port_Name (Self : Midi_In; Number : Natural) return String is
   begin
      return Get_Port_Name (Self.Device, Number);
   end Get_Port_Name;

   ----------------------------------------------------------------------------
   procedure Create (Self : in out Midi_In) is

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
     (Self             : in out Midi_In; Api : Rtmidi_Api := Unspecified;
      Client_Name      :        String   := "RtMidi Input Client";
      Queue_Size_Limit :        Positive := 100)
   is

      function Internal
        (Api              : Rtmidi_Api; Client_Name : ICS.chars_ptr;
         Queue_Size_Limit : IC.unsigned) return RtMidiPtr with
        Import => True, Convention => C, External_Name => "rtmidi_in_create";

   begin
      if Self.Device /= null then
         Self.Free;
      end if;

      Self.Device :=
        Internal
          (Api, ICS.New_String (Client_Name), IC.unsigned (Queue_Size_Limit));
   end Create;

   ----------------------------------------------------------------------------
   procedure Free (Self : in out Midi_In) is

      procedure Internal (Device : RtMidiPtr) with
        Import => True, Convention => C, External_Name => "rtmidi_in_free";

   begin
      Internal (Self.Device);
      Self.Device := null;
   end Free;

   ----------------------------------------------------------------------------
   function Get_Current_Api (Self : Midi_In) return Rtmidi_Api is

      function Internal (Device : RtMidiPtr) return Rtmidi_Api with
        Import        => True, Convention => C,
        External_Name => "rtmidi_in_get_current_api";

   begin
      return Internal (Self.Device);
   end Get_Current_Api;

   ----------------------------------------------------------------------------
   procedure Ignore_Types
     (Self      : in out Midi_In; Midi_Sysex : Boolean := True;
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
   procedure Cancel_Callback (Self : in out Midi_In) is

      procedure Internal (Device : RtMidiPtr) with
        Import        => True, Convention => C,
        External_Name => "rtmidi_in_cancel_callback";

   begin
      Internal (Self.Device);
   end Cancel_Callback;

   ----------------------------------------------------------------------------
   package body Callback_Factory is

      procedure Set_Callback
        (Self      : in out Midi_In; Callback : Callback_Type;
         User_Data :        access User_Data_Type)
      is

         type Proxy is
           access procedure
             (Delta_Time : IC.double; Buffer : IC.char_array; Len : IC.size_t;
              User_Data  : access User_Data_Type) with
           Convention => C;

         procedure Internal
           (Device    : RtMidiPtr; Callback : Proxy;
            User_Data : System.Address) with
           Import        => True, Convention => C,
           External_Name => "rtmidi_in_set_callback";

         function Convert_User_Data is new Ada.Unchecked_Conversion
           (User_Data_Access, System.Address);

         procedure Wrapper
           (Delta_Time : IC.double; Buffer : IC.char_array; Len : IC.size_t;
            User_Data  : access User_Data_Type) with
           Convention => C;

         procedure Wrapper
           (Delta_Time : IC.double; Buffer : IC.char_array; Len : IC.size_t;
            User_Data  : access User_Data_Type)
         is
         begin
            Callback (Float (Delta_Time), To_Message (Buffer, Len), User_Data);
         end Wrapper;

      begin
         Internal
           (Device    => Self.Device, Callback => Wrapper'Access,
            User_Data => Convert_User_Data (User_Data_Access (User_Data)));
      end Set_Callback;
   end Callback_Factory;

   ----------------------------------------------------------------------------
   function Get_Message (Self : Midi_In; Delta_Time : out Float) return Message
   is

      use type Interfaces.C.size_t;
      use type Interfaces.C.double;

      function Internal
        (Device : RtMidiPtr; Message : out IC.char_array; size : out IC.size_t)
         return IC.double with
        Import        => True, Convention => C,
        External_Name => "rtmidi_in_get_message";

      Buf_Len : IC.size_t := 1_024;
      Buffer  : IC.char_array (0 .. Buf_Len - 1);
      Ret     : IC.double := 0.0;
      Empty   : Message (1 .. 0);

   begin
      Ret := Internal (Self.Device, Buffer, Buf_Len);

      if Ret <= 0.0 then
         Delta_Time := 0.0;
         return Empty;
      else
         Delta_Time := Float (Ret);
         --  Buf_Len has been updated to the real length
         return To_Message (Buffer, Buf_Len);
      end if;

   end Get_Message;

   ----------------------------------------------------------------------------
   function Get_Message (Self : Midi_In) return Message is
      Delta_Time : Float := 0.0;
   begin
      return Get_Message (Self, Delta_Time);
   end Get_Message;

   ----------------------------------------------------------------------------
   procedure Put_Message (Self : Midi_In) is

      Msg : constant String := To_String (Get_Message (Self));

   begin

      if Msg'First < Msg'Last then
         Ada.Text_IO.Put_Line (Msg);
      end if;

   end Put_Message;

   ----------------------------------------------------------------------------
   overriding procedure Finalize (Self : in out Midi_In) is
   begin
      Self.Free;
   end Finalize;

end Rtmidi.Midi_In;

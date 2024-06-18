package body Rtmidi.Midi_In.Generic_Callbacks is

   package IC renames Interfaces.C;

   procedure Do_Nothing
     (Delta_Time : Float; Msg : Message;
      User_Data  : access User_Data_Type) is null;

   type Infos_Record is record
      Real_User_Data : access User_Data_Type;
      Real_Callback  : Callback_Type := Do_Nothing'Access;
   end record;

   type Infos_Record_Access is access all Infos_Record;

   Infos : aliased Infos_Record;

   procedure Set_Callback
     (Self      : in out Midi_In; Callback : Callback_Type;
      User_Data :        access User_Data_Type)
   is

      procedure Internal
        (Device : RtMidiPtr; Callback : System.Address;
         Infos  : Infos_Record_Access) with
        Import        => True, Convention => C,
        External_Name => "rtmidi_in_set_callback";

      procedure Wrapper
        (Delta_Time : IC.double; Buffer : IC.char_array; Len : IC.size_t;
         Infos      : Infos_Record_Access) with
        Convention => C;

      procedure Wrapper
        (Delta_Time : IC.double; Buffer : IC.char_array; Len : IC.size_t;
         Infos      : Infos_Record_Access)
      is
      begin
         Infos.Real_Callback
           (Float (Delta_Time), To_Message (Buffer, Len),
            Infos.Real_User_Data);
      end Wrapper;

   begin
      Infos.Real_Callback  := Callback;
      Infos.Real_User_Data := User_Data;
      Internal
        (Device => Self.Device, Callback => Wrapper'Address,
         Infos  => Infos'Access);
      Self.Callback_Is_Set := True;
   end Set_Callback;

end Rtmidi.Midi_In.Generic_Callbacks;

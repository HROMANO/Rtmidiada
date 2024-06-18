package Rtmidi.Midi_In.Simple_Callback is

   type Callback_Type is access procedure (Delta_Time : Float; Msg : Message);

   procedure Set_Callback
     (Self : in out Midi_In; Callback : Callback_Type) with
     Pre => Self.Callback_Already_Set = False,
   Post => Self.Callback_Already_Set = True;

end Rtmidi.Midi_In.Simple_Callback;

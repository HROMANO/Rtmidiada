generic

   type User_Data_Type is private;

package Rtmidi.Midi_In.Generic_Callbacks
is

   type Callback_Type is
     not null access procedure
       (Delta_Time : Float; Msg : Message; User_Data : access User_Data_Type);

   procedure Set_Callback
     (Self      : in out Midi_In'Class;
      Callback  : Callback_Type;
      User_Data : access User_Data_Type)
   with Pre => Self.Callback_Already_Set = False;

end Rtmidi.Midi_In.Generic_Callbacks;

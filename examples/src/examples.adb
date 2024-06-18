pragma Ada_2022;

with Ada.Text_IO; use Ada.Text_IO;

with Rtmidi;
with Rtmidi.Midi_In;
with Rtmidi.Midi_In.Generic_Callbacks;
with Rtmidi.Midi_In.Simple_Callback;
with Rtmidi.Midi_Out;

with Callback;

procedure Examples is

   apis     : constant Rtmidi.Rtmidi_Api_Array := Rtmidi.Get_Compiled_Apis;
   midi_in  : array (apis'Range) of Rtmidi.Midi_In.Midi_In;
   midi_out : array (apis'Range) of Rtmidi.Midi_Out.Midi_Out;
   nb_ports : array (apis'Range) of Natural    := [others => 0];

   type Essai is new String (1 .. 10);

   package cb_int is new Rtmidi.Midi_In.Generic_Callbacks
     (User_Data_Type => Integer);

   package cb_str is new Rtmidi.Midi_In.Generic_Callbacks
     (User_Data_Type => Essai);

   procedure cb0
     (Delta_Time : Float; Msg : Rtmidi.Message; User_Data : access Integer)
   is
   begin
      User_Data.all := User_Data.all + 1;
      Put ("Deltatime = " & Delta_Time'Image & " - Message read = ");
      Put (Rtmidi.To_String (Msg));
      Put_Line (" - User data = " & Integer'Image (User_Data.all));
   end cb0;

   procedure cb1
     (Delta_Time : Float; Msg : Rtmidi.Message; User_Data : access Essai)
   is
   begin
      User_Data.all (1) := User_Data.all (2);
      Put ("Delta = " & Delta_Time'Image & " - Message lu = ");
      Put (Rtmidi.To_String (Msg));
      Put_Line (" - User data = " & String (User_Data.all));
   end cb1;

   u       : aliased Integer := 0;
   v       : aliased Essai   := "abc       ";
   message : String          := "00";
   ret     : Integer         := 0;

begin

   New_Line;
   Put_Line ("--------");
   New_Line;
   Put_Line ("RtMidi version: " & Rtmidi.Get_Version);
   New_Line;

   for i in apis'Range loop

      Put_Line ("--------");
      New_Line;
      Put_Line ("API name: " & Rtmidi.Api_Name (apis (i)));
      Put_Line ("API display name: " & Rtmidi.Api_Display_Name (apis (i)));
      New_Line;

      Rtmidi.Midi_In.Create (midi_in (i), apis (i));
      nb_ports (i) := midi_in (i).Get_Port_Count;
      Put_Line ("Input ports number:" & Natural'Image (nb_ports (i)));
      for j in 0 .. nb_ports (i) - 1 loop
         Put_Line
           ("Port name" & Natural'Image (j) & ": " &
            midi_in (i).Get_Port_Name (j));
      end loop;

      midi_in (i).Open_Port (0, "First");
      Put_Line
        ("Current API: " &
         Rtmidi.Api_Display_Name (midi_in (i).Get_Current_Api));
      Put_Line ("Valid: " & midi_in (i).Valid'Image);
      Put_Line ("Error message: " & midi_in (i).Error_Message);
      midi_in (i).Close_Port;
      New_Line;

      Rtmidi.Midi_Out.Create (midi_out (i), apis (i));
      nb_ports (i) := midi_out (i).Get_Port_Count;
      Put_Line ("Output ports number:" & Natural'Image (nb_ports (i)));
      for j in 0 .. nb_ports (i) - 1 loop
         Put_Line
           ("Port name" & Natural'Image (j) & ": " &
            midi_out (i).Get_Port_Name (j));
      end loop;

      midi_out (i).Open_Port (0, "First");
      Put_Line
        ("Current API: " &
         Rtmidi.Api_Display_Name (midi_out (i).Get_Current_Api));
      Put_Line ("Valid: " & midi_out (i).Valid'Image);
      Put_Line ("Error message: " & midi_out (i).Error_Message);
      midi_out (i).Close_Port;

      New_Line;

   end loop;

   Put_Line ("--------");
   New_Line;
   Rtmidi.Midi_In.Create (midi_in (1), Rtmidi.Linux_Alsa);
   midi_in (1).Open_Port (1, "First");
   midi_in (1).Ignore_Types (False, False, False);
   Put_Line ("Waiting for messages...");

   --  Simple callback with no user data
   --  Must be in a separate package (library level)
   New_Line;
   Put_Line ("No user data");
   Rtmidi.Midi_In.Simple_Callback.Set_Callback
     (midi_in (1), Callback.cb'Access);
   delay 5.0;
   midi_in (1).Cancel_Callback;

   --  Callback with Integer user data
   New_Line;
   Put_Line ("User data is Integer");
   cb_int.Set_Callback (midi_in (1), cb0'Access, u'Access);
   delay 5.0;
   midi_in (1).Cancel_Callback;

   --  Callback with String (1..10) user data
   New_Line;
   Put_Line ("User data is String (1..10)");
   cb_str.Set_Callback (midi_in (1), cb1'Access, v'Access);
   delay 5.0;
   midi_in (1).Cancel_Callback;

   --  Midi output
   New_Line;
   Put_Line ("Some MIDI Out");
   Rtmidi.Midi_Out.Create (midi_out (1), Rtmidi.Linux_Alsa);
   midi_out (1).Open_Port (1, "First");
   message (1) := Character'Val (16#C0#);
   message (2) := Character'Val (16#11#);
   ret         := midi_out (1).Send_Message (message);
   pragma Unused (ret);
   delay 1.0;

   --  Midi in without callback
   --  Put_Line ("Switching to non callback.");
   --  loop
   --     midi_in (1).Put_Message;
   --  end loop;

end Examples;

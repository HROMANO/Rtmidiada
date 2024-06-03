pragma Ada_2022;

with Ada.Text_IO; use Ada.Text_IO;

with Rtmidi;
with Rtmidi.MidiIn;
with Rtmidi.MidiOut;

procedure Examples is

   apis     : constant Rtmidi.RtMidiApi_Array := Rtmidi.Get_Compiled_Apis;
   midi_in  : array (apis'Range) of Rtmidi.MidiIn.MidiIn;
   midi_out : array (apis'Range) of Rtmidi.MidiOut.MidiOut;
   nb_ports : array (apis'Range) of Natural   := [others => 0];

   type essai is new String (1 .. 10);

   package cb_int is new Rtmidi.MidiIn.Callback_Factory
     (User_Data_Type => Integer);
   package cb_str is new Rtmidi.MidiIn.Callback_Factory
     (User_Data_Type => essai);

   procedure cb0
     (deltatime : Float; msg : Rtmidi.Message; user_data : access Integer)
   is
   begin
      user_data.all := user_data.all + 1;
      Put ("Deltatime = " & deltatime'Image & " - Message read = ");
      Put (Rtmidi.To_String (msg));
      Put_Line (" - User data = " & Integer'Image (user_data.all));
   end cb0;

   procedure cb1
     (deltatime : Float; msg : Rtmidi.Message; user_data : access essai)
   is
   begin
      user_data.all (1) := user_data.all (2);
      Put ("Delta = " & deltatime'Image & " - Message lu = ");
      Put (Rtmidi.To_String (msg));
      Put_Line (" - User data = " & String (user_data.all));
   end cb1;

   u       : aliased Integer := 0;
   v       : aliased essai   := "abc       ";
   message : String          := "00";
   ret     : Integer         := 0;

begin

   for i in apis'Range loop

      Put_Line ("API name: " & Rtmidi.Api_Name (apis (i)));
      Put_Line ("API display name: " & Rtmidi.Api_Display_Name (apis (i)));
      Put_Line ("-----------");

      Rtmidi.MidiIn.Create (midi_in (i), apis (i));
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
      midi_in (i).Close_Port;
      Put_Line ("-----------");

      Rtmidi.MidiOut.Create (midi_out (i), apis (i));
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
      midi_out (i).Close_Port;
      Put_Line ("-----------");

      New_Line;

   end loop;

   Rtmidi.MidiIn.Create (midi_in (1), Rtmidi.RTMIDI_API_LINUX_ALSA);
   midi_in (1).Open_Port (1, "First");
   midi_in (1).Ignore_Types (False, False, False);
   Put_Line ("Waiting for messages...");
   Put_Line ("User data is Integer");
   cb_int.Set_Callback (midi_in (1), cb0'Access, u'Access);
   delay 5.0;

   midi_in (1).Cancel_Callback;
   Put_Line ("User data is String(1..10)");
   cb_str.Set_Callback (midi_in (1), cb1'Access, v'Access);
   delay 5.0;

   Put_Line ("Some MIDI Out");
   Rtmidi.MidiOut.Create (midi_out (1), Rtmidi.RTMIDI_API_LINUX_ALSA);
   midi_out (1).Open_Port (1, "First");
   message (1) := Character'Val (16#C0#);
   message (2) := Character'Val (16#11#);
   ret         := midi_out (1).Send_Message (message);
   pragma Unused (ret);
   delay 1.0;

   midi_in (1).Cancel_Callback;
   Put_Line ("Switching to non callback.");
   loop
      midi_in (1).Put_Message;
   end loop;

end Examples;

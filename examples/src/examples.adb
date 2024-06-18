pragma Ada_2022;

with Ada.Text_IO; use Ada.Text_IO;

with Rtmidi;
with Rtmidi.Midi_In;
with Rtmidi.Midi_In.Generic_Callbacks;
with Rtmidi.Midi_In.Simple_Callback;
with Rtmidi.Midi_Out;

with Callback;

procedure Examples is

   Apis : constant Rtmidi.Rtmidi_Api_Array := Rtmidi.Get_Compiled_Apis;
   Midi_In_Array   : array (Apis'Range) of Rtmidi.Midi_In.Midi_In;
   Midi_Out_Array  : array (Apis'Range) of Rtmidi.Midi_Out.Midi_Out;
   Number_Of_Ports : array (Apis'Range) of Natural    := [others => 0];

   type String_10 is new String (1 .. 10);

   package Callback_Integer_Package is new Rtmidi.Midi_In.Generic_Callbacks
     (User_Data_Type => Integer);

   package Callback_String_10_Package is new Rtmidi.Midi_In.Generic_Callbacks
     (User_Data_Type => String_10);

   procedure Callback_Integer
     (Delta_Time : Float; Msg : Rtmidi.Message; User_Data : access Integer)
   is
   begin
      User_Data.all := User_Data.all + 1;

      Put ("Deltatime = " & Delta_Time'Image & " - Message read = ");
      Put (Rtmidi.To_String (Msg));
      Put_Line (" - User data = " & Integer'Image (User_Data.all));
   end Callback_Integer;

   procedure Callback_String_10
     (Delta_Time : Float; Msg : Rtmidi.Message; User_Data : access String_10)
   is
   begin
      User_Data.all (1) := User_Data.all (2);

      Put ("Delta = " & Delta_Time'Image & " - Message lu = ");
      Put (Rtmidi.To_String (Msg));
      Put_Line (" - User data = " & String (User_Data.all));
   end Callback_String_10;

   The_Integer   : aliased Integer   := 0;
   The_String_10 : aliased String_10 := "abc       ";
   Message       : String            := "00";
   Result        : Integer           := 0;

begin

   New_Line;
   Put_Line ("--------");
   New_Line;
   Put_Line ("RtMidi version: " & Rtmidi.Get_Version);
   New_Line;

   for Api in Apis'Range loop

      Put_Line ("--------");
      New_Line;
      Put_Line ("API name: " & Rtmidi.Api_Name (Apis (Api)));
      Put_Line ("API display name: " & Rtmidi.Api_Display_Name (Apis (Api)));
      New_Line;

      Rtmidi.Midi_In.Create (Midi_In_Array (Api), Apis (Api));
      Number_Of_Ports (Api) := Midi_In_Array (Api).Get_Port_Count;
      Put_Line ("Input ports number:" & Natural'Image (Number_Of_Ports (Api)));

      for Port_In in 0 .. Number_Of_Ports (Api) - 1 loop
         Put_Line
           ("Port name" & Natural'Image (Port_In) & ": " &
            Midi_In_Array (Api).Get_Port_Name (Port_In));
      end loop;

      Midi_In_Array (Api).Open_Port (0, "First");
      Put_Line
        ("Current API: " &
         Rtmidi.Api_Display_Name (Midi_In_Array (Api).Get_Current_Api));

      if Midi_In_Array (Api).Success then
         Put_Line ("Success");
      else
         Put_Line ("Error message: " & Midi_In_Array (Api).Error_Message);
      end if;

      Midi_In_Array (Api).Close_Port;
      New_Line;

      Rtmidi.Midi_Out.Create (Midi_Out_Array (Api), Apis (Api));
      Number_Of_Ports (Api) := Midi_Out_Array (Api).Get_Port_Count;
      Put_Line
        ("Output ports number:" & Natural'Image (Number_Of_Ports (Api)));

      for Port_Out in 0 .. Number_Of_Ports (Api) - 1 loop
         Put_Line
           ("Port name" & Natural'Image (Port_Out) & ": " &
            Midi_Out_Array (Api).Get_Port_Name (Port_Out));
      end loop;

      Midi_Out_Array (Api).Open_Port (0, "First");
      Put_Line
        ("Current API: " &
         Rtmidi.Api_Display_Name (Midi_Out_Array (Api).Get_Current_Api));

      if Midi_Out_Array (Api).Success then
         Put_Line ("Success");
      else
         Put_Line ("Error message: " & Midi_Out_Array (Api).Error_Message);
      end if;
      Midi_Out_Array (Api).Close_Port;

      New_Line;

   end loop;

   Put_Line ("--------");
   New_Line;

   Rtmidi.Midi_In.Create (Midi_In_Array (1), Rtmidi.Linux_Alsa);
   Midi_In_Array (1).Open_Port (1, "First");
   Midi_In_Array (1).Ignore_Types (False, False, False);

   Put_Line ("Waiting for messages...");

   --  Simple callback with no user data
   --  Must be in a separate package (library level)
   New_Line;
   Put_Line ("No user data");
   Rtmidi.Midi_In.Simple_Callback.Set_Callback
     (Midi_In_Array (1), Callback.cb'Access);
   delay 5.0;
   Midi_In_Array (1).Cancel_Callback;

   --  Callback with Integer user data
   New_Line;
   Put_Line ("User data is Integer");
   Callback_Integer_Package.Set_Callback
     (Midi_In_Array (1), Callback_Integer'Access, The_Integer'Access);
   delay 5.0;
   Midi_In_Array (1).Cancel_Callback;

   --  Callback with String (1 .. 10) user data
   New_Line;
   Put_Line ("User data is String (1 .. 10)");
   Callback_String_10_Package.Set_Callback
     (Midi_In_Array (1), Callback_String_10'Access, The_String_10'Access);
   delay 5.0;
   Midi_In_Array (1).Cancel_Callback;

   --  Midi output
   New_Line;
   Put_Line ("Some MIDI Out");
   Rtmidi.Midi_Out.Create (Midi_Out_Array (1), Rtmidi.Linux_Alsa);
   Midi_Out_Array (1).Open_Port (1, "First");
   Message (1) := Character'Val (16#C0#);
   Message (2) := Character'Val (16#11#);
   Result      := Midi_Out_Array (1).Send_Message (Message);
   pragma Unused (Result);
   delay 1.0;

   --  Midi in without callback
   --  Put_Line ("Switching to non callback.");
   --  loop
   --     Midi_In_Array (1).Put_Message;
   --  end loop;

end Examples;

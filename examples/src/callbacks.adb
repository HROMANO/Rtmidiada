with Ada.Text_IO;

package body Callbacks is

   use Ada.Text_IO;

   procedure Example_Callback (Delta_Time : Float; Msg : Rtmidi.Message) is
   begin
      Put ("Deltatime = " & Delta_Time'Image & " - Message read = ");
      Put_Line (Rtmidi.To_String (Msg));
   end Example_Callback;

end Callbacks;

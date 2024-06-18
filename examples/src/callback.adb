with Ada.Text_IO;

package body Callback is

   use Ada.Text_IO;

   procedure cb (Delta_Time : Float; Msg : Rtmidi.Message) is
   begin
      Put ("Deltatime = " & Delta_Time'Image & " - Message read = ");
      Put_Line (Rtmidi.To_String (Msg));
   end cb;

end Callback;

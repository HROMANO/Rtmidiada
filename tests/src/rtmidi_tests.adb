with Ada.Text_IO; use Ada.Text_IO;

with RtMidi;
with RtMidi.MidiIn;
with RtMidi.MidiOut;
with Utils;

procedure Rtmidi_Tests is

    apis     : RtMidi.RtMidiApi_Array := RtMidi.get_compiled_apis;
    midi_in  : Array(apis'Range) of RtMidi.MidiIn.MidiIn;
    midi_out : Array(apis'Range) of RtMidi.MidiOut.MidiOut;
    nb_ports : Array(apis'Range) of Natural := (others => 0);

	type essai is new String(1 .. 10);

	package cb_int is new RtMidi.MidiIn.CallBack(User_Data_Type => Integer);
	package cb_str is new RtMidi.MidiIn.CallBack(User_Data_Type => essai);

	procedure cb0 (deltatime : Float;
			       msg       : RtMidi.Message;
				   user_data : access Integer) is
    begin
	  	user_data.all := user_data.all + 1;
    	Put("Deltatime = " & deltatime'Image & " - Message read = ");
        Put(RtMidi.to_string(msg));
        Put_Line(" - User data = " & Integer'Image(user_data.all));
    end cb0;

  	procedure cb1 (deltatime : Float;
			       msg       : RtMidi.Message;
				   user_data : access essai) is
    begin
	  	user_data.all(1) := user_data.all(2);
    	Put("Delta = " & deltatime'Image & " - Message lu = ");
    	Put(RtMidi.to_string(msg));
        Put_Line(" - User data = " & String(user_data.all));
    end cb1;

    u       : aliased Integer := 0;
    v       : aliased essai := "abc       ";
 	message : String := "00";
 	ret     : integer := 0;

begin

    for i in apis'Range loop

        Put_Line("API name: " & RtMidi.api_name(apis(i)));
        Put_Line("API display name: " & RtMidi.api_display_name(apis(i)));
        Put_Line("-----------");

        RtMidi.MidiIn.create(midi_in(i), apis(i));
        nb_ports(i) := midi_in(i).port_count;
        Put_Line("Input ports number:" & Natural'Image(nb_ports(i)));
        for j in 0 .. nb_ports(i) - 1 loop
            Put_Line("Port name" & Natural'Image(j) & ": " & midi_in(i).port_name(j));
        end loop;

        midi_in(i).open_port(0, "First");
        Put_Line("Current API: " & RtMidi.api_display_name(midi_in(i).get_current_api));
        midi_in(i).close_port;
        -- midi_in(i).free;
        Put_Line("-----------");

        RtMidi.MidiOut.create(midi_out(i), apis(i));
        nb_ports(i) := midi_out(i).port_count;
        Put_Line("Output ports number:" & Natural'Image(nb_ports(i)));
        for j in 0 .. nb_ports(i) - 1 loop
            Put_Line("Port name" & Natural'Image(j) & ": " & midi_out(i).port_name(j));
        end loop;

        midi_out(i).open_port(0, "First");
        Put_Line("Current API: " & RtMidi.api_display_name(midi_out(i).get_current_api));
        midi_out(i).close_port;
        Put_Line("-----------");

        New_Line;

    end loop;

    RtMidi.MidiIn.create(midi_in(1), RtMidi.RTMIDI_API_LINUX_ALSA);
    midi_in(1).open_port(1, "First");
    midi_in(1).ignore_types(False, False, False);
    Put_Line("Waiting for messages…");
    cb_int.set_callback(midi_in(1), cb0'Access, u'Access);
    -- cb_str.set_callback(midi_in(1), cb1'Access, v'Access);

    RtMidi.MidiOut.create(midi_out(1), RtMidi.RTMIDI_API_LINUX_ALSA);
    midi_out(1).open_port(1, "First");
    message(1) := Character'Val(16#C0#);
    message(2) := Character'Val(16#11#);
    ret := midi_out(1).send_message(message);

    delay 10.0;
    midi_in(1).cancel_callback;
    Put_Line("Switching to non callback.");
    loop
        midi_in(1).put_message;
    end loop;

end Rtmidi_Tests;

with Interfaces.C;
with Interfaces.C.Strings;

package body RtMidi is

    ----------------------------------------------------------------------------
    function api_name (api : RtMidiApi) return String is
    
    	use Interfaces.C.Strings;
    	
        function rtmidi_api_name (api : RtMidiApi) 
            return chars_ptr
        with Import        => True,
             Convention    => C,
             External_Name => "rtmidi_api_name";
             
        name : String := Value(rtmidi_api_name(api));
    begin
        return name;
    end api_name;
    
    ----------------------------------------------------------------------------
    function api_display_name (api : RtMidiApi) return String is
    
    	use Interfaces.C.Strings;
    	
        function rtmidi_api_display_name (api : RtMidiApi) 
            return chars_ptr
        with Import       => True,
            Convention    => C,
            External_Name => "rtmidi_api_display_name";
            
        name : String := Value(rtmidi_api_display_name (api));
    begin
        return name;
    end api_display_name;
    
    ----------------------------------------------------------------------------
    function compiled_api_by_name(name : String) return RtMidiApi is
    
    	use Interfaces.C.Strings;
    	
        function rtmidi_compiled_api_by_name (name : chars_ptr) 
            return RtMidiApi
        with Import       => True,
            Convention    => C,
            External_Name => "rtmidi_compiled_api_by_name";
    
        api : RtMidiAPi 
            := rtmidi_compiled_api_by_name (New_String(name));
    begin
        return api;
    end compiled_api_by_name;
    
    ----------------------------------------------------------------------------
    --procedure error (error_type : RtMidiErrorType; 
    --                 msg : String) is
        
            --procedure rtmidi_error (c_type      : RtMidiErrorType; 
            --                        errorString : Strings.chars_ptr)
            --with Import       => True,
            --    Convention    => C,
            --    External_Name => "rtmidi_error";
    
    --begin
    --    rtmidi_error(error_type, 
    --                 Strings.New_String(msg));
    --end error;
    
    ----------------------------------------------------------------------------
    procedure open_port (device : in out RtMidiPtr; 
                         number : Natural; 
                         name   : String) is

		use Interfaces.C;
		use Interfaces.C.Strings;
		                         
        procedure rtmidi_open_port
            (device    : RtMidiPtr;
            portNumber : unsigned; -- if 0, the first available.
            portName   : chars_ptr)
        with Import       => True,
            Convention    => C,
            External_Name => "rtmidi_open_port";
            
    begin
        rtmidi_open_port(device, unsigned(number), 
                         New_String(name));
    end open_port;
    
    ----------------------------------------------------------------------------
    procedure open_virtual_port (device : in out RtMidiPtr; 
                                 name   : String) is
    
    	use Interfaces.C.Strings;
    	
        procedure rtmidi_open_virtual_port 
            (device   : RtMidiPtr; 
             portName : chars_ptr)
        with Import       => True,
            Convention    => C,
            External_Name => "rtmidi_open_virtual_port";
    
    begin
        rtmidi_open_virtual_port(device, 
                                 New_String(name));
    end open_virtual_port;
    
    ----------------------------------------------------------------------------
    procedure close_port (device : in out RtMidiPtr) is
    
        procedure rtmidi_close_port (device : RtMidiPtr)
        with Import       => True,
            Convention    => C,
            External_Name => "rtmidi_close_port";
    
    begin
        rtmidi_close_port(device);
    end close_port;
    
    ----------------------------------------------------------------------------
    function port_count (device : RtMidiPtr) return Natural is
    
    	use Interfaces.C;
    	
        function rtmidi_get_port_count (device : RtMidiPtr) 
            return unsigned
        with Import       => True,
            Convention    => C,
            External_Name => "rtmidi_get_port_count";
    
    begin
        return Natural(rtmidi_get_port_count(device));
    end port_count;
    
    ----------------------------------------------------------------------------
    function get_port_name (device : RtMidiPtr;
                            number : Natural := 0)
		return String is

		use Interfaces.C;
		use Interfaces.C.Strings;
		
        function rtmidi_get_port_name
            (device     : RtMidiPtr;
             portNumber : unsigned;
             bufOut     : chars_ptr;
             bufLen     : out int) 
            return int
        with Import       => True,
            Convention    => C,
            External_Name => "rtmidi_get_port_name";

        result : int := 0;
        buflen : int := 0;
        name : chars_ptr;
    begin
        result := rtmidi_get_port_name(device, 
                                       unsigned(number), 
                                       Null_Ptr, 
                                       buflen);

        if result < 0 then
            return "";
        end if;

        name := New_String( (1 .. Integer(buflen) => ' ') );
        result := rtmidi_get_port_name(device, 
                                       unsigned(number), 
                                       name, 
                                       buflen);

        if result <= 0 then
            return "";
        end if;

        return Value(name, size_t(result));

    end get_port_name;
    
    ----------------------------------------------------------------------------
    function get_compiled_apis return RtMidiApi_Array is
    
    	-- TODO: quite ugly.
    	
    	use Interfaces.C;
    	use Interfaces.C.Strings;
		 
		function rtmidi_get_compiled_api (apis      : in out RtMidiApi_Array; 
		                                  apis_size : unsigned) 
			return int
		with Import        => True,
		     Convention    => C,
		     External_Name => "rtmidi_get_compiled_api";
		 
		 function get_num_compiled_api (apis      : chars_ptr; 
		                                apis_size : unsigned) 
			return int
		with Import        => True,
			 Convention    => C,
			 External_Name => "rtmidi_get_compiled_api";
		         
	     size : unsigned := 0;
	     ret  : int := 0;
	     
    begin
    	ret := get_num_compiled_api(Null_Ptr, 0);
    	
    	if ret <= 0 then
    		declare
    			result : RtMidiApi_Array (1 .. 0);
			begin
    			return result;
			end;
		else
			size := unsigned(ret);
		end if;
		
		declare
			result : RtMidiApi_Array (1 .. Integer(size));
		begin
			ret := rtmidi_get_compiled_api(result, size);
			if ret <= 0 then
				declare
					result : RtMidiApi_Array (1 .. 0);
				begin
					return result;
				end;
			else
				return result;
			end if;
		end;

	end get_compiled_apis;                       

end RtMidi;

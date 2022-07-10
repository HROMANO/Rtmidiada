with Ada.Strings.Unbounded;

package body Utils is

    function to_hex (value : Natural; pad : Boolean := True) return String is

    	use Ada.Strings.Unbounded;

		temp1 : Natural;
		temp2 : Natural;

		hexa : String := "0123456789ABCDEF";
		result : Unbounded_String := Null_Unbounded_String;
    begin

		if value = 0 then
			if pad = True then
				return "00";
			else
				return "0";
			end if;
		end if;

		temp2 := value;
		while temp2 > 0 loop
			temp1 := temp2 mod 16;
			result := hexa(temp1 + 1) & result;
			temp2 := temp2 / 16;
		end loop;

		if pad = True and (Length(result) mod 2) /= 0 then
			result := "0" & result;
		end if;

		return To_String(result);
	end to_hex;

end Utils;

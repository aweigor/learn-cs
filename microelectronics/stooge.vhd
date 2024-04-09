library ieee;
use ieee.std_logic_1164.all;

package types is
	type massType is array (9 downto 0) of std_logic_vector (7 downto 0); 
	function stooge_sort(arr: massType; left: integer; right: integer) return massType;
end types;
package body types is

	function stooge_sort(arr: massType; left: integer; right: integer) return massType is
		variable tmp: std_logic_vector (7 downto 0);
		variable k: integer;
		variable result: massType := arr;
	begin
		if (arr(left) > arr(right)) then
			tmp := arr(left);
			result(left) := result(right);
			result(right) := tmp;
		end if;
	
		if ((left + 1) >= right) then
			return result;	
		end if;
	
		k := ((right - left +1) / 3);
		result := stooge_sort(result, left, right - k);
		result := stooge_sort(result, left + k, right);
		result := stooge_sort(result, left, right - k);
		
		return result;
	end stooge_sort;
end types;

library ieee;
use ieee.std_logic_1164.all;
use work.types.all;
entity stooge is
	port (
		clk: in std_logic;
		reset: in std_logic;
		working: out std_logic;
		data_in: in massType;
		data_out: out massType
	);
end stooge;
architecture behav of stooge is
	signal run:std_logic := '0';
begin
	process (clk)
		variable i, e: integer := 0;
		variable arr: massType;
	begin
		arr := stooge_sort(data_in, 0, 9);
		data_out <= arr;
	end process;
end behav;
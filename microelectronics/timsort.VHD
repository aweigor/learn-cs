library ieee;
use ieee.std_logic_1164.all;

package types is
	type massType is array (9 downto 0) of std_logic_vector (7 downto 0);
	function toInteger(a: std_logic_vector) return integer; 
	function min(one: integer; two: integer) return integer;
	function insertionSort(arr: massType; left: integer; right: integer) return massType;
	function merge(arr: massType; l: integer; m: integer; r: integer) return massType;
end types;
package body types is

	function toInteger(a: std_logic_vector) return integer is
		alias av: std_logic_vector (1 to a'length) is a;
		variable val: integer := 0;
		variable b: integer := 1;
	begin
		for i in a'length downto 1 loop
			if (av(i) = '1') then
				val := val + b;
			end if;
			b := b * 2;
		end loop;
		return val;
	end toInteger;

	function min(one: integer; two: integer) return integer is
		variable result: integer := one;
	begin
		if (two < one) then
			result := two;
		end if;
		return result;
	end min;
	
	function insertionSort(arr: massType; left: integer; right: integer) return massType is
		variable tmp: std_logic_vector (7 downto 0);
		variable i: integer;
		variable j: integer;
		variable result: massType := arr;
	begin
		i := left + 1;
		while (i <= right) loop
			tmp := result(i);
			j := i - 1;
			while (j >= left and toInteger(result(j)) > toInteger(tmp)) loop
				result(j + 1) := result(j);
				j := j - 1;
			end loop;
			result(j + 1) := tmp;
			i := i + 1;
		end loop;
		return result;
	end insertionSort;
	
	function merge(arr: massType; l: integer; m: integer; r: integer) return massType is
		variable len1: integer;
		variable len2: integer;
		variable left: massType;
		variable right: massType;
		variable result: massType := arr;
		variable i, j, k: integer;
	begin
		len1 := m - l + 1;
		len2 := r - m;
	
		for i in 0 to len1 - 1 loop
			left(i) := arr(l + i);
		end loop;
		for i in 0 to len2 - 1 loop
			right(i) := arr(m + i + 1);
		end loop;
		
		i := 0;
		j := 0;
		k := l;
		
		while (i < len1 and j < len2) loop
			if (toInteger(left(i)) <= toInteger(right(j))) then
				result(k) := left(i);
				i := i + 1;
			else
				result(k) := right(j);
				j := j + 1;
			end if;
			k := k + 1;
		end loop;
		
		while (j < len2) loop
			result(k) := right(j);
			k := k + 1;
			j := j + 1;
		end loop;
		
		while (i < len1) loop
			result(k) := left(i);
			k := k + 1;
			i := i + 1;
		end loop;
		
		
		
		return result;
	end merge;	
end types;

library ieee;
use ieee.std_logic_1164.all;
use work.types.all;
entity timsort is
	port (
		clk: in std_logic;
		reset: in std_logic;
		working: out std_logic;
		data_in: in massType;
		data_out: out massType
	);
end timsort;
architecture behav of timsort is
	signal run:std_logic := '0';
	signal sorted: massType;
begin
	process (clk)
		variable i: integer := 0;
		variable n: integer := 10;
		variable minrun: integer := 4;
		variable arr: massType;
		variable size, left, mid, right: integer;
	begin
		size := minrun;
		if rising_edge(clk) then
			-- 1. Insertion sort
			if (i < n) then
				sorted <= insertionSort(data_in, i, min((i + minrun - 1), (n - 1)));
				i := i + minrun;
			else
			-- 2. Merge
				while (size < n) loop
					left := 0;
					while (left < n) loop
						mid := left + size - 1;
						right := min((left + (2 * size) - 1), (n - 1));
						if (mid < right) then
							sorted <= merge(sorted, left, mid, right);
						end if;
						left := left + (2 * size);
					end loop;
					size := 2 * size;
				end loop;
			-- if (size < n) then
			--	if (size < n) then
			--		if (left < n) then 
			--			mid := left + size - 1;
			--			right := min((left + (2 * size) - 1), (n - 1));
			--			if (mid < right) then
			--				sorted <= merge(sorted, left, mid, right);
			--			end if;
			--			left := left + (2 * size);
			--		else 
			--			-- loop end
			--			left := 0;
			--			--size := 2 * size;
			--		end if;
			--	end if;
			end if;
		end if;
	end process;
	data_out <= sorted;
end behav;

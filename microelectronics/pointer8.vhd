library IEEE;
use IEEE.std_logic_1164.all;

entity pointer8 is
port (
	EN: in std_logic;
	R: in std_logic_vector (7 downto 0);
	Q: out std_logic_vector (7 downto 0)
);
end pointer8;

architecture behavior of pointer8 is
begin

	Q <= "00000000" when EN = '0' else
	 	 "10000000" when R(7) = '1' else
	 	 "01000000" when R(6) = '1' else
		 "00100000" when R(5) = '1' else
		 "00010000" when R(4) = '1' else
		 "00001000" when R(3) = '1' else
		 "00000100" when R(2) = '1' else
		 "00000010" when R(1) = '1' else
		 "00000001" when R(0) = '1' else
		 "00000000";

end behavior;
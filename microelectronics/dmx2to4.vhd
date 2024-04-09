library IEEE;
use IEEE.STD_LOGIC_1164.all;

entity dmx2to4 is
	port 
	(
		X: in  		std_logic_vector (0 to 1);
		EN: in 		std_logic;
		F: out 		std_logic_vector (0 to 3)
	);
end dmx2to4;

architecture behavior of dmx2to4 is
begin
	F <= "0000" when EN = '0' else
		 "1000" when X = "00" else
		 "0100" when X = "01" else
		 "0010" when X = "10" else
		 "0001" when X = "11" else
		 "0000";
end behavior;

library IEEE;
use IEEE.STD_LOGIC_1164.all;

entity mux1of4 is
	port 
	(
		X0, X1, X2, X3: in std_logic;
		SEL: in std_logic_vector (1 downto 0);
		EN: in std_logic;
		Y: out std_logic
	);
end mux1of4;

architecture behavior of mux1of4 is
begin

	Y <= '0' when EN = '0' else
		 X0 when SEL = "00" else
		 X1 when SEL = "01" else
		 X2 when SEL = "10" else
		 X3;

	
end behavior;
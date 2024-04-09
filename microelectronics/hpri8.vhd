library ieee;
use ieee.std_logic_1164.all;
entity hpri8 is
	port(
		R: in 		std_logic_vector(7 downto 0);
		EIN: in 	std_logic;
		EOUT: out 	std_logic;
		G: out 		std_logic;
		A: out 		std_logic_vector(2 downto 0)
	);
end hpri8;
architecture behavior of hpri8 is
begin
	process(R, EIN)
	begin
		if (EIN = '0')then
			A <= ('0','0','0');
		else
			A(2) <= (
				R(7) 
				or R(6) 
				or R(5) 
				or R(4)
			);
			A(1) <= (
				R(7) 
				or R(6) 
				or (not R(5) and not R(4) and R(3)) 
				or (not R(5) and not R(4) and R(2))
			);
			A(0) <= (
				R(7) 
				or (not R(6) and R(5)) 
				or (not R(6) and not R(4) and R(3)) 
				or (not R(6) and not R(4) and not R(2) and R(1))
			);
		end if;
	end process;
	
	EOUT <= '1' when (R = "00000000") else '0';
	G <= '0' when (R = "00000000") else '1';
	
end behavior;
library IEEE;
use IEEE.STD_LOGIC_1164.all;


entity hpri16 is
	port
	(
		R: in 		std_logic_vector(15 downto 0);
		EIN: in 	std_logic;
		EOUT: out 	std_logic;
		G: out 		std_logic;
		A: out 		std_logic_vector(3 downto 0)
	);
end hpri16;


architecture structure of hpri16 is
	component hpri8
		port 
		(			
			r: in 		std_logic_vector(7 downto 0);
			ein: in 	std_logic;
			eout: out 	std_logic;
			g: out 		std_logic;
			a: out 		std_logic_vector(2 downto 0)
		);
	end component;
	
	-- G signals
	signal s_g_main: std_logic;
	signal s_g_top: std_logic;
	signal s_g_down: std_logic;
	
	-- Enable out signals
	signal s_eout_main: std_logic;
	signal s_eout_top: std_logic;
	signal s_eout_down: std_logic;
	
	-- Output signals
	signal s_a_top: std_logic_vector(2 downto 0);
	signal s_a_down: std_logic_vector(2 downto 0);
	
begin
	
	-- r0 - r7
	hDown:hpri8 port map (
		r => 	R(7 downto 0), 
		ein => 	s_eout_top, 
		eout => EOUT, 
		g =>	s_g_down,
		a => 	s_a_down
	);
		
	-- r8 - r15
	hTop:hpri8 port map (
		r => 	R(15 downto 8), 
		ein =>	EIN, 
		eout =>	s_eout_top, 
		g =>	s_g_top,
		a => 	s_a_top
	);
	
	
	--g_a0:or2_ port map (data1=>sAT(0), data2=>sAD(0), result=>s_a_main(0));
	--g_a1:or2_ port map (data1=>sAT(1), data2=>sAD(1), result=>s_a_main(1));
	--g_a2:or2_ port map (data1=>sAT(2), data2=>sAD(2), result=>s_a_main(2));
	--g_g:or2_ port map (data1=>sGT, data2=>sGD, result=>s_g_main);
	
	
	A(0) <= s_a_top(0) or s_a_down(0);
	A(1) <= s_a_top(1) or s_a_down(1);
	A(2) <= s_a_top(2) or s_a_down(2);
	A(3) <= not s_eout_top;
	
	G <= s_g_top or s_g_down;
	--EOUT <= s_eout_main;
	
end structure;

use work.all;

configuration config of hpri16 is
	for structure
		for hTop, hDown : hpri8
			use entity hpri8(behavior);
		end for;
	end for;
end config;

-- HPRI 16 in END
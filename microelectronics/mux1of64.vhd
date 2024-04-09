library IEEE;
use IEEE.STD_LOGIC_1164.all;

entity mux1of64 is
	port 
	(
		X: in 	std_logic_vector(63 downto 0);
		SEL: in std_logic_vector (0 to 5);
		EN: in 	std_logic;
		Y: out 	std_logic
	);
	constant ns: integer := 21;
end mux1of64;

use work.all;

architecture structure of mux1of64 is
	component mux1of4 port 
	(
		X0, X1, X2, X3: in std_logic;
		SEL: in std_logic_vector (0 to 1);
		EN: in std_logic;
		Y: out std_logic
	);
	end component;
	
	signal s_sel:  	std_logic_vector (0 to 5);
	signal s_x_m0: 	std_logic_vector(63 downto 0);
	signal s_x_m1: 	std_logic_vector(15 downto 0);
	signal s_x_m2: 	std_logic_vector(3 downto 0);
	signal s_en: 	std_logic;
	signal s_y: 	std_logic;
begin

	MGEN1: for i in 0 to 15 generate
		mux: mux1of4 port map(
			X0 => s_x_m0(i * 4 + 0),
			X1 => s_x_m0(i * 4 + 1),
			X2 => s_x_m0(i * 4 + 2),
			X3 => s_x_m0(i * 4 + 3),
			SEL => s_sel(4 to 5),
			EN => s_en,
			Y => s_x_m1(i)
		);
	end generate;
	
	MGEN2: for i in 0 to 3 generate
		mux: mux1of4 port map(
			X0 => s_x_m1(i * 4 + 0),
			X1 => s_x_m1(i * 4 + 1),
			X2 => s_x_m1(i * 4 + 2),
			X3 => s_x_m1(i * 4 + 3),
			SEL => s_sel(2 to 3),
			EN => s_en,
			Y => s_x_m2(i)
		);
	end generate;
	
	mux: mux1of4 port map(
		X0 => s_x_m2(0),
		X1 => s_x_m2(1),
		X2 => s_x_m2(2),
		X3 => s_x_m2(3),
		SEL => s_sel(0 to 1),
		EN => s_en,
		Y => s_y
	);
	
	s_sel <= SEL;
	s_x_m0 <= X;
	s_en <= EN;
	Y <= s_y;
	
end structure;
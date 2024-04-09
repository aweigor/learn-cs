library ieee;
use ieee.std_logic_1164.ALL;
entity jkrstr is
	port(
		J, K, R, S: in bit;
		Q, Qi: out bit
	);
end jkrstr;
architecture behavior of jkrstr is
	signal Qs, Qis: bit;
begin
	process(J, K, R, S)
		-- ������ ����� ���������� ��������
		variable Q2: bit;
	begin
		-- ������ ���������� ����� ����� ��� rs 1:1
		-- ������������ �������� rs 0:0 �� �����������
		if (R /= S) then Qs <= not S;
		else
			if (K = '0') then
				-- ������ ������� (�������)
				Q2 := not Qs;
				-- ��������� 1:0 - 1 �� ������ ������
				if (J = '1') then Qs <= '1';
				-- ��������� 0:0 - ��������
				else Qs <= Qs;
				end if;
			else
				-- ��������� 1:1 - ��������
				if (J = '1') then Qs <= Q2;
				-- ��������� 0:1 - 0 �� ������ ������
				else Qs <= '0';
				end if;
			end if;
		end if;
	end process;
	Q <= Qs;
	Qi <= Qis;
	Qis <= not Qs;
end behavior;


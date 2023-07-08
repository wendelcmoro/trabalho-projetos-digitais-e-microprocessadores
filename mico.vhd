-- ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
-- UFPR, BCC, ci210 2017-2 trabalho semestral, autor: Roberto Hexsel, 21out
-- ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


-- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
-- processador MICO XI
-- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
--+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
library ieee; use ieee.std_logic_1164.all; 
use IEEE.numeric_std.all;
use work.p_wires.all;


entity mico is
  port (rst,clk : in bit);
end mico;

architecture functional of mico is

	component adderCSA32 is
		port(inpA, inpB : in bit_vector;
			vem: in bit;
			outC : out bit_vector;
			vai  : out bit);
   end component adderCSA32;

  component mem_prog is                 -- no arquivo mem.vhd
    port (ender : in  reg7;
          instr : out reg32);
  end component mem_prog;

  component display is                  -- neste arquivo
    port (rst,clk : in bit;
          enable  : in bit;
          data    : in reg32);
  end component display;

  component ULA is                      -- neste arquivo
    port (fun : in reg4;
          alfa,beta : in  reg32;
          gama      : out reg32);
  end component ULA;
 
  component R is                        -- neste arquivo
    port (clk         : in  bit;
          wr_en       : in  bit;
          r_a,r_b,r_c : in  reg4;
          A,B         : out reg32;
          C           : in  reg32);
  end component R;

  component RAM is                      -- neste arquivo
    port (rst, clk : in  bit;
          sel      : in  bit;           -- ativo em 1
          wr       : in  bit;           -- ativo em 1
          ender    : in  reg16;
          data_inp : in  reg32;
          data_out : out reg32);
  end component RAM;
  
 component adderAdianta16 is
  port(inpA, inpB : in reg16;
       outC : out reg16;
       vem  : in bit;             -- '0' soma, '1' subtrai    
       vai  : out bit
       );
 end component adderAdianta16;

  type t_control_type is record
    selNxtIP   : reg2;     -- seleciona fonte do incremento do IP
    selC       : reg2;     -- seleciona fonte da escrita no reg destino
    wr_reg     : bit;      -- atualiza banco de registradores
    selBeta    : bit;      -- seleciona fonte para entrada B da ULA
    mem_sel    : bit;      -- habilita acesso a RAM
    mem_wr     : bit;      -- habilita escrita na RAM
    wr_display : bit;      -- atualiza display=1
  end record;

  type t_control_mem is array (0 to 15) of t_control_type;
  
  constant ctrl_table : t_control_mem := (
  --sNxtIP selC  wrR selB  Msel Mwr wrDsp
    ("00", "00", '0', '0', '0', '0', '0'),            -- NOP
    ("00", "10", '1', '0', '0', '0', '0'),            -- ADD
    ("00", "10", '1', '0', '0', '0', '0'),            -- SUB
    ("00", "10", '1', '0', '0', '0', '0'),            -- MUL
    ("00", "10", '1', '0', '0', '0', '0'),            -- AND
    ("00", "10", '1', '0', '0', '0', '0'),            -- OR
    ("00", "10", '1', '0', '0', '0', '0'),            -- XOR
    ("00", "10", '1', '0', '0', '0', '0'),            -- NOT
    ("00", "10", '1', '1', '0', '0', '0'),            -- ADDI
    ("00", "11", '1', '1', '1', '0', '0'),            -- LD
    ("00", "00", '0', '1', '1', '1', '0'),            -- ST
    ("00", "00", '0', '0', '0', '0', '1'),            -- SHOW
    ("10", "01", '1', '0', '0', '0', '0'),            -- JAL
    ("11", "00", '0', '0', '0', '0', '0'),            -- JR
    ("01", "00", '0', '0', '0', '0', '0'),            -- BRANCH
    ("00", "00", '0', '0', '0', '0', '0'));           -- HALT
    

  constant HALT : bit_vector := x"f";

--sinais
  signal selNxtIP, selC : reg2;
  signal selBeta, wr_display, wr_reg : bit;
  signal mem_sel, mem_wr : bit;
  
  signal instr, A, B, C, beta, extended, ula_D, mem_D, ipmaisum : reg32;
  signal this  : t_control_type;
  signal const, ip, ext_zeros, ext_sinal : reg16;
  signal opcode, ra, rb, rc : reg4;
  signal i_opcode : natural range 0 to 15;
  signal ipaux, ipx: reg16;
  
begin  -- functional


	ADD: adderAdianta16 port map(ip, x"0001", ipaux, '0', open); -- incrementa o IP em 1
	
	--processo do mux do ip, possui um comparador como bonus ate o momento
	process(clk, rst)
	begin
		if rst = '1' then
			ip <= x"0000";
		elsif rising_edge(clk) and selNxtIP = "01" and A = B then --para branch
			ip <= const;
		elsif rising_edge(clk) and selNxtIP = "10" then -- para jal
			ip <= const;
		elsif rising_edge(clk) and selNxtIP = "11" then -- para jr
			ip <= A(15 downto 0);
		elsif rising_edge(clk) then -- ip+1
			ip <= ipaux;
		end if;
	end process;
	
  -- memoria de programa contem somente 128 palavras
  U_mem_prog: mem_prog port map(ip(6 downto 0), instr);
  
  --sinais
  opcode <= instr(31 downto 28);
  i_opcode <= BV2INT4(opcode);          -- indice do vetor DEVE ser inteiro
  const <= instr(15 downto 0);
  ra <= instr(27 downto 24);
  rb <= instr(23 downto 20);
  
  --força r(c) receber o registrador 15 quando a instr é JAL
  rc <= x"f" when opcode = "1100" else
		instr(19 downto 16);
  
  this <= ctrl_table(i_opcode);         -- sinais de controle

  selBeta    <= this.selBeta;
  wr_display <= this.wr_display;
  selNxtIP   <= this.selNxtIP;
  wr_reg     <= this.wr_reg;
  selC       <= this.selC;
  mem_sel    <= this.mem_sel;
  mem_wr     <= this.mem_wr;
  
  --manipulaçao da const para uso das operaçoe de soma
  ext_sinal  <= x"ffff";
  ext_zeros  <= x"0000";
  extended	 <= ext_zeros & const when const < x"8000" and (opcode = "1000" or opcode = "1010" or opcode = "1001")  else
				ext_sinal & const;

	--seleçao para o beta
	beta <= B when selBeta = '0' else
		    extended when selBeta = '1';
		
	
	U_regs: R port map (clk, wr_reg,ra ,rb ,rc ,A ,B ,C);--sinais do registrador
	U_ULA: ULA port map (opcode, A	,beta ,ula_D ); --sinais da ULA
    U_mem: RAM port map (rst, clk, mem_sel, mem_wr, ula_D(15 downto 0), B , mem_D); --sinais da RAM
  
  --seleçao de ip mais um para C

  ADD2: adderAdianta16 port map(ip, x"0001", ipx, '0', open); -- incrementa o IP em 1
  ipmaisum <= x"0000" & ipx;
  
	--seleçao do que o registrador C recebera
    C <= ipmaisum when selC = "01" else
		 ula_D when selC = "10" else
		 mem_D when selC = "11";
				 
   
  -- nao altere esta linha
   U_display: display port map (rst, clk, wr_display, A);

  
  
  assert opcode /= HALT
    report LF & LF & "simulation halted: " & 
    "ender = "&integer'image(BV2INT16(ip))&" = "&BV16HEX(ip)&LF
    severity failure;
    			
end functional;


-- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
--ULA A PARTIR DAQUI --
library IEEE; use IEEE.std_logic_1164.all; use IEEE.numeric_std.all;
use work.p_wires.all;

entity ULA is
  port (fun : in reg4;
        alfa,beta : in  reg32;
        gama      : out reg32);
end ULA;
architecture structral of ULA is

	component adderCSA32 is
		port(inpA, inpB : in bit_vector;
			vem: in bit;
			outC : out bit_vector;
			vai  : out bit);
   end component adderCSA32;

	component mult16x16 is
  	port(A, B : in  reg16;   -- entradas A,B
       prod : out reg32);  -- produto
	end component mult16x16;

	signal multi, soma, sub,  multf : reg32;
	signal ainv, b_inv, beta_n: reg32;
	signal mult : reg16;

begin

--para soma e subtraçao
	 beta_n <= not(beta);
	 U_inv_b1: adderCSA32 port map (beta_n, x"00000001", '0', b_inv, open);
	 U_sub: adderCSA32 port map (alfa, b_inv, '0', sub, open);
	 U_soma: adderCSA32 port map(alfa, beta, '0',  soma, open);
--
	
--apenas multiplicaçao  
	 U_mul: mult16x16 port map(alfa(15 downto 0), beta(15 downto 0), multi);
	 mult <= x"ffff" when multi(15) = '1' else
			x"0000";
	
	multf <= mult & multi(15 downto 0);
--
	
		gama <= soma when fun = "0001" else
				sub when fun = "0010" else
				multf when fun = "0011" else
				alfa and beta when fun = "0100" else
				alfa or beta when fun = "0101" else
				alfa xor beta when fun = "0110" else
				not alfa when fun = "0111" else
				soma when fun = "1000" else
				soma when fun = "1001" else
				soma when fun = "1010";
				
end structral;
-- -----------------------------------------------------------------------



-- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
-- display: exibe inteiro na saida padrao do simulador
--          NAO ALTERE ESTE MODELO
-- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
library IEEE; use std.textio.all;
use work.p_wires.all;

entity display is
  port (rst,clk : in bit;
        enable  : in bit;
        data    : in reg32);
end display;

architecture functional of display is
  file output : text open write_mode is "STD_OUTPUT";
begin  -- functional

  U_WRITE_OUT: process(clk)
    variable msg : line;
  begin
    if falling_edge(clk) and enable = '1' then
      write ( msg, string'(BV32HEX(data)) );
      writeline( output, msg );
    end if;
  end process U_WRITE_OUT;

end functional;
-- ++ display ++++++++++++++++++++++++++++++++++++++++++++++++++++++++



-- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
-- memoria RAM, com capacidade de 64K palavras de 32 bits
--          NAO ALTERE ESTE MODELO
-- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
library ieee; use ieee.std_logic_1164.all;
use IEEE.numeric_std.all;
use work.p_wires.all;

entity RAM is
  port (rst, clk : in  bit;
        sel      : in  bit;          -- ativo em 1
        wr       : in  bit;          -- ativo em 1
        ender    : in  reg16;
        data_inp : in  reg32;
        data_out : out reg32);

  constant DATA_MEM_SZ : natural := 2**16;
  constant DATA_ADDRS_BITS : natural := log2_ceil(DATA_MEM_SZ);

end RAM;

architecture rtl of RAM is
  
  subtype t_address is unsigned((DATA_ADDRS_BITS - 1) downto 0);
  
  subtype word is bit_vector(31 downto 0);
  type storage_array is
    array (natural range 0 to (DATA_MEM_SZ - 1)) of word;
  signal storage : storage_array;
begin
  
  accessRAM: process(rst, clk, sel, wr, ender, data_inp)
    variable u_addr : t_address;
    variable index, latched : natural;

    variable d : reg32 := (others => '0');
    variable val, i : integer;

  begin

    if (rst = '0') and (sel = '1') then -- normal operation

      index := BV2INT16(ender);

      if  (wr = '1') and rising_edge(clk) then
        
        assert (index >= 0) and (index < DATA_MEM_SZ)
          report "ramWR index out of bounds: " & natural'image(index)
          severity failure;

        storage(index) <= data_inp;
        
        assert TRUE report "ramWR["& natural'image(index) &"] "
          & BV32HEX(data_inp); -- DEBUG
        
      else

        assert (index >= 0) and (index < DATA_MEM_SZ)
          report "ramRD index out of bounds: " & natural'image(index)
          severity failure;

        d := storage(index);
        
        assert TRUE report "ramRD["& natural'image(index) &"] "
          & BV32HEX(d);  -- DEBUG

      end if; -- normal operation

      data_out <= d;

    else

      data_out <= (others=>'0');

    end if; -- is reset?
    
  end process accessRAM; -- ---------------------------------------------
  
end rtl;
-- -----------------------------------------------------------------------



-- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
-- banco de registradores
--          NAO ALTERE ESTE MODELO
-- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
use work.p_wires.all;

entity R is
  port (clk         : in  bit;
        wr_en       : in  bit;          -- ativo em 1
        r_a,r_b,r_c : in  reg4;
        A,B         : out reg32;
        C           : in  reg32);
end R;

architecture rtl of R is
  type reg_file is array(0 to 15) of reg32;
  signal reg_file_A : reg_file;
  signal reg_file_B : reg_file;
  signal int_ra, int_rb, int_rc : integer range 0 to 15;
begin

  int_ra <= BV2INT4(r_a);
  int_rb <= BV2INT4(r_b);
  int_rc <= BV2INT4(r_c);

  A <= reg_file_A( int_ra ) when r_a /= b"0000" else
       x"00000000";                        -- reg0 always zero
  B <= reg_file_B( int_rb ) when r_b /= b"0000" else
       x"00000000";

  WRITE_REG_BANKS: process(clk)
  begin
    if rising_edge(clk) then
      if wr_en = '1' and r_c /= b"0000" then
        reg_file_A( int_rc ) <= C;
        reg_file_B( int_rc ) <= C;
      end if;
    end if;
  end process WRITE_REG_BANKS;
  
end rtl;
-- -----------------------------------------------------------------------



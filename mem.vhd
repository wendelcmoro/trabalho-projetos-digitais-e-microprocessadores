-- ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
-- UFPR, BCC, ci210 2018-2 trabalho semestral, autor: Roberto Hexsel, 31out
-- ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

use work.p_wires.all;

entity mem_prog is
  port (ender : in  reg7;
        instr : out reg32);

  type t_prog_mem is array (0 to 127) of reg32;

  -- memoria de programa contem somente 64 palavras
  constant program : t_prog_mem := (
    
--main:
    x"00000000", -- nop                  0x00
    x"80040010", -- addi r4, r0, 16      0x01 #carrega a variavel size = 16
	x"84020000", -- addi a0,r4,0         0x02 #carrega o valor 16 como argumento
	x"80030000", -- addi a1,r0,0         0x03 #carrega o endreço da matriz (considerando que seja 0 o seu inicio) como argumento
    x"c0000008", -- jal init         	 0x04 #init(M, 16)
    x"c000001a", -- jal diag             0x05 #diag(M, 16)
    x"81070000", -- addi r7, v0, 0       0x06 #r = diag(M, 16)
    x"e0000029", -- be r0, r0, display   0x07 #go to display
  
--init:  
    x"80080000", -- addi r8, r0, 0       0x08 #i = 0
    x"80090000", -- addi r9, r0, 0       0x09 #j = 0
    x"8f0b0000", -- addi r11, ra, 0      0x0a #r11 = ra(apenas salva o endereço anterior)
--fori:
    x"e8200011", -- beq r8, a0, saii   	 0x0b #if i = 16 then go to returnfromfori  
    x"328a0000", -- mul r10, a0, r8      0x0c #r10 = 16 * i
    x"c0000012", -- jal forj         	 0x0d
    x"88080001", -- addi r8, r8, 1       0x0e #i = i+1
    x"10090000", -- add r9, r0, r0       0x0f #j = 0
    x"e000000b", -- beq r0, r0, fori	 0x10 # recomeça o laço
--returnfromfori
	x"db000000", -- jr r11   	 		 0x11 #retorna a main
	
--forj:
	x"e9200019", -- beq r9, a0, saij     0x12 #if j = 16 then go to returnfromforj
    x"1a9c0000", -- add r12, r10, r9     0x13 #r12 = 16*i + j
    x"1c3c0000", -- add r12, r12, a1     0x14 #r12 recebe o r12 mais o endereço inicial da matriz
    x"189d0000", -- add r13, r8, r9      0x15 #r13 = i + j
    x"acd00000", -- st r13, (r12)   	 0x16 #guarda r13 no endereço apontado por r12
    x"89090001", -- addi r9, r9, 1  	 0x17 #j = j + 1
    x"e0000012", -- beq r0, r0, forj	 0x18 #recomeça o laço
  --returnfromforj
	x"df000000", -- jr ra   	 		 0x19 #retorna para fori
    
--diag
	x"8f0d0000", -- addi r13, ra, 0	     0x1a #guarda o endereço de ra 
    x"80090000", -- addi r9, r0, 0       0x1b #s = 0
    x"80080000", -- addi r8, r0, 0       0x1c #i = 0
    x"c0000020", -- jal foridiag		 0x1d
    x"89010000", -- addi v0, r9, 0		 0x1e #v0 = s
    x"dd000000", -- jr r13				 0x1f #return s
--foridiag:
	x"e8200028", -- beq r8, a0, saidi 	 0x20 #if i = 16 then go to returnformfordiag
    x"328a0000", -- mul r10, r8, a0      0x21 #r10 = i * 16
    x"1a8a0000", -- add r10, r10, r8     0x22 #r10 = i*16 + i
    x"1a3a0000", -- add r10, r10, a1     0x23 #r10 recebe o endereço inicial da matrz + r10
    x"9a0c0000", -- ld r12, (r10)		 0x24 #r12 = o conteudo do endereço de r10
    x"19c90000", -- add r9, r9, r12 	 0x25 #s = s + r12
    x"88080001", -- addi r8, r8, 1		 0x26 #i = i + 1
    x"e0000020", -- beq r0, r0, foridiag 0x27 #recomeça o laço
--returnfromfordiag
	x"df000000", -- jr ra   	 		 0x28 #retorna para diag
    
--display
    x"b7000000", -- show r7				 0x29
    x"f0000000", -- halt				 0x2a
    x"00000000",
    x"00000000",
    x"00000000",
	x"00000000",
    x"00000000",
    x"00000000",
    x"00000000",
    x"00000000",
    x"00000000",
    x"00000000",
    x"00000000",
    x"00000000",
    x"00000000",
    x"00000000",
    x"00000000",
    x"00000000",
    x"00000000",
    x"00000000",
    x"00000000",
    x"00000000",
    x"00000000",

    x"00000000",
    x"00000000",
    x"00000000",
    x"00000000",
    x"00000000",
    x"00000000",
    x"00000000",
    x"00000000",
    x"00000000",
    x"00000000",
    x"00000000",
    x"00000000",
    x"00000000",
    x"00000000",
    x"00000000",
    x"00000000",
    x"00000000",
    x"00000000",
    x"00000000",
    x"00000000",
    x"00000000",
    x"00000000",
    x"00000000",
    x"00000000",
    x"00000000",
    x"00000000",
    x"00000000",
    x"00000000",
    x"00000000",
    x"00000000",
    x"00000000",
    x"00000000",

    x"00000000",
    x"00000000",
    x"00000000",
    x"00000000",
    x"00000000",
    x"00000000",
    x"00000000",
    x"00000000",
    x"00000000",
    x"00000000",
    x"00000000",
    x"00000000",
    x"00000000",
    x"00000000",
    x"00000000",
    x"00000000",
    x"00000000",
    x"00000000",
    x"00000000",
    x"00000000",
    x"00000000",
    x"00000000",
    x"00000000",
    x"00000000",
    x"00000000",
    x"00000000",
    x"00000000",
    x"00000000",
    x"00000000",
    x"00000000",
    x"00000000",
    x"00000000"
  );


  function BV2INT7(S: reg7) return integer is
    variable result: integer;
  begin
    for i in S'range loop
      result := result * 2;
      if S(i) = '1' then
        result := result + 1;
      end if;
    end loop;
    return result;
  end BV2INT7;
  
end mem_prog;

-- nao altere esta arquitetura
architecture tabela of mem_prog is
begin  -- tabela

  instr <= program( BV2INT7(ender) );

end tabela;


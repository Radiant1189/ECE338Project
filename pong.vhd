library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
-- btn connected to up/down pushbuttons for now but
-- eventually will get data from UART
entity pong_graph_st is
	port(
	clk, reset: in std_logic;
	btn:in std_logic_vector(3 downto 0);
	video_on:in std_logic;
	pixel_x:in unsigned(9 downto 0);
	pixel_y:in unsigned(9 downto 0);
	graph_rgb: out std_logic_vector (2 downto 0)
	);
end pong_graph_st;

architecture sq_ball_arch of pong_graph_st is

   signal refr_tick: std_logic;

   signal pix_x: unsigned(9 downto 0); 
   signal pix_y: unsigned(9 downto 0);

   constant MAX_X: integer := 640;
   constant MAX_Y: integer := 480;

   constant WALL_X_L: integer := 32;
   constant WALL_X_R: integer := 35;

-- For right bar
   constant BAR1_X_L: integer := 600;
   constant BAR1_X_R: integer := 603;
   signal bar1_y_t, bar1_y_b: unsigned(9 downto 0);
   constant BAR1_Y_SIZE: integer := 72;

-- Variable for general score
   signal xp_score_val: unsigned(6 downto 0);
   
-- To track player 2 score
   signal score1_reg, score1_next: unsigned(6 downto 0);
   signal score1_add_reg, score1_add_next: unsigned(6 downto 0);
   --signal SCORE_UP: integer := 1;

-- To track player 2 score
   signal score2_reg, score2_next: unsigned(6 downto 0);
   signal SCORE_UP: unsigned(6 downto 0) := to_unsigned(1,7);
   
-- For left bar
   constant BAR2_X_L: integer := 32;
   constant BAR2_X_R: integer := 35;
   signal bar2_y_t, bar2_y_b: unsigned(9 downto 0);
   constant BAR2_Y_SIZE: integer := 72;

   signal bar1_y_reg, bar1_y_next: unsigned(9 downto 0);
   signal bar2_y_reg, bar2_y_next: unsigned(9 downto 0);

   constant BAR_V: integer:= 4;

   constant BALL_SIZE: integer := 8;

   -- square ball -- ball left, right, top and bottom
   -- all vary. Left and top driven by registers below.
   signal ball_x_l, ball_x_r: unsigned(9 downto 0);
   signal ball_y_t, ball_y_b: unsigned(9 downto 0);
   
   -- reg to track left and top boundary
   signal ball_x_reg, ball_x_next: unsigned(9 downto 0);
   signal ball_y_reg, ball_y_next: unsigned(9 downto 0);

   signal x_delta_reg, x_delta_next:
      unsigned(9 downto 0);
   signal y_delta_reg, y_delta_next:
      unsigned(9 downto 0);

   constant BALL_V_P: unsigned(9 downto 0):=
   		to_unsigned(2,10);
   constant BALL_V_N: unsigned(9 downto 0):=
   		unsigned(to_signed(-2,10));

   type rom_type is array( 0 to 7) of
   		std_logic_vector(0 to 7);
   constant BALL_ROM: rom_type:= (
      "00111100",
      "01111110",
      "11111111",
      "11111111",
      "11111111",
      "11111111",
      "01111110",
      "00111100");		

   signal rom_addr, rom_col: unsigned(2 downto 0);
   signal rom_data: std_logic_vector(7 downto 0);
   signal rom_bit: std_logic;
   
   -- rom for score listing
   type word_rom_type is array(0 to 7) of 
         std_logic_vector(0 to 31);
   constant SCORE_ROM: word_rom_type:= (
        "00000000000000000000000000000000",
        "00011000110000110001111001111000",
        "00100001001001001001001001000000",
        "00010001000001001001111001111000",
        "00001001000001001001100001000000",
        "00101001001001001001010001000000",
        "00010000110000110001001001111000",
        "00000000000000000000000000000000");
    
   signal rom_addrS: unsigned(2 downto 0); 
   signal rom_colS: unsigned(4 downto 0);
   signal rom_dataS: std_logic_vector(0 to 31);
   signal rom_bitS: std_logic;
        
   
   type score_rom_type is array(0 to 15) of 
         std_logic_vector(0 to 15);
    constant ZERO_ROM: score_rom_type:= (
        "0000000000000000",
        "0000011111100000",
        "0000100000010000",
        "0000100000010000",
        "0000100000010000",
        "0000100000010000",
        "0000100000010000",
        "0000100000010000",
        "0000100000010000",
        "0000100000010000",
        "0000100000010000",
        "0000100000010000",
        "0000100000010000",
        "0000100000010000",
        "0000011111100000",
        "0000000000000000");
        
    constant ONE_ROM: score_rom_type:= (
        "0000000000000000",
        "0000000110000000",
        "0000011110000000",
        "0000110110000000",
        "0001100110000000",
        "0000000110000000",
        "0000000110000000",
        "0000000110000000",
        "0000000110000000",
        "0000000110000000",
        "0000000110000000",
        "0000000110000000",
        "0000000110000000",
        "0000000110000000",
        "0001111111111000",
        "0000000000000000");
        
    constant TWO_ROM: score_rom_type:= (
        "0000000000000000",
        "0000000110000000",
        "0000011001100000",
        "0000110000011000",
        "0000000000110000",
        "0000000001100000",
        "0000000011000000",
        "0000000110000000",
        "0000001100000000",
        "0000011000000000",
        "0000011000000000",
        "0000110000000000",
        "0000110000000000",
        "0001100000000000",
        "0001111111111000",
        "0000000000000000");
        
     constant THREE_ROM: score_rom_type:= (
        "0000000000000000",
        "0000000110000000",
        "0000011001100000",
        "0000110000011000",
        "0000110000011000",
        "0000000000011000",
        "0000000000011000",
        "0000011111110000",
        "0000011111110000",
        "0000000000011000",
        "0000000000011000",
        "0000110000011000",
        "0000110000011000",
        "0000011001100000",
        "0000000110000000",
        "0000000000000000");
        
     constant FOUR_ROM: score_rom_type:= (
        "0000000000000000",
        "0000000110000000",
        "0000011001100000",
        "0000110000011000",
        "0001111111111110",
        "0000000000011000",
        "0000000000011000",
        "0000000000011000",
        "0000000000011000",
        "0000000000011000",
        "0000000000011000",
        "0000000000011000",
        "0000000000011000",
        "0000000000011000",
        "0000000000000000",
        "0000000000000000");
        
     constant FIVE_ROM: score_rom_type:= (
        "0000000000000000",
        "0001111111111000",
        "0001100000000000",
        "0001100000000000",
        "0001100000000000",
        "0001100000000000",
        "0001111111111000",
        "0000000000011000",
        "0000000000011000",
        "0000000000011000",
        "0001100000011000",
        "0001100000011000",
        "0001100000011000",
        "0001111111111000",
        "0000000000000000",
        "0000000000000000");
     
     constant SIX_ROM: score_rom_type:= (
        "0000000000000000",
        "0000001111100000",
        "000011000111000",
        "0001100000011100",
        "0001100000001100",
        "0001100000000000",
        "0001100111000000",
        "0001111000110000",
        "0001100000011000",
        "0001100000011000",
        "0001100000011000",
        "0001100000011000",
        "0001100000011000",
        "0000011111100000",
        "0000000000000000",
        "0000000000000000");
     
     constant SEVEN_ROM: score_rom_type:= (
        "0000000000000000",
        "0001111111111000",
        "0000000000011000",
        "0000000000011000",
        "0000000000011000",
        "0000000000110000",
        "0000000001100000",
        "0000000011000000",
        "0000000110000000",
        "0000001100000000",
        "0000011000000000",
        "0000110000000000",
        "0001100000000000",
        "0001100000000000",
        "0001100000000000",
        "0000000000000000");
        
     constant EIGHT_ROM: score_rom_type:= (
        "0000000000000000",
        "0000011111100000",
        "0001100000011000",
        "0001100000011000",
        "0001100000011000",
        "0001100000011000",
        "0001100000011000",
        "0000011111100000",
        "0000011111100000",
        "0001100000011000",
        "0001100000011000",
        "0001100000011000",
        "0001100000011000",
        "0001100000011000",
        "0000011111100000",
        "0000000000000000");
        
    constant NINE_ROM: score_rom_type:= (
        "0000000000000000",
        "0000011111100000",
        "0001100000011000",
        "0001100000011000",
        "0001100000011000",
        "0001100000011000",
        "0001100000011000",
        "0000011111111000",
        "0000000000011000",
        "0000000000011000",
        "0000000000011000",
        "0001100000011000",
        "0001100000011000",
        "0001110000111000",
        "0000011111100000",
        "0000000000000000");
   
        
   -- For score generation
   signal rom_addrN, rom_colN: unsigned(3 downto 0);
   signal rom_dataN: std_logic_vector(0 to 15);
   signal rom_bitN: std_logic;
   
   --tiles for score
   signal p1_x_l, p1_x_r: unsigned(9 downto 0);
   signal p1_y_t, p1_y_b: unsigned(9 downto 0);
   constant P1_X_BOX: unsigned(9 downto 0):=
   		to_unsigned(240,10);
   constant P1_Y_BOX: unsigned(9 downto 0):=
   		unsigned(to_signed(0,10));
   signal p2_x_l, p2_x_r: unsigned(9 downto 0);
   signal p2_y_t, p2_y_b: unsigned(9 downto 0);
   constant P2_X_BOX: unsigned(9 downto 0):=
   		to_unsigned(352,10);
   constant P2_Y_BOX: unsigned(9 downto 0):=
   		unsigned(to_signed(0,10));
   signal p1_score_on, sq_score1_on, p2_score_on, sq_score2_on: std_logic;
   
   -- tiles for word score
   signal score_x_l, score_x_r: unsigned(9 downto 0);
   signal score_y_t, score_y_b: unsigned(9 downto 0);
   constant SCORE_X_BOX: unsigned(9 downto 0):=
   		to_unsigned(288,10);
   constant SCORE_Y_BOX: unsigned(9 downto 0):=
   		unsigned(to_signed(0,10)); 
   signal word_score_on, sq_word_on: std_logic;
   
   -- signal for the score
   signal score_reg, score_next: 
        unsigned(6 downto 0);
      


   signal wall_on, bar1_on, sq_ball_on,
      rd_ball_on, bar2_on: std_logic;
   signal wall_rgb, bar_rgb, ball_rgb, num1_rgb, 
      num2_rgb, score_rgb: 
      std_logic_vector(2 downto 0);

   begin

   process (clk, reset)
   		begin
   		if (reset = '1') then
   		   bar1_y_reg <= (others => '0');
   		   bar2_y_reg <= (others => '0');
   		   score1_reg <= (others => '0');
   		   score2_reg <= (others => '0');
   		   score1_add_reg <= (others => '0');
   		   ball_x_reg <= (others => '0');
   		   ball_y_reg <= (others => '0');
   		   x_delta_reg <= ("0000000100");
   		   y_delta_reg <= ("0000000100");
   		elsif (clk'event and clk = '1') then
           bar1_y_reg <= bar1_y_next;
           bar2_y_reg <= bar2_y_next;
           score1_reg <= score1_next;
           score2_reg <= score2_next;
           score1_add_reg <= score1_add_next;
           ball_x_reg <= ball_x_next;
           ball_y_reg <= ball_y_next;
           x_delta_reg <= x_delta_next;
           y_delta_reg <= y_delta_next;
   			end if;
		end process;

    pix_x <= unsigned(pixel_x);
    pix_y <= unsigned(pixel_y);

    refr_tick <= '1' when (pix_y = 1) and (pix_x = 1)
       else '0';

--    wall_on <= '1' when (WALL_X_L <= pix_x) and
--       (pix_x <= WALL_X_R) else '0';
--    wall_rgb <= "001"; -- red

    bar1_y_t <= bar1_y_reg;
    bar1_y_b <= bar1_y_t + BAR1_Y_SIZE - 1;
    bar1_on <= '1' when (BAR1_X_L <= pix_x) and
    	(pix_x <= BAR1_X_R) and (bar1_y_t <= pix_y) and
    	(pix_y <= bar1_y_b) else '0';
    bar_rgb <= "010"; -- blue
    
    bar2_y_t <= bar2_y_reg;
    bar2_y_b <= bar2_y_t + BAR2_Y_SIZE - 1;
    bar2_on <= '1' when (BAR2_X_L <= pix_x) and
    	(pix_x <= BAR2_X_R) and (bar2_y_t <= pix_y) and
    	(pix_y <= bar2_y_b) else '0';
    --bar_rgb <= "010"; -- blue

    process( bar1_y_reg, bar1_y_b, bar1_y_t, refr_tick, btn)
    	begin
    	bar1_y_next <= bar1_y_reg; -- no move 

    	if ( refr_tick = '1' ) then
    -- if btn 1 pressed and paddle not at bottom yet
    		if ( btn(1) = '1' and bar1_y_b <
    			(MAX_Y - 1 - BAR_V)) then
    			bar1_y_next <= bar1_y_reg + BAR_V;
	-- if btn 0 pressed and bar not at top yet
			elsif ( btn(0) = '1' and bar1_y_t > BAR_V) then
				bar1_y_next <= bar1_y_reg - BAR_V;
			end if;
		end if;
end process;

    process( bar2_y_reg, bar2_y_b, bar2_y_t, refr_tick, btn)
    	begin
    	bar2_y_next <= bar2_y_reg; -- no move 

    	if ( refr_tick = '1' ) then
    -- if btn 1 pressed and paddle not at bottom yet
    		if ( btn(3) = '1' and bar2_y_b <
    			(MAX_Y - 1 - BAR_V)) then
    			bar2_y_next <= bar2_y_reg + BAR_V;
	-- if btn 0 pressed and bar not at top yet
			elsif ( btn(2) = '1' and bar2_y_t > BAR_V) then
				bar2_y_next <= bar2_y_reg - BAR_V;
			end if;
		end if;
end process;
	-- set coordinates of square ball.
	ball_x_l <= ball_x_reg;
	ball_y_t <= ball_y_reg;
	ball_x_r <= ball_x_l + BALL_SIZE - 1;
	ball_y_b <= ball_y_t + BALL_SIZE - 1;

	-- pixel within square ball
		sq_ball_on <= '1' when (ball_x_l <= pix_x) and
			(pix_x <= ball_x_r) and (ball_y_t <= pix_y) and
			(pix_y <= ball_y_b) else '0';
	-- map scan coord to ROM addr/col 
	-- use low order three
	-- bits of pixel and ball positions.
	-- ROM row
	  rom_addr <= pix_y(2 downto 0) - ball_y_t(2 downto 0);
	-- ROM column
	  rom_col <= pix_x(2 downto 0) - ball_x_l(2 downto 0);
	-- Get row data
	  rom_data <= BALL_ROM(to_integer(rom_addr));
	  -- Get column bit
	  rom_bit <= rom_data(to_integer(rom_col));
	  -- Turn ball on only if within square and ROM bit is 1.
	  rd_ball_on <= '1' when (sq_ball_on = '1') and
			(rom_bit = '1') else '0';
	  ball_rgb <= "100"; -- green

	  -- Update the ball position 60 times per second.
	  ball_x_next <= ball_x_reg + x_delta_reg when
			 refr_tick = '1' else ball_x_reg;
	  ball_y_next <= ball_y_reg + y_delta_reg when
			 refr_tick = '1' else ball_y_reg;
      score1_next <= score1_reg + score1_add_reg when
             refr_tick = '1' else score1_reg; 
	  -- Set the value of the next ball position according to
	  -- the boundaries.
	  process(x_delta_reg, y_delta_reg, score1_add_reg, ball_y_t, ball_x_l, ball_x_r, ball_y_t, ball_y_b, bar1_y_t, bar1_y_b, bar2_y_t, bar2_y_b)
		  begin
		  x_delta_next <= x_delta_reg;
		  y_delta_next <= y_delta_reg;
		  score1_add_next <= score1_add_reg;
	  -- ball reached top, make offset positive
		  if ( ball_y_t < 1 ) then
			  y_delta_next <= BALL_V_P;
	  -- reached bottom, make negative
		  elsif (ball_y_b > (MAX_Y - 1)) then
			  y_delta_next <= BALL_V_N;
	  -- Reached right, rebound.
	      elsif (ball_x_r > (MAX_X - 1)) then
	          x_delta_next <= BALL_V_N;
	  -- Reached left, rebound.
	      elsif (ball_x_l < 1 ) then
	          x_delta_next <= BALL_V_P;
	  -- left corner of ball inside bar2,
		  elsif (( ball_x_l <= BAR2_X_R ) and 
		  (BAR2_X_L <= ball_x_l)) then
      --some portion of ball hitting paddle, reverse dir
		      if ((bar2_y_t <= ball_y_b) and
		      (ball_y_t <= bar2_y_b)) then
		       x_delta_next <= BALL_V_P;
		  end if;
	  -- right corner of ball inside bar1
		  elsif ((BAR1_X_L <= ball_x_r) and 
		  (ball_x_r <= BAR1_X_R)) then
	  -- some portion of ball hitting paddle, reverse dir
		      if ((bar1_y_t <= ball_y_b) and
		      (ball_y_t <= bar1_y_b)) then
		      x_delta_next <= BALL_V_N;
	      end if;
    end if;
end process;

--Set down tiles for Player scores
    p1_x_l <= P1_X_BOX; -- Player 1 score
    p1_y_t <= P1_Y_BOX;
    p1_x_r <= p1_x_l + 15;
    p1_y_b <= p1_y_t + 15;
    
    p2_x_l <= P2_X_BOX; -- Player 2 score
    p2_y_t <= P2_Y_BOX;
    p2_x_r <= p2_x_l + 15;
    p2_y_b <= p2_y_t + 15;

-- pixel within score number 
    sq_score1_on <= '1' when (p1_x_l <= pix_x) and
			(pix_x <= p1_x_r) and (p1_y_t <= pix_y) and
			(pix_y <= p1_y_b) else '0';
    sq_score2_on <= '1' when (p2_x_l <= pix_x) and
			(pix_x <= p2_x_r) and (p2_y_t <= pix_y) and
			(pix_y <= p2_y_b) else '0';

-- Logic to display score
process(p1_score_on, p2_score_on, xp_score_val, score1_next, score2_next)
            begin
            xp_score_val <= "0000000";
            if(sq_score1_on = '1') then
                xp_score_val <= "0000010";
            end if;
            if(sq_score2_on = '1') then
                xp_score_val <= "0000100";
            end if;
            case(xp_score_val) is
                when "0000000" =>
                    rom_addrN <= pix_y(3 downto 0);
                    -- ROM column
                    rom_colN <= pix_x(3 downto 0);
                    -- Get row data
                    rom_dataN <= ZERO_ROM(to_integer(rom_addrN));
                    -- Get column bit
                    rom_bitN <= rom_dataN(to_integer(rom_colN));
                when "0000001" =>
                    rom_addrN <= pix_y(3 downto 0);
                    -- ROM column
                    rom_colN <= pix_x(3 downto 0);
                    -- Get row data
                    rom_dataN <= ONE_ROM(to_integer(rom_addrN));
                    -- Get column bit
                    rom_bitN <= rom_dataN(to_integer(rom_colN));
                when "0000010" =>
                    rom_addrN <= pix_y(3 downto 0);
                    -- ROM column
                    rom_colN <= pix_x(3 downto 0);
                    -- Get row data
                    rom_dataN <= TWO_ROM(to_integer(rom_addrN));
                    -- Get column bit
                    rom_bitN <= rom_dataN(to_integer(rom_colN));
                when "0000011" =>
                    rom_addrN <= pix_y(3 downto 0);
                    -- ROM column
                    rom_colN <= pix_x(3 downto 0);
                    -- Get row data
                    rom_dataN <= THREE_ROM(to_integer(rom_addrN));
                    -- Get column bit
                    rom_bitN <= rom_dataN(to_integer(rom_colN));
                when "0000100" =>
                    rom_addrN <= pix_y(3 downto 0);
                    -- ROM column
                    rom_colN <= pix_x(3 downto 0);
                    -- Get row data
                    rom_dataN <= FOUR_ROM(to_integer(rom_addrN));
                    -- Get column bit
                    rom_bitN <= rom_dataN(to_integer(rom_colN));
                when "0000101" =>
                    rom_addrN <= pix_y(3 downto 0);
                    -- ROM column
                    rom_colN <= pix_x(3 downto 0);
                    -- Get row data
                    rom_dataN <= FIVE_ROM(to_integer(rom_addrN));
                    -- Get column bit
                    rom_bitN <= rom_dataN(to_integer(rom_colN));
                when "0000110" =>
                    rom_addrN <= pix_y(3 downto 0);
                    -- ROM column
                    rom_colN <= pix_x(3 downto 0);
                    -- Get row data
                    rom_dataN <= SIX_ROM(to_integer(rom_addrN));
                    -- Get column bit
                    rom_bitN <= rom_dataN(to_integer(rom_colN));
                when "0000111" =>
                    rom_addrN <= pix_y(3 downto 0);
                    -- ROM column
                    rom_colN <= pix_x(3 downto 0);
                    -- Get row data
                    rom_dataN <= SEVEN_ROM(to_integer(rom_addrN));
                    -- Get column bit
                    rom_bitN <= rom_dataN(to_integer(rom_colN));
                when "0001000" =>
                    rom_addrN <= pix_y(3 downto 0);
                    -- ROM column
                    rom_colN <= pix_x(3 downto 0);
                    -- Get row data
                    rom_dataN <= EIGHT_ROM(to_integer(rom_addrN));
                    -- Get column bit
                    rom_bitN <= rom_dataN(to_integer(rom_colN));
                when "0001001" =>
                    rom_addrN <= pix_y(3 downto 0);
                    -- ROM column
                    rom_colN <= pix_x(3 downto 0);
                    -- Get row data
                    rom_dataN <= NINE_ROM(to_integer(rom_addrN));
                    -- Get column bit
                    rom_bitN <= rom_dataN(to_integer(rom_colN));
                when others =>
                    rom_addrN <= pix_y(3 downto 0);
                    -- ROM column
                    rom_colN <= pix_x(3 downto 0);
                    -- Get row data
                    rom_dataN <= FIVE_ROM(to_integer(rom_addrN));
                    -- Get column bit
                    rom_bitN <= rom_dataN(to_integer(rom_colN));
            end case;
      end process;
    
    -- Turn scores on only if within square and ROM bit is 1.
    p1_score_on <= '1' when (sq_score1_on = '1') and
          (rom_bitN = '1') else '0';
    num1_rgb <= "100"; -- green
    p2_score_on <= '1' when (sq_score2_on = '1') and
          (rom_bitN = '1') else '0';
    num2_rgb <= "001"; -- red
    
  -- Pixel within word score
    sq_word_on <= '1' when (score_x_l <= pix_x) and
			(pix_x <= score_x_r) and (score_y_t <= pix_y) and
			(pix_y <= score_y_b) else '0';
  --Set down tiles for Player scores
     score_x_l <= SCORE_X_BOX; -- SCORE placement
     score_y_t <= SCORE_Y_BOX;
     score_x_r <= score_x_l + 31;
     score_y_b <= score_y_t + 7;
     
    rom_addrS <= pix_y(2 downto 0);
	-- ROM column
	rom_colS <= pix_x(4 downto 0);
	-- Get row data
	rom_dataS <= SCORE_ROM(to_integer(rom_addrS));
    -- Get column bit
    rom_bitS <= rom_dataS(to_integer(rom_colS));
    -- Turn word on only if within square and ROM bit is 1.
    word_score_on <= '1' when (sq_word_on = '1') and
          (rom_bitS = '1') else '0';
    score_rgb <= "111"; -- White
    
    process (video_on, wall_on, bar1_on, rd_ball_on, wall_rgb, bar_rgb, ball_rgb, num1_rgb, num2_rgb, p1_score_on, p2_score_on, word_score_on, score_rgb, bar2_on)
		  begin
		  if (video_on = '0') then
			  graph_rgb <= "000"; -- blank
	          else
--			  if (wall_on = '1') then
--				  graph_rgb <= wall_rgb;
              if (bar2_on = '1') then
                  graph_rgb <= bar_rgb;
			  elsif (bar1_on = '1') then
				  graph_rgb <= bar_rgb;
			  elsif (rd_ball_on = '1') then
				  graph_rgb <= ball_rgb;
			  elsif (p1_score_on = '1') then
			      graph_rgb <= num1_rgb;
			  elsif (p2_score_on = '1') then
			      graph_rgb <= num2_rgb;
			  elsif (word_score_on = '1') then
			      graph_rgb <= score_rgb;
			  else
				  graph_rgb <= "000"; -- yellow bkgnd
			  end if;
		  end if;
	  end process;
  end sq_ball_arch;






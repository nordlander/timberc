#include <avr/io.h>
#include <stdlib.h>

unsigned int LCD_character_table[] = {
    0x0000,     // 
    0x0001,     // 
    0x0002,     // 
    0x0003,     // 
    0x0004,     // 
    0x0005,     // 
    0x0006,     // 
    0x0007,     // 
    0x0008,     // 
    0x0009,     // 
    0x000a,     // 
    0x000b,     // 
    0x000c,     // 
    0x000d,     // 
    0x000e,     // 
    0x000f,     // 

    0x0010,     // 
    0x0011,     // 
    0x0012,     // 
    0x0013,     // 
    0x0014,     // 
    0x0015,     // 
    0x0016,     // 
    0x0017,     // 
    0x0018,     // 
    0x0019,     // 
    0x001a,     // 
    0x001b,     // 
    0x001c,     // 
    0x001d,     // 
    0x001e,     // 
    0x001f,     // 

    0x0020,     // ' '
    0x0000,     // '!' (Not defined)
    0x0000,     // '"' (Not defined)
    0x0000,     // '#' (Not defined)
    0x0000,     // '$' (Not defined)
    0x0000,     // '%' (Not defined)
    0x0000,     // '&' (Not defined)
    0x0000,     // ''' (Not defined)
    0x0000,     // '(' (Not defined)
    0x0000,     // ')' (Not defined)
    0xECC8,     // '*'
    0x2A80,     // '+'
    0x0000,     // ',' (Not defined)
    0x0A00,     // '-'
    0x0A51,     // '.' Degree sign
    0x4008,     // '/'

    0x5559,     // '0'
    0x0118,     // '1'
    0x1e11,     // '2
    0x1b11,     // '3
    0x0b50,     // '4
    0x1b41,     // '5
    0x1f41,     // '6
    0x0111,     // '7
    0x1f51,     // '8
    0x1b51,     // '9'
    0x0000,     // ':' (Not defined)
    0x0000,     // ';' (Not defined)
    0x0000,     // '<' (Not defined)
    0x0000,     // '=' (Not defined)
    0x0000,     // '>' (Not defined)
    0x0000,     // '?' (Not defined)

    0x0000,     // '@' (Not defined)
    0x0f51,     // 'A'
    0x3991,     // 'B'
    0x1441,     // 'C'
    0x3191,     // 'D'
    0x1e41,     // 'E'
    0x0e41,     // 'F'
    0x1d41,     // 'G'
    0x0f50,     // 'H'
    0x2080,     // 'I'
    0x1510,     // 'J'
    0x8648,     // 'K'
    0x1440,     // 'L'
    0x0578,     // 'M'
    0x8570,     // 'N'
    0x1551,     // 'O'

    0x0e51,     // 'P'
    0x9551,     // 'Q'
    0x8e51,     // 'R'
    0x9021,     // 'S'
    0x2081,     // 'T'
    0x1550,     // 'U'
    0x4448,     // 'V'
    0xc550,     // 'W'
    0xc028,     // 'X'
    0x2028,     // 'Y'
    0x5009,     // 'Z'
    0x1441,     // '['
    0x8020,     // '\'
    0x1111,     // ']'
    0x0000,     // '^' (Not defined)
    0x1000,     // '_'

    0x0000,     // '`' (Not defined)
    0x0f51,     // 'a'
    0x3991,     // 'b'
    0x1441,     // 'c'
    0x3191,     // 'd'
    0x1e41,     // 'e'
    0x0e41,     // 'f'
    0x1d41,     // 'g'
    0x0f50,     // 'h'
    0x2080,     // 'i'
    0x1510,     // 'j'
    0x8648,     // 'k'
    0x1440,     // 'l'
    0x0578,     // 'm'
    0x8570,     // 'n'
    0x1551,     // 'o'

    0x0e51,     // 'p'
    0x9551,     // 'q'
    0x8e51,     // 'r'
    0x9021,     // 's'
    0x2081,     // 't'
    0x1550,     // 'u'
    0x4448,     // 'v'
    0xc550,     // 'w'
    0xc028,     // 'x'
    0x2028,     // 'y'
    0x5009,     // 'z'
    0x1461,     // '{'
    0x2080,     // '|'
    0x1911,     // '}'
    0x0000,     // '~' (Not defined)
    0x0000      // 'DEL' (Not defined)
};

void initLCD (void) {

	// Set the LCD contrast level
	LCDCCR = 0x0F;

    // Select asynchronous clock source, enable all COM pins and enable all segment pins.
	LCDCRB = 0xB7;
	
    // Set LCD prescaler to give a framerate of 32,0 Hz
    LCDFRR = 0x07;
	
    // Enable LCD and set low power waveform
	LCDCRA = 0xC0;

}

void writeChar(char pos, char chr) {
	switch (pos)
	{
		case 1:
			LCDDR0  = (LCDDR0 & 0xF0) | ((LCD_character_table[chr-0] & 0x000F));
			LCDDR5  = (LCDDR5 & 0xF0) | ((LCD_character_table[chr-0] & 0x00F0) >> 4);
			LCDDR10 = (LCDDR10 & 0xF0) | ((LCD_character_table[chr-0] & 0x0F00) >> 8);
			LCDDR15 = (LCDDR15 & 0xF0) | ((LCD_character_table[chr-0] & 0xF000) >> 12);
			break;
		case 2:
			LCDDR0  = (LCDDR0 & 0x0F) | ((LCD_character_table[chr-0] & 0x000F) << 4);
			LCDDR5  = (LCDDR5 & 0x0F) | ((LCD_character_table[chr-0] & 0x00F0));
			LCDDR10 = (LCDDR10 & 0x0F) | ((LCD_character_table[chr-0] & 0x0F00) >> 4);
			LCDDR15 = (LCDDR15 & 0x0F) | ((LCD_character_table[chr-0] & 0xF000) >> 8);
			break;
		case 3:
			LCDDR1  = (LCDDR1 & 0xF0) | ((LCD_character_table[chr-0] & 0x000F));
			LCDDR6  = (LCDDR6 & 0xF0) | ((LCD_character_table[chr-0] & 0x00F0) >> 4);
			LCDDR11 = (LCDDR11 & 0xF0) | ((LCD_character_table[chr-0] & 0x0F00) >> 8);
			LCDDR16 = (LCDDR16 & 0xF0) | ((LCD_character_table[chr-0] & 0xF000) >> 12);
			break;
		case 4:
			LCDDR1  = (LCDDR1 & 0x0F) | ((LCD_character_table[chr-0] & 0x000F) << 4);
			LCDDR6  = (LCDDR6 & 0x0F) | ((LCD_character_table[chr-0] & 0x00F0));
			LCDDR11 = (LCDDR11 & 0x0F) | ((LCD_character_table[chr-0] & 0x0F00) >> 4);
			LCDDR16 = (LCDDR16 & 0x0F) | ((LCD_character_table[chr-0] & 0xF000) >> 8);
			break;
		case 5:
			LCDDR2  = (LCDDR2 & 0xF0) | ((LCD_character_table[chr-0] & 0x000F));
			LCDDR7  = (LCDDR7 & 0xF0) | ((LCD_character_table[chr-0] & 0x00F0) >> 4);
			LCDDR12 = (LCDDR12 & 0xF0) | ((LCD_character_table[chr-0] & 0x0F00) >> 8);
			LCDDR17 = (LCDDR17 & 0xF0) | ((LCD_character_table[chr-0] & 0xF000) >> 12);
			break;
		case 6:
			LCDDR2  = (LCDDR2 & 0x0F) | ((LCD_character_table[chr-0] & 0x000F) << 4);
			LCDDR7  = (LCDDR7 & 0x0F) | ((LCD_character_table[chr-0] & 0x00F0));
			LCDDR12 = (LCDDR12 & 0x0F) | ((LCD_character_table[chr-0] & 0x0F00) >> 4);
			LCDDR17 = (LCDDR17 & 0x0F) | ((LCD_character_table[chr-0] & 0xF000) >> 8);
			break;
	}
}

void clearLCD(void) {
    LCDDR0 = 0;
    LCDDR5 = 0;
    LCDDR10 = 0;
    LCDDR15 = 0;
    LCDDR1 = 0;
    LCDDR6 = 0;
    LCDDR11 = 0;
    LCDDR16 = 0;
    LCDDR2 = 0;
    LCDDR7 = 0;
    LCDDR12 = 0;
    LCDDR17 = 0;
    LCDDR3 = 0;
    LCDDR8 = 0;
    LCDDR13 = 0;
    LCDDR18 = 0;
}

void writeStr(char *str) {
	int i=0;

	while(str[i] && (i < 6)) {
		writeChar(i+1, str[i]);
		i++;
	}
}

void writeInt(int value) {
	char str[10];

	itoa(value, str, 10);
	writeStr(str);
}

void writeLong(long value) {
	char str[10];

	ltoa(value, str, 10);
	writeStr(str);
}

void debug(int n) {
    writeChar(n, '0' + n);
}

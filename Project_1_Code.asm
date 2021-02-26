; EFM8_Receiver.asm:  This program implements a simple serial port
; communication protocol to program, verify, and read an SPI flash memory.  Since
; the program was developed to store wav audio files, it also allows 
; for the playback of said audio.  It is assumed that the wav sampling rate is
; 22050Hz, 8-bit, mono.
;
; Connections:
; 
; EFM8 board  SPI_FLASH
; P0.0        Pin 6 (SPI_CLK)
; P0.1        Pin 2 (MISO)
; P0.2        Pin 5 (MOSI)
; P0.3        Pin 1 (CS/)
; GND         Pin 4
; 3.3V        Pins 3, 7, 8  (The MCP1700 3.3V voltage regulator or similar is required)
;
; P3.0 is the DAC output which should be connected to the input of power amplifier (LM386 or similar)
;

$NOLIST
$MODEFM8LB1
$LIST

org 0x0000 ; Reset vector
    ljmp MainProgram

org 0x0003 ; External interrupt 0 vector (not used in this code)
	reti

org 0x000B ; Timer/Counter 0 overflow interrupt vector (not used in this code)
	reti

org 0x0013 ; External interrupt 1 vector (not used in this code)
	reti

org 0x001B ; Timer/Counter 1 overflow interrupt vector (not used in this code
	reti

org 0x0023 ; Serial port receive/transmit interrupt vector (not used in this code)
	reti

org 0x005b ; Timer 2 interrupt vector.  Used in this code to replay the wave file.
	ljmp Timer2_ISR

SYSCLK         EQU 72000000  ; Microcontroller system clock frequency in Hz
TIMER2_RATE    EQU 22050     ; 22050Hz is the sampling rate of the wav file we are playing
TIMER2_RELOAD  EQU 0x10000-(SYSCLK/TIMER2_RATE)
F_SCK_MAX      EQU 20000000
BAUDRATE       EQU 115200

sound_index :
    ZERO EQU 0x00
	ZERO1 EQU 0x00
	ZERO2 EQU 0x4d ;0
	ONE EQU 0x00
	ONE1 EQU 0x3a
	ONE2 EQU 0x02	;1
	TWO EQU 0x00  
	TWO1 EQU 0x76
	TWO2 EQU 0xd4  ;2
	THREE EQU 0x00
	THREE1 EQU 0x97
	THREE2 EQU 0xb9 ; 3 
	FOUR EQU 0x00
	FOUR1 EQU 0xca
	FOUR2 EQU 0x2d ; 4 
	FIVE EQU 0x01
	FIVE1 EQU 0x07
	FIVE2 EQU 0xb5 ; 5 
	SIX EQU 0x01
	SIX1 EQU 0x32
	SIX2 EQU 0xdc ; 6 
	SEVEN EQU 0x01
	SEVEN1 EQU 0x69
	SEVEN2 EQU 0xc5 ; 7 
	EIGHT EQU 0x01
	EIGHT1 EQU 0xa8
	EIGHT2 EQU 0xe3 ; 8 
	NINE EQU 0x01
	NINE1 EQU 0xcb
	NINE2 EQU 0xd1 ; 9 
	TEN EQU 0x02
	TEN1 EQU 0x06
	TEN2 EQU 0xcd ; 10 
	ELEVEN EQU 0x02
	ELEVEN1 EQU 0x3f
	ELEVEN2 EQU 0xee ; 11 
	TWELVE EQU 0x02
	TWELVE1 EQU 0x85
	TWELVE2 EQU 0x51 ; 12 
	THIRTEEN EQU 0x02
	THIRTEEN1 EQU 0xca
	THIRTEEN2 EQU 0xf3 ; 13 
	FOURTEEN EQU 0x03
	FOURTEEN1 EQU 0x0e
	FOURTEEN2 EQU 0x3c ; 14 
	FIFTEEN EQU 0x03
	FIFTEEN1 EQU 0x5b
	FIFTEEN2 EQU 0x4b ; 15 
	SIXTEEN EQU 0x03
	SIXTEEN1 EQU 0xa7
	SIXTEEN2 EQU 0xb5 ; 16 
	SEVENTEEN EQU 0x03
	SEVENTEEN1 EQU 0xf6
	SEVENTEEN2 EQU 0x8e ; 17 
	EIGHTEEEN EQU 0x04
	EIGHTEEN1 EQU 0x4e
	EIGHTEEN2 EQU 0x98 ; 18 
	NINETEEN EQU 0x04
	NINETEEN1 EQU 0x8c
	NINETEEN2 EQU 0x64 ; 19 
	TWENTY EQU 0x04
	TWENTY1 EQU 0xd6
	TWENTY2 EQU 0x78 ; 20 
	THIRTY EQU 0x05
	THIRTY1 EQU 0x20
	THIRTY2 EQU 0x67 ; 21 
	FORTY EQU 0x05
	FORTY1 EQU 0x66
	FORTY2 EQU 0xc7 ; 22 
	FIFTY EQU 0x05
	FIFTY1 EQU 0xb6
	FIFTY2 EQU 0xda ; 23 
	SIXTY EQU 0x06
	SIXTY1 EQU 0x04
	SIXTY2 EQU 0x87 ; 24 
	SEVENTY EQU 0x06
	SEVENTY1 EQU 0x5a
	SEVENTY2 EQU 0x38 ; 25 
	EIGHTY EQU 0x06
	EIGHTY1 EQU 0xb5
	EIGHTY2 EQU 0x0b ; 26 
	NINETY EQU 0x06
	NINETY1 EQU 0xf4
	NINETY2 EQU 0xfc ; 27 
	HUNDRED EQU 0x07
	HUNDRED1 EQU 0x49
	HUNDRED2 EQU 0x20 ; 28 
	POINT EQU 0x07
	POINT1 EQU 0x8c
	POINT2 EQU 0x87 ; 29 
	NANO EQU 0x07
	NANO1 EQU 0xc8
	NANO2 EQU 0xe1 ; 30 
	MICRO EQU 0x08
	MICRO1 EQU 0x07
	MICRO2 EQU 0xb8 ; 31 
	FARADS EQU 0x08
	FARADS1 EQU 0x53
	FARADS2 EQU 0x47 ; 32 
	WATER EQU 0x08
	WATER1 EQU 0x92
	WATER2 EQU 0xc7 ; 33 
	LEVEL EQU 0x08
	LEVEL1 EQU 0xca
	LEVEL2 EQU 0xfb ; 34 
	CENTI EQU 0x09
	CENTI1 EQU 0x12
	CENTI2 EQU 0x09 ; 35 
	MILLI EQU 0x09
	MILLI1 EQU 0x5f
	MILLI2 EQU 0x89 ; 36 
	METERS EQU 0x09
	METERS1 EQU 0x98
	METERS2 EQU 0x1d ; 37 
	PER EQU 0x09
	PER1 EQU 0xed
	PER2 EQU 0xf9 ; 38 
	CENT EQU 0x0a
	CENT1 EQU 0x1d
	CENT2 EQU 0xb5 ; 39 
	CUP EQU 0x0a
	CUP1 EQU 0x54
	CUP2 EQU 0x25 ; 40 
	IS EQU 0x0a
	IS1 EQU 0x81
	IS2 EQU 0x61 ; 41 
	FULL EQU 0x0a
	FULL1 EQU 0xa5
	FULL2 EQU 0x07 ; 42 
	EMPTY EQU 0x0a
	EMPTY1 EQU 0xcf
	EMPTY2 EQU 0xf5 ; 43  
	;bruh1 EQU 0x0b16e0 ; 44 
	;bruh2 EQU 0x0b2f5c :


; Size of each sound in 'sound_index'
Size_sound:
	ZERO_LEN EQU 0x00
	ZERO_LEN1 EQU 0x39
	ZERO_LEN2 EQU 0xb5 ; 0 
	ONE_LEN EQU 0x00
	ONE_LEN1 EQU 0x3c
	ONE_LEN2 EQU 0xd2 ; 1 
	TWO_LEN EQU 0x00
	TWO_LEN1 EQU 0x20
	TWO_LEN2 EQU 0xe5 ; 2 
	THREE_LEN EQU 0x00
	THREE_LEN1 EQU 0x32
	THREE_LEN2 EQU 0x74 ; 3 
	FOUR_LEN EQU 0x00
	FOUR_LEN1 EQU 0x3d
	FOUR_LEN2 EQU 0x88 ; 4 
	FIVE_LEN EQU 0x00
	FIVE_LEN1 EQU 0x2b
	FIVE_LEN2 EQU 0x27 ; 5 
	SIX_LEN EQU 0x00
	SIX_LEN1 EQU 0x36
	SIX_LEN2 EQU 0xe9 ; 6 
	SEVEN_LEN EQU 0x00
	SEVEN_LEN1 EQU 0x3f
	SEVEN_LEN2 EQU 0x1e ; 7 
	EIGHT_LEN EQU 0x00
	EIGHT_LEN1 EQU 	0x22
	EIGHT_LEN2 EQU 0xee ; 8 
	NINE_LEN EQU 0x00
	NINE_LEN1 EQU 0x3a
	NINE_LEN2 EQU 0xfc ; 9 
	TEN_LEN EQU 0x00
	TEN_LEN1 EQU 0x39
	TEN_LEN2 EQU 0x21 ; 10 
	ELEVEN_LEN EQU 0x00
	ELEVEN_LEN1 EQU 0x45
	ELEVEN_LEN2 EQU 0x63 ; 11 
	TWELVE_LEN EQU 0x00
	TWELVE_LEN1 EQU 0x45
	TWELVE_LEN2 EQU 0xa2 ; 12 
	THIRTEEN_LEN EQU 0x00
	THIRTEEN_LEN1 EQU 0x43
	THIRTEEN_LEN2 EQU 0x49 ; 13 
	FOURTEEN_LEN EQU 0x00
	FOURTEEN_LEN1 EQU 0x4d
	FOURTEEN_LEN2 EQU 0x0f ; 14 
	FIFTEEN_LEN EQU 0x00
	FIFTEEN_LEN1 EQU 0x4c
	FIFTEEN_LEN2 EQU 0x6a ; 15 
	SIXTEEN_LEN EQU 0x00
	SIXTEEN_LEN1 EQU 0x4e
	SIXTEEN_LEN2 EQU 0xd9 ; 16 
	SEVENTEEN_LEN EQU 0x00
	SEVENTEEN_LEN1 EQU 0x58
	SEVENTEEN_LEN2 EQU 0x0a ; 17 
	EIGHTEEN_LEN EQU 0x00
	EIGHTEEN_LEN1 EQU 0x3d
	EIGHTEEN_LEN2 EQU 0xcc ; 18 
	NINETEEN_LEN EQU 0x00
	NINETEEN_LEN1 EQU 0x4a
	NINETEEN_LEN2 EQU 0x14 ; 19 
	TWENTY_LEN EQU 0x00
	TWENTY_LEN1 EQU 0x49
	TWENTY_LEN2 EQU 0xef ; 20 
	THIRTY_LEN EQU 0x00
	THIRTY_LEN1 EQU 0x46
	THIRTY_LEN2 EQU 0x60 ; 21 
	FORTY_LEN EQU 0x00
	FORTY_LEN1 EQU 0x50
	FORTY_LEN2 EQU 0x13 ; 22 
	FIFTY_LEN EQU 0x00
	FIFTY_LEN1 EQU 0x4d
	FIFTY_LEN2 EQU 0xad ; 23 
	SIXTY_LEN EQU 0x00
	SIXTY_LEN1 EQU 0x55
	SIXTY_LEN2 EQU 0xb1 ; 24 
	SEVENTY_LEN EQU 0x00
	SEVENTY_LEN1 EQU 0x5a
	SEVENTY_LEN2 EQU 0xd3 ; 25 
	EIGHTY_LEN EQU 0x00
	EIGHTY_LEN1 EQU 0x3f
	EIGHTY_LEN2 EQU 0xf1 ; 26 
	NINETY_LEN EQU 0x00
	NINETY_LEN1 EQU 0x54
	NINETY_LEN2 EQU 0x24 ; 27 
	HUNDRED_LEN EQU 0x00
	HUNDRED_LEN1 EQU 0x43
	HUNDRED_LEN2 EQU 0x67 ; 28 
	POINT_LEN EQU 0x00
	POINT_LEN1 EQU 0x3c
	POINT_LEN2 EQU 0x5a ; 29 
	NANO_LEN EQU 0x00
	NANO_LEN1 EQU 0x3e
	NANO_LEN2 EQU 0xd7 ; 30 
	MICRO_LEN EQU 0x00
	MICRO_LEN1 EQU 0x4b
	MICRO_LEN2 EQU 0x8f ; 31 
	FARADS_LEN EQU 0x00
	FARADS_LEN1 EQU 0x3f
	FARADS_LEN2 EQU 0x80 ; 32 
	WATER_LEN EQU 0x00
	WATER_LEN1 EQU 0x38
	WATER_LEN2 EQU 0x34 ; 33 
	LEVEL_LEN EQU 0x00
	LEVEL_LEN1 EQU 0x47
	LEVEL_LEN2 EQU 0x0e ; 34 
	CENTI_LEN EQU 0x00
	CENTI_LEN1 EQU 0x4d
	CENTI_LEN2 EQU 0x80 ; 35 
	MILLI_LEN EQU 0x00
	MILLI_LEN1 EQU 0x38
	MILLI_LEN2 EQU 0x94 ; 36 
	METERS_LEN EQU 0x00
	METERS_LEN1 EQU 0x55
	METERS_LEN2 EQU 0xdc ; 37 
	PER_LEN EQU 0x00
	PER_LEN1 EQU 0x2f
	PER_LEN2 EQU 0xbc ; 38 
	CENT_LEN EQU 0x00
	CENT_LEN1 EQU 0x36
	CENT_LEN2 EQU 0x70 ; 39 
	CUP_LEN EQU 0x00
	CUP_LEN1 EQU 0x2d
	CUP_LEN2 EQU 0x3c ; 40 
	IS_LEN EQU 0x00
	IS_LEN1 EQU 0x23
	IS_LEN2 EQU 0xa6 ; 41 
	FULL_LEN EQU 0x00
	FULL_LEN1 EQU 0x2a
	FULL_LEN2 EQU 0xee ; 42 
	EMPTY_LEN EQU 0x00
	EMPTY_LEN1 EQU 0x46
	EMPTY_LEN2 EQU 0xeb ; 43 
	;bruh_LEN EQU 0x00187c ; 44 


FLASH_CE EQU P0.3
SPEAKER  EQU P1.3

AUTO            equ P2.1
ONREQ           equ P2.4
CAL_freq        equ P3.3
CAL_nF          equ P3.1
CAL_uF          equ P2.6

LCD_RS equ P2.0
LCD_RW equ P1.7
LCD_E  equ P1.6
LCD_D4 equ P1.1
LCD_D5 equ P1.0
LCD_D6 equ P0.7
LCD_D7 equ P0.6

; Commands supported by the SPI flash memory according to the datasheet
WRITE_ENABLE     EQU 0x06  ; Address:0 Dummy:0 Num:0
WRITE_DISABLE    EQU 0x04  ; Address:0 Dummy:0 Num:0
READ_STATUS      EQU 0x05  ; Address:0 Dummy:0 Num:1 to infinite
READ_BYTES       EQU 0x03  ; Address:3 Dummy:0 Num:1 to infinite
READ_SILICON_ID  EQU 0xab  ; Address:0 Dummy:3 Num:1 to infinite
FAST_READ        EQU 0x0b  ; Address:3 Dummy:1 Num:1 to infinite
WRITE_STATUS     EQU 0x01  ; Address:0 Dummy:0 Num:1
WRITE_BYTES      EQU 0x02  ; Address:3 Dummy:0 Num:1 to 256
ERASE_ALL        EQU 0xc7  ; Address:0 Dummy:0 Num:0
ERASE_BLOCK      EQU 0xd8  ; Address:3 Dummy:0 Num:0
READ_DEVICE_ID   EQU 0x9f  ; Address:0 Dummy:2 Num:1 to infinite

; Variables used in the program:
dseg at 30H
	w:   ds 3 ; 24-bit play counter.  Decremented in Timer 2 ISR.
	x:   ds 4
	y:   ds 4
	bcd: ds 5
    second_counter: ds 2
	
BSEG
mf: 	dbit 1
mode: 	dbit 1
sound_ready: dbit 1
one_second_passed: dbit 1
sound_playing: dbit 1

$NOLIST
$include(LCD_4bit_og.inc)
$include(math32.inc)
$LIST

cseg

Msgfreq:     db 'Frequency   (Hz)', 0
Msgpfn:      db 'Capacitance (pF)', 0
Msgpercent:  db 'Percentage   (%)', 0
WelcomeMsg1: db '     Welcome    ', 0
WelcomeMsg2: db 'Choose an Option', 0
WelcomeMsg3: db 'B1: Auto Recalc ', 0
WelcomeMsg4: db 'B2: Calc on Req ', 0
WelcomeMsg5: db 'B3: Frequency   ', 0
WelcomeMsg6: db 'B4: Cap in nF   ', 0
WelcomeMsg7: db 'B5: Cap in uF   ', 0
WelcomeMsg8: db 'B2 to view again', 0
Clear_Line:  db '                ', 0


play_sound mac
    push acc

    ; clr TR2 ; Stop Timer 2 ISR from playing previous request
    ; wait for previous sound to finish playing
    jnb sound_ready, $
	setb FLASH_CE
	
	clr FLASH_CE ; Enable SPI Flash
	mov a, #READ_BYTES
	lcall Send_SPI
	; Get the initial position in memory where to start playing
	mov a, %0 ; Address bits 16 to 23
	lcall Send_SPI
	mov a, %1 ; Address bits 8 to 15
	lcall Send_SPI
	mov a, %2 ; Address bits 0 to 7
	lcall Send_SPI
	; Get how many bytes to play
	mov a, %3
	mov w+2, a

	mov a, %4
	mov w+1, a

	mov a, %5
	mov w+0, a
	
	mov a, #0x00 ; Request first byte to send to DAC
	lcall Send_SPI
	
    setb SPEAKER
    clr sound_ready
    setb sound_playing
	;setb TR2 ; Start playback by enabling timer 2

    pop acc
    endmac

announce_percent mac
	push acc

	play_sound(#CUP, #CUP1, #CUP2, #CUP_LEN, #CUP_LEN1, #CUP_LEN2)
    play_sound(#IS, #IS1, #IS2, #IS_LEN, #IS_LEN1, #IS_LEN2)
    play_sound(%0, %1, %2, %3, %4, %5)
	play_sound(#PER, #PER1, #PER2, #PER_LEN, #PER_LEN1, #PER_LEN2)
	play_sound(#CENT, #CENT1, #CENT2, #CENT_LEN, #CENT_LEN1, #CENT_LEN2)
	play_sound(#FULL, #FULL1, #FULL2, #FULL_LEN, #FULL_LEN1, #FULL_LEN2)

    pop acc
    endmac

announce_percent_2 mac

	push acc

	play_sound(#CUP, #CUP1, #CUP2, #CUP_LEN, #CUP_LEN1, #CUP_LEN2)
    play_sound(#IS, #IS1, #IS2, #IS_LEN, #IS_LEN1, #IS_LEN2)
    play_sound(%0, %1, %2, %3, %4, %5)
	play_sound(#FIVE, #FIVE1, #FIVE2, #FIVE_LEN, #FIVE_LEN1, #FIVE_LEN2)
	play_sound(#PER, #PER1, #PER2, #PER_LEN, #PER_LEN1, #PER_LEN2)
	play_sound(#CENT, #CENT1, #CENT2, #CENT_LEN, #CENT_LEN1, #CENT_LEN2)
	play_sound(#FULL, #FULL1, #FULL2, #FULL_LEN, #FULL_LEN1, #FULL_LEN2)

    pop acc
    endmac

;-------------------------------------;
; ISR for Timer 2.  Used to playback  ;
; the WAV file stored in the SPI      ;
; flash memory.                       ;
;-------------------------------------;
Timer2_ISR:
	mov	SFRPAGE, #0x00
	clr	TF2H ; Clear Timer2 interrupt flag

	; The registers used in the ISR must be saved in the stack
	push acc
	push psw
	
	; Check if the play counter is zero.  If so, stop playing sound.

    inc second_counter+0    ; Increment the low 8-bits first
	mov a, second_counter+0 ; If the low 8-bits overflow, then increment high 8-bits
	jnz Inc_Done_1
	inc second_counter+1

Inc_Done_1:
	; Check if second has passed
	mov a, second_counter+0
	cjne a, #low(22050), Inc_Done ; Warning: this instruction changes the carry flag!
	mov a, second_counter+1
	cjne a, #high(22050), Inc_Done

    setb one_second_passed

    ;clr a 

    ;mov second_counter+0, a
    ;mov second_counter+1, a

Inc_Done:

    jnb sound_playing, Timer2_ISR_Done
	mov a, w+0
	orl a, w+1
	orl a, w+2
	jz stop_playing
	
	; Decrement play counter 'w'.  In this implementation 'w' is a 24-bit counter.
	mov a, #0xff
	dec w+0
	cjne a, w+0, keep_playing
	dec w+1
	cjne a, w+1, keep_playing
	dec w+2
	
keep_playing:

	setb SPEAKER
	lcall Send_SPI ; Read the next byte from the SPI Flash...
	
	; It gets a bit complicated here because we read 8 bits from the flash but we need to write 12 bits to DAC:
	mov SFRPAGE, #0x30 ; DAC registers are in page 0x30
	push acc ; Save the value we got from flash
	swap a
	anl a, #0xf0
	mov DAC0L, a
	pop acc
	swap a
	anl a, #0x0f
	mov DAC0H, a
	mov SFRPAGE, #0x00
	
	sjmp Timer2_ISR_Done

stop_playing:
	;clr TR2 ; Stop timer 2
	setb FLASH_CE  ; Disable SPI Flash
	clr SPEAKER ; Turn off speaker.  Removes hissing noise when not playing sound.
    clr sound_playing
    setb sound_ready

Timer2_ISR_Done:	
	pop psw
	pop acc
	reti

;---------------------------------;
; Sends a byte via serial port    ;
;---------------------------------;
putchar:
	jbc	TI,putchar_L1
	sjmp putchar
putchar_L1:
	mov	SBUF,a
	ret

;---------------------------------;
; Receive a byte from serial port ;
;---------------------------------;
getchar:
	jbc	RI,getchar_L1
	sjmp getchar
getchar_L1:
	mov	a,SBUF
	ret

;---------------------------------;
; Sends AND receives a byte via   ;
; SPI.                            ;
;---------------------------------;
Send_SPI:
	mov	SPI0DAT, a
Send_SPI_L1:
	jnb	SPIF, Send_SPI_L1 ; Wait for SPI transfer complete
	clr SPIF ; Clear SPI complete flag 
	mov	a, SPI0DAT
	ret

;---------------------------------;
; SPI flash 'write enable'        ;
; instruction.                    ;
;---------------------------------;
Enable_Write:
	clr FLASH_CE
	mov a, #WRITE_ENABLE
	lcall Send_SPI
	setb FLASH_CE
	ret

;---------------------------------;
; This function checks the 'write ;
; in progress' bit of the SPI     ;
; flash memory.                   ;
;---------------------------------;
Check_WIP:
	clr FLASH_CE
	mov a, #READ_STATUS
	lcall Send_SPI
	mov a, #0x55
	lcall Send_SPI
	setb FLASH_CE
	jb acc.0, Check_WIP ;  Check the Write in Progress bit
	ret
	
Init_all:
	; Disable WDT:
	mov	WDTCN, #0xDE
	mov	WDTCN, #0xAD
	
	mov	VDM0CN, #0x80
	mov	RSTSRC, #0x06
	
	; Switch SYSCLK to 72 MHz.  First switch to 24MHz:
	mov	SFRPAGE, #0x10
	mov	PFE0CN, #0x20
	mov	SFRPAGE, #0x00
	mov	CLKSEL, #0x00
	mov	CLKSEL, #0x00 ; Second write to CLKSEL is required according to datasheet
	
	; Wait for clock to settle at 24 MHz by checking the most significant bit of CLKSEL:
Init_L1:
	mov	a, CLKSEL
	jnb	acc.7, Init_L1
	
	; Now switch to 72MHz:
	mov	CLKSEL, #0x03
	mov	CLKSEL, #0x03  ; Second write to CLKSEL is required according to datasheet
	
	;Initializes timer/counter 0 as a 16-bit counter
    clr TR0 ; Stop timer 0


	; Wait for clock to settle at 72 MHz by checking the most significant bit of CLKSEL:
Init_L2:
	mov	a, CLKSEL
	jnb	acc.7, Init_L2

	mov	SFRPAGE, #0x00
	
	; Configure P3.0 as analog output.  P3.0 pin is the output of DAC0.
	anl	P3MDIN, #0xFE
	orl	P3, #0x01
	
	; Configure the pins used for SPI (P0.0 to P0.3)
	mov	P0MDOUT, #0x1D ; SCK, MOSI, P0.3, TX0 are push-pull, all others open-drain

	mov	XBR0, #0x03 ; Enable SPI and UART0: SPI0E=1, URT0E=1
	mov	XBR1, #0x10
	mov	XBR2, #0x40 ; Enable crossbar and weak pull-ups

	orl P0SKIP, #0B_1100_1000
	orl P1SKIP, #0B_0000_0011

	lcall LCD_4BIT

	; Enable serial communication and set up baud rate using timer 1
	mov	SCON0, #0x10	
	mov	TH1, #(0x100-((SYSCLK/BAUDRATE)/(12*2)))
	mov	TL1, TH1
	anl	TMOD, #0x0F ; Clear the bits of timer 1 in TMOD
	orl	TMOD, #0x25 ; Set timer 1 in 8-bit auto-reload mode.  Don't change the bits of timer 0
	setb TR1 ; START Timer 1
	setb TI ; Indicate TX0 ready
	
	; Configure DAC 0
	mov	SFRPAGE, #0x30 ; To access DAC 0 we use register page 0x30
	mov	DACGCF0, #0b_1000_1000 ; 1:D23REFSL(VCC) 1:D3AMEN(NORMAL) 2:D3SRC(DAC3H:DAC3L) 1:D01REFSL(VCC) 1:D1AMEN(NORMAL) 1:D1SRC(DAC1H:DAC1L)
	mov	DACGCF1, #0b_0000_0000
	mov	DACGCF2, #0b_0010_0010 ; Reference buffer gain 1/3 for all channels
	mov	DAC0CF0, #0b_1000_0000 ; Enable DAC 0
	mov	DAC0CF1, #0b_0000_0010 ; DAC gain is 3.  Therefore the overall gain is 1.
	; Initial value of DAC 0 is mid scale:
	mov	DAC0L, #0x00
	mov	DAC0H, #0x08
	mov	SFRPAGE, #0x00
	
	; Configure SPI
	mov	SPI0CKR, #((SYSCLK/(2*F_SCK_MAX))-1)
	mov	SPI0CFG, #0b_0100_0000 ; SPI in master mode
	mov	SPI0CN0, #0b_0000_0001 ; SPI enabled and in three wire mode
	setb FLASH_CE ; CS=1 for SPI flash memory
	clr SPEAKER ; Turn off speaker.
	
	; Configure Timer 2 and its interrupt
	mov	TMR2CN0,#0x00 ; Stop Timer2; Clear TF2
	orl	CKCON0,#0b_0001_0000 ; Timer 2 uses the system clock
	; Initialize reload value:
	mov	TMR2RLL, #low(TIMER2_RELOAD)
	mov	TMR2RLH, #high(TIMER2_RELOAD)
	; Set timer to reload immediately
	mov	TMR2H,#0xFF
	mov	TMR2L,#0xFF
	setb ET2 ; Enable Timer 2 interrupts
	; setb TR2 ; Timer 2 is only enabled to play stored sound
	
	setb EA ; Enable interrupts

	; Configure LCD and display initial message
    
	
	ret

;---------------------------------;
; Main program. Includes hardware ;
; initialization and 'forever'    ;
; loop.                           ;
;---------------------------------;
MainProgram:
    lcall Init_all ; Initialize the hardware
	mov SP, #0x7f ; Setup stack pointer to the start of indirectly accessable data memory minus one

cap_loop:
	clr mode 
	Send_Constant_String_L1(#WelcomeMsg1)
    ;WaitSec(#1)
    Send_Constant_String_L2(#WelcomeMsg2)
    ;WaitSec(#2)
show_again:
    Send_Constant_String_L1(#WelcomeMsg3)
    Send_Constant_String_L2(#WelcomeMsg4)
    ;WaitSec(#2)
    Send_Constant_String_L1(#WelcomeMsg5)
    Send_Constant_String_L2(#WelcomeMsg6)
    ;WaitSec(#2)
    Send_Constant_String_L1(#WelcomeMsg7)
    Send_Constant_String_L2(#WelcomeMsg8)

    setb TR2
    mov a, #0x00
    ;lcall determine_digit
    ljmp calc_percent

forever2:
    sjmp forever2
    ; wait_for_response(show_again)

timer_count:
    push acc
    ; Measure the frequency applied to pin T0 (T0 is routed to pin P0.0 using the 'crossbar')
    clr TR0 ; Stop counter 0
    mov TL0, #0
    mov TH0, #0
    clr a
    mov second_counter+0, a
    mov second_counter+1, a
    setb TR0 ; Start counter 0
    clr one_second_passed
    jnb one_second_passed, $
    clr TR0 ; Stop counter 0, TH0-TL0 has the frequency
    pop acc
    ret

percentage_loop:
    lcall Wait_one_second
    Send_Constant_String_L1(#Msgpfn)
    Send_Constant_String_L2(#Clear_Line)
    lcall timer_count
    Load_x(1000000000)
    mov y+0, TL0
    mov y+1, TH0
    mov y+2, #0
    mov y+3, #0
    lcall div32
    Load_y(144) ; left shift by 2 decimals
    lcall mul32
    ; 1.44 / (Ra+2Rb)*C
    ; Ra = 9845, Rb = 9840+9830
    Load_y(492) ; left shift by 2 decimals
    lcall div32
    lcall hex2bcd
    lcall Display_formated_BCD_2
    ; will format to have to have percent digits in upper 4 bits of bcd+4
    wait_for_response(calc_percent)


FREQ:
    Send_Constant_String_L1(#Msgfreq)
    Send_Constant_String_L2(#Clear_Line)
    lcall timer_count
    Set_Cursor(2, 1)
    lcall hex2bcd_5
    lcall DisplayBCD_5
    wait_for_response(FREQ)
	
calc_percent:
    Send_Constant_String_L1(#Msgpercent)
    Send_Constant_String_L2(#Clear_Line)
    lcall timer_count
    Load_x(1000000000)
    mov y+0, TL0
    mov y+1, TH0
    mov y+2, #0
    mov y+3, #0
    lcall div32
    Load_y(144000) ; left shift by 2 decimals
    lcall mul32
    ; 1.44 / (Ra+2Rb)*C
    ; Ra = 9860, Rb = 9860
    Load_y(492) ; left shift by 2 decimals
    lcall div32
    
	; 1.8909*x - 1061.3

    mov helper_1+0, x+0
    mov helper_1+1, x+1
    mov helper_1+2, x+2
    mov helper_1+3, x+3

	Load_y(18909)
	lcall mul32

	Load_y(10000)
	lcall div32

	Load_y(1061)
	lcall sub32

	Load_y(25)
	lcall div32
	Load_y(10)
	lcall mul32
	
    lcall hex2bcd
    lcall Display_unformated_BCD
    wait_for_response(calc_percent)

Cap_uF:
    Send_Constant_String_L1(#Msgpfn)
    Send_Constant_String_L2(#Clear_Line)
    lcall timer_count
    Load_x(1000000000) ; 
    mov y+0, TL0
    mov y+1, TH0
    mov y+2, #0
    mov y+3, #0
    lcall div32
    Load_y(144000)
    lcall mul32
    Load_y(296)
    lcall div32
    lcall hex2bcd
    lcall Display_unformated_BCD
    wait_for_response(Cap_uF)

determine_digit:
    push acc
    cjne a, #0x00, skip_empty
    sjmp play_empty
   
skip_empty:
	ljmp compare_5
    
play_empty:
    play_sound(#CUP, #CUP1, #CUP2, #CUP_LEN, #CUP_LEN1, #CUP_LEN2)
    play_sound(#IS, #IS1, #IS2, #IS_LEN, #IS_LEN1, #IS_LEN2)
    play_sound(#EMPTY, #EMPTY1, #EMPTY2, #EMPTY_LEN, #EMPTY_LEN1, #EMPTY_LEN2)

compare_5:
    cjne a, #0x05, skip_5
	sjmp play_5
   
skip_5:
	ljmp compare_10

play_5:
	announce_percent(#FIVE, #FIVE1, #FIVE2, #FIVE_LEN, #FIVE_LEN1, #FIVE_LEN2)


compare_10:
    cjne a, #0x00, skip_10
	sjmp play_10
   
skip_10:
	ljmp compare_15

play_10:
	announce_percent(#TEN, #TEN1, #TEN2, #TEN_LEN, #TEN_LEN1, #TEN_LEN2)


compare_15:
    cjne a, #0x00, skip_15
	sjmp play_15
   
skip_15:
	ljmp compare_20

play_15:
	announce_percent(#FIFTEEN, #FIFTEEN1, #FIFTEEN2, #FIFTEEN_LEN, #FIFTEEN_LEN1, #FIFTEEN_LEN2)


compare_20:
    cjne a, #0x20, skip_20
	sjmp play_20
   
skip_20:
	ljmp compare_25

play_20:
	announce_percent(#TWENTY, #TWENTY1, #TWENTY2, #TWENTY_LEN, #TWENTY_LEN1, #TWENTY_LEN2)


compare_25:
    cjne a, #0x25, skip_25
	sjmp play_25
   
skip_25:
	ljmp compare_30

play_25:
	announce_percent_2(#TWENTY, #TWENTY1, #TWENTY2, #TWENTY_LEN, #TWENTY_LEN1, #TWENTY_LEN2)


compare_30:
    cjne a, #0x30, skip_30
	sjmp play_30
   
skip_30:
	ljmp compare_35

play_30:
	announce_percent(#THIRTY, #THIRTY1, #THIRTY2, #THIRTY_LEN, #THIRTY_LEN1, #THIRTY_LEN2)


compare_35:
    cjne a, #0x35, skip_35
	sjmp play_35
   
skip_35:
	ljmp compare_40

play_35:
	announce_percent_2(#THIRTY, #THIRTY1, #THIRTY2, #THIRTY_LEN, #THIRTY_LEN1, #THIRTY_LEN2)


compare_40:
    cjne a, #0x40, skip_40
	sjmp play_40
   
skip_40:
	ljmp compare_45

play_40:
	announce_percent(#FORTY, #FORTY1, #FORTY2, #FORTY_LEN, #FORTY_LEN1, #FORTY_LEN2)


compare_45:
    cjne a, #0x45, skip_45
	sjmp play_45
   
skip_45:
	ljmp compare_50

play_45:
	announce_percent_2(#FORTY, #FORTY1, #FORTY2, #FORTY_LEN, #FORTY_LEN1, #FORTY_LEN2)


compare_50:
    cjne a, #0x50, skip_50
	sjmp play_50
   
skip_50:
	ljmp compare_55

play_50:
	announce_percent(#FIFTY, #FIFTY1, #FIFTY2, #FIFTY_LEN, #FIFTY_LEN1, #FIFTY_LEN2)


compare_55:
    cjne a, #0x55, skip_55
	sjmp play_55
   
skip_55:
	ljmp compare_60

play_55:
	announce_percent_2(#FIFTY, #FIFTY1, #FIFTY2, #FIFTY_LEN, #FIFTY_LEN1, #FIFTY_LEN2)


compare_60:
    cjne a, #0x60, skip_60
	sjmp play_60
   
skip_60:
	ljmp compare_65

play_60:
	announce_percent(#SIXTY, #SIXTY1, #SIXTY2, #SIXTY_LEN, #SIXTY_LEN1, #SIXTY_LEN2)


compare_65:
    cjne a, #0x65, skip_65
	sjmp play_65
   
skip_65:
	ljmp compare_70

play_65:
	announce_percent_2(#SIXTY, #SIXTY1, #SIXTY2, #SIXTY_LEN, #SIXTY_LEN1, #SIXTY_LEN2)


compare_70:
    cjne a, #0x70, skip_70
	sjmp play_70
   
skip_70:
	ljmp compare_75

play_70:
	announce_percent(#SEVENTY, #SEVENTY1, #SEVENTY2, #SEVENTY_LEN, #SEVENTY_LEN1, #SEVENTY_LEN2)


compare_75:
    cjne a, #0x75, skip_75
	sjmp play_75
   
skip_75:
	ljmp compare_80

play_75:
	announce_percent_2(#SEVENTY, #SEVENTY1, #SEVENTY2, #SEVENTY_LEN, #SEVENTY_LEN1, #SEVENTY_LEN2)


compare_80:
    cjne a, #0x80, skip_80
	sjmp play_80
   
skip_80:
	ljmp compare_85

play_80:
	announce_percent(#EIGHTY, #EIGHTY1, #EIGHTY2, #EIGHTY_LEN, #EIGHTY_LEN1, #EIGHTY_LEN2)


compare_85:
    cjne a, #0x85, skip_85
	sjmp play_85
   
skip_85:
	ljmp compare_90

play_85:
	announce_percent_2(#EIGHTY, #EIGHTY1, #EIGHTY2, #EIGHTY_LEN, #EIGHTY_LEN1, #EIGHTY_LEN2)


compare_90:
    cjne a, #0x90, skip_90
	sjmp play_90
   
skip_90:
	ljmp compare_95

play_90:
	announce_percent(#NINETY, #NINETY1, #NINETY2, #NINETY_LEN, #NINETY_LEN1, #NINETY_LEN2)


compare_95:
    cjne a, #0x95, skip_95
	sjmp play_95
   
skip_95:
	ljmp compare_full

play_95:
	announce_percent_2(#NINETY, #NINETY1, #NINETY2, #NINETY_LEN, #NINETY_LEN1, #NINETY_LEN2)


compare_full:
    cjne a, #00, skip_full
	sjmp play_full
   
skip_full:
	ljmp return_determine

play_full:
	play_sound(#CUP, #CUP1, #CUP2, #CUP_LEN, #CUP_LEN1, #CUP_LEN2)
    play_sound(#IS, #IS1, #IS2, #IS_LEN, #IS_LEN1, #IS_LEN2)
    play_sound(#FULL, #FULL1, #FULL2, #FULL_LEN, #FULL_LEN1, #FULL_LEN2)

return_determine:
	pop acc
	ret




END

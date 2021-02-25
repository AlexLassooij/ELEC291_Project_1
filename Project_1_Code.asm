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
    ZERO EQU 0x00004d ; 0 
    ONE EQU 0x003a02 ; 1 
    TWO EQU 0x0076d4 ; 2 
    THREE EQU 0x0097b9 ; 3 
    FOUR EQU 0x00ca2d ; 4 
    FIVE EQU 0x0107b5 ; 5 
    SIX EQU 0x0132dc ; 6 
    SEVEN EQU 0x0169c5 ; 7 
    EIGHT EQU 0x01a8e3 ; 8 
    NINE EQU 0x01cbd1 ; 9 
    TEN EQU 0x0206cd ; 10 
    ELEVEN EQU 0x023fee ; 11 
    TWELVE EQU 0x028551 ; 12 
    THIRTEEN EQU 0x02caf3 ; 13 
    FOURTEEN EQU 0x030e3c ; 14 
    FIFTEEN EQU 0x035b4b ; 15 
    SIXTEEN EQU 0x03a7b5 ; 16 
    SEVENTEEN EQU 0x03f68e ; 17 
    EIGHTEEEN EQU 0x044e98 ; 18 
    NINETEEN EQU 0x048c64 ; 19 
    TWENTY EQU 0x04d678 ; 20 
    THIRTY EQU 0x052067 ; 21 
    FORTY EQU 0x0566c7 ; 22 
    FIFTY EQU 0x05b6da ; 23 
    SIXTY EQU 0x060487 ; 24 
    SEVENTY EQU 0x065a38 ; 25 
    EIGHTY EQU 0x06b50b ; 26 
    NINETY EQU 0x06f4fc ; 27 
    HUNDRED EQU 0x074920 ; 28 
    POINT EQU 0x078c87 ; 29 
    NANO EQU 0x07c8e1 ; 30 
    MICRO EQU 0x0807b8 ; 31 
    FARADS EQU 0x085347 ; 32 
    WATER EQU 0x0892c7 ; 33 
    LEVEL EQU 0x08cafb ; 34 
    CENTI EQU 0x091209 ; 35 
    MILLI EQU 0x095f89 ; 36 
    METERS EQU 0x09981d ; 37 
    PER EQU 0x09edf9 ; 38 
    CENT EQU 0x0a1db5 ; 39 
    CUP EQU 0x0a
    CUP1 EQU 0X54
    CUP2 EQU 0X25 ; 40 
    IS EQU 0x0a
    IS1 EQU 0x81
    IS2 EQU 0x61 ; 41 
    FULL EQU 0x0aa507 ; 42 
    EMPTY EQU 0x0a
    EMPTY1 EQU 0xcf
    EMPTY2 EQU 0xf5 ; 43  
    ;bruh1 EQU 0x0b16e0 ; 44 
    ;bruh2 EQU 0x0b2f5c :


; Size of each sound in 'sound_index'
Size_sound:
    ZERO_LEN EQU 0x0039b5 ; 0 
    ONE_LEN EQU 0x003cd2 ; 1 
    TWO_LEN EQU 0x0020e5 ; 2 
    THREE_LEN EQU 0x003274 ; 3 
    FOUR_LEN EQU 0x003d88 ; 4 
    FIVE_LEN EQU 0x002b27 ; 5 
    SIX_LEN EQU 0x0036e9 ; 6 
    SEVEN_LEN EQU 0x003f1e ; 7 
    EIGHT_LEN EQU 0x0022ee ; 8 
    NINE_LEN EQU 0x003afc ; 9 
    TEN_LEN EQU 0x003921 ; 10 
    ELEVEN_LEN EQU 0x004563 ; 11 
    TWELVE_LEN EQU 0x0045a2 ; 12 
    THIRTEEN_LEN EQU 0x004349 ; 13 
    FOURTEEN_LEN EQU 0x004d0f ; 14 
    FIFTEEN_LEN EQU 0x004c6a ; 15 
    SIXTEEN_LEN EQU 0x004ed9 ; 16 
    SEVENTEEN_LEN EQU 0x00580a ; 17 
    EIGHTEEN_LEN EQU 0x003dcc ; 18 
    NINETEEN_LEN EQU 0x004a14 ; 19 
    TWENTY_LEN EQU 0x0049ef ; 20 
    THIRTY_LEN EQU 0x004660 ; 21 
    FORTY_LEN EQU 0x005013 ; 22 
    FIFTY_LEN EQU 0x004dad ; 23 
    SIXTY_LEN EQU 0x0055b1 ; 24 
    SEVENTY_LEN EQU 0x005ad3 ; 25 
    EIGHTY_LEN EQU 0x003ff1 ; 26 
    NINETY_LEN EQU 0x005424 ; 27 
    HUNDRED_LEN EQU 0x004367 ; 28 
    POINT_LEN EQU 0x003c5a ; 29 
    NANO_LEN EQU 0x003ed7 ; 30 
    MICRO_LEN EQU 0x004b8f ; 31 
    FARADS_LEN EQU 0x003f80 ; 32 
    WATER_LEN EQU 0x003834 ; 33 
    LEVEL_LEN EQU 0x00470e ; 34 
    CENTI_LEN EQU 0x004d80 ; 35 
    MILLI_LEN EQU 0x003894 ; 36 
    METERS_LEN EQU 0x0055dc ; 37 
    PER_LEN EQU 0x002fbc ; 38 
    CENT_LEN EQU 0x003670 ; 39 
    CUP_LEN EQU 0x00
    CUP_LEN1 EQU 0X2d
    CUP_LEN2 EQU 0x3c ; 40 
    IS_LEN EQU 0x00
    IS_LEN1 EQU 0x23
    IS_LEN2 EQU 0xa6 ; 41 
    FULL_LEN EQU 0x002aee ; 42 
    EMPTY_LEN EQU 0x00
    EMPTY_LEN1 EQU 0x46
    EMPTY_LEN2 EQU 0xeb ; 43 
    ;bruh_LEN EQU 0x00187c ; 44 

FLASH_CE EQU P0.3
SPEAKER  EQU P2.5

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
Msgpfn:     db 'Capacitance (pF)', 0
Msgcapu:     db 'Capacitance (uF)', 0
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

	
forever_loop:
	ljmp cap_loop
	;RI: Receive interrupt flag, set to 1 when the receive FIFO contains data
	jb RI, serial_get
	jb P3.7, forever_loop ; Check if push-button pressed
	jnb P3.7, $ ; Wait for push-button release
	; Play the whole memory
	;clr TR2 ; Stop Timer 2 ISR from playing previous request
	setb FLASH_CE
	clr SPEAKER ; Turn off speaker.
	
	clr FLASH_CE ; Enable SPI Flash
	mov a, #READ_BYTES
	lcall Send_SPI
	; Set the initial position in memory where to start playing
	mov a, #0x00
	lcall Send_SPI
	mov a, #0x00
	lcall Send_SPI
	mov a, #0x00
	lcall Send_SPI
	mov a, #0x00 ; Request first byte to send to DAC
	lcall Send_SPI
	
	; How many bytes to play? All of them!  Asume 4Mbytes memory: 0x3fffff
	mov w+2, #0x3f
	mov w+1, #0xff
	mov w+0, #0xff
	
	setb SPEAKER ; Turn on speaker.
	;setb TR2 ; Start playback by enabling Timer 2
	ljmp forever_loop
	
serial_get:
	lcall getchar ; Wait for data to arrive
	cjne a, #'#', forever_loop ; Message format is #n[data] where 'n' is '0' to '9'
	;clr TR2 ; Stop Timer 2 from playing previous request
	setb FLASH_CE ; Disable SPI Flash	
	clr SPEAKER ; Turn off speaker.
	lcall getchar

;---------------------------------------------------------	
	cjne a, #'0' , Command_0_skip
Command_0_start: ; Identify command
	clr FLASH_CE ; Enable SPI Flash	
	mov a, #READ_DEVICE_ID
	lcall Send_SPI	
	mov a, #0x55
	lcall Send_SPI
	lcall putchar
	mov a, #0x55
	lcall Send_SPI
	lcall putchar
	mov a, #0x55
	lcall Send_SPI
	lcall putchar
	setb FLASH_CE ; Disable SPI Flash
	ljmp forever_loop	
Command_0_skip:

;---------------------------------------------------------	
	cjne a, #'1' , Command_1_skip 
Command_1_start: ; Erase whole flash (takes a long time)
	lcall Enable_Write
	clr FLASH_CE
	mov a, #ERASE_ALL
	lcall Send_SPI
	setb FLASH_CE
	lcall Check_WIP
	mov a, #0x01 ; Send 'I am done' reply
	lcall putchar		
	ljmp forever_loop	
Command_1_skip:

;---------------------------------------------------------	
	cjne a, #'2' , Command_2_skip 
Command_2_start: ; Load flash page (256 bytes or less)
	lcall Enable_Write
	clr FLASH_CE
	mov a, #WRITE_BYTES
	lcall Send_SPI
	lcall getchar ; Address bits 16 to 23
	lcall Send_SPI
	lcall getchar ; Address bits 8 to 15
	lcall Send_SPI
	lcall getchar ; Address bits 0 to 7
	lcall Send_SPI
	lcall getchar ; Number of bytes to write (0 means 256 bytes)
	mov r0, a
Command_2_loop:
	lcall getchar
	lcall Send_SPI
	djnz r0, Command_2_loop
	setb FLASH_CE
	lcall Check_WIP
	mov a, #0x01 ; Send 'I am done' reply
	lcall putchar		
	ljmp forever_loop	
Command_2_skip:

;---------------------------------------------------------	
	cjne a, #'3' , Command_3_skip 
Command_3_start: ; Read flash bytes (256 bytes or less)
	clr FLASH_CE
	mov a, #READ_BYTES
	lcall Send_SPI
	lcall getchar ; Address bits 16 to 23
	lcall Send_SPI
	lcall getchar ; Address bits 8 to 15
	lcall Send_SPI
	lcall getchar ; Address bits 0 to 7
	lcall Send_SPI
	lcall getchar ; Number of bytes to read and send back (0 means 256 bytes)
	mov r0, a

Command_3_loop:
	mov a, #0x55
	lcall Send_SPI
	lcall putchar
	djnz r0, Command_3_loop
	setb FLASH_CE	
	ljmp forever_loop	
Command_3_skip:

;---------------------------------------------------------	
	cjne a, #'4' , Command_4_skip 
Command_4_start: ; Playback a portion of the stored wav file
	;clr TR2 ; Stop Timer 2 ISR from playing previous request
	setb FLASH_CE
	
	clr FLASH_CE ; Enable SPI Flash
	mov a, #READ_BYTES
	lcall Send_SPI
	; Get the initial position in memory where to start playing
	lcall getchar
	lcall Send_SPI
	lcall getchar
	lcall Send_SPI
	lcall getchar
	lcall Send_SPI
	; Get how many bytes to play
	lcall getchar
	mov w+2, a
	lcall getchar
	mov w+1, a
	lcall getchar
	mov w+0, a
	
	mov a, #0x00 ; Request first byte to send to DAC
	lcall Send_SPI
	
	;setb TR2 ; Start playback by enabling timer 2
	ljmp forever_loop	
Command_4_skip:

;---------------------------------------------------------	
	cjne a, #'5' , Command_5_skip 
Command_5_start: ; Calculate and send CRC-16 of ISP flash memory from zero to the 24-bit passed value.
	; Get how many bytes to use to calculate the CRC.  Store in [r5,r4,r3]
	lcall getchar
	mov r5, a
	lcall getchar
	mov r4, a
	lcall getchar
	mov r3, a
	
	; Since we are using the 'djnz' instruction to check, we need to add one to each byte of the counter.
	; A side effect is that the down counter becomes efectively a 23-bit counter, but that is ok
	; because the max size of the 25Q32 SPI flash memory is 400000H.
	inc r3
	inc r4
	inc r5
	
	; Initial CRC must be zero.
	mov	SFRPAGE, #0x20 ; UART0, CRC, and SPI can work on this page
	mov	CRC0CN0, #0b_0000_1000 ; // Initialize hardware CRC result to zero;

	clr FLASH_CE
	mov a, #READ_BYTES
	lcall Send_SPI
	clr a ; Address bits 16 to 23
	lcall Send_SPI
	clr a ; Address bits 8 to 15
	lcall Send_SPI
	clr a ; Address bits 0 to 7
	lcall Send_SPI
	mov	SPI0DAT, a ; Request first byte from SPI flash
	sjmp Command_5_loop_start

Command_5_loop:
	jnb SPIF, Command_5_loop 	; Check SPI Transfer Completion Flag
	clr SPIF				    ; Clear SPI Transfer Completion Flag	
	mov a, SPI0DAT				; Save received SPI byte to accumulator
	mov SPI0DAT, a				; Request next byte from SPI flash; while it arrives we calculate the CRC:
	mov	CRC0IN, a               ; Feed new byte to hardware CRC calculator

Command_5_loop_start:
	; Drecrement counter:
	djnz r3, Command_5_loop
	djnz r4, Command_5_loop
	djnz r5, Command_5_loop
Command_5_loop2:	
	jnb SPIF, Command_5_loop2 	; Check SPI Transfer Completion Flag
	clr SPIF			    	; Clear SPI Transfer Completion Flag
	mov a, SPI0DAT	            ; This dummy read is needed otherwise next transfer fails (why?)
	setb FLASH_CE 				; Done reading from SPI flash
	
	; Computation of CRC is complete.  Send 16-bit result using the serial port
	mov	CRC0CN0, #0x01 ; Set bit to read hardware CRC high byte
	mov	a, CRC0DAT
	lcall putchar

	mov	CRC0CN0, #0x00 ; Clear bit to read hardware CRC low byte
	mov	a, CRC0DAT
	lcall putchar
	
	mov	SFRPAGE, #0x00

	ljmp forever_loop	
Command_5_skip:

;---------------------------------------------------------	
	cjne a, #'6' , Command_6_skip 
Command_6_start: ; Fill flash page (256 bytes)
	lcall Enable_Write
	clr FLASH_CE
	mov a, #WRITE_BYTES
	lcall Send_SPI
	lcall getchar ; Address bits 16 to 23
	lcall Send_SPI
	lcall getchar ; Address bits 8 to 15
	lcall Send_SPI
	lcall getchar ; Address bits 0 to 7
	lcall Send_SPI
	lcall getchar ; Byte to write
	mov r1, a
	mov r0, #0 ; 256 bytes
Command_6_loop:
	mov a, r1
	lcall Send_SPI
	djnz r0, Command_6_loop
	setb FLASH_CE
	lcall Check_WIP
	mov a, #0x01 ; Send 'I am done' reply
	lcall putchar		
	ljmp forever_loop	
Command_6_skip:

	ljmp forever_loop


cap_loop:
	clr mode 
	Send_Constant_String_L1(#WelcomeMsg1)
    WaitSec(#1)
    Send_Constant_String_L2(#WelcomeMsg2)
    WaitSec(#2)
show_again:
    Send_Constant_String_L1(#WelcomeMsg3)
    Send_Constant_String_L2(#WelcomeMsg4)
    WaitSec(#2)
    Send_Constant_String_L1(#WelcomeMsg5)
    Send_Constant_String_L2(#WelcomeMsg6)
    WaitSec(#2)
    Send_Constant_String_L1(#WelcomeMsg7)
    Send_Constant_String_L2(#WelcomeMsg8)

    setb TR2
    mov a, #0x00
    lcall determine_digit
    ;ljmp FREQ

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
    wait_for_response(Cap_nF)


FREQ:
    Send_Constant_String_L1(#Msgfreq)
    Send_Constant_String_L2(#Clear_Line)
    lcall timer_count
    Set_Cursor(2, 1)
    lcall hex2bcd_5
    lcall DisplayBCD_5
    wait_for_response(FREQ)
	
Cap_nF:
    Send_Constant_String_L1(#Msgpfn)
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
    lcall hex2bcd
    lcall Display_formated_BCD_2
    wait_for_response(Cap_nF)

Cap_uF:
    Send_Constant_String_L1(#Msgcapu)
    Send_Constant_String_L2(#Clear_Line)
    lcall timer_count
    Load_x(1000000) ; 
    mov y+0, TL0
    mov y+1, TH0
    mov y+2, #0
    mov y+3, #0
    lcall div32
    Load_y(144)
    lcall mul32
    Load_y(296)
    lcall div32
    lcall hex2bcd
    lcall Display_formated_BCD_2
    wait_for_response(Cap_uF)

determine_digit:
    push acc
    cjne a, #0x00, skip_1
    sjmp play_1
   
skip_1:
	ljmp compare_1
    
play_1:
    play_sound(#CUP, #CUP1, #CUP2, #CUP_LEN, #CUP_LEN1, #CUP_LEN2)
    play_sound(#IS, #IS1, #IS2, #IS_LEN, #IS_LEN1, #IS_LEN2)
    play_sound(#EMPTY, #EMPTY1, #EMPTY2, #EMPTY_LEN, #EMPTY_LEN1, #EMPTY_LEN2)

compare_1:
    cjne a, #0x01, compare_2
    ;play_sound(#CUP, #CUP_LEN)
    ;play_sound(#IS, #IS_LEN)
    ;play_sound(#ONE, #ONE_LEN)
    ;play_sound(#PER, #PER_LEN)
    ;play_sound(#CENT, #CENT_LEN)
    ;play_sound(#FULL, #FULL_LEN)

compare_2:
    pop acc
    ret





END

; Lab_3_38574752.asm: Uses 555 timer and math library built-ins to calculate capacitance

$NOLIST
$MODEFM8LB1
$LIST

org 0000H
   ljmp MyProgram

; These register definitions needed by 'math32.inc'
DSEG at 30H
x:   ds 4
y:   ds 4
bcd: ds 5

BSEG

mf: dbit 1

; These 'equ' must match the hardware wiring
; They are used by 'LCD_4bit.inc'
LCD_RS equ P2.0
LCD_RW equ P1.7
LCD_E  equ P1.6
LCD_D4 equ P1.1
LCD_D5 equ P1.0
LCD_D6 equ P0.7
LCD_D7 equ P0.6


$NOLIST
$include(LCD_4bit_og.inc)
$include(math32.inc)
$LIST

RECALC          equ P2.1
CAL_freq        equ P3.3
CAL_nF          equ P3.1
CAL_uF          equ P2.3

CSEG

Msgfreq:     db 'Frequency   (Hz)', 0
Msgcapn:     db 'Capacitance (nF)', 0
Msgcapu:     db 'Capacitance (uF)', 0
WelcomeMsg1: db '     Welcome    ', 0
WelcomeMsg2: db 'Choose an Option', 0
WelcomeMsg3: db 'B1: Frequency   ', 0
WelcomeMsg4: db 'B2: Cap in nF   ', 0
WelcomeMsg5: db 'B3: Cap in uF   ', 0
WelcomeMsg6: db 'B4: Recalculate ', 0
Clear_Line:  db '                ', 0

;Converts the hex number in TH0-TL0 to packed BCD in R2-R1-R0
    
MyProgram:
	mov sp, #0x7F ; Initialize the stack pointer
    
    ; DISABLE WDT: provide Watchdog disable keys
	mov	WDTCN,#0xDE ; First key
	mov	WDTCN,#0xAD ; Second key

    ; Enable crossbar and weak pull-ups
	mov	XBR0,#0x00
	mov	XBR1,#0x10 ; Enable T0 on P0.0.  T0 is the external clock input to Timer/Counter 0
	mov	XBR2,#0x40

	; Switch clock to 24.5 MHz
	mov	CLKSEL, #0x00 ; 
	mov	CLKSEL, #0x00 ; Second write to CLKSEL is required according to the user manual (page 77)
	
	; Wait for the 24.5 MHz oscillator to stabilze by checking bit DIVRDY in CLKSEL
waitclockstable:
	mov a, CLKSEL
	jnb acc.7, waitclockstable
	
	;Initializes timer/counter 0 as a 16-bit counter
    clr TR0 ; Stop timer 0
    mov a, TMOD
    anl a, #0b_1111_0000 ; Clear the bits of timer/counter 0
    orl a, #0b_0000_0101 ; Sets the bits of timer/counter 0 for a 16-bit counter
    mov TMOD, a

	; Configure LCD and display initial message
    lcall LCD_4BIT
	
main_loop:

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

    wait_for_response(show_again)

timer_count:
    ; Measure the frequency applied to pin T0 (T0 is routed to pin P0.0 using the 'crossbar')
    clr TR0 ; Stop counter 0
    mov TL0, #0
    mov TH0, #0
    setb TR0 ; Start counter 0
    lcall Wait_one_second
    clr TR0 ; Stop counter 0, TH0-TL0 has the frequency

    ret

FREQ:
    Send_Constant_String_L1(#Msgfreq)
    Send_Constant_String_L2(#Clear_Line)
    lcall timer_count
    Set_Cursor(2, 1)
    lcall hex2bcd_5
    lcall DisplayBCD_5
    wait_for_response(FREQ)
	
Cap_nF:
    Send_Constant_String_L1(#Msgcapn)
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
    ; Ra = 9860, Rb = 9860
    Load_y(296) ; left shift by 2 decimals
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
	
END

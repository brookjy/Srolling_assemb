#define LCD_LIBONLY
.include "lcd.asm"




.cseg

 ;initialize the lcd
 ;clear the lcd
 ;copy the strings from program to data memory
 ;set l1ptr and l2ptr to point at the start of the display strings
 ;do forever:
 ;clear the lcd
 ;display line1 and line2
 ;move the pointers forward (wrap around when appropriate)
 ;copy from the pointers in msg1 and msg2 to line1 and line2
 ;delay
				
	  
      call lcd_init	          
      call lcd_clr             
      call init_strings 
	  call setpointer1
	  call setpointer2
loop: 
      
      call lcd_clr
	  call display_strings
	  call move
	  call copy
	  call copy2
	  call delay
	  call check_button
	  cpi r24,0    ;no button
	  breq loop
	  cpi r24,1   ;right button	 
	  breq loop2 
	  cpi r24,2    ;up button
	  breq stop
      cpi r24,8    ;left button
	  breq slow
	  cpi r24,16   ;select button
      
	  jmp loop
loop2:
	  call lcd_clr
	  call display_strings
	  call move
	  call copy
	  call copy2
	  call delay1
	  call check_button
	  cpi r24,0    ;no button
	  breq loop
	  cpi r24,1   ;right button	 
	  breq loop2 
	  cpi r24,2    ;up button
	  breq stop
      cpi r24,8    ;left button
	  breq slow
	  ;cpi r24,16   ;select button
	  jmp loop
	
stop:
      call check_button
	  cpi r24,4     ;down button
      breq loop
	  rjmp stop
slow:
   	 call delay
	 call delay
	 jmp loop

lp:	jmp lp

setpointer1:	
      push YH
	  push YL
	  push ZH
	  push ZL
	  push r19
	  push r20
	  ldi YH, high(line1)
	  ldi YL, low(line1)

      ldi ZH, high(l1ptr)
	  ldi ZL, low(l1ptr)
	
	  ldi r19, high(msg1)
	  ldi r20, low(msg1)
	  
	  st Z+, r20
	  st Z, r19
	  sbiw ZH:ZL,1
	  pop r20
	  pop r19
	  pop ZL
	  pop ZH
	  pop YL
	  pop YH
      ret
setpointer2:
      push YH
	  push YL
	  push ZH
	  push ZL
	  push r19
	  push r20
	  ldi YH, high(line2)
	  ldi YL, low(line2)

      ldi ZH, high(l2ptr)
	  ldi ZL, low(l2ptr)
	
	  ldi r19, high(msg2)
	  ldi r20, low(msg2)
	  
	  st Z+, r20
	  st Z, r19
	  sbiw ZH:ZL,1
	  pop r20
	  pop r19
	  pop ZL
	  pop ZH
	  pop YL
	  pop YH
      ret

move:       ; move pointer that pointer to the next letter and store the loacation
      push r16        ;LINE1
 	  push ZL
 	  push ZH
 	  push XL
 	  push XH

 	  ldi XL,low(l1ptr)
 	  ldi XH, high(l1ptr)
 	  ld ZL,X+
 	  ld ZH,X

	  adiw ZH:ZL,1
 	  ld r16,Z
 	  cpi r16,0x00
 	  brne yes1
 	  ldi ZL, low(msg1)
 	  ldi ZH, high(msg1)
yes1: 
 	  ldi XL,low(l1ptr)
 	  ldi XH, high(l1ptr)
	  st X+, ZL
 	  st X,ZH
 	  
 	  pop XH
 	  pop XL
 	  pop ZH
 	  pop ZL
 	  pop r16



move2:    ; move pointer that pointer to the next letter and store the loacation   
      push r16          ;LINE2
 	  push ZL
 	  push ZH
 	  push XL
 	  push XH

 	  ldi XL,low(l2ptr)
 	  ldi XH, high(l2ptr)
 	  ld ZL,X+
 	  ld ZH,X

	  adiw ZH:ZL,1
 	  ld r16,Z
 	  cpi r16,0x00
 	  brne yes2
 	  ldi ZL, low(msg2)
 	  ldi ZH, high(msg2)
yes2:
 	  ldi XL,low(l2ptr)
 	  ldi XH, high(l2ptr)
 	  st X+, ZL
 	  st X,ZH
 	  pop XH
 	  pop XL
 	  pop ZH
 	  pop ZL
 	  pop r16
      ret
    

copy:
      push r21
      push r23
      push XH
      push XL
      push YL
      push YH
      push ZH
      push ZL


      clr r21
      clr r23
      ldi XH, high(l1ptr)
      ldi XL, low(l1ptr)

      ld YL,X+
      ld YH,X
      sbiw XH:XL,1

      ldi ZH, high(line1)
      ldi ZL, low(line1)

copying1:

      ld r21, Y+
	  cpi r21,0
	  breq end1
      st Z+, r21
      ;adiw YH:YL,1
      ;adiw ZH:ZL,1
      inc r23
      cpi r23,16
      breq eq1
      rjmp copying1
end1:
	  ldi YH,high(msg1)
	  ldi YL,low(msg1)
	  jmp copying1
eq1:
      pop ZL
      pop ZH
      pop YH
      pop YL
      pop XL
      pop XH
      pop r23
      pop r21
      ret	
	

copy2:
      push r21
      push r23
      push XH
      push XL
      push YL
      push YH
      push ZH
      push ZL


      clr r21
      clr r23
      ldi XH, high(l2ptr)
      ldi XL, low(l2ptr)

      ld YL,X+
      ld YH,X
      sbiw XH:XL,1

      ldi ZH, high(line2)
      ldi ZL, low(line2)

copying2:

      ld r21, Y+
	  cpi r21,0
	  breq some2
      st Z+, r21
      ;adiw YH:YL,1
      ;adiw ZH:ZL,1
      inc r23
      cpi r23,16
      breq eq2
      rjmp copying2
some2:
	  ldi YH,high(msg2)
	  ldi YL,low(msg2)
	  jmp copying2
eq2:
      pop ZL
      pop ZH
      pop YH
      pop YL
      pop XL
      pop XH
      pop r23
      pop r21
      ret	
		
	 
delay:	
del1:		nop
		ldi r21,0x40	
del2:		nop
		ldi r22, 0x40
del3:		nop
		dec r22
		brne del3
		dec r21
		brne del2
		dec r20
		brne del1	
		ret
delay1:	
del4:		nop
		ldi r21,0x0F	
del5:		nop
		ldi r22, 0x0F
del6:		nop
		dec r22
		brne del6
		dec r21
		brne del5
		dec r20
		brne del4	
		ret
                              


init_strings:
	push r16
	; copy strings from program memory to data memory
	ldi r16, high(msg1)		; this the destination
	push r16
	ldi r16, low(msg1)
	push r16
	ldi r16, high(msg1_p << 1) ; this is the source
	push r16
	ldi r16, low(msg1_p << 1)
	push r16
	call str_init			; copy from program to data
	pop r16					; remove the parameters from the stack
	pop r16
	pop r16
	pop r16

	ldi r16, high(msg2)
	push r16
	ldi r16, low(msg2)
	push r16
	ldi r16, high(msg2_p << 1)
	push r16
	ldi r16, low(msg2_p << 1)
	push r16
	call str_init
	pop r16
	pop r16
	pop r16
	pop r16

	pop r16
	ret

display_strings:

	; This subroutine sets the position the next
	; character will be output on the lcd
	;
	; The first parameter pushed on the stack is the Y position
	; 
	; The second parameter pushed on the stack is the X position
	; 
	; This call moves the cursor to the top left (ie. 0,0)

	push r16

	call lcd_clr

	ldi r16, 0x00
	push r16
	ldi r16, 0x00
	push r16
	call lcd_gotoxy
	pop r16
	pop r16

	; Now display msg1 on the first line
	ldi r16, high(line1)
	push r16
	ldi r16, low(line1)
	push r16
	call lcd_puts
	pop r16
	pop r16

	; Now move the cursor to the second line (ie. 0,1)
	ldi r16, 0x01
	push r16
	ldi r16, 0x00
	push r16
	call lcd_gotoxy
	pop r16
	pop r16

	; Now display msg1 on the second line
	ldi r16, high(line2)
	push r16
	ldi r16, low(line2)
	push r16
	call lcd_puts
	pop r16
	pop r16

	pop r16
	ret

;
; An improved version of the button test subroutine
;
; Returns in r24:
;	0 - no button pressed
;	1 - right button pressed
;	2 - up button pressed
;	4 - down button pressed
;	8 - left button pressed
;	16- select button pressed
;
; this function uses registers:
;	r16
;	r17
;	r24
;
; if you consider the word:
;	 value = (ADCH << 8) +  ADCL
; then:
;
; value > 0x3E8 - no button pressed
;
; Otherwise:
; value < 0x032 - right button pressed
; value < 0x0C3 - up button pressed
; value < 0x17C - down button pressed
; value < 0x22B - left button pressed
; value < 0x316 - select button pressed
; 
check_button:
		push r16
		push r17
		
		; initialize the Analog to Digital conversion

		ldi r16, 0x87
		sts ADCSRA, r16
		ldi r16, 0x40
		sts ADMUX, r16

		; initialize PORTB and PORTL for ouput
		ldi	r16, 0xFF
		sts DDRB,r16
		sts DDRL,r16
		; start a2d
		lds	r16, ADCSRA	
		ori r16, 0x40
		sts	ADCSRA, r16

		; wait for it to complete
wait:	lds r16, ADCSRA
		andi r16, 0x40
		brne wait

		; read the value
		lds r16, ADCL
		lds r17, ADCH

		clr r24
		cpi r17, 3			;  if > 0x3E8, no button pressed 
		brne bsk1		    ;  
		cpi r16, 0xE8		; 
		brsh bsk_done		; 
bsk1:	tst r17				; if ADCH is 0, might be right or up  
		brne bsk2			; 
		cpi r16, 0x32		; < 0x32 is right
		brsh bsk3
		ldi r24, 0x01		; right button
		rjmp bsk_done
bsk3:	cpi r16, 0xC3		
		brsh bsk4	
		ldi r24, 0x02		; up			
		rjmp bsk_done
bsk4:	ldi r24, 0x04		; down (can happen in two tests)
		rjmp bsk_done
bsk2:	cpi r17, 0x01		; could be up,down, left or select
		brne bsk5
		cpi r16, 0x7c		; 
		brsh bsk7
		ldi r24, 0x04		; other possiblity for down
		rjmp bsk_done
bsk7:	ldi r24, 0x08		; left
		rjmp bsk_done
bsk5:	cpi r17, 0x02
		brne bsk6
		cpi r16, 0x2b
		brsh bsk6
		ldi r24, 0x08
		rjmp bsk_done
bsk6:	ldi r24, 0x10
bsk_done:
		
		pop r17
		pop r16
		ret

; in the program memory 
msg1_p:	.db "This is the first message displayed on the first line of the LCD.", 0	
msg2_p: .db "On the second line of the LCD there is another message that is scrolled.", 0

.dseg
;
; The program copies the strings from program memory
; into data memory.  These are the strings
; that are actually displayed on the lcd
;
msg1:	.byte 200
msg2:	.byte 200

line1:  .byte 17
line2:  .byte 17

l1ptr:  .byte 2
l2ptr:  .byte 2




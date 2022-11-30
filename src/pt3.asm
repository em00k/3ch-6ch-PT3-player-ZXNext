; 	PT3Driver v2 for NextZXOS written by em00k 2/4/2022
; 	based on driver template by Garry Lancaster
;	some snippets from Remy Sharps AYFX driver  

	DEVICE ZXSPECTRUMNEXT
	ORG $0000

	macro BREAK 
		dw $01dd				; CSpect macro for break point
	endm


MMU5_A000_NR_55		equ $55
MMU6_C000_NR_56		equ $56
MMU7_E000_NR_57		equ $57
PT3_ADDRESS			equ $c000

PT3_INIT			equ $a003
PT3_PLAY			equ $a005
PT3_MUTE			equ $a008
; PT3_MUTE			equ $a008

; Status 
ST_NOT_PLAYING			equ 0 
ST_INIT_TUNE			equ 1
ST_PLAYING				equ 2 
ST_STOP 				equ 3 
NEXTREG_PORT_243B		equ $243b
AY_CHIP_INFO_BFF5		equ $BFF5
AY_CHIP_SELECT_FFFD		equ $FFFD
PER_4_REGISTER_09		equ $09 

; **************************************************************
; * API
; The documentation that comes with the driver describes how to use it. Some
; drivers may make use of the new DRIVER command. This has the following form:
; DRIVER driverid,callid[,n1[,n2]] [TO var1[,var2[,var3]]]
; where n1 and n2 are optional values to pass to the driver, and var1, var2 and
; var3 are optional variables to receive results from the driver call. The
; documentation for each driver will describe the individual DRIVER commands that
; you can use.
; 
; id : 
; - 1: set bank id for player & tune   							DRIVER 127,1,n,n
; - 2: music control 											DRIVER 127,2,n
; 

; **************************************************************

api_entry:
	jr		pt3_api
	nop

; At $0003 is the entry point for the interrupt handler (which is why there's a
; `nop` above). This is called because bit 7 of the driver id byte has been set
; in the .DRV file.

im1_entry:
; if the user has not setup player / tune banks then we skip out of the im 
;

reloc_ir_1:												; pt3_banks_imr
	ld		hl,pt3_banks
	ld		a,(hl)										; read pt3_bank value
	or		a									
	jr		z, im1_skip									; it was zero so skip im
	inc		hl											
	ld		a,(hl)										; read pt3_bank+1 (tune banks)
	or		a
	jr		z, im1_skip									; it was zero so skip im 

	push	hl,bc,de,ix,iy 								; push all registers 
	exx
	push	hl,de,bc
	ex		af, af'
	push	af

reloc_ir_2:												; call backup banks routines	

	call	activate_user_bank							
														; and set banks for player 
reloc_ir_3:
	ld		a,(pt3_status)								; 0 not playing, 1 init, 2 playing, 3 mute and stop
	or		a
	jr		z, skipplayer

	cp		ST_PLAYING									; are we playing?
	jr		z, playtune 									

	cp		ST_INIT_TUNE								; init tune? 
	jr		z, init_tune

	; then we must be stopping 
	
	call 	PT3_MUTE					
	xor 	a
reloc_ir_4:												; pt3_status 2
	ld		(pt3_status),a 
	jr 		skipplayer									; now lets exit 

init_tune: 
	; init player 
	
	ld		hl,PT3_ADDRESS								; point to tune 
	;ld 		a,0
	call 	PT3_INIT									; init tune 

	ld 		a,ST_PLAYING 								; set staus to playing
reloc_ir_5:	
	ld		(pt3_status),a 				
playtune: 				
	ld		bc,AY_CHIP_INFO_BFF5						; we want to read the current
	in		a,(c)										; selected AY config 
	push 	af  										; save on the stack



	call 	PT3_PLAY									; Play a frame of music 

	pop		af 											; get the previous 
	ld		bc,AY_CHIP_INFO_BFF5
	out		(c),a 

skipplayer:
reloc_ir_6:
	call	restore_bank								; restore banks 
	pop		af 
	ex		af, af' 
	pop		bc,de,hl
	exx 
	pop		iy,ix,de,bc,hl								; restore regs 
im1_skip:
	ret

; ***************************************************************************
; * PT3 driver API                                                      *
; ***************************************************************************
; On entry, use B=call id with HL,DE other parameters.
; (NOTE: HL will contain the value that was either provided in HL (when called
;        from dot commands) or IX (when called from a standard program).
;
; When called from the DRIVER command, DE is the first input and HL is the second.
;
; When returning values, the DRIVER command will place the contents of BC into
; the first return variable, then DE and then HL.

pt3_api:
	djnz	bnot1										; On if B<>1

; **************************************************************
; * B=1 and DE=pt3 player bank, hl=pt3 tune bank
; **************************************************************

callId1_set_playerbanks:

reloc_c1_1: 
	call	backup_bank
	ld		a,e											; user bank is in DE and set in A and as
	add		a,a											; NextBASIC banks are in 16k: double it
	ld		e,l											; put tune bank into e from l as hl is used

reloc_c1_2:
	ld		hl, pt3_banks								; point to start of pt3_banks
	ld		(hl), a	 									; save pt3 player bank
	nextreg MMU5_A000_NR_55, a							; set mmu 5 to pt3_banks

	ld		a,e 										; get tune bank 
	add		a,a 										; * 2 for 16kb banks 
	inc		hl											; save in pt3_banks+1
	ld		(hl), a	 									; 
	nextreg MMU6_C000_NR_56, a							; set mmu 6 to frist 8kb

	inc 	a											; next bank along 
	inc		hl											; store in pt3_banks+2
	ld		(hl), a	 									; 
	nextreg MMU7_E000_NR_57, a							; set mmu7 with second bank

	push	hl,bc,de,ix,iy 								; push all registers 
	exx
	push 	hl,de,bc
	ex		af, af'
	push 	af 

	ld		hl,PT3_ADDRESS								; point to start of tune
	ld 		a,32
	call	PT3_INIT									; init the tune in the player

	pop 	af 
	ex		af, af' 
	pop		bc,de,hl
	exx 
	pop		iy,ix,de,bc,hl 								; restore regs 

reloc_c1_3:
	call	restore_bank								; recover OS banks
	and     a                       					; clear carry to indicate success
	ret

bnot1:
	djnz    bnot2                   					; On if B<>2

; **************************************************************
; * B=2 and DE= control for music
; 0 stopped 
; 1 initialise 
; 2 playing 
; 3 mute/puse
; hl = chip select
; **************************************************************

callId2_music_control:
	;BREAK
reloc_c2_1:

	call 	activate_user_bank
	ld		a,e											; E = effect id
	and		%111										; 

reloc_c2_2:
	ld		(pt3_status),a

	ld		a,l											; get ay select 
	dec		a											; take 1 off as user will input 1-3
	and		%11											; only want 0-2
	ld		l,a											; 
	ld		a,$ff 										; start with 255 AY1
	sub 	l 											; sub ay select 

reloc_c2_3:
	ld		(ay_chip_select),a							; save ay select 

reloc_c2_4:
	call	restore_bank
	and     a											; clear carry to indicate success
	ret

bnot2:
	djnz	bnot3 

display_driver_version:

reloc_c3_0:
	ld		hl,version

print_loop:
	ld		a,(hl)
	jr		z,finished_print
	;rst		16
	inc 	hl
	jr		print_loop	

finished_print:
	and     a											; clear carry to indicate success
	ld		bc,201
	ret

bnot3:

api_error:
	xor	a												; A=0, unsupported call id
	scf													; Fc=1, signals error
	ret


; **************************************************************
; Bank routines				
; 
; **************************************************************
backup_bank:
	push	hl,de 
	ld 		bc,NEXTREG_PORT_243B				; Next Register port 
	in		a,(c)								; read current reg value 
reloc_br_0:
	ld		(saved_reg), a						; save this 

	; back up MMU5
	ld 		a, MMU5_A000_NR_55					; back up MMU5 
	out 	(c),a
	inc 	b									; 
	in 		a,(c)
reloc_br_1:
	ld		hl, stored_banks
	ld 		(hl),a 								; save MMU 5 in stored_banks 
	inc 	hl

	; backup MMU 6 
	ld 		a, MMU6_C000_NR_56					; back up MMU6 
	dec		b 									; bc = NEXTREG_PORT_243B
	out 	(c),a								; set port 
	inc 	b									; bc = 253B read 
	in 		a,(c)								; read MMU6 value 
	ld		(hl),a								; store MMU6 stored_banks+1
	inc		hl									; 
	
	; backup MMU 7
	ld 		a, MMU7_E000_NR_57					; back up MMU7 
	dec		b 
	out 	(c),a
	inc 	b
	in 		a,(c)								; read MMU7 value
	ld		(hl),a								; store MMU7 bank at stored_banks+2

	ld 		bc,NEXTREG_PORT_243B				
	ld		a,2

reloc_br_1_1:	
	ld		a,(ay_chip_select)
	ld		bc,AY_CHIP_SELECT_FFFD
	out		(c),a 

saved_reg equ $-1
	out		(c),a								; just in case IRQ was in between registry work

	pop 	de,hl
	ret

activate_user_bank:
reloc_br_2:
	call	backup_bank
reloc_br_3:
	ld		hl, pt3_banks							; point hl to pt3_banks
	ld		a, (hl)									; get the player bank 
	nextreg	MMU5_A000_NR_55, a						; set it to MMu5 $a000

	inc		hl										; move to next stored bank 
	ld		a, (hl)									; read tune first bank 
	nextreg	MMU6_C000_NR_56, a						; set to mmu6 $c000 

	inc 	a 										; second 8kb bank will always be 
	nextreg	MMU7_E000_NR_57, a						; tunebank + 1 so we can just inc a and set to mmu7
	ret

restore_bank:
reloc_br_4:
	ld		hl, stored_banks						; point hl to start of stored banks 
	ld 		a, (hl)
	nextreg MMU5_A000_NR_55, a						; restore mmu5 

	inc 	hl 
	ld 		a, (hl)
	nextreg MMU6_C000_NR_56, a						; mmu6
	
	inc 	hl 
	ld		a, (hl)
	nextreg MMU7_E000_NR_57, a						; mmu7 

	ret

pt3_banks:
		defb	0,0,0 								; first bank pt3 player, second + third tune banks
ay_chip_select:										; 0-2 AY chip
		defb	0
pt3_status:
		defb	0 									; holds 0,1,2,3 for ST_STOP, ST_INIT, ST_PLAYING, ST_MUTE+PAUSE
stored_banks:
		defb	0,0,0
	
version:
		db 		"pt3driver v2.1 4/3/22",0

; drivers must be 512 bytes in total excluding relocation tables

	IF $ > 512
		DISPLAY "Driver code exceeds 512 bytes"
		LUA ALLPASS 
		;	 sj.exit(1000)
		ENDLUA
		
	ELSE
		defs    512-$

	ENDIF

reloc_start:
		
	defw	reloc_ir_1+2				; ld a,(pt3_bank)
	defw	reloc_ir_2+2				; user bank 
	defw	reloc_ir_3+2
	defw	reloc_ir_4+2
	defw	reloc_ir_5+2
	defw	reloc_ir_6+2
	defw	reloc_c1_1+2
	defw	reloc_c1_2+2
	defw	reloc_c1_3+2
	defw	reloc_c2_1+2
	defw	reloc_c2_2+2
	defw	reloc_c2_3+2
	defw	reloc_c2_4+2
	defw	reloc_c3_0+2
	defw	reloc_br_0+2
	defw	reloc_br_1+2
	defw	reloc_br_1_1+2
	defw	reloc_br_2+2
	defw	reloc_br_3+2
	defw	reloc_br_4+2
	
reloc_end:

		
		SAVEBIN "pt3.bin",api_entry,reloc_end-api_entry

		; now assemble the main pt3_drv.asm 
		; 
		SHELLEXEC "Z:/zxenv/emulator/sjasmplus.exe src/pt3_drv.asm"


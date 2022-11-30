
    MODULE  interrupts 

;------------------------------------------------------------------------------
; Interrupt table

ISR_TABLE_ADDRESS       equ $FE00                               ; address of our vector table

setup_isr:
        ; set up 257 bytes table
        ld      hl, ISR_TABLE_ADDRESS                           ; we *dont* really need a 257 byte vector, but cspect jumps to XX00 instead of $XXFE, so 
        ld      de, ISR                                         ; we patch both start and end. 
        ld      a, h
        ld      i, a 
        im      2                                               ; enable IM2 
        ld      b, l                                            ; we will do 256+1 loops 
        dec     hl 

.im_setup:
        ld      (hl), a 
        inc     hl 
        djnz    .im_setup

        dec     hl 
        ld      (hl), e                                         ; de contains our jump address for out im routine 
        inc     hl 
        ld      (hl), d 
        ld      hl,ISR_TABLE_ADDRESS                            ; patch the start for cspect 
        ld      (hl), e
        inc     hl 
        ld      (hl), d 
        ei 
        ret 

ISR:
        ; we need to preserve all registers when the interrupt kicks in 
        push    af
        push    bc 
        push    hl
        push    de 
        push    ix 
        push    iy 
        exx 
        push    hl 
        push    de 
        push    bc 
        ex      af, af' 
        push    af 
            
        ; this is where your interrupt call would be 
        call    backup_bank                     ; set our banks 
        call    do_stuff_interrupt              ; call do_stuff_interrupt
        call    restore_bank                    ; restore banks 

        pop     af 
        ex      af, af' 
        pop     bc 
        pop     de 
        pop     hl 
        exx     
        pop     iy 
        pop     ix 
        pop     de 
        pop     hl 
        pop     bc 
        pop     af 
        ei 
        ret                                             ; ret from interrupt, if we want ROM calls we can change this to JP 56

backup_bank:

        ld 	bc,TBBLUE_REGISTER_SELECT_P_243B        ; Next Register port so we can return the port to last used port 
        in	a,(c)								; 
        ld	(backup_port), a		        ; save this 


        ld	hl, stored_banks                        ; point hl to our bank buffer 
        ld      b, 3                                    ; 3 loops 
        ld      d, MMU0_0000_NR_50                      ; point d to first reg for MMU 0 = $50 

backup_bank_loop: 

        push    bc 
        ld      bc, TBBLUE_REGISTER_SELECT_P_243B
        ld 	a, d                                    ; back up MMU0 , MMU1 , MMU2 
        out 	(c),a
        inc 	b
        in 	a,(c)

        ld 	(hl),a 	
        inc 	hl
        inc     d 
        pop     bc 
        djnz    backup_bank_loop
        
        ret 

restore_bank:

        ld	hl, stored_banks                    ; point hl to our bank buffer 

        ld      a, (hl)
        nextreg $50, a 
        
        inc     hl 
        ld      a, (hl)
        nextreg $51, a 
        
        inc     hl 
        ld      a, (hl)
        nextreg $52, a 
            
        ld      a, (backup_port)                    ; restore port 
        ld      bc, TBBLUE_REGISTER_SELECT_P_243B
        out     (c), a
        ret    

backup_port:

        db  0 

stored_banks:

        db 0,0,0

    ENDMODULE 
; example for playing 3/6channel PT3 files from banks for the ZX Next 
; em00k 30/11/2022
; uses sjasmplus - plays a tune, press a key to play another tune 
; 

        SLDOPT COMMENT WPMEM, LOGPOINT, ASSERTION
        DEVICE ZXSPECTRUMNEXT

        CSPECTMAP "interrupts.map"

        include "defines.asm"

        org     $8000                                      ; build our code to $8000

        ; some macros to make starting and stopping the tune easier to read in our code 

        MACRO         EnableMusic 
                ld              a, 1 
                ld              (player_enable), a 
        ENDM 

        MACRO         DisableMusic 
                xor             a 
                ld              (player_enable), a 
                call            init_music
        ENDM 

        MACRO         StartMusic N 
                ld              a, (music_bank_0+N)
                call            init_music
        ENDM 


; these are the call addresses into player.bin that do various tasks

PT3_INIT                equ $4003           ; initalise a tune, HL = address of PT3 file
PT3_PLAY                equ $4005           ; play a quark of music once per frame 
PT3_MUTE                equ $4008           ; mute the AY channels

main:

        
        call    interrupts.setup_isr            ; set up the interrupt routine 
        
play_loop: 

        StartMusic      $0                      ; start tune 0 
        EnableMusic                             ; enable the music 
        
        call    wait_key                        ; we keep looping until a key is pressed 

        StartMusic      $1                      ; now play tune 1 
        EnableMusic                             ; enable the music 

        call    wait_key                        ; wait for a key to be pressed 

        jr      play_loop                       ; jump back to the play_loop 


wait_key:
        ; this loops while waiting for a key and makes the border and screen flash 

        ld      a, r                            ; get a "random" value from R reg 
        and     7                               ; we only want 3 bits 
        out     ($fe), a                        ; out to the border 
        ld      bc, $fffd                       ; port $fffd 
        ld      a, 10                           ; AY channel volume C
        out     (c), a                          
        in      a, (c)                          ; get the amplitude

        ld      hl, 22528                       ; fille the screen with some attribute colour
        ld      de, 22529                       ; based on the AY channel C volume 
        sla     a                                           ; some shifts to make it into the PAPER bit range
        sla     a
        sla     a
        sla     a

        ld      (hl), a                         ; 
        ld      bc,768
        ldir                    

        xor     a                               ; scrub a and flags 
        in      a,($fe)                         ; read port $fe 
        cpl                                             
        and     15                              ; anything pressed on the keyboard
        jr      z,wait_key                      ; no then loop 

        
        
        ld      b,10                            ; a pause to fake debounce 
.wait_loop2:
        halt 
        djnz    .wait_loop2
        ret 


init_music:

        ld      e, a                            ; store the bank that has the music 
        call    interrupts.backup_bank          ; back up the banks 
        xor     a                               ; flatten a & flags 
        ld      (player_enable), a              ; set music to not be playing 
        ld      a, e                            ; get back bank with music in 
        ld      (current_music_bank), a         ; save current music bank
        nextreg $52,$32                         ; player.bin bank 
        nextreg $50,a                           ; page in the music banks to slots 0/1 
        inc     a
        nextreg $51,a
        ld      hl, 0                           ; HL = start address of music 
        ld      a, 32                           ; this is flags for the player 3 channel tune (it can do 6 channels)
        ld      ($400A), a                      ; set this into the player binary 
  
        call    PT3_INIT                        ; init the tune 
        call    PT3_MUTE                        ; call a mute 
        call    interrupts.restore_bank         ; restore the banks 
        ret 


do_stuff_interrupt:
        
        ;       bring in the player banks, they will be restored auto magically on interrupt exit 
        ld      a, (player_enable)              ; is player enable?
        or      a 
        ret     z                               ; no then return 



        nextreg $52, $32                        ; put player.bin into $4000 - $5FFF
        ld      a, (current_music_bank)         ; get the music bank we want 
        nextreg $50, a                          ; place the music into $0000 - $3FFF (so we can have 16k tunes!)
        inc     a
        nextreg $51, a 
        call    PT3_PLAY                        ; play a quark 
 
        ret 

;------------------------------------------------------------------------------
; Stack reservation
STACK_SIZE      equ     100

stack_bottom:
        defs    STACK_SIZE * 2
stack_top:
        defw    0

;------------------------------------------------------------------------------
; vairables

music_bank_0:
        db      $33 , $34                       ; two banks that contain two different tunes 

current_music_bank:
        db      0                               ; remember the current bank 

player_enable: 
        db      0                               ; flag to check if music should be playing 

;------------------------------------------------------------------------------
; Output configuration

        include "interrupts.asm"                ; contains the interrupt code 
        

;------------------------------------------------------------------------------
; NEX bank configuration

        mmu     7 n, $32                         ; include the player binary into bank 32
        org     $e000 
        incbin  "player.bin"

        mmu     7 n, $33                         ; music file 
        org     $e000 
        incbin  "music.pt3"

        mmu     7 n, $34                         ; music file 
        org     $e000 
        incbin  "music03.pt3"

;------------------------------------------------------------------------------
; Output configuration
        SAVENEX OPEN "interrupts.nex", main, stack_top 
        SAVENEX CORE 2,0,0
        SAVENEX CFG 7,0,0,0
        SAVENEX AUTO 
        SAVENEX CLOSE

; we assemble this to $0000 and create a binary file output, we only need to do this once
; this wil then create player.bin which we then include in our code with an 


	output player.bin 

	MODULE pt3 

	org 	$4000
;universal pt2'n'pt3 turbo sound player for zx spectrum
;(c)2004-2007 s.v.bulba <vorobey@mail.khstu.ru>
;specially for alco
;http://bulba.untergrund.net/ (http://bulba.at.kz/)

;release number
release equ "0"

;conditional assembly
;1) current position counters at (vars1+0) and (vars2+0)
curposcounter=0
;2) allow channels allocation bits at (start+10)
acbbac=0
;3) allow loop checking and disabling
loopchecker=1
;4) insert official identificator
id=1
;5) set iy for correct return to zx basic
basic=1

;features
;--------
;-can be compiled at any address (i.e. no need rounding org
; address).
;-variables (vars) can be located at any address (not only after
; code block).
;-init subprogram checks pt3-module version and rightly
; generates both note and volume tables outside of code block
; (in vars).
;-two portamento (spc. command 3xxx) algorithms (depending of
; pt3 module version).
;-new 1.xx and 2.xx special command behaviour (only for pt v3.7
; and higher).
;-any tempo value are accepted (including tempo=1 and tempo=2).
;-ts modes: 2xpt3, 2xpt2 and pt v3.7 ts standard.
;-fully compatible with ay_emul pt3 and pt2 players codes.
;-see also notes at the end of this source code.

;limitations
;-----------
;-can run in ram only (self-modified code is used).
;-pt2 position list must be end by #ff marker only.

;warning!!! play subprogram can crash if no module are loaded
;into ram or init subprogram was not called before.

;call mute or init one more time to mute sound after stopping
;playing 

	;org #c000

;test codes (commented)
;	ld a,32 ;singlept3(ts if tspt3.7),abc,looped
;	ld (start+10),a
;	ld hl,#8000 ;mod1
;	ld de,#a000 ;mod2 (optional)
;	call start+3
;	ei
;_lp	halt
;	call start+5
;	xor a
;	in a,(#fe)
;	cpl
;	and 15
;	jr z,_lp
;	jr start+8

tona	equ 0
tonb	equ 2
tonc	equ 4
noise	equ 6
mixer	equ 7
ampla	equ 8
amplb	equ 9
amplc	equ 10
env	equ 11
envtp	equ 13

;entry and other points
;start initialize playing of modules at mdladdr (single module)
;start+3 initialization with module address in hl and de (ts)
;start+5 play one quark
;start+8 mute
;start+10 setup and status flags

musicplay
	ld hl,mdladdr ;de - address of 2nd module for ts
	jr init
	jp play
	jr mute
setup	db 0 ;set bit0, if you want to play without looping
	     ;(optional);
	     ;set bit1 for pt2 and reset for pt3 before
	     ;calling init;
	     ;bits2-3: %00-abc, %01-acb, %10-bac (optional);
	     ;bits4-5: %00-no ts, %01-2 modules ts, %10-
	     ;autodetect pt3 ts-format by alco (pt 3.7+);
	     ;remark: old pt3 ts-format by alco (pt 3.6) is not
	     ;documented and must be converted to new standard.
	     ;bit6 is set each time, when loop point of 2nd ts
	     ;module is passed (optional).
	     ;bit7 is set each time, when loop point of 1st ts
	     ;or of single module is passed (optional).

;identifier
	if id
	db "=unipt2/pt3/ts-player r.",release,"="
	endif

	if loopchecker
checklp	ld hl,setup
	bit 0,(iy-100+vrs.modnum)
	jr z,chl1
	set 6,(hl)
	jr chl2
chl1	set 7,(hl)
chl2	bit 0,(hl)
	ret z
	pop hl
	inc (iy-100+vrs.delycnt)
	inc (iy-100+vrs.chana+chp.ntskcn)
	xor a
	ld (iy-100+vrs.ayregs+ampla),a
	ld (iy-100+vrs.ayregs+amplb),a
	ld (iy-100+vrs.ayregs+amplc),a
	ret
	endif

mute	xor a
	ld h,a
	ld l,a
	ld (vars1+vrs.ayregs+ampla),a
	ld (vars1+vrs.ayregs+amplb),hl
	ld (vars2+vrs.ayregs+ampla),a
	ld (vars2+vrs.ayregs+amplb),hl
	jp rout

init
;hl - addressofmodule
;de - addresof2ndmodule
	push de
	push hl
	ld hl,vars
	ld (hl),0
	ld de,vars+1
	ld bc,var0end-vars-1
	ldir
	inc hl
	ld (vars1+vrs.adinpta),hl ;ptr to zero
	ld (vars2+vrs.adinpta),hl

	pop hl
	ld iy,vars1+100
	ld a,(musicplay+10)
	and 2
	jp nz,i_pt2

	call initpt3
	ld hl,(e_-samcnv-2)*256+#18
	ld (samcnv),hl
	ld a,#ba
	ld (orncp),a
	ld (samcp),a
	ld a,#7b
	ld (ornld),a
	ld (samld),a
	ld a,#87
	ld (samclc2),a
	pop hl
	;use version and ton table of 1st module
	ld a,(ix+13-100) ;extract version number
	sub #30
	jr c,l20
	cp 10
	jr c,l21
l20	ld a,6
l21	ld (version),a
	push af ;voltable version
	cp 4
	ld a,(ix+99-100) ;tone table number
	rla
	and 7
	push af ;notetable number

	ld iy,vars2+100
	ld a,(musicplay+10)
	and 48
	jr z,nots
	cp 16
	jr z,twopt3s
	ld a,(version)
	cp 7
	jr c,nots
	ld a,(ix+98-100) ;alco ts marker
	cp #20
	jr z,nots
	ld hl,vars1
	ld de,vars2
	ld bc,vrs
	ldir
	set 1,(iy-100+vrs.modnum)
	ld c,a
	add a,a
	add a,c
	sub 2
	ld (tssub),a
	jr alcots_
twopt3s	call initpt3
alcots_	ld a,1
	ld (is_ts),a
	set 0,(iy-100+vrs.modnum)

nots	ld bc,pt3pd
	ld hl,0
	ld de,pt3emptyorn
	jr initcommon

i_pt2	call initpt2
	ld hl,#51cb
	ld (samcnv),hl
	ld a,#bb
	ld (orncp),a
	ld (samcp),a
	ld a,#7a
	ld (ornld),a
	ld (samld),a
	ld a,#80
	ld (samclc2),a
	pop hl
	ld a,5
	ld (version),a
	push af
	ld a,2
	push af

	ld a,(musicplay+10)
	and 48
	jr z,nots2

	ld iy,vars2+100
	ld a,1
	ld (is_ts),a
	set 0,(iy-100+vrs.modnum)
	call initpt2

nots2	ld bc,pt2pd
	ld hl,#8687
	ld de,pt2emptyorn

initcommon

	if basic
	ld iy,#5c3a
	endif

	ld (ptdec),bc
	ld (pscalc),hl
	push de

;note table data depacker
;(c) ivan roshin
	ld de,t_pack
	ld bc,t1_+(2*49)-1
tp_0	ld a,(de)
	inc de
	cp 15*2
	jr nc,tp_1
	ld h,a
	ld a,(de)
	ld l,a
	inc de
	jr tp_2
tp_1	push de
	ld d,0
	ld e,a
	add hl,de
	add hl,de
	pop de
tp_2	ld a,h
	ld (bc),a
	dec bc
	ld a,l
	ld (bc),a
	dec bc
	sub #f8*2
	jr nz,tp_0

	inc a
	ld (vars1+vrs.delycnt),a
	ld (vars2+vrs.delycnt),a
	ld hl,#f001 ;h - chp.volume, l - chp.ntskcn
	ld (vars1+vrs.chana+chp.ntskcn),hl
	ld (vars1+vrs.chanb+chp.ntskcn),hl
	ld (vars1+vrs.chanc+chp.ntskcn),hl
	ld (vars2+vrs.chana+chp.ntskcn),hl
	ld (vars2+vrs.chanb+chp.ntskcn),hl
	ld (vars2+vrs.chanc+chp.ntskcn),hl
	pop hl
	ld (vars1+vrs.chana+chp.ornptr),hl
	ld (vars1+vrs.chanb+chp.ornptr),hl
	ld (vars1+vrs.chanc+chp.ornptr),hl
	ld (vars2+vrs.chana+chp.ornptr),hl
	ld (vars2+vrs.chanb+chp.ornptr),hl
	ld (vars2+vrs.chanc+chp.ornptr),hl

	pop af

;notetablecreator (c) ivan roshin
;a - notetablenumber*2+versionfornotetable
;(xx1b - 3.xx..3.4r, xx0b - 3.4x..3.6x..vtii1.0)

	ld hl,nt_data
	ld d,0
	add a,a
	ld e,a
	add hl,de
	ld e,(hl)
	inc hl
	srl e
	sbc a,a
	and #a7 ;#00 (nop) or #a7 (and a)
	ld (l3),a
	ex de,hl
	ld bc,t1_
	add hl,bc

	ld a,(de)
	add a,t_
	ld c,a
	adc a,t_/256
	sub c
	ld b,a
	push bc
	ld de,nt_
	push de

	ld b,12
l1	push bc
	ld c,(hl)
	inc hl
	push hl
	ld b,(hl)

	push de
	ex de,hl
	ld de,23
	ld ixh,8

l2	srl b
	rr c
l3	db #19	;and a or nop
	ld a,c
	adc a,d	;=adc 0
	ld (hl),a
	inc hl
	ld a,b
	adc a,d
	ld (hl),a
	add hl,de
	dec ixh
	jr nz,l2

	pop de
	inc de
	inc de
	pop hl
	inc hl
	pop bc
	djnz l1

	pop hl
	pop de

	ld a,e
	cp tcold_1
	jr nz,corr_1
	ld a,#fd
	ld (nt_+#2e),a

corr_1	ld a,(de)
	and a
	jr z,tc_exit
	rra
	push af
	add a,a
	ld c,a
	add hl,bc
	pop af
	jr nc,corr_2
	dec (hl)
	dec (hl)
corr_2	inc (hl)
	and a
	sbc hl,bc
	inc de
	jr corr_1

tc_exit

	pop af

;voltablecreator (c) ivan roshin
;a - versionforvolumetable (0..4 - 3.xx..3.4x;
			   ;5.. - 2.x,3.5x..3.6x..vtii1.0)

	cp 5
	ld hl,#11
	ld d,h
	ld e,h
	ld a,#17
	jr nc,m1
	dec l
	ld e,l
	xor a
m1      ld (m2),a

	ld ix,vt_+16

	ld c,#f
initv2  push hl

	add hl,de
	ex de,hl
	sbc hl,hl

	ld b,#10
initv1  ld a,l
m2      db #7d
	ld a,h
	adc a,0
	ld (ix),a
	inc ix
	add hl,de
	djnz initv1

	pop hl
	ld a,e
	cp #77
	jr nz,m3
	inc e
m3      dec c
	jr nz,initv2

	jp rout

initpt3	call setmdad
	push hl
	ld de,100
	add hl,de
	ld a,(hl)
	ld (iy-100+vrs.delay),a
	push hl
	pop ix
	add hl,de
	call setcppt
	ld e,(ix+102-100)
	inc hl

	if curposcounter
	ld (iy-100+vrs.possub),l
	endif

	add hl,de
	call setlppt
	pop de
	ld l,(ix+103-100)
	ld h,(ix+104-100)
	add hl,de
	call setptpt
	ld hl,169
	add hl,de
	call setorpt
	ld hl,105
	add hl,de

setsmpt ld (iy-100+vrs.samptrs),l
	ld (iy-100+vrs.samptrs+1),h
	ret

initpt2	ld a,(hl)
	ld (iy-100+vrs.delay),a
	push hl
	push hl
	push hl
	inc hl
	inc hl
	ld a,(hl)
	inc hl
	call setsmpt
	ld e,(hl)
	inc hl
	ld d,(hl)
	pop hl
	and a
	sbc hl,de
	call setmdad
	pop hl
	ld de,67
	add hl,de
	call setorpt
	ld e,32
	add hl,de
	ld c,(hl)
	inc hl
	ld b,(hl)
	ld e,30
	add hl,de
	call setcppt
	ld e,a
	inc hl

	if curposcounter
	ld (iy-100+vrs.possub),l
	endif

	add hl,de
	call setlppt
	pop hl
	add hl,bc

setptpt	ld (iy-100+vrs.patsptr),l
	ld (iy-100+vrs.patsptr+1),h
	ret

setmdad	ld (iy-100+vrs.modaddr),l
	ld (iy-100+vrs.modaddr+1),h
	ret

setorpt	ld (iy-100+vrs.ornptrs),l
	ld (iy-100+vrs.ornptrs+1),h
	ret

setcppt	ld (iy-100+vrs.crpsptr),l
	ld (iy-100+vrs.crpsptr+1),h
	ret

setlppt	ld (iy-100+vrs.lposptr),l
	ld (iy-100+vrs.lposptr+1),h
	ret

setenbs	ld (iy-100+vrs.envbase),l
	ld (iy-100+vrs.envbase+1),h
	ret

setesld	ld (iy-100+vrs.curesld),l
	ld (iy-100+vrs.curesld+1),h
	ret

getix	push iy
	pop ix
	add ix,de
	ret

ptdecod call getix
ptdec	equ $+1
	jp #c3c3

;pt2 pattern decoder
pd2_sam	call setsam
	jr pd2_loop

pd2_eoff ld (ix-12+chp.env_en),a
	jr pd2_loop

pd2_env	ld (ix-12+chp.env_en),16
	ld (iy-100+vrs.ayregs+envtp),a
	ld a,(bc)
	inc bc
	ld l,a
	ld a,(bc)
	inc bc
	ld h,a
	call setenbs
	jr pd2_loop

pd2_orn	call setorn
	jr pd2_loop

pd2_skip inc a
	ld (ix-12+chp.nntskp),a
	jr pd2_loop

pd2_vol	rrca
	rrca
	rrca
	rrca
	ld (ix-12+chp.volume),a
	jr pd2_loop

pd2_del	call c_delay
	jr pd2_loop

pd2_glis set 2,(ix-12+chp.flags)
	inc a
	ld (ix-12+chp.tnsldl),a
	ld (ix-12+chp.tslcnt),a
	ld a,(bc)
	inc bc
        ld (ix-12+chp.tslstp),a
	add a,a
	sbc a,a
        ld (ix-12+chp.tslstp+1),a
	scf
	jr pd2_lp2

pt2pd	and a

pd2_lp2	ex af,af'

pd2_loop ld a,(bc)
	inc bc
	add a,#20
	jr z,pd2_rel
	jr c,pd2_sam
	add a,96
	jr c,pd2_note
	inc a
	jr z,pd2_eoff
	add a,15
	jp z,pd_fin
	jr c,pd2_env
	add a,#10
	jr c,pd2_orn
	add a,#40
	jr c,pd2_skip
	add a,#10
	jr c,pd2_vol
	inc a
	jr z,pd2_del
	inc a
	jr z,pd2_glis
	inc a
	jr z,pd2_port
	inc a
	jr z,pd2_stop
	ld a,(bc)
	inc bc
	ld (ix-12+chp.crnssl),a
	jr pd2_loop

pd2_port res 2,(ix-12+chp.flags)
	ld a,(bc)
	inc bc
	inc bc ;ignoring precalc delta to right sound
	inc bc
	scf
	jr pd2_lp2

pd2_stop ld (ix-12+chp.tslcnt),a
	jr pd2_loop

pd2_rel	ld (ix-12+chp.flags),a
	jr pd2_exit

pd2_note ld l,a
	ld a,(ix-12+chp.note)
	ld (prnote+1),a
	ld (ix-12+chp.note),l
	xor a
	ld (ix-12+chp.tslcnt),a
	set 0,(ix-12+chp.flags)
	ex af,af'
	jr nc,noglis2
	bit 2,(ix-12+chp.flags)
	jr nz,noport2
	ld (lostep),a
	add a,a
	sbc a,a
	ex af,af'
	ld h,a
	ld l,a
	inc a
	call setport
noport2	ld (ix-12+chp.tslcnt),1
noglis2	xor a


pd2_exit ld (ix-12+chp.psinsm),a
	ld (ix-12+chp.psinor),a
	ld (ix-12+chp.crtnsl),a
	ld (ix-12+chp.crtnsl+1),a
	jp pd_fin

;pt3 pattern decoder
pd_orsm	ld (ix-12+chp.env_en),0
	call setorn
pd_sam_	ld a,(bc)
	inc bc
	rrca

pd_sam	call setsam
	jr pd_loop

pd_vol	rrca
	rrca
	rrca
	rrca
	ld (ix-12+chp.volume),a
	jr pd_lp2
	
pd_eoff	ld (ix-12+chp.env_en),a
	ld (ix-12+chp.psinor),a
	jr pd_lp2

pd_sore	dec a
	jr nz,pd_env
	ld a,(bc)
	inc bc
	ld (ix-12+chp.nntskp),a
	jr pd_lp2

pd_env	call setenv
	jr pd_lp2

pd_orn	call setorn
	jr pd_loop

pd_esam	ld (ix-12+chp.env_en),a
	ld (ix-12+chp.psinor),a
	call nz,setenv
	jr pd_sam_

pt3pd	ld a,(ix-12+chp.note)
	ld (prnote+1),a
	ld l,(ix-12+chp.crtnsl)
	ld h,(ix-12+chp.crtnsl+1)
	ld (prslide+1),hl

pd_loop	ld de,#2010
pd_lp2	ld a,(bc)
	inc bc
	add a,e
	jr c,pd_orsm
	add a,d
	jr z,pd_fin
	jr c,pd_sam
	add a,e
	jr z,pd_rel
	jr c,pd_vol
	add a,e
	jr z,pd_eoff
	jr c,pd_sore
	add a,96
	jr c,pd_note
	add a,e
	jr c,pd_orn
	add a,d
	jr c,pd_nois
	add a,e
	jr c,pd_esam
	add a,a
	ld e,a
	ld hl,spccoms+#ff20-#2000
	add hl,de
	ld e,(hl)
	inc hl
	ld d,(hl)
	push de
	jr pd_loop

pd_nois	ld (iy-100+vrs.ns_base),a
	jr pd_lp2

pd_rel	res 0,(ix-12+chp.flags)
	jr pd_res

pd_note	ld (ix-12+chp.note),a
	set 0,(ix-12+chp.flags)
	xor a

pd_res	ld (pdsp_+1),sp
	ld sp,ix
	ld h,a
	ld l,a
	push hl
	push hl
	push hl
	push hl
	push hl
	push hl
pdsp_	ld sp,#3131

pd_fin	ld a,(ix-12+chp.nntskp)
	ld (ix-12+chp.ntskcn),a
	ret

c_portm ld a,(bc)
	inc bc
;skip precalculated tone delta (because
;cannot be right after pt3 compilation)
	inc bc
	inc bc
	ex af,af'
	ld a,(bc) ;signed tone step
	inc bc
	ld (lostep),a
	ld a,(bc)
	inc bc
	and a
	ex af,af'
	ld l,(ix-12+chp.crtnsl)
	ld h,(ix-12+chp.crtnsl+1)

;set portamento variables
;a - delay; a' - hi(step); zf' - (a'=0); hl - crtnsl

setport	res 2,(ix-12+chp.flags)
	ld (ix-12+chp.tnsldl),a
	ld (ix-12+chp.tslcnt),a
	push hl
	ld de,nt_
	ld a,(ix-12+chp.note)
	ld (ix-12+chp.sltont),a
	add a,a
	ld l,a
	ld h,0
	add hl,de
	ld a,(hl)
	inc hl
	ld h,(hl)
	ld l,a
	push hl
prnote	ld a,#3e
	ld (ix-12+chp.note),a
	add a,a
	ld l,a
	ld h,0
	add hl,de
	ld e,(hl)
	inc hl
	ld d,(hl)
	pop hl
	sbc hl,de
	ld (ix-12+chp.tndelt),l
	ld (ix-12+chp.tndelt+1),h
	pop de
version equ $+1
	ld a,#3e
	cp 6
	jr c,oldprtm ;old 3xxx for pt v3.5-
prslide	ld de,#1111
	ld (ix-12+chp.crtnsl),e
	ld (ix-12+chp.crtnsl+1),d
lostep	equ $+1
oldprtm	ld a,#3e
	ex af,af'
	jr z,nosig
	ex de,hl
nosig	sbc hl,de
	jp p,set_stp
	cpl
	ex af,af'
	neg
	ex af,af'
set_stp	ld (ix-12+chp.tslstp+1),a
	ex af,af'
	ld (ix-12+chp.tslstp),a
	ld (ix-12+chp.conoff),0
	ret

c_gliss	set 2,(ix-12+chp.flags)
	ld a,(bc)
	inc bc
	ld (ix-12+chp.tnsldl),a
	and a
	jr nz,gl36
	ld a,(version) ;alco pt3.7+
	cp 7
	sbc a,a
	inc a
gl36	ld (ix-12+chp.tslcnt),a
	ld a,(bc)
	inc bc
	ex af,af'
	ld a,(bc)
	inc bc
	jr set_stp

c_smpos	ld a,(bc)
	inc bc
	ld (ix-12+chp.psinsm),a
	ret

c_orpos	ld a,(bc)
	inc bc
	ld (ix-12+chp.psinor),a
	ret

c_vibrt	ld a,(bc)
	inc bc
	ld (ix-12+chp.onoffd),a
	ld (ix-12+chp.conoff),a
	ld a,(bc)
	inc bc
	ld (ix-12+chp.offond),a
	xor a
	ld (ix-12+chp.tslcnt),a
	ld (ix-12+chp.crtnsl),a
	ld (ix-12+chp.crtnsl+1),a
	ret

c_engls	ld a,(bc)
	inc bc
	ld (iy-100+vrs.env_del),a
	ld (iy-100+vrs.curedel),a
	ld a,(bc)
	inc bc
	ld l,a
	ld a,(bc)
	inc bc
	ld h,a
	ld (iy-100+vrs.esldadd),l
	ld (iy-100+vrs.esldadd+1),h
	ret

c_delay	ld a,(bc)
	inc bc
	ld (iy-100+vrs.delay),a
	ld hl,vars2+vrs.modnum ;if alco_ts
	bit 1,(hl)
	ret z
	ld (vars1+vrs.delay),a
	ld (vars1+vrs.delycnt),a
	ld (vars2+vrs.delay),a
	ret
	
setenv	ld (ix-12+chp.env_en),e
	ld (iy-100+vrs.ayregs+envtp),a
	ld a,(bc)
	inc bc
	ld h,a
	ld a,(bc)
	inc bc
	ld l,a
	call setenbs
	xor a
	ld (ix-12+chp.psinor),a
	ld (iy-100+vrs.curedel),a
	ld h,a
	ld l,a
	jp setesld

setorn	add a,a
	ld e,a
	ld d,0
	ld (ix-12+chp.psinor),d
	ld l,(iy-100+vrs.ornptrs)
	ld h,(iy-100+vrs.ornptrs+1)
	add hl,de
	ld e,(hl)
	inc hl
	ld d,(hl)
	ld l,(iy-100+vrs.modaddr)
	ld h,(iy-100+vrs.modaddr+1)
	add hl,de
	ld (ix-12+chp.ornptr),l
	ld (ix-12+chp.ornptr+1),h
c_nop	ret

setsam	add a,a
	ld e,a
	ld d,0
	ld l,(iy-100+vrs.samptrs);
	ld h,(iy-100+vrs.samptrs+1);
	add hl,de
	ld e,(hl)
	inc hl
	ld d,(hl)
	ld l,(iy-100+vrs.modaddr)
	ld h,(iy-100+vrs.modaddr+1)
	add hl,de
	ld (ix-12+chp.samptr),l
	ld (ix-12+chp.samptr+1),h
	ret

;all 16 addresses to protect from broken pt3 modules
spccoms dw c_nop
	dw c_gliss
	dw c_portm
	dw c_smpos
	dw c_orpos
	dw c_vibrt
	dw c_nop
	dw c_nop
	dw c_engls
	dw c_delay
	dw c_nop
	dw c_nop
	dw c_nop
	dw c_nop
	dw c_nop
	dw c_nop

chregs	call getix
	xor a
	ld (ampl),a
	bit 0,(ix+chp.flags)
	push hl
	jp z,ch_exit
	ld (csp_+1),sp
	ld l,(ix+chp.ornptr)
	ld h,(ix+chp.ornptr+1)
	ld sp,hl
	pop de
	ld h,a
	ld a,(ix+chp.psinor)
	ld l,a
	add hl,sp
	inc a
		;pt2	pt3
orncp	inc a	;cp e	cp d
	jr c,ch_orps
ornld	db 1	;ld a,d	ld a,e
ch_orps	ld (ix+chp.psinor),a
	ld a,(ix+chp.note)
	add a,(hl)
	jp p,ch_ntp
	xor a
ch_ntp	cp 96
	jr c,ch_nok
	ld a,95
ch_nok	add a,a
	ex af,af'
	ld l,(ix+chp.samptr)
	ld h,(ix+chp.samptr+1)
	ld sp,hl
	pop de
	ld h,0
	ld a,(ix+chp.psinsm)
	ld b,a
	add a,a
samclc2	add a,a ;or add a,b for pt2
	ld l,a
	add hl,sp
	ld sp,hl
	ld a,b
	inc a
		;pt2	pt3
samcp	inc a	;cp e	cp d
	jr c,ch_smps
samld	db 1	;ld a,d	ld a,e
ch_smps	ld (ix+chp.psinsm),a
	pop bc
	pop hl

;convert pt2 sample to pt3
		;pt2		pt3
samcnv	pop hl  ;bit 2,c	jr e_
	pop hl	
	ld h,b
	jr nz,$+8
	ex de,hl
	and a
	sbc hl,hl
	sbc hl,de
	ld d,c
	rr c
	sbc a,a
	cpl
	and #3e
	rr c
	rr b
	and c
	ld c,a
	ld a,b
	rra
	rra
	rr d
	rra
	and #9f
	ld b,a

e_	ld e,(ix+chp.tnacc)
	ld d,(ix+chp.tnacc+1)
	add hl,de
	bit 6,b
	jr z,ch_noac
	ld (ix+chp.tnacc),l
	ld (ix+chp.tnacc+1),h
ch_noac ex de,hl
	ex af,af'
	add a,nt_
	ld l,a
	adc a,nt_/256
	sub l
	ld h,a
	ld sp,hl
	pop hl
	add hl,de
	ld e,(ix+chp.crtnsl)
	ld d,(ix+chp.crtnsl+1)
	add hl,de
csp_	ld sp,#3131
	ex (sp),hl
	xor a
	or (ix+chp.tslcnt)
	jr z,ch_amp
	dec (ix+chp.tslcnt)
	jr nz,ch_amp
	ld a,(ix+chp.tnsldl)
	ld (ix+chp.tslcnt),a
	ld l,(ix+chp.tslstp)
	ld h,(ix+chp.tslstp+1)
	ld a,h
	add hl,de
	ld (ix+chp.crtnsl),l
	ld (ix+chp.crtnsl+1),h
	bit 2,(ix+chp.flags)
	jr nz,ch_amp
	ld e,(ix+chp.tndelt)
	ld d,(ix+chp.tndelt+1)
	and a
	jr z,ch_stpp
	ex de,hl
ch_stpp sbc hl,de
	jp m,ch_amp
	ld a,(ix+chp.sltont)
	ld (ix+chp.note),a
	xor a
	ld (ix+chp.tslcnt),a
	ld (ix+chp.crtnsl),a
	ld (ix+chp.crtnsl+1),a
ch_amp	ld a,(ix+chp.cramsl)
	bit 7,c
	jr z,ch_noam
	bit 6,c
	jr z,ch_amin
	cp 15
	jr z,ch_noam
	inc a
	jr ch_svam
ch_amin	cp -15
	jr z,ch_noam
	dec a
ch_svam	ld (ix+chp.cramsl),a
ch_noam	ld l,a
	ld a,b
	and 15
	add a,l
	jp p,ch_apos
	xor a
ch_apos	cp 16
	jr c,ch_vol
	ld a,15
ch_vol	or (ix+chp.volume)
	add a,vt_
	ld l,a
	adc a,vt_/256
	sub l
	ld h,a
	ld a,(hl)
ch_env	bit 0,c
	jr nz,ch_noen
	or (ix+chp.env_en)
ch_noen	ld (ampl),a
	bit 7,b
	ld a,c
	jr z,no_ensl
	rla
	rla
	sra a
	sra a
	sra a
	add a,(ix+chp.crensl) ;see comment below
	bit 5,b
	jr z,no_enac
	ld (ix+chp.crensl),a
no_enac	add a,(iy-100+vrs.addtoen) ;bug in pt3 - need word here
	ld (iy-100+vrs.addtoen),a
	jr ch_mix
no_ensl rra
	add a,(ix+chp.crnssl)
	ld (iy-100+vrs.addtons),a
	bit 5,b
	jr z,ch_mix
	ld (ix+chp.crnssl),a
ch_mix	ld a,b
	rra
	and #48
ch_exit	or (iy-100+vrs.ayregs+mixer)
	rrca
	ld (iy-100+vrs.ayregs+mixer),a
	pop hl
	xor a
	or (ix+chp.conoff)
	ret z
	dec (ix+chp.conoff)
	ret nz
	xor (ix+chp.flags)
	ld (ix+chp.flags),a
	rra
	ld a,(ix+chp.onoffd)
	jr c,ch_ondl
	ld a,(ix+chp.offond)
ch_ondl	ld (ix+chp.conoff),a
	ret

play_	xor a
	ld (iy-100+vrs.addtoen),a
	ld (iy-100+vrs.ayregs+mixer),a
	dec a
	ld (iy-100+vrs.ayregs+envtp),a
	dec (iy-100+vrs.delycnt)
	jp nz,pl2
	dec (iy-100+vrs.chana+chp.ntskcn)
	jr nz,pl1b
	ld c,(iy-100+vrs.adinpta)
	ld b,(iy-100+vrs.adinpta+1)
	ld a,(bc)
	and a
	jr nz,pl1a
	ld d,a
	ld (iy-100+vrs.ns_base),a
	ld l,(iy-100+vrs.crpsptr)
	ld h,(iy-100+vrs.crpsptr+1)
	inc hl
	ld a,(hl)
	inc a
	jr nz,plnlp

	if loopchecker
	call checklp
	endif

	ld l,(iy-100+vrs.lposptr)
	ld h,(iy-100+vrs.lposptr+1)
	ld a,(hl)
	inc a
plnlp	call setcppt
	dec a
	bit 1,(iy-100+vrs.modnum)
	jr z,noalco
tssub	equ $+1
	sub #d6
	cpl
noalco
		;pt2		pt3
pscalc	dec a	;add a,a	nop
	dec a	;add a,(hl)	nop
	add a,a
	ld e,a
	rl d

	if curposcounter
	ld a,l
	sub (iy-100+vrs.possub)
	ld (iy-100+vrs.curpos),a
	endif

	ld l,(iy-100+vrs.patsptr)
	ld h,(iy-100+vrs.patsptr+1)
	add hl,de
	ld e,(iy-100+vrs.modaddr)
	ld d,(iy-100+vrs.modaddr+1)
	ld (psp_+1),sp
	ld sp,hl
	pop hl
	add hl,de
	ld b,h
	ld c,l
	pop hl
	add hl,de
	ld (iy-100+vrs.adinptb),l
	ld (iy-100+vrs.adinptb+1),h
	pop hl
	add hl,de
	ld (iy-100+vrs.adinptc),l
	ld (iy-100+vrs.adinptc+1),h
psp_	ld sp,#3131
pl1a	ld de,vrs.chana+12-100
	call ptdecod
	ld (iy-100+vrs.adinpta),c
	ld (iy-100+vrs.adinpta+1),b

pl1b	dec (iy-100+vrs.chanb+chp.ntskcn)
	jr nz,pl1c
	ld de,vrs.chanb+12-100
	ld c,(iy-100+vrs.adinptb)
	ld b,(iy-100+vrs.adinptb+1)
	call ptdecod
	ld (iy-100+vrs.adinptb),c
	ld (iy-100+vrs.adinptb+1),b

pl1c	dec (iy-100+vrs.chanc+chp.ntskcn)
	jr nz,pl1d
	ld de,vrs.chanc+12-100
	ld c,(iy-100+vrs.adinptc)
	ld b,(iy-100+vrs.adinptc+1)
	call ptdecod
	ld (iy-100+vrs.adinptc),c
	ld (iy-100+vrs.adinptc+1),b

pl1d	ld a,(iy-100+vrs.delay)
	ld (iy-100+vrs.delycnt),a

pl2	ld de,vrs.chana-100
	ld l,(iy-100+vrs.ayregs+tona)
	ld h,(iy-100+vrs.ayregs+tona+1)
	call chregs
	ld (iy-100+vrs.ayregs+tona),l
	ld (iy-100+vrs.ayregs+tona+1),h
ampl	equ $+1
	ld a,#3e
	ld (iy-100+vrs.ayregs+ampla),a
	ld de,vrs.chanb-100
	ld l,(iy-100+vrs.ayregs+tonb)
	ld h,(iy-100+vrs.ayregs+tonb+1)
	call chregs
	ld (iy-100+vrs.ayregs+tonb),l
	ld (iy-100+vrs.ayregs+tonb+1),h
	ld a,(ampl)
	ld (iy-100+vrs.ayregs+amplb),a
	ld de,vrs.chanc-100
	ld l,(iy-100+vrs.ayregs+tonc)
	ld h,(iy-100+vrs.ayregs+tonc+1)
	call chregs
	ld (iy-100+vrs.ayregs+tonc),l
	ld (iy-100+vrs.ayregs+tonc+1),h
	ld a,(ampl)
	ld (iy-100+vrs.ayregs+amplc),a

	ld a,(iy-100+vrs.ns_base)
	add (iy-100+vrs.addtons)
	ld (iy-100+vrs.ayregs+noise),a

	ld a,(iy-100+vrs.addtoen)
	ld e,a
	add a,a
	sbc a,a
	ld d,a
	ld l,(iy-100+vrs.envbase)
	ld h,(iy-100+vrs.envbase+1)
	add hl,de
	ld e,(iy-100+vrs.curesld)
	ld d,(iy-100+vrs.curesld+1)
	add hl,de
	ld (iy-100+vrs.ayregs+env),l
	ld (iy-100+vrs.ayregs+env+1),h

	xor a
	or (iy-100+vrs.curedel)
	ret z
	dec (iy-100+vrs.curedel)
	ret nz
	ld a,(iy-100+vrs.env_del)
	ld (iy-100+vrs.curedel),a
	ld l,(iy-100+vrs.esldadd)
	ld h,(iy-100+vrs.esldadd+1)
	add hl,de
	jp setesld

play    ld iy,vars1+100
	call play_
	ld a,(is_ts)
	and a
	jr z,pl_nts
	ld iy,vars2+100
	call play_
pl_nts
	if basic
	ld iy,#5c3a
	endif

rout	ld bc,#fffd
	ld a,(is_ts)
	and a
	jr z,r_nts ;keep old standard
	out (c),b
r_nts	ex af,af'

	if acbbac
	ld ix,vars1+vrs.ayregs
	else
	ld hl,vars1+vrs.ayregs
	endif

	call rout_
	ex af,af'
	ret z
	ld b,d
	cpl
	out (c),a

	if acbbac
	ld ix,vars2+vrs.ayregs
	else
	ld hl,vars2+vrs.ayregs
	endif

rout_
	if acbbac
	ld a,(setup)
	and 12
	jr z,abc
	add a,chtable
	ld e,a
	adc a,chtable/256
	sub e
	ld d,a
	ld b,0
	push ix
	pop hl
	ld a,(de)
	inc de
	ld c,a
	add hl,bc
	ld a,(ix+tonb)
	ld c,(hl)
	ld (ix+tonb),c
	ld (hl),a
	inc hl
	ld a,(ix+tonb+1)
	ld c,(hl)
	ld (ix+tonb+1),c
	ld (hl),a
	ld a,(de)
	inc de
	ld c,a
	add hl,bc
	ld a,(ix+amplb)
	ld c,(hl)
	ld (ix+amplb),c
	ld (hl),a
	ld a,(de)
	inc de
	ld (rxca1),a
	xor 8
	ld (rxca2),a
	ld a,(de)
	and (ix+mixer)
	ld e,a
	ld a,(ix+mixer)
rxca1	db #e6
	and %010010
	or e
	ld e,a
	ld a,(ix+mixer)
	and %010010
rxca2	or e
	or e
	ld (ix+mixer),a
abc
	endif

	xor a
	ld de,#ffbf

	if acbbac
	ld bc,#fffd
	push ix
	pop hl
	endif

lout	out (c),a
	ld b,e
	outi 
	ld b,d
	inc a
	cp 13
	jr nz,lout
	out (c),a
	ld a,(hl)
	and a
	ret m
	ld b,e
	out (c),a
	ret

	if acbbac
chtable	equ $-4
	db 4,5,15,%001001,0,7,7,%100100
	endif

nt_data	db (t_new_0-t1_)*2
	db tcnew_0-t_
	db (t_old_0-t1_)*2+1
	db tcold_0-t_
	db (t_new_1-t1_)*2+1
	db tcnew_1-t_
	db (t_old_1-t1_)*2+1
	db tcold_1-t_
	db (t_new_2-t1_)*2
	db tcnew_2-t_
	db (t_old_2-t1_)*2
	db tcold_2-t_
	db (t_new_3-t1_)*2
	db tcnew_3-t_
	db (t_old_3-t1_)*2
	db tcold_3-t_

t_

tcold_0	db #00+1,#04+1,#08+1,#0a+1,#0c+1,#0e+1,#12+1,#14+1
	db #18+1,#24+1,#3c+1,0
tcold_1	db #5c+1,0
tcold_2	db #30+1,#36+1,#4c+1,#52+1,#5e+1,#70+1,#82,#8c,#9c
	db #9e,#a0,#a6,#a8,#aa,#ac,#ae,#ae,0
tcnew_3	db #56+1
tcold_3	db #1e+1,#22+1,#24+1,#28+1,#2c+1,#2e+1,#32+1,#be+1,0
tcnew_0	db #1c+1,#20+1,#22+1,#26+1,#2a+1,#2c+1,#30+1,#54+1
	db #bc+1,#be+1,0
tcnew_1 equ tcold_1
tcnew_2	db #1a+1,#20+1,#24+1,#28+1,#2a+1,#3a+1,#4c+1,#5e+1
	db #ba+1,#bc+1,#be+1,0

pt3emptyorn equ $-1
	db 1,0

;first 12 values of tone tables (packed)

t_pack	db #06ec*2/256,#06ec*2
	db #0755-#06ec
	db #07c5-#0755
	db #083b-#07c5
	db #08b8-#083b
	db #093d-#08b8
	db #09ca-#093d
	db #0a5f-#09ca
	db #0afc-#0a5f
	db #0ba4-#0afc
	db #0c55-#0ba4
	db #0d10-#0c55
	db #066d*2/256,#066d*2
	db #06cf-#066d
	db #0737-#06cf
	db #07a4-#0737
	db #0819-#07a4
	db #0894-#0819
	db #0917-#0894
	db #09a1-#0917
	db #0a33-#09a1
	db #0acf-#0a33
	db #0b73-#0acf
	db #0c22-#0b73
	db #0cda-#0c22
	db #0704*2/256,#0704*2
	db #076e-#0704
	db #07e0-#076e
	db #0858-#07e0
	db #08d6-#0858
	db #095c-#08d6
	db #09ec-#095c
	db #0a82-#09ec
	db #0b22-#0a82
	db #0bcc-#0b22
	db #0c80-#0bcc
	db #0d3e-#0c80
	db #07e0*2/256,#07e0*2
	db #0858-#07e0
	db #08e0-#0858
	db #0960-#08e0
	db #09f0-#0960
	db #0a88-#09f0
	db #0b28-#0a88
	db #0bd8-#0b28
	db #0c80-#0bd8
	db #0d60-#0c80
	db #0e10-#0d60
	db #0ef8-#0e10

;vars from here can be stripped
;you can move vars to any other address

vars

is_ts	db 0

;channelsvars
	struct	chp
;reset group
psinor	db 0
psinsm	db 0
cramsl	db 0
crnssl	db 0
crensl	db 0
tslcnt	db 0
crtnsl	dw 0
tnacc	dw 0
conoff	db 0
;reset group

onoffd	db 0

;ix for ptdecod here (+12)
offond	db 0
ornptr	dw 0
samptr	dw 0
nntskp	db 0
note	db 0
sltont	db 0
env_en	db 0
flags	db 0
 ;enabled - 0, simplegliss - 2
tnsldl	db 0
tslstp	dw 0
tndelt	dw 0
ntskcn	db 0
volume	db 0
	ends

	struct	vrs

;if not works in struct in sjasm :(
;	if curposcounter
curpos	db 0
possub	db 0
;	endif

modnum	db 0 ;bit0: chipnum
	     ;bit1: 1-reversed patterns order (alco ts)

chana	ds chp
chanb	ds chp
chanc	ds chp

;globalvars
modaddr	dw 0
ornptrs	dw 0
samptrs	dw 0
patsptr	dw 0
adinpta	dw 0
adinptb	dw 0
adinptc	dw 0
crpsptr	dw 0
lposptr	dw 0
delay	db 0
delycnt	db 0
esldadd	dw 0
curesld	dw 0
env_del	db 0
curedel	db 0
ns_base	db 0
addtons	db 0
addtoen	db 0
envbase	dw 0
ayregs	ds 14
	ends

vars1	ds vrs
vars2	ds vrs

vt_	equ $-16
	ds 256-16 ;createdvolumetableaddress

t1_	equ vt_+16 ;tone tables data depacked here

t_old_1	equ t1_
t_old_2	equ t_old_1+24
t_old_3	equ t_old_2+24
t_old_0	equ t_old_3+2
t_new_0	equ t_old_0
t_new_1	equ t_old_1
t_new_2	equ t_new_0+24
t_new_3	equ t_old_3

pt2emptyorn equ vt_+31 ;1,0,0 sequence

nt_	ds 192 ;creatednotetableaddress

var0end	equ vt_+16 ;init zeroes from vars to var0end-1

varsend equ $

mdladdr equ $

;release 0 steps:
;04/21/2007
;works start (ptxplay adaptation); first beta.
;04/22/2007
;job finished; beta-testing.
;04/23/2007
;pt v3.7 ts mode corrected (after alco remarks).
;04/29/2007
;added 1.xx and 2.xx special commands interpretation for pt3
;modules of v3.7+.

;size (minimal build for zx spectrum):
;code block #908 bytes
;variables #2bf bytes (can be stripped)
;total size #908+#2bf=#bc7 (3015) bytes
	ENDMODULE 

;release 0 steps:
;04/21/2007
;works start (ptxplay adaptation); first beta.
;04/22/2007
;job finished; beta-testing.
;04/23/2007
;pt v3.7 ts mode corrected (after alco remarks).
;04/29/2007
;added 1.xx and 2.xx special commands interpretation for pt3
;modules of v3.7+.

;size (minimal build for zx spectrum):
;code block 0x908 bytes
;variables 0x2bf bytes (can be stripped)
;total size 0x908+0x2bf=0xbc7 (3015) bytes
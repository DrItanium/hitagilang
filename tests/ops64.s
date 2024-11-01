	.text
	.align 4
	.globl _addol
	#  Function 'addol'
	#  Registers used: g0 g1 g2 g3 g4 g5 g6 fp cc 
	#		   
_addol:
	movl	g0,g4 # m4
	addo	g0,g2,g0
	cmpo	g0,g4
	testl	g6
	addo	g5,g3,g1
	addo	g6,g1,g1
.Li960R1:	ret
	.align 4
	.globl _subol
	#  Function 'subol'
	#  Registers used: g0 g1 g2 g3 g4 g5 g6 fp cc 
	#		   
_subol:
	movl	g0,g4 # m4
	subo	g2,g0,g0
	cmpo	g0,g4
	testg	g6
	subo	g3,g5,g1
	subo	g6,g1,g1
.Li960R2:	ret
	.align 4
	.globl _mulol
	#  Function 'mulol'
	#  Registers used: g0 g1 g2 g3 g4 g5 g6 fp 
_mulol:
	movl	g0,g4 # m4
	emul	g0,g2,g0
	mulo	g4,g3,g6
	addo	g1,g6,g6
	mulo	g2,g5,g2
	addo	g6,g2,g1
.Li960R3:	ret
	.align 4
	.globl _divol
	#  Function 'divol'
	#  Registers used: g0 g1 g2 g3 fp sp 
	#		   r3* 
_divol:
	addo	16,sp,sp
	#Prologue stats:
	#  Total Frame Size: 16 bytes
	#End Prologue#
	callx	___udivdi3
.Li960R4:	ret
	.align 4
	.globl _remol
	#  Function 'remol'
	#  Registers used: g0 g1 g2 g3 fp sp 
	#		   r3* 
_remol:
	addo	16,sp,sp
	#Prologue stats:
	#  Total Frame Size: 16 bytes
	#End Prologue#
	callx	___umoddi3
.Li960R5:	ret
	.align 4
	.globl _shlol
	#  Function 'shlol'
	#  Registers used: g0 g1 g2 g3 fp sp 
	#		   r3* 
_shlol:
	addo	16,sp,sp
	#Prologue stats:
	#  Total Frame Size: 16 bytes
	#End Prologue#
	callx	___ashldi3
.Li960R6:	ret
	.align 4
	.globl _shrol
	#  Function 'shrol'
	#  Registers used: g0 g1 g2 g3 fp sp 
	#		   r3* 
_shrol:
	addo	16,sp,sp
	#Prologue stats:
	#  Total Frame Size: 16 bytes
	#End Prologue#
	callx	___lshrdi3
.Li960R7:	ret
	.align 4
	.globl _andl
	#  Function 'andl'
	#  Registers used: g0 g1 g2 g3 fp 
_andl:
	and	g0,g2,g0
	and	g1,g3,g1
.Li960R8:	ret
	.align 4
	.globl _orl
	#  Function 'orl'
	#  Registers used: g0 g1 g2 g3 fp 
_orl:
	or	g0,g2,g0
	or	g1,g3,g1
.Li960R9:	ret
	.align 4
	.globl _notl
	#  Function 'notl'
	#  Registers used: g0 g1 fp 
_notl:
	not	g0,g0
	not	g1,g1
.Li960R10:	ret
	.align 4
	.globl _xorl
	#  Function 'xorl'
	#  Registers used: g0 g1 g2 g3 fp 
_xorl:
	xor	g0,g2,g0
	xor	g1,g3,g1
.Li960R11:	ret
	.align 4
	.globl _xnorl
	#  Function 'xnorl'
	#  Registers used: g0 g1 g2 g3 fp 
_xnorl:
	xor	g2,g0,g2
	xor	g3,g1,g3
	not	g2,g2
	not	g3,g3
	movl	g2,g0 # m4
.Li960R12:	ret
	.align 4
	.globl _nandl
	#  Function 'nandl'
	#  Registers used: g0 g1 g2 g3 fp 
_nandl:
	and	g2,g0,g2
	and	g3,g1,g3
	not	g2,g2
	not	g3,g3
	movl	g2,g0 # m4
.Li960R13:	ret
	.align 4
	.globl _norl
	#  Function 'norl'
	#  Registers used: g0 g1 g2 g3 fp 
_norl:
	or	g2,g0,g2
	or	g3,g1,g3
	not	g2,g2
	not	g3,g3
	movl	g2,g0 # m4
.Li960R14:	ret
	.align 4
	.globl _addil
	#  Function 'addil'
	#  Registers used: g0 g1 g2 g3 g4 g5 g6 fp cc 
	#		   
_addil:
	movl	g0,g4 # m4
	addo	g0,g2,g0
	cmpo	g0,g4
	testl	g6
	addo	g5,g3,g1
	addo	g6,g1,g1
.Li960R15:	ret
	.align 4
	.globl _subil
	#  Function 'subil'
	#  Registers used: g0 g1 g2 g3 g4 g5 g6 fp cc 
	#		   
_subil:
	movl	g0,g4 # m4
	subo	g2,g0,g0
	cmpo	g0,g4
	testg	g6
	subo	g3,g5,g1
	subo	g6,g1,g1
.Li960R16:	ret
	.align 4
	.globl _mulil
	#  Function 'mulil'
	#  Registers used: g0 g1 g2 g3 g4 g5 g6 fp 
_mulil:
	movl	g0,g4 # m4
	emul	g0,g2,g0
	mulo	g4,g3,g6
	addo	g1,g6,g6
	mulo	g2,g5,g2
	addo	g6,g2,g1
.Li960R17:	ret
	.align 4
	.globl _divil
	#  Function 'divil'
	#  Registers used: g0 g1 g2 g3 fp sp 
	#		   r3* 
_divil:
	addo	16,sp,sp
	#Prologue stats:
	#  Total Frame Size: 16 bytes
	#End Prologue#
	callx	___divdi3
.Li960R18:	ret
	.align 4
	.globl _remil
	#  Function 'remil'
	#  Registers used: g0 g1 g2 g3 fp sp 
	#		   r3* 
_remil:
	addo	16,sp,sp
	#Prologue stats:
	#  Total Frame Size: 16 bytes
	#End Prologue#
	callx	___moddi3
.Li960R19:	ret
	.align 4
	.globl _shlil
	#  Function 'shlil'
	#  Registers used: g0 g1 g2 g3 fp sp 
	#		   r3* 
_shlil:
	addo	16,sp,sp
	#Prologue stats:
	#  Total Frame Size: 16 bytes
	#End Prologue#
	callx	___ashldi3
.Li960R20:	ret
	.align 4
	.globl _shril
	#  Function 'shril'
	#  Registers used: g0 g1 g2 g3 fp sp 
	#		   r3* 
_shril:
	addo	16,sp,sp
	#Prologue stats:
	#  Total Frame Size: 16 bytes
	#End Prologue#
	callx	___ashrdi3
.Li960R21:	ret
	.align 4
	.globl _addo
	#  Function 'addo'
	#  Registers used: g0 g1 fp 
_addo:
	addo	g0,g1,g0
.Li960R22:	ret
	.align 4
	.globl _subo
	#  Function 'subo'
	#  Registers used: g0 g1 fp 
_subo:
	subo	g1,g0,g0
.Li960R23:	ret
	.align 4
	.globl _mulo
	#  Function 'mulo'
	#  Registers used: g0 g1 fp 
_mulo:
	mulo	g0,g1,g0
.Li960R24:	ret
	.align 4
	.globl _divo
	#  Function 'divo'
	#  Registers used: g0 g1 fp 
_divo:
	divo	g1,g0,g0
.Li960R25:	ret
	.align 4
	.globl _remo
	#  Function 'remo'
	#  Registers used: g0 g1 fp 
_remo:
	remo	g1,g0,g0
.Li960R26:	ret
	.align 4
	.globl _shlo
	#  Function 'shlo'
	#  Registers used: g0 g1 fp 
_shlo:
	shlo	g1,g0,g0
.Li960R27:	ret
	.align 4
	.globl _shro
	#  Function 'shro'
	#  Registers used: g0 g1 fp 
_shro:
	shro	g1,g0,g0
.Li960R28:	ret
	.align 4
	.globl _and
	#  Function 'and'
	#  Registers used: g0 g1 fp 
_and:
	and	g0,g1,g0
.Li960R29:	ret
	.align 4
	.globl _or
	#  Function 'or'
	#  Registers used: g0 g1 fp 
_or:
	or	g0,g1,g0
.Li960R30:	ret
	.align 4
	.globl _not
	#  Function 'not'
	#  Registers used: g0 fp 
_not:
	not	g0,g0
.Li960R31:	ret
	.align 4
	.globl _xor
	#  Function 'xor'
	#  Registers used: g0 g1 fp 
_xor:
	xor	g0,g1,g0
.Li960R32:	ret
	.align 4
	.globl _xnor
	#  Function 'xnor'
	#  Registers used: g0 g1 fp 
_xnor:
	xnor	g0,g1,g0
.Li960R33:	ret
	.align 4
	.globl _nand
	#  Function 'nand'
	#  Registers used: g0 g1 fp 
_nand:
	nand	g1,g0,g0
.Li960R34:	ret
	.align 4
	.globl _nor
	#  Function 'nor'
	#  Registers used: g0 g1 fp 
_nor:
	nor	g1,g0,g0
.Li960R35:	ret
	.align 4
	.globl _addi
	#  Function 'addi'
	#  Registers used: g0 g1 fp 
_addi:
	addo	g0,g1,g0
.Li960R36:	ret
	.align 4
	.globl _subi
	#  Function 'subi'
	#  Registers used: g0 g1 fp 
_subi:
	subo	g1,g0,g0
.Li960R37:	ret
	.align 4
	.globl _muli
	#  Function 'muli'
	#  Registers used: g0 g1 fp 
_muli:
	mulo	g0,g1,g0
.Li960R38:	ret
	.align 4
	.globl _divi
	#  Function 'divi'
	#  Registers used: g0 g1 fp 
_divi:
	divi	g1,g0,g0
.Li960R39:	ret
	.align 4
	.globl _remi
	#  Function 'remi'
	#  Registers used: g0 g1 fp 
_remi:
	remi	g1,g0,g0
.Li960R40:	ret
	.align 4
	.globl _shli
	#  Function 'shli'
	#  Registers used: g0 g1 fp 
_shli:
	shlo	g1,g0,g0
.Li960R41:	ret
	.align 4
	.globl _shri
	#  Function 'shri'
	#  Registers used: g0 g1 fp 
_shri:
	shri	g1,g0,g0
.Li960R42:	ret
	.align 4
	.globl _addi2
	#  Function 'addi2'
	#  Registers used: g0 g1 fp 
_addi2:
	addo	g0,g1,g0
.Li960R43:	ret
	.align 4
	.globl _subi2
	#  Function 'subi2'
	#  Registers used: g0 g1 fp 
_subi2:
	subo	g1,g0,g0
.Li960R44:	ret
	.align 4
	.globl _muli2
	#  Function 'muli2'
	#  Registers used: g0 g1 fp 
_muli2:
	mulo	g0,g1,g0
.Li960R45:	ret
	.align 4
	.globl _divi2
	#  Function 'divi2'
	#  Registers used: g0 g1 fp 
_divi2:
	divi	g1,g0,g0
.Li960R46:	ret
	.align 4
	.globl _remi2
	#  Function 'remi2'
	#  Registers used: g0 g1 fp 
_remi2:
	remi	g1,g0,g0
.Li960R47:	ret
	.align 4
	.globl _shli2
	#  Function 'shli2'
	#  Registers used: g0 g1 fp 
_shli2:
	shlo	g1,g0,g0
.Li960R48:	ret
	.align 4
	.globl _shri2
	#  Function 'shri2'
	#  Registers used: g0 g1 fp 
_shri2:
	shri	g1,g0,g0
.Li960R49:	ret
	.align 4
	.globl _ldol
	#  Function 'ldol'
	#  Registers used: g0 g1 fp 
_ldol:
	ldl	(g0),g0
.Li960R50:	ret
	.align 4
	.globl _ldil
	#  Function 'ldil'
	#  Registers used: g0 g1 fp 
_ldil:
	ldl	(g0),g0
.Li960R51:	ret
	.align 4
	.globl _setbit
	#  Function 'setbit'
	#  Registers used: g0 g1 fp 
_setbit:
	setbit	g1,g0,g0
.Li960R52:	ret
	.align 4
	.globl _clearbit
	#  Function 'clearbit'
	#  Registers used: g0 g1 fp 
_clearbit:
	clrbit	g1,g0,g0
.Li960R53:	ret
	.align 4
	.globl _getA_a
	#  Function 'getA_a'
	#  Registers used: g0 fp 
_getA_a:
	ld	(g0),g0
.Li960R54:	ret
	.align 4
	.globl _getA_b
	#  Function 'getA_b'
	#  Registers used: g0 g1 fp 
_getA_b:
	ldl	8(g0),g0
.Li960R55:	ret
	.align 4
	.globl _getA_c
	#  Function 'getA_c'
	#  Registers used: g0 g1 g2 g3 fp 
_getA_c:
	ldq	16(g0),g0
.Li960R56:	ret
	.align 4
	.globl _getA_d
	#  Function 'getA_d'
	#  Registers used: g0 g1 g2 g3 fp 
_getA_d:
	ldq	32(g0),g0
.Li960R57:	ret
	.align 4
	.globl _countItems
	#  Function 'countItems'
	#  Registers used: g0 g1 g2 g3 g4 g5 g6 fp cc 
	#		   
_countItems:
	movl	0,g2 #m5.1
	ld	(g0),g4
	cmpobe	0,g4,.L64
.L62:
	addo	g2,1,g4
	cmpo	g4,g2
	testl	g6
	addo	g3,g6,g5
	movl	g4,g2 # m4
	addo	g0,4,g0
	ld	(g0),g4
	cmpobne	0,g4,.L62
.L64:
	movl	g2,g0 # m4
.Li960R58:	ret
	.align 4
	.globl _B_or
	#  Function 'B_or'
	#  Registers used: g0 g1 g2 g3 g4 g5 g6 g7 fp r4* 
	#		   r5* r6* r7* 
_B_or:
	or	g0,g4,r4
	or	g1,g5,r5
	or	g2,g6,r6
	or	g3,g7,r7
	movq	r4,g0
.Li960R59:	ret
	.align 4
	.globl _B_orp
	#  Function 'B_orp'
	#  Registers used: g0 g1 g2 g3 g4 g5 g6 g7 fp 
_B_orp:
	ld	(g0),g3
	ld	(g1),g2
	or	g3,g2,g4
	ld	4(g0),g3
	ld	4(g1),g2
	or	g3,g2,g5
	ld	8(g0),g3
	ld	8(g1),g2
	or	g3,g2,g6
	ld	12(g0),g2
	ld	12(g1),g0
	or	g2,g0,g7
	movq	g4,g0
.Li960R60:	ret
	.align 4
	.globl _addr
	#  Function 'addr'
	#  Registers used: g0 g1 fp 
_addr:
	addr	g0,g1,g0
.Li960R61:	ret
	.align 4
	.globl _subr
	#  Function 'subr'
	#  Registers used: g0 g1 fp 
_subr:
	subr	g1,g0,g0
.Li960R62:	ret
	.align 4
	.globl _mulr
	#  Function 'mulr'
	#  Registers used: g0 g1 fp 
_mulr:
	mulr	g0,g1,g0
.Li960R63:	ret
	.align 4
	.globl _divr
	#  Function 'divr'
	#  Registers used: g0 g1 fp 
_divr:
	divr	g1,g0,g0
.Li960R64:	ret
	.align 4
	.globl _addrl
	#  Function 'addrl'
	#  Registers used: g0 g1 g2 g3 fp 
_addrl:
	addrl	g0,g2,g0
.Li960R65:	ret
	.align 4
	.globl _subrl
	#  Function 'subrl'
	#  Registers used: g0 g1 g2 g3 fp 
_subrl:
	subrl	g2,g0,g0
.Li960R66:	ret
	.align 4
	.globl _mulrl
	#  Function 'mulrl'
	#  Registers used: g0 g1 g2 g3 fp 
_mulrl:
	mulrl	g0,g2,g0
.Li960R67:	ret
	.align 4
	.globl _divrl
	#  Function 'divrl'
	#  Registers used: g0 g1 g2 g3 fp 
_divrl:
	divrl	g2,g0,g0
.Li960R68:	ret
	.align 4
	.globl _addre
	#  Function 'addre'
	#  Registers used: g0 g1 g2 g3 g4 g5 g6 g7 fp fp0 
	#		   fp1 
_addre:
	movre	g0,fp0
	movre	g4,fp1
	addr	fp0,fp1,fp0
	movre	fp0,g0
.Li960R69:	ret
	.align 4
	.globl _subre
	#  Function 'subre'
	#  Registers used: g0 g1 g2 g3 g4 g5 g6 g7 fp fp0 
	#		   fp1 
_subre:
	movre	g0,fp0
	movre	g4,fp1
	subr	fp1,fp0,fp0
	movre	fp0,g0
.Li960R70:	ret
	.align 4
	.globl _mulre
	#  Function 'mulre'
	#  Registers used: g0 g1 g2 g3 g4 g5 g6 g7 fp fp0 
	#		   fp1 
_mulre:
	movre	g0,fp0
	movre	g4,fp1
	mulr	fp0,fp1,fp0
	movre	fp0,g0
.Li960R71:	ret
	.align 4
	.globl _divre
	#  Function 'divre'
	#  Registers used: g0 g1 g2 g3 g4 g5 g6 g7 fp fp0 
	#		   fp1 
_divre:
	movre	g0,fp0
	movre	g4,fp1
	divr	fp1,fp0,fp0
	movre	fp0,g0
.Li960R72:	ret
	.align 4
	.globl _eqol
	#  Function 'eqol'
	#  Registers used: g0 g1 g2 g3 g4 fp cc 
	#		   
_eqol:
	mov	0,g4
	cmpobne	g2,g0,.L80
	cmpobne	g3,g1,.L80
	mov	1,g4
.L80:
	mov	g4,g0
.Li960R73:	ret
	.align 4
	.globl _neol
	#  Function 'neol'
	#  Registers used: g0 g1 g2 g3 g4 fp cc 
	#		   
_neol:
	mov	0,g4
	cmpobne	g2,g0,.L83
	cmpobe	g3,g1,.L82
.L83:
	mov	1,g4
.L82:
	mov	g4,g0
.Li960R74:	ret
	.align 4
	.globl _lol
	#  Function 'lol'
	#  Registers used: g0 g1 g2 g3 g4 fp cc 
	#		   
_lol:
	mov	0,g4
	cmpobl	g1,g3,.L86
	cmpobne	g1,g3,.L85
	cmpobge	g0,g2,.L85
.L86:
	mov	1,g4
.L85:
	mov	g4,g0
.Li960R75:	ret
	.align 4
	.globl _leol
	#  Function 'leol'
	#  Registers used: g0 g1 g2 g3 g4 fp cc 
	#		   
_leol:
	mov	0,g4
	cmpobl	g3,g1,.L88
	cmpobne	g3,g1,.L89
	cmpobl	g2,g0,.L88
.L89:
	mov	1,g4
.L88:
	mov	g4,g0
.Li960R76:	ret
	.align 4
	.globl _gol
	#  Function 'gol'
	#  Registers used: g0 g1 g2 g3 g4 fp cc 
	#		   
_gol:
	mov	0,g4
	cmpobl	g3,g1,.L92
	cmpobne	g3,g1,.L91
	cmpobge	g2,g0,.L91
.L92:
	mov	1,g4
.L91:
	mov	g4,g0
.Li960R77:	ret
	.align 4
	.globl _geol
	#  Function 'geol'
	#  Registers used: g0 g1 g2 g3 g4 fp cc 
	#		   
_geol:
	mov	0,g4
	cmpobl	g1,g3,.L94
	cmpobne	g1,g3,.L95
	cmpobl	g0,g2,.L94
.L95:
	mov	1,g4
.L94:
	mov	g4,g0
.Li960R78:	ret
	.align 4
	.globl _eqil
	#  Function 'eqil'
	#  Registers used: g0 g1 g2 g3 g4 fp cc 
	#		   
_eqil:
	mov	0,g4
	cmpobne	g2,g0,.L97
	cmpobne	g3,g1,.L97
	mov	1,g4
.L97:
	mov	g4,g0
.Li960R79:	ret
	.align 4
	.globl _neil
	#  Function 'neil'
	#  Registers used: g0 g1 g2 g3 g4 fp cc 
	#		   
_neil:
	mov	0,g4
	cmpobne	g2,g0,.L100
	cmpobe	g3,g1,.L99
.L100:
	mov	1,g4
.L99:
	mov	g4,g0
.Li960R80:	ret
	.align 4
	.globl _lil
	#  Function 'lil'
	#  Registers used: g0 g1 g2 g3 g4 fp cc 
	#		   
_lil:
	mov	0,g4
	cmpibl	g1,g3,.L103
	cmpobne	g1,g3,.L102
	cmpobge	g0,g2,.L102
.L103:
	mov	1,g4
.L102:
	mov	g4,g0
.Li960R81:	ret
	.align 4
	.globl _leil
	#  Function 'leil'
	#  Registers used: g0 g1 g2 g3 g4 fp cc 
	#		   
_leil:
	mov	0,g4
	cmpibl	g3,g1,.L105
	cmpobne	g3,g1,.L106
	cmpobl	g2,g0,.L105
.L106:
	mov	1,g4
.L105:
	mov	g4,g0
.Li960R82:	ret
	.align 4
	.globl _gil
	#  Function 'gil'
	#  Registers used: g0 g1 g2 g3 g4 fp cc 
	#		   
_gil:
	mov	0,g4
	cmpibl	g3,g1,.L109
	cmpobne	g3,g1,.L108
	cmpobge	g2,g0,.L108
.L109:
	mov	1,g4
.L108:
	mov	g4,g0
.Li960R83:	ret
	.align 4
	.globl _geil
	#  Function 'geil'
	#  Registers used: g0 g1 g2 g3 g4 fp cc 
	#		   
_geil:
	mov	0,g4
	cmpibl	g1,g3,.L111
	cmpobne	g1,g3,.L112
	cmpobl	g0,g2,.L111
.L112:
	mov	1,g4
.L111:
	mov	g4,g0
.Li960R84:	ret
	.align 4
	.globl _eqo
	#  Function 'eqo'
	#  Registers used: g0 g1 fp cc 
	#		   
_eqo:
	cmpi	g0,g1
	teste	g0
.Li960R85:	ret
	.align 4
	.globl _neo
	#  Function 'neo'
	#  Registers used: g0 g1 fp cc 
	#		   
_neo:
	cmpi	g0,g1
	testne	g0
.Li960R86:	ret
	.align 4
	.globl _lo
	#  Function 'lo'
	#  Registers used: g0 g1 fp cc 
	#		   
_lo:
	cmpo	g0,g1
	testl	g0
.Li960R87:	ret
	.align 4
	.globl _leo
	#  Function 'leo'
	#  Registers used: g0 g1 fp cc 
	#		   
_leo:
	cmpo	g0,g1
	testle	g0
.Li960R88:	ret
	.align 4
	.globl _go
	#  Function 'go'
	#  Registers used: g0 g1 fp cc 
	#		   
_go:
	cmpo	g0,g1
	testg	g0
.Li960R89:	ret
	.align 4
	.globl _geo
	#  Function 'geo'
	#  Registers used: g0 g1 fp cc 
	#		   
_geo:
	cmpo	g0,g1
	testge	g0
.Li960R90:	ret
	.align 4
	.globl _eqi
	#  Function 'eqi'
	#  Registers used: g0 g1 fp cc 
	#		   
_eqi:
	cmpi	g0,g1
	teste	g0
.Li960R91:	ret
	.align 4
	.globl _nei
	#  Function 'nei'
	#  Registers used: g0 g1 fp cc 
	#		   
_nei:
	cmpi	g0,g1
	testne	g0
.Li960R92:	ret
	.align 4
	.globl _li
	#  Function 'li'
	#  Registers used: g0 g1 fp cc 
	#		   
_li:
	cmpi	g0,g1
	testl	g0
.Li960R93:	ret
	.align 4
	.globl _lei
	#  Function 'lei'
	#  Registers used: g0 g1 fp cc 
	#		   
_lei:
	cmpi	g0,g1
	testle	g0
.Li960R94:	ret
	.align 4
	.globl _gi
	#  Function 'gi'
	#  Registers used: g0 g1 fp cc 
	#		   
_gi:
	cmpi	g0,g1
	testg	g0
.Li960R95:	ret
	.align 4
	.globl _gei
	#  Function 'gei'
	#  Registers used: g0 g1 fp cc 
	#		   
_gei:
	cmpi	g0,g1
	testge	g0
.Li960R96:	ret

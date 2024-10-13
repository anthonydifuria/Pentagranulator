<CsoundSynthesizer>
<CsOptions>
</CsOptions>
<CsInstruments>

sr = 44100
ksmps = 256
nchnls = 2
0dbfs = 1

giLiveBuf1	ftgen		0, 0, 131072, -2, 0
giLiveBuf2	ftgen		0, 0, 131072, -2, 0	
giLiveBuf3	ftgen		0, 0, 131072, -2, 0
giLiveBuf4	ftgen		0, 0, 131072, -2, 0
giLiveBuf5	ftgen		0, 0, 131072, -2, 0

giphasfreq1 = sr/ftlen(giLiveBuf1)
giphasfreq2 = sr/ftlen(giLiveBuf2)
giphasfreq3 = sr/ftlen(giLiveBuf3)
giphasfreq4 = sr/ftlen(giLiveBuf4)
giphasfreq5 = sr/ftlen(giLiveBuf5)

instr 1 ;CONTROLLI GLOBALI e CONTROLLER SINISTRO

kxL invalue "x_L"
kyL invalue "y_L"
kselL invalue "selL"
kselLL invalue "selLL"

if (kselL == 0) then ;CONTROLLA GRANULATORE 1

  if (kselLL == 0) then
  
  kxL scale kxL, 150,0.1
  kyL scale kyL, 0.5,0.01
  
  outvalue "dens1",kxL
  outvalue "dur1" ,kyL
  
  elseif (kselLL == 1) then
 
  kxL scale kxL, 150,0.1
  kyL scale kyL, 4,0.25
  
  outvalue "dens1",kxL
  outvalue "cps1" ,kyL
  
  elseif (kselLL == 2) then
 
  kxL scale kxL, 150,0.1
  kyL scale kyL, 1,0
  
  outvalue "dens1",kxL
  outvalue "vel1" ,kyL
  
  elseif (kselLL == 3) then
 
  kxL scale kxL, 0.5,0.01
  kyL scale kyL, 4,0.25
  
  outvalue "dur1",kxL
  outvalue "cps1" ,kyL
  
  elseif (kselLL == 4) then
 
  kxL scale kxL, 0.5,0.01
  kyL scale kyL, 1,0
  
  outvalue "dur1",kxL
  outvalue "vel1" ,kyL
  
  elseif (kselLL == 5) then
 
  outvalue "fmd1",kxL
  outvalue "pmd1" ,kyL
  
  elseif (kselLL == 6) then
 
  outvalue "frpow1",kxL
  outvalue "prpow1" ,kyL
  
  endif

elseif (kselL == 1) then ;CONTROLLA GRANULATORE 2
  
  if (kselLL == 0) then
  
  kxL scale kxL, 150,0.1
  kyL scale kyL, 0.5,0.01
  
  outvalue "dens2",kxL
  outvalue "dur2" ,kyL
  
  elseif (kselLL == 1) then
 
  kxL scale kxL, 150,0.1
  kyL scale kyL, 4,0.25
  
  outvalue "dens2",kxL
  outvalue "cps2" ,kyL
  
  elseif (kselLL == 2) then
 
  kxL scale kxL, 150,0.1
  kyL scale kyL, 1,0
  
  outvalue "dens2",kxL
  outvalue "vel2" ,kyL
  
  elseif (kselLL == 3) then
 
  kxL scale kxL, 0.5,0.01
  kyL scale kyL, 4,0.25
  
  outvalue "dur2",kxL
  outvalue "cps2" ,kyL
  
  elseif (kselLL == 4) then
 
  kxL scale kxL, 0.5,0.01
  kyL scale kyL, 1,0
  
  outvalue "dur2",kxL
  outvalue "vel2" ,kyL
  
  elseif (kselLL == 5) then
 
  outvalue "fmd2",kxL
  outvalue "pmd2" ,kyL
  
  elseif (kselLL == 6) then
 
  outvalue "frpow2",kxL
  outvalue "prpow2" ,kyL
  
  endif
  
elseif (kselL == 2) then ;CONTROLLA GRANULATORE 3
  
  if (kselLL == 0) then
  
  kxL scale kxL, 150,0.1
  kyL scale kyL, 0.5,0.01
  
  outvalue "dens3",kxL
  outvalue "dur3" ,kyL
  
  elseif (kselLL == 1) then
 
  kxL scale kxL, 150,0.1
  kyL scale kyL, 4,0.25
  
  outvalue "dens3",kxL
  outvalue "cps3" ,kyL
  
  elseif (kselLL == 2) then
 
  kxL scale kxL, 150,0.1
  kyL scale kyL, 1,0
  
  outvalue "dens3",kxL
  outvalue "vel3" ,kyL
  
  elseif (kselLL == 3) then
 
  kxL scale kxL, 0.5,0.01
  kyL scale kyL, 4,0.25
  
  outvalue "dur3",kxL
  outvalue "cps3" ,kyL
  
  elseif (kselLL == 4) then
 
  kxL scale kxL, 0.5,0.01
  kyL scale kyL, 1,0
  
  outvalue "dur3",kxL
  outvalue "vel3" ,kyL
  
  elseif (kselLL == 5) then
 
  outvalue "fmd3",kxL
  outvalue "pmd3" ,kyL
  
  elseif (kselLL == 6) then
 
  outvalue "frpow3",kxL
  outvalue "prpow3" ,kyL
  
  endif
  
elseif (kselL == 3) then ;CONTROLLA GRANULATORE 4

  if (kselLL == 0) then
  
  kxL scale kxL, 150,0.1
  kyL scale kyL, 0.5,0.01
  
  outvalue "dens4",kxL
  outvalue "dur4" ,kyL
  
  elseif (kselLL == 1) then
 
  kxL scale kxL, 150,0.1
  kyL scale kyL, 4,0.25
  
  outvalue "dens4",kxL
  outvalue "cps4" ,kyL
  
  elseif (kselLL == 2) then
 
  kxL scale kxL, 150,0.1
  kyL scale kyL, 1,0
  
  outvalue "dens4",kxL
  outvalue "vel4" ,kyL
  
  elseif (kselLL == 3) then
 
  kxL scale kxL, 0.5,0.01
  kyL scale kyL, 4,0.25
  
  outvalue "dur4",kxL
  outvalue "cps4" ,kyL
  
  elseif (kselLL == 4) then
 
  kxL scale kxL, 0.5,0.01
  kyL scale kyL, 1,0
  
  outvalue "dur4",kxL
  outvalue "vel4" ,kyL
  
  elseif (kselLL == 5) then
 
  outvalue "fmd4",kxL
  outvalue "pmd4" ,kyL
  
  elseif (kselLL == 6) then
 
  outvalue "frpow4",kxL
  outvalue "prpow4" ,kyL
  
  endif

endif  
  
endin

instr 2

kamp   invalue "ampi"
kintvl invalue "intv"
kfreq1 invalue "wfreq1"
kfreq2 invalue "wfreq2"
kfreq3 invalue "wfreq3"
kfreq4 invalue "wfreq4"
kfreq5 invalue "wfreq5"
kcutoff1 invalue "cutoff1"
kcutoff2 invalue "cutoff2"
kcutoff3 invalue "cutoff3"
kcutoff4 invalue "cutoff4"
kcutoff5 invalue "cutoff5"
kfeedback1 invalue "wfeed1"
kfeedback2 invalue "wfeed2"
kfeedback3 invalue "wfeed3"
kfeedback4 invalue "wfeed4"
kfeedback5 invalue "wfeed5"

asig mpulse kamp, kintvl
ares1 wguide1 asig, kfreq1, kcutoff1, kfeedback1
ares2 wguide1 asig, kfreq2, kcutoff2, kfeedback2
ares3 wguide1 asig, kfreq3, kcutoff3, kfeedback3
ares4 wguide1 asig, kfreq4, kcutoff4, kfeedback4
ares5 wguide1 asig, kfreq5, kcutoff5, kfeedback5

garesum = (ares1+ares2+ares3+ares4+ares5)

;outs aresum,aresum


endin


instr 3 ;INPUT LIVE

kLin invalue "Lin"
kRin invalue "Rin"

aL inch 1
aR inch 2
 
krmsL rms aL * kLin
krmsR rms aR * kRin

outvalue "disp_rmsL", krmsL ;* 0.0002
outvalue "disp_rmsR", krmsR ;* 0.0002

gagrainL = aL * kLin
gagrainR = aR * kRin 

endin

instr 4; SYNTH

kampsin invalue "ampsin"
kfreqsin invalue "kfreqsin"
kampfm invalue "ampfm"
kfreqfm invalue "freqfm"
kcar invalue "car"
kmod invalue "mod"
kndx invalue "ndx"
ifn = 1

katt invalue "att"
krel invalue "krel"
kselsynth1 invalue "selsynth1"
kselsynth2 invalue "selsynth2"

if (kselsynth1 == 0)then

asynthL inch 1
asynthR inch 2

alive follow2 asynthL+asynthR, katt, krel

kcps, krms pitchamdf alive, 50, 2000


 if (kselsynth2 == 0)then
kfreqsin = kcps
kampsin  = krms

gaSIN oscili kampsin, kfreqsin, ifn

elseif (kselsynth2 == 1) then

kfreqfm = kcps
kampfm  = krms
gaFM foscili kampfm, kfreqfm, kcar, kmod, kndx, ifn 

endif

elseif (kselsynth1 == 1)then

if (kselsynth2 == 0)then

gaSIN oscili kampsin, kfreqsin, ifn

elseif (kselsynth2 == 1)then

gaFM foscili kampfm, kfreqfm, kcar, kmod, kndx, ifn 

endif

elseif (kselsynth1 == 2)then

kfreq invalue "freqfib"

afib1 oscili 1,kfreq*1,1
afib2 oscili 1,kfreq*1,1
afib3 oscili 1,kfreq*2,1
afib4 oscili 1,kfreq*3,1
afib5 oscili 1,kfreq*5,1
afib6 oscili 1,kfreq*8,1
afib7 oscili 1,kfreq*13,1

gafib = (afib1+afib2+afib3+afib4+afib5+afib6+afib7)/7

endif

;outs gaSIN,gaFM

endin

instr 5 ;PLAY_FILE1
kfile1 invalue "file1"
	Sfile		invalue		"_Browse1"
	kskip		invalue		"skip"
	kloop		invalue		"loop"
	ichn		filenchnls		Sfile; check if mono or stereo
	if ichn == 1 then		;mono
		aL	diskin2	Sfile, 1, i(kskip), i(kloop), 0, 1
		gagrain1	=	aL
		krmsfile1 rms gagrain1 * kfile1
		outvalue "disp_rmsfile", krmsfile1
	else				;stereo
		gagrain1, gagrain1	diskin2		Sfile, 1, i(kskip), i(kloop), 0, 1
	      krmsfile1 rms gagrain1 * kfile1
	      outvalue "disp_rmsfile", krmsfile1
	endif
	
endin

instr 6 ;STOP FILE1
	turnoff2		4, 0, 0
endin

instr 7 ;PLAY FILE2
kfile2 invalue "file2"
	Sfile		invalue		"_Browse2"
	kskip		invalue		"skip"
	kloop		invalue		"loop"
	ichn		filenchnls		Sfile; check if mono or stereo
	if ichn == 1 then		;mono
		aL	diskin2	Sfile, 1, i(kskip), i(kloop), 0, 1
		gagrain2	=	aL
		krmsfile2 rms gagrain2 * kfile2
		outvalue "disp_rmsfile", krmsfile2
	else				;stereo
		gagrain2, gagrain2 	diskin2		Sfile, 1, i(kskip), i(kloop), 0, 1
		krmsfile2 rms gagrain2 * kfile2
		outvalue "disp_rmsfile", krmsfile2
	endif
	
endin

instr 8 ;STOP FILE2
	turnoff2		6, 0, 0
endin

instr 9 ;PLAY FILE3
kfile3 invalue "file3"
	Sfile		invalue		"_Browse3"
	kskip		invalue		"skip"
	kloop		invalue		"loop"
	ichn		filenchnls		Sfile; check if mono or stereo
	if ichn == 1 then		;mono
		aL	diskin2	Sfile, 1, i(kskip), i(kloop), 0, 1
		gagrain3	=	aL
		krmsfile3 rms gagrain3 * kfile3
		outvalue "disp_rmsfile", krmsfile3
	else				;stereo
		gagrain3, gagrain3	diskin2		Sfile, 1, i(kskip), i(kloop), 0, 1
		krmsfile3 rms gagrain3 * kfile3
		outvalue "disp_rmsfile", krmsfile3
	endif
	
endin

instr 10 ;STOP FILE3
	turnoff2		8, 0, 0
endin

instr 11 ;PLAY FILE4
      kfile4 invalue "file4"
	Sfile		invalue		"_Browse4"
	kskip		invalue		"skip"
	kloop		invalue		"loop"
	ichn		filenchnls		Sfile; check if mono or stereo
	if ichn == 1 then		;mono
		aL	diskin2	Sfile, 1, i(kskip), i(kloop), 0, 1
		gagrain4	=	aL
		krmsfile4 rms gagrain4 * kfile4
		outvalue "disp_rmsfile", krmsfile4
	else				;stereo
		gagrain4, gagrain4	diskin2		Sfile, 1, i(kskip), i(kloop), 0, 1
	      krmsfile4 rms gagrain4 * kfile4
		outvalue "disp_rmsfile", krmsfile4
	endif
	
endin

instr 12 ;STOP FILE4
	turnoff2		10, 0, 0
endin

instr 13;INITIALITATION GRANULATION 1

kfreeze1	invalue	"freeze1"; if checked, freeze writing (and reading) of the buffer
kphasfreq1	=		(kfreeze1 == 1 ? 0 : giphasfreq1)
awritpnt	phasor		kphasfreq1
		tablew		gagrainL+gagrainR+gagrain1+gaSIN+gaFM+garesum+gafib, awritpnt, giLiveBuf1, 1, 0, 1
		
kfreeze2	invalue	"freeze2"; if checked, freeze writing (and reading) of the buffer
kphasfreq2	=		(kfreeze2 == 1 ? 0 : giphasfreq2)
awritpnt	phasor		kphasfreq2
		tablew		gagrainL+gagrainR+gagrain2+gaSIN+gaFM+garesum+gafib, awritpnt, giLiveBuf2, 1, 0, 1
	
kfreeze3	invalue	"freeze3"; if checked, freeze writing (and reading) of the buffer
kphasfreq3	=		(kfreeze3 == 1 ? 0 : giphasfreq3)
awritpnt	phasor		kphasfreq3
		tablew		gagrainL+gagrainR+gagrain3+gaSIN+gaFM+garesum+gafib, awritpnt, giLiveBuf3, 1, 0, 1
		
kfreeze4	invalue	"freeze4"; if checked, freeze writing (and reading) of the buffer
kphasfreq4	=		(kfreeze4 == 1 ? 0 : giphasfreq4)
awritpnt	phasor		kphasfreq4
		tablew		gagrainL+gagrainR+gagrain4+gaSIN+gaFM+garesum+gafib, awritpnt, giLiveBuf4, 1, 0, 1

						
gagrainL = 0
gagrainR = 0
gagrain1 = 0
gagrain2 = 0
gagrain3 = 0
gagrain4 = 0
garesum = 0
gaSIN = 0
gaFM = 0	

endin

instr 14;GRAN1--FUNCTION

gkfn1 invalue "kfn1"
kvel1 invalue "vel1"
gkramp1 phasor kvel1

endin

instr 15 ;GRANULATOR 1

kdens1 invalue "dens1"
kdur1 invalue "dur1"
kcps1 invalue "cps1"
kfmd1 invalue "fmd1"
kpmd1 invalue "pmd1"
kfrpow1 invalue "frpow1"
kprpow1 invalue "prpow1"

reset:
      ifn1 = i(gkfn1)
      iphs1 = i(gkramp1)
timout 0,0.1,cont 
reinit reset
cont: 

gares1 grain3 giphasfreq1*kcps1 , iphs1, kfmd1, kpmd1, kdur1+0.001, kdens1, 100, giLiveBuf1, 20 + ifn1, kfrpow1, kprpow1

endin

instr 16;GRAN2--FUNCTION

gkfn2 invalue "kfn2"
kvel2 invalue "vel2"
gkramp2 phasor kvel2

endin

instr 17 ;GRANULATOR 2

kdens2 invalue "dens2"
kdur2 invalue "dur2"
kcps2 invalue "cps2"
kfmd2 invalue "fmd2"
kpmd2 invalue "pmd2"
kfrpow2 invalue "frpow2"
kprpow2 invalue "prpow2"

reset:
      ifn2 = i(gkfn2)
      iphs2 = i(gkramp2)
timout 0,0.1,cont 
reinit reset
cont: 

gares2 grain3 giphasfreq2*kcps2 , iphs2, kfmd2, kpmd2, kdur2+0.001, kdens2, 100, giLiveBuf2, 20 + ifn2, kfrpow2, kprpow2

endin

instr 18;GRAN3--FUNCTION

gkfn3 invalue "kfn3"
kvel3 invalue "vel3"
gkramp3 phasor kvel3

endin

instr 20 ;GRANULATOR 3

kdens3 invalue "dens3"
kdur3 invalue "dur3"
kcps3 invalue "cps3"
kfmd3 invalue "fmd3"
kpmd3 invalue "pmd3"
kfrpow3 invalue "frpow3"
kprpow3 invalue "prpow3"

reset:
      ifn3 = i(gkfn3)
      iphs3 = i(gkramp3)
timout 0,0.1,cont 
reinit reset
cont: 

gares3 grain3 giphasfreq3*kcps3 , iphs3, kfmd3, kpmd3, kdur3+0.001, kdens3, 100, giLiveBuf3, 20 + ifn3, kfrpow3, kprpow3

endin

instr 23;GRAN4--FUNCTION

gkfn4 invalue "kfn4"
kvel4 invalue "vel4"
gkramp4 phasor kvel4

endin

instr 25 ;GRANULATOR 4

kdens4 invalue "dens4"
kdur4 invalue "dur4"
kcps4 invalue "cps4"
kfmd4 invalue "fmd4"
kpmd4 invalue "pmd4"
kfrpow4 invalue "frpow4"
kprpow4 invalue "prpow4"

reset:
      ifn4 = i(gkfn4)
      iphs4 = i(gkramp4)
timout 0,0.1,cont 
reinit reset
cont: 

gares4 grain3 giphasfreq4*kcps4 , iphs4, kfmd4, kpmd4, kdur4+0.001, kdens4, 100, giLiveBuf4, 20 + ifn4, kfrpow4,kprpow4

endin

instr 27;GRAN5--FUNCTION

kfreeze5	invalue	"freeze5"; if checked, freeze writing (and reading) of the buffer
kphasfreq5	=		(kfreeze5 == 1 ? 0 : giphasfreq5)
awritpnt	phasor		kphasfreq5
		tablew		gares1+gares2+gares3+gares4, awritpnt, giLiveBuf5, 1, 0, 1
		
endin

instr 28;GRAN5--FUNCTION

gkfn5 invalue "kfn5"
kvel5 invalue "vel5"
gkramp5 phasor kvel5

endin

instr 30 ;GRANULATOR 5
kselgrain5 invalue "selgrain5"


ain inch 1

kcps, krms pitchamdf ain, 50, 1000

kdens scale kcps, 0.06, 0.0001
kdur  scale krms, 0.5,0.001
kcpss  scale kcps, 0.005,0.0001
kfmd  scale kcps, 0.0012,0.0001
kpmd  scale krms, 1,0
kfrpow  scale kcps, 0.0012,0.0001
kprpow  scale krms, 1,0

if (kselgrain5 == 0) then

kdens5 invalue "dens5"
kdur5 invalue "dur5"
kcps5 invalue "cps5"
kfmd5 invalue "fmd5"
kpmd5 invalue "pmd5"
kfrpow5 invalue "frpow5"
kprpow5 invalue "prpow5"

elseif (kselgrain5 == 1) then
outvalue "dens5", kdens
outvalue "dur5", kdur
outvalue "cps5", kcpss
outvalue "fmd5", kfmd
outvalue "pmd5", kpmd
outvalue "frpow5", kfrpow
outvalue "prpow5", kprpow

kdens5 invalue "dens5"
kdur5 invalue "dur5"
kcps5 invalue "cps5"
kfmd5 invalue "fmd5"
kpmd5 invalue "pmd5"
kfrpow5 invalue "frpow5"
kprpow5 invalue "prpow5"

endif

reset:
      ifn5 = i(gkfn5)
      iphs5 = i(gkramp5)
timout 0,0.1,cont 
reinit reset
cont: 

gares5 grain3 giphasfreq5*kcps5 , iphs5, kfmd5, kpmd5, kdur5+0.001, kdens5, 100, giLiveBuf5, 20 + ifn5, kfrpow5, kprpow5 

endin

instr 38 ;AUTOMAZIONE PAN

kpan1 oscil 1,0.11,1
kpan2 oscil 1,0.2,1
kpan3 oscil 1,-0.11,1
kpan4 oscil 1,-0.2,1

outvalue "pan1",kpan1
outvalue "pan2",kpan2
outvalue "pan3",kpan3
outvalue "pan4",kpan4

endin

instr 39 ;AUTOMAZIONE VOL

kvol1 oscil 1,0.11,1
kvol2 oscil 1,0.2,1
kvol3 oscil 1,-0.11,1
kvol4 oscil 1,-0.2,1

outvalue "gres1",kvol1
outvalue "gres2",kvol2
outvalue "gres3",kvol3
outvalue "gres4",kvol4

endin

instr 40; CONTROLLO USCITE

kseluscite invalue "seluscite"

if (kseluscite == 0) then

kresx gauss 10
kx scale kresx,1,0
kresy gauss 10
ky scale kresy,1,0

kx oscili kx,ky,1
ky oscili ky,kx,1

kx port kx, 1
ky port ky, 1

outvalue "x",kx+0.5
outvalue "y",ky+0.5

elseif (kseluscite == 1) then

kx invalue "x"
ky invalue "y"
kx scale kx, 1, -1
ky scale ky, 1, -1

elseif (kseluscite == 2) then

kx oscili 0.5,0.2,1
ky oscili 0.5,0.1,1
 
outvalue "x",kx+0.5
outvalue "y",ky+0.5

elseif (kseluscite == 3) then

kmod1 invalue "mod1"
kmod2 invalue "mod2"
kmod3 invalue "mod3"
kmod4 invalue "mod4"
kmod5 invalue "mod5"
kmod6 invalue "mod6"
kmod7 invalue "mod7"
kmod8 invalue "mod8"

kmodd1 oscil kmod1,kmod2,1
kmodd2 oscil kmod3,kmod4,1
kmodd3 oscil kmod5,kmod6,1
kmodd4 oscil kmod7,kmod8,1

kampx  oscil 1*kmodd1,1*kmodd1,1
kfreqx oscil 1*kmodd2,1*kmodd2,1
kampy  oscil 1*kmodd3,1*kmodd3,1
kfreqy oscil 1*kmodd4,1*kmodd4,1 

kx oscili kampx,kfreqx,1
ky oscili kampy,kfreqy,1

outvalue "x",kx+0.5
outvalue "y",ky+0.5

endif


ktime = 0
ifn = 0

gares1, a2, a3, a4 space gares1, ifn, ktime, 0, kx, ky
a2 = 0
a3 = 0
a4 = 0

a1, gares2, a3, a4 space gares2, ifn, ktime, 0, kx, ky
a1 = 0
a3 = 0
a4 = 0

a1, a2, gares3, a4 space gares3, ifn, ktime, 0, kx, ky
a1 = 0
a2 = 0
a4 = 0

a1, a2, a3, gares4 space gares4, ifn, ktime, 0, kx, ky
a1 = 0
a2 = 0
a3 = 0

kgres1 invalue "gres1"
kgres2 invalue "gres2"
kgres3 invalue "gres3"
kgres4 invalue "gres4"
kgres5 invalue "gres5"

gares1 = gares1*kgres1
gares2 = gares2*kgres2
gares3 = gares3*kgres3
gares4 = gares4*kgres4
gares5 = gares5*kgres5

kpan1 invalue "pan1"
kpan2 invalue "pan2"
kpan3 invalue "pan3"
kpan4 invalue "pan4"
kpan5 invalue "pan5"

aoutL1, aoutR1 pan2 gares1, kpan1
aoutL2, aoutR2 pan2 gares2, kpan2
aoutL3, aoutR3 pan2 gares3, kpan3
aoutL4, aoutR4 pan2 gares4, kpan4
aoutL5, aoutR5 pan2 gares5, kpan5

koutL invalue "outmaster"
koutR invalue "outmaster"

koL rms aoutL1+aoutL2+aoutL3+aoutL4+aoutL5 * koutL
koR rms aoutR1+aoutR2+aoutR3+aoutR4+aoutR5 * koutR

outvalue "disp_rmsout", koL
outvalue "disp_rmsout", koR

gaoutL = (aoutL1+aoutL2+aoutL3+aoutL4+aoutL5)/5
gaoutR = (aoutR1+aoutR2+aoutR3+aoutR4+aoutR5)/5

outs gaoutL*koutL,gaoutR*koutR

gares1 = 0
gares2 = 0
gares3 = 0
gares4 = 0
gares5 = 0

endin

instr 50;REVERB

krevlev invalue "levrev"
kRoomSize invalue "room"
kHFDamp invalue "damp"

aoutL, aoutR freeverb gaoutL*krevlev, gaoutR*krevlev, kRoomSize, kHFDamp

outs aoutL,aoutR

endin

</CsInstruments>
<CsScore>

f 1 0 1024 10 1
; f 2 0 0 28 "move"

f20 0 8192 20 2 1
f21 0 1024 7 0 128 1 768 1 128 0

;i 2  0 36000 
i 13 0 36000
i 14 0 36000
i 16 0 36000
i 18 0 36000
i 23 0 36000
i 27 0 36000
i 28 0 36000
i 40 0 36000
i 50 0 36000
e 36000

</CsScore>
</CsoundSynthesizer>
<bsbPanel>
 <label>Widgets</label>
 <objectName/>
 <x>2</x>
 <y>38</y>
 <width>1432</width>
 <height>852</height>
 <visible>true</visible>
 <uuid/>
 <bgcolor mode="background">
  <r>0</r>
  <g>0</g>
  <b>0</b>
 </bgcolor>
 <bsbObject version="2" type="BSBLabel">
  <objectName/>
  <x>2</x>
  <y>6</y>
  <width>1031</width>
  <height>837</height>
  <uuid>{3dff4130-6e21-4521-99b5-f6331fa14252}</uuid>
  <visible>true</visible>
  <midichan>0</midichan>
  <midicc>0</midicc>
  <label/>
  <alignment>left</alignment>
  <font>Arial</font>
  <fontsize>12</fontsize>
  <precision>3</precision>
  <color>
   <r>255</r>
   <g>255</g>
   <b>255</b>
  </color>
  <bgcolor mode="background">
   <r>0</r>
   <g>0</g>
   <b>128</b>
  </bgcolor>
  <bordermode>border</bordermode>
  <borderradius>10</borderradius>
  <borderwidth>5</borderwidth>
 </bsbObject>
 <bsbObject version="2" type="BSBLabel">
  <objectName/>
  <x>513</x>
  <y>9</y>
  <width>364</width>
  <height>268</height>
  <uuid>{1693ccee-f5b1-41a0-804c-4e36a3e958b8}</uuid>
  <visible>true</visible>
  <midichan>0</midichan>
  <midicc>0</midicc>
  <label/>
  <alignment>left</alignment>
  <font>Arial</font>
  <fontsize>12</fontsize>
  <precision>3</precision>
  <color>
   <r>255</r>
   <g>255</g>
   <b>255</b>
  </color>
  <bgcolor mode="background">
   <r>0</r>
   <g>0</g>
   <b>0</b>
  </bgcolor>
  <bordermode>border</bordermode>
  <borderradius>99</borderradius>
  <borderwidth>5</borderwidth>
 </bsbObject>
 <bsbObject version="2" type="BSBLabel">
  <objectName/>
  <x>667</x>
  <y>275</y>
  <width>363</width>
  <height>267</height>
  <uuid>{ed136603-5048-4a80-8caf-7bfc2b1b7e6f}</uuid>
  <visible>true</visible>
  <midichan>0</midichan>
  <midicc>0</midicc>
  <label/>
  <alignment>left</alignment>
  <font>Arial</font>
  <fontsize>12</fontsize>
  <precision>3</precision>
  <color>
   <r>255</r>
   <g>255</g>
   <b>255</b>
  </color>
  <bgcolor mode="background">
   <r>0</r>
   <g>0</g>
   <b>0</b>
  </bgcolor>
  <bordermode>border</bordermode>
  <borderradius>99</borderradius>
  <borderwidth>5</borderwidth>
 </bsbObject>
 <bsbObject version="2" type="BSBLabel">
  <objectName/>
  <x>270</x>
  <y>511</y>
  <width>491</width>
  <height>327</height>
  <uuid>{f49087bc-5601-4bfa-83f2-1cce3cb23a19}</uuid>
  <visible>true</visible>
  <midichan>0</midichan>
  <midicc>0</midicc>
  <label/>
  <alignment>left</alignment>
  <font>Arial</font>
  <fontsize>12</fontsize>
  <precision>3</precision>
  <color>
   <r>255</r>
   <g>255</g>
   <b>255</b>
  </color>
  <bgcolor mode="background">
   <r>0</r>
   <g>0</g>
   <b>0</b>
  </bgcolor>
  <bordermode>border</bordermode>
  <borderradius>99</borderradius>
  <borderwidth>5</borderwidth>
 </bsbObject>
 <bsbObject version="2" type="BSBLabel">
  <objectName/>
  <x>148</x>
  <y>9</y>
  <width>366</width>
  <height>268</height>
  <uuid>{1e79f767-fe93-4c6d-8d71-75ab43884dc1}</uuid>
  <visible>true</visible>
  <midichan>0</midichan>
  <midicc>0</midicc>
  <label/>
  <alignment>left</alignment>
  <font>Arial</font>
  <fontsize>12</fontsize>
  <precision>3</precision>
  <color>
   <r>255</r>
   <g>255</g>
   <b>255</b>
  </color>
  <bgcolor mode="background">
   <r>0</r>
   <g>0</g>
   <b>0</b>
  </bgcolor>
  <bordermode>border</bordermode>
  <borderradius>99</borderradius>
  <borderwidth>5</borderwidth>
 </bsbObject>
 <bsbObject version="2" type="BSBLabel">
  <objectName/>
  <x>1</x>
  <y>276</y>
  <width>363</width>
  <height>267</height>
  <uuid>{04899201-4e65-430d-9eca-6807d67efb96}</uuid>
  <visible>true</visible>
  <midichan>0</midichan>
  <midicc>0</midicc>
  <label/>
  <alignment>left</alignment>
  <font>Arial</font>
  <fontsize>12</fontsize>
  <precision>3</precision>
  <color>
   <r>255</r>
   <g>255</g>
   <b>255</b>
  </color>
  <bgcolor mode="background">
   <r>0</r>
   <g>0</g>
   <b>0</b>
  </bgcolor>
  <bordermode>border</bordermode>
  <borderradius>99</borderradius>
  <borderwidth>5</borderwidth>
 </bsbObject>
 <bsbObject version="2" type="BSBLabel">
  <objectName/>
  <x>1034</x>
  <y>6</y>
  <width>390</width>
  <height>79</height>
  <uuid>{ca9a7290-40ec-4ad3-89c1-33a84877058e}</uuid>
  <visible>true</visible>
  <midichan>0</midichan>
  <midicc>0</midicc>
  <label>PENTAGRANULATOR ver 0.2
by Anthony Di Furia</label>
  <alignment>center</alignment>
  <font>Apple Chancery</font>
  <fontsize>28</fontsize>
  <precision>3</precision>
  <color>
   <r>255</r>
   <g>255</g>
   <b>255</b>
  </color>
  <bgcolor mode="nobackground">
   <r>76</r>
   <g>76</g>
   <b>76</b>
  </bgcolor>
  <bordermode>border</bordermode>
  <borderradius>10</borderradius>
  <borderwidth>5</borderwidth>
 </bsbObject>
 <bsbObject version="2" type="BSBLabel">
  <objectName/>
  <x>1034</x>
  <y>86</y>
  <width>390</width>
  <height>515</height>
  <uuid>{00b9fdfd-e850-4ded-860d-bdbc74a48a07}</uuid>
  <visible>true</visible>
  <midichan>0</midichan>
  <midicc>0</midicc>
  <label>MIXER</label>
  <alignment>center</alignment>
  <font>Arial</font>
  <fontsize>27</fontsize>
  <precision>3</precision>
  <color>
   <r>204</r>
   <g>204</g>
   <b>204</b>
  </color>
  <bgcolor mode="background">
   <r>102</r>
   <g>102</g>
   <b>102</b>
  </bgcolor>
  <bordermode>border</bordermode>
  <borderradius>10</borderradius>
  <borderwidth>5</borderwidth>
 </bsbObject>
 <bsbObject version="2" type="BSBLabel">
  <objectName/>
  <x>1065</x>
  <y>410</y>
  <width>51</width>
  <height>21</height>
  <uuid>{f447297a-992d-43ae-abd5-a13c36e439f4}</uuid>
  <visible>true</visible>
  <midichan>0</midichan>
  <midicc>-3</midicc>
  <label>GRAN 1</label>
  <alignment>left</alignment>
  <font>Arial</font>
  <fontsize>12</fontsize>
  <precision>3</precision>
  <color>
   <r>204</r>
   <g>204</g>
   <b>204</b>
  </color>
  <bgcolor mode="nobackground">
   <r>255</r>
   <g>255</g>
   <b>255</b>
  </bgcolor>
  <bordermode>noborder</bordermode>
  <borderradius>1</borderradius>
  <borderwidth>1</borderwidth>
 </bsbObject>
 <bsbObject version="2" type="BSBLabel">
  <objectName/>
  <x>1270</x>
  <y>410</y>
  <width>50</width>
  <height>22</height>
  <uuid>{4422514e-1677-4b3c-bd3c-35968d31ab71}</uuid>
  <visible>true</visible>
  <midichan>0</midichan>
  <midicc>-3</midicc>
  <label>GRAN 5</label>
  <alignment>left</alignment>
  <font>Arial</font>
  <fontsize>12</fontsize>
  <precision>3</precision>
  <color>
   <r>204</r>
   <g>204</g>
   <b>204</b>
  </color>
  <bgcolor mode="nobackground">
   <r>255</r>
   <g>255</g>
   <b>255</b>
  </bgcolor>
  <bordermode>noborder</bordermode>
  <borderradius>1</borderradius>
  <borderwidth>1</borderwidth>
 </bsbObject>
 <bsbObject version="2" type="BSBLabel">
  <objectName/>
  <x>1167</x>
  <y>410</y>
  <width>51</width>
  <height>21</height>
  <uuid>{ebef62bb-18c9-42cb-a4ea-d6905789e44a}</uuid>
  <visible>true</visible>
  <midichan>0</midichan>
  <midicc>-3</midicc>
  <label>GRAN 3</label>
  <alignment>left</alignment>
  <font>Arial</font>
  <fontsize>12</fontsize>
  <precision>3</precision>
  <color>
   <r>204</r>
   <g>204</g>
   <b>204</b>
  </color>
  <bgcolor mode="nobackground">
   <r>255</r>
   <g>255</g>
   <b>255</b>
  </bgcolor>
  <bordermode>noborder</bordermode>
  <borderradius>1</borderradius>
  <borderwidth>1</borderwidth>
 </bsbObject>
 <bsbObject version="2" type="BSBLabel">
  <objectName/>
  <x>1219</x>
  <y>410</y>
  <width>50</width>
  <height>22</height>
  <uuid>{ee898193-8566-491c-ae1a-9fcb4fe23e9f}</uuid>
  <visible>true</visible>
  <midichan>0</midichan>
  <midicc>-3</midicc>
  <label>GRAN 4</label>
  <alignment>left</alignment>
  <font>Arial</font>
  <fontsize>12</fontsize>
  <precision>3</precision>
  <color>
   <r>204</r>
   <g>204</g>
   <b>204</b>
  </color>
  <bgcolor mode="nobackground">
   <r>255</r>
   <g>255</g>
   <b>255</b>
  </bgcolor>
  <bordermode>noborder</bordermode>
  <borderradius>1</borderradius>
  <borderwidth>1</borderwidth>
 </bsbObject>
 <bsbObject version="2" type="BSBLabel">
  <objectName/>
  <x>1117</x>
  <y>410</y>
  <width>49</width>
  <height>21</height>
  <uuid>{1785e88c-6891-496a-87cd-43b2607a3ac6}</uuid>
  <visible>true</visible>
  <midichan>0</midichan>
  <midicc>-3</midicc>
  <label>GRAN 2</label>
  <alignment>left</alignment>
  <font>Arial</font>
  <fontsize>12</fontsize>
  <precision>3</precision>
  <color>
   <r>204</r>
   <g>204</g>
   <b>204</b>
  </color>
  <bgcolor mode="nobackground">
   <r>255</r>
   <g>255</g>
   <b>255</b>
  </bgcolor>
  <bordermode>noborder</bordermode>
  <borderradius>1</borderradius>
  <borderwidth>1</borderwidth>
 </bsbObject>
 <bsbObject version="2" type="BSBButton">
  <objectName/>
  <x>267</x>
  <y>12</y>
  <width>137</width>
  <height>26</height>
  <uuid>{a93db68e-72ba-4113-aea9-a45093bfdfa3}</uuid>
  <visible>true</visible>
  <midichan>0</midichan>
  <midicc>0</midicc>
  <type>event</type>
  <pressedValue>1.00000000</pressedValue>
  <stringvalue/>
  <text>GRANULATOR 1</text>
  <image>/</image>
  <eventLine>i15 0 -1</eventLine>
  <latch>true</latch>
  <latched>false</latched>
 </bsbObject>
 <bsbObject version="2" type="BSBButton">
  <objectName/>
  <x>386</x>
  <y>515</y>
  <width>138</width>
  <height>28</height>
  <uuid>{33ab612c-4041-4e07-b3e7-fef9aac4954e}</uuid>
  <visible>true</visible>
  <midichan>0</midichan>
  <midicc>0</midicc>
  <type>event</type>
  <pressedValue>1.00000000</pressedValue>
  <stringvalue/>
  <text>GRANULATOR 5</text>
  <image>/</image>
  <eventLine>i30 0 -10</eventLine>
  <latch>true</latch>
  <latched>false</latched>
 </bsbObject>
 <bsbObject version="2" type="BSBButton">
  <objectName/>
  <x>787</x>
  <y>281</y>
  <width>137</width>
  <height>26</height>
  <uuid>{21e48f96-a3fa-40fb-80be-7a46e258ee79}</uuid>
  <visible>true</visible>
  <midichan>0</midichan>
  <midicc>0</midicc>
  <type>event</type>
  <pressedValue>1.00000000</pressedValue>
  <stringvalue/>
  <text>GRANULATOR 4</text>
  <image>/</image>
  <eventLine>i25 0 -10</eventLine>
  <latch>true</latch>
  <latched>false</latched>
 </bsbObject>
 <bsbObject version="2" type="BSBButton">
  <objectName/>
  <x>116</x>
  <y>282</y>
  <width>137</width>
  <height>26</height>
  <uuid>{9602f2db-b93d-49c5-9718-d4d6e4738c1c}</uuid>
  <visible>true</visible>
  <midichan>0</midichan>
  <midicc>0</midicc>
  <type>event</type>
  <pressedValue>1.00000000</pressedValue>
  <stringvalue/>
  <text>GRANULATOR 3</text>
  <image>/</image>
  <eventLine>i20 0 -10</eventLine>
  <latch>true</latch>
  <latched>false</latched>
 </bsbObject>
 <bsbObject version="2" type="BSBButton">
  <objectName/>
  <x>632</x>
  <y>9</y>
  <width>138</width>
  <height>28</height>
  <uuid>{d04f0673-0bfa-4f10-bc94-cd9c9773c399}</uuid>
  <visible>true</visible>
  <midichan>0</midichan>
  <midicc>0</midicc>
  <type>event</type>
  <pressedValue>1.00000000</pressedValue>
  <stringvalue/>
  <text>GRANULATOR 2</text>
  <image>/</image>
  <eventLine>i17 0 -10</eventLine>
  <latch>true</latch>
  <latched>false</latched>
 </bsbObject>
 <bsbObject version="2" type="BSBLabel">
  <objectName/>
  <x>1034</x>
  <y>602</y>
  <width>390</width>
  <height>77</height>
  <uuid>{93063d16-8e39-4582-abd9-68232b6a8da0}</uuid>
  <visible>true</visible>
  <midichan>0</midichan>
  <midicc>0</midicc>
  <label>REVERB

</label>
  <alignment>center</alignment>
  <font>Arial</font>
  <fontsize>12</fontsize>
  <precision>3</precision>
  <color>
   <r>102</r>
   <g>102</g>
   <b>102</b>
  </color>
  <bgcolor mode="background">
   <r>204</r>
   <g>204</g>
   <b>204</b>
  </bgcolor>
  <bordermode>border</bordermode>
  <borderradius>10</borderradius>
  <borderwidth>5</borderwidth>
 </bsbObject>
 <bsbObject version="2" type="BSBLabel">
  <objectName/>
  <x>174</x>
  <y>40</y>
  <width>40</width>
  <height>22</height>
  <uuid>{adee9d84-d75c-4792-abc9-2a9f891c9b45}</uuid>
  <visible>true</visible>
  <midichan>0</midichan>
  <midicc>-3</midicc>
  <label>DENS</label>
  <alignment>left</alignment>
  <font>Arial Black</font>
  <fontsize>12</fontsize>
  <precision>3</precision>
  <color>
   <r>204</r>
   <g>204</g>
   <b>204</b>
  </color>
  <bgcolor mode="nobackground">
   <r>255</r>
   <g>255</g>
   <b>255</b>
  </bgcolor>
  <bordermode>noborder</bordermode>
  <borderradius>1</borderradius>
  <borderwidth>1</borderwidth>
 </bsbObject>
 <bsbObject version="2" type="BSBLabel">
  <objectName/>
  <x>214</x>
  <y>40</y>
  <width>35</width>
  <height>22</height>
  <uuid>{d48c6f48-1f46-4fb4-901e-ed130ae5b43f}</uuid>
  <visible>true</visible>
  <midichan>0</midichan>
  <midicc>-3</midicc>
  <label>DUR</label>
  <alignment>center</alignment>
  <font>Arial Black</font>
  <fontsize>12</fontsize>
  <precision>3</precision>
  <color>
   <r>204</r>
   <g>204</g>
   <b>204</b>
  </color>
  <bgcolor mode="nobackground">
   <r>255</r>
   <g>255</g>
   <b>255</b>
  </bgcolor>
  <bordermode>noborder</bordermode>
  <borderradius>1</borderradius>
  <borderwidth>1</borderwidth>
 </bsbObject>
 <bsbObject version="2" type="BSBLabel">
  <objectName/>
  <x>251</x>
  <y>40</y>
  <width>36</width>
  <height>22</height>
  <uuid>{c80d960d-7fb0-42ad-9bda-f0986922bafa}</uuid>
  <visible>true</visible>
  <midichan>0</midichan>
  <midicc>-3</midicc>
  <label>CPS
</label>
  <alignment>center</alignment>
  <font>Arial Black</font>
  <fontsize>12</fontsize>
  <precision>3</precision>
  <color>
   <r>204</r>
   <g>204</g>
   <b>204</b>
  </color>
  <bgcolor mode="nobackground">
   <r>255</r>
   <g>255</g>
   <b>255</b>
  </bgcolor>
  <bordermode>noborder</bordermode>
  <borderradius>1</borderradius>
  <borderwidth>1</borderwidth>
 </bsbObject>
 <bsbObject version="2" type="BSBLabel">
  <objectName/>
  <x>289</x>
  <y>40</y>
  <width>38</width>
  <height>22</height>
  <uuid>{c04c64a5-20c5-4c79-af4c-809fe37e2131}</uuid>
  <visible>true</visible>
  <midichan>0</midichan>
  <midicc>-3</midicc>
  <label>VEL
</label>
  <alignment>center</alignment>
  <font>Arial Black</font>
  <fontsize>12</fontsize>
  <precision>3</precision>
  <color>
   <r>204</r>
   <g>204</g>
   <b>204</b>
  </color>
  <bgcolor mode="nobackground">
   <r>255</r>
   <g>255</g>
   <b>255</b>
  </bgcolor>
  <bordermode>noborder</bordermode>
  <borderradius>1</borderradius>
  <borderwidth>1</borderwidth>
 </bsbObject>
 <bsbObject version="2" type="BSBLabel">
  <objectName/>
  <x>329</x>
  <y>40</y>
  <width>36</width>
  <height>22</height>
  <uuid>{1c950a06-4a67-4f55-8fce-66493d4b1b51}</uuid>
  <visible>true</visible>
  <midichan>0</midichan>
  <midicc>-3</midicc>
  <label>FMD</label>
  <alignment>center</alignment>
  <font>Arial Black</font>
  <fontsize>12</fontsize>
  <precision>3</precision>
  <color>
   <r>204</r>
   <g>204</g>
   <b>204</b>
  </color>
  <bgcolor mode="nobackground">
   <r>255</r>
   <g>255</g>
   <b>255</b>
  </bgcolor>
  <bordermode>noborder</bordermode>
  <borderradius>1</borderradius>
  <borderwidth>1</borderwidth>
 </bsbObject>
 <bsbObject version="2" type="BSBLabel">
  <objectName/>
  <x>366</x>
  <y>40</y>
  <width>39</width>
  <height>22</height>
  <uuid>{27ff6f8f-12b8-495f-bee2-45ac3c435af1}</uuid>
  <visible>true</visible>
  <midichan>0</midichan>
  <midicc>-3</midicc>
  <label>PMD
</label>
  <alignment>center</alignment>
  <font>Arial Black</font>
  <fontsize>12</fontsize>
  <precision>3</precision>
  <color>
   <r>204</r>
   <g>204</g>
   <b>204</b>
  </color>
  <bgcolor mode="nobackground">
   <r>255</r>
   <g>255</g>
   <b>255</b>
  </bgcolor>
  <bordermode>noborder</bordermode>
  <borderradius>1</borderradius>
  <borderwidth>1</borderwidth>
 </bsbObject>
 <bsbObject version="2" type="BSBLabel">
  <objectName/>
  <x>401</x>
  <y>40</y>
  <width>46</width>
  <height>22</height>
  <uuid>{9a6ee7f3-f276-4eaa-8353-96610a4b8bbd}</uuid>
  <visible>true</visible>
  <midichan>0</midichan>
  <midicc>-3</midicc>
  <label>FRPOW</label>
  <alignment>center</alignment>
  <font>Arial Black</font>
  <fontsize>12</fontsize>
  <precision>3</precision>
  <color>
   <r>204</r>
   <g>204</g>
   <b>204</b>
  </color>
  <bgcolor mode="nobackground">
   <r>255</r>
   <g>255</g>
   <b>255</b>
  </bgcolor>
  <bordermode>noborder</bordermode>
  <borderradius>1</borderradius>
  <borderwidth>1</borderwidth>
 </bsbObject>
 <bsbObject version="2" type="BSBLabel">
  <objectName/>
  <x>442</x>
  <y>40</y>
  <width>48</width>
  <height>22</height>
  <uuid>{d0827e6c-d0f6-41a5-8f16-00a25984911b}</uuid>
  <visible>true</visible>
  <midichan>0</midichan>
  <midicc>-3</midicc>
  <label>PRPOW</label>
  <alignment>center</alignment>
  <font>Arial Black</font>
  <fontsize>12</fontsize>
  <precision>3</precision>
  <color>
   <r>204</r>
   <g>204</g>
   <b>204</b>
  </color>
  <bgcolor mode="nobackground">
   <r>255</r>
   <g>255</g>
   <b>255</b>
  </bgcolor>
  <bordermode>noborder</bordermode>
  <borderradius>1</borderradius>
  <borderwidth>1</borderwidth>
 </bsbObject>
 <bsbObject version="2" type="BSBVSlider">
  <objectName>dens1</objectName>
  <x>181</x>
  <y>86</y>
  <width>21</width>
  <height>131</height>
  <uuid>{8d2ef7d8-97c1-4dc3-aecf-9e52b78a5c54}</uuid>
  <visible>true</visible>
  <midichan>0</midichan>
  <midicc>0</midicc>
  <minimum>1.00000000</minimum>
  <maximum>200.00000000</maximum>
  <value>1.00000000</value>
  <mode>lin</mode>
  <mouseControl act="jump">continuous</mouseControl>
  <resolution>-1.00000000</resolution>
  <randomizable group="0">false</randomizable>
 </bsbObject>
 <bsbObject version="2" type="BSBVSlider">
  <objectName>prpow1</objectName>
  <x>454</x>
  <y>86</y>
  <width>21</width>
  <height>131</height>
  <uuid>{887ba5e4-8bb3-4db2-a9b7-160feb31aabd}</uuid>
  <visible>true</visible>
  <midichan>0</midichan>
  <midicc>-3</midicc>
  <minimum>0.00000000</minimum>
  <maximum>1.00000000</maximum>
  <value>1.00000000</value>
  <mode>lin</mode>
  <mouseControl act="jump">continuous</mouseControl>
  <resolution>-1.00000000</resolution>
  <randomizable group="0">false</randomizable>
 </bsbObject>
 <bsbObject version="2" type="BSBVSlider">
  <objectName>frpow1</objectName>
  <x>415</x>
  <y>86</y>
  <width>21</width>
  <height>131</height>
  <uuid>{4639dd8d-42b5-4111-baee-2726017bb26d}</uuid>
  <visible>true</visible>
  <midichan>0</midichan>
  <midicc>-3</midicc>
  <minimum>0.00000000</minimum>
  <maximum>1.00000000</maximum>
  <value>1.00000000</value>
  <mode>lin</mode>
  <mouseControl act="jump">continuous</mouseControl>
  <resolution>-1.00000000</resolution>
  <randomizable group="0">false</randomizable>
 </bsbObject>
 <bsbObject version="2" type="BSBVSlider">
  <objectName>pmd1</objectName>
  <x>375</x>
  <y>86</y>
  <width>21</width>
  <height>131</height>
  <uuid>{730150c2-935d-408d-8582-c7728348c645}</uuid>
  <visible>true</visible>
  <midichan>0</midichan>
  <midicc>-3</midicc>
  <minimum>0.00000000</minimum>
  <maximum>1.00000000</maximum>
  <value>1.00000000</value>
  <mode>lin</mode>
  <mouseControl act="jump">continuous</mouseControl>
  <resolution>-1.00000000</resolution>
  <randomizable group="0">false</randomizable>
 </bsbObject>
 <bsbObject version="2" type="BSBVSlider">
  <objectName>fmd1</objectName>
  <x>336</x>
  <y>86</y>
  <width>21</width>
  <height>131</height>
  <uuid>{5ec19e13-bddc-4d86-b69b-e9afe29fd541}</uuid>
  <visible>true</visible>
  <midichan>0</midichan>
  <midicc>-3</midicc>
  <minimum>0.00000000</minimum>
  <maximum>1.00000000</maximum>
  <value>0.16793893</value>
  <mode>lin</mode>
  <mouseControl act="jump">continuous</mouseControl>
  <resolution>-1.00000000</resolution>
  <randomizable group="0">false</randomizable>
 </bsbObject>
 <bsbObject version="2" type="BSBVSlider">
  <objectName>vel1</objectName>
  <x>296</x>
  <y>86</y>
  <width>21</width>
  <height>131</height>
  <uuid>{dbfc25d9-1a75-4511-8995-115cfbe86e72}</uuid>
  <visible>true</visible>
  <midichan>0</midichan>
  <midicc>-3</midicc>
  <minimum>0.00000000</minimum>
  <maximum>1.00000000</maximum>
  <value>1.00000000</value>
  <mode>lin</mode>
  <mouseControl act="jump">continuous</mouseControl>
  <resolution>-1.00000000</resolution>
  <randomizable group="0">false</randomizable>
 </bsbObject>
 <bsbObject version="2" type="BSBVSlider">
  <objectName>cps1</objectName>
  <x>259</x>
  <y>86</y>
  <width>21</width>
  <height>131</height>
  <uuid>{a1113006-70da-4872-8bbe-c17fee4372f8}</uuid>
  <visible>true</visible>
  <midichan>0</midichan>
  <midicc>-3</midicc>
  <minimum>0.03250000</minimum>
  <maximum>6.00000000</maximum>
  <value>0.03250000</value>
  <mode>lin</mode>
  <mouseControl act="jump">continuous</mouseControl>
  <resolution>-1.00000000</resolution>
  <randomizable group="0">false</randomizable>
 </bsbObject>
 <bsbObject version="2" type="BSBVSlider">
  <objectName>dur1</objectName>
  <x>220</x>
  <y>86</y>
  <width>21</width>
  <height>131</height>
  <uuid>{798e938c-2884-47b4-b1b8-7a9a9d0f6d0f}</uuid>
  <visible>true</visible>
  <midichan>0</midichan>
  <midicc>-3</midicc>
  <minimum>0.01000000</minimum>
  <maximum>0.50000000</maximum>
  <value>0.06236641</value>
  <mode>lin</mode>
  <mouseControl act="jump">continuous</mouseControl>
  <resolution>-1.00000000</resolution>
  <randomizable group="0">false</randomizable>
 </bsbObject>
 <bsbObject version="2" type="BSBScrollNumber">
  <objectName>dens1</objectName>
  <x>174</x>
  <y>62</y>
  <width>38</width>
  <height>24</height>
  <uuid>{75679e9b-301a-40ee-bd4e-0b6295e3ab63}</uuid>
  <visible>true</visible>
  <midichan>0</midichan>
  <midicc>0</midicc>
  <alignment>center</alignment>
  <font>Arial</font>
  <fontsize>12</fontsize>
  <color>
   <r>128</r>
   <g>128</g>
   <b>128</b>
  </color>
  <bgcolor mode="background">
   <r>0</r>
   <g>0</g>
   <b>0</b>
  </bgcolor>
  <value>1.00000000</value>
  <resolution>1.00000000</resolution>
  <minimum>-999999999999.00000000</minimum>
  <maximum>999999999999.00000000</maximum>
  <bordermode>noborder</bordermode>
  <borderradius>1</borderradius>
  <borderwidth>1</borderwidth>
  <randomizable group="0">false</randomizable>
  <mouseControl act=""/>
 </bsbObject>
 <bsbObject version="2" type="BSBScrollNumber">
  <objectName>cps1</objectName>
  <x>250</x>
  <y>62</y>
  <width>38</width>
  <height>24</height>
  <uuid>{63bd83e6-db51-4b5a-bb4c-df150be17e55}</uuid>
  <visible>true</visible>
  <midichan>0</midichan>
  <midicc>-3</midicc>
  <alignment>left</alignment>
  <font>Arial</font>
  <fontsize>12</fontsize>
  <color>
   <r>128</r>
   <g>128</g>
   <b>128</b>
  </color>
  <bgcolor mode="background">
   <r>0</r>
   <g>0</g>
   <b>0</b>
  </bgcolor>
  <value>0.03250000</value>
  <resolution>0.00100000</resolution>
  <minimum>-999999999999.00000000</minimum>
  <maximum>99999999999999.00000000</maximum>
  <bordermode>noborder</bordermode>
  <borderradius>1</borderradius>
  <borderwidth>1</borderwidth>
  <randomizable group="0">false</randomizable>
  <mouseControl act=""/>
 </bsbObject>
 <bsbObject version="2" type="BSBScrollNumber">
  <objectName>vel1</objectName>
  <x>289</x>
  <y>62</y>
  <width>38</width>
  <height>24</height>
  <uuid>{a41fc1aa-b3e7-439b-b75e-a3cac9e6ad84}</uuid>
  <visible>true</visible>
  <midichan>0</midichan>
  <midicc>-3</midicc>
  <alignment>left</alignment>
  <font>Arial</font>
  <fontsize>12</fontsize>
  <color>
   <r>128</r>
   <g>128</g>
   <b>128</b>
  </color>
  <bgcolor mode="background">
   <r>0</r>
   <g>0</g>
   <b>0</b>
  </bgcolor>
  <value>1.00000000</value>
  <resolution>0.00100000</resolution>
  <minimum>-999999999999.00000000</minimum>
  <maximum>99999999999999.00000000</maximum>
  <bordermode>noborder</bordermode>
  <borderradius>1</borderradius>
  <borderwidth>1</borderwidth>
  <randomizable group="0">false</randomizable>
  <mouseControl act=""/>
 </bsbObject>
 <bsbObject version="2" type="BSBScrollNumber">
  <objectName>fmd1</objectName>
  <x>328</x>
  <y>62</y>
  <width>38</width>
  <height>24</height>
  <uuid>{66e91a6c-f268-4d9e-bb39-0d6cf942a23c}</uuid>
  <visible>true</visible>
  <midichan>0</midichan>
  <midicc>-3</midicc>
  <alignment>left</alignment>
  <font>Arial</font>
  <fontsize>12</fontsize>
  <color>
   <r>128</r>
   <g>128</g>
   <b>128</b>
  </color>
  <bgcolor mode="background">
   <r>0</r>
   <g>0</g>
   <b>0</b>
  </bgcolor>
  <value>0.16793893</value>
  <resolution>0.00100000</resolution>
  <minimum>-999999999999.00000000</minimum>
  <maximum>99999999999999.00000000</maximum>
  <bordermode>noborder</bordermode>
  <borderradius>1</borderradius>
  <borderwidth>1</borderwidth>
  <randomizable group="0">false</randomizable>
  <mouseControl act=""/>
 </bsbObject>
 <bsbObject version="2" type="BSBScrollNumber">
  <objectName>pmd1</objectName>
  <x>367</x>
  <y>62</y>
  <width>38</width>
  <height>24</height>
  <uuid>{7daec5fa-1cfa-4bad-9895-0aa515b83f95}</uuid>
  <visible>true</visible>
  <midichan>0</midichan>
  <midicc>-3</midicc>
  <alignment>left</alignment>
  <font>Arial</font>
  <fontsize>12</fontsize>
  <color>
   <r>128</r>
   <g>128</g>
   <b>128</b>
  </color>
  <bgcolor mode="background">
   <r>0</r>
   <g>0</g>
   <b>0</b>
  </bgcolor>
  <value>1.00000000</value>
  <resolution>0.00100000</resolution>
  <minimum>-999999999999.00000000</minimum>
  <maximum>99999999999999.00000000</maximum>
  <bordermode>noborder</bordermode>
  <borderradius>1</borderradius>
  <borderwidth>1</borderwidth>
  <randomizable group="0">false</randomizable>
  <mouseControl act=""/>
 </bsbObject>
 <bsbObject version="2" type="BSBScrollNumber">
  <objectName>frpow1</objectName>
  <x>406</x>
  <y>62</y>
  <width>38</width>
  <height>24</height>
  <uuid>{4252898e-85a2-4c82-8f29-5b272c37e799}</uuid>
  <visible>true</visible>
  <midichan>0</midichan>
  <midicc>-3</midicc>
  <alignment>left</alignment>
  <font>Arial</font>
  <fontsize>12</fontsize>
  <color>
   <r>128</r>
   <g>128</g>
   <b>128</b>
  </color>
  <bgcolor mode="background">
   <r>0</r>
   <g>0</g>
   <b>0</b>
  </bgcolor>
  <value>1.00000000</value>
  <resolution>0.00100000</resolution>
  <minimum>-999999999999.00000000</minimum>
  <maximum>99999999999999.00000000</maximum>
  <bordermode>noborder</bordermode>
  <borderradius>1</borderradius>
  <borderwidth>1</borderwidth>
  <randomizable group="0">false</randomizable>
  <mouseControl act=""/>
 </bsbObject>
 <bsbObject version="2" type="BSBScrollNumber">
  <objectName>prpow1</objectName>
  <x>446</x>
  <y>62</y>
  <width>38</width>
  <height>24</height>
  <uuid>{a1c69a27-fe41-4977-a46e-cecb68fc4a08}</uuid>
  <visible>true</visible>
  <midichan>0</midichan>
  <midicc>-3</midicc>
  <alignment>left</alignment>
  <font>Arial</font>
  <fontsize>12</fontsize>
  <color>
   <r>128</r>
   <g>128</g>
   <b>128</b>
  </color>
  <bgcolor mode="background">
   <r>0</r>
   <g>0</g>
   <b>0</b>
  </bgcolor>
  <value>1.00000000</value>
  <resolution>0.00100000</resolution>
  <minimum>-999999999999.00000000</minimum>
  <maximum>99999999999999.00000000</maximum>
  <bordermode>noborder</bordermode>
  <borderradius>1</borderradius>
  <borderwidth>1</borderwidth>
  <randomizable group="0">false</randomizable>
  <mouseControl act=""/>
 </bsbObject>
 <bsbObject version="2" type="BSBScrollNumber">
  <objectName>dur1</objectName>
  <x>211</x>
  <y>62</y>
  <width>38</width>
  <height>24</height>
  <uuid>{14493137-e53a-4abc-8be2-df3c934fef1b}</uuid>
  <visible>true</visible>
  <midichan>0</midichan>
  <midicc>-3</midicc>
  <alignment>left</alignment>
  <font>Arial</font>
  <fontsize>12</fontsize>
  <color>
   <r>128</r>
   <g>128</g>
   <b>128</b>
  </color>
  <bgcolor mode="background">
   <r>0</r>
   <g>0</g>
   <b>0</b>
  </bgcolor>
  <value>0.06236641</value>
  <resolution>0.00100000</resolution>
  <minimum>-999999999999.00000000</minimum>
  <maximum>99999999999999.00000000</maximum>
  <bordermode>noborder</bordermode>
  <borderradius>1</borderradius>
  <borderwidth>1</borderwidth>
  <randomizable group="0">false</randomizable>
  <mouseControl act=""/>
 </bsbObject>
 <bsbObject version="2" type="BSBLabel">
  <objectName/>
  <x>538</x>
  <y>36</y>
  <width>39</width>
  <height>24</height>
  <uuid>{41523d2a-61d6-4cd2-8082-818da3e4b2c4}</uuid>
  <visible>true</visible>
  <midichan>0</midichan>
  <midicc>-3</midicc>
  <label>DENS</label>
  <alignment>left</alignment>
  <font>Arial Black</font>
  <fontsize>12</fontsize>
  <precision>3</precision>
  <color>
   <r>204</r>
   <g>204</g>
   <b>204</b>
  </color>
  <bgcolor mode="nobackground">
   <r>255</r>
   <g>255</g>
   <b>255</b>
  </bgcolor>
  <bordermode>noborder</bordermode>
  <borderradius>1</borderradius>
  <borderwidth>1</borderwidth>
 </bsbObject>
 <bsbObject version="2" type="BSBLabel">
  <objectName/>
  <x>578</x>
  <y>36</y>
  <width>36</width>
  <height>24</height>
  <uuid>{b2a94cff-09b2-459f-9aa9-03f3c1b3fa69}</uuid>
  <visible>true</visible>
  <midichan>0</midichan>
  <midicc>-3</midicc>
  <label>DUR</label>
  <alignment>center</alignment>
  <font>Arial Black</font>
  <fontsize>12</fontsize>
  <precision>3</precision>
  <color>
   <r>204</r>
   <g>204</g>
   <b>204</b>
  </color>
  <bgcolor mode="nobackground">
   <r>255</r>
   <g>255</g>
   <b>255</b>
  </bgcolor>
  <bordermode>noborder</bordermode>
  <borderradius>1</borderradius>
  <borderwidth>1</borderwidth>
 </bsbObject>
 <bsbObject version="2" type="BSBLabel">
  <objectName/>
  <x>615</x>
  <y>36</y>
  <width>37</width>
  <height>24</height>
  <uuid>{e42ec295-0c96-4ce7-b1f4-e90c0f36a760}</uuid>
  <visible>true</visible>
  <midichan>0</midichan>
  <midicc>-3</midicc>
  <label>CPS
</label>
  <alignment>center</alignment>
  <font>Arial Black</font>
  <fontsize>12</fontsize>
  <precision>3</precision>
  <color>
   <r>204</r>
   <g>204</g>
   <b>204</b>
  </color>
  <bgcolor mode="nobackground">
   <r>255</r>
   <g>255</g>
   <b>255</b>
  </bgcolor>
  <bordermode>noborder</bordermode>
  <borderradius>1</borderradius>
  <borderwidth>1</borderwidth>
 </bsbObject>
 <bsbObject version="2" type="BSBLabel">
  <objectName/>
  <x>653</x>
  <y>36</y>
  <width>39</width>
  <height>24</height>
  <uuid>{6c87fb0c-7e66-47a8-82cc-1624e674d66d}</uuid>
  <visible>true</visible>
  <midichan>0</midichan>
  <midicc>-3</midicc>
  <label>VEL
</label>
  <alignment>center</alignment>
  <font>Arial Black</font>
  <fontsize>12</fontsize>
  <precision>3</precision>
  <color>
   <r>204</r>
   <g>204</g>
   <b>204</b>
  </color>
  <bgcolor mode="nobackground">
   <r>255</r>
   <g>255</g>
   <b>255</b>
  </bgcolor>
  <bordermode>noborder</bordermode>
  <borderradius>1</borderradius>
  <borderwidth>1</borderwidth>
 </bsbObject>
 <bsbObject version="2" type="BSBLabel">
  <objectName/>
  <x>693</x>
  <y>36</y>
  <width>37</width>
  <height>24</height>
  <uuid>{da281254-e82b-400f-93e0-5697ffb232a5}</uuid>
  <visible>true</visible>
  <midichan>0</midichan>
  <midicc>-3</midicc>
  <label>FMD</label>
  <alignment>center</alignment>
  <font>Arial Black</font>
  <fontsize>12</fontsize>
  <precision>3</precision>
  <color>
   <r>204</r>
   <g>204</g>
   <b>204</b>
  </color>
  <bgcolor mode="nobackground">
   <r>255</r>
   <g>255</g>
   <b>255</b>
  </bgcolor>
  <bordermode>noborder</bordermode>
  <borderradius>1</borderradius>
  <borderwidth>1</borderwidth>
 </bsbObject>
 <bsbObject version="2" type="BSBLabel">
  <objectName/>
  <x>730</x>
  <y>36</y>
  <width>40</width>
  <height>24</height>
  <uuid>{b6bf6263-719c-409f-919f-2c52e12ff411}</uuid>
  <visible>true</visible>
  <midichan>0</midichan>
  <midicc>-3</midicc>
  <label>PMD
</label>
  <alignment>center</alignment>
  <font>Arial Black</font>
  <fontsize>12</fontsize>
  <precision>3</precision>
  <color>
   <r>204</r>
   <g>204</g>
   <b>204</b>
  </color>
  <bgcolor mode="nobackground">
   <r>255</r>
   <g>255</g>
   <b>255</b>
  </bgcolor>
  <bordermode>noborder</bordermode>
  <borderradius>1</borderradius>
  <borderwidth>1</borderwidth>
 </bsbObject>
 <bsbObject version="2" type="BSBLabel">
  <objectName/>
  <x>765</x>
  <y>36</y>
  <width>47</width>
  <height>24</height>
  <uuid>{f612a600-f769-4d9b-a8f0-1d8bad9a6b5c}</uuid>
  <visible>true</visible>
  <midichan>0</midichan>
  <midicc>-3</midicc>
  <label>FRPOW</label>
  <alignment>center</alignment>
  <font>Arial Black</font>
  <fontsize>12</fontsize>
  <precision>3</precision>
  <color>
   <r>204</r>
   <g>204</g>
   <b>204</b>
  </color>
  <bgcolor mode="nobackground">
   <r>255</r>
   <g>255</g>
   <b>255</b>
  </bgcolor>
  <bordermode>noborder</bordermode>
  <borderradius>1</borderradius>
  <borderwidth>1</borderwidth>
 </bsbObject>
 <bsbObject version="2" type="BSBLabel">
  <objectName/>
  <x>806</x>
  <y>36</y>
  <width>49</width>
  <height>24</height>
  <uuid>{1c7e821d-1700-4dc8-8fb0-fe524c38b2b4}</uuid>
  <visible>true</visible>
  <midichan>0</midichan>
  <midicc>-3</midicc>
  <label>PRPOW</label>
  <alignment>center</alignment>
  <font>Arial Black</font>
  <fontsize>12</fontsize>
  <precision>3</precision>
  <color>
   <r>204</r>
   <g>204</g>
   <b>204</b>
  </color>
  <bgcolor mode="nobackground">
   <r>255</r>
   <g>255</g>
   <b>255</b>
  </bgcolor>
  <bordermode>noborder</bordermode>
  <borderradius>1</borderradius>
  <borderwidth>1</borderwidth>
 </bsbObject>
 <bsbObject version="2" type="BSBVSlider">
  <objectName>dens2</objectName>
  <x>544</x>
  <y>81</y>
  <width>22</width>
  <height>133</height>
  <uuid>{ed6119bf-8bc0-4c8d-9055-6db831909094}</uuid>
  <visible>true</visible>
  <midichan>0</midichan>
  <midicc>-3</midicc>
  <minimum>0.01000000</minimum>
  <maximum>200.00000000</maximum>
  <value>105.26789474</value>
  <mode>lin</mode>
  <mouseControl act="jump">continuous</mouseControl>
  <resolution>-1.00000000</resolution>
  <randomizable group="0">false</randomizable>
 </bsbObject>
 <bsbObject version="2" type="BSBVSlider">
  <objectName>prpow2</objectName>
  <x>817</x>
  <y>81</y>
  <width>22</width>
  <height>133</height>
  <uuid>{ec5b3810-dc5b-4a35-b4e2-58abbbb64bfc}</uuid>
  <visible>true</visible>
  <midichan>0</midichan>
  <midicc>-3</midicc>
  <minimum>0.00000000</minimum>
  <maximum>1.00000000</maximum>
  <value>0.97744361</value>
  <mode>lin</mode>
  <mouseControl act="jump">continuous</mouseControl>
  <resolution>-1.00000000</resolution>
  <randomizable group="0">false</randomizable>
 </bsbObject>
 <bsbObject version="2" type="BSBVSlider">
  <objectName>frpow2</objectName>
  <x>778</x>
  <y>81</y>
  <width>22</width>
  <height>133</height>
  <uuid>{87595d33-2e3e-4d1b-918b-9a0279283498}</uuid>
  <visible>true</visible>
  <midichan>0</midichan>
  <midicc>-3</midicc>
  <minimum>0.00000000</minimum>
  <maximum>1.00000000</maximum>
  <value>1.00000000</value>
  <mode>lin</mode>
  <mouseControl act="jump">continuous</mouseControl>
  <resolution>-1.00000000</resolution>
  <randomizable group="0">false</randomizable>
 </bsbObject>
 <bsbObject version="2" type="BSBVSlider">
  <objectName>pmd2</objectName>
  <x>738</x>
  <y>81</y>
  <width>22</width>
  <height>133</height>
  <uuid>{748d74ab-2efb-4bfc-a991-755b73c3d7c3}</uuid>
  <visible>true</visible>
  <midichan>0</midichan>
  <midicc>-3</midicc>
  <minimum>0.00000000</minimum>
  <maximum>1.00000000</maximum>
  <value>1.00000000</value>
  <mode>lin</mode>
  <mouseControl act="jump">continuous</mouseControl>
  <resolution>-1.00000000</resolution>
  <randomizable group="0">false</randomizable>
 </bsbObject>
 <bsbObject version="2" type="BSBVSlider">
  <objectName>fmd2</objectName>
  <x>699</x>
  <y>81</y>
  <width>22</width>
  <height>133</height>
  <uuid>{3b8a5d39-f2f6-4d63-861c-0931bb53e00b}</uuid>
  <visible>true</visible>
  <midichan>0</midichan>
  <midicc>-3</midicc>
  <minimum>0.00000000</minimum>
  <maximum>1.00000000</maximum>
  <value>0.12030075</value>
  <mode>lin</mode>
  <mouseControl act="jump">continuous</mouseControl>
  <resolution>-1.00000000</resolution>
  <randomizable group="0">false</randomizable>
 </bsbObject>
 <bsbObject version="2" type="BSBVSlider">
  <objectName>vel2</objectName>
  <x>659</x>
  <y>81</y>
  <width>22</width>
  <height>133</height>
  <uuid>{22f80061-d32d-4fda-a8e0-ad2f0e9fdb6b}</uuid>
  <visible>true</visible>
  <midichan>0</midichan>
  <midicc>-3</midicc>
  <minimum>0.00000000</minimum>
  <maximum>1.00000000</maximum>
  <value>1.00000000</value>
  <mode>lin</mode>
  <mouseControl act="jump">continuous</mouseControl>
  <resolution>-1.00000000</resolution>
  <randomizable group="0">false</randomizable>
 </bsbObject>
 <bsbObject version="2" type="BSBVSlider">
  <objectName>cps2</objectName>
  <x>622</x>
  <y>81</y>
  <width>22</width>
  <height>133</height>
  <uuid>{6e95fbea-0542-4801-9972-53ab18dc3d81}</uuid>
  <visible>true</visible>
  <midichan>0</midichan>
  <midicc>-3</midicc>
  <minimum>0.03250000</minimum>
  <maximum>6.00000000</maximum>
  <value>6.00000000</value>
  <mode>lin</mode>
  <mouseControl act="jump">continuous</mouseControl>
  <resolution>-1.00000000</resolution>
  <randomizable group="0">false</randomizable>
 </bsbObject>
 <bsbObject version="2" type="BSBVSlider">
  <objectName>dur2</objectName>
  <x>583</x>
  <y>81</y>
  <width>22</width>
  <height>133</height>
  <uuid>{be0fd264-cdf7-4564-a2ad-94ff74006dda}</uuid>
  <visible>true</visible>
  <midichan>0</midichan>
  <midicc>-3</midicc>
  <minimum>0.01000000</minimum>
  <maximum>0.50000000</maximum>
  <value>0.01000000</value>
  <mode>lin</mode>
  <mouseControl act="jump">continuous</mouseControl>
  <resolution>-1.00000000</resolution>
  <randomizable group="0">false</randomizable>
 </bsbObject>
 <bsbObject version="2" type="BSBScrollNumber">
  <objectName>dens2</objectName>
  <x>538</x>
  <y>58</y>
  <width>39</width>
  <height>26</height>
  <uuid>{97adf1ba-4ef7-4174-8228-a0f17891512a}</uuid>
  <visible>true</visible>
  <midichan>0</midichan>
  <midicc>-3</midicc>
  <alignment>left</alignment>
  <font>Arial</font>
  <fontsize>12</fontsize>
  <color>
   <r>128</r>
   <g>128</g>
   <b>128</b>
  </color>
  <bgcolor mode="background">
   <r>0</r>
   <g>0</g>
   <b>0</b>
  </bgcolor>
  <value>105.26789474</value>
  <resolution>0.00100000</resolution>
  <minimum>-999999999999.00000000</minimum>
  <maximum>99999999999999.00000000</maximum>
  <bordermode>noborder</bordermode>
  <borderradius>1</borderradius>
  <borderwidth>1</borderwidth>
  <randomizable group="0">false</randomizable>
  <mouseControl act=""/>
 </bsbObject>
 <bsbObject version="2" type="BSBScrollNumber">
  <objectName>cps2</objectName>
  <x>614</x>
  <y>58</y>
  <width>39</width>
  <height>26</height>
  <uuid>{638a9e00-8ed1-4748-84b0-3c2108141ee8}</uuid>
  <visible>true</visible>
  <midichan>0</midichan>
  <midicc>-3</midicc>
  <alignment>left</alignment>
  <font>Arial</font>
  <fontsize>12</fontsize>
  <color>
   <r>128</r>
   <g>128</g>
   <b>128</b>
  </color>
  <bgcolor mode="background">
   <r>0</r>
   <g>0</g>
   <b>0</b>
  </bgcolor>
  <value>6.00000000</value>
  <resolution>0.00100000</resolution>
  <minimum>-999999999999.00000000</minimum>
  <maximum>99999999999999.00000000</maximum>
  <bordermode>noborder</bordermode>
  <borderradius>1</borderradius>
  <borderwidth>1</borderwidth>
  <randomizable group="0">false</randomizable>
  <mouseControl act=""/>
 </bsbObject>
 <bsbObject version="2" type="BSBScrollNumber">
  <objectName>vel2</objectName>
  <x>653</x>
  <y>58</y>
  <width>39</width>
  <height>26</height>
  <uuid>{bcaa03ec-1733-48a3-9187-cd3756ab62be}</uuid>
  <visible>true</visible>
  <midichan>0</midichan>
  <midicc>-3</midicc>
  <alignment>left</alignment>
  <font>Arial</font>
  <fontsize>12</fontsize>
  <color>
   <r>128</r>
   <g>128</g>
   <b>128</b>
  </color>
  <bgcolor mode="background">
   <r>0</r>
   <g>0</g>
   <b>0</b>
  </bgcolor>
  <value>1.00000000</value>
  <resolution>0.00100000</resolution>
  <minimum>-999999999999.00000000</minimum>
  <maximum>99999999999999.00000000</maximum>
  <bordermode>noborder</bordermode>
  <borderradius>1</borderradius>
  <borderwidth>1</borderwidth>
  <randomizable group="0">false</randomizable>
  <mouseControl act=""/>
 </bsbObject>
 <bsbObject version="2" type="BSBScrollNumber">
  <objectName>fmd2</objectName>
  <x>692</x>
  <y>58</y>
  <width>39</width>
  <height>26</height>
  <uuid>{1e1df7b2-78e4-40fa-ac8c-c2c5d7f13330}</uuid>
  <visible>true</visible>
  <midichan>0</midichan>
  <midicc>-3</midicc>
  <alignment>left</alignment>
  <font>Arial</font>
  <fontsize>12</fontsize>
  <color>
   <r>128</r>
   <g>128</g>
   <b>128</b>
  </color>
  <bgcolor mode="background">
   <r>0</r>
   <g>0</g>
   <b>0</b>
  </bgcolor>
  <value>0.12030075</value>
  <resolution>0.00100000</resolution>
  <minimum>-999999999999.00000000</minimum>
  <maximum>99999999999999.00000000</maximum>
  <bordermode>noborder</bordermode>
  <borderradius>1</borderradius>
  <borderwidth>1</borderwidth>
  <randomizable group="0">false</randomizable>
  <mouseControl act=""/>
 </bsbObject>
 <bsbObject version="2" type="BSBScrollNumber">
  <objectName>pmd2</objectName>
  <x>731</x>
  <y>58</y>
  <width>39</width>
  <height>26</height>
  <uuid>{621d0afe-4220-491b-ad28-315b80a916d7}</uuid>
  <visible>true</visible>
  <midichan>0</midichan>
  <midicc>-3</midicc>
  <alignment>left</alignment>
  <font>Arial</font>
  <fontsize>12</fontsize>
  <color>
   <r>128</r>
   <g>128</g>
   <b>128</b>
  </color>
  <bgcolor mode="background">
   <r>0</r>
   <g>0</g>
   <b>0</b>
  </bgcolor>
  <value>1.00000000</value>
  <resolution>0.00100000</resolution>
  <minimum>-999999999999.00000000</minimum>
  <maximum>99999999999999.00000000</maximum>
  <bordermode>noborder</bordermode>
  <borderradius>1</borderradius>
  <borderwidth>1</borderwidth>
  <randomizable group="0">false</randomizable>
  <mouseControl act=""/>
 </bsbObject>
 <bsbObject version="2" type="BSBScrollNumber">
  <objectName/>
  <x>770</x>
  <y>58</y>
  <width>39</width>
  <height>26</height>
  <uuid>{4a04e905-7de7-417c-8ba5-dd733abc100f}</uuid>
  <visible>true</visible>
  <midichan>0</midichan>
  <midicc>-3</midicc>
  <alignment>left</alignment>
  <font>Arial</font>
  <fontsize>12</fontsize>
  <color>
   <r>128</r>
   <g>128</g>
   <b>128</b>
  </color>
  <bgcolor mode="background">
   <r>0</r>
   <g>0</g>
   <b>0</b>
  </bgcolor>
  <value>0.00000000</value>
  <resolution>0.00100000</resolution>
  <minimum>-999999999999.00000000</minimum>
  <maximum>99999999999999.00000000</maximum>
  <bordermode>noborder</bordermode>
  <borderradius>1</borderradius>
  <borderwidth>1</borderwidth>
  <randomizable group="0">false</randomizable>
  <mouseControl act=""/>
 </bsbObject>
 <bsbObject version="2" type="BSBScrollNumber">
  <objectName/>
  <x>810</x>
  <y>58</y>
  <width>39</width>
  <height>26</height>
  <uuid>{eec91f15-57a0-4497-a262-366420ff546f}</uuid>
  <visible>true</visible>
  <midichan>0</midichan>
  <midicc>-3</midicc>
  <alignment>left</alignment>
  <font>Arial</font>
  <fontsize>12</fontsize>
  <color>
   <r>128</r>
   <g>128</g>
   <b>128</b>
  </color>
  <bgcolor mode="background">
   <r>0</r>
   <g>0</g>
   <b>0</b>
  </bgcolor>
  <value>0.00000000</value>
  <resolution>0.00100000</resolution>
  <minimum>-999999999999.00000000</minimum>
  <maximum>99999999999999.00000000</maximum>
  <bordermode>noborder</bordermode>
  <borderradius>1</borderradius>
  <borderwidth>1</borderwidth>
  <randomizable group="0">false</randomizable>
  <mouseControl act=""/>
 </bsbObject>
 <bsbObject version="2" type="BSBScrollNumber">
  <objectName>dur2</objectName>
  <x>575</x>
  <y>58</y>
  <width>39</width>
  <height>26</height>
  <uuid>{24a2f130-17fd-4058-b5ea-1c89eb9381b6}</uuid>
  <visible>true</visible>
  <midichan>0</midichan>
  <midicc>-3</midicc>
  <alignment>left</alignment>
  <font>Arial</font>
  <fontsize>12</fontsize>
  <color>
   <r>128</r>
   <g>128</g>
   <b>128</b>
  </color>
  <bgcolor mode="background">
   <r>0</r>
   <g>0</g>
   <b>0</b>
  </bgcolor>
  <value>0.01000000</value>
  <resolution>0.00100000</resolution>
  <minimum>-999999999999.00000000</minimum>
  <maximum>99999999999999.00000000</maximum>
  <bordermode>noborder</bordermode>
  <borderradius>1</borderradius>
  <borderwidth>1</borderwidth>
  <randomizable group="0">false</randomizable>
  <mouseControl act=""/>
 </bsbObject>
 <bsbObject version="2" type="BSBLabel">
  <objectName/>
  <x>25</x>
  <y>308</y>
  <width>38</width>
  <height>22</height>
  <uuid>{8cf5060c-e884-4fd4-8e79-c71da09dfb0a}</uuid>
  <visible>true</visible>
  <midichan>0</midichan>
  <midicc>-3</midicc>
  <label>DENS</label>
  <alignment>left</alignment>
  <font>Arial Black</font>
  <fontsize>12</fontsize>
  <precision>3</precision>
  <color>
   <r>204</r>
   <g>204</g>
   <b>204</b>
  </color>
  <bgcolor mode="nobackground">
   <r>255</r>
   <g>255</g>
   <b>255</b>
  </bgcolor>
  <bordermode>noborder</bordermode>
  <borderradius>1</borderradius>
  <borderwidth>1</borderwidth>
 </bsbObject>
 <bsbObject version="2" type="BSBLabel">
  <objectName/>
  <x>64</x>
  <y>308</y>
  <width>35</width>
  <height>22</height>
  <uuid>{71da3b57-3c54-4db1-abe8-e820abb9c505}</uuid>
  <visible>true</visible>
  <midichan>0</midichan>
  <midicc>-3</midicc>
  <label>DUR</label>
  <alignment>center</alignment>
  <font>Arial Black</font>
  <fontsize>12</fontsize>
  <precision>3</precision>
  <color>
   <r>204</r>
   <g>204</g>
   <b>204</b>
  </color>
  <bgcolor mode="nobackground">
   <r>255</r>
   <g>255</g>
   <b>255</b>
  </bgcolor>
  <bordermode>noborder</bordermode>
  <borderradius>1</borderradius>
  <borderwidth>1</borderwidth>
 </bsbObject>
 <bsbObject version="2" type="BSBLabel">
  <objectName/>
  <x>101</x>
  <y>308</y>
  <width>36</width>
  <height>22</height>
  <uuid>{3eba4882-47c7-49fe-93bc-50c79a679739}</uuid>
  <visible>true</visible>
  <midichan>0</midichan>
  <midicc>-3</midicc>
  <label>CPS
</label>
  <alignment>center</alignment>
  <font>Arial Black</font>
  <fontsize>12</fontsize>
  <precision>3</precision>
  <color>
   <r>204</r>
   <g>204</g>
   <b>204</b>
  </color>
  <bgcolor mode="nobackground">
   <r>255</r>
   <g>255</g>
   <b>255</b>
  </bgcolor>
  <bordermode>noborder</bordermode>
  <borderradius>1</borderradius>
  <borderwidth>1</borderwidth>
 </bsbObject>
 <bsbObject version="2" type="BSBLabel">
  <objectName/>
  <x>139</x>
  <y>308</y>
  <width>38</width>
  <height>22</height>
  <uuid>{7c3fe9de-755f-4c0f-8f1f-a5dac6fbb706}</uuid>
  <visible>true</visible>
  <midichan>0</midichan>
  <midicc>-3</midicc>
  <label>VEL
</label>
  <alignment>center</alignment>
  <font>Arial Black</font>
  <fontsize>12</fontsize>
  <precision>3</precision>
  <color>
   <r>204</r>
   <g>204</g>
   <b>204</b>
  </color>
  <bgcolor mode="nobackground">
   <r>255</r>
   <g>255</g>
   <b>255</b>
  </bgcolor>
  <bordermode>noborder</bordermode>
  <borderradius>1</borderradius>
  <borderwidth>1</borderwidth>
 </bsbObject>
 <bsbObject version="2" type="BSBLabel">
  <objectName/>
  <x>179</x>
  <y>308</y>
  <width>36</width>
  <height>22</height>
  <uuid>{22921d71-63f6-4d7e-947a-65c04b671363}</uuid>
  <visible>true</visible>
  <midichan>0</midichan>
  <midicc>-3</midicc>
  <label>FMD</label>
  <alignment>center</alignment>
  <font>Arial Black</font>
  <fontsize>12</fontsize>
  <precision>3</precision>
  <color>
   <r>204</r>
   <g>204</g>
   <b>204</b>
  </color>
  <bgcolor mode="nobackground">
   <r>255</r>
   <g>255</g>
   <b>255</b>
  </bgcolor>
  <bordermode>noborder</bordermode>
  <borderradius>1</borderradius>
  <borderwidth>1</borderwidth>
 </bsbObject>
 <bsbObject version="2" type="BSBLabel">
  <objectName/>
  <x>216</x>
  <y>308</y>
  <width>39</width>
  <height>22</height>
  <uuid>{538f3701-0aa9-4371-883b-1a8043713b57}</uuid>
  <visible>true</visible>
  <midichan>0</midichan>
  <midicc>-3</midicc>
  <label>PMD
</label>
  <alignment>center</alignment>
  <font>Arial Black</font>
  <fontsize>12</fontsize>
  <precision>3</precision>
  <color>
   <r>204</r>
   <g>204</g>
   <b>204</b>
  </color>
  <bgcolor mode="nobackground">
   <r>255</r>
   <g>255</g>
   <b>255</b>
  </bgcolor>
  <bordermode>noborder</bordermode>
  <borderradius>1</borderradius>
  <borderwidth>1</borderwidth>
 </bsbObject>
 <bsbObject version="2" type="BSBLabel">
  <objectName/>
  <x>251</x>
  <y>308</y>
  <width>46</width>
  <height>22</height>
  <uuid>{33a659b1-a059-450b-8b27-1bb0bbc21c32}</uuid>
  <visible>true</visible>
  <midichan>0</midichan>
  <midicc>-3</midicc>
  <label>FRPOW</label>
  <alignment>center</alignment>
  <font>Arial Black</font>
  <fontsize>12</fontsize>
  <precision>3</precision>
  <color>
   <r>204</r>
   <g>204</g>
   <b>204</b>
  </color>
  <bgcolor mode="nobackground">
   <r>255</r>
   <g>255</g>
   <b>255</b>
  </bgcolor>
  <bordermode>noborder</bordermode>
  <borderradius>1</borderradius>
  <borderwidth>1</borderwidth>
 </bsbObject>
 <bsbObject version="2" type="BSBLabel">
  <objectName/>
  <x>292</x>
  <y>308</y>
  <width>48</width>
  <height>22</height>
  <uuid>{6c996685-f3fb-43cb-9171-5461bdc6ade0}</uuid>
  <visible>true</visible>
  <midichan>0</midichan>
  <midicc>-3</midicc>
  <label>PRPOW</label>
  <alignment>center</alignment>
  <font>Arial Black</font>
  <fontsize>12</fontsize>
  <precision>3</precision>
  <color>
   <r>204</r>
   <g>204</g>
   <b>204</b>
  </color>
  <bgcolor mode="nobackground">
   <r>255</r>
   <g>255</g>
   <b>255</b>
  </bgcolor>
  <bordermode>noborder</bordermode>
  <borderradius>1</borderradius>
  <borderwidth>1</borderwidth>
 </bsbObject>
 <bsbObject version="2" type="BSBVSlider">
  <objectName>dens3</objectName>
  <x>31</x>
  <y>352</y>
  <width>21</width>
  <height>131</height>
  <uuid>{5180c8c3-b925-440a-832b-d1d17f941a0f}</uuid>
  <visible>true</visible>
  <midichan>0</midichan>
  <midicc>-3</midicc>
  <minimum>0.01000000</minimum>
  <maximum>200.00000000</maximum>
  <value>80.92198473</value>
  <mode>lin</mode>
  <mouseControl act="jump">continuous</mouseControl>
  <resolution>-1.00000000</resolution>
  <randomizable group="0">false</randomizable>
 </bsbObject>
 <bsbObject version="2" type="BSBVSlider">
  <objectName>prpow3</objectName>
  <x>304</x>
  <y>352</y>
  <width>21</width>
  <height>131</height>
  <uuid>{b4996a21-6f9a-41fd-91b6-caad82af73f5}</uuid>
  <visible>true</visible>
  <midichan>0</midichan>
  <midicc>-3</midicc>
  <minimum>0.00000000</minimum>
  <maximum>1.00000000</maximum>
  <value>1.00000000</value>
  <mode>lin</mode>
  <mouseControl act="jump">continuous</mouseControl>
  <resolution>-1.00000000</resolution>
  <randomizable group="0">false</randomizable>
 </bsbObject>
 <bsbObject version="2" type="BSBVSlider">
  <objectName>frpow3</objectName>
  <x>265</x>
  <y>352</y>
  <width>21</width>
  <height>131</height>
  <uuid>{3e2ca79f-6417-4085-b0df-99ed0b99f994}</uuid>
  <visible>true</visible>
  <midichan>0</midichan>
  <midicc>-3</midicc>
  <minimum>0.00000000</minimum>
  <maximum>1.00000000</maximum>
  <value>0.99236641</value>
  <mode>lin</mode>
  <mouseControl act="jump">continuous</mouseControl>
  <resolution>-1.00000000</resolution>
  <randomizable group="0">false</randomizable>
 </bsbObject>
 <bsbObject version="2" type="BSBVSlider">
  <objectName>pmd3</objectName>
  <x>225</x>
  <y>352</y>
  <width>21</width>
  <height>131</height>
  <uuid>{c7a3281f-733f-48f7-9721-1ef1ec761bbd}</uuid>
  <visible>true</visible>
  <midichan>0</midichan>
  <midicc>-3</midicc>
  <minimum>0.00000000</minimum>
  <maximum>1.00000000</maximum>
  <value>1.00000000</value>
  <mode>lin</mode>
  <mouseControl act="jump">continuous</mouseControl>
  <resolution>-1.00000000</resolution>
  <randomizable group="0">false</randomizable>
 </bsbObject>
 <bsbObject version="2" type="BSBVSlider">
  <objectName>fmd3</objectName>
  <x>186</x>
  <y>352</y>
  <width>21</width>
  <height>131</height>
  <uuid>{cea7b434-4516-4c0b-894a-6c6bf615066b}</uuid>
  <visible>true</visible>
  <midichan>0</midichan>
  <midicc>-3</midicc>
  <minimum>0.00000000</minimum>
  <maximum>1.00000000</maximum>
  <value>0.12213740</value>
  <mode>lin</mode>
  <mouseControl act="jump">continuous</mouseControl>
  <resolution>-1.00000000</resolution>
  <randomizable group="0">false</randomizable>
 </bsbObject>
 <bsbObject version="2" type="BSBVSlider">
  <objectName>vel3</objectName>
  <x>146</x>
  <y>352</y>
  <width>21</width>
  <height>131</height>
  <uuid>{4fa40ad7-f658-4ce9-8e3e-cd18130885f8}</uuid>
  <visible>true</visible>
  <midichan>0</midichan>
  <midicc>-3</midicc>
  <minimum>0.00000000</minimum>
  <maximum>1.00000000</maximum>
  <value>1.00000000</value>
  <mode>lin</mode>
  <mouseControl act="jump">continuous</mouseControl>
  <resolution>-1.00000000</resolution>
  <randomizable group="0">false</randomizable>
 </bsbObject>
 <bsbObject version="2" type="BSBVSlider">
  <objectName>cps3</objectName>
  <x>109</x>
  <y>352</y>
  <width>21</width>
  <height>131</height>
  <uuid>{6b88094d-b427-4fdd-b8d5-f545b0b616ae}</uuid>
  <visible>true</visible>
  <midichan>0</midichan>
  <midicc>-3</midicc>
  <minimum>0.03250000</minimum>
  <maximum>6.00000000</maximum>
  <value>0.03250000</value>
  <mode>lin</mode>
  <mouseControl act="jump">continuous</mouseControl>
  <resolution>-1.00000000</resolution>
  <randomizable group="0">false</randomizable>
 </bsbObject>
 <bsbObject version="2" type="BSBVSlider">
  <objectName>dur3</objectName>
  <x>70</x>
  <y>352</y>
  <width>21</width>
  <height>131</height>
  <uuid>{f22dbf6b-5a5d-458c-aedf-833b9a2dafb1}</uuid>
  <visible>true</visible>
  <midichan>0</midichan>
  <midicc>-3</midicc>
  <minimum>0.01000000</minimum>
  <maximum>0.50000000</maximum>
  <value>0.50000000</value>
  <mode>lin</mode>
  <mouseControl act="jump">continuous</mouseControl>
  <resolution>-1.00000000</resolution>
  <randomizable group="0">false</randomizable>
 </bsbObject>
 <bsbObject version="2" type="BSBScrollNumber">
  <objectName>dens3</objectName>
  <x>24</x>
  <y>330</y>
  <width>38</width>
  <height>24</height>
  <uuid>{df58ba88-c8c4-4240-95a5-7fa9b842e61c}</uuid>
  <visible>true</visible>
  <midichan>0</midichan>
  <midicc>-3</midicc>
  <alignment>left</alignment>
  <font>Arial</font>
  <fontsize>12</fontsize>
  <color>
   <r>128</r>
   <g>128</g>
   <b>128</b>
  </color>
  <bgcolor mode="background">
   <r>0</r>
   <g>0</g>
   <b>0</b>
  </bgcolor>
  <value>80.92198473</value>
  <resolution>0.00100000</resolution>
  <minimum>-999999999999.00000000</minimum>
  <maximum>99999999999999.00000000</maximum>
  <bordermode>noborder</bordermode>
  <borderradius>1</borderradius>
  <borderwidth>1</borderwidth>
  <randomizable group="0">false</randomizable>
  <mouseControl act=""/>
 </bsbObject>
 <bsbObject version="2" type="BSBScrollNumber">
  <objectName>cps3</objectName>
  <x>100</x>
  <y>330</y>
  <width>38</width>
  <height>24</height>
  <uuid>{04fa1473-0e2e-403a-9d0b-30ab98150557}</uuid>
  <visible>true</visible>
  <midichan>0</midichan>
  <midicc>-3</midicc>
  <alignment>left</alignment>
  <font>Arial</font>
  <fontsize>12</fontsize>
  <color>
   <r>128</r>
   <g>128</g>
   <b>128</b>
  </color>
  <bgcolor mode="background">
   <r>0</r>
   <g>0</g>
   <b>0</b>
  </bgcolor>
  <value>0.03250000</value>
  <resolution>0.00100000</resolution>
  <minimum>-999999999999.00000000</minimum>
  <maximum>99999999999999.00000000</maximum>
  <bordermode>noborder</bordermode>
  <borderradius>1</borderradius>
  <borderwidth>1</borderwidth>
  <randomizable group="0">false</randomizable>
  <mouseControl act=""/>
 </bsbObject>
 <bsbObject version="2" type="BSBScrollNumber">
  <objectName>vel3</objectName>
  <x>139</x>
  <y>330</y>
  <width>38</width>
  <height>24</height>
  <uuid>{ab1842a3-e08b-4892-bc1a-948f36e5d820}</uuid>
  <visible>true</visible>
  <midichan>0</midichan>
  <midicc>-3</midicc>
  <alignment>left</alignment>
  <font>Arial</font>
  <fontsize>12</fontsize>
  <color>
   <r>128</r>
   <g>128</g>
   <b>128</b>
  </color>
  <bgcolor mode="background">
   <r>0</r>
   <g>0</g>
   <b>0</b>
  </bgcolor>
  <value>1.00000000</value>
  <resolution>0.00100000</resolution>
  <minimum>-999999999999.00000000</minimum>
  <maximum>99999999999999.00000000</maximum>
  <bordermode>noborder</bordermode>
  <borderradius>1</borderradius>
  <borderwidth>1</borderwidth>
  <randomizable group="0">false</randomizable>
  <mouseControl act=""/>
 </bsbObject>
 <bsbObject version="2" type="BSBScrollNumber">
  <objectName>fmd3</objectName>
  <x>178</x>
  <y>330</y>
  <width>38</width>
  <height>24</height>
  <uuid>{177a4244-7ab7-40f4-9685-1d542ed2dc7a}</uuid>
  <visible>true</visible>
  <midichan>0</midichan>
  <midicc>-3</midicc>
  <alignment>left</alignment>
  <font>Arial</font>
  <fontsize>12</fontsize>
  <color>
   <r>128</r>
   <g>128</g>
   <b>128</b>
  </color>
  <bgcolor mode="background">
   <r>0</r>
   <g>0</g>
   <b>0</b>
  </bgcolor>
  <value>0.12213740</value>
  <resolution>0.00100000</resolution>
  <minimum>-999999999999.00000000</minimum>
  <maximum>99999999999999.00000000</maximum>
  <bordermode>noborder</bordermode>
  <borderradius>1</borderradius>
  <borderwidth>1</borderwidth>
  <randomizable group="0">false</randomizable>
  <mouseControl act=""/>
 </bsbObject>
 <bsbObject version="2" type="BSBScrollNumber">
  <objectName>pmd3</objectName>
  <x>217</x>
  <y>330</y>
  <width>38</width>
  <height>24</height>
  <uuid>{46664a8d-ad7a-46ad-8baa-0262825d0af5}</uuid>
  <visible>true</visible>
  <midichan>0</midichan>
  <midicc>-3</midicc>
  <alignment>left</alignment>
  <font>Arial</font>
  <fontsize>12</fontsize>
  <color>
   <r>128</r>
   <g>128</g>
   <b>128</b>
  </color>
  <bgcolor mode="background">
   <r>0</r>
   <g>0</g>
   <b>0</b>
  </bgcolor>
  <value>1.00000000</value>
  <resolution>0.00100000</resolution>
  <minimum>-999999999999.00000000</minimum>
  <maximum>99999999999999.00000000</maximum>
  <bordermode>noborder</bordermode>
  <borderradius>1</borderradius>
  <borderwidth>1</borderwidth>
  <randomizable group="0">false</randomizable>
  <mouseControl act=""/>
 </bsbObject>
 <bsbObject version="2" type="BSBScrollNumber">
  <objectName/>
  <x>256</x>
  <y>330</y>
  <width>38</width>
  <height>24</height>
  <uuid>{30089f7d-7596-461e-80c4-41a07ebd501e}</uuid>
  <visible>true</visible>
  <midichan>0</midichan>
  <midicc>-3</midicc>
  <alignment>left</alignment>
  <font>Arial</font>
  <fontsize>12</fontsize>
  <color>
   <r>128</r>
   <g>128</g>
   <b>128</b>
  </color>
  <bgcolor mode="background">
   <r>0</r>
   <g>0</g>
   <b>0</b>
  </bgcolor>
  <value>0.00000000</value>
  <resolution>0.00100000</resolution>
  <minimum>-999999999999.00000000</minimum>
  <maximum>99999999999999.00000000</maximum>
  <bordermode>noborder</bordermode>
  <borderradius>1</borderradius>
  <borderwidth>1</borderwidth>
  <randomizable group="0">false</randomizable>
  <mouseControl act=""/>
 </bsbObject>
 <bsbObject version="2" type="BSBScrollNumber">
  <objectName/>
  <x>296</x>
  <y>330</y>
  <width>38</width>
  <height>24</height>
  <uuid>{3f96615b-810c-41ff-88df-4c7425ef0f55}</uuid>
  <visible>true</visible>
  <midichan>0</midichan>
  <midicc>-3</midicc>
  <alignment>left</alignment>
  <font>Arial</font>
  <fontsize>12</fontsize>
  <color>
   <r>128</r>
   <g>128</g>
   <b>128</b>
  </color>
  <bgcolor mode="background">
   <r>0</r>
   <g>0</g>
   <b>0</b>
  </bgcolor>
  <value>0.00000000</value>
  <resolution>0.00100000</resolution>
  <minimum>-999999999999.00000000</minimum>
  <maximum>99999999999999.00000000</maximum>
  <bordermode>noborder</bordermode>
  <borderradius>1</borderradius>
  <borderwidth>1</borderwidth>
  <randomizable group="0">false</randomizable>
  <mouseControl act=""/>
 </bsbObject>
 <bsbObject version="2" type="BSBScrollNumber">
  <objectName>dur3</objectName>
  <x>61</x>
  <y>330</y>
  <width>38</width>
  <height>24</height>
  <uuid>{b4329cb4-da20-4671-bc51-bd8b897d0f09}</uuid>
  <visible>true</visible>
  <midichan>0</midichan>
  <midicc>-3</midicc>
  <alignment>left</alignment>
  <font>Arial</font>
  <fontsize>12</fontsize>
  <color>
   <r>128</r>
   <g>128</g>
   <b>128</b>
  </color>
  <bgcolor mode="background">
   <r>0</r>
   <g>0</g>
   <b>0</b>
  </bgcolor>
  <value>0.50000000</value>
  <resolution>0.00100000</resolution>
  <minimum>-999999999999.00000000</minimum>
  <maximum>99999999999999.00000000</maximum>
  <bordermode>noborder</bordermode>
  <borderradius>1</borderradius>
  <borderwidth>1</borderwidth>
  <randomizable group="0">false</randomizable>
  <mouseControl act=""/>
 </bsbObject>
 <bsbObject version="2" type="BSBLabel">
  <objectName/>
  <x>690</x>
  <y>307</y>
  <width>38</width>
  <height>22</height>
  <uuid>{5a2d3680-927e-48a1-91e9-da9ab1fd7be0}</uuid>
  <visible>true</visible>
  <midichan>0</midichan>
  <midicc>-3</midicc>
  <label>DENS</label>
  <alignment>left</alignment>
  <font>Arial Black</font>
  <fontsize>12</fontsize>
  <precision>3</precision>
  <color>
   <r>204</r>
   <g>204</g>
   <b>204</b>
  </color>
  <bgcolor mode="nobackground">
   <r>255</r>
   <g>255</g>
   <b>255</b>
  </bgcolor>
  <bordermode>noborder</bordermode>
  <borderradius>1</borderradius>
  <borderwidth>1</borderwidth>
 </bsbObject>
 <bsbObject version="2" type="BSBLabel">
  <objectName/>
  <x>730</x>
  <y>307</y>
  <width>35</width>
  <height>22</height>
  <uuid>{8d7e4f8c-4c2c-4e7a-b3fa-d49b6de2c53d}</uuid>
  <visible>true</visible>
  <midichan>0</midichan>
  <midicc>-3</midicc>
  <label>DUR</label>
  <alignment>center</alignment>
  <font>Arial Black</font>
  <fontsize>12</fontsize>
  <precision>3</precision>
  <color>
   <r>204</r>
   <g>204</g>
   <b>204</b>
  </color>
  <bgcolor mode="nobackground">
   <r>255</r>
   <g>255</g>
   <b>255</b>
  </bgcolor>
  <bordermode>noborder</bordermode>
  <borderradius>1</borderradius>
  <borderwidth>1</borderwidth>
 </bsbObject>
 <bsbObject version="2" type="BSBLabel">
  <objectName/>
  <x>767</x>
  <y>307</y>
  <width>36</width>
  <height>22</height>
  <uuid>{0d83db62-b7c2-4aa7-a8ee-9f9e21fabc92}</uuid>
  <visible>true</visible>
  <midichan>0</midichan>
  <midicc>-3</midicc>
  <label>CPS
</label>
  <alignment>center</alignment>
  <font>Arial Black</font>
  <fontsize>12</fontsize>
  <precision>3</precision>
  <color>
   <r>204</r>
   <g>204</g>
   <b>204</b>
  </color>
  <bgcolor mode="nobackground">
   <r>255</r>
   <g>255</g>
   <b>255</b>
  </bgcolor>
  <bordermode>noborder</bordermode>
  <borderradius>1</borderradius>
  <borderwidth>1</borderwidth>
 </bsbObject>
 <bsbObject version="2" type="BSBLabel">
  <objectName/>
  <x>805</x>
  <y>307</y>
  <width>38</width>
  <height>22</height>
  <uuid>{bf60e396-69d5-4716-b973-673c5ad5889e}</uuid>
  <visible>true</visible>
  <midichan>0</midichan>
  <midicc>-3</midicc>
  <label>VEL
</label>
  <alignment>center</alignment>
  <font>Arial Black</font>
  <fontsize>12</fontsize>
  <precision>3</precision>
  <color>
   <r>204</r>
   <g>204</g>
   <b>204</b>
  </color>
  <bgcolor mode="nobackground">
   <r>255</r>
   <g>255</g>
   <b>255</b>
  </bgcolor>
  <bordermode>noborder</bordermode>
  <borderradius>1</borderradius>
  <borderwidth>1</borderwidth>
 </bsbObject>
 <bsbObject version="2" type="BSBLabel">
  <objectName/>
  <x>845</x>
  <y>307</y>
  <width>36</width>
  <height>22</height>
  <uuid>{ee569606-1fd7-40e4-83ea-aff2d46b80c5}</uuid>
  <visible>true</visible>
  <midichan>0</midichan>
  <midicc>-3</midicc>
  <label>FMD</label>
  <alignment>center</alignment>
  <font>Arial Black</font>
  <fontsize>12</fontsize>
  <precision>3</precision>
  <color>
   <r>204</r>
   <g>204</g>
   <b>204</b>
  </color>
  <bgcolor mode="nobackground">
   <r>255</r>
   <g>255</g>
   <b>255</b>
  </bgcolor>
  <bordermode>noborder</bordermode>
  <borderradius>1</borderradius>
  <borderwidth>1</borderwidth>
 </bsbObject>
 <bsbObject version="2" type="BSBLabel">
  <objectName/>
  <x>882</x>
  <y>307</y>
  <width>39</width>
  <height>22</height>
  <uuid>{0774ac46-2a28-43b3-a331-164af81508d5}</uuid>
  <visible>true</visible>
  <midichan>0</midichan>
  <midicc>-3</midicc>
  <label>PMD
</label>
  <alignment>center</alignment>
  <font>Arial Black</font>
  <fontsize>12</fontsize>
  <precision>3</precision>
  <color>
   <r>204</r>
   <g>204</g>
   <b>204</b>
  </color>
  <bgcolor mode="nobackground">
   <r>255</r>
   <g>255</g>
   <b>255</b>
  </bgcolor>
  <bordermode>noborder</bordermode>
  <borderradius>1</borderradius>
  <borderwidth>1</borderwidth>
 </bsbObject>
 <bsbObject version="2" type="BSBLabel">
  <objectName/>
  <x>917</x>
  <y>306</y>
  <width>46</width>
  <height>22</height>
  <uuid>{237f1e84-1e95-4120-bcf5-20deed195d57}</uuid>
  <visible>true</visible>
  <midichan>0</midichan>
  <midicc>-3</midicc>
  <label>FRPOW</label>
  <alignment>center</alignment>
  <font>Arial Black</font>
  <fontsize>12</fontsize>
  <precision>3</precision>
  <color>
   <r>204</r>
   <g>204</g>
   <b>204</b>
  </color>
  <bgcolor mode="nobackground">
   <r>255</r>
   <g>255</g>
   <b>255</b>
  </bgcolor>
  <bordermode>noborder</bordermode>
  <borderradius>1</borderradius>
  <borderwidth>1</borderwidth>
 </bsbObject>
 <bsbObject version="2" type="BSBLabel">
  <objectName/>
  <x>958</x>
  <y>306</y>
  <width>48</width>
  <height>22</height>
  <uuid>{6c5fd7aa-f46e-4a29-9c00-e5d3f3ee98fe}</uuid>
  <visible>true</visible>
  <midichan>0</midichan>
  <midicc>-3</midicc>
  <label>PRPOW</label>
  <alignment>center</alignment>
  <font>Arial Black</font>
  <fontsize>12</fontsize>
  <precision>3</precision>
  <color>
   <r>204</r>
   <g>204</g>
   <b>204</b>
  </color>
  <bgcolor mode="nobackground">
   <r>255</r>
   <g>255</g>
   <b>255</b>
  </bgcolor>
  <bordermode>noborder</bordermode>
  <borderradius>1</borderradius>
  <borderwidth>1</borderwidth>
 </bsbObject>
 <bsbObject version="2" type="BSBVSlider">
  <objectName>dens4</objectName>
  <x>697</x>
  <y>352</y>
  <width>21</width>
  <height>131</height>
  <uuid>{36514580-05b8-463e-b011-bc7623e87333}</uuid>
  <visible>true</visible>
  <midichan>0</midichan>
  <midicc>-3</midicc>
  <minimum>0.01000000</minimum>
  <maximum>200.00000000</maximum>
  <value>67.18221374</value>
  <mode>lin</mode>
  <mouseControl act="jump">continuous</mouseControl>
  <resolution>-1.00000000</resolution>
  <randomizable group="0">false</randomizable>
 </bsbObject>
 <bsbObject version="2" type="BSBVSlider">
  <objectName>prpow4</objectName>
  <x>970</x>
  <y>352</y>
  <width>21</width>
  <height>131</height>
  <uuid>{10744156-fbda-4191-930e-0175137f662c}</uuid>
  <visible>true</visible>
  <midichan>0</midichan>
  <midicc>-3</midicc>
  <minimum>0.00000000</minimum>
  <maximum>1.00000000</maximum>
  <value>1.00000000</value>
  <mode>lin</mode>
  <mouseControl act="jump">continuous</mouseControl>
  <resolution>-1.00000000</resolution>
  <randomizable group="0">false</randomizable>
 </bsbObject>
 <bsbObject version="2" type="BSBVSlider">
  <objectName>frpow4</objectName>
  <x>931</x>
  <y>352</y>
  <width>21</width>
  <height>131</height>
  <uuid>{cabe667f-970d-4e14-abb3-7cdb11104d54}</uuid>
  <visible>true</visible>
  <midichan>0</midichan>
  <midicc>-3</midicc>
  <minimum>0.00000000</minimum>
  <maximum>1.00000000</maximum>
  <value>1.00000000</value>
  <mode>lin</mode>
  <mouseControl act="jump">continuous</mouseControl>
  <resolution>-1.00000000</resolution>
  <randomizable group="0">false</randomizable>
 </bsbObject>
 <bsbObject version="2" type="BSBVSlider">
  <objectName>pmd4</objectName>
  <x>891</x>
  <y>352</y>
  <width>21</width>
  <height>131</height>
  <uuid>{66dcd458-e051-435f-9fc3-1d901d2976a8}</uuid>
  <visible>true</visible>
  <midichan>0</midichan>
  <midicc>-3</midicc>
  <minimum>0.00000000</minimum>
  <maximum>1.00000000</maximum>
  <value>1.00000000</value>
  <mode>lin</mode>
  <mouseControl act="jump">continuous</mouseControl>
  <resolution>-1.00000000</resolution>
  <randomizable group="0">false</randomizable>
 </bsbObject>
 <bsbObject version="2" type="BSBVSlider">
  <objectName>fmd4</objectName>
  <x>852</x>
  <y>352</y>
  <width>21</width>
  <height>131</height>
  <uuid>{9b1e3517-a55e-49ee-b284-947bf39917ed}</uuid>
  <visible>true</visible>
  <midichan>0</midichan>
  <midicc>-3</midicc>
  <minimum>0.00000000</minimum>
  <maximum>1.00000000</maximum>
  <value>0.12213740</value>
  <mode>lin</mode>
  <mouseControl act="jump">continuous</mouseControl>
  <resolution>-1.00000000</resolution>
  <randomizable group="0">false</randomizable>
 </bsbObject>
 <bsbObject version="2" type="BSBVSlider">
  <objectName>vel4</objectName>
  <x>812</x>
  <y>352</y>
  <width>21</width>
  <height>131</height>
  <uuid>{41d24da6-1f59-4149-9fd5-24a9475a4234}</uuid>
  <visible>true</visible>
  <midichan>0</midichan>
  <midicc>-3</midicc>
  <minimum>0.00000000</minimum>
  <maximum>1.00000000</maximum>
  <value>1.00000000</value>
  <mode>lin</mode>
  <mouseControl act="jump">continuous</mouseControl>
  <resolution>-1.00000000</resolution>
  <randomizable group="0">false</randomizable>
 </bsbObject>
 <bsbObject version="2" type="BSBVSlider">
  <objectName>cps4</objectName>
  <x>776</x>
  <y>352</y>
  <width>21</width>
  <height>131</height>
  <uuid>{1990f442-8d09-4ab9-8eea-b2c8e328ff6d}</uuid>
  <visible>true</visible>
  <midichan>0</midichan>
  <midicc>-3</midicc>
  <minimum>0.03250000</minimum>
  <maximum>6.00000000</maximum>
  <value>6.00000000</value>
  <mode>lin</mode>
  <mouseControl act="jump">continuous</mouseControl>
  <resolution>-1.00000000</resolution>
  <randomizable group="0">false</randomizable>
 </bsbObject>
 <bsbObject version="2" type="BSBVSlider">
  <objectName>dur4</objectName>
  <x>736</x>
  <y>352</y>
  <width>21</width>
  <height>131</height>
  <uuid>{32cdceec-1b7c-4064-a902-9fafc8ce553b}</uuid>
  <visible>true</visible>
  <midichan>0</midichan>
  <midicc>-3</midicc>
  <minimum>0.01000000</minimum>
  <maximum>0.50000000</maximum>
  <value>0.50000000</value>
  <mode>lin</mode>
  <mouseControl act="jump">continuous</mouseControl>
  <resolution>-1.00000000</resolution>
  <randomizable group="0">false</randomizable>
 </bsbObject>
 <bsbObject version="2" type="BSBScrollNumber">
  <objectName>dens4</objectName>
  <x>690</x>
  <y>329</y>
  <width>38</width>
  <height>24</height>
  <uuid>{a812c95a-5e1f-47a6-9520-a057840598b4}</uuid>
  <visible>true</visible>
  <midichan>0</midichan>
  <midicc>-3</midicc>
  <alignment>left</alignment>
  <font>Arial</font>
  <fontsize>12</fontsize>
  <color>
   <r>128</r>
   <g>128</g>
   <b>128</b>
  </color>
  <bgcolor mode="background">
   <r>0</r>
   <g>0</g>
   <b>0</b>
  </bgcolor>
  <value>67.18221374</value>
  <resolution>0.00100000</resolution>
  <minimum>-999999999999.00000000</minimum>
  <maximum>99999999999999.00000000</maximum>
  <bordermode>noborder</bordermode>
  <borderradius>1</borderradius>
  <borderwidth>1</borderwidth>
  <randomizable group="0">false</randomizable>
  <mouseControl act=""/>
 </bsbObject>
 <bsbObject version="2" type="BSBScrollNumber">
  <objectName>cps4</objectName>
  <x>766</x>
  <y>329</y>
  <width>38</width>
  <height>24</height>
  <uuid>{606e4551-33bb-4706-a072-8847c15ccf71}</uuid>
  <visible>true</visible>
  <midichan>0</midichan>
  <midicc>-3</midicc>
  <alignment>left</alignment>
  <font>Arial</font>
  <fontsize>12</fontsize>
  <color>
   <r>128</r>
   <g>128</g>
   <b>128</b>
  </color>
  <bgcolor mode="background">
   <r>0</r>
   <g>0</g>
   <b>0</b>
  </bgcolor>
  <value>6.00000000</value>
  <resolution>0.00100000</resolution>
  <minimum>-999999999999.00000000</minimum>
  <maximum>99999999999999.00000000</maximum>
  <bordermode>noborder</bordermode>
  <borderradius>1</borderradius>
  <borderwidth>1</borderwidth>
  <randomizable group="0">false</randomizable>
  <mouseControl act=""/>
 </bsbObject>
 <bsbObject version="2" type="BSBScrollNumber">
  <objectName>vel4</objectName>
  <x>805</x>
  <y>329</y>
  <width>38</width>
  <height>24</height>
  <uuid>{06c442c0-6a6f-4e30-8b87-d685aec8e8d1}</uuid>
  <visible>true</visible>
  <midichan>0</midichan>
  <midicc>-3</midicc>
  <alignment>left</alignment>
  <font>Arial</font>
  <fontsize>12</fontsize>
  <color>
   <r>128</r>
   <g>128</g>
   <b>128</b>
  </color>
  <bgcolor mode="background">
   <r>0</r>
   <g>0</g>
   <b>0</b>
  </bgcolor>
  <value>1.00000000</value>
  <resolution>0.00100000</resolution>
  <minimum>-999999999999.00000000</minimum>
  <maximum>99999999999999.00000000</maximum>
  <bordermode>noborder</bordermode>
  <borderradius>1</borderradius>
  <borderwidth>1</borderwidth>
  <randomizable group="0">false</randomizable>
  <mouseControl act=""/>
 </bsbObject>
 <bsbObject version="2" type="BSBScrollNumber">
  <objectName>fmd4</objectName>
  <x>844</x>
  <y>329</y>
  <width>38</width>
  <height>24</height>
  <uuid>{d76af2ff-f15d-4c26-aa73-2e8455324623}</uuid>
  <visible>true</visible>
  <midichan>0</midichan>
  <midicc>-3</midicc>
  <alignment>left</alignment>
  <font>Arial</font>
  <fontsize>12</fontsize>
  <color>
   <r>128</r>
   <g>128</g>
   <b>128</b>
  </color>
  <bgcolor mode="background">
   <r>0</r>
   <g>0</g>
   <b>0</b>
  </bgcolor>
  <value>0.12213740</value>
  <resolution>0.00100000</resolution>
  <minimum>-999999999999.00000000</minimum>
  <maximum>99999999999999.00000000</maximum>
  <bordermode>noborder</bordermode>
  <borderradius>1</borderradius>
  <borderwidth>1</borderwidth>
  <randomizable group="0">false</randomizable>
  <mouseControl act=""/>
 </bsbObject>
 <bsbObject version="2" type="BSBScrollNumber">
  <objectName>pmd4</objectName>
  <x>883</x>
  <y>329</y>
  <width>38</width>
  <height>24</height>
  <uuid>{c21b576f-10ce-4799-bced-52177e356623}</uuid>
  <visible>true</visible>
  <midichan>0</midichan>
  <midicc>-3</midicc>
  <alignment>left</alignment>
  <font>Arial</font>
  <fontsize>12</fontsize>
  <color>
   <r>128</r>
   <g>128</g>
   <b>128</b>
  </color>
  <bgcolor mode="background">
   <r>0</r>
   <g>0</g>
   <b>0</b>
  </bgcolor>
  <value>1.00000000</value>
  <resolution>0.00100000</resolution>
  <minimum>-999999999999.00000000</minimum>
  <maximum>99999999999999.00000000</maximum>
  <bordermode>noborder</bordermode>
  <borderradius>1</borderradius>
  <borderwidth>1</borderwidth>
  <randomizable group="0">false</randomizable>
  <mouseControl act=""/>
 </bsbObject>
 <bsbObject version="2" type="BSBScrollNumber">
  <objectName/>
  <x>922</x>
  <y>329</y>
  <width>38</width>
  <height>24</height>
  <uuid>{3a5f4ff4-5a1e-4ce4-8670-62369384087d}</uuid>
  <visible>true</visible>
  <midichan>0</midichan>
  <midicc>-3</midicc>
  <alignment>left</alignment>
  <font>Arial</font>
  <fontsize>12</fontsize>
  <color>
   <r>128</r>
   <g>128</g>
   <b>128</b>
  </color>
  <bgcolor mode="background">
   <r>0</r>
   <g>0</g>
   <b>0</b>
  </bgcolor>
  <value>0.00000000</value>
  <resolution>0.00100000</resolution>
  <minimum>-999999999999.00000000</minimum>
  <maximum>99999999999999.00000000</maximum>
  <bordermode>noborder</bordermode>
  <borderradius>1</borderradius>
  <borderwidth>1</borderwidth>
  <randomizable group="0">false</randomizable>
  <mouseControl act=""/>
 </bsbObject>
 <bsbObject version="2" type="BSBScrollNumber">
  <objectName/>
  <x>962</x>
  <y>328</y>
  <width>38</width>
  <height>24</height>
  <uuid>{e94564e3-385a-410c-954e-ac75ed287d29}</uuid>
  <visible>true</visible>
  <midichan>0</midichan>
  <midicc>0</midicc>
  <alignment>left</alignment>
  <font>Arial</font>
  <fontsize>12</fontsize>
  <color>
   <r>128</r>
   <g>128</g>
   <b>128</b>
  </color>
  <bgcolor mode="background">
   <r>0</r>
   <g>0</g>
   <b>0</b>
  </bgcolor>
  <value>0.00000000</value>
  <resolution>0.00100000</resolution>
  <minimum>-999999999999.00000000</minimum>
  <maximum>999999999999.00000000</maximum>
  <bordermode>noborder</bordermode>
  <borderradius>1</borderradius>
  <borderwidth>1</borderwidth>
  <randomizable group="0">false</randomizable>
  <mouseControl act=""/>
 </bsbObject>
 <bsbObject version="2" type="BSBScrollNumber">
  <objectName>dur4</objectName>
  <x>727</x>
  <y>329</y>
  <width>38</width>
  <height>24</height>
  <uuid>{2bec1261-f099-4e7e-9689-e4f8cd44cae6}</uuid>
  <visible>true</visible>
  <midichan>0</midichan>
  <midicc>-3</midicc>
  <alignment>left</alignment>
  <font>Arial</font>
  <fontsize>12</fontsize>
  <color>
   <r>128</r>
   <g>128</g>
   <b>128</b>
  </color>
  <bgcolor mode="background">
   <r>0</r>
   <g>0</g>
   <b>0</b>
  </bgcolor>
  <value>0.50000000</value>
  <resolution>0.00100000</resolution>
  <minimum>-999999999999.00000000</minimum>
  <maximum>99999999999999.00000000</maximum>
  <bordermode>noborder</bordermode>
  <borderradius>1</borderradius>
  <borderwidth>1</borderwidth>
  <randomizable group="0">false</randomizable>
  <mouseControl act=""/>
 </bsbObject>
 <bsbObject version="2" type="BSBLabel">
  <objectName/>
  <x>291</x>
  <y>541</y>
  <width>39</width>
  <height>24</height>
  <uuid>{0575d95e-f3f0-4bf8-bca8-5152854c44c8}</uuid>
  <visible>true</visible>
  <midichan>0</midichan>
  <midicc>-3</midicc>
  <label>DENS</label>
  <alignment>left</alignment>
  <font>Arial Black</font>
  <fontsize>12</fontsize>
  <precision>3</precision>
  <color>
   <r>204</r>
   <g>204</g>
   <b>204</b>
  </color>
  <bgcolor mode="nobackground">
   <r>255</r>
   <g>255</g>
   <b>255</b>
  </bgcolor>
  <bordermode>noborder</bordermode>
  <borderradius>1</borderradius>
  <borderwidth>1</borderwidth>
 </bsbObject>
 <bsbObject version="2" type="BSBLabel">
  <objectName/>
  <x>331</x>
  <y>541</y>
  <width>36</width>
  <height>24</height>
  <uuid>{985cec93-efd8-4091-9431-7de934cc7c6c}</uuid>
  <visible>true</visible>
  <midichan>0</midichan>
  <midicc>-3</midicc>
  <label>DUR</label>
  <alignment>center</alignment>
  <font>Arial Black</font>
  <fontsize>12</fontsize>
  <precision>3</precision>
  <color>
   <r>204</r>
   <g>204</g>
   <b>204</b>
  </color>
  <bgcolor mode="nobackground">
   <r>255</r>
   <g>255</g>
   <b>255</b>
  </bgcolor>
  <bordermode>noborder</bordermode>
  <borderradius>1</borderradius>
  <borderwidth>1</borderwidth>
 </bsbObject>
 <bsbObject version="2" type="BSBLabel">
  <objectName/>
  <x>368</x>
  <y>541</y>
  <width>37</width>
  <height>24</height>
  <uuid>{828cebf3-a297-4ed5-8745-717788682af3}</uuid>
  <visible>true</visible>
  <midichan>0</midichan>
  <midicc>-3</midicc>
  <label>CPS
</label>
  <alignment>center</alignment>
  <font>Arial Black</font>
  <fontsize>12</fontsize>
  <precision>3</precision>
  <color>
   <r>204</r>
   <g>204</g>
   <b>204</b>
  </color>
  <bgcolor mode="nobackground">
   <r>255</r>
   <g>255</g>
   <b>255</b>
  </bgcolor>
  <bordermode>noborder</bordermode>
  <borderradius>1</borderradius>
  <borderwidth>1</borderwidth>
 </bsbObject>
 <bsbObject version="2" type="BSBLabel">
  <objectName/>
  <x>406</x>
  <y>541</y>
  <width>39</width>
  <height>24</height>
  <uuid>{b037e4f2-3a6c-4b46-8b90-28e808109741}</uuid>
  <visible>true</visible>
  <midichan>0</midichan>
  <midicc>-3</midicc>
  <label>VEL
</label>
  <alignment>center</alignment>
  <font>Arial Black</font>
  <fontsize>12</fontsize>
  <precision>3</precision>
  <color>
   <r>204</r>
   <g>204</g>
   <b>204</b>
  </color>
  <bgcolor mode="nobackground">
   <r>255</r>
   <g>255</g>
   <b>255</b>
  </bgcolor>
  <bordermode>noborder</bordermode>
  <borderradius>1</borderradius>
  <borderwidth>1</borderwidth>
 </bsbObject>
 <bsbObject version="2" type="BSBLabel">
  <objectName/>
  <x>446</x>
  <y>541</y>
  <width>37</width>
  <height>24</height>
  <uuid>{87b77fb2-c02e-44db-906f-1ed1170c2197}</uuid>
  <visible>true</visible>
  <midichan>0</midichan>
  <midicc>-3</midicc>
  <label>FMD</label>
  <alignment>center</alignment>
  <font>Arial Black</font>
  <fontsize>12</fontsize>
  <precision>3</precision>
  <color>
   <r>204</r>
   <g>204</g>
   <b>204</b>
  </color>
  <bgcolor mode="nobackground">
   <r>255</r>
   <g>255</g>
   <b>255</b>
  </bgcolor>
  <bordermode>noborder</bordermode>
  <borderradius>1</borderradius>
  <borderwidth>1</borderwidth>
 </bsbObject>
 <bsbObject version="2" type="BSBLabel">
  <objectName/>
  <x>483</x>
  <y>541</y>
  <width>40</width>
  <height>24</height>
  <uuid>{a7050d4c-e4c9-41f5-865b-7bfedf647db6}</uuid>
  <visible>true</visible>
  <midichan>0</midichan>
  <midicc>-3</midicc>
  <label>PMD
</label>
  <alignment>center</alignment>
  <font>Arial Black</font>
  <fontsize>12</fontsize>
  <precision>3</precision>
  <color>
   <r>204</r>
   <g>204</g>
   <b>204</b>
  </color>
  <bgcolor mode="nobackground">
   <r>255</r>
   <g>255</g>
   <b>255</b>
  </bgcolor>
  <bordermode>noborder</bordermode>
  <borderradius>1</borderradius>
  <borderwidth>1</borderwidth>
 </bsbObject>
 <bsbObject version="2" type="BSBLabel">
  <objectName/>
  <x>518</x>
  <y>541</y>
  <width>47</width>
  <height>24</height>
  <uuid>{6bf4146c-4d36-4198-9839-15549fac6ec3}</uuid>
  <visible>true</visible>
  <midichan>0</midichan>
  <midicc>-3</midicc>
  <label>FRPOW</label>
  <alignment>center</alignment>
  <font>Arial Black</font>
  <fontsize>12</fontsize>
  <precision>3</precision>
  <color>
   <r>204</r>
   <g>204</g>
   <b>204</b>
  </color>
  <bgcolor mode="nobackground">
   <r>255</r>
   <g>255</g>
   <b>255</b>
  </bgcolor>
  <bordermode>noborder</bordermode>
  <borderradius>1</borderradius>
  <borderwidth>1</borderwidth>
 </bsbObject>
 <bsbObject version="2" type="BSBLabel">
  <objectName/>
  <x>559</x>
  <y>541</y>
  <width>49</width>
  <height>24</height>
  <uuid>{06c937fb-18cd-423f-a0b4-df4001e2e2cd}</uuid>
  <visible>true</visible>
  <midichan>0</midichan>
  <midicc>-3</midicc>
  <label>PRPOW</label>
  <alignment>center</alignment>
  <font>Arial Black</font>
  <fontsize>12</fontsize>
  <precision>3</precision>
  <color>
   <r>204</r>
   <g>204</g>
   <b>204</b>
  </color>
  <bgcolor mode="nobackground">
   <r>255</r>
   <g>255</g>
   <b>255</b>
  </bgcolor>
  <bordermode>noborder</bordermode>
  <borderradius>1</borderradius>
  <borderwidth>1</borderwidth>
 </bsbObject>
 <bsbObject version="2" type="BSBVSlider">
  <objectName>dens5</objectName>
  <x>298</x>
  <y>587</y>
  <width>22</width>
  <height>133</height>
  <uuid>{b8bfb42c-db5e-4c3d-bab9-3681d313d3f6}</uuid>
  <visible>true</visible>
  <midichan>0</midichan>
  <midicc>-3</midicc>
  <minimum>1.00000000</minimum>
  <maximum>50.00000000</maximum>
  <value>50.00000000</value>
  <mode>lin</mode>
  <mouseControl act="jump">continuous</mouseControl>
  <resolution>-1.00000000</resolution>
  <randomizable group="0">false</randomizable>
 </bsbObject>
 <bsbObject version="2" type="BSBVSlider">
  <objectName>prpow5</objectName>
  <x>571</x>
  <y>587</y>
  <width>22</width>
  <height>133</height>
  <uuid>{256ca81c-33c5-4aa7-95c1-08419486893f}</uuid>
  <visible>true</visible>
  <midichan>0</midichan>
  <midicc>-3</midicc>
  <minimum>0.00000000</minimum>
  <maximum>1.00000000</maximum>
  <value>1.00000000</value>
  <mode>lin</mode>
  <mouseControl act="jump">continuous</mouseControl>
  <resolution>-1.00000000</resolution>
  <randomizable group="0">false</randomizable>
 </bsbObject>
 <bsbObject version="2" type="BSBVSlider">
  <objectName>frpow5</objectName>
  <x>532</x>
  <y>587</y>
  <width>22</width>
  <height>133</height>
  <uuid>{d62bc00b-f3b5-4ce4-b90f-ab0044d2b4a5}</uuid>
  <visible>true</visible>
  <midichan>0</midichan>
  <midicc>-3</midicc>
  <minimum>0.00000000</minimum>
  <maximum>1.00000000</maximum>
  <value>1.00000000</value>
  <mode>lin</mode>
  <mouseControl act="jump">continuous</mouseControl>
  <resolution>-1.00000000</resolution>
  <randomizable group="0">false</randomizable>
 </bsbObject>
 <bsbObject version="2" type="BSBVSlider">
  <objectName>pmd5</objectName>
  <x>492</x>
  <y>587</y>
  <width>22</width>
  <height>133</height>
  <uuid>{3f7f83f1-95f0-4ad7-a1ba-7a9d2ff939cd}</uuid>
  <visible>true</visible>
  <midichan>0</midichan>
  <midicc>-3</midicc>
  <minimum>0.00000000</minimum>
  <maximum>1.00000000</maximum>
  <value>0.00000000</value>
  <mode>lin</mode>
  <mouseControl act="jump">continuous</mouseControl>
  <resolution>-1.00000000</resolution>
  <randomizable group="0">false</randomizable>
 </bsbObject>
 <bsbObject version="2" type="BSBVSlider">
  <objectName>fmd5</objectName>
  <x>453</x>
  <y>587</y>
  <width>22</width>
  <height>133</height>
  <uuid>{a3d8addb-99a7-4387-ac89-9fe5cd06c08e}</uuid>
  <visible>true</visible>
  <midichan>0</midichan>
  <midicc>-3</midicc>
  <minimum>0.00000000</minimum>
  <maximum>1.00000000</maximum>
  <value>0.00000000</value>
  <mode>lin</mode>
  <mouseControl act="jump">continuous</mouseControl>
  <resolution>-1.00000000</resolution>
  <randomizable group="0">false</randomizable>
 </bsbObject>
 <bsbObject version="2" type="BSBVSlider">
  <objectName>vel5</objectName>
  <x>413</x>
  <y>587</y>
  <width>22</width>
  <height>133</height>
  <uuid>{5e55eb8c-a28d-444f-83ac-faa3aa5d28e0}</uuid>
  <visible>true</visible>
  <midichan>0</midichan>
  <midicc>-3</midicc>
  <minimum>0.00000000</minimum>
  <maximum>1.00000000</maximum>
  <value>1.00000000</value>
  <mode>lin</mode>
  <mouseControl act="jump">continuous</mouseControl>
  <resolution>-1.00000000</resolution>
  <randomizable group="0">false</randomizable>
 </bsbObject>
 <bsbObject version="2" type="BSBVSlider">
  <objectName>cps5</objectName>
  <x>376</x>
  <y>587</y>
  <width>22</width>
  <height>133</height>
  <uuid>{1ace283f-162b-47a5-a992-63ee5317b1f9}</uuid>
  <visible>true</visible>
  <midichan>0</midichan>
  <midicc>-3</midicc>
  <minimum>0.03250000</minimum>
  <maximum>6.00000000</maximum>
  <value>0.03250000</value>
  <mode>lin</mode>
  <mouseControl act="jump">continuous</mouseControl>
  <resolution>-1.00000000</resolution>
  <randomizable group="0">false</randomizable>
 </bsbObject>
 <bsbObject version="2" type="BSBVSlider">
  <objectName>dur5</objectName>
  <x>337</x>
  <y>587</y>
  <width>22</width>
  <height>133</height>
  <uuid>{94839f78-9f80-4aad-a3e2-8b565acb6209}</uuid>
  <visible>true</visible>
  <midichan>0</midichan>
  <midicc>-3</midicc>
  <minimum>0.01000000</minimum>
  <maximum>0.50000000</maximum>
  <value>0.50000000</value>
  <mode>lin</mode>
  <mouseControl act="jump">continuous</mouseControl>
  <resolution>-1.00000000</resolution>
  <randomizable group="0">false</randomizable>
 </bsbObject>
 <bsbObject version="2" type="BSBScrollNumber">
  <objectName>dens5</objectName>
  <x>291</x>
  <y>563</y>
  <width>39</width>
  <height>26</height>
  <uuid>{7f789549-a9dd-4dee-8572-528267190df3}</uuid>
  <visible>true</visible>
  <midichan>0</midichan>
  <midicc>-3</midicc>
  <alignment>left</alignment>
  <font>Arial</font>
  <fontsize>12</fontsize>
  <color>
   <r>128</r>
   <g>128</g>
   <b>128</b>
  </color>
  <bgcolor mode="background">
   <r>0</r>
   <g>0</g>
   <b>0</b>
  </bgcolor>
  <value>50.00000000</value>
  <resolution>0.00100000</resolution>
  <minimum>-999999999999.00000000</minimum>
  <maximum>99999999999999.00000000</maximum>
  <bordermode>noborder</bordermode>
  <borderradius>1</borderradius>
  <borderwidth>1</borderwidth>
  <randomizable group="0">false</randomizable>
  <mouseControl act=""/>
 </bsbObject>
 <bsbObject version="2" type="BSBScrollNumber">
  <objectName>cps5</objectName>
  <x>367</x>
  <y>563</y>
  <width>39</width>
  <height>26</height>
  <uuid>{9db9cb6a-ff37-4cf3-bfda-db61ae7e213e}</uuid>
  <visible>true</visible>
  <midichan>0</midichan>
  <midicc>-3</midicc>
  <alignment>left</alignment>
  <font>Arial</font>
  <fontsize>12</fontsize>
  <color>
   <r>128</r>
   <g>128</g>
   <b>128</b>
  </color>
  <bgcolor mode="background">
   <r>0</r>
   <g>0</g>
   <b>0</b>
  </bgcolor>
  <value>0.03250000</value>
  <resolution>0.00100000</resolution>
  <minimum>-999999999999.00000000</minimum>
  <maximum>99999999999999.00000000</maximum>
  <bordermode>noborder</bordermode>
  <borderradius>1</borderradius>
  <borderwidth>1</borderwidth>
  <randomizable group="0">false</randomizable>
  <mouseControl act=""/>
 </bsbObject>
 <bsbObject version="2" type="BSBScrollNumber">
  <objectName>vel5</objectName>
  <x>406</x>
  <y>563</y>
  <width>39</width>
  <height>26</height>
  <uuid>{ac7e928f-6ad3-435d-9c93-9879877c8342}</uuid>
  <visible>true</visible>
  <midichan>0</midichan>
  <midicc>-3</midicc>
  <alignment>left</alignment>
  <font>Arial</font>
  <fontsize>12</fontsize>
  <color>
   <r>128</r>
   <g>128</g>
   <b>128</b>
  </color>
  <bgcolor mode="background">
   <r>0</r>
   <g>0</g>
   <b>0</b>
  </bgcolor>
  <value>0.00000000</value>
  <resolution>0.00100000</resolution>
  <minimum>-999999999999.00000000</minimum>
  <maximum>99999999999999.00000000</maximum>
  <bordermode>noborder</bordermode>
  <borderradius>1</borderradius>
  <borderwidth>1</borderwidth>
  <randomizable group="0">false</randomizable>
  <mouseControl act=""/>
 </bsbObject>
 <bsbObject version="2" type="BSBScrollNumber">
  <objectName>fmd5</objectName>
  <x>445</x>
  <y>563</y>
  <width>39</width>
  <height>26</height>
  <uuid>{dadc1945-57c3-48c8-9409-26550ba9c089}</uuid>
  <visible>true</visible>
  <midichan>0</midichan>
  <midicc>-3</midicc>
  <alignment>left</alignment>
  <font>Arial</font>
  <fontsize>12</fontsize>
  <color>
   <r>128</r>
   <g>128</g>
   <b>128</b>
  </color>
  <bgcolor mode="background">
   <r>0</r>
   <g>0</g>
   <b>0</b>
  </bgcolor>
  <value>0.00000000</value>
  <resolution>0.00100000</resolution>
  <minimum>-999999999999.00000000</minimum>
  <maximum>99999999999999.00000000</maximum>
  <bordermode>noborder</bordermode>
  <borderradius>1</borderradius>
  <borderwidth>1</borderwidth>
  <randomizable group="0">false</randomizable>
  <mouseControl act=""/>
 </bsbObject>
 <bsbObject version="2" type="BSBScrollNumber">
  <objectName>pmd5</objectName>
  <x>484</x>
  <y>563</y>
  <width>39</width>
  <height>26</height>
  <uuid>{69abf54d-830d-40fb-a3a0-c0e2ee008fe2}</uuid>
  <visible>true</visible>
  <midichan>0</midichan>
  <midicc>-3</midicc>
  <alignment>left</alignment>
  <font>Arial</font>
  <fontsize>12</fontsize>
  <color>
   <r>128</r>
   <g>128</g>
   <b>128</b>
  </color>
  <bgcolor mode="background">
   <r>0</r>
   <g>0</g>
   <b>0</b>
  </bgcolor>
  <value>0.00000000</value>
  <resolution>0.00100000</resolution>
  <minimum>-999999999999.00000000</minimum>
  <maximum>99999999999999.00000000</maximum>
  <bordermode>noborder</bordermode>
  <borderradius>1</borderradius>
  <borderwidth>1</borderwidth>
  <randomizable group="0">false</randomizable>
  <mouseControl act=""/>
 </bsbObject>
 <bsbObject version="2" type="BSBScrollNumber">
  <objectName>frpow5</objectName>
  <x>523</x>
  <y>563</y>
  <width>39</width>
  <height>26</height>
  <uuid>{f7dab61a-7c7e-402c-bc3c-9e774dbab95c}</uuid>
  <visible>true</visible>
  <midichan>0</midichan>
  <midicc>-3</midicc>
  <alignment>left</alignment>
  <font>Arial</font>
  <fontsize>12</fontsize>
  <color>
   <r>128</r>
   <g>128</g>
   <b>128</b>
  </color>
  <bgcolor mode="background">
   <r>0</r>
   <g>0</g>
   <b>0</b>
  </bgcolor>
  <value>1.00000000</value>
  <resolution>0.00100000</resolution>
  <minimum>-999999999999.00000000</minimum>
  <maximum>99999999999999.00000000</maximum>
  <bordermode>noborder</bordermode>
  <borderradius>1</borderradius>
  <borderwidth>1</borderwidth>
  <randomizable group="0">false</randomizable>
  <mouseControl act=""/>
 </bsbObject>
 <bsbObject version="2" type="BSBScrollNumber">
  <objectName>frpow5</objectName>
  <x>563</x>
  <y>563</y>
  <width>39</width>
  <height>26</height>
  <uuid>{0e0700cb-ab42-48ca-b1b9-c5451642a7cb}</uuid>
  <visible>true</visible>
  <midichan>0</midichan>
  <midicc>-3</midicc>
  <alignment>left</alignment>
  <font>Arial</font>
  <fontsize>12</fontsize>
  <color>
   <r>128</r>
   <g>128</g>
   <b>128</b>
  </color>
  <bgcolor mode="background">
   <r>0</r>
   <g>0</g>
   <b>0</b>
  </bgcolor>
  <value>1.00000000</value>
  <resolution>0.00100000</resolution>
  <minimum>-999999999999.00000000</minimum>
  <maximum>99999999999999.00000000</maximum>
  <bordermode>noborder</bordermode>
  <borderradius>1</borderradius>
  <borderwidth>1</borderwidth>
  <randomizable group="0">false</randomizable>
  <mouseControl act=""/>
 </bsbObject>
 <bsbObject version="2" type="BSBScrollNumber">
  <objectName>dur5</objectName>
  <x>328</x>
  <y>563</y>
  <width>39</width>
  <height>26</height>
  <uuid>{1b16ba92-c512-47f9-a6a8-25355f1750a6}</uuid>
  <visible>true</visible>
  <midichan>0</midichan>
  <midicc>-3</midicc>
  <alignment>left</alignment>
  <font>Arial</font>
  <fontsize>12</fontsize>
  <color>
   <r>128</r>
   <g>128</g>
   <b>128</b>
  </color>
  <bgcolor mode="background">
   <r>0</r>
   <g>0</g>
   <b>0</b>
  </bgcolor>
  <value>0.50000000</value>
  <resolution>0.00100000</resolution>
  <minimum>-999999999999.00000000</minimum>
  <maximum>99999999999999.00000000</maximum>
  <bordermode>noborder</bordermode>
  <borderradius>1</borderradius>
  <borderwidth>1</borderwidth>
  <randomizable group="0">false</randomizable>
  <mouseControl act=""/>
 </bsbObject>
 <bsbObject version="2" type="BSBVSlider">
  <objectName>gres1</objectName>
  <x>1082</x>
  <y>470</y>
  <width>20</width>
  <height>100</height>
  <uuid>{3019fcfd-823d-41fc-b511-208c3de54640}</uuid>
  <visible>true</visible>
  <midichan>0</midichan>
  <midicc>-3</midicc>
  <minimum>0.00000000</minimum>
  <maximum>1.00000000</maximum>
  <value>1.00000000</value>
  <mode>lin</mode>
  <mouseControl act="jump">continuous</mouseControl>
  <resolution>-1.00000000</resolution>
  <randomizable group="0">false</randomizable>
 </bsbObject>
 <bsbObject version="2" type="BSBVSlider">
  <objectName>gres5</objectName>
  <x>1287</x>
  <y>471</y>
  <width>20</width>
  <height>100</height>
  <uuid>{10a67040-5a6e-4a33-b62d-10e3653d9afd}</uuid>
  <visible>true</visible>
  <midichan>0</midichan>
  <midicc>-3</midicc>
  <minimum>0.00000000</minimum>
  <maximum>1.00000000</maximum>
  <value>1.00000000</value>
  <mode>lin</mode>
  <mouseControl act="jump">continuous</mouseControl>
  <resolution>-1.00000000</resolution>
  <randomizable group="0">false</randomizable>
 </bsbObject>
 <bsbObject version="2" type="BSBVSlider">
  <objectName>gres4</objectName>
  <x>1237</x>
  <y>471</y>
  <width>20</width>
  <height>100</height>
  <uuid>{53a33dfc-7ed0-42c3-8c62-74ed36d8ce39}</uuid>
  <visible>true</visible>
  <midichan>0</midichan>
  <midicc>-3</midicc>
  <minimum>0.00000000</minimum>
  <maximum>1.00000000</maximum>
  <value>1.00000000</value>
  <mode>lin</mode>
  <mouseControl act="jump">continuous</mouseControl>
  <resolution>-1.00000000</resolution>
  <randomizable group="0">false</randomizable>
 </bsbObject>
 <bsbObject version="2" type="BSBVSlider">
  <objectName>gres3</objectName>
  <x>1184</x>
  <y>470</y>
  <width>20</width>
  <height>100</height>
  <uuid>{8c58b242-350a-4089-b640-38716ad560d4}</uuid>
  <visible>true</visible>
  <midichan>0</midichan>
  <midicc>-3</midicc>
  <minimum>0.00000000</minimum>
  <maximum>1.00000000</maximum>
  <value>1.00000000</value>
  <mode>lin</mode>
  <mouseControl act="jump">continuous</mouseControl>
  <resolution>-1.00000000</resolution>
  <randomizable group="0">false</randomizable>
 </bsbObject>
 <bsbObject version="2" type="BSBVSlider">
  <objectName>gres2</objectName>
  <x>1134</x>
  <y>470</y>
  <width>20</width>
  <height>100</height>
  <uuid>{87b0dee6-87d1-4557-b2fe-3b316384ae5c}</uuid>
  <visible>true</visible>
  <midichan>0</midichan>
  <midicc>-3</midicc>
  <minimum>0.00000000</minimum>
  <maximum>1.00000000</maximum>
  <value>1.00000000</value>
  <mode>lin</mode>
  <mouseControl act="jump">continuous</mouseControl>
  <resolution>-1.00000000</resolution>
  <randomizable group="0">false</randomizable>
 </bsbObject>
 <bsbObject version="2" type="BSBKnob">
  <objectName>pan1</objectName>
  <x>1065</x>
  <y>431</y>
  <width>52</width>
  <height>38</height>
  <uuid>{0649d6ff-19e2-4311-925f-9c45a2ed4885}</uuid>
  <visible>true</visible>
  <midichan>0</midichan>
  <midicc>-3</midicc>
  <minimum>0.00000000</minimum>
  <maximum>1.00000000</maximum>
  <value>-0.52458966</value>
  <mode>lin</mode>
  <mouseControl act="jump">continuous</mouseControl>
  <resolution>0.01000000</resolution>
  <randomizable group="0">false</randomizable>
 </bsbObject>
 <bsbObject version="2" type="BSBKnob">
  <objectName>pan2</objectName>
  <x>1116</x>
  <y>431</y>
  <width>52</width>
  <height>38</height>
  <uuid>{5632250d-9a75-4a1f-8494-5ec947ed36d0}</uuid>
  <visible>true</visible>
  <midichan>0</midichan>
  <midicc>-3</midicc>
  <minimum>0.00000000</minimum>
  <maximum>1.00000000</maximum>
  <value>0.84155500</value>
  <mode>lin</mode>
  <mouseControl act="jump">continuous</mouseControl>
  <resolution>0.01000000</resolution>
  <randomizable group="0">false</randomizable>
 </bsbObject>
 <bsbObject version="2" type="BSBKnob">
  <objectName>pan3</objectName>
  <x>1167</x>
  <y>431</y>
  <width>52</width>
  <height>38</height>
  <uuid>{0633253c-99f0-47e7-8554-b049655bced5}</uuid>
  <visible>true</visible>
  <midichan>0</midichan>
  <midicc>-3</midicc>
  <minimum>0.00000000</minimum>
  <maximum>1.00000000</maximum>
  <value>0.52980363</value>
  <mode>lin</mode>
  <mouseControl act="jump">continuous</mouseControl>
  <resolution>0.01000000</resolution>
  <randomizable group="0">false</randomizable>
 </bsbObject>
 <bsbObject version="2" type="BSBKnob">
  <objectName>pan4</objectName>
  <x>1218</x>
  <y>431</y>
  <width>52</width>
  <height>38</height>
  <uuid>{edef8288-b403-466d-a0dd-66d8806a8ecd}</uuid>
  <visible>true</visible>
  <midichan>0</midichan>
  <midicc>-3</midicc>
  <minimum>0.00000000</minimum>
  <maximum>1.00000000</maximum>
  <value>-0.83822471</value>
  <mode>lin</mode>
  <mouseControl act="jump">continuous</mouseControl>
  <resolution>0.01000000</resolution>
  <randomizable group="0">false</randomizable>
 </bsbObject>
 <bsbObject version="2" type="BSBKnob">
  <objectName>pan5</objectName>
  <x>1270</x>
  <y>431</y>
  <width>52</width>
  <height>38</height>
  <uuid>{bd83edb8-11ef-4f88-97d0-fd0f66a132ee}</uuid>
  <visible>true</visible>
  <midichan>0</midichan>
  <midicc>-3</midicc>
  <minimum>0.00000000</minimum>
  <maximum>1.00000000</maximum>
  <value>0.50505100</value>
  <mode>lin</mode>
  <mouseControl act="jump">continuous</mouseControl>
  <resolution>0.01000000</resolution>
  <randomizable group="0">false</randomizable>
 </bsbObject>
 <bsbObject version="2" type="BSBLabel">
  <objectName/>
  <x>1045</x>
  <y>440</y>
  <width>33</width>
  <height>21</height>
  <uuid>{cb9c0b1c-8f14-4748-8e6a-1826d4ecf58c}</uuid>
  <visible>true</visible>
  <midichan>0</midichan>
  <midicc>-3</midicc>
  <label>PAN</label>
  <alignment>left</alignment>
  <font>Arial</font>
  <fontsize>12</fontsize>
  <precision>3</precision>
  <color>
   <r>204</r>
   <g>204</g>
   <b>204</b>
  </color>
  <bgcolor mode="nobackground">
   <r>255</r>
   <g>255</g>
   <b>255</b>
  </bgcolor>
  <bordermode>noborder</bordermode>
  <borderradius>1</borderradius>
  <borderwidth>1</borderwidth>
 </bsbObject>
 <bsbObject version="2" type="BSBLabel">
  <objectName/>
  <x>1049</x>
  <y>572</y>
  <width>34</width>
  <height>23</height>
  <uuid>{8c972452-b4da-4694-b8c9-988d73dac49b}</uuid>
  <visible>true</visible>
  <midichan>0</midichan>
  <midicc>-3</midicc>
  <label>OUT</label>
  <alignment>left</alignment>
  <font>Arial</font>
  <fontsize>12</fontsize>
  <precision>3</precision>
  <color>
   <r>204</r>
   <g>204</g>
   <b>204</b>
  </color>
  <bgcolor mode="nobackground">
   <r>255</r>
   <g>255</g>
   <b>255</b>
  </bgcolor>
  <bordermode>noborder</bordermode>
  <borderradius>1</borderradius>
  <borderwidth>1</borderwidth>
 </bsbObject>
 <bsbObject version="2" type="BSBHSlider">
  <objectName>outmaster</objectName>
  <x>1079</x>
  <y>572</y>
  <width>241</width>
  <height>13</height>
  <uuid>{6022ff25-f09c-4faf-9418-406e39bcb796}</uuid>
  <visible>true</visible>
  <midichan>0</midichan>
  <midicc>-3</midicc>
  <minimum>0.00000000</minimum>
  <maximum>1.00000000</maximum>
  <value>1.00000000</value>
  <mode>lin</mode>
  <mouseControl act="jump">continuous</mouseControl>
  <resolution>-1.00000000</resolution>
  <randomizable group="0">false</randomizable>
 </bsbObject>
 <bsbObject version="2" type="BSBController">
  <objectName>vert155</objectName>
  <x>1079</x>
  <y>584</y>
  <width>241</width>
  <height>11</height>
  <uuid>{890dfd41-a4ae-4487-8e6e-f4dc81da5314}</uuid>
  <visible>true</visible>
  <midichan>0</midichan>
  <midicc>-3</midicc>
  <objectName2>hor155</objectName2>
  <xMin>0.00000000</xMin>
  <xMax>1.00000000</xMax>
  <yMin>0.00000000</yMin>
  <yMax>1.00000000</yMax>
  <xValue>0.00000000</xValue>
  <yValue>0.00000000</yValue>
  <type>fill</type>
  <pointsize>1</pointsize>
  <fadeSpeed>0.00000000</fadeSpeed>
  <mouseControl act="press">jump</mouseControl>
  <color>
   <r>0</r>
   <g>234</g>
   <b>0</b>
  </color>
  <randomizable mode="both" group="0">false</randomizable>
  <bgcolor>
   <r>0</r>
   <g>0</g>
   <b>0</b>
  </bgcolor>
 </bsbObject>
 <bsbObject version="2" type="BSBHSlider">
  <objectName>Lin</objectName>
  <x>1082</x>
  <y>149</y>
  <width>241</width>
  <height>13</height>
  <uuid>{e1132961-c050-4a18-8fbe-4671670b1377}</uuid>
  <visible>true</visible>
  <midichan>0</midichan>
  <midicc>-3</midicc>
  <minimum>0.00000000</minimum>
  <maximum>1.00000000</maximum>
  <value>1.00000000</value>
  <mode>lin</mode>
  <mouseControl act="jump">continuous</mouseControl>
  <resolution>-1.00000000</resolution>
  <randomizable group="0">false</randomizable>
 </bsbObject>
 <bsbObject version="2" type="BSBController">
  <objectName>disp_rmsL</objectName>
  <x>1082</x>
  <y>161</y>
  <width>241</width>
  <height>11</height>
  <uuid>{d9230ae7-51a7-41bc-bc52-30bb39cc44ee}</uuid>
  <visible>true</visible>
  <midichan>0</midichan>
  <midicc>-3</midicc>
  <objectName2>disp_rmsL</objectName2>
  <xMin>0.00000000</xMin>
  <xMax>1.00000000</xMax>
  <yMin>0.00000000</yMin>
  <yMax>1.00000000</yMax>
  <xValue>0.00571979</xValue>
  <yValue>0.00571979</yValue>
  <type>fill</type>
  <pointsize>1</pointsize>
  <fadeSpeed>0.00000000</fadeSpeed>
  <mouseControl act="press">jump</mouseControl>
  <color>
   <r>0</r>
   <g>234</g>
   <b>0</b>
  </color>
  <randomizable mode="both" group="0">false</randomizable>
  <bgcolor>
   <r>0</r>
   <g>0</g>
   <b>0</b>
  </bgcolor>
 </bsbObject>
 <bsbObject version="2" type="BSBHSlider">
  <objectName>Rin</objectName>
  <x>1082</x>
  <y>183</y>
  <width>241</width>
  <height>13</height>
  <uuid>{434cfb2c-aada-4397-9c92-77f8752a4973}</uuid>
  <visible>true</visible>
  <midichan>0</midichan>
  <midicc>-3</midicc>
  <minimum>0.00000000</minimum>
  <maximum>1.00000000</maximum>
  <value>1.00000000</value>
  <mode>lin</mode>
  <mouseControl act="jump">continuous</mouseControl>
  <resolution>-1.00000000</resolution>
  <randomizable group="0">false</randomizable>
 </bsbObject>
 <bsbObject version="2" type="BSBController">
  <objectName>disp_rmsR</objectName>
  <x>1082</x>
  <y>172</y>
  <width>241</width>
  <height>11</height>
  <uuid>{1f41e764-23f5-43cc-8858-615383af020c}</uuid>
  <visible>true</visible>
  <midichan>0</midichan>
  <midicc>-3</midicc>
  <objectName2>disp_rmsR</objectName2>
  <xMin>0.00000000</xMin>
  <xMax>1.00000000</xMax>
  <yMin>0.00000000</yMin>
  <yMax>1.00000000</yMax>
  <xValue>0.00104146</xValue>
  <yValue>0.00104146</yValue>
  <type>fill</type>
  <pointsize>1</pointsize>
  <fadeSpeed>0.00000000</fadeSpeed>
  <mouseControl act="press">jump</mouseControl>
  <color>
   <r>0</r>
   <g>234</g>
   <b>0</b>
  </color>
  <randomizable mode="both" group="0">false</randomizable>
  <bgcolor>
   <r>0</r>
   <g>0</g>
   <b>0</b>
  </bgcolor>
 </bsbObject>
 <bsbObject version="2" type="BSBLabel">
  <objectName/>
  <x>1049</x>
  <y>149</y>
  <width>34</width>
  <height>23</height>
  <uuid>{fe324f07-0541-4db8-abb9-b287a40a2c8d}</uuid>
  <visible>true</visible>
  <midichan>0</midichan>
  <midicc>-3</midicc>
  <label>in L</label>
  <alignment>left</alignment>
  <font>Arial</font>
  <fontsize>12</fontsize>
  <precision>3</precision>
  <color>
   <r>204</r>
   <g>204</g>
   <b>204</b>
  </color>
  <bgcolor mode="nobackground">
   <r>255</r>
   <g>255</g>
   <b>255</b>
  </bgcolor>
  <bordermode>noborder</bordermode>
  <borderradius>1</borderradius>
  <borderwidth>1</borderwidth>
 </bsbObject>
 <bsbObject version="2" type="BSBLabel">
  <objectName/>
  <x>1049</x>
  <y>173</y>
  <width>34</width>
  <height>23</height>
  <uuid>{5ba044e6-76d0-4865-9796-c93fb07bb7c3}</uuid>
  <visible>true</visible>
  <midichan>0</midichan>
  <midicc>-3</midicc>
  <label>in R</label>
  <alignment>left</alignment>
  <font>Arial</font>
  <fontsize>12</fontsize>
  <precision>3</precision>
  <color>
   <r>204</r>
   <g>204</g>
   <b>204</b>
  </color>
  <bgcolor mode="nobackground">
   <r>255</r>
   <g>255</g>
   <b>255</b>
  </bgcolor>
  <bordermode>noborder</bordermode>
  <borderradius>1</borderradius>
  <borderwidth>1</borderwidth>
 </bsbObject>
 <bsbObject version="2" type="BSBButton">
  <objectName>_Browse1</objectName>
  <x>1126</x>
  <y>229</y>
  <width>48</width>
  <height>24</height>
  <uuid>{2f621461-a6e1-4955-8b4e-2a88475bce15}</uuid>
  <visible>true</visible>
  <midichan>0</midichan>
  <midicc>0</midicc>
  <type>value</type>
  <pressedValue>1.00000000</pressedValue>
  <stringvalue/>
  <text>Gr 1</text>
  <image>/</image>
  <eventLine/>
  <latch>false</latch>
  <latched>false</latched>
 </bsbObject>
 <bsbObject version="2" type="BSBLineEdit">
  <objectName>_Browse1</objectName>
  <x>1175</x>
  <y>229</y>
  <width>238</width>
  <height>23</height>
  <uuid>{e6f7501a-83d3-4aa0-975a-5cd7a8b944f3}</uuid>
  <visible>true</visible>
  <midichan>0</midichan>
  <midicc>-3</midicc>
  <label>composizione elettroacustica 2/fiume_01.wav</label>
  <alignment>left</alignment>
  <font>Lucida Grande</font>
  <fontsize>12</fontsize>
  <precision>3</precision>
  <color>
   <r>76</r>
   <g>76</g>
   <b>76</b>
  </color>
  <bgcolor mode="nobackground">
   <r>255</r>
   <g>255</g>
   <b>255</b>
  </bgcolor>
  <background>nobackground</background>
 </bsbObject>
 <bsbObject version="2" type="BSBLineEdit">
  <objectName>_Browse4</objectName>
  <x>1174</x>
  <y>346</y>
  <width>239</width>
  <height>22</height>
  <uuid>{045acc1d-556b-4a94-9010-f0b1618b3f9a}</uuid>
  <visible>true</visible>
  <midichan>0</midichan>
  <midicc>-3</midicc>
  <label>composizione elettroacustica 2/fiume_01.wav</label>
  <alignment>left</alignment>
  <font>Lucida Grande</font>
  <fontsize>12</fontsize>
  <precision>3</precision>
  <color>
   <r>76</r>
   <g>76</g>
   <b>76</b>
  </color>
  <bgcolor mode="nobackground">
   <r>255</r>
   <g>255</g>
   <b>255</b>
  </bgcolor>
  <background>nobackground</background>
 </bsbObject>
 <bsbObject version="2" type="BSBLineEdit">
  <objectName>_Browse3</objectName>
  <x>1175</x>
  <y>309</y>
  <width>239</width>
  <height>23</height>
  <uuid>{da2955b1-4101-4e62-8d26-f7dc9f5c899c}</uuid>
  <visible>true</visible>
  <midichan>0</midichan>
  <midicc>-3</midicc>
  <label>composizione elettroacustica 2/fiume_01.wav</label>
  <alignment>left</alignment>
  <font>Lucida Grande</font>
  <fontsize>12</fontsize>
  <precision>3</precision>
  <color>
   <r>76</r>
   <g>76</g>
   <b>76</b>
  </color>
  <bgcolor mode="nobackground">
   <r>255</r>
   <g>255</g>
   <b>255</b>
  </bgcolor>
  <background>nobackground</background>
 </bsbObject>
 <bsbObject version="2" type="BSBLineEdit">
  <objectName>_Browse2</objectName>
  <x>1174</x>
  <y>271</y>
  <width>240</width>
  <height>23</height>
  <uuid>{c390592e-8b12-4527-a39c-4b94f6d625fc}</uuid>
  <visible>true</visible>
  <midichan>0</midichan>
  <midicc>-3</midicc>
  <label>composizione elettroacustica 2/fiume_01.wav</label>
  <alignment>left</alignment>
  <font>Lucida Grande</font>
  <fontsize>12</fontsize>
  <precision>3</precision>
  <color>
   <r>76</r>
   <g>76</g>
   <b>76</b>
  </color>
  <bgcolor mode="nobackground">
   <r>255</r>
   <g>255</g>
   <b>255</b>
  </bgcolor>
  <background>nobackground</background>
 </bsbObject>
 <bsbObject version="2" type="BSBButton">
  <objectName/>
  <x>1041</x>
  <y>229</y>
  <width>49</width>
  <height>24</height>
  <uuid>{045efb15-09b1-4296-bec2-5ff2e57f6f44}</uuid>
  <visible>true</visible>
  <midichan>0</midichan>
  <midicc>-3</midicc>
  <type>event</type>
  <pressedValue>1.00000000</pressedValue>
  <stringvalue/>
  <text>Play</text>
  <image>/</image>
  <eventLine>i 5 0 9999</eventLine>
  <latch>false</latch>
  <latched>false</latched>
 </bsbObject>
 <bsbObject version="2" type="BSBButton">
  <objectName/>
  <x>1141</x>
  <y>123</y>
  <width>79</width>
  <height>28</height>
  <uuid>{57d17ecc-0d40-4b77-b065-34f5f6506e56}</uuid>
  <visible>true</visible>
  <midichan>0</midichan>
  <midicc>0</midicc>
  <type>event</type>
  <pressedValue>1.00000000</pressedValue>
  <stringvalue/>
  <text>LIVE IN</text>
  <image/>
  <eventLine>i3 0 -10</eventLine>
  <latch>true</latch>
  <latched>false</latched>
 </bsbObject>
 <bsbObject version="2" type="BSBLabel">
  <objectName/>
  <x>1034</x>
  <y>402</y>
  <width>389</width>
  <height>8</height>
  <uuid>{af6adf41-4f08-4da7-8281-d3c095c007ed}</uuid>
  <visible>true</visible>
  <midichan>0</midichan>
  <midicc>0</midicc>
  <label/>
  <alignment>left</alignment>
  <font>Arial</font>
  <fontsize>12</fontsize>
  <precision>3</precision>
  <color>
   <r>0</r>
   <g>0</g>
   <b>0</b>
  </color>
  <bgcolor mode="background">
   <r>204</r>
   <g>204</g>
   <b>204</b>
  </bgcolor>
  <bordermode>noborder</bordermode>
  <borderradius>1</borderradius>
  <borderwidth>1</borderwidth>
 </bsbObject>
 <bsbObject version="2" type="BSBLabel">
  <objectName/>
  <x>1035</x>
  <y>121</y>
  <width>389</width>
  <height>7</height>
  <uuid>{79387264-afac-4f83-b42b-b4f29c9ec390}</uuid>
  <visible>true</visible>
  <midichan>0</midichan>
  <midicc>0</midicc>
  <label/>
  <alignment>left</alignment>
  <font>Arial</font>
  <fontsize>12</fontsize>
  <precision>3</precision>
  <color>
   <r>0</r>
   <g>0</g>
   <b>0</b>
  </color>
  <bgcolor mode="background">
   <r>204</r>
   <g>204</g>
   <b>204</b>
  </bgcolor>
  <bordermode>noborder</bordermode>
  <borderradius>1</borderradius>
  <borderwidth>1</borderwidth>
 </bsbObject>
 <bsbObject version="2" type="BSBLabel">
  <objectName/>
  <x>875</x>
  <y>10</y>
  <width>153</width>
  <height>266</height>
  <uuid>{ffc5d886-5cfc-4e65-947d-ecc8a1489857}</uuid>
  <visible>true</visible>
  <midichan>0</midichan>
  <midicc>0</midicc>
  <label/>
  <alignment>center</alignment>
  <font>Arial Black</font>
  <fontsize>12</fontsize>
  <precision>3</precision>
  <color>
   <r>0</r>
   <g>0</g>
   <b>0</b>
  </color>
  <bgcolor mode="background">
   <r>128</r>
   <g>0</g>
   <b>0</b>
  </bgcolor>
  <bordermode>border</bordermode>
  <borderradius>10</borderradius>
  <borderwidth>5</borderwidth>
 </bsbObject>
 <bsbObject version="2" type="BSBLabel">
  <objectName/>
  <x>5</x>
  <y>10</y>
  <width>146</width>
  <height>266</height>
  <uuid>{c5851a5d-63b8-4411-812e-f4e9d3c6218d}</uuid>
  <visible>true</visible>
  <midichan>0</midichan>
  <midicc>0</midicc>
  <label/>
  <alignment>center</alignment>
  <font>Arial Black</font>
  <fontsize>12</fontsize>
  <precision>3</precision>
  <color>
   <r>0</r>
   <g>0</g>
   <b>0</b>
  </color>
  <bgcolor mode="background">
   <r>128</r>
   <g>0</g>
   <b>0</b>
  </bgcolor>
  <bordermode>border</bordermode>
  <borderradius>10</borderradius>
  <borderwidth>5</borderwidth>
 </bsbObject>
 <bsbObject version="2" type="BSBLabel">
  <objectName/>
  <x>786</x>
  <y>218</y>
  <width>39</width>
  <height>24</height>
  <uuid>{aa60408b-a17d-41d6-8654-b67a63e129fd}</uuid>
  <visible>true</visible>
  <midichan>0</midichan>
  <midicc>-3</midicc>
  <label>PHS
</label>
  <alignment>center</alignment>
  <font>Arial Black</font>
  <fontsize>12</fontsize>
  <precision>3</precision>
  <color>
   <r>204</r>
   <g>204</g>
   <b>204</b>
  </color>
  <bgcolor mode="nobackground">
   <r>255</r>
   <g>255</g>
   <b>255</b>
  </bgcolor>
  <bordermode>noborder</bordermode>
  <borderradius>1</borderradius>
  <borderwidth>1</borderwidth>
 </bsbObject>
 <bsbObject version="2" type="BSBHSlider">
  <objectName>slider62</objectName>
  <x>554</x>
  <y>220</y>
  <width>197</width>
  <height>20</height>
  <uuid>{78e06bc9-1da7-42b2-afa8-87aa5b35015d}</uuid>
  <visible>true</visible>
  <midichan>0</midichan>
  <midicc>-3</midicc>
  <minimum>0.00000000</minimum>
  <maximum>1.00000000</maximum>
  <value>0.49238579</value>
  <mode>lin</mode>
  <mouseControl act="jump">continuous</mouseControl>
  <resolution>-1.00000000</resolution>
  <randomizable group="0">false</randomizable>
 </bsbObject>
 <bsbObject version="2" type="BSBScrollNumber">
  <objectName/>
  <x>750</x>
  <y>217</y>
  <width>39</width>
  <height>26</height>
  <uuid>{155115ca-efbc-484a-a6c4-6c508f5e9ddd}</uuid>
  <visible>true</visible>
  <midichan>0</midichan>
  <midicc>-3</midicc>
  <alignment>left</alignment>
  <font>Arial</font>
  <fontsize>12</fontsize>
  <color>
   <r>128</r>
   <g>128</g>
   <b>128</b>
  </color>
  <bgcolor mode="background">
   <r>0</r>
   <g>0</g>
   <b>0</b>
  </bgcolor>
  <value>0.00000000</value>
  <resolution>0.00100000</resolution>
  <minimum>-999999999999.00000000</minimum>
  <maximum>99999999999999.00000000</maximum>
  <bordermode>noborder</bordermode>
  <borderradius>1</borderradius>
  <borderwidth>1</borderwidth>
  <randomizable group="0">false</randomizable>
  <mouseControl act=""/>
 </bsbObject>
 <bsbObject version="2" type="BSBLabel">
  <objectName/>
  <x>941</x>
  <y>488</y>
  <width>38</width>
  <height>22</height>
  <uuid>{686257ca-ead0-434d-b3ef-eb814417efc9}</uuid>
  <visible>true</visible>
  <midichan>0</midichan>
  <midicc>-3</midicc>
  <label>PHS
</label>
  <alignment>center</alignment>
  <font>Arial Black</font>
  <fontsize>12</fontsize>
  <precision>3</precision>
  <color>
   <r>204</r>
   <g>204</g>
   <b>204</b>
  </color>
  <bgcolor mode="nobackground">
   <r>255</r>
   <g>255</g>
   <b>255</b>
  </bgcolor>
  <bordermode>noborder</bordermode>
  <borderradius>1</borderradius>
  <borderwidth>1</borderwidth>
 </bsbObject>
 <bsbObject version="2" type="BSBHSlider">
  <objectName>slider62</objectName>
  <x>708</x>
  <y>490</y>
  <width>196</width>
  <height>18</height>
  <uuid>{b1380f7f-b40b-492d-a713-f2c19f680f1d}</uuid>
  <visible>true</visible>
  <midichan>0</midichan>
  <midicc>-3</midicc>
  <minimum>0.00000000</minimum>
  <maximum>1.00000000</maximum>
  <value>0.49238579</value>
  <mode>lin</mode>
  <mouseControl act="jump">continuous</mouseControl>
  <resolution>-1.00000000</resolution>
  <randomizable group="0">false</randomizable>
 </bsbObject>
 <bsbObject version="2" type="BSBScrollNumber">
  <objectName/>
  <x>904</x>
  <y>487</y>
  <width>38</width>
  <height>24</height>
  <uuid>{762f938e-275c-4cbf-8f83-4a85e2beecf3}</uuid>
  <visible>true</visible>
  <midichan>0</midichan>
  <midicc>-3</midicc>
  <alignment>left</alignment>
  <font>Arial</font>
  <fontsize>12</fontsize>
  <color>
   <r>128</r>
   <g>128</g>
   <b>128</b>
  </color>
  <bgcolor mode="background">
   <r>0</r>
   <g>0</g>
   <b>0</b>
  </bgcolor>
  <value>0.00000000</value>
  <resolution>0.00100000</resolution>
  <minimum>-999999999999.00000000</minimum>
  <maximum>99999999999999.00000000</maximum>
  <bordermode>noborder</bordermode>
  <borderradius>1</borderradius>
  <borderwidth>1</borderwidth>
  <randomizable group="0">false</randomizable>
  <mouseControl act=""/>
 </bsbObject>
 <bsbObject version="2" type="BSBLabel">
  <objectName/>
  <x>425</x>
  <y>220</y>
  <width>38</width>
  <height>22</height>
  <uuid>{f1a5d65f-ef73-4006-aa96-535416d57752}</uuid>
  <visible>true</visible>
  <midichan>0</midichan>
  <midicc>-3</midicc>
  <label>PHS
</label>
  <alignment>center</alignment>
  <font>Arial Black</font>
  <fontsize>12</fontsize>
  <precision>3</precision>
  <color>
   <r>204</r>
   <g>204</g>
   <b>204</b>
  </color>
  <bgcolor mode="nobackground">
   <r>255</r>
   <g>255</g>
   <b>255</b>
  </bgcolor>
  <bordermode>noborder</bordermode>
  <borderradius>1</borderradius>
  <borderwidth>1</borderwidth>
 </bsbObject>
 <bsbObject version="2" type="BSBHSlider">
  <objectName/>
  <x>192</x>
  <y>223</y>
  <width>196</width>
  <height>18</height>
  <uuid>{1135d0db-bd23-4538-8784-8cb6d2450d42}</uuid>
  <visible>true</visible>
  <midichan>0</midichan>
  <midicc>-3</midicc>
  <minimum>0.00000000</minimum>
  <maximum>1.00000000</maximum>
  <value>0.53871000</value>
  <mode>lin</mode>
  <mouseControl act="jump">continuous</mouseControl>
  <resolution>-1.00000000</resolution>
  <randomizable group="0">false</randomizable>
 </bsbObject>
 <bsbObject version="2" type="BSBScrollNumber">
  <objectName/>
  <x>388</x>
  <y>219</y>
  <width>38</width>
  <height>24</height>
  <uuid>{e952e03c-5ab8-42cc-8d42-5a292cc450ae}</uuid>
  <visible>true</visible>
  <midichan>0</midichan>
  <midicc>-3</midicc>
  <alignment>left</alignment>
  <font>Arial</font>
  <fontsize>12</fontsize>
  <color>
   <r>128</r>
   <g>128</g>
   <b>128</b>
  </color>
  <bgcolor mode="background">
   <r>0</r>
   <g>0</g>
   <b>0</b>
  </bgcolor>
  <value>0.00000000</value>
  <resolution>0.00100000</resolution>
  <minimum>-999999999999.00000000</minimum>
  <maximum>99999999999999.00000000</maximum>
  <bordermode>noborder</bordermode>
  <borderradius>1</borderradius>
  <borderwidth>1</borderwidth>
  <randomizable group="0">false</randomizable>
  <mouseControl act=""/>
 </bsbObject>
 <bsbObject version="2" type="BSBLabel">
  <objectName/>
  <x>278</x>
  <y>488</y>
  <width>38</width>
  <height>22</height>
  <uuid>{6f48bcae-0bd5-442a-a620-d6fc9253d52d}</uuid>
  <visible>true</visible>
  <midichan>0</midichan>
  <midicc>-3</midicc>
  <label>PHS
</label>
  <alignment>center</alignment>
  <font>Arial Black</font>
  <fontsize>12</fontsize>
  <precision>3</precision>
  <color>
   <r>204</r>
   <g>204</g>
   <b>204</b>
  </color>
  <bgcolor mode="nobackground">
   <r>255</r>
   <g>255</g>
   <b>255</b>
  </bgcolor>
  <bordermode>noborder</bordermode>
  <borderradius>1</borderradius>
  <borderwidth>1</borderwidth>
 </bsbObject>
 <bsbObject version="2" type="BSBHSlider">
  <objectName>slider62</objectName>
  <x>43</x>
  <y>491</y>
  <width>196</width>
  <height>18</height>
  <uuid>{7fff523a-72d4-4df1-b979-d7303d6450e6}</uuid>
  <visible>true</visible>
  <midichan>0</midichan>
  <midicc>-3</midicc>
  <minimum>0.00000000</minimum>
  <maximum>1.00000000</maximum>
  <value>0.49238579</value>
  <mode>lin</mode>
  <mouseControl act="jump">continuous</mouseControl>
  <resolution>-1.00000000</resolution>
  <randomizable group="0">false</randomizable>
 </bsbObject>
 <bsbObject version="2" type="BSBScrollNumber">
  <objectName/>
  <x>240</x>
  <y>488</y>
  <width>38</width>
  <height>24</height>
  <uuid>{59fe3392-560b-4ce0-baf1-74f9afb04fc5}</uuid>
  <visible>true</visible>
  <midichan>0</midichan>
  <midicc>-3</midicc>
  <alignment>left</alignment>
  <font>Arial</font>
  <fontsize>12</fontsize>
  <color>
   <r>128</r>
   <g>128</g>
   <b>128</b>
  </color>
  <bgcolor mode="background">
   <r>0</r>
   <g>0</g>
   <b>0</b>
  </bgcolor>
  <value>0.00000000</value>
  <resolution>0.00100000</resolution>
  <minimum>-999999999999.00000000</minimum>
  <maximum>99999999999999.00000000</maximum>
  <bordermode>noborder</bordermode>
  <borderradius>1</borderradius>
  <borderwidth>1</borderwidth>
  <randomizable group="0">false</randomizable>
  <mouseControl act=""/>
 </bsbObject>
 <bsbObject version="2" type="BSBLabel">
  <objectName/>
  <x>541</x>
  <y>724</y>
  <width>39</width>
  <height>24</height>
  <uuid>{24b91f51-0b14-47a3-94c7-9f615305e097}</uuid>
  <visible>true</visible>
  <midichan>0</midichan>
  <midicc>-3</midicc>
  <label>PHS
</label>
  <alignment>center</alignment>
  <font>Arial Black</font>
  <fontsize>12</fontsize>
  <precision>3</precision>
  <color>
   <r>204</r>
   <g>204</g>
   <b>204</b>
  </color>
  <bgcolor mode="nobackground">
   <r>255</r>
   <g>255</g>
   <b>255</b>
  </bgcolor>
  <bordermode>noborder</bordermode>
  <borderradius>1</borderradius>
  <borderwidth>1</borderwidth>
 </bsbObject>
 <bsbObject version="2" type="BSBHSlider">
  <objectName>slider62</objectName>
  <x>310</x>
  <y>725</y>
  <width>197</width>
  <height>20</height>
  <uuid>{442a0278-e793-453b-bec2-f3c2946fe9c5}</uuid>
  <visible>true</visible>
  <midichan>0</midichan>
  <midicc>-3</midicc>
  <minimum>0.00000000</minimum>
  <maximum>1.00000000</maximum>
  <value>0.49238579</value>
  <mode>lin</mode>
  <mouseControl act="jump">continuous</mouseControl>
  <resolution>-1.00000000</resolution>
  <randomizable group="0">false</randomizable>
 </bsbObject>
 <bsbObject version="2" type="BSBScrollNumber">
  <objectName/>
  <x>506</x>
  <y>721</y>
  <width>39</width>
  <height>26</height>
  <uuid>{4f62dd33-f4c4-44a8-b07a-1d35e6d862c9}</uuid>
  <visible>true</visible>
  <midichan>0</midichan>
  <midicc>-3</midicc>
  <alignment>left</alignment>
  <font>Arial</font>
  <fontsize>12</fontsize>
  <color>
   <r>128</r>
   <g>128</g>
   <b>128</b>
  </color>
  <bgcolor mode="background">
   <r>0</r>
   <g>0</g>
   <b>0</b>
  </bgcolor>
  <value>0.00000000</value>
  <resolution>0.00100000</resolution>
  <minimum>-999999999999.00000000</minimum>
  <maximum>99999999999999.00000000</maximum>
  <bordermode>noborder</bordermode>
  <borderradius>1</borderradius>
  <borderwidth>1</borderwidth>
  <randomizable group="0">false</randomizable>
  <mouseControl act=""/>
 </bsbObject>
 <bsbObject version="2" type="BSBLabel">
  <objectName/>
  <x>7</x>
  <y>543</y>
  <width>263</width>
  <height>294</height>
  <uuid>{66d5548c-82b9-48c4-ae6d-9b7b53f710a0}</uuid>
  <visible>true</visible>
  <midichan>0</midichan>
  <midicc>0</midicc>
  <label/>
  <alignment>left</alignment>
  <font>Arial</font>
  <fontsize>12</fontsize>
  <precision>3</precision>
  <color>
   <r>0</r>
   <g>0</g>
   <b>0</b>
  </color>
  <bgcolor mode="background">
   <r>128</r>
   <g>0</g>
   <b>0</b>
  </bgcolor>
  <bordermode>border</bordermode>
  <borderradius>10</borderradius>
  <borderwidth>5</borderwidth>
 </bsbObject>
 <bsbObject version="2" type="BSBLabel">
  <objectName/>
  <x>363</x>
  <y>275</y>
  <width>305</width>
  <height>239</height>
  <uuid>{f6ae2566-71a2-4275-b78c-1c0d61168b9e}</uuid>
  <visible>true</visible>
  <midichan>0</midichan>
  <midicc>0</midicc>
  <label/>
  <alignment>left</alignment>
  <font>Arial</font>
  <fontsize>12</fontsize>
  <precision>3</precision>
  <color>
   <r>0</r>
   <g>0</g>
   <b>0</b>
  </color>
  <bgcolor mode="background">
   <r>128</r>
   <g>0</g>
   <b>0</b>
  </bgcolor>
  <bordermode>border</bordermode>
  <borderradius>5</borderradius>
  <borderwidth>5</borderwidth>
 </bsbObject>
 <bsbObject version="2" type="BSBController">
  <objectName>y</objectName>
  <x>402</x>
  <y>282</y>
  <width>225</width>
  <height>225</height>
  <uuid>{68e90f27-86e2-4646-b6b1-df657fb74ae1}</uuid>
  <visible>true</visible>
  <midichan>0</midichan>
  <midicc>0</midicc>
  <objectName2>x</objectName2>
  <xMin>0.00000000</xMin>
  <xMax>1.00000000</xMax>
  <yMin>0.00000000</yMin>
  <yMax>1.00000000</yMax>
  <xValue>0.09862888</xValue>
  <yValue>0.97869372</yValue>
  <type>point</type>
  <pointsize>15</pointsize>
  <fadeSpeed>0.00000000</fadeSpeed>
  <mouseControl act="press">jump</mouseControl>
  <color>
   <r>179</r>
   <g>179</g>
   <b>179</b>
  </color>
  <randomizable mode="both" group="0">false</randomizable>
  <bgcolor>
   <r>0</r>
   <g>0</g>
   <b>0</b>
  </bgcolor>
 </bsbObject>
 <bsbObject version="2" type="BSBController">
  <objectName>y_L</objectName>
  <x>15</x>
  <y>550</y>
  <width>223</width>
  <height>223</height>
  <uuid>{b9b1c550-3ec4-41ca-bd2a-5190f34461ea}</uuid>
  <visible>true</visible>
  <midichan>0</midichan>
  <midicc>-3</midicc>
  <objectName2>x_L</objectName2>
  <xMin>0.00000000</xMin>
  <xMax>1.00000000</xMax>
  <yMin>0.00000000</yMin>
  <yMax>1.00000000</yMax>
  <xValue>0.59192825</xValue>
  <yValue>0.56502242</yValue>
  <type>point</type>
  <pointsize>15</pointsize>
  <fadeSpeed>0.00000000</fadeSpeed>
  <mouseControl act="press">jump</mouseControl>
  <color>
   <r>76</r>
   <g>76</g>
   <b>76</b>
  </color>
  <randomizable mode="both" group="0">false</randomizable>
  <bgcolor>
   <r>0</r>
   <g>0</g>
   <b>0</b>
  </bgcolor>
 </bsbObject>
 <bsbObject version="2" type="BSBLabel">
  <objectName/>
  <x>905</x>
  <y>63</y>
  <width>41</width>
  <height>22</height>
  <uuid>{060c427e-409b-416a-a547-7088c1a8858f}</uuid>
  <visible>true</visible>
  <midichan>0</midichan>
  <midicc>0</midicc>
  <label>DENS</label>
  <alignment>center</alignment>
  <font>Arial Black</font>
  <fontsize>12</fontsize>
  <precision>3</precision>
  <color>
   <r>0</r>
   <g>0</g>
   <b>0</b>
  </color>
  <bgcolor mode="nobackground">
   <r>255</r>
   <g>255</g>
   <b>255</b>
  </bgcolor>
  <bordermode>noborder</bordermode>
  <borderradius>1</borderradius>
  <borderwidth>1</borderwidth>
 </bsbObject>
 <bsbObject version="2" type="BSBLabel">
  <objectName/>
  <x>960</x>
  <y>63</y>
  <width>41</width>
  <height>22</height>
  <uuid>{41517618-3fe5-427a-b6ec-fc2235ccba6e}</uuid>
  <visible>true</visible>
  <midichan>0</midichan>
  <midicc>-3</midicc>
  <label>DUR</label>
  <alignment>center</alignment>
  <font>Arial Black</font>
  <fontsize>12</fontsize>
  <precision>3</precision>
  <color>
   <r>0</r>
   <g>0</g>
   <b>0</b>
  </color>
  <bgcolor mode="nobackground">
   <r>255</r>
   <g>255</g>
   <b>255</b>
  </bgcolor>
  <bordermode>noborder</bordermode>
  <borderradius>1</borderradius>
  <borderwidth>1</borderwidth>
 </bsbObject>
 <bsbObject version="2" type="BSBLabel">
  <objectName/>
  <x>903</x>
  <y>166</y>
  <width>41</width>
  <height>22</height>
  <uuid>{0bc99fff-040b-4912-99cc-d395a01d19b3}</uuid>
  <visible>true</visible>
  <midichan>0</midichan>
  <midicc>-3</midicc>
  <label>CPS</label>
  <alignment>center</alignment>
  <font>Arial Black</font>
  <fontsize>12</fontsize>
  <precision>3</precision>
  <color>
   <r>0</r>
   <g>0</g>
   <b>0</b>
  </color>
  <bgcolor mode="nobackground">
   <r>255</r>
   <g>255</g>
   <b>255</b>
  </bgcolor>
  <bordermode>noborder</bordermode>
  <borderradius>1</borderradius>
  <borderwidth>1</borderwidth>
 </bsbObject>
 <bsbObject version="2" type="BSBLabel">
  <objectName/>
  <x>949</x>
  <y>166</y>
  <width>68</width>
  <height>24</height>
  <uuid>{19c84525-e1d1-4e52-903c-474d9353c6f7}</uuid>
  <visible>true</visible>
  <midichan>0</midichan>
  <midicc>-3</midicc>
  <label>OVERLAPS</label>
  <alignment>center</alignment>
  <font>Arial Black</font>
  <fontsize>12</fontsize>
  <precision>3</precision>
  <color>
   <r>0</r>
   <g>0</g>
   <b>0</b>
  </color>
  <bgcolor mode="nobackground">
   <r>255</r>
   <g>255</g>
   <b>255</b>
  </bgcolor>
  <bordermode>noborder</bordermode>
  <borderradius>1</borderradius>
  <borderwidth>1</borderwidth>
 </bsbObject>
 <bsbObject version="2" type="BSBVSlider">
  <objectName>densrand2</objectName>
  <x>914</x>
  <y>96</y>
  <width>20</width>
  <height>66</height>
  <uuid>{78bc9879-b77c-4fda-8b4b-156f2ca98545}</uuid>
  <visible>true</visible>
  <midichan>0</midichan>
  <midicc>-3</midicc>
  <minimum>0.00000000</minimum>
  <maximum>1.00000000</maximum>
  <value>1.00000000</value>
  <mode>lin</mode>
  <mouseControl act="jump">continuous</mouseControl>
  <resolution>-1.00000000</resolution>
  <randomizable group="0">false</randomizable>
 </bsbObject>
 <bsbObject version="2" type="BSBVSlider">
  <objectName>durrand2</objectName>
  <x>972</x>
  <y>96</y>
  <width>20</width>
  <height>66</height>
  <uuid>{48af4a55-e117-4673-9aeb-0395df991e79}</uuid>
  <visible>true</visible>
  <midichan>0</midichan>
  <midicc>-3</midicc>
  <minimum>0.00000000</minimum>
  <maximum>1.00000000</maximum>
  <value>1.00000000</value>
  <mode>lin</mode>
  <mouseControl act="jump">continuous</mouseControl>
  <resolution>-1.00000000</resolution>
  <randomizable group="0">false</randomizable>
 </bsbObject>
 <bsbObject version="2" type="BSBVSlider">
  <objectName>cpsrand2</objectName>
  <x>915</x>
  <y>199</y>
  <width>20</width>
  <height>66</height>
  <uuid>{04e9928f-b11e-4826-a86a-ad15d8cb7920}</uuid>
  <visible>true</visible>
  <midichan>0</midichan>
  <midicc>-3</midicc>
  <minimum>0.00000000</minimum>
  <maximum>1.00000000</maximum>
  <value>1.00000000</value>
  <mode>lin</mode>
  <mouseControl act="jump">continuous</mouseControl>
  <resolution>-1.00000000</resolution>
  <randomizable group="0">false</randomizable>
 </bsbObject>
 <bsbObject version="2" type="BSBVSlider">
  <objectName>overlaps2</objectName>
  <x>972</x>
  <y>199</y>
  <width>20</width>
  <height>66</height>
  <uuid>{bd27299c-a11f-41b5-9f88-8cc8b4c9467f}</uuid>
  <visible>true</visible>
  <midichan>0</midichan>
  <midicc>-3</midicc>
  <minimum>0.00000000</minimum>
  <maximum>1.00000000</maximum>
  <value>1.00000000</value>
  <mode>lin</mode>
  <mouseControl act="jump">continuous</mouseControl>
  <resolution>-1.00000000</resolution>
  <randomizable group="0">false</randomizable>
 </bsbObject>
 <bsbObject version="2" type="BSBScrollNumber">
  <objectName/>
  <x>908</x>
  <y>80</y>
  <width>36</width>
  <height>17</height>
  <uuid>{57618193-3156-4713-a361-b8bdb2c2be81}</uuid>
  <visible>true</visible>
  <midichan>0</midichan>
  <midicc>-3</midicc>
  <alignment>left</alignment>
  <font>Arial</font>
  <fontsize>12</fontsize>
  <color>
   <r>128</r>
   <g>128</g>
   <b>128</b>
  </color>
  <bgcolor mode="background">
   <r>0</r>
   <g>0</g>
   <b>0</b>
  </bgcolor>
  <value>0.00000000</value>
  <resolution>0.00100000</resolution>
  <minimum>-999999999999.00000000</minimum>
  <maximum>99999999999999.00000000</maximum>
  <bordermode>noborder</bordermode>
  <borderradius>1</borderradius>
  <borderwidth>1</borderwidth>
  <randomizable group="0">false</randomizable>
  <mouseControl act=""/>
 </bsbObject>
 <bsbObject version="2" type="BSBScrollNumber">
  <objectName/>
  <x>963</x>
  <y>80</y>
  <width>36</width>
  <height>17</height>
  <uuid>{a61d992d-6b1c-4d18-83ed-147bd6b88666}</uuid>
  <visible>true</visible>
  <midichan>0</midichan>
  <midicc>-3</midicc>
  <alignment>left</alignment>
  <font>Arial</font>
  <fontsize>12</fontsize>
  <color>
   <r>128</r>
   <g>128</g>
   <b>128</b>
  </color>
  <bgcolor mode="background">
   <r>0</r>
   <g>0</g>
   <b>0</b>
  </bgcolor>
  <value>0.00000000</value>
  <resolution>0.00100000</resolution>
  <minimum>-999999999999.00000000</minimum>
  <maximum>99999999999999.00000000</maximum>
  <bordermode>noborder</bordermode>
  <borderradius>1</borderradius>
  <borderwidth>1</borderwidth>
  <randomizable group="0">false</randomizable>
  <mouseControl act=""/>
 </bsbObject>
 <bsbObject version="2" type="BSBScrollNumber">
  <objectName/>
  <x>907</x>
  <y>182</y>
  <width>36</width>
  <height>17</height>
  <uuid>{98e1a67e-8133-46f6-8490-55b48be33277}</uuid>
  <visible>true</visible>
  <midichan>0</midichan>
  <midicc>-3</midicc>
  <alignment>left</alignment>
  <font>Arial</font>
  <fontsize>12</fontsize>
  <color>
   <r>128</r>
   <g>128</g>
   <b>128</b>
  </color>
  <bgcolor mode="background">
   <r>0</r>
   <g>0</g>
   <b>0</b>
  </bgcolor>
  <value>0.00000000</value>
  <resolution>0.00100000</resolution>
  <minimum>-999999999999.00000000</minimum>
  <maximum>99999999999999.00000000</maximum>
  <bordermode>noborder</bordermode>
  <borderradius>1</borderradius>
  <borderwidth>1</borderwidth>
  <randomizable group="0">false</randomizable>
  <mouseControl act=""/>
 </bsbObject>
 <bsbObject version="2" type="BSBScrollNumber">
  <objectName/>
  <x>965</x>
  <y>182</y>
  <width>36</width>
  <height>17</height>
  <uuid>{7257c085-0ff6-4c78-8fa9-52772522c01c}</uuid>
  <visible>true</visible>
  <midichan>0</midichan>
  <midicc>-3</midicc>
  <alignment>left</alignment>
  <font>Arial</font>
  <fontsize>12</fontsize>
  <color>
   <r>128</r>
   <g>128</g>
   <b>128</b>
  </color>
  <bgcolor mode="background">
   <r>0</r>
   <g>0</g>
   <b>0</b>
  </bgcolor>
  <value>0.00000000</value>
  <resolution>0.00100000</resolution>
  <minimum>-999999999999.00000000</minimum>
  <maximum>99999999999999.00000000</maximum>
  <bordermode>noborder</bordermode>
  <borderradius>1</borderradius>
  <borderwidth>1</borderwidth>
  <randomizable group="0">false</randomizable>
  <mouseControl act=""/>
 </bsbObject>
 <bsbObject version="2" type="BSBLabel">
  <objectName/>
  <x>25</x>
  <y>59</y>
  <width>41</width>
  <height>22</height>
  <uuid>{3b6e403b-fb92-46a6-8250-e653a2b67254}</uuid>
  <visible>true</visible>
  <midichan>0</midichan>
  <midicc>-3</midicc>
  <label>DENS</label>
  <alignment>center</alignment>
  <font>Arial Black</font>
  <fontsize>12</fontsize>
  <precision>3</precision>
  <color>
   <r>0</r>
   <g>0</g>
   <b>0</b>
  </color>
  <bgcolor mode="nobackground">
   <r>255</r>
   <g>255</g>
   <b>255</b>
  </bgcolor>
  <bordermode>noborder</bordermode>
  <borderradius>1</borderradius>
  <borderwidth>1</borderwidth>
 </bsbObject>
 <bsbObject version="2" type="BSBLabel">
  <objectName/>
  <x>80</x>
  <y>59</y>
  <width>41</width>
  <height>22</height>
  <uuid>{46c53894-b8e4-4339-9552-124e43157b56}</uuid>
  <visible>true</visible>
  <midichan>0</midichan>
  <midicc>-3</midicc>
  <label>DUR</label>
  <alignment>center</alignment>
  <font>Arial Black</font>
  <fontsize>12</fontsize>
  <precision>3</precision>
  <color>
   <r>0</r>
   <g>0</g>
   <b>0</b>
  </color>
  <bgcolor mode="nobackground">
   <r>255</r>
   <g>255</g>
   <b>255</b>
  </bgcolor>
  <bordermode>noborder</bordermode>
  <borderradius>1</borderradius>
  <borderwidth>1</borderwidth>
 </bsbObject>
 <bsbObject version="2" type="BSBLabel">
  <objectName/>
  <x>22</x>
  <y>165</y>
  <width>41</width>
  <height>22</height>
  <uuid>{2b1a7b39-2a35-4f39-87af-1b04b7e482e3}</uuid>
  <visible>true</visible>
  <midichan>0</midichan>
  <midicc>-3</midicc>
  <label>CPS</label>
  <alignment>center</alignment>
  <font>Arial Black</font>
  <fontsize>12</fontsize>
  <precision>3</precision>
  <color>
   <r>0</r>
   <g>0</g>
   <b>0</b>
  </color>
  <bgcolor mode="nobackground">
   <r>255</r>
   <g>255</g>
   <b>255</b>
  </bgcolor>
  <bordermode>noborder</bordermode>
  <borderradius>1</borderradius>
  <borderwidth>1</borderwidth>
 </bsbObject>
 <bsbObject version="2" type="BSBLabel">
  <objectName/>
  <x>68</x>
  <y>165</y>
  <width>68</width>
  <height>24</height>
  <uuid>{afdb2f16-e866-4264-bada-30ed2f5e2a2b}</uuid>
  <visible>true</visible>
  <midichan>0</midichan>
  <midicc>-3</midicc>
  <label>OVERLAPS</label>
  <alignment>center</alignment>
  <font>Arial Black</font>
  <fontsize>12</fontsize>
  <precision>3</precision>
  <color>
   <r>0</r>
   <g>0</g>
   <b>0</b>
  </color>
  <bgcolor mode="nobackground">
   <r>255</r>
   <g>255</g>
   <b>255</b>
  </bgcolor>
  <bordermode>noborder</bordermode>
  <borderradius>1</borderradius>
  <borderwidth>1</borderwidth>
 </bsbObject>
 <bsbObject version="2" type="BSBVSlider">
  <objectName>densrand1</objectName>
  <x>34</x>
  <y>93</y>
  <width>20</width>
  <height>71</height>
  <uuid>{d34b6355-078f-4654-aec3-abfc14ceb1f8}</uuid>
  <visible>true</visible>
  <midichan>0</midichan>
  <midicc>-3</midicc>
  <minimum>0.00000000</minimum>
  <maximum>1.00000000</maximum>
  <value>0.00000000</value>
  <mode>lin</mode>
  <mouseControl act="jump">continuous</mouseControl>
  <resolution>-1.00000000</resolution>
  <randomizable group="0">false</randomizable>
 </bsbObject>
 <bsbObject version="2" type="BSBVSlider">
  <objectName>durrand1</objectName>
  <x>92</x>
  <y>92</y>
  <width>20</width>
  <height>71</height>
  <uuid>{6cdf3d73-f009-47b6-b941-53d6d4997ce2}</uuid>
  <visible>true</visible>
  <midichan>0</midichan>
  <midicc>-3</midicc>
  <minimum>0.00000000</minimum>
  <maximum>1.00000000</maximum>
  <value>0.00000000</value>
  <mode>lin</mode>
  <mouseControl act="jump">continuous</mouseControl>
  <resolution>-1.00000000</resolution>
  <randomizable group="0">false</randomizable>
 </bsbObject>
 <bsbObject version="2" type="BSBVSlider">
  <objectName>cpsrand1</objectName>
  <x>34</x>
  <y>198</y>
  <width>20</width>
  <height>71</height>
  <uuid>{426fd047-99d6-4544-bc9e-bf054e06892e}</uuid>
  <visible>true</visible>
  <midichan>0</midichan>
  <midicc>-3</midicc>
  <minimum>0.00000000</minimum>
  <maximum>1.00000000</maximum>
  <value>0.00000000</value>
  <mode>lin</mode>
  <mouseControl act="jump">continuous</mouseControl>
  <resolution>-1.00000000</resolution>
  <randomizable group="0">false</randomizable>
 </bsbObject>
 <bsbObject version="2" type="BSBVSlider">
  <objectName>overlapsrand1</objectName>
  <x>91</x>
  <y>198</y>
  <width>20</width>
  <height>71</height>
  <uuid>{2fe0269c-31d3-42ed-b322-c79a67548af4}</uuid>
  <visible>true</visible>
  <midichan>0</midichan>
  <midicc>-3</midicc>
  <minimum>0.00000000</minimum>
  <maximum>1.00000000</maximum>
  <value>0.62921300</value>
  <mode>lin</mode>
  <mouseControl act="jump">continuous</mouseControl>
  <resolution>-1.00000000</resolution>
  <randomizable group="0">false</randomizable>
 </bsbObject>
 <bsbObject version="2" type="BSBScrollNumber">
  <objectName/>
  <x>28</x>
  <y>76</y>
  <width>36</width>
  <height>17</height>
  <uuid>{f3ce0f7a-9aa8-4a22-9aee-d6167bd67767}</uuid>
  <visible>true</visible>
  <midichan>0</midichan>
  <midicc>-3</midicc>
  <alignment>left</alignment>
  <font>Arial</font>
  <fontsize>12</fontsize>
  <color>
   <r>128</r>
   <g>128</g>
   <b>128</b>
  </color>
  <bgcolor mode="background">
   <r>0</r>
   <g>0</g>
   <b>0</b>
  </bgcolor>
  <value>0.00000000</value>
  <resolution>0.00100000</resolution>
  <minimum>-999999999999.00000000</minimum>
  <maximum>99999999999999.00000000</maximum>
  <bordermode>noborder</bordermode>
  <borderradius>1</borderradius>
  <borderwidth>1</borderwidth>
  <randomizable group="0">false</randomizable>
  <mouseControl act=""/>
 </bsbObject>
 <bsbObject version="2" type="BSBScrollNumber">
  <objectName/>
  <x>83</x>
  <y>76</y>
  <width>36</width>
  <height>17</height>
  <uuid>{47be4f24-424b-4061-aa6d-e7f29ec9b5d3}</uuid>
  <visible>true</visible>
  <midichan>0</midichan>
  <midicc>-3</midicc>
  <alignment>left</alignment>
  <font>Arial</font>
  <fontsize>12</fontsize>
  <color>
   <r>128</r>
   <g>128</g>
   <b>128</b>
  </color>
  <bgcolor mode="background">
   <r>0</r>
   <g>0</g>
   <b>0</b>
  </bgcolor>
  <value>0.00000000</value>
  <resolution>0.00100000</resolution>
  <minimum>-999999999999.00000000</minimum>
  <maximum>99999999999999.00000000</maximum>
  <bordermode>noborder</bordermode>
  <borderradius>1</borderradius>
  <borderwidth>1</borderwidth>
  <randomizable group="0">false</randomizable>
  <mouseControl act=""/>
 </bsbObject>
 <bsbObject version="2" type="BSBScrollNumber">
  <objectName/>
  <x>26</x>
  <y>181</y>
  <width>36</width>
  <height>17</height>
  <uuid>{738306f7-f4fb-469f-b161-4a6efe859e99}</uuid>
  <visible>true</visible>
  <midichan>0</midichan>
  <midicc>-3</midicc>
  <alignment>left</alignment>
  <font>Arial</font>
  <fontsize>12</fontsize>
  <color>
   <r>128</r>
   <g>128</g>
   <b>128</b>
  </color>
  <bgcolor mode="background">
   <r>0</r>
   <g>0</g>
   <b>0</b>
  </bgcolor>
  <value>0.00000000</value>
  <resolution>0.00100000</resolution>
  <minimum>-999999999999.00000000</minimum>
  <maximum>99999999999999.00000000</maximum>
  <bordermode>noborder</bordermode>
  <borderradius>1</borderradius>
  <borderwidth>1</borderwidth>
  <randomizable group="0">false</randomizable>
  <mouseControl act=""/>
 </bsbObject>
 <bsbObject version="2" type="BSBScrollNumber">
  <objectName/>
  <x>84</x>
  <y>181</y>
  <width>36</width>
  <height>17</height>
  <uuid>{ba9d522f-a012-4960-b079-70afa639993f}</uuid>
  <visible>true</visible>
  <midichan>0</midichan>
  <midicc>-3</midicc>
  <alignment>left</alignment>
  <font>Arial</font>
  <fontsize>12</fontsize>
  <color>
   <r>128</r>
   <g>128</g>
   <b>128</b>
  </color>
  <bgcolor mode="background">
   <r>0</r>
   <g>0</g>
   <b>0</b>
  </bgcolor>
  <value>0.00000000</value>
  <resolution>0.00100000</resolution>
  <minimum>-999999999999.00000000</minimum>
  <maximum>99999999999999.00000000</maximum>
  <bordermode>noborder</bordermode>
  <borderradius>1</borderradius>
  <borderwidth>1</borderwidth>
  <randomizable group="0">false</randomizable>
  <mouseControl act=""/>
 </bsbObject>
 <bsbObject version="2" type="BSBDropdown">
  <objectName>selL</objectName>
  <x>148</x>
  <y>773</y>
  <width>101</width>
  <height>24</height>
  <uuid>{54a39f7b-98d8-4d19-aa5b-0b2aa4e707bd}</uuid>
  <visible>true</visible>
  <midichan>0</midichan>
  <midicc>-3</midicc>
  <bsbDropdownItemList>
   <bsbDropdownItem>
    <name>GRAN 1</name>
    <value>0</value>
    <stringvalue/>
   </bsbDropdownItem>
   <bsbDropdownItem>
    <name>GRAN 2</name>
    <value>1</value>
    <stringvalue/>
   </bsbDropdownItem>
   <bsbDropdownItem>
    <name>GRAN 3</name>
    <value>2</value>
    <stringvalue/>
   </bsbDropdownItem>
   <bsbDropdownItem>
    <name>GRAN 4</name>
    <value>3</value>
    <stringvalue/>
   </bsbDropdownItem>
   <bsbDropdownItem>
    <name>GRAN5</name>
    <value>4</value>
    <stringvalue/>
   </bsbDropdownItem>
  </bsbDropdownItemList>
  <selectedIndex>4</selectedIndex>
  <randomizable group="0">false</randomizable>
 </bsbObject>
 <bsbObject version="2" type="BSBDropdown">
  <objectName>selLL</objectName>
  <x>111</x>
  <y>806</y>
  <width>101</width>
  <height>24</height>
  <uuid>{ef6ff187-a951-490d-9cc9-1a55421d1fcc}</uuid>
  <visible>true</visible>
  <midichan>0</midichan>
  <midicc>-3</midicc>
  <bsbDropdownItemList>
   <bsbDropdownItem>
    <name>DENS/DUR</name>
    <value>0</value>
    <stringvalue/>
   </bsbDropdownItem>
   <bsbDropdownItem>
    <name>DENS/CPS</name>
    <value>1</value>
    <stringvalue/>
   </bsbDropdownItem>
   <bsbDropdownItem>
    <name>DENS/VEL</name>
    <value>2</value>
    <stringvalue/>
   </bsbDropdownItem>
   <bsbDropdownItem>
    <name>DUR/CPS</name>
    <value>3</value>
    <stringvalue/>
   </bsbDropdownItem>
   <bsbDropdownItem>
    <name>DUR/VEL</name>
    <value>4</value>
    <stringvalue/>
   </bsbDropdownItem>
   <bsbDropdownItem>
    <name>FMD/PMD</name>
    <value>5</value>
    <stringvalue/>
   </bsbDropdownItem>
   <bsbDropdownItem>
    <name>FRPOW/PRPOW</name>
    <value>6</value>
    <stringvalue/>
   </bsbDropdownItem>
  </bsbDropdownItemList>
  <selectedIndex>0</selectedIndex>
  <randomizable group="0">false</randomizable>
 </bsbObject>
 <bsbObject version="2" type="BSBDropdown">
  <objectName>kfn2</objectName>
  <x>617</x>
  <y>242</y>
  <width>105</width>
  <height>25</height>
  <uuid>{33f90032-1c05-48b2-ba4c-3f95c0769daa}</uuid>
  <visible>true</visible>
  <midichan>0</midichan>
  <midicc>-3</midicc>
  <bsbDropdownItemList>
   <bsbDropdownItem>
    <name>GAUSS</name>
    <value>0</value>
    <stringvalue/>
   </bsbDropdownItem>
   <bsbDropdownItem>
    <name>TRAPEZOID</name>
    <value>1</value>
    <stringvalue/>
   </bsbDropdownItem>
  </bsbDropdownItemList>
  <selectedIndex>0</selectedIndex>
  <randomizable group="0">false</randomizable>
 </bsbObject>
 <bsbObject version="2" type="BSBDropdown">
  <objectName>kfn1</objectName>
  <x>261</x>
  <y>245</y>
  <width>104</width>
  <height>23</height>
  <uuid>{6e123f46-fb9c-493f-95b7-0cc2539f1628}</uuid>
  <visible>true</visible>
  <midichan>0</midichan>
  <midicc>-3</midicc>
  <bsbDropdownItemList>
   <bsbDropdownItem>
    <name>GAUSS</name>
    <value>0</value>
    <stringvalue/>
   </bsbDropdownItem>
   <bsbDropdownItem>
    <name>TRAPEZOID</name>
    <value>1</value>
    <stringvalue/>
   </bsbDropdownItem>
  </bsbDropdownItemList>
  <selectedIndex>0</selectedIndex>
  <randomizable group="0">false</randomizable>
 </bsbObject>
 <bsbObject version="2" type="BSBDropdown">
  <objectName>kfn3</objectName>
  <x>99</x>
  <y>513</y>
  <width>104</width>
  <height>23</height>
  <uuid>{42ab7534-ed85-43e0-ab9e-efb400ee8114}</uuid>
  <visible>true</visible>
  <midichan>0</midichan>
  <midicc>-3</midicc>
  <bsbDropdownItemList>
   <bsbDropdownItem>
    <name>GAUSS</name>
    <value>0</value>
    <stringvalue/>
   </bsbDropdownItem>
   <bsbDropdownItem>
    <name>TRAPEZOID</name>
    <value>1</value>
    <stringvalue/>
   </bsbDropdownItem>
  </bsbDropdownItemList>
  <selectedIndex>0</selectedIndex>
  <randomizable group="0">false</randomizable>
 </bsbObject>
 <bsbObject version="2" type="BSBDropdown">
  <objectName>kfn4</objectName>
  <x>775</x>
  <y>512</y>
  <width>104</width>
  <height>23</height>
  <uuid>{aa039c4b-ec41-4592-ada8-f7bdf521345e}</uuid>
  <visible>true</visible>
  <midichan>0</midichan>
  <midicc>-3</midicc>
  <bsbDropdownItemList>
   <bsbDropdownItem>
    <name>GAUSS</name>
    <value>0</value>
    <stringvalue/>
   </bsbDropdownItem>
   <bsbDropdownItem>
    <name>TRAPEZOID</name>
    <value>1</value>
    <stringvalue/>
   </bsbDropdownItem>
  </bsbDropdownItemList>
  <selectedIndex>0</selectedIndex>
  <randomizable group="0">false</randomizable>
 </bsbObject>
 <bsbObject version="2" type="BSBDropdown">
  <objectName>kfn5</objectName>
  <x>321</x>
  <y>747</y>
  <width>105</width>
  <height>25</height>
  <uuid>{edb05029-02c3-4cdf-b938-37a286e507bb}</uuid>
  <visible>true</visible>
  <midichan>0</midichan>
  <midicc>-3</midicc>
  <bsbDropdownItemList>
   <bsbDropdownItem>
    <name>GAUSS</name>
    <value>0</value>
    <stringvalue/>
   </bsbDropdownItem>
   <bsbDropdownItem>
    <name>TRAPEZOID</name>
    <value>1</value>
    <stringvalue/>
   </bsbDropdownItem>
  </bsbDropdownItemList>
  <selectedIndex>0</selectedIndex>
  <randomizable group="0">false</randomizable>
 </bsbObject>
 <bsbObject version="2" type="BSBButton">
  <objectName>freeze2</objectName>
  <x>726</x>
  <y>242</y>
  <width>77</width>
  <height>25</height>
  <uuid>{cce4b589-fb50-4afb-8a1c-fb6be65fd8d5}</uuid>
  <visible>true</visible>
  <midichan>0</midichan>
  <midicc>0</midicc>
  <type>value</type>
  <pressedValue>1.00000000</pressedValue>
  <stringvalue/>
  <text>FREEZE</text>
  <image>/</image>
  <eventLine>i1 0 10</eventLine>
  <latch>true</latch>
  <latched>false</latched>
 </bsbObject>
 <bsbObject version="2" type="BSBButton">
  <objectName>freeze4</objectName>
  <x>885</x>
  <y>511</y>
  <width>76</width>
  <height>23</height>
  <uuid>{730e3013-a2e4-4350-b399-29dd7238138f}</uuid>
  <visible>true</visible>
  <midichan>0</midichan>
  <midicc>0</midicc>
  <type>value</type>
  <pressedValue>1.00000000</pressedValue>
  <stringvalue/>
  <text>FREEZE</text>
  <image>/</image>
  <eventLine>i1 0 10</eventLine>
  <latch>true</latch>
  <latched>false</latched>
 </bsbObject>
 <bsbObject version="2" type="BSBButton">
  <objectName>freeze1</objectName>
  <x>372</x>
  <y>245</y>
  <width>76</width>
  <height>23</height>
  <uuid>{d8dec684-00ae-45a9-a6f1-c80a98e64376}</uuid>
  <visible>true</visible>
  <midichan>0</midichan>
  <midicc>0</midicc>
  <type>value</type>
  <pressedValue>1.00000000</pressedValue>
  <stringvalue/>
  <text>FREEZE</text>
  <image>/</image>
  <eventLine>i1 0 10</eventLine>
  <latch>true</latch>
  <latched>false</latched>
 </bsbObject>
 <bsbObject version="2" type="BSBButton">
  <objectName>freeze5</objectName>
  <x>482</x>
  <y>747</y>
  <width>98</width>
  <height>26</height>
  <uuid>{2a6c4dd8-fce4-4da4-9267-f1f17460bbe0}</uuid>
  <visible>true</visible>
  <midichan>0</midichan>
  <midicc>0</midicc>
  <type>value</type>
  <pressedValue>1.00000000</pressedValue>
  <stringvalue/>
  <text>FREEZE</text>
  <image>/</image>
  <eventLine>i1 0 10</eventLine>
  <latch>true</latch>
  <latched>false</latched>
 </bsbObject>
 <bsbObject version="2" type="BSBButton">
  <objectName>freeze3</objectName>
  <x>219</x>
  <y>513</y>
  <width>76</width>
  <height>23</height>
  <uuid>{b53681b0-b75d-4b40-a4f9-9716aec2bc11}</uuid>
  <visible>true</visible>
  <midichan>0</midichan>
  <midicc>0</midicc>
  <type>value</type>
  <pressedValue>1.00000000</pressedValue>
  <stringvalue/>
  <text>FREEZE</text>
  <image>/</image>
  <eventLine>i1 0 10</eventLine>
  <latch>true</latch>
  <latched>false</latched>
 </bsbObject>
 <bsbObject version="2" type="BSBHSlider">
  <objectName>file1</objectName>
  <x>1040</x>
  <y>255</y>
  <width>374</width>
  <height>17</height>
  <uuid>{7d33b36d-f760-42cb-857d-ddf287d6d1e1}</uuid>
  <visible>true</visible>
  <midichan>0</midichan>
  <midicc>-3</midicc>
  <minimum>0.00000000</minimum>
  <maximum>1.00000000</maximum>
  <value>1.00000000</value>
  <mode>lin</mode>
  <mouseControl act="jump">continuous</mouseControl>
  <resolution>-1.00000000</resolution>
  <randomizable group="0">false</randomizable>
 </bsbObject>
 <bsbObject version="2" type="BSBHSlider">
  <objectName>file2</objectName>
  <x>1040</x>
  <y>293</y>
  <width>373</width>
  <height>16</height>
  <uuid>{7e1a9ff8-02d4-4327-b110-623e219ee8f1}</uuid>
  <visible>true</visible>
  <midichan>0</midichan>
  <midicc>-3</midicc>
  <minimum>0.00000000</minimum>
  <maximum>1.00000000</maximum>
  <value>1.00000000</value>
  <mode>lin</mode>
  <mouseControl act="jump">continuous</mouseControl>
  <resolution>-1.00000000</resolution>
  <randomizable group="0">false</randomizable>
 </bsbObject>
 <bsbObject version="2" type="BSBHSlider">
  <objectName>file3</objectName>
  <x>1041</x>
  <y>331</y>
  <width>372</width>
  <height>16</height>
  <uuid>{4c7df905-bd88-4c72-8e4c-0cf40c2b3589}</uuid>
  <visible>true</visible>
  <midichan>0</midichan>
  <midicc>-3</midicc>
  <minimum>0.00000000</minimum>
  <maximum>1.00000000</maximum>
  <value>1.00000000</value>
  <mode>lin</mode>
  <mouseControl act="jump">continuous</mouseControl>
  <resolution>-1.00000000</resolution>
  <randomizable group="0">false</randomizable>
 </bsbObject>
 <bsbObject version="2" type="BSBHSlider">
  <objectName>file4</objectName>
  <x>1042</x>
  <y>370</y>
  <width>371</width>
  <height>16</height>
  <uuid>{f624c29d-7506-4b56-8f5d-f549ab544011}</uuid>
  <visible>true</visible>
  <midichan>0</midichan>
  <midicc>-3</midicc>
  <minimum>0.00000000</minimum>
  <maximum>1.00000000</maximum>
  <value>1.00000000</value>
  <mode>lin</mode>
  <mouseControl act="jump">continuous</mouseControl>
  <resolution>-1.00000000</resolution>
  <randomizable group="0">false</randomizable>
 </bsbObject>
 <bsbObject version="2" type="BSBController">
  <objectName>disp_rmsout</objectName>
  <x>1079</x>
  <y>584</y>
  <width>241</width>
  <height>11</height>
  <uuid>{a7f5b685-1ae2-4ffe-a60d-587cfd1b7b9b}</uuid>
  <visible>true</visible>
  <midichan>0</midichan>
  <midicc>-3</midicc>
  <objectName2>disp_rmsout</objectName2>
  <xMin>0.00000000</xMin>
  <xMax>1.00000000</xMax>
  <yMin>0.00000000</yMin>
  <yMax>1.00000000</yMax>
  <xValue>0.03436967</xValue>
  <yValue>0.03436967</yValue>
  <type>fill</type>
  <pointsize>1</pointsize>
  <fadeSpeed>0.00000000</fadeSpeed>
  <mouseControl act="press">jump</mouseControl>
  <color>
   <r>0</r>
   <g>234</g>
   <b>0</b>
  </color>
  <randomizable mode="both" group="0">false</randomizable>
  <bgcolor>
   <r>0</r>
   <g>0</g>
   <b>0</b>
  </bgcolor>
 </bsbObject>
 <bsbObject version="2" type="BSBController">
  <objectName>disp_rmsfile</objectName>
  <x>1042</x>
  <y>385</y>
  <width>371</width>
  <height>15</height>
  <uuid>{574f5c4e-bda6-4141-8609-3b53398d1ab2}</uuid>
  <visible>true</visible>
  <midichan>0</midichan>
  <midicc>-3</midicc>
  <objectName2>disp_rmsfile</objectName2>
  <xMin>0.00000000</xMin>
  <xMax>1.00000000</xMax>
  <yMin>0.00000000</yMin>
  <yMax>1.00000000</yMax>
  <xValue>0.08456600</xValue>
  <yValue>0.08456600</yValue>
  <type>fill</type>
  <pointsize>1</pointsize>
  <fadeSpeed>0.00000000</fadeSpeed>
  <mouseControl act="press">jump</mouseControl>
  <color>
   <r>0</r>
   <g>234</g>
   <b>0</b>
  </color>
  <randomizable mode="both" group="0">false</randomizable>
  <bgcolor>
   <r>0</r>
   <g>0</g>
   <b>0</b>
  </bgcolor>
 </bsbObject>
 <bsbObject version="2" type="BSBButton">
  <objectName>_Play</objectName>
  <x>1044</x>
  <y>90</y>
  <width>46</width>
  <height>27</height>
  <uuid>{6c07e034-9215-4f8c-a441-8aa2da9d4fc1}</uuid>
  <visible>true</visible>
  <midichan>0</midichan>
  <midicc>0</midicc>
  <type>value</type>
  <pressedValue>1.00000000</pressedValue>
  <stringvalue/>
  <text>RUN</text>
  <image>/</image>
  <eventLine>i1 0 10</eventLine>
  <latch>false</latch>
  <latched>true</latched>
 </bsbObject>
 <bsbObject version="2" type="BSBButton">
  <objectName>_Stop</objectName>
  <x>1091</x>
  <y>90</y>
  <width>55</width>
  <height>27</height>
  <uuid>{468ce0c1-f2b0-43f2-8797-1d16e4a3e3b5}</uuid>
  <visible>true</visible>
  <midichan>0</midichan>
  <midicc>0</midicc>
  <type>value</type>
  <pressedValue>1.00000000</pressedValue>
  <stringvalue/>
  <text>STOP</text>
  <image>/</image>
  <eventLine>i1 0 10</eventLine>
  <latch>false</latch>
  <latched>false</latched>
 </bsbObject>
 <bsbObject version="2" type="BSBButton">
  <objectName>_Browse4</objectName>
  <x>1126</x>
  <y>345</y>
  <width>48</width>
  <height>26</height>
  <uuid>{0637ad0c-8923-456c-ab8b-76f0cdff2850}</uuid>
  <visible>true</visible>
  <midichan>0</midichan>
  <midicc>0</midicc>
  <type>value</type>
  <pressedValue>1.00000000</pressedValue>
  <stringvalue/>
  <text>Gr 4</text>
  <image>/</image>
  <eventLine/>
  <latch>false</latch>
  <latched>false</latched>
 </bsbObject>
 <bsbObject version="2" type="BSBButton">
  <objectName>_Browse3</objectName>
  <x>1127</x>
  <y>307</y>
  <width>48</width>
  <height>24</height>
  <uuid>{7847a643-ec77-447f-b3f9-9435dffd77e0}</uuid>
  <visible>true</visible>
  <midichan>0</midichan>
  <midicc>0</midicc>
  <type>value</type>
  <pressedValue>1.00000000</pressedValue>
  <stringvalue/>
  <text>Gr 3</text>
  <image>/</image>
  <eventLine/>
  <latch>false</latch>
  <latched>false</latched>
 </bsbObject>
 <bsbObject version="2" type="BSBButton">
  <objectName>_Browse2</objectName>
  <x>1127</x>
  <y>270</y>
  <width>47</width>
  <height>24</height>
  <uuid>{e985aec2-0fa8-4f77-9403-160a08f7f2b2}</uuid>
  <visible>true</visible>
  <midichan>0</midichan>
  <midicc>0</midicc>
  <type>value</type>
  <pressedValue>1.00000000</pressedValue>
  <stringvalue/>
  <text>Gr 2</text>
  <image>/</image>
  <eventLine/>
  <latch>false</latch>
  <latched>false</latched>
 </bsbObject>
 <bsbObject version="2" type="BSBButton">
  <objectName/>
  <x>1082</x>
  <y>345</y>
  <width>49</width>
  <height>24</height>
  <uuid>{2cf4d94e-a06b-4ee8-9498-af4791ca7e64}</uuid>
  <visible>true</visible>
  <midichan>0</midichan>
  <midicc>-3</midicc>
  <type>event</type>
  <pressedValue>1.00000000</pressedValue>
  <stringvalue/>
  <text>Stop</text>
  <image>/</image>
  <eventLine>i 12 0 .1</eventLine>
  <latch>false</latch>
  <latched>false</latched>
 </bsbObject>
 <bsbObject version="2" type="BSBButton">
  <objectName/>
  <x>1043</x>
  <y>345</y>
  <width>49</width>
  <height>24</height>
  <uuid>{6bde4cf3-28ca-4a02-84d7-1f3fa617da58}</uuid>
  <visible>true</visible>
  <midichan>0</midichan>
  <midicc>-3</midicc>
  <type>event</type>
  <pressedValue>1.00000000</pressedValue>
  <stringvalue/>
  <text>Play</text>
  <image>/</image>
  <eventLine>i 11 0 9999</eventLine>
  <latch>false</latch>
  <latched>false</latched>
 </bsbObject>
 <bsbObject version="2" type="BSBButton">
  <objectName/>
  <x>1082</x>
  <y>307</y>
  <width>49</width>
  <height>24</height>
  <uuid>{97de43ef-7378-4c83-9551-5c45b5d107fb}</uuid>
  <visible>true</visible>
  <midichan>0</midichan>
  <midicc>-3</midicc>
  <type>event</type>
  <pressedValue>1.00000000</pressedValue>
  <stringvalue/>
  <text>Stop</text>
  <image>/</image>
  <eventLine>i 10 0 .1</eventLine>
  <latch>false</latch>
  <latched>false</latched>
 </bsbObject>
 <bsbObject version="2" type="BSBButton">
  <objectName/>
  <x>1042</x>
  <y>307</y>
  <width>49</width>
  <height>24</height>
  <uuid>{1de9d949-7ecb-4c52-8fd2-938e14d58f10}</uuid>
  <visible>true</visible>
  <midichan>0</midichan>
  <midicc>-3</midicc>
  <type>event</type>
  <pressedValue>1.00000000</pressedValue>
  <stringvalue/>
  <text>Play</text>
  <image>/</image>
  <eventLine>i 9 0 9999</eventLine>
  <latch>false</latch>
  <latched>false</latched>
 </bsbObject>
 <bsbObject version="2" type="BSBButton">
  <objectName/>
  <x>1082</x>
  <y>270</y>
  <width>49</width>
  <height>24</height>
  <uuid>{5f44cc1f-aca7-4bfc-b457-e2169f6355db}</uuid>
  <visible>true</visible>
  <midichan>0</midichan>
  <midicc>-3</midicc>
  <type>event</type>
  <pressedValue>1.00000000</pressedValue>
  <stringvalue/>
  <text>Stop</text>
  <image>/</image>
  <eventLine>i 8 0 .1</eventLine>
  <latch>false</latch>
  <latched>false</latched>
 </bsbObject>
 <bsbObject version="2" type="BSBButton">
  <objectName/>
  <x>1042</x>
  <y>270</y>
  <width>49</width>
  <height>24</height>
  <uuid>{33d74f6c-04ee-4528-aadd-775e963b7564}</uuid>
  <visible>true</visible>
  <midichan>0</midichan>
  <midicc>-3</midicc>
  <type>event</type>
  <pressedValue>1.00000000</pressedValue>
  <stringvalue/>
  <text>Play</text>
  <image>/</image>
  <eventLine>i 7 0 9999</eventLine>
  <latch>false</latch>
  <latched>false</latched>
 </bsbObject>
 <bsbObject version="2" type="BSBButton">
  <objectName/>
  <x>1081</x>
  <y>229</y>
  <width>49</width>
  <height>24</height>
  <uuid>{f83905c4-e473-41b9-98fb-2e5071eac940}</uuid>
  <visible>true</visible>
  <midichan>0</midichan>
  <midicc>-3</midicc>
  <type>event</type>
  <pressedValue>1.00000000</pressedValue>
  <stringvalue/>
  <text>Stop</text>
  <image>/</image>
  <eventLine>i 6 0 .1</eventLine>
  <latch>false</latch>
  <latched>false</latched>
 </bsbObject>
 <bsbObject version="2" type="BSBHSlider">
  <objectName>levrev</objectName>
  <x>1095</x>
  <y>656</y>
  <width>321</width>
  <height>15</height>
  <uuid>{9bb90bbf-528c-4860-9404-5818ee2ca53a}</uuid>
  <visible>true</visible>
  <midichan>0</midichan>
  <midicc>-3</midicc>
  <minimum>0.00000000</minimum>
  <maximum>1.00000000</maximum>
  <value>0.91588785</value>
  <mode>lin</mode>
  <mouseControl act="jump">continuous</mouseControl>
  <resolution>-1.00000000</resolution>
  <randomizable group="0">false</randomizable>
 </bsbObject>
 <bsbObject version="2" type="BSBHSlider">
  <objectName>room</objectName>
  <x>1095</x>
  <y>641</y>
  <width>321</width>
  <height>14</height>
  <uuid>{41b04d9f-c9d8-4d1d-b5f0-aa596adc2c4b}</uuid>
  <visible>true</visible>
  <midichan>0</midichan>
  <midicc>-3</midicc>
  <minimum>0.00000000</minimum>
  <maximum>1.00000000</maximum>
  <value>1.00000000</value>
  <mode>lin</mode>
  <mouseControl act="jump">continuous</mouseControl>
  <resolution>-1.00000000</resolution>
  <randomizable group="0">false</randomizable>
 </bsbObject>
 <bsbObject version="2" type="BSBHSlider">
  <objectName>damp</objectName>
  <x>1095</x>
  <y>626</y>
  <width>321</width>
  <height>14</height>
  <uuid>{657a9974-1fbd-47f2-8efc-1b9064131da4}</uuid>
  <visible>true</visible>
  <midichan>0</midichan>
  <midicc>-3</midicc>
  <minimum>0.00000000</minimum>
  <maximum>1.00000000</maximum>
  <value>0.71651090</value>
  <mode>lin</mode>
  <mouseControl act="jump">continuous</mouseControl>
  <resolution>-1.00000000</resolution>
  <randomizable group="0">false</randomizable>
 </bsbObject>
 <bsbObject version="2" type="BSBButton">
  <objectName/>
  <x>8</x>
  <y>771</y>
  <width>126</width>
  <height>28</height>
  <uuid>{80b14975-eb64-4d64-92b3-ddda15e97fe3}</uuid>
  <visible>true</visible>
  <midichan>0</midichan>
  <midicc>0</midicc>
  <type>event</type>
  <pressedValue>1.00000000</pressedValue>
  <stringvalue/>
  <text>CONTROLLER1</text>
  <image>/</image>
  <eventLine>i1 0 -10</eventLine>
  <latch>true</latch>
  <latched>false</latched>
 </bsbObject>
 <bsbObject version="2" type="BSBDropdown">
  <objectName>selgrain5</objectName>
  <x>426</x>
  <y>748</y>
  <width>62</width>
  <height>25</height>
  <uuid>{3a026b22-115c-47bc-8ce5-1c87a7a1126d}</uuid>
  <visible>true</visible>
  <midichan>0</midichan>
  <midicc>-3</midicc>
  <bsbDropdownItemList>
   <bsbDropdownItem>
    <name>MAN</name>
    <value>0</value>
    <stringvalue/>
   </bsbDropdownItem>
   <bsbDropdownItem>
    <name>LIVE</name>
    <value>1</value>
    <stringvalue/>
   </bsbDropdownItem>
  </bsbDropdownItemList>
  <selectedIndex>0</selectedIndex>
  <randomizable group="0">false</randomizable>
 </bsbObject>
 <bsbObject version="2" type="BSBDropdown">
  <objectName>seluscite</objectName>
  <x>490</x>
  <y>253</y>
  <width>44</width>
  <height>24</height>
  <uuid>{8cf5ea92-4943-489e-8e51-10af27f3995e}</uuid>
  <visible>true</visible>
  <midichan>0</midichan>
  <midicc>-3</midicc>
  <bsbDropdownItemList>
   <bsbDropdownItem>
    <name>1-RAND</name>
    <value>0</value>
    <stringvalue/>
   </bsbDropdownItem>
   <bsbDropdownItem>
    <name>2-MANUAL</name>
    <value>1</value>
    <stringvalue/>
   </bsbDropdownItem>
   <bsbDropdownItem>
    <name>3-OSCIL</name>
    <value>2</value>
    <stringvalue/>
   </bsbDropdownItem>
   <bsbDropdownItem>
    <name>4-MOD</name>
    <value>3</value>
    <stringvalue/>
   </bsbDropdownItem>
  </bsbDropdownItemList>
  <selectedIndex>2</selectedIndex>
  <randomizable group="0">false</randomizable>
 </bsbObject>
 <bsbObject version="2" type="BSBKnob">
  <objectName>mod1</objectName>
  <x>366</x>
  <y>285</y>
  <width>36</width>
  <height>42</height>
  <uuid>{6aa4496e-130e-4422-b717-831ae05567a2}</uuid>
  <visible>true</visible>
  <midichan>0</midichan>
  <midicc>-3</midicc>
  <minimum>0.00000000</minimum>
  <maximum>1.00000000</maximum>
  <value>0.47474700</value>
  <mode>lin</mode>
  <mouseControl act="jump">continuous</mouseControl>
  <resolution>0.01000000</resolution>
  <randomizable group="0">false</randomizable>
 </bsbObject>
 <bsbObject version="2" type="BSBKnob">
  <objectName>mod2</objectName>
  <x>367</x>
  <y>344</y>
  <width>36</width>
  <height>42</height>
  <uuid>{0160ecb9-affa-4ece-847b-143a811147ed}</uuid>
  <visible>true</visible>
  <midichan>0</midichan>
  <midicc>-3</midicc>
  <minimum>0.00000000</minimum>
  <maximum>1.00000000</maximum>
  <value>0.53535400</value>
  <mode>lin</mode>
  <mouseControl act="jump">continuous</mouseControl>
  <resolution>0.01000000</resolution>
  <randomizable group="0">false</randomizable>
 </bsbObject>
 <bsbObject version="2" type="BSBKnob">
  <objectName>mod3</objectName>
  <x>367</x>
  <y>403</y>
  <width>36</width>
  <height>42</height>
  <uuid>{aee9f71d-c136-45f1-8e60-6e0e6e30b534}</uuid>
  <visible>true</visible>
  <midichan>0</midichan>
  <midicc>-3</midicc>
  <minimum>0.00000000</minimum>
  <maximum>1.00000000</maximum>
  <value>0.47474700</value>
  <mode>lin</mode>
  <mouseControl act="jump">continuous</mouseControl>
  <resolution>0.01000000</resolution>
  <randomizable group="0">false</randomizable>
 </bsbObject>
 <bsbObject version="2" type="BSBKnob">
  <objectName>mod4</objectName>
  <x>366</x>
  <y>462</y>
  <width>36</width>
  <height>42</height>
  <uuid>{d90d725c-2c52-46cd-9004-042ece68a9b9}</uuid>
  <visible>true</visible>
  <midichan>0</midichan>
  <midicc>-3</midicc>
  <minimum>0.00000000</minimum>
  <maximum>1.00000000</maximum>
  <value>0.47474700</value>
  <mode>lin</mode>
  <mouseControl act="jump">continuous</mouseControl>
  <resolution>0.01000000</resolution>
  <randomizable group="0">false</randomizable>
 </bsbObject>
 <bsbObject version="2" type="BSBKnob">
  <objectName>mod5</objectName>
  <x>626</x>
  <y>285</y>
  <width>36</width>
  <height>42</height>
  <uuid>{451d1520-65bb-478b-a420-7519643a3046}</uuid>
  <visible>true</visible>
  <midichan>0</midichan>
  <midicc>-3</midicc>
  <minimum>0.00000000</minimum>
  <maximum>1.00000000</maximum>
  <value>0.54545500</value>
  <mode>lin</mode>
  <mouseControl act="jump">continuous</mouseControl>
  <resolution>0.01000000</resolution>
  <randomizable group="0">false</randomizable>
 </bsbObject>
 <bsbObject version="2" type="BSBKnob">
  <objectName>mod6</objectName>
  <x>627</x>
  <y>344</y>
  <width>36</width>
  <height>42</height>
  <uuid>{6d244b5e-d11d-4097-84e6-158336720159}</uuid>
  <visible>true</visible>
  <midichan>0</midichan>
  <midicc>-3</midicc>
  <minimum>0.00000000</minimum>
  <maximum>1.00000000</maximum>
  <value>0.47474700</value>
  <mode>lin</mode>
  <mouseControl act="jump">continuous</mouseControl>
  <resolution>0.01000000</resolution>
  <randomizable group="0">false</randomizable>
 </bsbObject>
 <bsbObject version="2" type="BSBKnob">
  <objectName>mod7</objectName>
  <x>627</x>
  <y>403</y>
  <width>36</width>
  <height>42</height>
  <uuid>{d9d6c35b-b62c-40a2-8497-01e3d9732d67}</uuid>
  <visible>true</visible>
  <midichan>0</midichan>
  <midicc>-3</midicc>
  <minimum>0.00000000</minimum>
  <maximum>1.00000000</maximum>
  <value>0.45454500</value>
  <mode>lin</mode>
  <mouseControl act="jump">continuous</mouseControl>
  <resolution>0.01000000</resolution>
  <randomizable group="0">false</randomizable>
 </bsbObject>
 <bsbObject version="2" type="BSBKnob">
  <objectName>mod8</objectName>
  <x>626</x>
  <y>462</y>
  <width>36</width>
  <height>42</height>
  <uuid>{bf5813c8-e773-4df1-adb3-efb483bb53c5}</uuid>
  <visible>true</visible>
  <midichan>0</midichan>
  <midicc>0</midicc>
  <minimum>0.00000000</minimum>
  <maximum>1.00000000</maximum>
  <value>0.47474700</value>
  <mode>lin</mode>
  <mouseControl act="jump">continuous</mouseControl>
  <resolution>0.01000000</resolution>
  <randomizable group="0">false</randomizable>
 </bsbObject>
 <bsbObject version="2" type="BSBButton">
  <objectName/>
  <x>1039</x>
  <y>418</y>
  <width>41</width>
  <height>28</height>
  <uuid>{fefe2e3f-6cf9-4236-9720-d66e2df6ea27}</uuid>
  <visible>true</visible>
  <midichan>0</midichan>
  <midicc>0</midicc>
  <type>event</type>
  <pressedValue>1.00000000</pressedValue>
  <stringvalue/>
  <text>A</text>
  <image>/</image>
  <eventLine>i38 0 -10</eventLine>
  <latch>true</latch>
  <latched>false</latched>
 </bsbObject>
 <bsbObject version="2" type="BSBButton">
  <objectName/>
  <x>1042</x>
  <y>512</y>
  <width>41</width>
  <height>28</height>
  <uuid>{ce631d3d-2e4f-4f3e-8956-a872862022cd}</uuid>
  <visible>true</visible>
  <midichan>0</midichan>
  <midicc>0</midicc>
  <type>event</type>
  <pressedValue>1.00000000</pressedValue>
  <stringvalue/>
  <text>A</text>
  <image>/</image>
  <eventLine>i39 0 -10</eventLine>
  <latch>true</latch>
  <latched>false</latched>
 </bsbObject>
 <bsbObject version="2" type="BSBLabel">
  <objectName/>
  <x>1051</x>
  <y>637</y>
  <width>42</width>
  <height>22</height>
  <uuid>{9d26029c-aaae-40ec-9c70-b6a3e2ed3b8b}</uuid>
  <visible>true</visible>
  <midichan>0</midichan>
  <midicc>-3</midicc>
  <label>Room </label>
  <alignment>left</alignment>
  <font>Arial</font>
  <fontsize>12</fontsize>
  <precision>3</precision>
  <color>
   <r>0</r>
   <g>0</g>
   <b>0</b>
  </color>
  <bgcolor mode="nobackground">
   <r>255</r>
   <g>255</g>
   <b>255</b>
  </bgcolor>
  <bordermode>noborder</bordermode>
  <borderradius>1</borderradius>
  <borderwidth>1</borderwidth>
 </bsbObject>
 <bsbObject version="2" type="BSBLabel">
  <objectName/>
  <x>1056</x>
  <y>651</y>
  <width>29</width>
  <height>23</height>
  <uuid>{9323c4d0-c20c-49f4-8b31-06ce27569dff}</uuid>
  <visible>true</visible>
  <midichan>0</midichan>
  <midicc>-3</midicc>
  <label>Vol </label>
  <alignment>left</alignment>
  <font>Arial</font>
  <fontsize>12</fontsize>
  <precision>3</precision>
  <color>
   <r>0</r>
   <g>0</g>
   <b>0</b>
  </color>
  <bgcolor mode="nobackground">
   <r>255</r>
   <g>255</g>
   <b>255</b>
  </bgcolor>
  <bordermode>noborder</bordermode>
  <borderradius>1</borderradius>
  <borderwidth>1</borderwidth>
 </bsbObject>
 <bsbObject version="2" type="BSBLabel">
  <objectName/>
  <x>1050</x>
  <y>620</y>
  <width>44</width>
  <height>23</height>
  <uuid>{3a76de18-bbf1-4a9b-b451-ef2007e9a727}</uuid>
  <visible>true</visible>
  <midichan>0</midichan>
  <midicc>-3</midicc>
  <label>Damp</label>
  <alignment>left</alignment>
  <font>Arial</font>
  <fontsize>12</fontsize>
  <precision>3</precision>
  <color>
   <r>0</r>
   <g>0</g>
   <b>0</b>
  </color>
  <bgcolor mode="nobackground">
   <r>255</r>
   <g>255</g>
   <b>255</b>
  </bgcolor>
  <bordermode>noborder</bordermode>
  <borderradius>1</borderradius>
  <borderwidth>1</borderwidth>
 </bsbObject>
 <bsbObject version="2" type="BSBLabel">
  <objectName/>
  <x>1034</x>
  <y>680</y>
  <width>390</width>
  <height>162</height>
  <uuid>{12e7ae42-14d5-475b-b71e-b2bb023af927}</uuid>
  <visible>true</visible>
  <midichan>0</midichan>
  <midicc>0</midicc>
  <label/>
  <alignment>center</alignment>
  <font>Arial</font>
  <fontsize>12</fontsize>
  <precision>3</precision>
  <color>
   <r>255</r>
   <g>255</g>
   <b>255</b>
  </color>
  <bgcolor mode="background">
   <r>0</r>
   <g>0</g>
   <b>0</b>
  </bgcolor>
  <bordermode>border</bordermode>
  <borderradius>10</borderradius>
  <borderwidth>5</borderwidth>
 </bsbObject>
 <bsbObject version="2" type="BSBVSlider">
  <objectName>ampsin</objectName>
  <x>1085</x>
  <y>760</y>
  <width>13</width>
  <height>75</height>
  <uuid>{00deb488-08da-4114-b267-8327f6993e17}</uuid>
  <visible>true</visible>
  <midichan>0</midichan>
  <midicc>-3</midicc>
  <minimum>0.00000000</minimum>
  <maximum>1.00000000</maximum>
  <value>1.00000000</value>
  <mode>lin</mode>
  <mouseControl act="jump">continuous</mouseControl>
  <resolution>-1.00000000</resolution>
  <randomizable group="0">false</randomizable>
 </bsbObject>
 <bsbObject version="2" type="BSBVSlider">
  <objectName>kfreqsin</objectName>
  <x>1115</x>
  <y>760</y>
  <width>13</width>
  <height>75</height>
  <uuid>{5f8f16bb-9daa-4f19-9e23-8e09550f5c30}</uuid>
  <visible>true</visible>
  <midichan>0</midichan>
  <midicc>0</midicc>
  <minimum>0.00000000</minimum>
  <maximum>20000.00000000</maximum>
  <value>15200.00000000</value>
  <mode>lin</mode>
  <mouseControl act="jump">continuous</mouseControl>
  <resolution>-1.00000000</resolution>
  <randomizable group="0">false</randomizable>
 </bsbObject>
 <bsbObject version="2" type="BSBVSlider">
  <objectName>freqfm</objectName>
  <x>1183</x>
  <y>760</y>
  <width>13</width>
  <height>75</height>
  <uuid>{3feb4144-6b1b-4273-a57e-957e1d96be1d}</uuid>
  <visible>true</visible>
  <midichan>0</midichan>
  <midicc>-3</midicc>
  <minimum>0.00000000</minimum>
  <maximum>10000.00000000</maximum>
  <value>10000.00000000</value>
  <mode>lin</mode>
  <mouseControl act="jump">continuous</mouseControl>
  <resolution>-1.00000000</resolution>
  <randomizable group="0">false</randomizable>
 </bsbObject>
 <bsbObject version="2" type="BSBLabel">
  <objectName/>
  <x>1073</x>
  <y>706</y>
  <width>67</width>
  <height>22</height>
  <uuid>{95ee9532-c7ae-4af5-b8e7-0c2eec3b82d0}</uuid>
  <visible>true</visible>
  <midichan>0</midichan>
  <midicc>0</midicc>
  <label>SIN
</label>
  <alignment>left</alignment>
  <font>Arial</font>
  <fontsize>12</fontsize>
  <precision>3</precision>
  <color>
   <r>255</r>
   <g>255</g>
   <b>255</b>
  </color>
  <bgcolor mode="background">
   <r>0</r>
   <g>0</g>
   <b>0</b>
  </bgcolor>
  <bordermode>noborder</bordermode>
  <borderradius>1</borderradius>
  <borderwidth>1</borderwidth>
 </bsbObject>
 <bsbObject version="2" type="BSBLabel">
  <objectName/>
  <x>1073</x>
  <y>726</y>
  <width>32</width>
  <height>20</height>
  <uuid>{76945466-805e-496a-83f3-8ba2b34f160e}</uuid>
  <visible>true</visible>
  <midichan>0</midichan>
  <midicc>0</midicc>
  <label>AMP
</label>
  <alignment>left</alignment>
  <font>Arial</font>
  <fontsize>8</fontsize>
  <precision>3</precision>
  <color>
   <r>255</r>
   <g>255</g>
   <b>255</b>
  </color>
  <bgcolor mode="background">
   <r>0</r>
   <g>0</g>
   <b>0</b>
  </bgcolor>
  <bordermode>noborder</bordermode>
  <borderradius>1</borderradius>
  <borderwidth>1</borderwidth>
 </bsbObject>
 <bsbObject version="2" type="BSBVSlider">
  <objectName>ndx</objectName>
  <x>1276</x>
  <y>760</y>
  <width>13</width>
  <height>75</height>
  <uuid>{f1e12a89-97c8-401b-be8f-cbe62a919f39}</uuid>
  <visible>true</visible>
  <midichan>0</midichan>
  <midicc>-3</midicc>
  <minimum>0.00000000</minimum>
  <maximum>10.00000000</maximum>
  <value>10.00000000</value>
  <mode>lin</mode>
  <mouseControl act="jump">continuous</mouseControl>
  <resolution>-1.00000000</resolution>
  <randomizable group="0">false</randomizable>
 </bsbObject>
 <bsbObject version="2" type="BSBVSlider">
  <objectName>mod</objectName>
  <x>1245</x>
  <y>760</y>
  <width>13</width>
  <height>75</height>
  <uuid>{47c3133c-3f89-4a47-b980-078e2dd31c66}</uuid>
  <visible>true</visible>
  <midichan>0</midichan>
  <midicc>-3</midicc>
  <minimum>0.00000000</minimum>
  <maximum>100.00000000</maximum>
  <value>100.00000000</value>
  <mode>lin</mode>
  <mouseControl act="jump">continuous</mouseControl>
  <resolution>-1.00000000</resolution>
  <randomizable group="0">false</randomizable>
 </bsbObject>
 <bsbObject version="2" type="BSBVSlider">
  <objectName>car</objectName>
  <x>1214</x>
  <y>760</y>
  <width>13</width>
  <height>75</height>
  <uuid>{1c9dab0f-a2e2-41e6-badd-3dbd1331991e}</uuid>
  <visible>true</visible>
  <midichan>0</midichan>
  <midicc>-3</midicc>
  <minimum>0.00000000</minimum>
  <maximum>100.00000000</maximum>
  <value>90.66666667</value>
  <mode>lin</mode>
  <mouseControl act="jump">continuous</mouseControl>
  <resolution>-1.00000000</resolution>
  <randomizable group="0">false</randomizable>
 </bsbObject>
 <bsbObject version="2" type="BSBVSlider">
  <objectName>att</objectName>
  <x>1314</x>
  <y>760</y>
  <width>13</width>
  <height>75</height>
  <uuid>{114b6320-6f70-4b76-a4af-9d7a48097d9b}</uuid>
  <visible>true</visible>
  <midichan>0</midichan>
  <midicc>-3</midicc>
  <minimum>0.00000000</minimum>
  <maximum>0.50000000</maximum>
  <value>0.24000000</value>
  <mode>lin</mode>
  <mouseControl act="jump">continuous</mouseControl>
  <resolution>-1.00000000</resolution>
  <randomizable group="0">false</randomizable>
 </bsbObject>
 <bsbObject version="2" type="BSBVSlider">
  <objectName>ampfm</objectName>
  <x>1152</x>
  <y>760</y>
  <width>13</width>
  <height>75</height>
  <uuid>{46a61292-1bc7-4bd8-a962-923c84ef733b}</uuid>
  <visible>true</visible>
  <midichan>0</midichan>
  <midicc>-3</midicc>
  <minimum>0.00000000</minimum>
  <maximum>1.00000000</maximum>
  <value>0.69333333</value>
  <mode>lin</mode>
  <mouseControl act="jump">continuous</mouseControl>
  <resolution>-1.00000000</resolution>
  <randomizable group="0">false</randomizable>
 </bsbObject>
 <bsbObject version="2" type="BSBVSlider">
  <objectName>krel</objectName>
  <x>1347</x>
  <y>760</y>
  <width>13</width>
  <height>75</height>
  <uuid>{c7fd8df7-f2aa-4472-ab11-bf35f38fe3d1}</uuid>
  <visible>true</visible>
  <midichan>0</midichan>
  <midicc>-3</midicc>
  <minimum>0.00000000</minimum>
  <maximum>0.50000000</maximum>
  <value>0.18666667</value>
  <mode>lin</mode>
  <mouseControl act="jump">continuous</mouseControl>
  <resolution>-1.00000000</resolution>
  <randomizable group="0">false</randomizable>
 </bsbObject>
 <bsbObject version="2" type="BSBDropdown">
  <objectName>selsynth1</objectName>
  <x>1200</x>
  <y>684</y>
  <width>78</width>
  <height>25</height>
  <uuid>{8062b6b3-90f5-4088-bc96-e8a855cace5d}</uuid>
  <visible>true</visible>
  <midichan>0</midichan>
  <midicc>0</midicc>
  <bsbDropdownItemList>
   <bsbDropdownItem>
    <name>LIVE</name>
    <value>0</value>
    <stringvalue/>
   </bsbDropdownItem>
   <bsbDropdownItem>
    <name>SYNTH</name>
    <value>1</value>
    <stringvalue/>
   </bsbDropdownItem>
   <bsbDropdownItem>
    <name>FIBONACCI</name>
    <value>2</value>
    <stringvalue/>
   </bsbDropdownItem>
  </bsbDropdownItemList>
  <selectedIndex>1</selectedIndex>
  <randomizable group="0">false</randomizable>
 </bsbObject>
 <bsbObject version="2" type="BSBDropdown">
  <objectName>selsynth2</objectName>
  <x>1276</x>
  <y>684</y>
  <width>52</width>
  <height>23</height>
  <uuid>{6abf93fc-c718-48fa-86a3-7e6b908801a4}</uuid>
  <visible>true</visible>
  <midichan>0</midichan>
  <midicc>0</midicc>
  <bsbDropdownItemList>
   <bsbDropdownItem>
    <name>SIN</name>
    <value>0</value>
    <stringvalue/>
   </bsbDropdownItem>
   <bsbDropdownItem>
    <name>FM</name>
    <value>1</value>
    <stringvalue/>
   </bsbDropdownItem>
  </bsbDropdownItemList>
  <selectedIndex>0</selectedIndex>
  <randomizable group="0">false</randomizable>
 </bsbObject>
 <bsbObject version="2" type="BSBButton">
  <objectName/>
  <x>1119</x>
  <y>682</y>
  <width>80</width>
  <height>27</height>
  <uuid>{f72c6a89-d4af-48f7-b2e3-7fe82e0494e8}</uuid>
  <visible>true</visible>
  <midichan>0</midichan>
  <midicc>0</midicc>
  <type>event</type>
  <pressedValue>1.00000000</pressedValue>
  <stringvalue/>
  <text>SYNTH</text>
  <image>/</image>
  <eventLine>i4 0 -10</eventLine>
  <latch>true</latch>
  <latched>false</latched>
 </bsbObject>
 <bsbObject version="2" type="BSBScrollNumber">
  <objectName>ampsin</objectName>
  <x>1073</x>
  <y>744</y>
  <width>34</width>
  <height>16</height>
  <uuid>{71449b46-fe1d-42b9-a296-cd356c57266f}</uuid>
  <visible>true</visible>
  <midichan>0</midichan>
  <midicc>-3</midicc>
  <alignment>left</alignment>
  <font>Arial</font>
  <fontsize>10</fontsize>
  <color>
   <r>255</r>
   <g>255</g>
   <b>255</b>
  </color>
  <bgcolor mode="background">
   <r>0</r>
   <g>0</g>
   <b>0</b>
  </bgcolor>
  <value>1.00000000</value>
  <resolution>0.00100000</resolution>
  <minimum>-999999999999.00000000</minimum>
  <maximum>99999999999999.00000000</maximum>
  <bordermode>noborder</bordermode>
  <borderradius>1</borderradius>
  <borderwidth>1</borderwidth>
  <randomizable group="0">false</randomizable>
  <mouseControl act=""/>
 </bsbObject>
 <bsbObject version="2" type="BSBScrollNumber">
  <objectName>kfreqsin</objectName>
  <x>1106</x>
  <y>744</y>
  <width>34</width>
  <height>16</height>
  <uuid>{020a4b5d-fd78-4e43-9f0d-fa242a2f2ee8}</uuid>
  <visible>true</visible>
  <midichan>0</midichan>
  <midicc>-3</midicc>
  <alignment>left</alignment>
  <font>Arial</font>
  <fontsize>10</fontsize>
  <color>
   <r>255</r>
   <g>255</g>
   <b>255</b>
  </color>
  <bgcolor mode="background">
   <r>0</r>
   <g>0</g>
   <b>0</b>
  </bgcolor>
  <value>15200.00000000</value>
  <resolution>1.00000000</resolution>
  <minimum>-999999999999.00000000</minimum>
  <maximum>99999999999999.00000000</maximum>
  <bordermode>noborder</bordermode>
  <borderradius>1</borderradius>
  <borderwidth>1</borderwidth>
  <randomizable group="0">false</randomizable>
  <mouseControl act=""/>
 </bsbObject>
 <bsbObject version="2" type="BSBLabel">
  <objectName/>
  <x>1104</x>
  <y>726</y>
  <width>36</width>
  <height>20</height>
  <uuid>{8c709c7c-6f1c-4cd8-a846-e9ac1fb8c690}</uuid>
  <visible>true</visible>
  <midichan>0</midichan>
  <midicc>0</midicc>
  <label>FREQ
</label>
  <alignment>left</alignment>
  <font>Arial</font>
  <fontsize>8</fontsize>
  <precision>3</precision>
  <color>
   <r>255</r>
   <g>255</g>
   <b>255</b>
  </color>
  <bgcolor mode="background">
   <r>0</r>
   <g>0</g>
   <b>0</b>
  </bgcolor>
  <bordermode>noborder</bordermode>
  <borderradius>1</borderradius>
  <borderwidth>1</borderwidth>
 </bsbObject>
 <bsbObject version="2" type="BSBLabel">
  <objectName/>
  <x>1142</x>
  <y>727</y>
  <width>33</width>
  <height>18</height>
  <uuid>{78998a9b-25a5-4add-ac1b-30934b4ec2ca}</uuid>
  <visible>true</visible>
  <midichan>0</midichan>
  <midicc>0</midicc>
  <label>AMP

</label>
  <alignment>left</alignment>
  <font>Arial</font>
  <fontsize>8</fontsize>
  <precision>3</precision>
  <color>
   <r>255</r>
   <g>255</g>
   <b>255</b>
  </color>
  <bgcolor mode="background">
   <r>0</r>
   <g>0</g>
   <b>0</b>
  </bgcolor>
  <bordermode>noborder</bordermode>
  <borderradius>1</borderradius>
  <borderwidth>1</borderwidth>
 </bsbObject>
 <bsbObject version="2" type="BSBLabel">
  <objectName/>
  <x>1172</x>
  <y>727</y>
  <width>36</width>
  <height>18</height>
  <uuid>{6ebcee06-52be-4606-9028-97d07aa3616f}</uuid>
  <visible>true</visible>
  <midichan>0</midichan>
  <midicc>0</midicc>
  <label>FREQ
</label>
  <alignment>left</alignment>
  <font>Arial</font>
  <fontsize>8</fontsize>
  <precision>3</precision>
  <color>
   <r>255</r>
   <g>255</g>
   <b>255</b>
  </color>
  <bgcolor mode="background">
   <r>0</r>
   <g>0</g>
   <b>0</b>
  </bgcolor>
  <bordermode>noborder</bordermode>
  <borderradius>1</borderradius>
  <borderwidth>1</borderwidth>
 </bsbObject>
 <bsbObject version="2" type="BSBLabel">
  <objectName/>
  <x>1205</x>
  <y>727</y>
  <width>33</width>
  <height>18</height>
  <uuid>{be5d7687-52e3-4aae-8592-2fdf42645ef5}</uuid>
  <visible>true</visible>
  <midichan>0</midichan>
  <midicc>0</midicc>
  <label>CAR

</label>
  <alignment>left</alignment>
  <font>Arial</font>
  <fontsize>8</fontsize>
  <precision>3</precision>
  <color>
   <r>255</r>
   <g>255</g>
   <b>255</b>
  </color>
  <bgcolor mode="background">
   <r>0</r>
   <g>0</g>
   <b>0</b>
  </bgcolor>
  <bordermode>noborder</bordermode>
  <borderradius>1</borderradius>
  <borderwidth>1</borderwidth>
 </bsbObject>
 <bsbObject version="2" type="BSBLabel">
  <objectName/>
  <x>1236</x>
  <y>727</y>
  <width>33</width>
  <height>18</height>
  <uuid>{34ffbca9-585f-48c5-b255-f11ce13ae5ef}</uuid>
  <visible>true</visible>
  <midichan>0</midichan>
  <midicc>0</midicc>
  <label>MOD
</label>
  <alignment>left</alignment>
  <font>Arial</font>
  <fontsize>8</fontsize>
  <precision>3</precision>
  <color>
   <r>255</r>
   <g>255</g>
   <b>255</b>
  </color>
  <bgcolor mode="background">
   <r>0</r>
   <g>0</g>
   <b>0</b>
  </bgcolor>
  <bordermode>noborder</bordermode>
  <borderradius>1</borderradius>
  <borderwidth>1</borderwidth>
 </bsbObject>
 <bsbObject version="2" type="BSBLabel">
  <objectName/>
  <x>1267</x>
  <y>727</y>
  <width>34</width>
  <height>18</height>
  <uuid>{4086c91a-15e0-4204-a0b7-98f4a590bda1}</uuid>
  <visible>true</visible>
  <midichan>0</midichan>
  <midicc>0</midicc>
  <label>INDX
</label>
  <alignment>left</alignment>
  <font>Arial</font>
  <fontsize>8</fontsize>
  <precision>3</precision>
  <color>
   <r>255</r>
   <g>255</g>
   <b>255</b>
  </color>
  <bgcolor mode="background">
   <r>0</r>
   <g>0</g>
   <b>0</b>
  </bgcolor>
  <bordermode>noborder</bordermode>
  <borderradius>1</borderradius>
  <borderwidth>1</borderwidth>
 </bsbObject>
 <bsbObject version="2" type="BSBLabel">
  <objectName/>
  <x>1304</x>
  <y>725</y>
  <width>33</width>
  <height>18</height>
  <uuid>{0edd7717-30e9-4d4a-9e8e-8498ac0dac8c}</uuid>
  <visible>true</visible>
  <midichan>0</midichan>
  <midicc>0</midicc>
  <label>ATT
</label>
  <alignment>left</alignment>
  <font>Arial</font>
  <fontsize>8</fontsize>
  <precision>3</precision>
  <color>
   <r>255</r>
   <g>255</g>
   <b>255</b>
  </color>
  <bgcolor mode="background">
   <r>0</r>
   <g>0</g>
   <b>0</b>
  </bgcolor>
  <bordermode>noborder</bordermode>
  <borderradius>1</borderradius>
  <borderwidth>1</borderwidth>
 </bsbObject>
 <bsbObject version="2" type="BSBLabel">
  <objectName/>
  <x>1336</x>
  <y>724</y>
  <width>32</width>
  <height>20</height>
  <uuid>{d4965d35-ddcf-4639-90fe-fec1a5e2a6aa}</uuid>
  <visible>true</visible>
  <midichan>0</midichan>
  <midicc>0</midicc>
  <label>REL
</label>
  <alignment>left</alignment>
  <font>Arial</font>
  <fontsize>8</fontsize>
  <precision>3</precision>
  <color>
   <r>255</r>
   <g>255</g>
   <b>255</b>
  </color>
  <bgcolor mode="background">
   <r>0</r>
   <g>0</g>
   <b>0</b>
  </bgcolor>
  <bordermode>noborder</bordermode>
  <borderradius>1</borderradius>
  <borderwidth>1</borderwidth>
 </bsbObject>
 <bsbObject version="2" type="BSBLabel">
  <objectName/>
  <x>1142</x>
  <y>706</y>
  <width>159</width>
  <height>22</height>
  <uuid>{9e4cb9d3-5f6c-4e9b-978d-4efce21e1af8}</uuid>
  <visible>true</visible>
  <midichan>0</midichan>
  <midicc>0</midicc>
  <label>FM
</label>
  <alignment>left</alignment>
  <font>Arial</font>
  <fontsize>12</fontsize>
  <precision>3</precision>
  <color>
   <r>255</r>
   <g>255</g>
   <b>255</b>
  </color>
  <bgcolor mode="background">
   <r>0</r>
   <g>0</g>
   <b>0</b>
  </bgcolor>
  <bordermode>noborder</bordermode>
  <borderradius>1</borderradius>
  <borderwidth>1</borderwidth>
 </bsbObject>
 <bsbObject version="2" type="BSBLabel">
  <objectName/>
  <x>1304</x>
  <y>706</y>
  <width>64</width>
  <height>21</height>
  <uuid>{f2f98ed5-42e0-47c3-b100-61cf170bb3f2}</uuid>
  <visible>true</visible>
  <midichan>0</midichan>
  <midicc>0</midicc>
  <label>LIVE ENV
</label>
  <alignment>left</alignment>
  <font>Arial</font>
  <fontsize>12</fontsize>
  <precision>3</precision>
  <color>
   <r>255</r>
   <g>255</g>
   <b>255</b>
  </color>
  <bgcolor mode="background">
   <r>0</r>
   <g>0</g>
   <b>0</b>
  </bgcolor>
  <bordermode>noborder</bordermode>
  <borderradius>1</borderradius>
  <borderwidth>1</borderwidth>
 </bsbObject>
 <bsbObject version="2" type="BSBScrollNumber">
  <objectName>freqfm</objectName>
  <x>1174</x>
  <y>744</y>
  <width>32</width>
  <height>16</height>
  <uuid>{fa379bc0-b7de-44c3-bc1f-f8828c58798c}</uuid>
  <visible>true</visible>
  <midichan>0</midichan>
  <midicc>-3</midicc>
  <alignment>left</alignment>
  <font>Arial</font>
  <fontsize>10</fontsize>
  <color>
   <r>255</r>
   <g>255</g>
   <b>255</b>
  </color>
  <bgcolor mode="background">
   <r>0</r>
   <g>0</g>
   <b>0</b>
  </bgcolor>
  <value>10000.00000000</value>
  <resolution>1.00000000</resolution>
  <minimum>-999999999999.00000000</minimum>
  <maximum>99999999999999.00000000</maximum>
  <bordermode>noborder</bordermode>
  <borderradius>1</borderradius>
  <borderwidth>1</borderwidth>
  <randomizable group="0">false</randomizable>
  <mouseControl act=""/>
 </bsbObject>
 <bsbObject version="2" type="BSBScrollNumber">
  <objectName>ndx</objectName>
  <x>1269</x>
  <y>744</y>
  <width>32</width>
  <height>16</height>
  <uuid>{dd5e562c-e393-4755-929e-3aff77a036ea}</uuid>
  <visible>true</visible>
  <midichan>0</midichan>
  <midicc>0</midicc>
  <alignment>center</alignment>
  <font>Arial</font>
  <fontsize>10</fontsize>
  <color>
   <r>255</r>
   <g>255</g>
   <b>255</b>
  </color>
  <bgcolor mode="background">
   <r>0</r>
   <g>0</g>
   <b>0</b>
  </bgcolor>
  <value>10.00000000</value>
  <resolution>1.00000000</resolution>
  <minimum>-999999999999.00000000</minimum>
  <maximum>999999999999.00000000</maximum>
  <bordermode>noborder</bordermode>
  <borderradius>1</borderradius>
  <borderwidth>1</borderwidth>
  <randomizable group="0">false</randomizable>
  <mouseControl act=""/>
 </bsbObject>
 <bsbObject version="2" type="BSBScrollNumber">
  <objectName>krel</objectName>
  <x>1336</x>
  <y>743</y>
  <width>32</width>
  <height>17</height>
  <uuid>{915f7f9e-6d3c-408e-8902-734de826659a}</uuid>
  <visible>true</visible>
  <midichan>0</midichan>
  <midicc>-3</midicc>
  <alignment>left</alignment>
  <font>Arial</font>
  <fontsize>10</fontsize>
  <color>
   <r>255</r>
   <g>255</g>
   <b>255</b>
  </color>
  <bgcolor mode="background">
   <r>0</r>
   <g>0</g>
   <b>0</b>
  </bgcolor>
  <value>0.18666667</value>
  <resolution>0.00100000</resolution>
  <minimum>-999999999999.00000000</minimum>
  <maximum>99999999999999.00000000</maximum>
  <bordermode>noborder</bordermode>
  <borderradius>1</borderradius>
  <borderwidth>1</borderwidth>
  <randomizable group="0">false</randomizable>
  <mouseControl act=""/>
 </bsbObject>
 <bsbObject version="2" type="BSBScrollNumber">
  <objectName>att</objectName>
  <x>1304</x>
  <y>743</y>
  <width>32</width>
  <height>17</height>
  <uuid>{605e6cb7-1186-4eca-b030-54e25bf32d0e}</uuid>
  <visible>true</visible>
  <midichan>0</midichan>
  <midicc>-3</midicc>
  <alignment>left</alignment>
  <font>Arial</font>
  <fontsize>10</fontsize>
  <color>
   <r>255</r>
   <g>255</g>
   <b>255</b>
  </color>
  <bgcolor mode="background">
   <r>0</r>
   <g>0</g>
   <b>0</b>
  </bgcolor>
  <value>0.24000000</value>
  <resolution>0.00100000</resolution>
  <minimum>-999999999999.00000000</minimum>
  <maximum>99999999999999.00000000</maximum>
  <bordermode>noborder</bordermode>
  <borderradius>1</borderradius>
  <borderwidth>1</borderwidth>
  <randomizable group="0">false</randomizable>
  <mouseControl act=""/>
 </bsbObject>
 <bsbObject version="2" type="BSBScrollNumber">
  <objectName>mod</objectName>
  <x>1237</x>
  <y>744</y>
  <width>32</width>
  <height>16</height>
  <uuid>{eaa12e11-95e4-47bc-b781-2c3c1be62c7b}</uuid>
  <visible>true</visible>
  <midichan>0</midichan>
  <midicc>0</midicc>
  <alignment>center</alignment>
  <font>Arial</font>
  <fontsize>10</fontsize>
  <color>
   <r>255</r>
   <g>255</g>
   <b>255</b>
  </color>
  <bgcolor mode="background">
   <r>0</r>
   <g>0</g>
   <b>0</b>
  </bgcolor>
  <value>100.00000000</value>
  <resolution>0.00000000</resolution>
  <minimum>-999999999999.00000000</minimum>
  <maximum>999999999999.00000000</maximum>
  <bordermode>noborder</bordermode>
  <borderradius>1</borderradius>
  <borderwidth>1</borderwidth>
  <randomizable group="0">false</randomizable>
  <mouseControl act=""/>
 </bsbObject>
 <bsbObject version="2" type="BSBScrollNumber">
  <objectName>car</objectName>
  <x>1206</x>
  <y>744</y>
  <width>32</width>
  <height>16</height>
  <uuid>{aa226581-6f2b-46e5-b735-87a956d46886}</uuid>
  <visible>true</visible>
  <midichan>0</midichan>
  <midicc>0</midicc>
  <alignment>center</alignment>
  <font>Arial</font>
  <fontsize>10</fontsize>
  <color>
   <r>255</r>
   <g>255</g>
   <b>255</b>
  </color>
  <bgcolor mode="background">
   <r>0</r>
   <g>0</g>
   <b>0</b>
  </bgcolor>
  <value>90.66666667</value>
  <resolution>1.00000000</resolution>
  <minimum>-999999999999.00000000</minimum>
  <maximum>999999999999.00000000</maximum>
  <bordermode>noborder</bordermode>
  <borderradius>1</borderradius>
  <borderwidth>1</borderwidth>
  <randomizable group="0">false</randomizable>
  <mouseControl act=""/>
 </bsbObject>
 <bsbObject version="2" type="BSBScrollNumber">
  <objectName>ampfm</objectName>
  <x>1142</x>
  <y>744</y>
  <width>32</width>
  <height>16</height>
  <uuid>{c2b1c004-ee9d-4b8e-8de1-ee97099ac164}</uuid>
  <visible>true</visible>
  <midichan>0</midichan>
  <midicc>-3</midicc>
  <alignment>left</alignment>
  <font>Arial</font>
  <fontsize>10</fontsize>
  <color>
   <r>255</r>
   <g>255</g>
   <b>255</b>
  </color>
  <bgcolor mode="background">
   <r>0</r>
   <g>0</g>
   <b>0</b>
  </bgcolor>
  <value>0.69333333</value>
  <resolution>0.00100000</resolution>
  <minimum>-999999999999.00000000</minimum>
  <maximum>99999999999999.00000000</maximum>
  <bordermode>noborder</bordermode>
  <borderradius>1</borderradius>
  <borderwidth>1</borderwidth>
  <randomizable group="0">false</randomizable>
  <mouseControl act=""/>
 </bsbObject>
 <bsbObject version="2" type="BSBButton">
  <objectName/>
  <x>888</x>
  <y>19</y>
  <width>131</width>
  <height>31</height>
  <uuid>{418b7ad0-2d12-4d45-8621-d563a09732c5}</uuid>
  <visible>true</visible>
  <midichan>0</midichan>
  <midicc>0</midicc>
  <type>event</type>
  <pressedValue>1.00000000</pressedValue>
  <stringvalue/>
  <text>GRAIN RAND 2</text>
  <image>/</image>
  <eventLine>i1 0 10</eventLine>
  <latch>true</latch>
  <latched>false</latched>
 </bsbObject>
 <bsbObject version="2" type="BSBButton">
  <objectName/>
  <x>12</x>
  <y>16</y>
  <width>128</width>
  <height>32</height>
  <uuid>{5844298b-17ef-4ac7-bccf-c3dd8ae0a000}</uuid>
  <visible>true</visible>
  <midichan>0</midichan>
  <midicc>0</midicc>
  <type>event</type>
  <pressedValue>1.00000000</pressedValue>
  <stringvalue/>
  <text>GRAIN RAND 1</text>
  <image>/</image>
  <eventLine>i1 0 10</eventLine>
  <latch>true</latch>
  <latched>false</latched>
 </bsbObject>
 <bsbObject version="2" type="BSBLabel">
  <objectName/>
  <x>760</x>
  <y>543</y>
  <width>270</width>
  <height>297</height>
  <uuid>{d99fc915-f08c-4552-9a32-40ab321b4b67}</uuid>
  <visible>true</visible>
  <midichan>0</midichan>
  <midicc>0</midicc>
  <label/>
  <alignment>left</alignment>
  <font>Arial</font>
  <fontsize>12</fontsize>
  <precision>3</precision>
  <color>
   <r>0</r>
   <g>0</g>
   <b>0</b>
  </color>
  <bgcolor mode="background">
   <r>128</r>
   <g>0</g>
   <b>0</b>
  </bgcolor>
  <bordermode>border</bordermode>
  <borderradius>10</borderradius>
  <borderwidth>5</borderwidth>
 </bsbObject>
 <bsbObject version="2" type="BSBHSlider">
  <objectName>wfreq1</objectName>
  <x>766</x>
  <y>569</y>
  <width>218</width>
  <height>19</height>
  <uuid>{24af29fa-0a6b-4a1d-941c-85d13879ba6f}</uuid>
  <visible>true</visible>
  <midichan>0</midichan>
  <midicc>0</midicc>
  <minimum>20.00000000</minimum>
  <maximum>1000.00000000</maximum>
  <value>640.36697248</value>
  <mode>lin</mode>
  <mouseControl act="jump">continuous</mouseControl>
  <resolution>-1.00000000</resolution>
  <randomizable group="0">false</randomizable>
 </bsbObject>
 <bsbObject version="2" type="BSBButton">
  <objectName/>
  <x>771</x>
  <y>544</y>
  <width>118</width>
  <height>25</height>
  <uuid>{afcec47e-f768-4e7b-b7dd-491d577338f4}</uuid>
  <visible>true</visible>
  <midichan>0</midichan>
  <midicc>0</midicc>
  <type>event</type>
  <pressedValue>1.00000000</pressedValue>
  <stringvalue/>
  <text>PULSE RESON</text>
  <image>/</image>
  <eventLine>i2 0 -10</eventLine>
  <latch>true</latch>
  <latched>false</latched>
 </bsbObject>
 <bsbObject version="2" type="BSBLabel">
  <objectName/>
  <x>985</x>
  <y>620</y>
  <width>43</width>
  <height>22</height>
  <uuid>{de05a5d4-76c5-4cc1-a578-f616be410599}</uuid>
  <visible>true</visible>
  <midichan>0</midichan>
  <midicc>0</midicc>
  <label>FREQ2</label>
  <alignment>left</alignment>
  <font>Arial</font>
  <fontsize>10</fontsize>
  <precision>3</precision>
  <color>
   <r>0</r>
   <g>0</g>
   <b>0</b>
  </color>
  <bgcolor mode="nobackground">
   <r>255</r>
   <g>255</g>
   <b>255</b>
  </bgcolor>
  <bordermode>noborder</bordermode>
  <borderradius>1</borderradius>
  <borderwidth>1</borderwidth>
 </bsbObject>
 <bsbObject version="2" type="BSBHSlider">
  <objectName>wfreq2</objectName>
  <x>766</x>
  <y>622</y>
  <width>218</width>
  <height>19</height>
  <uuid>{c2c33c3f-e49e-4e88-afea-433654ebfb7d}</uuid>
  <visible>true</visible>
  <midichan>0</midichan>
  <midicc>0</midicc>
  <minimum>20.00000000</minimum>
  <maximum>1000.00000000</maximum>
  <value>608.89908257</value>
  <mode>lin</mode>
  <mouseControl act="jump">continuous</mouseControl>
  <resolution>-1.00000000</resolution>
  <randomizable group="0">false</randomizable>
 </bsbObject>
 <bsbObject version="2" type="BSBHSlider">
  <objectName>wfreq3</objectName>
  <x>767</x>
  <y>677</y>
  <width>218</width>
  <height>19</height>
  <uuid>{d4e77985-f5ea-4dc7-953c-237c59296dc1}</uuid>
  <visible>true</visible>
  <midichan>0</midichan>
  <midicc>0</midicc>
  <minimum>20.00000000</minimum>
  <maximum>1000.00000000</maximum>
  <value>541.46788991</value>
  <mode>lin</mode>
  <mouseControl act="jump">continuous</mouseControl>
  <resolution>-1.00000000</resolution>
  <randomizable group="0">false</randomizable>
 </bsbObject>
 <bsbObject version="2" type="BSBHSlider">
  <objectName>wfreq4</objectName>
  <x>766</x>
  <y>731</y>
  <width>218</width>
  <height>19</height>
  <uuid>{12c88fe2-7f5e-4459-9448-dc43699f15a3}</uuid>
  <visible>true</visible>
  <midichan>0</midichan>
  <midicc>0</midicc>
  <minimum>20.00000000</minimum>
  <maximum>1000.00000000</maximum>
  <value>505.50458716</value>
  <mode>lin</mode>
  <mouseControl act="jump">continuous</mouseControl>
  <resolution>-1.00000000</resolution>
  <randomizable group="0">false</randomizable>
 </bsbObject>
 <bsbObject version="2" type="BSBHSlider">
  <objectName>wfreq5</objectName>
  <x>766</x>
  <y>782</y>
  <width>218</width>
  <height>19</height>
  <uuid>{b4768e51-9980-4423-a063-8a3d5856e994}</uuid>
  <visible>true</visible>
  <midichan>0</midichan>
  <midicc>0</midicc>
  <minimum>20.00000000</minimum>
  <maximum>1000.00000000</maximum>
  <value>501.00917431</value>
  <mode>lin</mode>
  <mouseControl act="jump">continuous</mouseControl>
  <resolution>-1.00000000</resolution>
  <randomizable group="0">false</randomizable>
 </bsbObject>
 <bsbObject version="2" type="BSBLabel">
  <objectName/>
  <x>985</x>
  <y>675</y>
  <width>43</width>
  <height>23</height>
  <uuid>{cb1c0a7d-411c-4c65-9b49-34215684cf25}</uuid>
  <visible>true</visible>
  <midichan>0</midichan>
  <midicc>0</midicc>
  <label>FREQ3</label>
  <alignment>left</alignment>
  <font>Arial</font>
  <fontsize>10</fontsize>
  <precision>3</precision>
  <color>
   <r>0</r>
   <g>0</g>
   <b>0</b>
  </color>
  <bgcolor mode="nobackground">
   <r>255</r>
   <g>255</g>
   <b>255</b>
  </bgcolor>
  <bordermode>noborder</bordermode>
  <borderradius>1</borderradius>
  <borderwidth>1</borderwidth>
 </bsbObject>
 <bsbObject version="2" type="BSBLabel">
  <objectName/>
  <x>985</x>
  <y>730</y>
  <width>43</width>
  <height>22</height>
  <uuid>{9fe9dc81-f25d-4f0e-ba23-54f8cd6be120}</uuid>
  <visible>true</visible>
  <midichan>0</midichan>
  <midicc>0</midicc>
  <label>FREQ4</label>
  <alignment>left</alignment>
  <font>Arial</font>
  <fontsize>10</fontsize>
  <precision>3</precision>
  <color>
   <r>0</r>
   <g>0</g>
   <b>0</b>
  </color>
  <bgcolor mode="nobackground">
   <r>255</r>
   <g>255</g>
   <b>255</b>
  </bgcolor>
  <bordermode>noborder</bordermode>
  <borderradius>1</borderradius>
  <borderwidth>1</borderwidth>
 </bsbObject>
 <bsbObject version="2" type="BSBLabel">
  <objectName/>
  <x>985</x>
  <y>781</y>
  <width>44</width>
  <height>22</height>
  <uuid>{7f707285-77ea-479d-a002-648982a64118}</uuid>
  <visible>true</visible>
  <midichan>0</midichan>
  <midicc>0</midicc>
  <label>FREQ5</label>
  <alignment>left</alignment>
  <font>Arial</font>
  <fontsize>10</fontsize>
  <precision>3</precision>
  <color>
   <r>0</r>
   <g>0</g>
   <b>0</b>
  </color>
  <bgcolor mode="nobackground">
   <r>255</r>
   <g>255</g>
   <b>255</b>
  </bgcolor>
  <bordermode>noborder</bordermode>
  <borderradius>1</borderradius>
  <borderwidth>1</borderwidth>
 </bsbObject>
 <bsbObject version="2" type="BSBLabel">
  <objectName/>
  <x>985</x>
  <y>568</y>
  <width>41</width>
  <height>22</height>
  <uuid>{47cc82f5-c5ca-4bc0-b6de-caafd45ceebd}</uuid>
  <visible>true</visible>
  <midichan>0</midichan>
  <midicc>0</midicc>
  <label>FREQ1</label>
  <alignment>left</alignment>
  <font>Arial</font>
  <fontsize>10</fontsize>
  <precision>3</precision>
  <color>
   <r>0</r>
   <g>0</g>
   <b>0</b>
  </color>
  <bgcolor mode="nobackground">
   <r>255</r>
   <g>255</g>
   <b>255</b>
  </bgcolor>
  <bordermode>noborder</bordermode>
  <borderradius>1</borderradius>
  <borderwidth>1</borderwidth>
 </bsbObject>
 <bsbObject version="2" type="BSBKnob">
  <objectName>cutoff1</objectName>
  <x>785</x>
  <y>582</y>
  <width>49</width>
  <height>45</height>
  <uuid>{dedf7bdc-151d-40eb-966d-25b166668e1b}</uuid>
  <visible>true</visible>
  <midichan>0</midichan>
  <midicc>0</midicc>
  <minimum>20.00000000</minimum>
  <maximum>10000.00000000</maximum>
  <value>10000.00000000</value>
  <mode>lin</mode>
  <mouseControl act="jump">continuous</mouseControl>
  <resolution>0.01000000</resolution>
  <randomizable group="0">false</randomizable>
 </bsbObject>
 <bsbObject version="2" type="BSBKnob">
  <objectName>wfeed1</objectName>
  <x>894</x>
  <y>582</y>
  <width>49</width>
  <height>45</height>
  <uuid>{c904b3be-37a1-4458-a101-13c9d907c5fc}</uuid>
  <visible>true</visible>
  <midichan>0</midichan>
  <midicc>0</midicc>
  <minimum>0.00000000</minimum>
  <maximum>1.00000000</maximum>
  <value>1.00000000</value>
  <mode>lin</mode>
  <mouseControl act="jump">continuous</mouseControl>
  <resolution>0.01000000</resolution>
  <randomizable group="0">false</randomizable>
 </bsbObject>
 <bsbObject version="2" type="BSBKnob">
  <objectName>cutoff2</objectName>
  <x>784</x>
  <y>636</y>
  <width>49</width>
  <height>45</height>
  <uuid>{92bf32d8-76c0-4a98-bd81-a50abbf73045}</uuid>
  <visible>true</visible>
  <midichan>0</midichan>
  <midicc>0</midicc>
  <minimum>20.00000000</minimum>
  <maximum>10000.00000000</maximum>
  <value>10000.00000000</value>
  <mode>lin</mode>
  <mouseControl act="jump">continuous</mouseControl>
  <resolution>0.01000000</resolution>
  <randomizable group="0">false</randomizable>
 </bsbObject>
 <bsbObject version="2" type="BSBKnob">
  <objectName>wfeed2</objectName>
  <x>894</x>
  <y>637</y>
  <width>49</width>
  <height>45</height>
  <uuid>{0fd9a4d5-95cd-410f-be49-c3e3d771332c}</uuid>
  <visible>true</visible>
  <midichan>0</midichan>
  <midicc>0</midicc>
  <minimum>0.00000000</minimum>
  <maximum>1.00000000</maximum>
  <value>1.00000000</value>
  <mode>lin</mode>
  <mouseControl act="jump">continuous</mouseControl>
  <resolution>0.01000000</resolution>
  <randomizable group="0">false</randomizable>
 </bsbObject>
 <bsbObject version="2" type="BSBKnob">
  <objectName>cutoff3</objectName>
  <x>784</x>
  <y>692</y>
  <width>49</width>
  <height>45</height>
  <uuid>{edbccfc4-030e-4c8e-8742-4d0fb28e4ed5}</uuid>
  <visible>true</visible>
  <midichan>0</midichan>
  <midicc>0</midicc>
  <minimum>20.00000000</minimum>
  <maximum>10000.00000000</maximum>
  <value>9800.40000000</value>
  <mode>lin</mode>
  <mouseControl act="jump">continuous</mouseControl>
  <resolution>0.01000000</resolution>
  <randomizable group="0">false</randomizable>
 </bsbObject>
 <bsbObject version="2" type="BSBKnob">
  <objectName>wfeed3</objectName>
  <x>894</x>
  <y>692</y>
  <width>49</width>
  <height>45</height>
  <uuid>{e6157fe5-5158-40e7-98c9-5214b7b42b91}</uuid>
  <visible>true</visible>
  <midichan>0</midichan>
  <midicc>0</midicc>
  <minimum>0.00000000</minimum>
  <maximum>1.00000000</maximum>
  <value>1.00000000</value>
  <mode>lin</mode>
  <mouseControl act="jump">continuous</mouseControl>
  <resolution>0.01000000</resolution>
  <randomizable group="0">false</randomizable>
 </bsbObject>
 <bsbObject version="2" type="BSBKnob">
  <objectName>cutoff4</objectName>
  <x>783</x>
  <y>744</y>
  <width>49</width>
  <height>45</height>
  <uuid>{fde496f9-4caa-4f87-ad4d-0fa1c2d7954b}</uuid>
  <visible>true</visible>
  <midichan>0</midichan>
  <midicc>0</midicc>
  <minimum>20.00000000</minimum>
  <maximum>10000.00000000</maximum>
  <value>10000.00000000</value>
  <mode>lin</mode>
  <mouseControl act="jump">continuous</mouseControl>
  <resolution>0.01000000</resolution>
  <randomizable group="0">false</randomizable>
 </bsbObject>
 <bsbObject version="2" type="BSBKnob">
  <objectName>wfeed4</objectName>
  <x>894</x>
  <y>744</y>
  <width>49</width>
  <height>45</height>
  <uuid>{d9e9db8d-80ac-4a5a-8e8b-48745d224d5c}</uuid>
  <visible>true</visible>
  <midichan>0</midichan>
  <midicc>0</midicc>
  <minimum>0.00000000</minimum>
  <maximum>1.00000000</maximum>
  <value>1.00000000</value>
  <mode>lin</mode>
  <mouseControl act="jump">continuous</mouseControl>
  <resolution>0.01000000</resolution>
  <randomizable group="0">false</randomizable>
 </bsbObject>
 <bsbObject version="2" type="BSBKnob">
  <objectName>cutoff5</objectName>
  <x>783</x>
  <y>793</y>
  <width>49</width>
  <height>45</height>
  <uuid>{9490bf65-381c-45dd-b4cb-1a0b2dde2338}</uuid>
  <visible>true</visible>
  <midichan>0</midichan>
  <midicc>0</midicc>
  <minimum>20.00000000</minimum>
  <maximum>10000.00000000</maximum>
  <value>9600.80000000</value>
  <mode>lin</mode>
  <mouseControl act="jump">continuous</mouseControl>
  <resolution>0.01000000</resolution>
  <randomizable group="0">false</randomizable>
 </bsbObject>
 <bsbObject version="2" type="BSBKnob">
  <objectName>wfeed5</objectName>
  <x>894</x>
  <y>794</y>
  <width>49</width>
  <height>45</height>
  <uuid>{8b0984e6-619a-4ed6-a4d1-858a77117495}</uuid>
  <visible>true</visible>
  <midichan>0</midichan>
  <midicc>0</midicc>
  <minimum>0.00000000</minimum>
  <maximum>1.00000000</maximum>
  <value>0.99000000</value>
  <mode>lin</mode>
  <mouseControl act="jump">continuous</mouseControl>
  <resolution>0.01000000</resolution>
  <randomizable group="0">false</randomizable>
 </bsbObject>
 <bsbObject version="2" type="BSBLabel">
  <objectName/>
  <x>843</x>
  <y>594</y>
  <width>41</width>
  <height>22</height>
  <uuid>{bf37f35d-c431-4c81-91a7-72164d8f998f}</uuid>
  <visible>true</visible>
  <midichan>0</midichan>
  <midicc>0</midicc>
  <label>CUT1
</label>
  <alignment>left</alignment>
  <font>Arial</font>
  <fontsize>10</fontsize>
  <precision>3</precision>
  <color>
   <r>0</r>
   <g>0</g>
   <b>0</b>
  </color>
  <bgcolor mode="nobackground">
   <r>255</r>
   <g>255</g>
   <b>255</b>
  </bgcolor>
  <bordermode>noborder</bordermode>
  <borderradius>1</borderradius>
  <borderwidth>1</borderwidth>
 </bsbObject>
 <bsbObject version="2" type="BSBLabel">
  <objectName/>
  <x>951</x>
  <y>808</y>
  <width>41</width>
  <height>22</height>
  <uuid>{67c31e16-ea83-4e00-9dcf-7f9609ad4cc1}</uuid>
  <visible>true</visible>
  <midichan>0</midichan>
  <midicc>0</midicc>
  <label>FEED5</label>
  <alignment>left</alignment>
  <font>Arial</font>
  <fontsize>10</fontsize>
  <precision>3</precision>
  <color>
   <r>0</r>
   <g>0</g>
   <b>0</b>
  </color>
  <bgcolor mode="nobackground">
   <r>255</r>
   <g>255</g>
   <b>255</b>
  </bgcolor>
  <bordermode>noborder</bordermode>
  <borderradius>1</borderradius>
  <borderwidth>1</borderwidth>
 </bsbObject>
 <bsbObject version="2" type="BSBLabel">
  <objectName/>
  <x>843</x>
  <y>808</y>
  <width>41</width>
  <height>22</height>
  <uuid>{c3288f27-b93d-41db-bf76-63a312c03c23}</uuid>
  <visible>true</visible>
  <midichan>0</midichan>
  <midicc>0</midicc>
  <label>CUT5</label>
  <alignment>left</alignment>
  <font>Arial</font>
  <fontsize>10</fontsize>
  <precision>3</precision>
  <color>
   <r>0</r>
   <g>0</g>
   <b>0</b>
  </color>
  <bgcolor mode="nobackground">
   <r>255</r>
   <g>255</g>
   <b>255</b>
  </bgcolor>
  <bordermode>noborder</bordermode>
  <borderradius>1</borderradius>
  <borderwidth>1</borderwidth>
 </bsbObject>
 <bsbObject version="2" type="BSBLabel">
  <objectName/>
  <x>949</x>
  <y>755</y>
  <width>41</width>
  <height>22</height>
  <uuid>{51cefbb5-1f7d-4f0b-b5c4-cad50a6e31ea}</uuid>
  <visible>true</visible>
  <midichan>0</midichan>
  <midicc>0</midicc>
  <label>FEED4</label>
  <alignment>left</alignment>
  <font>Arial</font>
  <fontsize>10</fontsize>
  <precision>3</precision>
  <color>
   <r>0</r>
   <g>0</g>
   <b>0</b>
  </color>
  <bgcolor mode="nobackground">
   <r>255</r>
   <g>255</g>
   <b>255</b>
  </bgcolor>
  <bordermode>noborder</bordermode>
  <borderradius>1</borderradius>
  <borderwidth>1</borderwidth>
 </bsbObject>
 <bsbObject version="2" type="BSBLabel">
  <objectName/>
  <x>845</x>
  <y>756</y>
  <width>41</width>
  <height>22</height>
  <uuid>{42c232c7-17d4-413e-b51f-e2ec66cb9b86}</uuid>
  <visible>true</visible>
  <midichan>0</midichan>
  <midicc>0</midicc>
  <label>CUT4</label>
  <alignment>left</alignment>
  <font>Arial</font>
  <fontsize>10</fontsize>
  <precision>3</precision>
  <color>
   <r>0</r>
   <g>0</g>
   <b>0</b>
  </color>
  <bgcolor mode="nobackground">
   <r>255</r>
   <g>255</g>
   <b>255</b>
  </bgcolor>
  <bordermode>noborder</bordermode>
  <borderradius>1</borderradius>
  <borderwidth>1</borderwidth>
 </bsbObject>
 <bsbObject version="2" type="BSBLabel">
  <objectName/>
  <x>951</x>
  <y>702</y>
  <width>41</width>
  <height>22</height>
  <uuid>{1e7c849f-e8cd-4b23-9a6d-72371f781664}</uuid>
  <visible>true</visible>
  <midichan>0</midichan>
  <midicc>0</midicc>
  <label>FEED3</label>
  <alignment>left</alignment>
  <font>Arial</font>
  <fontsize>10</fontsize>
  <precision>3</precision>
  <color>
   <r>0</r>
   <g>0</g>
   <b>0</b>
  </color>
  <bgcolor mode="nobackground">
   <r>255</r>
   <g>255</g>
   <b>255</b>
  </bgcolor>
  <bordermode>noborder</bordermode>
  <borderradius>1</borderradius>
  <borderwidth>1</borderwidth>
 </bsbObject>
 <bsbObject version="2" type="BSBLabel">
  <objectName/>
  <x>844</x>
  <y>702</y>
  <width>41</width>
  <height>22</height>
  <uuid>{157fff1b-eaf1-4f82-ac18-4799af33e198}</uuid>
  <visible>true</visible>
  <midichan>0</midichan>
  <midicc>0</midicc>
  <label>CUT3</label>
  <alignment>left</alignment>
  <font>Arial</font>
  <fontsize>10</fontsize>
  <precision>3</precision>
  <color>
   <r>0</r>
   <g>0</g>
   <b>0</b>
  </color>
  <bgcolor mode="nobackground">
   <r>255</r>
   <g>255</g>
   <b>255</b>
  </bgcolor>
  <bordermode>noborder</bordermode>
  <borderradius>1</borderradius>
  <borderwidth>1</borderwidth>
 </bsbObject>
 <bsbObject version="2" type="BSBLabel">
  <objectName/>
  <x>950</x>
  <y>649</y>
  <width>41</width>
  <height>22</height>
  <uuid>{5b51054c-97bd-465d-a2e5-88b502b3e085}</uuid>
  <visible>true</visible>
  <midichan>0</midichan>
  <midicc>0</midicc>
  <label>FEED2</label>
  <alignment>left</alignment>
  <font>Arial</font>
  <fontsize>10</fontsize>
  <precision>3</precision>
  <color>
   <r>0</r>
   <g>0</g>
   <b>0</b>
  </color>
  <bgcolor mode="nobackground">
   <r>255</r>
   <g>255</g>
   <b>255</b>
  </bgcolor>
  <bordermode>noborder</bordermode>
  <borderradius>1</borderradius>
  <borderwidth>1</borderwidth>
 </bsbObject>
 <bsbObject version="2" type="BSBLabel">
  <objectName/>
  <x>845</x>
  <y>650</y>
  <width>41</width>
  <height>22</height>
  <uuid>{8dd69c0c-e27c-4c46-bffa-6227a91d7e49}</uuid>
  <visible>true</visible>
  <midichan>0</midichan>
  <midicc>0</midicc>
  <label>CUT2</label>
  <alignment>left</alignment>
  <font>Arial</font>
  <fontsize>10</fontsize>
  <precision>3</precision>
  <color>
   <r>0</r>
   <g>0</g>
   <b>0</b>
  </color>
  <bgcolor mode="nobackground">
   <r>255</r>
   <g>255</g>
   <b>255</b>
  </bgcolor>
  <bordermode>noborder</bordermode>
  <borderradius>1</borderradius>
  <borderwidth>1</borderwidth>
 </bsbObject>
 <bsbObject version="2" type="BSBLabel">
  <objectName/>
  <x>948</x>
  <y>593</y>
  <width>41</width>
  <height>22</height>
  <uuid>{12fc60b2-8051-447e-9fb6-d4191e609c1c}</uuid>
  <visible>true</visible>
  <midichan>0</midichan>
  <midicc>0</midicc>
  <label>FEED1</label>
  <alignment>left</alignment>
  <font>Arial</font>
  <fontsize>10</fontsize>
  <precision>3</precision>
  <color>
   <r>0</r>
   <g>0</g>
   <b>0</b>
  </color>
  <bgcolor mode="nobackground">
   <r>255</r>
   <g>255</g>
   <b>255</b>
  </bgcolor>
  <bordermode>noborder</bordermode>
  <borderradius>1</borderradius>
  <borderwidth>1</borderwidth>
 </bsbObject>
 <bsbObject version="2" type="BSBKnob">
  <objectName>ampi</objectName>
  <x>890</x>
  <y>543</y>
  <width>37</width>
  <height>33</height>
  <uuid>{8b5d9a61-abb8-4d45-a662-8a386ed332e9}</uuid>
  <visible>true</visible>
  <midichan>0</midichan>
  <midicc>0</midicc>
  <minimum>0.00000000</minimum>
  <maximum>1.00000000</maximum>
  <value>0.89000000</value>
  <mode>lin</mode>
  <mouseControl act="jump">continuous</mouseControl>
  <resolution>0.01000000</resolution>
  <randomizable group="0">false</randomizable>
 </bsbObject>
 <bsbObject version="2" type="BSBKnob">
  <objectName>intv</objectName>
  <x>955</x>
  <y>543</y>
  <width>37</width>
  <height>33</height>
  <uuid>{cbd2df87-dbf5-4103-9849-260502895469}</uuid>
  <visible>true</visible>
  <midichan>0</midichan>
  <midicc>0</midicc>
  <minimum>0.00000000</minimum>
  <maximum>2.00000000</maximum>
  <value>0.24000000</value>
  <mode>lin</mode>
  <mouseControl act="jump">continuous</mouseControl>
  <resolution>0.01000000</resolution>
  <randomizable group="0">false</randomizable>
 </bsbObject>
 <bsbObject version="2" type="BSBVSlider">
  <objectName>freqfib</objectName>
  <x>1391</x>
  <y>761</y>
  <width>13</width>
  <height>75</height>
  <uuid>{873d1d63-92c6-485a-af54-a2c071a044af}</uuid>
  <visible>true</visible>
  <midichan>0</midichan>
  <midicc>0</midicc>
  <minimum>0.00000000</minimum>
  <maximum>10000.00000000</maximum>
  <value>10000.00000000</value>
  <mode>lin</mode>
  <mouseControl act="jump">continuous</mouseControl>
  <resolution>-1.00000000</resolution>
  <randomizable group="0">false</randomizable>
 </bsbObject>
 <bsbObject version="2" type="BSBScrollNumber">
  <objectName>freqfib</objectName>
  <x>1382</x>
  <y>744</y>
  <width>32</width>
  <height>17</height>
  <uuid>{186003d8-b351-4e1b-a37f-e4ffa57a0701}</uuid>
  <visible>true</visible>
  <midichan>0</midichan>
  <midicc>0</midicc>
  <alignment>left</alignment>
  <font>Arial</font>
  <fontsize>10</fontsize>
  <color>
   <r>255</r>
   <g>255</g>
   <b>255</b>
  </color>
  <bgcolor mode="background">
   <r>0</r>
   <g>0</g>
   <b>0</b>
  </bgcolor>
  <value>10000.00000000</value>
  <resolution>1.00000000</resolution>
  <minimum>-999999999999.00000000</minimum>
  <maximum>999999999999.00000000</maximum>
  <bordermode>noborder</bordermode>
  <borderradius>1</borderradius>
  <borderwidth>1</borderwidth>
  <randomizable group="0">false</randomizable>
  <mouseControl act=""/>
 </bsbObject>
 <bsbObject version="2" type="BSBLabel">
  <objectName/>
  <x>1367</x>
  <y>726</y>
  <width>56</width>
  <height>19</height>
  <uuid>{8c55cf38-e7b1-4804-b411-11b26107194a}</uuid>
  <visible>true</visible>
  <midichan>0</midichan>
  <midicc>0</midicc>
  <label>FIB_FREQ
</label>
  <alignment>left</alignment>
  <font>Arial</font>
  <fontsize>8</fontsize>
  <precision>3</precision>
  <color>
   <r>255</r>
   <g>255</g>
   <b>255</b>
  </color>
  <bgcolor mode="background">
   <r>0</r>
   <g>0</g>
   <b>0</b>
  </bgcolor>
  <bordermode>noborder</bordermode>
  <borderradius>1</borderradius>
  <borderwidth>1</borderwidth>
 </bsbObject>
</bsbPanel>
<bsbPresets>
</bsbPresets>
<MacOptions>
Version: 3
Render: Real
Ask: Yes
Functions: ioObject
Listing: Window
WindowBounds: 2 38 1432 852
CurrentView: io
IOViewEdit: On
Options:
</MacOptions>

<MacGUI>
ioView background {0, 0, 0}
ioText {2, 6} {1031, 837} label 0.000000 0.00100 "" left "Arial" 12 {65280, 65280, 65280} {0, 0, 32768} nobackground noborder 
ioText {513, 9} {364, 268} label 0.000000 0.00100 "" left "Arial" 12 {65280, 65280, 65280} {0, 0, 0} nobackground noborder 
ioText {667, 275} {363, 267} label 0.000000 0.00100 "" left "Arial" 12 {65280, 65280, 65280} {0, 0, 0} nobackground noborder 
ioText {270, 511} {491, 327} label 0.000000 0.00100 "" left "Arial" 12 {65280, 65280, 65280} {0, 0, 0} nobackground noborder 
ioText {148, 9} {366, 268} label 0.000000 0.00100 "" left "Arial" 12 {65280, 65280, 65280} {0, 0, 0} nobackground noborder 
ioText {1, 276} {363, 267} label 0.000000 0.00100 "" left "Arial" 12 {65280, 65280, 65280} {0, 0, 0} nobackground noborder 
ioText {1034, 6} {390, 79} label 0.000000 0.00100 "" center "Apple Chancery" 28 {65280, 65280, 65280} {65280, 65280, 65280} nobackground noborder PENTAGRANULATOR ver 0.2Â¬by Anthony Di Furia
ioText {1034, 86} {390, 515} label 0.000000 0.00100 "" center "Arial" 27 {52224, 52224, 52224} {26112, 26112, 26112} nobackground noborder MIXER
ioText {1065, 410} {51, 21} label 0.000000 0.00100 "" left "Arial" 12 {52224, 52224, 52224} {65280, 65280, 65280} nobackground noborder GRAN 1
ioText {1270, 410} {50, 22} label 0.000000 0.00100 "" left "Arial" 12 {52224, 52224, 52224} {65280, 65280, 65280} nobackground noborder GRAN 5
ioText {1167, 410} {51, 21} label 0.000000 0.00100 "" left "Arial" 12 {52224, 52224, 52224} {65280, 65280, 65280} nobackground noborder GRAN 3
ioText {1219, 410} {50, 22} label 0.000000 0.00100 "" left "Arial" 12 {52224, 52224, 52224} {65280, 65280, 65280} nobackground noborder GRAN 4
ioText {1117, 410} {49, 21} label 0.000000 0.00100 "" left "Arial" 12 {52224, 52224, 52224} {65280, 65280, 65280} nobackground noborder GRAN 2
ioButton {267, 12} {137, 26} event 1.000000 "" "GRANULATOR 1" "/" i15 0 -1
ioButton {386, 515} {138, 28} event 1.000000 "" "GRANULATOR 5" "/" i30 0 -10
ioButton {787, 281} {137, 26} event 1.000000 "" "GRANULATOR 4" "/" i25 0 -10
ioButton {116, 282} {137, 26} event 1.000000 "" "GRANULATOR 3" "/" i20 0 -10
ioButton {632, 9} {138, 28} event 1.000000 "" "GRANULATOR 2" "/" i17 0 -10
ioText {1034, 602} {390, 77} label 0.000000 0.00100 "" center "Arial" 12 {26112, 26112, 26112} {52224, 52224, 52224} nobackground noborder REVERBÂ¬Â¬
ioText {174, 40} {38, 22} label 0.000000 0.00100 "" left "Arial Black" 12 {52224, 52224, 52224} {65280, 65280, 65280} nobackground noborder DENS
ioText {214, 40} {35, 22} label 0.000000 0.00100 "" center "Arial Black" 12 {52224, 52224, 52224} {65280, 65280, 65280} nobackground noborder DUR
ioText {251, 40} {36, 22} label 0.000000 0.00100 "" center "Arial Black" 12 {52224, 52224, 52224} {65280, 65280, 65280} nobackground noborder CPSÂ¬
ioText {289, 40} {38, 22} label 0.000000 0.00100 "" center "Arial Black" 12 {52224, 52224, 52224} {65280, 65280, 65280} nobackground noborder VELÂ¬
ioText {329, 40} {36, 22} label 0.000000 0.00100 "" center "Arial Black" 12 {52224, 52224, 52224} {65280, 65280, 65280} nobackground noborder FMD
ioText {366, 40} {39, 22} label 0.000000 0.00100 "" center "Arial Black" 12 {52224, 52224, 52224} {65280, 65280, 65280} nobackground noborder PMDÂ¬
ioText {401, 40} {46, 22} label 0.000000 0.00100 "" center "Arial Black" 12 {52224, 52224, 52224} {65280, 65280, 65280} nobackground noborder FRPOW
ioText {442, 40} {48, 22} label 0.000000 0.00100 "" center "Arial Black" 12 {52224, 52224, 52224} {65280, 65280, 65280} nobackground noborder PRPOW
ioSlider {181, 86} {21, 131} 1.000000 200.000000 1.000000 dens1
ioSlider {454, 86} {21, 131} 0.000000 1.000000 1.000000 prpow1
ioSlider {415, 86} {21, 131} 0.000000 1.000000 1.000000 frpow1
ioSlider {375, 86} {21, 131} 0.000000 1.000000 1.000000 pmd1
ioSlider {336, 86} {21, 131} 0.000000 1.000000 0.167939 fmd1
ioSlider {296, 86} {21, 131} 0.000000 1.000000 1.000000 vel1
ioSlider {259, 86} {21, 131} 0.032500 6.000000 0.032500 cps1
ioSlider {220, 86} {21, 131} 0.010000 0.500000 0.062366 dur1
ioText {174, 62} {38, 24} scroll 1.000000 1.000000 "dens1" center "Arial" 12 {32768, 32768, 32768} {0, 0, 0} background noborder 1.00000000
ioText {250, 62} {38, 24} scroll 0.032500 0.001000 "cps1" left "Arial" 12 {32768, 32768, 32768} {0, 0, 0} background noborder 
ioText {289, 62} {38, 24} scroll 1.000000 0.001000 "vel1" left "Arial" 12 {32768, 32768, 32768} {0, 0, 0} background noborder 
ioText {328, 62} {38, 24} scroll 0.167939 0.001000 "fmd1" left "Arial" 12 {32768, 32768, 32768} {0, 0, 0} background noborder 
ioText {367, 62} {38, 24} scroll 1.000000 0.001000 "pmd1" left "Arial" 12 {32768, 32768, 32768} {0, 0, 0} background noborder 
ioText {406, 62} {38, 24} scroll 1.000000 0.001000 "frpow1" left "Arial" 12 {32768, 32768, 32768} {0, 0, 0} background noborder 
ioText {446, 62} {38, 24} scroll 1.000000 0.001000 "prpow1" left "Arial" 12 {32768, 32768, 32768} {0, 0, 0} background noborder 
ioText {211, 62} {38, 24} scroll 0.062366 0.001000 "dur1" left "Arial" 12 {32768, 32768, 32768} {0, 0, 0} background noborder 
ioText {538, 36} {39, 24} label 0.000000 0.00100 "" left "Arial Black" 12 {52224, 52224, 52224} {65280, 65280, 65280} nobackground noborder DENS
ioText {578, 36} {36, 24} label 0.000000 0.00100 "" center "Arial Black" 12 {52224, 52224, 52224} {65280, 65280, 65280} nobackground noborder DUR
ioText {615, 36} {37, 24} label 0.000000 0.00100 "" center "Arial Black" 12 {52224, 52224, 52224} {65280, 65280, 65280} nobackground noborder CPSÂ¬
ioText {653, 36} {39, 24} label 0.000000 0.00100 "" center "Arial Black" 12 {52224, 52224, 52224} {65280, 65280, 65280} nobackground noborder VELÂ¬
ioText {693, 36} {37, 24} label 0.000000 0.00100 "" center "Arial Black" 12 {52224, 52224, 52224} {65280, 65280, 65280} nobackground noborder FMD
ioText {730, 36} {40, 24} label 0.000000 0.00100 "" center "Arial Black" 12 {52224, 52224, 52224} {65280, 65280, 65280} nobackground noborder PMDÂ¬
ioText {765, 36} {47, 24} label 0.000000 0.00100 "" center "Arial Black" 12 {52224, 52224, 52224} {65280, 65280, 65280} nobackground noborder FRPOW
ioText {806, 36} {49, 24} label 0.000000 0.00100 "" center "Arial Black" 12 {52224, 52224, 52224} {65280, 65280, 65280} nobackground noborder PRPOW
ioSlider {544, 81} {22, 133} 0.010000 200.000000 105.267895 dens2
ioSlider {817, 81} {22, 133} 0.000000 1.000000 0.977444 prpow2
ioSlider {778, 81} {22, 133} 0.000000 1.000000 1.000000 frpow2
ioSlider {738, 81} {22, 133} 0.000000 1.000000 1.000000 pmd2
ioSlider {699, 81} {22, 133} 0.000000 1.000000 0.120301 fmd2
ioSlider {659, 81} {22, 133} 0.000000 1.000000 1.000000 vel2
ioSlider {622, 81} {22, 133} 0.032500 6.000000 6.000000 cps2
ioSlider {583, 81} {22, 133} 0.010000 0.500000 0.010000 dur2
ioText {538, 58} {39, 26} scroll 105.267895 0.001000 "dens2" left "Arial" 12 {32768, 32768, 32768} {0, 0, 0} background noborder 
ioText {614, 58} {39, 26} scroll 6.000000 0.001000 "cps2" left "Arial" 12 {32768, 32768, 32768} {0, 0, 0} background noborder 
ioText {653, 58} {39, 26} scroll 1.000000 0.001000 "vel2" left "Arial" 12 {32768, 32768, 32768} {0, 0, 0} background noborder 
ioText {692, 58} {39, 26} scroll 0.120301 0.001000 "fmd2" left "Arial" 12 {32768, 32768, 32768} {0, 0, 0} background noborder 
ioText {731, 58} {39, 26} scroll 1.000000 0.001000 "pmd2" left "Arial" 12 {32768, 32768, 32768} {0, 0, 0} background noborder 
ioText {770, 58} {39, 26} scroll 0.000000 0.001000 "" left "Arial" 12 {32768, 32768, 32768} {0, 0, 0} background noborder 
ioText {810, 58} {39, 26} scroll 0.000000 0.001000 "" left "Arial" 12 {32768, 32768, 32768} {0, 0, 0} background noborder 
ioText {575, 58} {39, 26} scroll 0.010000 0.001000 "dur2" left "Arial" 12 {32768, 32768, 32768} {0, 0, 0} background noborder 
ioText {25, 308} {38, 22} label 0.000000 0.00100 "" left "Arial Black" 12 {52224, 52224, 52224} {65280, 65280, 65280} nobackground noborder DENS
ioText {64, 308} {35, 22} label 0.000000 0.00100 "" center "Arial Black" 12 {52224, 52224, 52224} {65280, 65280, 65280} nobackground noborder DUR
ioText {101, 308} {36, 22} label 0.000000 0.00100 "" center "Arial Black" 12 {52224, 52224, 52224} {65280, 65280, 65280} nobackground noborder CPSÂ¬
ioText {139, 308} {38, 22} label 0.000000 0.00100 "" center "Arial Black" 12 {52224, 52224, 52224} {65280, 65280, 65280} nobackground noborder VELÂ¬
ioText {179, 308} {36, 22} label 0.000000 0.00100 "" center "Arial Black" 12 {52224, 52224, 52224} {65280, 65280, 65280} nobackground noborder FMD
ioText {216, 308} {39, 22} label 0.000000 0.00100 "" center "Arial Black" 12 {52224, 52224, 52224} {65280, 65280, 65280} nobackground noborder PMDÂ¬
ioText {251, 308} {46, 22} label 0.000000 0.00100 "" center "Arial Black" 12 {52224, 52224, 52224} {65280, 65280, 65280} nobackground noborder FRPOW
ioText {292, 308} {48, 22} label 0.000000 0.00100 "" center "Arial Black" 12 {52224, 52224, 52224} {65280, 65280, 65280} nobackground noborder PRPOW
ioSlider {31, 352} {21, 131} 0.010000 200.000000 80.921985 dens3
ioSlider {304, 352} {21, 131} 0.000000 1.000000 1.000000 prpow3
ioSlider {265, 352} {21, 131} 0.000000 1.000000 0.992366 frpow3
ioSlider {225, 352} {21, 131} 0.000000 1.000000 1.000000 pmd3
ioSlider {186, 352} {21, 131} 0.000000 1.000000 0.122137 fmd3
ioSlider {146, 352} {21, 131} 0.000000 1.000000 1.000000 vel3
ioSlider {109, 352} {21, 131} 0.032500 6.000000 0.032500 cps3
ioSlider {70, 352} {21, 131} 0.010000 0.500000 0.500000 dur3
ioText {24, 330} {38, 24} scroll 80.921985 0.001000 "dens3" left "Arial" 12 {32768, 32768, 32768} {0, 0, 0} background noborder 
ioText {100, 330} {38, 24} scroll 0.032500 0.001000 "cps3" left "Arial" 12 {32768, 32768, 32768} {0, 0, 0} background noborder 
ioText {139, 330} {38, 24} scroll 1.000000 0.001000 "vel3" left "Arial" 12 {32768, 32768, 32768} {0, 0, 0} background noborder 
ioText {178, 330} {38, 24} scroll 0.122137 0.001000 "fmd3" left "Arial" 12 {32768, 32768, 32768} {0, 0, 0} background noborder 
ioText {217, 330} {38, 24} scroll 1.000000 0.001000 "pmd3" left "Arial" 12 {32768, 32768, 32768} {0, 0, 0} background noborder 
ioText {256, 330} {38, 24} scroll 0.000000 0.001000 "" left "Arial" 12 {32768, 32768, 32768} {0, 0, 0} background noborder 
ioText {296, 330} {38, 24} scroll 0.000000 0.001000 "" left "Arial" 12 {32768, 32768, 32768} {0, 0, 0} background noborder 
ioText {61, 330} {38, 24} scroll 0.500000 0.001000 "dur3" left "Arial" 12 {32768, 32768, 32768} {0, 0, 0} background noborder 
ioText {690, 307} {38, 22} label 0.000000 0.00100 "" left "Arial Black" 12 {52224, 52224, 52224} {65280, 65280, 65280} nobackground noborder DENS
ioText {730, 307} {35, 22} label 0.000000 0.00100 "" center "Arial Black" 12 {52224, 52224, 52224} {65280, 65280, 65280} nobackground noborder DUR
ioText {767, 307} {36, 22} label 0.000000 0.00100 "" center "Arial Black" 12 {52224, 52224, 52224} {65280, 65280, 65280} nobackground noborder CPSÂ¬
ioText {805, 307} {38, 22} label 0.000000 0.00100 "" center "Arial Black" 12 {52224, 52224, 52224} {65280, 65280, 65280} nobackground noborder VELÂ¬
ioText {845, 307} {36, 22} label 0.000000 0.00100 "" center "Arial Black" 12 {52224, 52224, 52224} {65280, 65280, 65280} nobackground noborder FMD
ioText {882, 307} {39, 22} label 0.000000 0.00100 "" center "Arial Black" 12 {52224, 52224, 52224} {65280, 65280, 65280} nobackground noborder PMDÂ¬
ioText {917, 306} {46, 22} label 0.000000 0.00100 "" center "Arial Black" 12 {52224, 52224, 52224} {65280, 65280, 65280} nobackground noborder FRPOW
ioText {958, 306} {48, 22} label 0.000000 0.00100 "" center "Arial Black" 12 {52224, 52224, 52224} {65280, 65280, 65280} nobackground noborder PRPOW
ioSlider {697, 352} {21, 131} 0.010000 200.000000 67.182214 dens4
ioSlider {970, 352} {21, 131} 0.000000 1.000000 1.000000 prpow4
ioSlider {931, 352} {21, 131} 0.000000 1.000000 1.000000 frpow4
ioSlider {891, 352} {21, 131} 0.000000 1.000000 1.000000 pmd4
ioSlider {852, 352} {21, 131} 0.000000 1.000000 0.122137 fmd4
ioSlider {812, 352} {21, 131} 0.000000 1.000000 1.000000 vel4
ioSlider {776, 352} {21, 131} 0.032500 6.000000 6.000000 cps4
ioSlider {736, 352} {21, 131} 0.010000 0.500000 0.500000 dur4
ioText {690, 329} {38, 24} scroll 67.182214 0.001000 "dens4" left "Arial" 12 {32768, 32768, 32768} {0, 0, 0} background noborder 
ioText {766, 329} {38, 24} scroll 6.000000 0.001000 "cps4" left "Arial" 12 {32768, 32768, 32768} {0, 0, 0} background noborder 
ioText {805, 329} {38, 24} scroll 1.000000 0.001000 "vel4" left "Arial" 12 {32768, 32768, 32768} {0, 0, 0} background noborder 
ioText {844, 329} {38, 24} scroll 0.122137 0.001000 "fmd4" left "Arial" 12 {32768, 32768, 32768} {0, 0, 0} background noborder 
ioText {883, 329} {38, 24} scroll 1.000000 0.001000 "pmd4" left "Arial" 12 {32768, 32768, 32768} {0, 0, 0} background noborder 
ioText {922, 329} {38, 24} scroll 0.000000 0.001000 "" left "Arial" 12 {32768, 32768, 32768} {0, 0, 0} background noborder 
ioText {962, 328} {38, 24} scroll 0.000000 0.001000 "" left "Arial" 12 {32768, 32768, 32768} {0, 0, 0} background noborder 
ioText {727, 329} {38, 24} scroll 0.500000 0.001000 "dur4" left "Arial" 12 {32768, 32768, 32768} {0, 0, 0} background noborder 
ioText {291, 541} {39, 24} label 0.000000 0.00100 "" left "Arial Black" 12 {52224, 52224, 52224} {65280, 65280, 65280} nobackground noborder DENS
ioText {331, 541} {36, 24} label 0.000000 0.00100 "" center "Arial Black" 12 {52224, 52224, 52224} {65280, 65280, 65280} nobackground noborder DUR
ioText {368, 541} {37, 24} label 0.000000 0.00100 "" center "Arial Black" 12 {52224, 52224, 52224} {65280, 65280, 65280} nobackground noborder CPSÂ¬
ioText {406, 541} {39, 24} label 0.000000 0.00100 "" center "Arial Black" 12 {52224, 52224, 52224} {65280, 65280, 65280} nobackground noborder VELÂ¬
ioText {446, 541} {37, 24} label 0.000000 0.00100 "" center "Arial Black" 12 {52224, 52224, 52224} {65280, 65280, 65280} nobackground noborder FMD
ioText {483, 541} {40, 24} label 0.000000 0.00100 "" center "Arial Black" 12 {52224, 52224, 52224} {65280, 65280, 65280} nobackground noborder PMDÂ¬
ioText {518, 541} {47, 24} label 0.000000 0.00100 "" center "Arial Black" 12 {52224, 52224, 52224} {65280, 65280, 65280} nobackground noborder FRPOW
ioText {559, 541} {49, 24} label 0.000000 0.00100 "" center "Arial Black" 12 {52224, 52224, 52224} {65280, 65280, 65280} nobackground noborder PRPOW
ioSlider {298, 587} {22, 133} 1.000000 50.000000 50.000000 dens5
ioSlider {571, 587} {22, 133} 0.000000 1.000000 1.000000 prpow5
ioSlider {532, 587} {22, 133} 0.000000 1.000000 1.000000 frpow5
ioSlider {492, 587} {22, 133} 0.000000 1.000000 0.000000 pmd5
ioSlider {453, 587} {22, 133} 0.000000 1.000000 0.000000 fmd5
ioSlider {413, 587} {22, 133} 0.000000 1.000000 1.000000 vel5
ioSlider {376, 587} {22, 133} 0.032500 6.000000 0.032500 cps5
ioSlider {337, 587} {22, 133} 0.010000 0.500000 0.500000 dur5
ioText {291, 563} {39, 26} scroll 50.000000 0.001000 "dens5" left "Arial" 12 {32768, 32768, 32768} {0, 0, 0} background noborder 
ioText {367, 563} {39, 26} scroll 0.032500 0.001000 "cps5" left "Arial" 12 {32768, 32768, 32768} {0, 0, 0} background noborder 
ioText {406, 563} {39, 26} scroll 0.000000 0.001000 "vel5" left "Arial" 12 {32768, 32768, 32768} {0, 0, 0} background noborder 
ioText {445, 563} {39, 26} scroll 0.000000 0.001000 "fmd5" left "Arial" 12 {32768, 32768, 32768} {0, 0, 0} background noborder 
ioText {484, 563} {39, 26} scroll 0.000000 0.001000 "pmd5" left "Arial" 12 {32768, 32768, 32768} {0, 0, 0} background noborder 
ioText {523, 563} {39, 26} scroll 1.000000 0.001000 "frpow5" left "Arial" 12 {32768, 32768, 32768} {0, 0, 0} background noborder 
ioText {563, 563} {39, 26} scroll 1.000000 0.001000 "frpow5" left "Arial" 12 {32768, 32768, 32768} {0, 0, 0} background noborder 
ioText {328, 563} {39, 26} scroll 0.500000 0.001000 "dur5" left "Arial" 12 {32768, 32768, 32768} {0, 0, 0} background noborder 
ioSlider {1082, 470} {20, 100} 0.000000 1.000000 1.000000 gres1
ioSlider {1287, 471} {20, 100} 0.000000 1.000000 1.000000 gres5
ioSlider {1237, 471} {20, 100} 0.000000 1.000000 1.000000 gres4
ioSlider {1184, 470} {20, 100} 0.000000 1.000000 1.000000 gres3
ioSlider {1134, 470} {20, 100} 0.000000 1.000000 1.000000 gres2
ioKnob {1065, 431} {52, 38} 1.000000 0.000000 0.010000 -0.524590 pan1
ioKnob {1116, 431} {52, 38} 1.000000 0.000000 0.010000 0.841555 pan2
ioKnob {1167, 431} {52, 38} 1.000000 0.000000 0.010000 0.529804 pan3
ioKnob {1218, 431} {52, 38} 1.000000 0.000000 0.010000 -0.838225 pan4
ioKnob {1270, 431} {52, 38} 1.000000 0.000000 0.010000 0.505051 pan5
ioText {1045, 440} {33, 21} label 0.000000 0.00100 "" left "Arial" 12 {52224, 52224, 52224} {65280, 65280, 65280} nobackground noborder PAN
ioText {1049, 572} {34, 23} label 0.000000 0.00100 "" left "Arial" 12 {52224, 52224, 52224} {65280, 65280, 65280} nobackground noborder OUT
ioSlider {1079, 572} {241, 13} 0.000000 1.000000 1.000000 outmaster
ioMeter {1079, 584} {241, 11} {0, 59904, 0} "vert155" 0.000000 "hor155" 0.000000 fill 1 0 mouse
ioSlider {1082, 149} {241, 13} 0.000000 1.000000 1.000000 Lin
ioMeter {1082, 161} {241, 11} {0, 59904, 0} "disp_rmsL" 0.005720 "disp_rmsL" 0.005720 fill 1 0 mouse
ioSlider {1082, 183} {241, 13} 0.000000 1.000000 1.000000 Rin
ioMeter {1082, 172} {241, 11} {0, 59904, 0} "disp_rmsR" 0.001041 "disp_rmsR" 0.001041 fill 1 0 mouse
ioText {1049, 149} {34, 23} label 0.000000 0.00100 "" left "Arial" 12 {52224, 52224, 52224} {65280, 65280, 65280} nobackground noborder in L
ioText {1049, 173} {34, 23} label 0.000000 0.00100 "" left "Arial" 12 {52224, 52224, 52224} {65280, 65280, 65280} nobackground noborder in R
ioButton {1126, 229} {48, 24} value 1.000000 "_Browse1" "Gr 1" "/" 
ioText {1175, 229} {238, 23} edit 0.000000 0.00100 "_Browse1"  "Lucida Grande" 12 {19456, 19456, 19456} {65280, 65280, 65280} falsenoborder composizione elettroacustica 2/fiume_01.wav
ioText {1174, 346} {239, 22} edit 0.000000 0.00100 "_Browse4"  "Lucida Grande" 12 {19456, 19456, 19456} {65280, 65280, 65280} falsenoborder composizione elettroacustica 2/fiume_01.wav
ioText {1175, 309} {239, 23} edit 0.000000 0.00100 "_Browse3"  "Lucida Grande" 12 {19456, 19456, 19456} {65280, 65280, 65280} falsenoborder composizione elettroacustica 2/fiume_01.wav
ioText {1174, 271} {240, 23} edit 0.000000 0.00100 "_Browse2"  "Lucida Grande" 12 {19456, 19456, 19456} {65280, 65280, 65280} falsenoborder composizione elettroacustica 2/fiume_01.wav
ioButton {1041, 229} {49, 24} event 1.000000 "" "Play" "/" i 5 0 9999
ioButton {1141, 123} {79, 28} event 1.000000 "" "LIVE IN" "" i3 0 -10
ioText {1034, 402} {389, 8} label 0.000000 0.00100 "" left "Arial" 12 {0, 0, 0} {52224, 52224, 52224} nobackground noborder 
ioText {1035, 121} {389, 7} label 0.000000 0.00100 "" left "Arial" 12 {0, 0, 0} {52224, 52224, 52224} nobackground noborder 
ioText {875, 10} {153, 266} label 0.000000 0.00100 "" center "Arial Black" 12 {0, 0, 0} {32768, 0, 0} nobackground noborder 
ioText {5, 10} {146, 266} label 0.000000 0.00100 "" center "Arial Black" 12 {0, 0, 0} {32768, 0, 0} nobackground noborder 
ioText {786, 218} {39, 24} label 0.000000 0.00100 "" center "Arial Black" 12 {52224, 52224, 52224} {65280, 65280, 65280} nobackground noborder PHSÂ¬
ioSlider {554, 220} {197, 20} 0.000000 1.000000 0.492386 slider62
ioText {750, 217} {39, 26} scroll 0.000000 0.001000 "" left "Arial" 12 {32768, 32768, 32768} {0, 0, 0} background noborder 
ioText {941, 488} {38, 22} label 0.000000 0.00100 "" center "Arial Black" 12 {52224, 52224, 52224} {65280, 65280, 65280} nobackground noborder PHSÂ¬
ioSlider {708, 490} {196, 18} 0.000000 1.000000 0.492386 slider62
ioText {904, 487} {38, 24} scroll 0.000000 0.001000 "" left "Arial" 12 {32768, 32768, 32768} {0, 0, 0} background noborder 
ioText {425, 220} {38, 22} label 0.000000 0.00100 "" center "Arial Black" 12 {52224, 52224, 52224} {65280, 65280, 65280} nobackground noborder PHSÂ¬
ioSlider {192, 223} {196, 18} 0.000000 1.000000 0.538710 
ioText {388, 219} {38, 24} scroll 0.000000 0.001000 "" left "Arial" 12 {32768, 32768, 32768} {0, 0, 0} background noborder 
ioText {278, 488} {38, 22} label 0.000000 0.00100 "" center "Arial Black" 12 {52224, 52224, 52224} {65280, 65280, 65280} nobackground noborder PHSÂ¬
ioSlider {43, 491} {196, 18} 0.000000 1.000000 0.492386 slider62
ioText {240, 488} {38, 24} scroll 0.000000 0.001000 "" left "Arial" 12 {32768, 32768, 32768} {0, 0, 0} background noborder 
ioText {541, 724} {39, 24} label 0.000000 0.00100 "" center "Arial Black" 12 {52224, 52224, 52224} {65280, 65280, 65280} nobackground noborder PHSÂ¬
ioSlider {310, 725} {197, 20} 0.000000 1.000000 0.492386 slider62
ioText {506, 721} {39, 26} scroll 0.000000 0.001000 "" left "Arial" 12 {32768, 32768, 32768} {0, 0, 0} background noborder 
ioText {7, 543} {263, 294} label 0.000000 0.00100 "" left "Arial" 12 {0, 0, 0} {32768, 0, 0} nobackground noborder 
ioText {363, 275} {305, 239} label 0.000000 0.00100 "" left "Arial" 12 {0, 0, 0} {32768, 0, 0} nobackground noborder 
ioMeter {402, 282} {225, 225} {45824, 45824, 45824} "y" 0.098629 "x" 0.978694 point 15 0 mouse
ioMeter {15, 550} {223, 223} {19456, 19456, 19456} "y_L" 0.591928 "x_L" 0.565022 point 15 0 mouse
ioText {905, 63} {41, 22} label 0.000000 0.00100 "" center "Arial Black" 12 {0, 0, 0} {65280, 65280, 65280} nobackground noborder DENS
ioText {960, 63} {41, 22} label 0.000000 0.00100 "" center "Arial Black" 12 {0, 0, 0} {65280, 65280, 65280} nobackground noborder DUR
ioText {903, 166} {41, 22} label 0.000000 0.00100 "" center "Arial Black" 12 {0, 0, 0} {65280, 65280, 65280} nobackground noborder CPS
ioText {949, 166} {68, 24} label 0.000000 0.00100 "" center "Arial Black" 12 {0, 0, 0} {65280, 65280, 65280} nobackground noborder OVERLAPS
ioSlider {914, 96} {20, 66} 0.000000 1.000000 1.000000 densrand2
ioSlider {972, 96} {20, 66} 0.000000 1.000000 1.000000 durrand2
ioSlider {915, 199} {20, 66} 0.000000 1.000000 1.000000 cpsrand2
ioSlider {972, 199} {20, 66} 0.000000 1.000000 1.000000 overlaps2
ioText {908, 80} {36, 17} scroll 0.000000 0.001000 "" left "Arial" 12 {32768, 32768, 32768} {0, 0, 0} background noborder 
ioText {963, 80} {36, 17} scroll 0.000000 0.001000 "" left "Arial" 12 {32768, 32768, 32768} {0, 0, 0} background noborder 
ioText {907, 182} {36, 17} scroll 0.000000 0.001000 "" left "Arial" 12 {32768, 32768, 32768} {0, 0, 0} background noborder 
ioText {965, 182} {36, 17} scroll 0.000000 0.001000 "" left "Arial" 12 {32768, 32768, 32768} {0, 0, 0} background noborder 
ioText {25, 59} {41, 22} label 0.000000 0.00100 "" center "Arial Black" 12 {0, 0, 0} {65280, 65280, 65280} nobackground noborder DENS
ioText {80, 59} {41, 22} label 0.000000 0.00100 "" center "Arial Black" 12 {0, 0, 0} {65280, 65280, 65280} nobackground noborder DUR
ioText {22, 165} {41, 22} label 0.000000 0.00100 "" center "Arial Black" 12 {0, 0, 0} {65280, 65280, 65280} nobackground noborder CPS
ioText {68, 165} {68, 24} label 0.000000 0.00100 "" center "Arial Black" 12 {0, 0, 0} {65280, 65280, 65280} nobackground noborder OVERLAPS
ioSlider {34, 93} {20, 71} 0.000000 1.000000 0.000000 densrand1
ioSlider {92, 92} {20, 71} 0.000000 1.000000 0.000000 durrand1
ioSlider {34, 198} {20, 71} 0.000000 1.000000 0.000000 cpsrand1
ioSlider {91, 198} {20, 71} 0.000000 1.000000 0.629213 overlapsrand1
ioText {28, 76} {36, 17} scroll 0.000000 0.001000 "" left "Arial" 12 {32768, 32768, 32768} {0, 0, 0} background noborder 
ioText {83, 76} {36, 17} scroll 0.000000 0.001000 "" left "Arial" 12 {32768, 32768, 32768} {0, 0, 0} background noborder 
ioText {26, 181} {36, 17} scroll 0.000000 0.001000 "" left "Arial" 12 {32768, 32768, 32768} {0, 0, 0} background noborder 
ioText {84, 181} {36, 17} scroll 0.000000 0.001000 "" left "Arial" 12 {32768, 32768, 32768} {0, 0, 0} background noborder 
ioMenu {148, 773} {101, 24} 4 303 "GRAN 1,GRAN 2,GRAN 3,GRAN 4,GRAN5" selL
ioMenu {111, 806} {101, 24} 0 303 "DENS/DUR,DENS/CPS,DENS/VEL,DUR/CPS,DUR/VEL,FMD/PMD,FRPOW/PRPOW" selLL
ioMenu {617, 242} {105, 25} 0 303 "GAUSS,TRAPEZOID" kfn2
ioMenu {261, 245} {104, 23} 0 303 "GAUSS,TRAPEZOID" kfn1
ioMenu {99, 513} {104, 23} 0 303 "GAUSS,TRAPEZOID" kfn3
ioMenu {775, 512} {104, 23} 0 303 "GAUSS,TRAPEZOID" kfn4
ioMenu {321, 747} {105, 25} 0 303 "GAUSS,TRAPEZOID" kfn5
ioButton {726, 242} {77, 25} value 1.000000 "freeze2" "FREEZE" "/" i1 0 10
ioButton {885, 511} {76, 23} value 1.000000 "freeze4" "FREEZE" "/" i1 0 10
ioButton {372, 245} {76, 23} value 1.000000 "freeze1" "FREEZE" "/" i1 0 10
ioButton {482, 747} {98, 26} value 1.000000 "freeze5" "FREEZE" "/" i1 0 10
ioButton {219, 513} {76, 23} value 1.000000 "freeze3" "FREEZE" "/" i1 0 10
ioSlider {1040, 255} {374, 17} 0.000000 1.000000 1.000000 file1
ioSlider {1040, 293} {373, 16} 0.000000 1.000000 1.000000 file2
ioSlider {1041, 331} {372, 16} 0.000000 1.000000 1.000000 file3
ioSlider {1042, 370} {371, 16} 0.000000 1.000000 1.000000 file4
ioMeter {1079, 584} {241, 11} {0, 59904, 0} "disp_rmsout" 0.034370 "disp_rmsout" 0.034370 fill 1 0 mouse
ioMeter {1042, 385} {371, 15} {0, 59904, 0} "disp_rmsfile" 0.084566 "disp_rmsfile" 0.084566 fill 1 0 mouse
ioButton {1044, 90} {46, 27} value 1.000000 "_Play" "RUN" "/" i1 0 10
ioButton {1091, 90} {55, 27} value 1.000000 "_Stop" "STOP" "/" i1 0 10
ioButton {1126, 345} {48, 26} value 1.000000 "_Browse4" "Gr 4" "/" 
ioButton {1127, 307} {48, 24} value 1.000000 "_Browse3" "Gr 3" "/" 
ioButton {1127, 270} {47, 24} value 1.000000 "_Browse2" "Gr 2" "/" 
ioButton {1082, 345} {49, 24} event 1.000000 "" "Stop" "/" i 12 0 .1
ioButton {1043, 345} {49, 24} event 1.000000 "" "Play" "/" i 11 0 9999
ioButton {1082, 307} {49, 24} event 1.000000 "" "Stop" "/" i 10 0 .1
ioButton {1042, 307} {49, 24} event 1.000000 "" "Play" "/" i 9 0 9999
ioButton {1082, 270} {49, 24} event 1.000000 "" "Stop" "/" i 8 0 .1
ioButton {1042, 270} {49, 24} event 1.000000 "" "Play" "/" i 7 0 9999
ioButton {1081, 229} {49, 24} event 1.000000 "" "Stop" "/" i 6 0 .1
ioSlider {1095, 656} {321, 15} 0.000000 1.000000 0.915888 levrev
ioSlider {1095, 641} {321, 14} 0.000000 1.000000 1.000000 room
ioSlider {1095, 626} {321, 14} 0.000000 1.000000 0.716511 damp
ioButton {8, 771} {126, 28} event 1.000000 "" "CONTROLLER1" "/" i1 0 -10
ioMenu {426, 748} {62, 25} 0 303 "MAN,LIVE" selgrain5
ioMenu {490, 253} {44, 24} 2 303 "1-RAND,2-MANUAL,3-OSCIL,4-MOD" seluscite
ioKnob {366, 285} {36, 42} 1.000000 0.000000 0.010000 0.474747 mod1
ioKnob {367, 344} {36, 42} 1.000000 0.000000 0.010000 0.535354 mod2
ioKnob {367, 403} {36, 42} 1.000000 0.000000 0.010000 0.474747 mod3
ioKnob {366, 462} {36, 42} 1.000000 0.000000 0.010000 0.474747 mod4
ioKnob {626, 285} {36, 42} 1.000000 0.000000 0.010000 0.545455 mod5
ioKnob {627, 344} {36, 42} 1.000000 0.000000 0.010000 0.474747 mod6
ioKnob {627, 403} {36, 42} 1.000000 0.000000 0.010000 0.454545 mod7
ioKnob {626, 462} {36, 42} 1.000000 0.000000 0.010000 0.474747 mod8
ioButton {1039, 418} {41, 28} event 1.000000 "" "A" "/" i38 0 -10
ioButton {1042, 512} {41, 28} event 1.000000 "" "A" "/" i39 0 -10
ioText {1051, 637} {42, 22} label 0.000000 0.00100 "" left "Arial" 12 {0, 0, 0} {65280, 65280, 65280} nobackground noborder Room 
ioText {1056, 651} {29, 23} label 0.000000 0.00100 "" left "Arial" 12 {0, 0, 0} {65280, 65280, 65280} nobackground noborder Vol 
ioText {1050, 620} {44, 23} label 0.000000 0.00100 "" left "Arial" 12 {0, 0, 0} {65280, 65280, 65280} nobackground noborder Damp
ioText {1034, 680} {390, 162} label 0.000000 0.00100 "" center "Arial" 12 {65280, 65280, 65280} {0, 0, 0} nobackground noborder 
ioSlider {1085, 760} {13, 75} 0.000000 1.000000 1.000000 ampsin
ioSlider {1115, 760} {13, 75} 0.000000 20000.000000 15200.000000 kfreqsin
ioSlider {1183, 760} {13, 75} 0.000000 10000.000000 10000.000000 freqfm
ioText {1073, 706} {67, 22} label 0.000000 0.00100 "" left "Arial" 12 {65280, 65280, 65280} {0, 0, 0} nobackground noborder SINÂ¬
ioText {1073, 726} {32, 20} label 0.000000 0.00100 "" left "Arial" 8 {65280, 65280, 65280} {0, 0, 0} nobackground noborder AMPÂ¬
ioSlider {1276, 760} {13, 75} 0.000000 10.000000 10.000000 ndx
ioSlider {1245, 760} {13, 75} 0.000000 100.000000 100.000000 mod
ioSlider {1214, 760} {13, 75} 0.000000 100.000000 90.666667 car
ioSlider {1314, 760} {13, 75} 0.000000 0.500000 0.240000 att
ioSlider {1152, 760} {13, 75} 0.000000 1.000000 0.693333 ampfm
ioSlider {1347, 760} {13, 75} 0.000000 0.500000 0.186667 krel
ioMenu {1200, 684} {78, 25} 1 303 "LIVE,SYNTH,FIBONACCI" selsynth1
ioMenu {1276, 684} {52, 23} 0 303 "SIN,FM" selsynth2
ioButton {1119, 682} {80, 27} event 1.000000 "" "SYNTH" "/" i4 0 -10
ioText {1073, 744} {34, 16} scroll 1.000000 0.001000 "ampsin" left "Arial" 10 {65280, 65280, 65280} {0, 0, 0} background noborder 
ioText {1106, 744} {34, 16} scroll 15200.000000 1.000000 "kfreqsin" left "Arial" 10 {65280, 65280, 65280} {0, 0, 0} background noborder 
ioText {1104, 726} {36, 20} label 0.000000 0.00100 "" left "Arial" 8 {65280, 65280, 65280} {0, 0, 0} nobackground noborder FREQÂ¬
ioText {1142, 727} {33, 18} label 0.000000 0.00100 "" left "Arial" 8 {65280, 65280, 65280} {0, 0, 0} nobackground noborder AMPÂ¬Â¬
ioText {1172, 727} {36, 18} label 0.000000 0.00100 "" left "Arial" 8 {65280, 65280, 65280} {0, 0, 0} nobackground noborder FREQÂ¬
ioText {1205, 727} {33, 18} label 0.000000 0.00100 "" left "Arial" 8 {65280, 65280, 65280} {0, 0, 0} nobackground noborder CARÂ¬Â¬
ioText {1236, 727} {33, 18} label 0.000000 0.00100 "" left "Arial" 8 {65280, 65280, 65280} {0, 0, 0} nobackground noborder MODÂ¬
ioText {1267, 727} {34, 18} label 0.000000 0.00100 "" left "Arial" 8 {65280, 65280, 65280} {0, 0, 0} nobackground noborder INDXÂ¬
ioText {1304, 725} {33, 18} label 0.000000 0.00100 "" left "Arial" 8 {65280, 65280, 65280} {0, 0, 0} nobackground noborder ATTÂ¬
ioText {1336, 724} {32, 20} label 0.000000 0.00100 "" left "Arial" 8 {65280, 65280, 65280} {0, 0, 0} nobackground noborder RELÂ¬
ioText {1142, 706} {159, 22} label 0.000000 0.00100 "" left "Arial" 12 {65280, 65280, 65280} {0, 0, 0} nobackground noborder FMÂ¬
ioText {1304, 706} {64, 21} label 0.000000 0.00100 "" left "Arial" 12 {65280, 65280, 65280} {0, 0, 0} nobackground noborder LIVE ENVÂ¬
ioText {1174, 744} {32, 16} scroll 10000.000000 1.000000 "freqfm" left "Arial" 10 {65280, 65280, 65280} {0, 0, 0} background noborder 
ioText {1269, 744} {32, 16} scroll 10.000000 1.000000 "ndx" center "Arial" 10 {65280, 65280, 65280} {0, 0, 0} background noborder 
ioText {1336, 743} {32, 17} scroll 0.186667 0.001000 "krel" left "Arial" 10 {65280, 65280, 65280} {0, 0, 0} background noborder 
ioText {1304, 743} {32, 17} scroll 0.240000 0.001000 "att" left "Arial" 10 {65280, 65280, 65280} {0, 0, 0} background noborder 
ioText {1237, 744} {32, 16} scroll 100.000000 0.000000 "mod" center "Arial" 10 {65280, 65280, 65280} {0, 0, 0} background noborder 
ioText {1206, 744} {32, 16} scroll 90.666667 1.000000 "car" center "Arial" 10 {65280, 65280, 65280} {0, 0, 0} background noborder 
ioText {1142, 744} {32, 16} scroll 0.693333 0.001000 "ampfm" left "Arial" 10 {65280, 65280, 65280} {0, 0, 0} background noborder 
ioButton {888, 19} {131, 31} event 1.000000 "" "GRAIN RAND 2" "/" i1 0 10
ioButton {12, 16} {128, 32} event 1.000000 "" "GRAIN RAND 1" "/" i1 0 10
ioText {760, 543} {270, 297} label 0.000000 0.00100 "" left "Arial" 12 {0, 0, 0} {32768, 0, 0} nobackground noborder 
ioSlider {766, 569} {218, 19} 20.000000 1000.000000 640.366972 wfreq1
ioButton {771, 544} {118, 25} event 1.000000 "" "PULSE RESON" "/" i2 0 -10
ioText {985, 620} {43, 22} label 0.000000 0.00100 "" left "Arial" 10 {0, 0, 0} {65280, 65280, 65280} nobackground noborder FREQ2
ioSlider {766, 622} {218, 19} 20.000000 1000.000000 608.899083 wfreq2
ioSlider {767, 677} {218, 19} 20.000000 1000.000000 541.467890 wfreq3
ioSlider {766, 731} {218, 19} 20.000000 1000.000000 505.504587 wfreq4
ioSlider {766, 782} {218, 19} 20.000000 1000.000000 501.009174 wfreq5
ioText {985, 675} {43, 23} label 0.000000 0.00100 "" left "Arial" 10 {0, 0, 0} {65280, 65280, 65280} nobackground noborder FREQ3
ioText {985, 730} {43, 22} label 0.000000 0.00100 "" left "Arial" 10 {0, 0, 0} {65280, 65280, 65280} nobackground noborder FREQ4
ioText {985, 781} {44, 22} label 0.000000 0.00100 "" left "Arial" 10 {0, 0, 0} {65280, 65280, 65280} nobackground noborder FREQ5
ioText {985, 568} {41, 22} label 0.000000 0.00100 "" left "Arial" 10 {0, 0, 0} {65280, 65280, 65280} nobackground noborder FREQ1
ioKnob {785, 582} {49, 45} 10000.000000 20.000000 0.010000 10000.000000 cutoff1
ioKnob {894, 582} {49, 45} 1.000000 0.000000 0.010000 1.000000 wfeed1
ioKnob {784, 636} {49, 45} 10000.000000 20.000000 0.010000 10000.000000 cutoff2
ioKnob {894, 637} {49, 45} 1.000000 0.000000 0.010000 1.000000 wfeed2
ioKnob {784, 692} {49, 45} 10000.000000 20.000000 0.010000 9800.400000 cutoff3
ioKnob {894, 692} {49, 45} 1.000000 0.000000 0.010000 1.000000 wfeed3
ioKnob {783, 744} {49, 45} 10000.000000 20.000000 0.010000 10000.000000 cutoff4
ioKnob {894, 744} {49, 45} 1.000000 0.000000 0.010000 1.000000 wfeed4
ioKnob {783, 793} {49, 45} 10000.000000 20.000000 0.010000 9600.800000 cutoff5
ioKnob {894, 794} {49, 45} 1.000000 0.000000 0.010000 0.990000 wfeed5
ioText {843, 594} {41, 22} label 0.000000 0.00100 "" left "Arial" 10 {0, 0, 0} {65280, 65280, 65280} nobackground noborder CUT1Â¬
ioText {951, 808} {41, 22} label 0.000000 0.00100 "" left "Arial" 10 {0, 0, 0} {65280, 65280, 65280} nobackground noborder FEED5
ioText {843, 808} {41, 22} label 0.000000 0.00100 "" left "Arial" 10 {0, 0, 0} {65280, 65280, 65280} nobackground noborder CUT5
ioText {949, 755} {41, 22} label 0.000000 0.00100 "" left "Arial" 10 {0, 0, 0} {65280, 65280, 65280} nobackground noborder FEED4
ioText {845, 756} {41, 22} label 0.000000 0.00100 "" left "Arial" 10 {0, 0, 0} {65280, 65280, 65280} nobackground noborder CUT4
ioText {951, 702} {41, 22} label 0.000000 0.00100 "" left "Arial" 10 {0, 0, 0} {65280, 65280, 65280} nobackground noborder FEED3
ioText {844, 702} {41, 22} label 0.000000 0.00100 "" left "Arial" 10 {0, 0, 0} {65280, 65280, 65280} nobackground noborder CUT3
ioText {950, 649} {41, 22} label 0.000000 0.00100 "" left "Arial" 10 {0, 0, 0} {65280, 65280, 65280} nobackground noborder FEED2
ioText {845, 650} {41, 22} label 0.000000 0.00100 "" left "Arial" 10 {0, 0, 0} {65280, 65280, 65280} nobackground noborder CUT2
ioText {948, 593} {41, 22} label 0.000000 0.00100 "" left "Arial" 10 {0, 0, 0} {65280, 65280, 65280} nobackground noborder FEED1
ioKnob {890, 543} {37, 33} 1.000000 0.000000 0.010000 0.890000 ampi
ioKnob {955, 543} {37, 33} 2.000000 0.000000 0.010000 0.240000 intv
ioSlider {1391, 761} {13, 75} 0.000000 10000.000000 10000.000000 freqfib
ioText {1382, 744} {32, 17} scroll 10000.000000 1.000000 "freqfib" left "Arial" 10 {65280, 65280, 65280} {0, 0, 0} background noborder 
ioText {1367, 726} {56, 19} label 0.000000 0.00100 "" left "Arial" 8 {65280, 65280, 65280} {0, 0, 0} nobackground noborder FIB_FREQÂ¬
</MacGUI>
<EventPanel name="" tempo="60.00000000" loop="8.00000000" x="360" y="248" width="596" height="322" visible="true" loopStart="0" loopEnd="0">    </EventPanel>

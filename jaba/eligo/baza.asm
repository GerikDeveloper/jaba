.def XH = r27
.def XL = r26
.def YH = r29
.def YL = r28
.def ZH = r31
.def ZL = r30
.cseg
.org 0x0000
ldi r16, 0x5f
out 0x3d, r16
ldi r16, 0x02
out 0x3e, r16
push YH
push YL
in YL, 0x3d
in YH, 0x3e
sbiw YH:YL, 0x3f
rcall baza
pop YL
pop YH
rjmp end
fib:
ldd r16, Y+61
push r16
ldd r16, Y+60
push r16
ldi r16, 0x00
push r16
ldi r16, 0x00
push r16
pop r18
pop r19
pop r16
pop r17
ldi r20, 0x00
cp r16, r18
cpc r17, r19
brne lbl1
ldi r20, 0x01
lbl1:
push r20
ldd r16, Y+61
push r16
ldd r16, Y+60
push r16
ldi r16, 0x00
push r16
ldi r16, 0x01
push r16
pop r18
pop r19
pop r16
pop r17
ldi r20, 0x00
cp r16, r18
cpc r17, r19
brne lbl2
ldi r20, 0x01
lbl2:
push r20
pop r17
pop r16
or r16, r17
push r16
pop r16
clr r17
cpse r16, r17
rjmp lbl3
rjmp lbl0
lbl3:
ldi r16, 0x00
push r16
ldi r16, 0x01
push r16
pop r16
std Y+62, r16
pop r16
std Y+63, r16
ldi XL, 0x39
ldi XH, 0x00
add XL, YL
adc XH, YH
out 0x3d, XL
out 0x3e, XH
ret
rjmp lbl4
lbl0:
ldd r16, Y+61
push r16
ldd r16, Y+60
push r16
ldi r16, 0x00
push r16
ldi r16, 0x01
push r16
pop r18
pop r19
pop r16
pop r17
sub r16, r18
sbc r17, r19
push r17
push r16
pop r16
pop r17
push YH
push YL
in YL, 0x3d
in YH, 0x3e
sbiw YH:YL, 0x3f
clr r18
push r18
push r18
push r17
push r16
rcall fib
pop r18
pop r18
pop r16
pop r17
pop YL
pop YH
push r17
push r16
ldd r16, Y+61
push r16
ldd r16, Y+60
push r16
ldi r16, 0x00
push r16
ldi r16, 0x02
push r16
pop r18
pop r19
pop r16
pop r17
sub r16, r18
sbc r17, r19
push r17
push r16
pop r16
pop r17
push YH
push YL
in YL, 0x3d
in YH, 0x3e
sbiw YH:YL, 0x3f
clr r18
push r18
push r18
push r17
push r16
rcall fib
pop r18
pop r18
pop r16
pop r17
pop YL
pop YH
push r17
push r16
ldd r16, Y+57
push r16
ldd r16, Y+56
push r16
ldd r16, Y+55
push r16
ldd r16, Y+54
push r16
pop r18
pop r19
pop r16
pop r17
add r16, r18
adc r17, r19
push r17
push r16
pop r16
std Y+62, r16
pop r16
std Y+63, r16
ldi XL, 0x39
ldi XH, 0x00
add XL, YL
adc XH, YH
out 0x3d, XL
out 0x3e, XH
ret
ldi XL, 0x45
ldi XH, 0x00
add XL, YL
adc XH, YH
out 0x3d, XL
out 0x3e, XH
lbl4:
ret
en:
lbl6:
ldd r16, Y+63
push r16
ldd r16, Y+62
push r16
ldi r16, 0x00
push r16
ldi r16, 0x00
push r16
pop r18
pop r19
pop r16
pop r17
ldi r20, 0x01
cp r16, r18
cpc r17, r19
brne lbl7
ldi r20, 0x00
lbl7:
push r20
pop r16
clr r17
cpse r16, r17
rjmp lbl8
rjmp lbl5
lbl8:
ldi r16, 0x00
push r16
ldi r16, 0x32
push r16
pop r16
pop r17
push YH
push YL
in YL, 0x3d
in YH, 0x3e
sbiw YH:YL, 0x3f
push r17
push r16
rcall prokrasto
pop r18
pop r18
pop YL
pop YH
ldi r16, 0x01
push r16
pop r16
push YH
push YL
in YL, 0x3d
in YH, 0x3e
sbiw YH:YL, 0x3f
push r16
rcall fik_portb
pop r18
pop YL
pop YH
ldi r16, 0x00
push r16
ldi r16, 0x32
push r16
pop r16
pop r17
push YH
push YL
in YL, 0x3d
in YH, 0x3e
sbiw YH:YL, 0x3f
push r17
push r16
rcall prokrasto
pop r18
pop r18
pop YL
pop YH
ldi r16, 0x00
push r16
pop r16
push YH
push YL
in YL, 0x3d
in YH, 0x3e
sbiw YH:YL, 0x3f
push r16
rcall fik_portb
pop r18
pop YL
pop YH
ldd r16, Y+63
push r16
ldd r16, Y+62
push r16
ldi r16, 0x00
push r16
ldi r16, 0x01
push r16
pop r18
pop r19
pop r16
pop r17
sub r16, r18
sbc r17, r19
push r17
push r16
pop r16
std Y+62, r16
pop r16
std Y+63, r16
rjmp lbl6
lbl5:
ret
baza:
ldi r16, 0x01
push r16
pop r16
push YH
push YL
in YL, 0x3d
in YH, 0x3e
sbiw YH:YL, 0x3f
push r16
rcall fik_ddrb
pop r18
pop YL
pop YH
ldi r16, 0x00
push r16
ldi r16, 0x00
push r16
lbla:
ldd r16, Y+61
push r16
ldd r16, Y+60
push r16
pop r16
pop r17
push YH
push YL
in YL, 0x3d
in YH, 0x3e
sbiw YH:YL, 0x3f
clr r18
push r18
push r18
push r17
push r16
rcall fib
pop r18
pop r18
pop r16
pop r17
pop YL
pop YH
push r17
push r16
pop r16
pop r17
push YH
push YL
in YL, 0x3d
in YH, 0x3e
sbiw YH:YL, 0x3f
push r17
push r16
rcall en
pop r18
pop r18
pop YL
pop YH
ldi r16, 0x00
push r16
ldi r16, 0x96
push r16
pop r16
pop r17
push YH
push YL
in YL, 0x3d
in YH, 0x3e
sbiw YH:YL, 0x3f
push r17
push r16
rcall prokrasto
pop r18
pop r18
pop YL
pop YH
ldd r16, Y+61
push r16
ldd r16, Y+60
push r16
ldi r16, 0x00
push r16
ldi r16, 0x01
push r16
pop r18
pop r19
pop r16
pop r17
add r16, r18
adc r17, r19
push r17
push r16
pop r16
std Y+60, r16
pop r16
std Y+61, r16
rjmp lbla
lbl9:
ldi XL, 0x41
ldi XH, 0x00
add XL, YL
adc XH, YH
out 0x3d, XL
out 0x3e, XH
ret
ldi XL, 0x3d
ldi XH, 0x00
add XL, YL
adc XH, YH
out 0x3d, XL
out 0x3e, XH
end:
nop
rjmp end
skr_kar:
ret
fik_ddrb:
ldd r16, Y+63
push r16
pop r16
out 0x17, r16
ret
fik_portb:
ldd r16, Y+63
push r16
pop r16
out 0x18, r16
ret
prokrasto:
ldd r16, Y+63
push r16
ldd r16, Y+62
push r16
pop ZL
pop ZH
lblb:
ldi XL, 0x08
ldi XH, 0x6e
lblc:
sbiw XH:XL, 0x01
brne lblc
sbiw ZH:ZL, 0x01
brne lblb
ret

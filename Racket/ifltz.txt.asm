psh 3
psh 0
psh 2
psh 0
swp
str
jmp 0
lbl 0
psh 0
rtr
psh 1
sub
dup
jez 1
psh 1
sub
dup
jez 2
psh 1
sub
dup
jez 3
psh 1
sub
jmp 5
lbl 1
dis
psh 2
swp
str
psh 3
rtr
jez 14
psh 500
psh 2
rtr
psh 0
swp
str
jmp 0
lbl 14
psh 15
psh 2
rtr
psh 0
swp
str
jmp 0
lbl 2
dis
psh 3
swp
str
psh 4
swp
str
psh 4
rtr
psh 1
psh 0
swp
str
jmp 0
lbl 3
dis
psh 5
swp
str
psh 5
rtr
pnm
jmp 5
lbl 5
end
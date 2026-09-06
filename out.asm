L2: ; add: entry 
  push RBP
  mov RBP, RSP
L1: ; add: body 
L0: ; add: exit 
  sub RSP, 4
  mov EAX, [rbp+20]
  add EAX, [rbp+24]
  mov [rbp-4], EAX
  mov EAX, [rbp-4]
  pop RBP
  ret
L5: ; main: entry 
  push RBP
  mov RBP, RSP
L4: ; main: body 
L3: ; main: exit 
  push 1
  push 2
  call add
  add RBP, 8
  mov EAX, EAX
  pop RBP
  ret

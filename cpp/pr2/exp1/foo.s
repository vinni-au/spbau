
foo:     file format elf32-i386


Disassembly of section .interp:

08048114 <.interp>:
 8048114:	2f                   	das    
 8048115:	6c                   	insb   (%dx),%es:(%edi)
 8048116:	69 62 2f 6c 64 2d 6c 	imul   $0x6c2d646c,0x2f(%edx),%esp
 804811d:	69 6e 75 78 2e 73 6f 	imul   $0x6f732e78,0x75(%esi),%ebp
 8048124:	2e 32 00             	xor    %cs:(%eax),%al

Disassembly of section .note.ABI-tag:

08048128 <.note.ABI-tag>:
 8048128:	04 00                	add    $0x0,%al
 804812a:	00 00                	add    %al,(%eax)
 804812c:	10 00                	adc    %al,(%eax)
 804812e:	00 00                	add    %al,(%eax)
 8048130:	01 00                	add    %eax,(%eax)
 8048132:	00 00                	add    %al,(%eax)
 8048134:	47                   	inc    %edi
 8048135:	4e                   	dec    %esi
 8048136:	55                   	push   %ebp
 8048137:	00 00                	add    %al,(%eax)
 8048139:	00 00                	add    %al,(%eax)
 804813b:	00 02                	add    %al,(%edx)
 804813d:	00 00                	add    %al,(%eax)
 804813f:	00 06                	add    %al,(%esi)
 8048141:	00 00                	add    %al,(%eax)
 8048143:	00 12                	add    %dl,(%edx)
 8048145:	00 00                	add    %al,(%eax)
	...

Disassembly of section .note.gnu.build-id:

08048148 <.note.gnu.build-id>:
 8048148:	04 00                	add    $0x0,%al
 804814a:	00 00                	add    %al,(%eax)
 804814c:	14 00                	adc    $0x0,%al
 804814e:	00 00                	add    %al,(%eax)
 8048150:	03 00                	add    (%eax),%eax
 8048152:	00 00                	add    %al,(%eax)
 8048154:	47                   	inc    %edi
 8048155:	4e                   	dec    %esi
 8048156:	55                   	push   %ebp
 8048157:	00 17                	add    %dl,(%edi)
 8048159:	d4 a1                	aam    $0xffffffa1
 804815b:	69 9e d1 18 46 0a e8 	imul   $0xf87855e8,0xa4618d1(%esi),%ebx
 8048162:	55 78 f8 
 8048165:	8f                   	(bad)  
 8048166:	e0 6f                	loopne 80481d7 <_init-0xc5>
 8048168:	ac                   	lods   %ds:(%esi),%al
 8048169:	94                   	xchg   %eax,%esp
 804816a:	f6 18                	negb   (%eax)

Disassembly of section .hash:

0804816c <.hash>:
 804816c:	03 00                	add    (%eax),%eax
 804816e:	00 00                	add    %al,(%eax)
 8048170:	05 00 00 00 01       	add    $0x1000000,%eax
 8048175:	00 00                	add    %al,(%eax)
 8048177:	00 02                	add    %al,(%edx)
 8048179:	00 00                	add    %al,(%eax)
 804817b:	00 03                	add    %al,(%ebx)
	...
 8048189:	00 00                	add    %al,(%eax)
 804818b:	00 04 00             	add    %al,(%eax,%eax,1)
 804818e:	00 00                	add    %al,(%eax)
 8048190:	00 00                	add    %al,(%eax)
	...

Disassembly of section .gnu.hash:

08048194 <.gnu.hash>:
 8048194:	02 00                	add    (%eax),%al
 8048196:	00 00                	add    %al,(%eax)
 8048198:	04 00                	add    $0x0,%al
 804819a:	00 00                	add    %al,(%eax)
 804819c:	01 00                	add    %eax,(%eax)
 804819e:	00 00                	add    %al,(%eax)
 80481a0:	05 00 00 00 00       	add    $0x0,%eax
 80481a5:	20 00                	and    %al,(%eax)
 80481a7:	20 00                	and    %al,(%eax)
 80481a9:	00 00                	add    %al,(%eax)
 80481ab:	00 04 00             	add    %al,(%eax,%eax,1)
 80481ae:	00 00                	add    %al,(%eax)
 80481b0:	ad                   	lods   %ds:(%esi),%eax
 80481b1:	4b                   	dec    %ebx
 80481b2:	e3 c0                	jecxz  8048174 <_init-0x128>

Disassembly of section .dynsym:

080481b4 <.dynsym>:
	...
 80481c4:	01 00                	add    %eax,(%eax)
	...
 80481ce:	00 00                	add    %al,(%eax)
 80481d0:	20 00                	and    %al,(%eax)
 80481d2:	00 00                	add    %al,(%eax)
 80481d4:	30 00                	xor    %al,(%eax)
	...
 80481de:	00 00                	add    %al,(%eax)
 80481e0:	12 00                	adc    (%eax),%al
 80481e2:	00 00                	add    %al,(%eax)
 80481e4:	29 00                	sub    %eax,(%eax)
	...
 80481ee:	00 00                	add    %al,(%eax)
 80481f0:	12 00                	adc    (%eax),%al
 80481f2:	00 00                	add    %al,(%eax)
 80481f4:	1a 00                	sbb    (%eax),%al
 80481f6:	00 00                	add    %al,(%eax)
 80481f8:	0c 85                	or     $0x85,%al
 80481fa:	04 08                	add    $0x8,%al
 80481fc:	04 00                	add    $0x0,%al
 80481fe:	00 00                	add    %al,(%eax)
 8048200:	11 00                	adc    %eax,(%eax)
 8048202:	10 00                	adc    %al,(%eax)

Disassembly of section .dynstr:

08048204 <.dynstr>:
 8048204:	00 5f 5f             	add    %bl,0x5f(%edi)
 8048207:	67 6d                	addr16 insl (%dx),%es:(%di)
 8048209:	6f                   	outsl  %ds:(%esi),(%dx)
 804820a:	6e                   	outsb  %ds:(%esi),(%dx)
 804820b:	5f                   	pop    %edi
 804820c:	73 74                	jae    8048282 <_init-0x1a>
 804820e:	61                   	popa   
 804820f:	72 74                	jb     8048285 <_init-0x17>
 8048211:	5f                   	pop    %edi
 8048212:	5f                   	pop    %edi
 8048213:	00 6c 69 62          	add    %ch,0x62(%ecx,%ebp,2)
 8048217:	63 2e                	arpl   %bp,(%esi)
 8048219:	73 6f                	jae    804828a <_init-0x12>
 804821b:	2e 36 00 5f 49       	add    %bl,%cs:%ss:0x49(%edi)
 8048220:	4f                   	dec    %edi
 8048221:	5f                   	pop    %edi
 8048222:	73 74                	jae    8048298 <_init-0x4>
 8048224:	64 69 6e 5f 75 73 65 	imul   $0x64657375,%fs:0x5f(%esi),%ebp
 804822b:	64 
 804822c:	00 70 72             	add    %dh,0x72(%eax)
 804822f:	69 6e 74 66 00 5f 5f 	imul   $0x5f5f0066,0x74(%esi),%ebp
 8048236:	6c                   	insb   (%dx),%es:(%edi)
 8048237:	69 62 63 5f 73 74 61 	imul   $0x6174735f,0x63(%edx),%esp
 804823e:	72 74                	jb     80482b4 <_init+0x18>
 8048240:	5f                   	pop    %edi
 8048241:	6d                   	insl   (%dx),%es:(%edi)
 8048242:	61                   	popa   
 8048243:	69 6e 00 47 4c 49 42 	imul   $0x42494c47,0x0(%esi),%ebp
 804824a:	43                   	inc    %ebx
 804824b:	5f                   	pop    %edi
 804824c:	32 2e                	xor    (%esi),%ch
 804824e:	30 00                	xor    %al,(%eax)

Disassembly of section .gnu.version:

08048250 <.gnu.version>:
 8048250:	00 00                	add    %al,(%eax)
 8048252:	00 00                	add    %al,(%eax)
 8048254:	02 00                	add    (%eax),%al
 8048256:	02 00                	add    (%eax),%al
 8048258:	01 00                	add    %eax,(%eax)

Disassembly of section .gnu.version_r:

0804825c <.gnu.version_r>:
 804825c:	01 00                	add    %eax,(%eax)
 804825e:	01 00                	add    %eax,(%eax)
 8048260:	10 00                	adc    %al,(%eax)
 8048262:	00 00                	add    %al,(%eax)
 8048264:	10 00                	adc    %al,(%eax)
 8048266:	00 00                	add    %al,(%eax)
 8048268:	00 00                	add    %al,(%eax)
 804826a:	00 00                	add    %al,(%eax)
 804826c:	10 69 69             	adc    %ch,0x69(%ecx)
 804826f:	0d 00 00 02 00       	or     $0x20000,%eax
 8048274:	42                   	inc    %edx
 8048275:	00 00                	add    %al,(%eax)
 8048277:	00 00                	add    %al,(%eax)
 8048279:	00 00                	add    %al,(%eax)
	...

Disassembly of section .rel.dyn:

0804827c <.rel.dyn>:
 804827c:	fc                   	cld    
 804827d:	95                   	xchg   %eax,%ebp
 804827e:	04 08                	add    $0x8,%al
 8048280:	06                   	push   %es
 8048281:	01 00                	add    %eax,(%eax)
	...

Disassembly of section .rel.plt:

08048284 <.rel.plt>:
 8048284:	0c 96                	or     $0x96,%al
 8048286:	04 08                	add    $0x8,%al
 8048288:	07                   	pop    %es
 8048289:	01 00                	add    %eax,(%eax)
 804828b:	00 10                	add    %dl,(%eax)
 804828d:	96                   	xchg   %eax,%esi
 804828e:	04 08                	add    $0x8,%al
 8048290:	07                   	pop    %es
 8048291:	02 00                	add    (%eax),%al
 8048293:	00 14 96             	add    %dl,(%esi,%edx,4)
 8048296:	04 08                	add    $0x8,%al
 8048298:	07                   	pop    %es
 8048299:	03 00                	add    (%eax),%eax
	...

Disassembly of section .init:

0804829c <_init>:
 804829c:	55                   	push   %ebp
 804829d:	89 e5                	mov    %esp,%ebp
 804829f:	53                   	push   %ebx
 80482a0:	83 ec 04             	sub    $0x4,%esp
 80482a3:	e8 00 00 00 00       	call   80482a8 <_init+0xc>
 80482a8:	5b                   	pop    %ebx
 80482a9:	81 c3 58 13 00 00    	add    $0x1358,%ebx
 80482af:	8b 93 fc ff ff ff    	mov    -0x4(%ebx),%edx
 80482b5:	85 d2                	test   %edx,%edx
 80482b7:	74 05                	je     80482be <_init+0x22>
 80482b9:	e8 1e 00 00 00       	call   80482dc <__gmon_start__@plt>
 80482be:	e8 dd 00 00 00       	call   80483a0 <frame_dummy>
 80482c3:	e8 f8 01 00 00       	call   80484c0 <__do_global_ctors_aux>
 80482c8:	58                   	pop    %eax
 80482c9:	5b                   	pop    %ebx
 80482ca:	c9                   	leave  
 80482cb:	c3                   	ret    

Disassembly of section .plt:

080482cc <__gmon_start__@plt-0x10>:
 80482cc:	ff 35 04 96 04 08    	pushl  0x8049604
 80482d2:	ff 25 08 96 04 08    	jmp    *0x8049608
 80482d8:	00 00                	add    %al,(%eax)
	...

080482dc <__gmon_start__@plt>:
 80482dc:	ff 25 0c 96 04 08    	jmp    *0x804960c
 80482e2:	68 00 00 00 00       	push   $0x0
 80482e7:	e9 e0 ff ff ff       	jmp    80482cc <_init+0x30>

080482ec <__libc_start_main@plt>:
 80482ec:	ff 25 10 96 04 08    	jmp    *0x8049610
 80482f2:	68 08 00 00 00       	push   $0x8
 80482f7:	e9 d0 ff ff ff       	jmp    80482cc <_init+0x30>

080482fc <printf@plt>:
 80482fc:	ff 25 14 96 04 08    	jmp    *0x8049614
 8048302:	68 10 00 00 00       	push   $0x10
 8048307:	e9 c0 ff ff ff       	jmp    80482cc <_init+0x30>

Disassembly of section .text:

08048310 <_start>:
 8048310:	31 ed                	xor    %ebp,%ebp
 8048312:	5e                   	pop    %esi
 8048313:	89 e1                	mov    %esp,%ecx
 8048315:	83 e4 f0             	and    $0xfffffff0,%esp
 8048318:	50                   	push   %eax
 8048319:	54                   	push   %esp
 804831a:	52                   	push   %edx
 804831b:	68 50 84 04 08       	push   $0x8048450
 8048320:	68 60 84 04 08       	push   $0x8048460
 8048325:	51                   	push   %ecx
 8048326:	56                   	push   %esi
 8048327:	68 c4 83 04 08       	push   $0x80483c4
 804832c:	e8 bb ff ff ff       	call   80482ec <__libc_start_main@plt>
 8048331:	f4                   	hlt    
 8048332:	90                   	nop
 8048333:	90                   	nop
 8048334:	90                   	nop
 8048335:	90                   	nop
 8048336:	90                   	nop
 8048337:	90                   	nop
 8048338:	90                   	nop
 8048339:	90                   	nop
 804833a:	90                   	nop
 804833b:	90                   	nop
 804833c:	90                   	nop
 804833d:	90                   	nop
 804833e:	90                   	nop
 804833f:	90                   	nop

08048340 <__do_global_dtors_aux>:
 8048340:	55                   	push   %ebp
 8048341:	89 e5                	mov    %esp,%ebp
 8048343:	53                   	push   %ebx
 8048344:	83 ec 04             	sub    $0x4,%esp
 8048347:	80 3d 20 96 04 08 00 	cmpb   $0x0,0x8049620
 804834e:	75 3f                	jne    804838f <__do_global_dtors_aux+0x4f>
 8048350:	a1 24 96 04 08       	mov    0x8049624,%eax
 8048355:	bb 24 95 04 08       	mov    $0x8049524,%ebx
 804835a:	81 eb 20 95 04 08    	sub    $0x8049520,%ebx
 8048360:	c1 fb 02             	sar    $0x2,%ebx
 8048363:	83 eb 01             	sub    $0x1,%ebx
 8048366:	39 d8                	cmp    %ebx,%eax
 8048368:	73 1e                	jae    8048388 <__do_global_dtors_aux+0x48>
 804836a:	8d b6 00 00 00 00    	lea    0x0(%esi),%esi
 8048370:	83 c0 01             	add    $0x1,%eax
 8048373:	a3 24 96 04 08       	mov    %eax,0x8049624
 8048378:	ff 14 85 20 95 04 08 	call   *0x8049520(,%eax,4)
 804837f:	a1 24 96 04 08       	mov    0x8049624,%eax
 8048384:	39 d8                	cmp    %ebx,%eax
 8048386:	72 e8                	jb     8048370 <__do_global_dtors_aux+0x30>
 8048388:	c6 05 20 96 04 08 01 	movb   $0x1,0x8049620
 804838f:	83 c4 04             	add    $0x4,%esp
 8048392:	5b                   	pop    %ebx
 8048393:	5d                   	pop    %ebp
 8048394:	c3                   	ret    
 8048395:	8d 74 26 00          	lea    0x0(%esi,%eiz,1),%esi
 8048399:	8d bc 27 00 00 00 00 	lea    0x0(%edi,%eiz,1),%edi

080483a0 <frame_dummy>:
 80483a0:	55                   	push   %ebp
 80483a1:	89 e5                	mov    %esp,%ebp
 80483a3:	83 ec 18             	sub    $0x18,%esp
 80483a6:	a1 28 95 04 08       	mov    0x8049528,%eax
 80483ab:	85 c0                	test   %eax,%eax
 80483ad:	74 12                	je     80483c1 <frame_dummy+0x21>
 80483af:	b8 00 00 00 00       	mov    $0x0,%eax
 80483b4:	85 c0                	test   %eax,%eax
 80483b6:	74 09                	je     80483c1 <frame_dummy+0x21>
 80483b8:	c7 04 24 28 95 04 08 	movl   $0x8049528,(%esp)
 80483bf:	ff d0                	call   *%eax
 80483c1:	c9                   	leave  
 80483c2:	c3                   	ret    
 80483c3:	90                   	nop

080483c4 <main>:
 80483c4:	55                   	push   %ebp
 80483c5:	89 e5                	mov    %esp,%ebp
 80483c7:	83 e4 f0             	and    $0xfffffff0,%esp
 80483ca:	83 ec 20             	sub    $0x20,%esp
 80483cd:	c7 44 24 1c 00 00 00 	movl   $0x0,0x1c(%esp)
 80483d4:	00 
 80483d5:	eb 1b                	jmp    80483f2 <main+0x2e>
 80483d7:	e8 24 00 00 00       	call   8048400 <foo>
 80483dc:	ba 10 85 04 08       	mov    $0x8048510,%edx
 80483e1:	89 44 24 04          	mov    %eax,0x4(%esp)
 80483e5:	89 14 24             	mov    %edx,(%esp)
 80483e8:	e8 0f ff ff ff       	call   80482fc <printf@plt>
 80483ed:	83 44 24 1c 01       	addl   $0x1,0x1c(%esp)
 80483f2:	83 7c 24 1c 63       	cmpl   $0x63,0x1c(%esp)
 80483f7:	7e de                	jle    80483d7 <main+0x13>
 80483f9:	b8 00 00 00 00       	mov    $0x0,%eax
 80483fe:	c9                   	leave  
 80483ff:	c3                   	ret    

08048400 <foo>:
 8048400:	55                   	push   %ebp
 8048401:	89 e5                	mov    %esp,%ebp
 8048403:	83 ec 04             	sub    $0x4,%esp
 8048406:	8b 15 28 96 04 08    	mov    0x8049628,%edx
 804840c:	89 d0                	mov    %edx,%eax
 804840e:	01 c0                	add    %eax,%eax
 8048410:	01 d0                	add    %edx,%eax
 8048412:	c1 e0 02             	shl    $0x2,%eax
 8048415:	01 d0                	add    %edx,%eax
 8048417:	8d 88 7f 5b 00 00    	lea    0x5b7f(%eax),%ecx
 804841d:	89 c8                	mov    %ecx,%eax
 804841f:	d1 e8                	shr    %eax
 8048421:	89 45 fc             	mov    %eax,-0x4(%ebp)
 8048424:	ba 05 d0 e8 27       	mov    $0x27e8d005,%edx
 8048429:	8b 45 fc             	mov    -0x4(%ebp),%eax
 804842c:	f7 e2                	mul    %edx
 804842e:	89 d0                	mov    %edx,%eax
 8048430:	c1 e8 14             	shr    $0x14,%eax
 8048433:	69 c0 ca 43 cd 00    	imul   $0xcd43ca,%eax,%eax
 8048439:	89 ca                	mov    %ecx,%edx
 804843b:	29 c2                	sub    %eax,%edx
 804843d:	89 d0                	mov    %edx,%eax
 804843f:	a3 28 96 04 08       	mov    %eax,0x8049628
 8048444:	a1 28 96 04 08       	mov    0x8049628,%eax
 8048449:	c9                   	leave  
 804844a:	c3                   	ret    
 804844b:	90                   	nop
 804844c:	90                   	nop
 804844d:	90                   	nop
 804844e:	90                   	nop
 804844f:	90                   	nop

08048450 <__libc_csu_fini>:
 8048450:	55                   	push   %ebp
 8048451:	89 e5                	mov    %esp,%ebp
 8048453:	5d                   	pop    %ebp
 8048454:	c3                   	ret    
 8048455:	8d 74 26 00          	lea    0x0(%esi,%eiz,1),%esi
 8048459:	8d bc 27 00 00 00 00 	lea    0x0(%edi,%eiz,1),%edi

08048460 <__libc_csu_init>:
 8048460:	55                   	push   %ebp
 8048461:	89 e5                	mov    %esp,%ebp
 8048463:	57                   	push   %edi
 8048464:	56                   	push   %esi
 8048465:	53                   	push   %ebx
 8048466:	e8 4f 00 00 00       	call   80484ba <__i686.get_pc_thunk.bx>
 804846b:	81 c3 95 11 00 00    	add    $0x1195,%ebx
 8048471:	83 ec 1c             	sub    $0x1c,%esp
 8048474:	e8 23 fe ff ff       	call   804829c <_init>
 8048479:	8d bb 18 ff ff ff    	lea    -0xe8(%ebx),%edi
 804847f:	8d 83 18 ff ff ff    	lea    -0xe8(%ebx),%eax
 8048485:	29 c7                	sub    %eax,%edi
 8048487:	c1 ff 02             	sar    $0x2,%edi
 804848a:	85 ff                	test   %edi,%edi
 804848c:	74 24                	je     80484b2 <__libc_csu_init+0x52>
 804848e:	31 f6                	xor    %esi,%esi
 8048490:	8b 45 10             	mov    0x10(%ebp),%eax
 8048493:	89 44 24 08          	mov    %eax,0x8(%esp)
 8048497:	8b 45 0c             	mov    0xc(%ebp),%eax
 804849a:	89 44 24 04          	mov    %eax,0x4(%esp)
 804849e:	8b 45 08             	mov    0x8(%ebp),%eax
 80484a1:	89 04 24             	mov    %eax,(%esp)
 80484a4:	ff 94 b3 18 ff ff ff 	call   *-0xe8(%ebx,%esi,4)
 80484ab:	83 c6 01             	add    $0x1,%esi
 80484ae:	39 fe                	cmp    %edi,%esi
 80484b0:	72 de                	jb     8048490 <__libc_csu_init+0x30>
 80484b2:	83 c4 1c             	add    $0x1c,%esp
 80484b5:	5b                   	pop    %ebx
 80484b6:	5e                   	pop    %esi
 80484b7:	5f                   	pop    %edi
 80484b8:	5d                   	pop    %ebp
 80484b9:	c3                   	ret    

080484ba <__i686.get_pc_thunk.bx>:
 80484ba:	8b 1c 24             	mov    (%esp),%ebx
 80484bd:	c3                   	ret    
 80484be:	90                   	nop
 80484bf:	90                   	nop

080484c0 <__do_global_ctors_aux>:
 80484c0:	55                   	push   %ebp
 80484c1:	89 e5                	mov    %esp,%ebp
 80484c3:	53                   	push   %ebx
 80484c4:	83 ec 04             	sub    $0x4,%esp
 80484c7:	a1 18 95 04 08       	mov    0x8049518,%eax
 80484cc:	83 f8 ff             	cmp    $0xffffffff,%eax
 80484cf:	74 13                	je     80484e4 <__do_global_ctors_aux+0x24>
 80484d1:	bb 18 95 04 08       	mov    $0x8049518,%ebx
 80484d6:	66 90                	xchg   %ax,%ax
 80484d8:	83 eb 04             	sub    $0x4,%ebx
 80484db:	ff d0                	call   *%eax
 80484dd:	8b 03                	mov    (%ebx),%eax
 80484df:	83 f8 ff             	cmp    $0xffffffff,%eax
 80484e2:	75 f4                	jne    80484d8 <__do_global_ctors_aux+0x18>
 80484e4:	83 c4 04             	add    $0x4,%esp
 80484e7:	5b                   	pop    %ebx
 80484e8:	5d                   	pop    %ebp
 80484e9:	c3                   	ret    
 80484ea:	90                   	nop
 80484eb:	90                   	nop

Disassembly of section .fini:

080484ec <_fini>:
 80484ec:	55                   	push   %ebp
 80484ed:	89 e5                	mov    %esp,%ebp
 80484ef:	53                   	push   %ebx
 80484f0:	83 ec 04             	sub    $0x4,%esp
 80484f3:	e8 00 00 00 00       	call   80484f8 <_fini+0xc>
 80484f8:	5b                   	pop    %ebx
 80484f9:	81 c3 08 11 00 00    	add    $0x1108,%ebx
 80484ff:	e8 3c fe ff ff       	call   8048340 <__do_global_dtors_aux>
 8048504:	59                   	pop    %ecx
 8048505:	5b                   	pop    %ebx
 8048506:	c9                   	leave  
 8048507:	c3                   	ret    

Disassembly of section .rodata:

08048508 <_fp_hw>:
 8048508:	03 00                	add    (%eax),%eax
	...

0804850c <_IO_stdin_used>:
 804850c:	01 00                	add    %eax,(%eax)
 804850e:	02 00                	add    (%eax),%al
 8048510:	25                   	.byte 0x25
 8048511:	64 20 00             	and    %al,%fs:(%eax)

Disassembly of section .eh_frame:

08048514 <__FRAME_END__>:
 8048514:	00 00                	add    %al,(%eax)
	...

Disassembly of section .ctors:

08049518 <__CTOR_LIST__>:
 8049518:	ff                   	(bad)  
 8049519:	ff                   	(bad)  
 804951a:	ff                   	(bad)  
 804951b:	ff 00                	incl   (%eax)

0804951c <__CTOR_END__>:
 804951c:	00 00                	add    %al,(%eax)
	...

Disassembly of section .dtors:

08049520 <__DTOR_LIST__>:
 8049520:	ff                   	(bad)  
 8049521:	ff                   	(bad)  
 8049522:	ff                   	(bad)  
 8049523:	ff 00                	incl   (%eax)

08049524 <__DTOR_END__>:
 8049524:	00 00                	add    %al,(%eax)
	...

Disassembly of section .jcr:

08049528 <__JCR_END__>:
 8049528:	00 00                	add    %al,(%eax)
	...

Disassembly of section .dynamic:

0804952c <_DYNAMIC>:
 804952c:	01 00                	add    %eax,(%eax)
 804952e:	00 00                	add    %al,(%eax)
 8049530:	10 00                	adc    %al,(%eax)
 8049532:	00 00                	add    %al,(%eax)
 8049534:	0c 00                	or     $0x0,%al
 8049536:	00 00                	add    %al,(%eax)
 8049538:	9c                   	pushf  
 8049539:	82                   	(bad)  
 804953a:	04 08                	add    $0x8,%al
 804953c:	0d 00 00 00 ec       	or     $0xec000000,%eax
 8049541:	84 04 08             	test   %al,(%eax,%ecx,1)
 8049544:	04 00                	add    $0x0,%al
 8049546:	00 00                	add    %al,(%eax)
 8049548:	6c                   	insb   (%dx),%es:(%edi)
 8049549:	81 04 08 f5 fe ff 6f 	addl   $0x6ffffef5,(%eax,%ecx,1)
 8049550:	94                   	xchg   %eax,%esp
 8049551:	81 04 08 05 00 00 00 	addl   $0x5,(%eax,%ecx,1)
 8049558:	04 82                	add    $0x82,%al
 804955a:	04 08                	add    $0x8,%al
 804955c:	06                   	push   %es
 804955d:	00 00                	add    %al,(%eax)
 804955f:	00 b4 81 04 08 0a 00 	add    %dh,0xa0804(%ecx,%eax,4)
 8049566:	00 00                	add    %al,(%eax)
 8049568:	4c                   	dec    %esp
 8049569:	00 00                	add    %al,(%eax)
 804956b:	00 0b                	add    %cl,(%ebx)
 804956d:	00 00                	add    %al,(%eax)
 804956f:	00 10                	add    %dl,(%eax)
 8049571:	00 00                	add    %al,(%eax)
 8049573:	00 15 00 00 00 00    	add    %dl,0x0
 8049579:	00 00                	add    %al,(%eax)
 804957b:	00 03                	add    %al,(%ebx)
 804957d:	00 00                	add    %al,(%eax)
 804957f:	00 00                	add    %al,(%eax)
 8049581:	96                   	xchg   %eax,%esi
 8049582:	04 08                	add    $0x8,%al
 8049584:	02 00                	add    (%eax),%al
 8049586:	00 00                	add    %al,(%eax)
 8049588:	18 00                	sbb    %al,(%eax)
 804958a:	00 00                	add    %al,(%eax)
 804958c:	14 00                	adc    $0x0,%al
 804958e:	00 00                	add    %al,(%eax)
 8049590:	11 00                	adc    %eax,(%eax)
 8049592:	00 00                	add    %al,(%eax)
 8049594:	17                   	pop    %ss
 8049595:	00 00                	add    %al,(%eax)
 8049597:	00 84 82 04 08 11 00 	add    %al,0x110804(%edx,%eax,4)
 804959e:	00 00                	add    %al,(%eax)
 80495a0:	7c 82                	jl     8049524 <__DTOR_END__>
 80495a2:	04 08                	add    $0x8,%al
 80495a4:	12 00                	adc    (%eax),%al
 80495a6:	00 00                	add    %al,(%eax)
 80495a8:	08 00                	or     %al,(%eax)
 80495aa:	00 00                	add    %al,(%eax)
 80495ac:	13 00                	adc    (%eax),%eax
 80495ae:	00 00                	add    %al,(%eax)
 80495b0:	08 00                	or     %al,(%eax)
 80495b2:	00 00                	add    %al,(%eax)
 80495b4:	fe                   	(bad)  
 80495b5:	ff                   	(bad)  
 80495b6:	ff 6f 5c             	ljmp   *0x5c(%edi)
 80495b9:	82                   	(bad)  
 80495ba:	04 08                	add    $0x8,%al
 80495bc:	ff                   	(bad)  
 80495bd:	ff                   	(bad)  
 80495be:	ff 6f 01             	ljmp   *0x1(%edi)
 80495c1:	00 00                	add    %al,(%eax)
 80495c3:	00 f0                	add    %dh,%al
 80495c5:	ff                   	(bad)  
 80495c6:	ff 6f 50             	ljmp   *0x50(%edi)
 80495c9:	82                   	(bad)  
 80495ca:	04 08                	add    $0x8,%al
	...

Disassembly of section .got:

080495fc <.got>:
 80495fc:	00 00                	add    %al,(%eax)
	...

Disassembly of section .got.plt:

08049600 <_GLOBAL_OFFSET_TABLE_>:
 8049600:	2c 95                	sub    $0x95,%al
 8049602:	04 08                	add    $0x8,%al
	...
 804960c:	e2 82                	loop   8049590 <_DYNAMIC+0x64>
 804960e:	04 08                	add    $0x8,%al
 8049610:	f2 82                	repnz (bad) 
 8049612:	04 08                	add    $0x8,%al
 8049614:	02                   	.byte 0x2
 8049615:	83                   	.byte 0x83
 8049616:	04 08                	add    $0x8,%al

Disassembly of section .data:

08049618 <__data_start>:
 8049618:	00 00                	add    %al,(%eax)
	...

0804961c <__dso_handle>:
 804961c:	00 00                	add    %al,(%eax)
	...

Disassembly of section .bss:

08049620 <completed.5982>:
 8049620:	00 00                	add    %al,(%eax)
	...

08049624 <dtor_idx.5984>:
 8049624:	00 00                	add    %al,(%eax)
	...

08049628 <seed.1248>:
 8049628:	00 00                	add    %al,(%eax)
	...

Disassembly of section .comment:

00000000 <.comment>:
   0:	47                   	inc    %edi
   1:	43                   	inc    %ebx
   2:	43                   	inc    %ebx
   3:	3a 20                	cmp    (%eax),%ah
   5:	28 44 65 62          	sub    %al,0x62(%ebp,%eiz,2)
   9:	69 61 6e 20 34 2e 34 	imul   $0x342e3420,0x6e(%ecx),%esp
  10:	2e                   	cs
  11:	35 2d 38 29 20       	xor    $0x2029382d,%eax
  16:	34 2e                	xor    $0x2e,%al
  18:	34 2e                	xor    $0x2e,%al
  1a:	35                   	.byte 0x35
	...

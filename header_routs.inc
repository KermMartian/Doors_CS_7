Cn2BasicOnHook:
;ok we have to check if its a onkey intterupts...wherere is tix code
	in a,(4)
	push af
	call Cn2_Int_Done_TIOSInterrupt
	pop af
	rrca    ;puts bit 0 into carry to see if ON key gens interrupt. 4cc savings.
	ret nc   ; quit if not an onintterupt not our place to handle it
	;if we get here the intterupt didnt trigger an on break this happens for some reason.
	; so now we setup for an bjump jerror
	ld a,E_BREAK
	bjump($44d7) ;jerror

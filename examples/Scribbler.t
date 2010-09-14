module Scribbler where

import DOM

root world = class
	Document {..} = new htmlDOM world

	c = new canvas [OnMousedown penDown, OnMouseup penUp, OnMousemove move] []
	CANVAS ctx = c
            
	drawing := False
	
	penDown ev = action
		ctx.beginPath
		ctx.moveTo ev.clientX ev.clientY
		drawing := True

	penUp ev = action
		drawing := False

	move ev = action
	    	if drawing then
			ctx.lineTo ev.clientX ev.clientY
		    	ctx.stroke

	clear _ = action
		ctx.clearRect 0 0 300 300
		
	elem = new div [] [wrap c, hr [], inputButton [Value "Clear", OnClick clear]]

	result action
		body.appendChild elem

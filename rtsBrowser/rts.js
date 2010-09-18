Array.prototype.oldConcat = Array.prototype.concat
Array.prototype.concat = function(tail) {
	if (this.length > 0 && typeof(this[0]) == "string" && typeof(tail) == "string") {
		var s = this.join("")
	 	return s.concat.apply(s,arguments)
	} else if (tail != undefined && tail.isNil == undefined) {
		return new Cyclic(this,tail,0)
	} else { 
		return this.oldConcat.apply(this,arguments) 
	}
}

function RAISE (err) {
	switch (err) {
		case 1: throw 'Pattern-match error'
		case 2: throw 'Illegal forward reference'
		case 3: throw 'Deadlock'
		default: throw ('Unknown exception: ' + err)
	}
}

Raise = RAISE

var Inherit = 0

function ASYNC(f,after,before) {
	if (after == undefined)
		after = 0
	return window.setTimeout(f,after)
}

function ABORT(msg) {
	window.clearTimeout(msg)
}

function LOCK(r) {
	if (r.LOCKED)
		RAISE(3)
	r.LOCKED = true
	return r
}

function UNLOCK(r) {
	r.LOCKED = false
}

function UPDATE(ptr,obj) {
	switch (typeof(obj)) {
		case "function":
			return obj
		case "string":   
			ptr.toString = function() { return obj }
			break
		case "object":
			if (obj.nodeType != undefined)
				RAISE(2)
			var names = Object.getOwnPropertyNames(obj)
			for (var i = 0; i < names.length; i++)
				ptr[names[i]] = obj[names[i]]
			ptr.__proto__ = obj.__proto__
			break
		default:
			throw "Internal: Cannot overwrite cyclic placeholder..."
	}
	return ptr
}

Array.prototype.isNil = function() {
	return this.length == 0
}
Array.prototype.strict = function() {
	return this
}

function Cyclic(prefix,tail,offset) {
	this.prefix = prefix
	this.tail = tail
	this.offset = offset
	this["0"] = prefix[offset]
}

Cyclic.prototype = { 
	isNil: function () {
		return false
	},
	slice: function (one) {
		var off = this.offset + 1
		return off == this.prefix.length ? this.tail : new Cyclic(this.prefix, this.tail, off)
	},
	strict: function () {
		if (this.tail.inProgress)
			RAISE(2)
		if (this.tail.strict == undefined)
			RAISE(2)
		this.inProgress = true
		var strictTail = this.tail.strict()
		var that = this.prefix.concat()
		for (var i = 0; i < strictTail.length; i++)
			that.push(strictTail[i])
		this.inProgress = undefined
		return that
	}
}

Document.prototype.oldCreateTextNode = Document.prototype.createTextNode
Document.prototype.createTextNode = function(str) { 
	var n = this.oldCreateTextNode(str)
	n.tagName = "TEXT"
	n.a = n.nodeValue 
	return n
}


Node.prototype.oldRemoveChild = Node.prototype.removeChild
Node.prototype.removeChild = function (i) {
	var refObj = typeof(i) == "number" ? this.childNodes[i] : i
	return refObj != undefined ? this.oldRemoveChild(refObj) : undefined
}
Node.prototype.oldReplaceChild = Node.prototype.replaceChild
Node.prototype.replaceChild = function (newObj, i) {
	var refObj = typeof(i) == "number" ? this.childNodes[i] : i
	if (newObj.nodeType == undefined) {
		newObj = document.createTextNode(newObj.a)
	}
	return refObj != undefined ? this.oldReplaceChild(newObj, refObj) : undefined
}
Node.prototype.oldInsertBefore = Node.prototype.insertBefore
Node.prototype.insertBefore = function (newObj, i) {
	var refObj = typeof(i) == "number" ? this.childNodes[i] : i
	if (newObj.nodeType == undefined) {
		newObj = document.createTextNode(newObj.a)
	}
	return this.oldInsertBefore(newObj, refObj)
}
Node.prototype.oldAppendChild = Node.prototype.appendChild
Node.prototype.appendChild = function (newObj) {
	if (newObj.nodeType == undefined) {
		newObj = document.createTextNode(newObj.a)
	}
	return this.oldAppendChild(newObj)
}
Node.prototype.getChildren = function() {
	var list = []
	for (var i = 0; i < this.childNodes.length; i++) {
		list.push(this.childNodes[i])
	}
	return list
}
Node.prototype.getInnerText = function () {
	return this.innerText
}
Node.prototype.getInnerHTML = function () {
	return this.innerHTML
}

function AttrValFromStr(tag,str) {
	switch (attrTypes[tag]) {
		case "str":
		case "enum":
			return str
		case "strings":
		case "enumset":
			return str.split(" ")
		case "bool":
			return str == "true" ? true : false
		case "int":
			return parseInt(str)
		case "id":
			return document.getElementById(str)
		case "flag":
			return true
		case "target":
			return str[0] == "_" ? str.substring(1) : {tagName:"T", a:str}
	}
	return undefined
}
function AttrValToStr(tag,val) {
	switch (attrTypes[tag]) {
		case "str":
		case "enum":
			return val
		case "strings":
		case "enumset":
			return val.join(" ")
		case "bool":
		case "int":
			return String(val)
		case "id":
			return val.id
		case "flag":
			return val ? val : undefined
		case "action":
			return val
		case "target":
			return typeof(val) == "str" ? "_"+val : val.a
	}
	return undefined
}
Element.prototype.getAttributes = function () {
	var list = []
	var attrs = this.attributes
	for (var i = 0; i < attrs.length; i++) {
		var tag = attrs[i].name
		var val = AttrValFromStr(tag,attrs[i].value)
		list.push({tagName:tag, a:val})
	}
	for (var ev in eventAttrs) {
		if (this[ev] != null) {
			list.push({tagName:ev, a:this[ev]})
		}
	}
	return list
}
Element.prototype.removeAttributes = function (pred) {
	var attrs = this.attributes
	for (var i = 0; i < attrs.length; i++) {
		var tag = attrs[i].name
		var val = AttrValFromStr(tag, attrs[i].value)
		if (pred({tagName:tag, a:val})) {
			this.removeAttribute(tag)
		}
	}
	for (var ev in eventAttrs) {
		if (this[ev] != null && pred({tagName:ev, a:this[ev]})) {
			this[ev] = null
		}
	}
}
Element.prototype.setAttributes = function (list) {
	list = list.strict()
	for (var i = 0; i < list.length; i++) {
		var tag = list[i].tagName
		var val = AttrValToStr(tag, list[i].a)
		if (typeof(val) == "function") {
			this[tag] = val
		} else if (val != undefined) {
			this.setAttribute(tag, val)
		} else {
			this.removeAttribute(tag)
		}
	}
}
var eventAttrs = [
		"onunload",
		"onabort",
		"onerror",
		"onselect",
		"onchange",
		"onsubmit",
		"onfocus",
		"onblur",
		"onscroll",
		"oninput",
		"onclick",
		"ondblclick",
		"onmousedown",
		"onmouseup",
		"onmousemove",
		"onmouseover",
		"onmouseout",
		"onkeydown",
		"onkeypress",
		"onkeyup"
]
var attrTypes = {
		accesskey:"str",
		classname:"strings",
		contenteditable:"bool",
		contextmenu:"id",
		dir:"enum",
		draggable:"bool",
		hidden:"bool",
		id:"str",
		lang:"str",
		spellcheck:"bool",
		tabindex:"int",
		title:"str",
		
		onload:"action",
		onunload:"action",
		onabort:"action",
		onerror:"action",
		onselect:"action",
		onchange:"action",
		onsubmit:"action",
		onfocus:"action",
		onblur:"action",
		onscroll:"action",
		oninput:"action",
		onclick:"action",
		ondblclick:"action",
		onmousedown:"action",
		onmouseup:"action",
		onmousemove:"action",
		onmouseover:"action",
		onmouseout:"action",
		onkeydown:"action",
		onkeypress:"action",
		onkeyup:"action",
		
		'for':"str",
		cite:"str",
		form:"str",
		disabled:"flag",
		'accept-charset':"strings",
		action:"str",
		autocomplete:"enum",
		enctype:"str",
		method:"enum",
		novalidate:"flag",
		bane:"str",
		colspan:"int",
		rowspan:"int",
		headers:"strings",
		span:"int",
		alt:"str",
		ismap:"flag",
		usemap:"str",
		width:"int",
		heigh:"int",
		src:"str",
		pubdate:"flag",
		datetime:"str",
		href:"str",
		ping:"strings",
		rel:"enumset",
		media:"strings",
		hreflang:"str",
		target:"target",
		type:"str",
		value:"str"
}


var idCount = 0

function HtmlDoc(d) {
	this.doc = d
	this.body = d.body
}

HtmlDoc.prototype = {
		
		taggedElement : function (tag, attrs, classes) {
			var e = this.doc.createElement(tag)
			e.a = e
			idCount++
			e.id = String(idCount)
			e.setAttributes(attrs)
			classes = classes.strict()
			for (var i = 0; i < classes.length; i++) {
				var obj = classes[i]()
				e.appendChild(obj)
			}
			if (tag == "CANVAS")
				e.a = e.getContext('2d')
			return e
		},

		a : function (attrs,classes) {
			return this.taggedElement("A", attrs, classes)
		},
		em : function (attrs,classes) {
			return this.taggedElement("EM", attrs, classes)
		},
		strong : function (attrs,classes) {
			return this.taggedElement("STRONG", attrs, classes)
		},
		small : function (attrs,classes) {
			return this.taggedElement("SMALL", attrs, classes)
		},
		cite : function (attrs,classes) {
			return this.taggedElement("CITE", attrs, classes)
		},
		q : function (attrs,classes) {
			return this.taggedElement("Q", attrs, classes)
		},
		dfn : function (attrs,classes) {
			return this.taggedElement("DFN", attrs, classes)
		},
		abbr : function (attrs,classes) {
			return this.taggedElement("ABBR", attrs, classes)
		},
		time : function (attrs,classes) {
			return this.taggedElement("TIME", attrs, classes)
		},
		code : function (attrs,classes) {
			return this.taggedElement("CODE", attrs, classes)
		},
		'var' : function (attrs,classes) {
			return this.taggedElement("VAR", attrs, classes)
		},
		samp : function (attrs,classes) {
			return this.taggedElement("SAMP", attrs, classes)
		},
		kbd : function (attrs,classes) {
			return this.taggedElement("KBD", attrs, classes)
		},
		sub : function (attrs,classes) {
			return this.taggedElement("SUB", attrs, classes)
		},
		sup : function (attrs,classes) {
			return this.taggedElement("SUP", attrs, classes)
		},
		i : function (attrs,classes) {
			return this.taggedElement("I", attrs, classes)
		},
		b : function (attrs,classes) {
			return this.taggedElement("B", attrs, classes)
		},
		mark : function (attrs,classes) {
			return this.taggedElement("MARK", attrs, classes)
		},
		ruby : function (attrs,classes) {
			return this.taggedElement("RUBY", attrs, classes)
		},
		bdo : function (attrs,classes) {
			return this.taggedElement("BDO", attrs, classes)
		},
		span : function (attrs,classes) {
			return this.taggedElement("SPAN", attrs, classes)
		},
		br : function (attrs) {
			return this.taggedElement("BR", attrs, [])
		},
		wbr : function (attrs) {
			return this.taggedElement("WBR", attrs, [])
		},
		ins : function (attrs,classes) {
			return this.taggedElement("INS", attrs, classes)
		},
		del : function (attrs,classes) {
			return this.taggedElement("DEL", attrs, classes)
		},
		img : function (attrs) {
			return this.taggedElement("IMG", attrs, [])
		},
		embed : function (attrs) {
			return this.taggedElement("EMBED", attrs, [])
		},
		canvas : function (attrs,classes) {
			return this.taggedElement("CANVAS", attrs, classes)
		},
		label : function (attrs,classes) {
			return this.taggedElement("LABEL", attrs, classes)
		},
		p : function (attrs,classes) {
			return this.taggedElement("A", attrs, classes)
		},
		hr : function (attrs) {
			return this.taggedElement("HR", attrs, [])
		},
		pre : function (attrs,classes) {
			return this.taggedElement("PRE", attrs, classes)
		},
		blockquote : function (attrs,classes) {
			return this.taggedElement("BLOCKQUOTE", attrs, classes)
		},
		ol : function (attrs,classes) {
			return this.taggedElement("OL", attrs, classes)
		},
		ul : function (attrs,classes) {
			return this.taggedElement("UL", attrs, classes)
		},
		dl : function (attrs,classes) {
			return this.taggedElement("DL", attrs, classes)
		},
		figure : function (attrs,classes) {
			return this.taggedElement("FIGURE", attrs, classes)
		},
		div : function (attrs,classes) {
			return this.taggedElement("DIV", attrs, classes)
		},
		section : function (attrs,classes) {
			return this.taggedElement("SECTION", attrs, classes)
		},
		nav : function (attrs,classes) {
			return this.taggedElement("NAV", attrs, classes)
		},
		article : function (attrs,classes) {
			return this.taggedElement("ARTICLE", attrs, classes)
		},
		aside : function (attrs,classes) {
			return this.taggedElement("ASIDE", attrs, classes)
		},
		header : function (attrs,classes) {
			return this.taggedElement("HEADER", attrs, classes)
		},
		footer : function (attrs,classes) {
			return this.taggedElement("FOOTER", attrs, classes)
		},
		address : function (attrs,classes) {
			return this.taggedElement("ADDRESS", attrs, classes)
		},
		hgroup : function (attrs,classes) {
			return this.taggedElement("HGROUP", attrs, classes)
		},
		table : function (attrs,classes) {
			return this.taggedElement("TABLE", attrs, classes)
		},
		form : function (attrs,classes) {
			return this.taggedElement("FORM", attrs, classes)
		},
		fieldset : function (attrs,classes) {
			return this.taggedElement("FIELDSET", attrs, classes)
		},
		legend : function (attrs,classes) {
			return this.taggedElement("LEGEND", attrs, classes)
		},
		caption : function (attrs,classes) {
			return this.taggedElement("CAPTION", attrs, classes)
		},
		colgroup : function (attrs,classes) {
			return this.taggedElement("COLGROUP", attrs, classes)
		},
		tbody : function (attrs,classes) {
			return this.taggedElement("TBODY", attrs, classes)
		},
		thead : function (attrs,classes) {
			return this.taggedElement("THEAD", attrs, classes)
		},
		tfoot : function (attrs,classes) {
			return this.taggedElement("TFOOT", attrs, classes)
		},
		tr : function (attrs,classes) {
			return this.taggedElement("TR", attrs, classes)
		},
		th : function (attrs,classes) {
			return this.taggedElement("TH", attrs, classes)
		},
		td : function (attrs,classes) {
			return this.taggedElement("TD", attrs, classes)
		},
		col : function (attrs,classes) {
			return this.taggedElement("COL", attrs, classes)
		},
		h1 : function (attrs,classes) {
			return this.taggedElement("H1", attrs, classes)
		},
		h2 : function (attrs,classes) {
			return this.taggedElement("H2", attrs, classes)
		},
		h3 : function (attrs,classes) {
			return this.taggedElement("H3", attrs, classes)
		},
		h4 : function (attrs,classes) {
			return this.taggedElement("H4", attrs, classes)
		},
		h5 : function (attrs,classes) {
			return this.taggedElement("H5", attrs, classes)
		},
		h6 : function (attrs,classes) {
			return this.taggedElement("H6", attrs, classes)
		},
		figcaption : function (attrs,classes) {
			return this.taggedElement("FIGCAPTION", attrs, classes)
		},
		dt : function (attrs,classes) {
			return this.taggedElement("DT", attrs, classes)
		},
		dd : function (attrs,classes) {
			return this.taggedElement("DD", attrs, classes)
		},
		li : function (attrs,classes) {
			return this.taggedElement("LI", attrs, classes)
		},
		menu : function (attrs,classes) {
			return this.taggedElement("MENU", attrs, classes)
		},
		inputButton : function(attrs) {
			return this.taggedElement("INPUT", [{tagName:"type",a:"button"}].concat(attrs), [])
		},
		text : function (str) {
			return this.doc.createTextNode(str)
		},
		close : function () {
			return this.doc.close()
		}
		
}

function wrap_DOM(a) {
	return a
}

function htmlDOM_DOM() {
	return new HtmlDoc(document)
}

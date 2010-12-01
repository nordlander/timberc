Document.prototype.oldCreateTextNode = Document.prototype.createTextNode
Document.prototype.createTextNode = function(str) { 
	var n = this.oldCreateTextNode(str)
	n.tagName = "TEXT"
	n.a = n.nodeValue 
	return n
}
Document.prototype.taggedElement = function (tag, attrs, children) {
	var e = this.createElement(tag)
	e.a = e
	idCount++
	e.id = String(idCount)
	e.setAttributes(attrs)
	children = children.strict()
	for (var i = 0; i < children.length; i++)
		e.appendChild(children[i])
	if (tag == "CANVAS")
		e.a = e.getContext('2d')
	return e
}
Document.prototype.domify = function (elem) {
	if (elem.nodeType == undefined) {
		if (elem.tagName == "TEXT") {
			elem = this.createTextNode(elem.a)
		} else {
			elem = this.taggedElement(elem.tagName, elem.a.getAttributes(), elem.a.getChildren())
		}
	}
	return elem
}

Node.prototype.oldRemoveChild = Node.prototype.removeChild
Node.prototype.removeChild = function (i) {
	var refObj = typeof(i) == "number" ? this.childNodes[i] : i
	return refObj != undefined ? this.oldRemoveChild(refObj) : undefined
}
Node.prototype.oldReplaceChild = Node.prototype.replaceChild
Node.prototype.replaceChild = function (newObj, i) {
	var refObj = typeof(i) == "number" ? this.childNodes[i] : i
	return refObj != undefined ? this.oldReplaceChild(this.ownerDocument.domify(newObj), refObj) : undefined
}
Node.prototype.oldInsertBefore = Node.prototype.insertBefore
Node.prototype.insertBefore = function (newObj, i) {
	var refObj = typeof(i) == "number" ? this.childNodes[i] : i
	return this.oldInsertBefore(this.ownerDocument.domify(newObj), refObj)
}
Node.prototype.oldAppendChild = Node.prototype.appendChild
Node.prototype.appendChild = function (newObj) {
	return this.oldAppendChild(this.ownerDocument.domify(newObj))
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
			return normString(val)
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
		if (tag != undefined) {
			var val = AttrValFromStr(tag,attrs[i].value)
			list.push({tagName:tag, a:val})
		}
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
	this.console = { log: function(x,dummy) { console.log(x) },
	                 warn: function(x,dummy) { console.warn(x) },
			 error: function(x,dummy) { console.error(x) }
		       }
}

HtmlDoc.prototype = {
		
		a : function (attrs,children) {
			return this.doc.taggedElement("A", attrs, children)
		},
		em : function (attrs,children) {
			return this.doc.taggedElement("EM", attrs, children)
		},
		strong : function (attrs,children) {
			return this.doc.taggedElement("STRONG", attrs, children)
		},
		small : function (attrs,children) {
			return this.doc.taggedElement("SMALL", attrs, children)
		},
		cite : function (attrs,children) {
			return this.doc.taggedElement("CITE", attrs, children)
		},
		q : function (attrs,children) {
			return this.doc.taggedElement("Q", attrs, children)
		},
		dfn : function (attrs,children) {
			return this.doc.taggedElement("DFN", attrs, children)
		},
		abbr : function (attrs,children) {
			return this.doc.taggedElement("ABBR", attrs, children)
		},
		time : function (attrs,children) {
			return this.doc.taggedElement("TIME", attrs, children)
		},
		code : function (attrs,children) {
			return this.doc.taggedElement("CODE", attrs, children)
		},
		'var' : function (attrs,children) {
			return this.doc.taggedElement("VAR", attrs, children)
		},
		samp : function (attrs,children) {
			return this.doc.taggedElement("SAMP", attrs, children)
		},
		kbd : function (attrs,children) {
			return this.doc.taggedElement("KBD", attrs, children)
		},
		sub : function (attrs,children) {
			return this.doc.taggedElement("SUB", attrs, children)
		},
		sup : function (attrs,children) {
			return this.doc.taggedElement("SUP", attrs, children)
		},
		i : function (attrs,children) {
			return this.doc.taggedElement("I", attrs, children)
		},
		b : function (attrs,children) {
			return this.doc.taggedElement("B", attrs, children)
		},
		mark : function (attrs,children) {
			return this.doc.taggedElement("MARK", attrs, children)
		},
		ruby : function (attrs,children) {
			return this.doc.taggedElement("RUBY", attrs, children)
		},
		bdo : function (attrs,children) {
			return this.doc.taggedElement("BDO", attrs, children)
		},
		span : function (attrs,children) {
			return this.doc.taggedElement("SPAN", attrs, children)
		},
		br : function (attrs) {
			return this.doc.taggedElement("BR", attrs, [])
		},
		wbr : function (attrs) {
			return this.doc.taggedElement("WBR", attrs, [])
		},
		ins : function (attrs,children) {
			return this.doc.taggedElement("INS", attrs, children)
		},
		del : function (attrs,children) {
			return this.doc.taggedElement("DEL", attrs, children)
		},
		img : function (attrs) {
			return this.doc.taggedElement("IMG", attrs, [])
		},
		embed : function (attrs) {
			return this.doc.taggedElement("EMBED", attrs, [])
		},
		canvas : function (attrs,children) {
			return this.doc.taggedElement("CANVAS", attrs, children)
		},
		label : function (attrs,children) {
			return this.doc.taggedElement("LABEL", attrs, children)
		},
		p : function (attrs,children) {
			return this.doc.taggedElement("A", attrs, children)
		},
		hr : function (attrs) {
			return this.doc.taggedElement("HR", attrs, [])
		},
		pre : function (attrs,children) {
			return this.doc.taggedElement("PRE", attrs, children)
		},
		blockquote : function (attrs,children) {
			return this.doc.taggedElement("BLOCKQUOTE", attrs, children)
		},
		ol : function (attrs,children) {
			return this.doc.taggedElement("OL", attrs, children)
		},
		ul : function (attrs,children) {
			return this.doc.taggedElement("UL", attrs, children)
		},
		dl : function (attrs,children) {
			return this.doc.taggedElement("DL", attrs, children)
		},
		figure : function (attrs,children) {
			return this.doc.taggedElement("FIGURE", attrs, children)
		},
		div : function (attrs,children) {
			return this.doc.taggedElement("DIV", attrs, children)
		},
		section : function (attrs,children) {
			return this.doc.taggedElement("SECTION", attrs, children)
		},
		nav : function (attrs,children) {
			return this.doc.taggedElement("NAV", attrs, children)
		},
		article : function (attrs,children) {
			return this.doc.taggedElement("ARTICLE", attrs, children)
		},
		aside : function (attrs,children) {
			return this.doc.taggedElement("ASIDE", attrs, children)
		},
		header : function (attrs,children) {
			return this.doc.taggedElement("HEADER", attrs, children)
		},
		footer : function (attrs,children) {
			return this.doc.taggedElement("FOOTER", attrs, children)
		},
		address : function (attrs,children) {
			return this.doc.taggedElement("ADDRESS", attrs, children)
		},
		hgroup : function (attrs,children) {
			return this.doc.taggedElement("HGROUP", attrs, children)
		},
		table : function (attrs,children) {
			return this.doc.taggedElement("TABLE", attrs, children)
		},
		form : function (attrs,children) {
			return this.doc.taggedElement("FORM", attrs, children)
		},
		fieldset : function (attrs,children) {
			return this.doc.taggedElement("FIELDSET", attrs, children)
		},
		legend : function (attrs,children) {
			return this.doc.taggedElement("LEGEND", attrs, children)
		},
		caption : function (attrs,children) {
			return this.doc.taggedElement("CAPTION", attrs, children)
		},
		colgroup : function (attrs,children) {
			return this.doc.taggedElement("COLGROUP", attrs, children)
		},
		tbody : function (attrs,children) {
			return this.doc.taggedElement("TBODY", attrs, children)
		},
		thead : function (attrs,children) {
			return this.doc.taggedElement("THEAD", attrs, children)
		},
		tfoot : function (attrs,children) {
			return this.doc.taggedElement("TFOOT", attrs, children)
		},
		tr : function (attrs,children) {
			return this.doc.taggedElement("TR", attrs, children)
		},
		th : function (attrs,children) {
			return this.doc.taggedElement("TH", attrs, children)
		},
		td : function (attrs,children) {
			return this.doc.taggedElement("TD", attrs, children)
		},
		col : function (attrs,children) {
			return this.doc.taggedElement("COL", attrs, children)
		},
		h1 : function (attrs,children) {
			return this.doc.taggedElement("H1", attrs, children)
		},
		h2 : function (attrs,children) {
			return this.doc.taggedElement("H2", attrs, children)
		},
		h3 : function (attrs,children) {
			return this.doc.taggedElement("H3", attrs, children)
		},
		h4 : function (attrs,children) {
			return this.doc.taggedElement("H4", attrs, children)
		},
		h5 : function (attrs,children) {
			return this.doc.taggedElement("H5", attrs, children)
		},
		h6 : function (attrs,children) {
			return this.doc.taggedElement("H6", attrs, children)
		},
		figcaption : function (attrs,children) {
			return this.doc.taggedElement("FIGCAPTION", attrs, children)
		},
		dt : function (attrs,children) {
			return this.doc.taggedElement("DT", attrs, children)
		},
		dd : function (attrs,children) {
			return this.doc.taggedElement("DD", attrs, children)
		},
		li : function (attrs,children) {
			return this.doc.taggedElement("LI", attrs, children)
		},
		menu : function (attrs,children) {
			return this.doc.taggedElement("MENU", attrs, children)
		},
		inputButton : function(attrs) {
			return this.doc.taggedElement("INPUT", [{tagName:"type",a:"button"}].concat(attrs), [])
		},
		close : function () {
			return this.doc.close()
		}
		
}

function htmlDOM_DOM() {
	return new HtmlDoc(document)
}

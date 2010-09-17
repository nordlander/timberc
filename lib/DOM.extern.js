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
		
		taggedElement : function (tag, attrs, children) {
			var e = this.doc.createElement(tag)
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
		},

		a : function (attrs,children) {
			return this.taggedElement("A", attrs, children)
		},
		em : function (attrs,children) {
			return this.taggedElement("EM", attrs, children)
		},
		strong : function (attrs,children) {
			return this.taggedElement("STRONG", attrs, children)
		},
		small : function (attrs,children) {
			return this.taggedElement("SMALL", attrs, children)
		},
		cite : function (attrs,children) {
			return this.taggedElement("CITE", attrs, children)
		},
		q : function (attrs,children) {
			return this.taggedElement("Q", attrs, children)
		},
		dfn : function (attrs,children) {
			return this.taggedElement("DFN", attrs, children)
		},
		abbr : function (attrs,children) {
			return this.taggedElement("ABBR", attrs, children)
		},
		time : function (attrs,children) {
			return this.taggedElement("TIME", attrs, children)
		},
		code : function (attrs,children) {
			return this.taggedElement("CODE", attrs, children)
		},
		'var' : function (attrs,children) {
			return this.taggedElement("VAR", attrs, children)
		},
		samp : function (attrs,children) {
			return this.taggedElement("SAMP", attrs, children)
		},
		kbd : function (attrs,children) {
			return this.taggedElement("KBD", attrs, children)
		},
		sub : function (attrs,children) {
			return this.taggedElement("SUB", attrs, children)
		},
		sup : function (attrs,children) {
			return this.taggedElement("SUP", attrs, children)
		},
		i : function (attrs,children) {
			return this.taggedElement("I", attrs, children)
		},
		b : function (attrs,children) {
			return this.taggedElement("B", attrs, children)
		},
		mark : function (attrs,children) {
			return this.taggedElement("MARK", attrs, children)
		},
		ruby : function (attrs,children) {
			return this.taggedElement("RUBY", attrs, children)
		},
		bdo : function (attrs,children) {
			return this.taggedElement("BDO", attrs, children)
		},
		span : function (attrs,children) {
			return this.taggedElement("SPAN", attrs, children)
		},
		br : function (attrs) {
			return this.taggedElement("BR", attrs, [])
		},
		wbr : function (attrs) {
			return this.taggedElement("WBR", attrs, [])
		},
		ins : function (attrs,children) {
			return this.taggedElement("INS", attrs, children)
		},
		del : function (attrs,children) {
			return this.taggedElement("DEL", attrs, children)
		},
		img : function (attrs) {
			return this.taggedElement("IMG", attrs, [])
		},
		embed : function (attrs) {
			return this.taggedElement("EMBED", attrs, [])
		},
		canvas : function (attrs,children) {
			return this.taggedElement("CANVAS", attrs, children)
		},
		label : function (attrs,children) {
			return this.taggedElement("LABEL", attrs, children)
		},
		p : function (attrs,children) {
			return this.taggedElement("A", attrs, children)
		},
		hr : function (attrs) {
			return this.taggedElement("HR", attrs, [])
		},
		pre : function (attrs,children) {
			return this.taggedElement("PRE", attrs, children)
		},
		blockquote : function (attrs,children) {
			return this.taggedElement("BLOCKQUOTE", attrs, children)
		},
		ol : function (attrs,children) {
			return this.taggedElement("OL", attrs, children)
		},
		ul : function (attrs,children) {
			return this.taggedElement("UL", attrs, children)
		},
		dl : function (attrs,children) {
			return this.taggedElement("DL", attrs, children)
		},
		figure : function (attrs,children) {
			return this.taggedElement("FIGURE", attrs, children)
		},
		div : function (attrs,children) {
			return this.taggedElement("DIV", attrs, children)
		},
		section : function (attrs,children) {
			return this.taggedElement("SECTION", attrs, children)
		},
		nav : function (attrs,children) {
			return this.taggedElement("NAV", attrs, children)
		},
		article : function (attrs,children) {
			return this.taggedElement("ARTICLE", attrs, children)
		},
		aside : function (attrs,children) {
			return this.taggedElement("ASIDE", attrs, children)
		},
		header : function (attrs,children) {
			return this.taggedElement("HEADER", attrs, children)
		},
		footer : function (attrs,children) {
			return this.taggedElement("FOOTER", attrs, children)
		},
		address : function (attrs,children) {
			return this.taggedElement("ADDRESS", attrs, children)
		},
		hgroup : function (attrs,children) {
			return this.taggedElement("HGROUP", attrs, children)
		},
		table : function (attrs,children) {
			return this.taggedElement("TABLE", attrs, children)
		},
		form : function (attrs,children) {
			return this.taggedElement("FORM", attrs, children)
		},
		fieldset : function (attrs,children) {
			return this.taggedElement("FIELDSET", attrs, children)
		},
		legend : function (attrs,children) {
			return this.taggedElement("LEGEND", attrs, children)
		},
		caption : function (attrs,children) {
			return this.taggedElement("CAPTION", attrs, children)
		},
		colgroup : function (attrs,children) {
			return this.taggedElement("COLGROUP", attrs, children)
		},
		tbody : function (attrs,children) {
			return this.taggedElement("TBODY", attrs, children)
		},
		thead : function (attrs,children) {
			return this.taggedElement("THEAD", attrs, children)
		},
		tfoot : function (attrs,children) {
			return this.taggedElement("TFOOT", attrs, children)
		},
		tr : function (attrs,children) {
			return this.taggedElement("TR", attrs, children)
		},
		th : function (attrs,children) {
			return this.taggedElement("TH", attrs, children)
		},
		td : function (attrs,children) {
			return this.taggedElement("TD", attrs, children)
		},
		col : function (attrs,children) {
			return this.taggedElement("COL", attrs, children)
		},
		h1 : function (attrs,children) {
			return this.taggedElement("H1", attrs, children)
		},
		h2 : function (attrs,children) {
			return this.taggedElement("H2", attrs, children)
		},
		h3 : function (attrs,children) {
			return this.taggedElement("H3", attrs, children)
		},
		h4 : function (attrs,children) {
			return this.taggedElement("H4", attrs, children)
		},
		h5 : function (attrs,children) {
			return this.taggedElement("H5", attrs, children)
		},
		h6 : function (attrs,children) {
			return this.taggedElement("H6", attrs, children)
		},
		figcaption : function (attrs,children) {
			return this.taggedElement("FIGCAPTION", attrs, children)
		},
		dt : function (attrs,children) {
			return this.taggedElement("DT", attrs, children)
		},
		dd : function (attrs,children) {
			return this.taggedElement("DD", attrs, children)
		},
		li : function (attrs,children) {
			return this.taggedElement("LI", attrs, children)
		},
		menu : function (attrs,children) {
			return this.taggedElement("MENU", attrs, children)
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

module DOM where

struct Document where
	close  :: Request ()
	body   :: Element BodyAttr FlowContent

	a      :: [AnchorAttr ARel] -> [PhrasingContent] -> Class PhrasingContent
	em     :: [GlobalAttr] -> [PhrasingContent] -> Class PhrasingContent
	strong :: [GlobalAttr] -> [PhrasingContent] -> Class PhrasingContent
	small  :: [GlobalAttr] -> [PhrasingContent] -> Class PhrasingContent
	cite   :: [GlobalAttr] -> [PhrasingContent] -> Class PhrasingContent
	q      :: [QuoteAttr] -> [PhrasingContent] -> Class PhrasingContent
	dfn    :: [GlobalAttr] -> [PhrasingContent] -> Class PhrasingContent
	abbr   :: [GlobalAttr] -> [PhrasingContent] -> Class PhrasingContent
	time   :: [TimeAttr] -> [PhrasingContent] -> Class PhrasingContent
	code   :: [GlobalAttr] -> [PhrasingContent] -> Class PhrasingContent
	var    :: [GlobalAttr] -> [PhrasingContent] -> Class PhrasingContent
	samp   :: [GlobalAttr] -> [PhrasingContent] -> Class PhrasingContent
	kbd    :: [GlobalAttr] -> [PhrasingContent] -> Class PhrasingContent
	sub    :: [GlobalAttr] -> [PhrasingContent] -> Class PhrasingContent
	sup    :: [GlobalAttr] -> [PhrasingContent] -> Class PhrasingContent
	i      :: [GlobalAttr] -> [PhrasingContent] -> Class PhrasingContent
	b      :: [GlobalAttr] -> [PhrasingContent] -> Class PhrasingContent
	mark   :: [GlobalAttr] -> [PhrasingContent] -> Class PhrasingContent
	ruby   :: [GlobalAttr] -> [RubyContent] -> Class PhrasingContent
	bdo    :: [GlobalAttr] -> [PhrasingContent] -> Class PhrasingContent
	span   :: [GlobalAttr] -> [PhrasingContent] -> Class PhrasingContent
	br     :: [GlobalAttr] -> Class PhrasingContent
	wbr    :: [GlobalAttr] -> Class PhrasingContent
	ins    :: [EditAttr] -> [PhrasingContent] -> Class PhrasingContent
	del    :: [EditAttr] -> [PhrasingContent] -> Class PhrasingContent
	img    :: [GlobalAttr] -> Class PhrasingContent
	embed  :: [GlobalAttr] -> Class PhrasingContent
	canvas :: [CanvasAttr] -> [PhrasingContent] -> Class PhrasingContent
	label  :: [LabelAttr] -> [PhrasingContent] -> Class PhrasingContent
	p          :: [GlobalAttr] -> [PhrasingContent] -> Class FlowContent
	hr         :: [GlobalAttr] -> Class FlowContent
	pre        :: [QuoteAttr] -> [FlowContent] -> Class FlowContent
	blockquote :: [GlobalAttr] -> [PhrasingContent] -> Class FlowContent
	ol         :: [OListAttr] -> [ListContent] -> Class FlowContent
	ul         :: [GlobalAttr] -> [ListContent] -> Class FlowContent
	dl         :: [GlobalAttr] -> [DefinitionContent] -> Class FlowContent
	figure     :: [GlobalAttr] -> [FigureContent] -> Class FlowContent
	div        :: [GlobalAttr] -> [FlowContent] -> Class FlowContent
	section    :: [GlobalAttr] -> [FlowContent] -> Class FlowContent
	nav        :: [GlobalAttr] -> [FlowContent] -> Class FlowContent
	article    :: [GlobalAttr] -> [FlowContent] -> Class FlowContent
	aside      :: [GlobalAttr] -> [FlowContent] -> Class FlowContent
	header     :: [GlobalAttr] -> [FlowContent] -> Class FlowContent
	footer     :: [GlobalAttr] -> [FlowContent] -> Class FlowContent
	address    :: [GlobalAttr] -> [FlowContent] -> Class FlowContent
	hgroup     :: [GlobalAttr] -> [HContent] -> Class FlowContent
	table      :: [GlobalAttr] -> [TableContent] -> Class FlowContent
	form       :: [FormAttr] -> [FlowContent] -> Class FlowContent
	fieldset   :: [GlobalAttr] -> [FieldsetContent] -> Class FlowContent
	legend :: [GlobalAttr] -> [PhrasingContent] -> Class FieldsetContent
	caption  :: [GlobalAttr] -> [FlowContent] -> Class TableContent
	colgroup :: [SpanAttr] -> [ColContent] -> Class TableContent
	tbody    :: [GlobalAttr] -> [FlowContent] -> Class TableContent
	thead    :: [GlobalAttr] -> [FlowContent] -> Class TableContent
	tfoot    :: [GlobalAttr] -> [FlowContent] -> Class TableContent
	tr :: [GlobalAttr] -> [TRowContent] -> Class TBodyContent
	th :: [ThAttr] -> [PhrasingContent] -> Class TRowContent
	td :: [TdAttr] -> [FlowContent] -> Class TRowContent
	col :: [SpanAttr] -> Class ColContent
	h1 :: [GlobalAttr] -> [PhrasingContent] -> Class HContent
	h2 :: [GlobalAttr] -> [PhrasingContent] -> Class HContent
	h3 :: [GlobalAttr] -> [PhrasingContent] -> Class HContent
	h4 :: [GlobalAttr] -> [PhrasingContent] -> Class HContent
	h5 :: [GlobalAttr] -> [PhrasingContent] -> Class HContent
	h6 :: [GlobalAttr] -> [PhrasingContent] -> Class HContent
	figcaption :: [GlobalAttr] -> [FlowContent] -> Class FigureContent
	dt :: [GlobalAttr] -> [PhrasingContent] -> Class DefinitionContent
	dd :: [GlobalAttr] -> [PhrasingContent] -> Class DefinitionContent
	li :: [GlobalAttr] -> [FlowContent] -> Class ListContent
	rt :: [GlobalAttr] -> [PhrasingContent] -> Class RubyContent
	rp :: [GlobalAttr] -> [PhrasingContent] -> Class RubyContent
	menu :: [MenuAttr] -> [ListContent] -> Class FlowContent
	inputButton :: [InputButtonAttr] -> Class PhrasingContent

extern htmlDOM :: World -> Class Document

struct Element a c where
	getAttributes :: Request [a]
	setAttributes :: [a] -> Request ()
	removeAttributes :: (a -> Bool) -> Request ()
	
	getChildren :: Request [c]
	appendChild :: c -> Request ()
	insertBefore :: c -> Int -> Request ()
	removeChild :: Int -> Request ()
	replaceChild :: Int -> c -> Request ()
	
--	style :: CSSStyleDeclaration
	
	getInnerText :: Request String
	getInnerHTML :: Request String
	
	click :: Request ()
	scrollIntoView :: Bool -> Request ()
	focus :: Request ()
	blur :: Request ()
	
data Dir = Ltr | Rtl

data GlobalAttr = 
	AccessKey [Char]
  | ClassName [String]
  | ContentEditable Bool
  | ContextMenu (Element MenuAttr ListContent)
  | Dir Dir
  | Draggable Bool
  | Hidden Bool
  | Id String
  | Lang String
  | SpellCheck Bool
  | Tabindex Int
  | Title String

  | OnLoad (Event -> Action)
  | OnUnload (Event -> Action)
  | OnAbort (Event -> Action)
  | OnError (Event -> Action)
  | OnSelect (Event -> Action)
  | OnChange (Event -> Action)
  | OnSubmit (Event -> Action)
  | OnFocus (Event -> Action)
  |	OnBlur (Event -> Action)
  | OnScroll (Event -> Action)
  | OnInput (Event -> Action)

  | OnClick (MouseEvent -> Action)
  | OnDblclick (MouseEvent -> Action)
  | OnMousedown (MouseEvent -> Action)
  | OnMouseup (MouseEvent -> Action)
  | OnMousemove (MouseEvent -> Action)
  | OnMouseover (MouseEvent -> Action)
  | OnMouseout (MouseEvent -> Action)

  | OnKeydown (MouseEvent -> Action)
  | OnKeypress (MouseEvent -> Action)
  | OnKeyup (MouseEvent -> Action)

data BodyAttr > GlobalAttr =
	OnAfterprint (Event -> Action)
  | OnBeforeprint (Event -> Action)
  | OnBeforeunload (Event -> Action)
  | OnHaschange (Event -> Action)
  | OnMessage (Event -> Action)
  | OnOffline (Event -> Action)
  | OnOnline (Event -> Action)
  | OnPagehide (Event -> Action)
  | OnPageshow (Event -> Action)
  | OnPopstate (Event -> Action)
  | OnResize (Event -> Action)
  | OnStorage (Event -> Action)
  | OnUndo (Event -> Action)

data PhrasingContent =
	A 	 	(Element (AnchorAttr ARel) PhrasingContent)
  | EM	 	(Element GlobalAttr PhrasingContent)
  | STRONG	(Element GlobalAttr PhrasingContent)
  | SMALL	(Element GlobalAttr PhrasingContent)
  | CITE	(Element GlobalAttr PhrasingContent)
  | Q		(Element QuoteAttr PhrasingContent)
  | DFN		(Element GlobalAttr PhrasingContent)
  | ABBR	(Element GlobalAttr PhrasingContent)
  | TIME	(Element TimeAttr PhrasingContent)
  | CODE	(Element GlobalAttr PhrasingContent)
  | VAR		(Element GlobalAttr PhrasingContent)
  | SAMP	(Element GlobalAttr PhrasingContent)
  | KBD		(Element GlobalAttr PhrasingContent)
  | SUB		(Element GlobalAttr PhrasingContent)
  | SUP		(Element GlobalAttr PhrasingContent)
  | I		(Element GlobalAttr PhrasingContent)
  | B		(Element GlobalAttr PhrasingContent)
  | MARK	(Element GlobalAttr PhrasingContent)
  | RUBY	(Element GlobalAttr RubyContent)
  | BDO		(Element GlobalAttr PhrasingContent)
  | SPAN	(Element GlobalAttr PhrasingContent)
  | BR		(Element GlobalAttr EmptyContent)
  | WBR		(Element GlobalAttr EmptyContent)
  | INS		(Element EditAttr PhrasingContent)
  | DEL		(Element EditAttr PhrasingContent)
  | IMG		(Element ImageAttr EmptyContent)
  | EMBED	(Element EmbedAttr EmptyContent)
  | CANVAS	(CanvasElement CanvasAttr PhrasingContent)
  | LABEL	(Element LabelAttr PhrasingContent)
  | INPUT	(Element InputButtonAttr EmptyContent)
  | TEXT	String


data InputAttr > GlobalAttr, TypeAttr1 InputType =
	Accept 

data InputType =
  	Text		
--  | Search		
  | Tel			
  | Url			
  | Email		
  | Password	
  | Datetime	
  | Date		
  | Month		
  | Week		
  | Time		
  | DatetimeLocal 
  | Number		
  | Range		
  | Color		
  | Checkbox	
  | Radio		
  | File		
  | Submit		
  | Image		
  | Reset		
  | Button			

data LabelAttr > GlobalAttr, FormAttr1 =
	For String
	
data LabelAttr1 =
	Label String
	
data MenuAttr > GlobalAttr, TypeAttr1 MenuType, LabelAttr1

data MenuType = Context | Toolbar

struct CanvasElement a c < Element a c where
	beginPath :: Request ()
	moveTo :: Int -> Int -> Request ()
	lineTo :: Int -> Int -> Request ()
	stroke :: Request ()
	clearRect :: Int -> Int -> Int -> Int -> Request ()
	

struct ContextWebGL --

data WebGLAttribute --

data NotYetContent =
   	IFRAME	-- 
  | OBJECT	--
  | MEDIA	--

data CanvasAttr > GlobalAttr, DimensionAttr

data EditAttr > QuoteAttr, DateTimeAttr

data QuoteAttr > GlobalAttr =
    Cite URL

data FlowContent > PhrasingContent, HContent =
    P		(Element GlobalAttr PhrasingContent)
  | HR		(Element GlobalAttr EmptyContent)
  | PRE		(Element GlobalAttr PhrasingContent)
  | BLOCKQUOTE (Element QuoteAttr FlowContent)
  | OL		(Element OListAttr ListContent)
  | UL		(Element GlobalAttr ListContent)
  | DL		(Element GlobalAttr DefinitionContent)
  | FIGURE	(Element GlobalAttr FigureContent)
  | DIV		(Element GlobalAttr FlowContent)
  | SECTION	(Element GlobalAttr FlowContent)
  | NAV		(Element GlobalAttr FlowContent)
  | ARTICLE	(Element GlobalAttr FlowContent)
  | ASIDE	(Element GlobalAttr FlowContent)
  | HEADER	(Element GlobalAttr FlowContent)
  | FOOTER	(Element GlobalAttr FlowContent)
  | ADDRESS	(Element GlobalAttr FlowContent)
  | HGROUP	(Element GlobalAttr HContent)
  | TABLE	(Element GlobalAttr TableContent)
  | FORM	(FormElement FormAttr FlowContent)
  | FIELDSET (Element FieldsetAttr FieldsetContent)



data FieldsetAttr > GlobalAttr, NameAttr, DisabledAttr, FormAttr1 

data FormAttr1 =
	Form String

data DisabledAttr =
	Disabled

data FieldsetContent > FlowContent =
	LEGEND	(Element GlobalAttr PhrasingContent)



struct FormElement a c < Element a c where
	submit :: Request ()
	reset :: Request ()
	checkValidity :: Request Bool
	dispatchFormInput :: Request ()
	dispatchFormChange :: Request ()
	

data FormAttr > GlobalAttr, NameAttr, TargetAttr =
	Accept_Charset [MIME]
  | Action URL
  | Autocomplete OnOff
  | Enctype MIME
  | Method Method
  | Novalidate

data NameAttr =
	Name String


data Method = GET | POST | PUT | DELETE

data OnOff = On | Off

data TableContent =
	CAPTION	(Element GlobalAttr FlowContent)
  | COLGROUP (Element SpanAttr ColContent)
  | TBODY	(Element GlobalAttr TBodyContent)
  | THEAD	(Element GlobalAttr TBodyContent)
  | TFOOT	(Element GlobalAttr TBodyContent)



data TBodyContent =
	TR		(Element GlobalAttr TRowContent)


data TRowContent =
	TH		(Element ThAttr PhrasingContent)
  | TD		(Element TdAttr FlowContent)



data TdAttr > GlobalAttr =
	Colspan Int
  | Rowspan Int
  | Headers [String]

data ThAttr > TdAttr =
	Scope Scope

data Scope = Row | Col | Rowgroup | Colgroup | Auto

data ColContent =
	COL		(Element SpanAttr EmptyContent)



data SpanAttr > GlobalAttr =
	Span Int
	
data ImageAttr > GlobalAttr, DimensionAttr, SrcAttr, UsemapAttr =
	Alt String
  | Ismap


data UsemapAttr =
	Usemap String


data DimensionAttr =
	Width Int
  | Height Int

data EmbedAttr > GlobalAttr, DimensionAttr, SrcAttr, TypeAttr1 MIME


data HContent =
	H1		(Element GlobalAttr PhrasingContent)
  | H2		(Element GlobalAttr PhrasingContent)
  | H3		(Element GlobalAttr PhrasingContent)
  | H4		(Element GlobalAttr PhrasingContent)
  | H5		(Element GlobalAttr PhrasingContent)
  | H6		(Element GlobalAttr PhrasingContent)


data FigureContent > FlowContent =
	FIGCAPTION (Element GlobalAttr FlowContent)


data DefinitionContent =
    DT		(Element GlobalAttr PhrasingContent)
  |	DD		(Element GlobalAttr PhrasingContent)



data ListContent =
    LI		(Element GlobalAttr FlowContent)



data SrcAttr =
	Src URL

data Sandbox =
	AllowSameOrigin
  | AllowTopNavigation
  | AllowForms
  | AllowScripts


data InputButtonAttr > GlobalAttr =
    Value String


data OListAttr > GlobalAttr =
	Reversed Bool
  | Start Int

data EmptyContent

data RubyContent > PhrasingContent =
	RT		(Element GlobalAttr PhrasingContent)
  | RP		(Element GlobalAttr PhrasingContent)



data TimeAttr > GlobalAttr, DateTimeAttr =
	PubDate

data DateTimeAttr =
    DateTime DateTime

type DateTime = String

data AnchorAttr r > GlobalAttr, TypeAttr1 MIME, TargetAttr =
	Href URL
  | Ping [URL]
  | Rel [r]
  | Media [MQ]
  | Hreflang LANG

data TargetAttr =
	Target Target

data TypeAttr1 a =
  	Type a
	
data Target = Blank | Self | Parent | Top | T String

data Rel = Alternate | Archives | Author | First | Help | Index | Last
         | License | Next | Prefetch | Prev | Search | Sidebar | Tag | Up

data LRel > Rel = Icon | Pingback | Stylesheet

data ARel > Rel = Bookmark | External | Nofollow | Noreferrer

type URL = String

type MQ = String

type LANG = String

type MIME = String



struct Event where
    bubbles :: Bool
    cancelable :: Bool
--    currentTarget :: Element GlobalAttr EmptyContent
    eventPhase :: Bool
--    target :: Element GlobalAttr EmptyContent
    timeStamp :: DateTime

    preventDefault :: Request ()
    stopPropagation :: Request ()

struct MouseEvent < Event where
    altKey :: Bool
    button :: Int
    charCode :: Char
    clientX :: Int
    clientY :: Int
    ctrlKey :: Bool
    metaKey :: Bool
    screenX :: Int
    screenY :: Int
    shiftKey :: Bool



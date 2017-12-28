/// ---------------------------
/// Attribution to original authors of this code
/// ---------------------------
/// This egine follows syntax of Suave ViewEngine but completly re-written by gerardtoconnor@gmail.com.
///

module Giraffe.GiraffeViewEngine

open System
open System.Net
open System.IO
open System.Xml

/// ---------------------------
/// Definition of different HTML content
///
/// For more info check:
/// - https://developer.mozilla.org/en-US/docs/Web/HTML/Element
/// - https://www.w3.org/TR/html5/syntax.html#void-elements
/// ---------------------------

// type OXmlAttribute =
//     | KeyValue of string * string
//     | Boolean  of string

// type OXmlElement   = string * OXmlAttribute[]    // Name * XML attributes

// type OXmlNode =
//     | ParentNode  of OXmlElement * OXmlNode list // An XML element which contains nested XML elements
//     | VoidElement of OXmlElement                // An XML element which cannot contain nested XML (e.g. <hr /> or <br />)
//     | EncodedText of string                    // XML encoded text content
//     | RawText     of string                    // Raw text content

// Assumptions to provide best performance
// Each load of list item should be a struct for caching perf
// cannot use easier interface option as struct would be boxed
// test both interface objects and DU struct types
// Unions are manual to reuse type fields as don't believe DU re-uses but creates unique for each case 


/// Helper operator to add strings to writer
let inline (++) (sr:StreamWriter) (str:string) = sr.Write(str) ; sr
let inline (+>) (sr:StreamWriter) (fn:StreamWriter -> StreamWriter) = fn sr

type AttrType =
| KeyValue = 0uy
| Boolean = 1uy
type XmlAttribute =
    struct
        val AType : AttrType
        val AField : string
        val AValue : string
    end
    new (atype,afield,avalue) = {AType = atype;AField = afield ; AValue = avalue} 
    static member KeyValue afield avalue = XmlAttribute(AttrType.KeyValue,afield,avalue )
    static member Boolean  afield        = XmlAttribute(AttrType.Boolean,afield,null)
    static member Write (writer:StreamWriter,attr:XmlAttribute) =
        match attr.AType with
        | AttrType.KeyValue -> 
            writer ++ " " ++ attr.AField ++ "=" ++ attr.AValue
        | AttrType.Boolean -> 
            writer ++ " " ++ attr.AField
        | x -> failwith "Unknown Attribute provided:" + x

// type NodeType =
// | ParentNode = 0uy
// | VoidNode = 1uy
// | EncodedText = 2uy
// | RawTest = 3uy
// | Comment = 4uy

type NodeTag =
| Html = 0uy
| Base = 1uy
| Head = 2uy
| Link = 3uy
| Meta = 4uy
| Style = 5uy
| Title = 6uy
| Body = 7uy
| Address = 8uy
| Article = 9uy
| Aside = 10uy
| Footer = 11uy
| Hgroup = 12uy
| H1 = 13uy
| H2 = 14uy
| H3 = 15uy
| H4 = 16uy
| H5 = 17uy
| H6 = 18uy
| Header = 19uy
| Nav = 20uy
| Section = 21uy
| Dd = 22uy
| Div = 23uy
| Dl = 24uy
| Dt = 25uy
| Figcaption = 26uy
| Figure = 27uy
| Hr = 28uy
| Li = 29uy
| Main = 30uy
| Ol = 31uy
| P = 32uy
| Pre = 33uy
| Ul = 34uy
| A = 35uy
| Abbr = 36uy
| B = 37uy
| Bdi = 38uy
| Bdo = 39uy
| Br = 40uy
| Cite = 41uy
| Code = 42uy
| Data = 43uy
| Dfn = 44uy
| Em = 45uy
| I = 46uy
| Kbd = 47uy
| Mark = 48uy
| Q = 49uy
| Rp = 50uy
| Rt = 51uy
| Rtc = 52uy
| Ruby = 53uy
| S = 54uy
| Samp = 55uy
| Small = 56uy
| Span = 57uy
| Strong = 58uy
| Sub = 59uy
| Sup = 60uy
| Time = 61uy
| U = 62uy
| Var = 63uy
| Wbr = 64uy
| Area = 65uy
| Audio = 66uy
| Img = 67uy
| Map = 68uy
| Track = 69uy
| Video = 70uy
| Embed = 71uy
| Object = 72uy
| Param = 73uy
| Source = 74uy
| Canvas = 75uy
| Noscript = 76uy
| Script = 77uy
| Del = 78uy
| Ins = 79uy
| Caption = 80uy
| Col = 81uy
| Colgroup = 82uy
| Table = 83uy
| Tbody = 84uy
| Td = 85uy
| Tfoot = 86uy
| Th = 87uy
| Thead = 88uy
| Tr = 89uy
| Button = 90uy
| Datalist = 91uy
| Fieldset = 92uy
| Form = 93uy
| Input = 94uy
| Label = 95uy
| Legend = 96uy
| Meter = 97uy
| Optgroup = 98uy
| Option = 99uy
| Output = 100uy
| Progress = 101uy
| Select = 102uy
| Textarea = 103uy
| Details = 104uy
| Dialog = 105uy
| Menu = 106uy
| Menuitem = 107uy
| Summary = 108uy
| Encodedtext = 109uy
| Rawtext = 110uy
| Emptytext = 111uy
| Comment = 112uy

[<Literal>] let private html' = "html"

type XmlNode =
    struct
        val NTag     : NodeTag
        val Attrs : XmlAttribute list
        val Childern  : XmlNode list
        val TextVal      : string
    end
    new (ntag,attr,children,text) = { NTag = ntag ; Attrs = attr ; Childern = children ; TextVal = text } 
    static member inline ParentNode ntype attr children = XmlNode(ntype,attr,children,null)
    static member inline VoidElement ntype attr = XmlNode(ntype,attr,Unchecked.defaultof<_ list>,null)
    static member inline Text ntype text = XmlNode(ntype,Unchecked.defaultof<_ list>,Unchecked.defaultof<_ list>,text)
    
    static member private WriteParentNode (node:XmlNode) (writer:StreamWriter) =
        for attr in node.Attrs do
            XmlAttribute.Write(writer,attr) |> ignore
        writer ++ " >" |> ignore
        for child in node.Childern do
            XmlNode.Write child writer
        writer

    static member private WriteVoidNode (node:XmlNode) (writer:StreamWriter) =
        for attr in node.Attrs do
            XmlAttribute.Write(writer,attr) |> ignore
        writer

    static member Write (node:XmlNode) (writer:StreamWriter) =
        let inline parentNode (tag:string,node) = writer ++ "<" ++ tag +> XmlNode.WriteParentNode node ++ " </" ++ tag ++ ">"
        let inline voidNode (tag:string,node) = writer ++ "<" ++ tag +> XmlNode.WriteVoidNode node ++ " />"

        match node.NTag with
        | NodeTag.Html       -> parentNode ("html",node)
        | NodeTag.Base       -> voidNode ("base",node)
        | NodeTag.Head       -> parentNode ("head",node)
        | NodeTag.Link       -> voidNode ("link",node)
        | NodeTag.Meta       -> voidNode ("meta",node)
        | NodeTag.Style      -> parentNode ("style",node)
        | NodeTag.Title      -> parentNode ("title",node)
        | NodeTag.Body       -> parentNode ("body",node)
        | NodeTag.Address    -> parentNode ("address",node)
        | NodeTag.Article    -> parentNode ("article",node)
        | NodeTag.Aside      -> parentNode ("aside",node)
        | NodeTag.Footer     -> parentNode ("footer",node)
        | NodeTag.Hgroup     -> parentNode ("hgroup",node)
        | NodeTag.H1         -> parentNode ("h1",node)
        | NodeTag.H2         -> parentNode ("h2",node)
        | NodeTag.H3         -> parentNode ("h3",node)
        | NodeTag.H4         -> parentNode ("h4",node)
        | NodeTag.H5         -> parentNode ("h5",node)
        | NodeTag.H6         -> parentNode ("h6",node)
        | NodeTag.Header     -> parentNode ("header",node)
        | NodeTag.Nav        -> parentNode ("nav",node)
        | NodeTag.Section    -> parentNode ("section",node)
        | NodeTag.Dd         -> parentNode ("dd",node)
        | NodeTag.Div        -> parentNode ("div",node)
        | NodeTag.Dl         -> parentNode ("dl",node)
        | NodeTag.Dt         -> parentNode ("dt",node)
        | NodeTag.Figcaption -> parentNode ("figcaption",node)
        | NodeTag.Figure     -> parentNode ("figure",node)
        | NodeTag.Hr         -> voidNode ("hr",node)
        | NodeTag.Li         -> parentNode ("li",node)
        | NodeTag.Main       -> parentNode ("main",node)
        | NodeTag.Ol         -> parentNode ("ol",node)
        | NodeTag.P          -> parentNode ("p",node)
        | NodeTag.Pre        -> parentNode ("pre",node)
        | NodeTag.Ul         -> parentNode ("ul",node)
        | NodeTag.A          -> parentNode ("a",node)
        | NodeTag.Abbr       -> parentNode ("abbr",node)
        | NodeTag.B          -> parentNode ("b",node)
        | NodeTag.Bdi        -> parentNode ("bdi",node)
        | NodeTag.Bdo        -> parentNode ("bdo",node)
        | NodeTag.Br         -> voidNode ("br",node)
        | NodeTag.Cite       -> parentNode ("cite",node)
        | NodeTag.Code       -> parentNode ("code",node)
        | NodeTag.Data       -> parentNode ("data",node)
        | NodeTag.Dfn        -> parentNode ("dfn",node)
        | NodeTag.Em         -> parentNode ("em",node)
        | NodeTag.I          -> parentNode ("i",node)
        | NodeTag.Kbd        -> parentNode ("kbd",node)
        | NodeTag.Mark       -> parentNode ("mark",node)
        | NodeTag.Q          -> parentNode ("q",node)
        | NodeTag.Rp         -> parentNode ("rp",node)
        | NodeTag.Rt         -> parentNode ("rt",node)
        | NodeTag.Rtc        -> parentNode ("rtc",node)
        | NodeTag.Ruby       -> parentNode ("ruby",node)
        | NodeTag.S          -> parentNode ("s",node)
        | NodeTag.Samp       -> parentNode ("samp",node)
        | NodeTag.Small      -> parentNode ("small",node)
        | NodeTag.Span       -> parentNode ("span",node)
        | NodeTag.Strong     -> parentNode ("strong",node)
        | NodeTag.Sub        -> parentNode ("sub",node)
        | NodeTag.Sup        -> parentNode ("sup",node)
        | NodeTag.Time       -> parentNode ("time",node)
        | NodeTag.U          -> parentNode ("u",node)
        | NodeTag.Var        -> parentNode ("var",node)
        | NodeTag.Wbr        -> voidNode ("wbr",node)
        | NodeTag.Area       -> voidNode ("area",node)
        | NodeTag.Audio      -> parentNode ("audio",node)
        | NodeTag.Img        -> voidNode ("img",node)
        | NodeTag.Map        -> parentNode ("map",node)
        | NodeTag.Track      -> voidNode ("track",node)
        | NodeTag.Video      -> parentNode ("video",node)
        | NodeTag.Embed      -> voidNode ("embed",node)
        | NodeTag.Object     -> parentNode ("object",node)
        | NodeTag.Param      -> voidNode ("param",node)
        | NodeTag.Source     -> voidNode ("source",node)
        | NodeTag.Canvas     -> parentNode ("canvas",node)
        | NodeTag.Noscript   -> parentNode ("noscript",node)
        | NodeTag.Script     -> parentNode ("script",node)
        | NodeTag.Del        -> parentNode ("del",node)
        | NodeTag.Ins        -> parentNode ("ins",node)
        | NodeTag.Caption    -> parentNode ("caption",node)
        | NodeTag.Col        -> voidNode ("col",node)
        | NodeTag.Colgroup   -> parentNode ("colgroup",node)
        | NodeTag.Table      -> parentNode ("table",node)
        | NodeTag.Tbody      -> parentNode ("tbody",node)
        | NodeTag.Td         -> parentNode ("td",node)
        | NodeTag.Tfoot      -> parentNode ("tfoot",node)
        | NodeTag.Th         -> parentNode ("th",node)
        | NodeTag.Thead      -> parentNode ("thead",node)
        | NodeTag.Tr         -> parentNode ("tr",node)
        | NodeTag.Button     -> parentNode ("button",node)
        | NodeTag.Datalist   -> parentNode ("datalist",node)
        | NodeTag.Fieldset   -> parentNode ("fieldset",node)
        | NodeTag.Form       -> parentNode ("form",node)
        | NodeTag.Input      -> voidNode ("input",node)
        | NodeTag.Label      -> parentNode ("label",node)
        | NodeTag.Legend     -> parentNode ("legend",node)
        | NodeTag.Meter      -> parentNode ("meter",node)
        | NodeTag.Optgroup   -> parentNode ("optgroup",node)
        | NodeTag.Option     -> parentNode ("option",node)
        | NodeTag.Output     -> parentNode ("output",node)
        | NodeTag.Progress   -> parentNode ("progress",node)
        | NodeTag.Select     -> parentNode ("select",node)
        | NodeTag.Textarea   -> parentNode ("textarea",node)
        | NodeTag.Details    -> parentNode ("details",node)
        | NodeTag.Dialog     -> parentNode ("dialog",node)
        | NodeTag.Menu       -> parentNode ("menu",node)
        | NodeTag.Menuitem   -> voidNode ("menuitem",node)
        | NodeTag.Summary    -> parentNode ("summary",node)
        | NodeTag.Encodedtext -> writer ++ WebUtility.HtmlEncode node.TextVal
        | NodeTag.Rawtext    -> writer ++ node.TextVal
        | NodeTag.Emptytext  -> writer ++ "\"\""
        | NodeTag.Comment    -> writer ++ "<!-- " ++ node.TextVal ++ " -->"
        |> ignore
/// ---------------------------
/// Building blocks
/// ---------------------------
/// ---------------------------
/// Render XML string
/// ---------------------------


let renderHtmlDocument ( document : XmlNode) (writer : StreamWriter) =
    writer ++ "<!DOCTYPE html>" +> XmlNode.Write document 
    writer ++ Environment.NewLine
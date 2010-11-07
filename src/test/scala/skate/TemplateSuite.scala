package skate

import scala.collection.mutable.HashMap
import scala.xml.Elem
import scala.xml.MetaData
import scala.xml.NodeSeq
import scala.xml.Null
import scala.xml.Text
import scala.xml.UnprefixedAttribute
import scala.xml.XML
import org.scalatest.FunSuite

class TemplateSuite extends FunSuite {

  def debug(a:Any*) {
    info(a.mkString)
  }

  //TemplateConfig.debug = debug _

  def assertSame(expected:NodeSeq, actual:NodeSeq) = {
    assert(ts(expected) == ts(actual))
  }

  def ts(ns:NodeSeq) = {
    if (ns.isEmpty) ""
    else {
      val sw = new java.io.StringWriter
      XML.write(sw, ns.head, "UTF-8", false, null)
      sw.toString
    }
  }

  test("Ignore elements that aren't of interest") {
    val in = <a href="http://www.yahoo.com">A simple Link</a>
    info(ts(new Template(in).eval))
    assertSame(in, new Template(in).eval)
  }

  test("Unrecognized element") {
    val in = <f:UnknownClass.unknownMethod xmlns:f={Template.NS}><a href="http://www.yahoo.com">A simple Link</a></f:UnknownClass.unknownMethod>
    assertSame(Template.elementHandlerNotFound(in),
	       new Template(in).eval)
  }

  test("Simple eval") {
    val in = <f:skate.MyClass.myElementHandler xmlns:f={Template.NS}/>
    info(ts(new Template(in).eval))
    assertSame(new MyClass().myElementHandler(NodeSeq.Empty, Null),
	       new Template(in).eval)
  }

  test("Nested eval") {
    val in = <html xmlns:f={Template.NS}><body><h1><f:skate.MyClass.myElementHandler/></h1></body></html>
    assertSame(<html xmlns:f={Template.NS}><body><h1>This test passed!</h1></body></html>,
	       new Template(in).eval)
  }

  test("Right params, wrong return type") {
    val in = <f:brokenElementHandler xmlns:f={Template.NS}/>
    assertSame(Template.elementHandlerNotFound(in), new Template(in).eval)
  }

  test("ignore") {
    val in = <f:ignore xmlns:f={Template.NS}>Welcome to the black hole <![CDATA[ ZOICKS ahsdalsdkjre r ]]> It's over</f:ignore>
    assertSame(NodeSeq.Empty, new Template(in).eval)
  }

  test("children") {
    import NodeSeq.seqToNodeSeq
    val in = <f:children xmlns:f={Template.NS}><p>Blah</p><br/><h1>And so on</h1></f:children>
    assertSame(in.child, new Template(in).eval)
  }

  test("include") {
    
    val included = <p>This is an included template!</p>

    TemplateConfig.findTemplate = (s:String) => Some(new Template(included))

    val outer = <html><body><h1>Some header</h1><f:include name="included" xmlns:f={Template.NS}/></body></html>

    val eval = new Template(outer).eval

    info(ts(eval))

    assertSame(<html><body><h1>Some header</h1>{included}</body></html>,
	       eval)

  }

  test("attribute") {

    val in = <a href="http://www.yahoo.com" f:skate.MyClass.myAttribute="" xmlns:f={Template.NS} rel="top">Link</a>

    val out = <a href="http://www.yahoo.com" class="flibber" xmlns:f={Template.NS} rel="top">Link</a>

    val eval = new Template(in).eval.head.asInstanceOf[Elem]

    info(ts(in))
    info(ts(eval))

    assert(out.attributes.asAttrMap.equals(eval.attributes.asAttrMap))
    
  }

  test("eager eval") {

    val inEager = <f:skate.MyClass.outer eager="true" xmlns:f={Template.NS}><f:skate.MyClass.inner>Foo</f:skate.MyClass.inner></f:skate.MyClass.outer>

    val outEager = Text("I saw nothing!")

    val eval = new Template(inEager).eval

    info(ts(eval))

    assertSame(outEager, eval)
  }

  test("surround") {

    val outer = <html xmlns:f={Template.NS}><body><h1>Header</h1><f:bind name="foo"/></body></html>

    val inner = <f:surround xmlns:f={Template.NS} with="outer"><f:bind-at name="foo"><p>This is surrounded content</p></f:bind-at></f:surround>

    val out = <html xmlns:f={Template.NS}><body><h1>Header</h1><p xmlns:f={Template.NS}>This is surrounded content</p></body></html>

    TemplateConfig.findTemplate = (s:String) => Some(new Template(outer))

    val eval = new Template(inner).eval

    info(ts(eval))

    assertSame(out, eval)

  }

  test("replace") {

    val in = <ul><li><u:foo/></li><li><u:bar/></li></ul>

    val replaced = Template.replace(in, Null) {
      case Elem(_, "foo", _, _, _*) => Text("Replaced foo!")
      case Elem("u", "bar", _, _, _*) => Text("Replaced bar!")
    }

    val out = <ul><li>Replaced foo!</li><li>Replaced bar!</li></ul>

    info(ts(replaced))

    assertSame(replaced, out)

  }

  test("eval with scope") {

    val in = <div><ul><li><f:skate.MyClass.myIdentity xmlns:f={Template.NS}/></li><li><f:skate.MyClass.myIdentity xmlns:f={Template.NS}/></li></ul></div>

    Template.instanceCache.withValue(Some(new HashMap())) {
      info(ts(new Template(in).eval))
      val out = new Template(in).eval
      val lis = out \\ "li"
      assert(lis.forall(li => li.text == lis.head.text))
    }

    assert(Template.instanceCache.value.isEmpty)

  }


}

class MyClass {

  def myAttribute(md:MetaData) = new UnprefixedAttribute("class", "flibber", md.next)

  def myElementHandler(ns:NodeSeq, md:MetaData) = Text("This test passed!")

  def myIdentity(ns:NodeSeq, md:MetaData) = Text(toString)

  def inner(ns:NodeSeq, md:MetaData) = NodeSeq.Empty

  def outer(ns:NodeSeq, md:MetaData) = {
    val children = ns.map(_.text)
    if (children.size == 0) Text("I saw nothing!")
    else Text("I saw " + children.mkString(", "))
  }

  // this has the right params, but wrong return type
  def brokenElementHandler(ns:NodeSeq, md:MetaData) { 
  }

}

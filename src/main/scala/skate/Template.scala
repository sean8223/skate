/**
 * Copyright 2010 Sean Wellington
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package skate

import scala.collection.mutable
import java.lang.reflect.Method
import scala.util.DynamicVariable
import scala.xml.Elem
import scala.xml.Group
import scala.xml.MetaData
import scala.xml.Null
import scala.xml.Node
import scala.xml.NodeSeq
import scala.xml.UnprefixedAttribute
import scala.xml.transform.RewriteRule
import scala.xml.transform.RuleTransformer

/**
 * A Template is wrapper around a well-formed XML document (or fragment). Elements and attributes
 * satisfying TemplateConfig.ElementPredicate and TemplateConfig.AttributePredicate will be treated as functions
 * over their content when the template is evaluated. Template instances have no mutable state and
 * are thread-safe.
 */
class Template(root:Elem) {

  val bindAts:Map[String, NodeSeq] = { 
    val m = mutable.Map[String, NodeSeq]()
    root \\ "bind-at" filter (x => x.namespace == Template.NS) foreach {
      z => {
	val name = z.attributes.get("name")
	name.foreach(q => m(q.toString) = z.child)
      }
    }
    m.toMap
  }

  /**
   * Evaluate this template, invoking ElementHandlers and AttributeHandlers to generate
   * dynamic content as needed.
   */
  def eval:NodeSeq = eval(root)

  /**
   * Evaluates a node in a Template, recursively invoking ElementHandlers and AttributeHandlers
   * over its children to generate dynamic content as indicated by TemplateConfig.isHandledElement and
   * TemplateConfig.isHandledAttribute.
   */
  def eval(ns:NodeSeq):NodeSeq = {
    TemplateConfig.debug("eval: ", ns.getClass, " ", { val s = ns.toString; if (s.length < 10) s.substring(0, s.length) else s.substring(0, 10) + " ..." })
    ns flatMap {
      x => x match {
	case e:Elem if (isHandledElement(e)) => eval(handleElement(e))
	case e:Elem => Elem(e.prefix, e.label, evalAttributes(e.attributes, e), e.scope, eval(e.child):_*)
	case _ => x
      }
    }
  }

  /**
   * Evaluates attributes of a XML element, invoking AttributeHandlers to generate
   * dynamic content as indicated by TemplateConfig.isHandledAttribute.
   */
  def evalAttributes(md:MetaData, e:Elem):MetaData = md match {
    case Null => Null
    case x if (TemplateConfig.isHandledAttribute(x, e)) => handleAttribute(x, e)
    case _ => md.copy(evalAttributes(md.next, e))
  }

  /**
   * Obtains an AttributeHandler for the supplied XML attribute and executes it.
   */
  def handleAttribute(md:MetaData, parent:Elem):MetaData = {
    val attributeHandler:Option[Template.AttributeHandler] = TemplateConfig.findAttributeHandler(md, parent)
    attributeHandler.map {
      x => try {
	x(md)
      }
      catch {
	case t => Template.exceptionInAttributeHandler(t, md, parent)
      }
    }.getOrElse(Template.attributeHandlerNotFound(md, parent))
  }

  /**
   * Obtains an ElementHandler for the supplied XML element and executes it.
   */
  def handleElement(e:Elem):NodeSeq = {
    val elementHandler:Option[Template.ElementHandler] = findElementHandler(e)
    TemplateConfig.debug("handleElement: ", e.label, " -> ", elementHandler)
    elementHandler.map {
      val eager:Boolean = e.attributes.find(x => x.key == "eager").map(y => y.value.toString == "true").getOrElse(false)
      x => try {
	x(if (eager) eval(e.child) else e.child, e.attributes)
      }
      catch {
	case t => Template.exceptionInElementHandler(t, e)
      }
    }.getOrElse(Template.elementHandlerNotFound(e))
  }

  /**
   * ElementHandlers are found by trying the following in order: the built-in ElementHandlerFinder
   * the system ElementHandlerFinder defined at TemplateConfig.findElementHandler.
   */
  def findElementHandler(e:Elem):Option[Template.ElementHandler] = 
    Template.builtInElementHandlerFinder(e, this) orElse TemplateConfig.findElementHandler(e)

  def isHandledElement(e:Elem) = Template.builtInElementHandlerFinder(e, this).isDefined || TemplateConfig.isHandledElement(e)

}

object Template {

  // interfaces

  /**
   * A ElementHandler is a function that takes the (possibly empty) body content and attributes of an
   * XML element identified by a ElementPredicate and processes it in some way. The value returned by
   * this function will replace the parent XML element in the Template.
   */
  type ElementHandler = Function2[NodeSeq, MetaData, NodeSeq]

  /**
   * An AttributeHandler is a function that takes a XML attribute identified by an AttributePredicate
   * and replaces it with alternative (possibly Null) content.
   */
  type AttributeHandler = Function1[MetaData, MetaData]

  /**
   * Recycle handler instances so tags can share state during a request.
   */
  object instanceCache extends DynamicVariable[Option[mutable.Map[Method, Any]]](None)

  /**
   * Finds and evaluates the named template. This is the main entry point for using templates.
   */
  def eval(name:String):NodeSeq = {
    val t = TemplateConfig.findTemplate(name)
    instanceCache.withValue(Some(new mutable.HashMap())) {
      t.map(_.eval.head).getOrElse(templateNotFound(name))
    }
  }

  // defaults

  /**
   * The namespace for Template artifacts -- "urn:skate:1"
   */
  val NS = "urn:skate:1"

  private def findHandler[T](key:String, methodCache:mutable.Map[String, Method], returnType:Class[_], paramTypes:Class[_]*)(f:(Method, Any) => T):Option[T] = 
    try {
      val method = methodCache.get(key) orElse {
	val dot = key.lastIndexOf(".")
	if (dot != -1) {
	  val (className, methodName) = (key.substring(0, dot), key.substring(dot + 1))
	  val klass = Class.forName(className)
	  val method = klass.getMethod(methodName, paramTypes:_*)
	  TemplateConfig.debug("findHandler: return type is ", method.getReturnType, ", needs ", returnType)
	  if (returnType.isAssignableFrom(method.getReturnType)) {
	    methodCache.put(key, method)
	    Some(method)
	  }
	  else {
	    None
	  }
	}
	else None
      }
      method.map {
	x => {
	  val instance = instanceCache.value match {
	    case None                => x.getDeclaringClass.newInstance
	    case Some(instanceCache) => instanceCache.get(x).getOrElse { 
	      val z = x.getDeclaringClass.newInstance
	      instanceCache.put(x, z)
	      z
	    }
	  }
	  f(x, instance)
	}
      }
    }
    catch {
      case e => None 
    }

  /**
   * An element is handled if it is in the Template.NS namespace.
   */
  def defaultIsHandledElement(e:Elem) = e.namespace == NS

  /**
   * The label of an Elem is presumed to be the fully qualified name of a method that
   * conforms to ElementHandler, with the package/class name/method name components separated by
   * dots. For example, a element with the label "com.mypackage.MyClass.myElementHandler" indicates
   * the method "myElementHandler" on the class "com.mypackage.MyClass".
   */
  def defaultElementHandlerFinder(e:Elem, methodCache:mutable.Map[String, Method]):Option[ElementHandler] = 
    findHandler[ElementHandler](e.label, methodCache, classOf[NodeSeq], classOf[NodeSeq], classOf[MetaData]) {
      (method, instance) => (ns:NodeSeq, md:MetaData) => method.invoke(instance, ns, md).asInstanceOf[NodeSeq]
    }

  /**
   * An attribute is handled if it is in the Template.NS namespace.
   */
  def defaultIsHandledAttribute(md:MetaData, e:Elem) = md.getNamespace(e) == NS

  /**
   * Logging is disabled by default.
   */
  def defaultDebugLogger(msg:Any*) {
  }

  /**
   * The key of an UnprefixedAttribute or PrefixedAttribute is presumed to be the fully qualified
   * name of a method that conforms to AttributeHandler, with the package/class name/method name
   * components separated by dots. For example, the attribute with the key
   * "com.mypackage.MyClass.myAttributeHandler" indicates the method "myAttributeHandler" on the
   * class "com.mypackage.MyClass".
   */
  def defaultAttributeHandlerFinder(md:MetaData, e:Elem, methodCache:mutable.Map[String, Method]):Option[AttributeHandler] =
    findHandler[AttributeHandler](md.key, methodCache, classOf[MetaData], classOf[MetaData]) {
      (method, instance) => (md:MetaData) => method.invoke(instance, md).asInstanceOf[MetaData]
    }

  private def ul(t:Option[Throwable]):NodeSeq = t.map(x => <div><h2>{x.toString}</h2><ul>{x.getStackTrace.flatMap(y => <li>{y}</li>)}</ul></div>).getOrElse(NodeSeq.Empty)

  def defaultElementErrorHandler(msg:String, t:Option[Throwable], e:Option[Elem]) = <div class="template-error"><h1>{msg}</h1>{ul(t)}</div>

  def defaultAttributeErrorHandler(msg:String, t:Option[Throwable], md:MetaData, e:Elem) = new UnprefixedAttribute(md.key, msg + ": " + t.map(x => t.toString + ": " + x.getStackTrace.take(5).mkString("; ") + " ...").getOrElse(""), md.next)

  def defaultTemplateErrorHandler(msg:String, t:Option[Throwable], path:String) = <html><body>{defaultElementErrorHandler(msg, t, None)}</body></html>

  // various errors

  def templateNotFound(name:String) = TemplateConfig.templateError("Can't find template '" + name + "'", None, name)

  def includedTemplateNotFound(name:String) = TemplateConfig.elementError("Can't find template '" + name + "'", None, None)

  def elementHandlerNotFound(e:Elem) = TemplateConfig.elementError("Can't find element handler for '" + e.label + "'", None, Some(e))

  def exceptionInElementHandler(t:Throwable, e:Elem) = TemplateConfig.elementError("Unhandled exception in ElementHander", Some(t), Some(e))

  def attributeHandlerNotFound(md:MetaData, e:Elem) = TemplateConfig.attributeError("Can't find attribute handler", None, md, e)

  def exceptionInAttributeHandler(t:Throwable, md:MetaData, e:Elem) = TemplateConfig.attributeError("Unhandled exception in AttributeHandler", Some(t), md, e)

  // built-in element handlers

  def builtInElementHandlerFinder(e:Elem, t:Template):Option[ElementHandler] = e.label match {
    case "ignore" if (e.namespace == NS)   => Some(ignore _)
    case "children" if (e.namespace == NS) => Some(children _)
    case "include" if (e.namespace == NS)  => Some(include _)
    case "surround" if (e.namespace == NS) => Some(surround(_, _, t))
    case "bind" if (e.namespace == NS)     => Some(bind _)
    case _                                 => None
  }

  /**
   * Body content of this element is removed during Template evaluation.
   */
  def ignore(ns:NodeSeq, md:MetaData):NodeSeq = NodeSeq.Empty

  /**
   * Body content of this element is returned during Template evaluation and is subject to further
   * processing. Useful as a container for a sequence of XML nodes when a surrounding context
   * requires a XML element.
   */
  def children(ns:NodeSeq, md:MetaData):NodeSeq = ns

  /**
   * Inserts one template inside another; the template to be inserted is identified by the
   * "name" attribute passed into the embed element, e.g. <![CDATA[<f:embed name="header.html"/>]]>.
   * This should be a string suitable for a TemplateFinder.
   */
  def include(ns:NodeSeq, md:MetaData):NodeSeq = {
    val name = md.get("name")
    name.map { 
      x => {
	val template = TemplateConfig.findTemplate(x.toString)
	template.map(_.eval).getOrElse(includedTemplateNotFound(x.toString))
      }
    }.getOrElse(includedTemplateNotFound("Missing 'name' attribute on 'include' element"))
  }

  object bindAts extends DynamicVariable[Option[Map[String, NodeSeq]]](None)

  /**
   * Replaces a "bind" element with the content of a corresponding "bind-at"
   * element.
   */
  def bind(ns:NodeSeq, md:MetaData):NodeSeq = {
    val name = md.get("name")
    name.flatMap {
      x => bindAts.value.flatMap(y => y.get(x.toString))
    }.getOrElse(NodeSeq.Empty)
  }

  /**
   * Loads a template and inserts content inside of it at places defined by "bind" elements.
   */
  def surround(ns:NodeSeq, md:MetaData, t:Template):NodeSeq = {
    val name = md.find(x => x.key == "with" || x.key == "name")
    name.map {
      x => {
	val template = TemplateConfig.findTemplate(x.value.toString)
	template.map {
	  y => {
	    bindAts.withValue(Some(t.bindAts)) {
	      y.eval
	    }
	  }
	}.getOrElse(includedTemplateNotFound(x.value.toString))
      }
    }.getOrElse(includedTemplateNotFound("Missing 'with' or 'name' attribute on 'surround' element"))
  }

  // misc

  /**
   * Convenience method for replacing text inside element handlers. Use it
   * like this:
   * <code>
   * def myElementHandler(in:NodeSeq, atts:MetaData) =
   *   Template.replace(in) {
   *      case Elem("u", "nameLabel", _, _, _) => <label for="name">Name:</label>
   *      case Elem("u", "nameInput", atts, _, _) => atts.flatMap(<input id="name" name="name" value="">(_ % _)
   *   }
   * </code>
   * This creates a new ElementHandler via the replacer function and invokes it. See
   * the documentation for replacer for more info.
   */
  def replace(ns:NodeSeq, md:MetaData)(rs:PartialFunction[NodeSeq, NodeSeq]):NodeSeq = replacer(rs)(ns, md)

  /**
   * Creates an ElementHandler that replaces nodes in its supplied content based on a series of
   * match expressions. Use it like this:
   * <code>
   * class MyClass {
   *   val myElementHandler = Template.replacer {
   *     case Elem("u", "nameLabel", _, _, _)    => Text("Name:")
   *     case Elem("u", "nameInput", atts, _, _) => atts.flatMap(<input id="name" name="name" value="">(_ % _)
   *   }
   * }
   * </code>
   * Then, when invoked with:
   * <code>
   * <![CDATA[
   * <MyClass.myElementHandler>
   *   <form action="foo">
   *     <label for="name"><u:nameLabel/></label>
   *     <u:nameInput id="name" size="25" maxlength="255"/>
   *   </form>
   * </MyClass.myElementHandler>
   * ]]>
   * </code>
   * The following output will be generated:
   * <code>
   * <![CDATA[
   * <form action="foo">
   *   <label for="name">Name:</label>
   *   <input name="name" value="" id="name" size="25" maxlength="255"/>
   * </form>
   * ]]>
   * 
   */
  def replacer(rs:PartialFunction[NodeSeq, NodeSeq]):ElementHandler = {

    object rt extends RuleTransformer(new RewriteRule {

      override def transform(ns: Seq[Node]): Seq[Node] = {

	// see http://lampsvn.epfl.ch/trac/scala/ticket/3689; remove 
	// when this issue is resolved

	val xs = ns.toStream map transform
	val (xs1, xs2) = xs zip ns span { case (x, n) => unchanged(n, x) }

	if (xs2.isEmpty) ns
	else (xs1 map (_._2)) ++ xs2.head._1 ++ transform(ns drop (xs1.length + 1))

      }

      override def transform(n:Node):Seq[Node] = rs.lift(n).getOrElse(n)

    })

    (ns:NodeSeq, md:MetaData) => ns flatMap {
      x => rt(x)
    }

  }

}


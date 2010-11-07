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

import scala.collection.JavaConversions._
import scala.xml.Elem
import scala.xml.MetaData
import java.lang.reflect.Method
import java.util.concurrent.ConcurrentHashMap

/**
 * Extension/customization hooks for Template processing.
 */
object TemplateConfig {

  /**
   * A TemplateFinder is a function that returns a Template identified by a string. If no such
   * Template exists, None is returned.
   */
  type TemplateFinder = Function1[String, Option[Template]]

  /**
   * Default is no-op; try setting this to ServletSupport.defaultTemplateFinder if you are running
   * in a J2EE/Servlet container.
   */
  var findTemplate:TemplateFinder = (s:String) => None

  /**
   * A TemplateErrorHandler generates substitute content for a template when a TemplateFinder
   * fails. The arguments to this function are a description of the error, the exception that
   * caused it (if any) and the name of the template.
   */
  type TemplateErrorHandler = Function3[String, Option[Throwable], String, Elem]

  var templateError:TemplateErrorHandler = Template.defaultTemplateErrorHandler _

  /**
   * A ElementPredicate examines a XML element and determines whether or not it should be
   * processed by a ElementHandler.
   */
  type ElementPredicate = Function1[Elem, Boolean]
  
  var isHandledElement:ElementPredicate = Template.defaultIsHandledElement _

  /**
   * A ElementHandlerFinder is a function that examines a XML element and returns a ElementHandler suitable
   * for processing its body content. If no ElementHandler can be found, None is returned.
   */
  type ElementHandlerFinder = Function1[Elem, Option[Template.ElementHandler]]

  var findElementHandler:ElementHandlerFinder = {
    val cache = new ConcurrentHashMap[String, Method]()
    Template.defaultElementHandlerFinder(_, cache)
  }

  /**
   * A ElementErrorHandler generates substitute content when a ElementHandler fails for any reason.
   * The arguments to this function are a description of the error, the exception that caused it
   * (if any), and optionally the element in question.
   */
  type ElementErrorHandler = Function3[String, Option[Throwable], Option[Elem], Elem]

  var elementError:ElementErrorHandler = Template.defaultElementErrorHandler _

  /**
   * An AttributePredicate examines a XML attribute and determines whether or not it should be
   * processed by an AttributeHandler. The arguments passeed to the function are the MetaData object
   * representing the attribute and its parent element.
   */
  type AttributePredicate = Function2[MetaData, Elem, Boolean]

  var isHandledAttribute:AttributePredicate = Template.defaultIsHandledAttribute _

  /**
   * An AttributeHandlerFinder is a function that examines a XML attribute and returns an
   * AttributeHandler suitable for processing it. If no AttributeHandler can be found, None is
   * returned.
   */
  type AttributeHandlerFinder = Function2[MetaData, Elem, Option[Template.AttributeHandler]]

  var findAttributeHandler:AttributeHandlerFinder = {
    val cache = new ConcurrentHashMap[String, Method]()
    Template.defaultAttributeHandlerFinder(_, _, cache)
  }

  /**
   * An AttributeErrorHandler generates substitute content when an AttributeHandler fails
   * for any reason. The arguments to this function are a description of the error, the
   * exception that caused it (if any) and the attribute in question.
   */
  type AttributeErrorHandler = Function4[String, Option[Throwable], MetaData, Elem, MetaData]

  var attributeError:AttributeErrorHandler = Template.defaultAttributeErrorHandler _

  /**
   * Set this to a function that is (Any*) => Unit if you want to enable debug logging. How
   * do I capture this as a type alias?
   */
  var debug = Template.defaultDebugLogger _

}

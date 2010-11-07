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

import java.io.File
import java.util.concurrent.ConcurrentHashMap
import javax.servlet.ServletContext
import javax.servlet.http.HttpServlet
import javax.servlet.http.HttpServletRequest
import javax.servlet.http.HttpServletResponse
import scala.collection.JavaConversions._
import scala.collection.mutable
import scala.xml.XML

/**
 * Helper functions for running in a Servlet container.
 */
object ServletSupport {

  /**
   * Evaluates the template indicated by req.requestURI and writes it to
   * the writer supplied resp.getWriter, flushing when done.
   */
  def render(req:HttpServletRequest, resp:HttpServletResponse) {
    XML.write(resp.getWriter, Template.eval(req.getRequestURI).head, "UTF-8", false, null)
    resp.getWriter.flush
  }
  
  def cat(root:String, uri:String) = {
    (root.endsWith("/"), uri.startsWith("/")) match {
      case (true, true)   => root + uri.substring(1)
      case (false, false) => root + "/" + uri
      case _              => root + uri
    }
  }

  val defaultTemplateFinder = {
    val cache = new ConcurrentHashMap[String, (Long, Template)]()
    findTemplate(_:String, _:ServletContext, cache, "/WEB-INF/templates")
  }

  /**
   * Loads templates from a directory in a web application/WAR file, specified by root. The
   * supplied cache will be searched for the template first, and if the modify stamp of
   * the actual file is newer than the cached copy, it will be reloaded.
   */
  def findTemplate(uri:String, sc:ServletContext, cache:mutable.Map[String, (Long, Template)], root:String):Option[Template] = {
    try {
      val path = cat(root, uri)
      val tst = cache.get(path)
      TemplateConfig.debug("defaultTemplateFinder: ", path, " -> ", tst, " ", cache)
      tst match {
	case None            => loadTemplate(path, sc, cache).map(x => x._2)
	case Some((-1L, t))  => Some(t)
	case Some((ts, t)) if (ts < calcLastModified(path, sc)) => loadTemplate(path, sc, cache).map(x => x._2)
	case Some((ts, t))   => Some(t)
      }
    }
    catch {
      case e => Some(new Template(TemplateConfig.templateError(e.toString, Some(e), uri)))
    }
  }

  def calcLastModified(path:String, sc:ServletContext):Long = {
    val realPath = Option(sc.getRealPath(path))
    val lastModified = realPath.map {
      x => new File(x).lastModified
    }.getOrElse(-1L) 
    TemplateConfig.debug("calcLastModified: ", path, " (", realPath, ") -> ", lastModified)
    lastModified
  }

  def loadTemplate(path:String, sc:ServletContext, cache:mutable.Map[String, (Long, Template)]):Option[(Long, Template)] = {
    val is = Option(sc.getResourceAsStream(path))
    is.map {
      x => {
	val t = new Template(XML.load(x))
	val ts = calcLastModified(path, sc)
	val tst = (ts, t)
	cache.put(path, tst)
	TemplateConfig.debug("loadTemplate: ", cache)
	tst
      }
    }
  }

}

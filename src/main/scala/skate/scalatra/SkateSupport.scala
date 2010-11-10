package skate.scalatra

import javax.servlet.ServletContext
import javax.servlet.ServletConfig
import javax.servlet.FilterConfig
import org.scalatra.Initializable
import skate.TemplateConfig
import skate.ServletSupport

/**
 * Support for using Skate with Scalatra. Just mix this in to either
 * a ScalatraServlet or ScalatraFilter
 */
trait SkateSupport extends Initializable {

  abstract override def initialize(config:Config) {
    super.initialize(config)
    config match {
      case sc:ServletConfig => setTemplateFinder(sc.getServletContext)
      case fc:FilterConfig  => setTemplateFinder(fc.getServletContext)
      case _ => ;
    }
  }

  def setTemplateFinder(sc:ServletContext) = TemplateConfig.findTemplate = ServletSupport.defaultTemplateFinder(_, sc)

}

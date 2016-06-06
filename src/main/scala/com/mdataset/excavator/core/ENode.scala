package com.mdataset.excavator.core

import com.ecfront.common.JsonHelper
import com.fasterxml.jackson.databind.JsonNode
import com.fasterxml.jackson.databind.node.ObjectNode

class ENode extends ENodeDef with HtmlVisitAble with DomVisitAble with DataProcessAble {


  def go(nodeName: String, urlOrCssQuery: Any, fun: this.type => Unit): Unit = {
    urlOrCssQuery match {
      case urls: Seq[String] => htmls(nodeName, urls, fun)
      case s: String => s.toLowerCase match {
        case ss if ss.startsWith("http://") || ss.startsWith("https://") => htmls(nodeName, s, fun)
        case _ => doms(nodeName, s, fun)
      }
      case _ => logger.error("Only use url or css query.")
    }
  }

}
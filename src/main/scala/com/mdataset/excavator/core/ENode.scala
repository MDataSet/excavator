package com.mdataset.excavator.core

import org.jsoup.nodes.Element

class ENode extends ENodeDef with HtmlVisitAble with DomVisitAble with DataProcessAble {

  def go(nodeName: String, cssQuery: Element => Seq[Element])(fun: this.type => Unit): Unit = {
    dom(nodeName, cssQuery, fun)
  }

  def go(nodeName: String, urls: Seq[String])(fun: this.type => Unit): Unit = {
    html(nodeName, urls, fun)
  }

  def go(nodeName: String, urlOrCssQuery: String)(fun: this.type => Unit): Unit = {
    urlOrCssQuery.toLowerCase match {
      case ss if ss.startsWith("http://") || ss.startsWith("https://") => html(nodeName, urlOrCssQuery, fun)
      case _ => dom(nodeName, urlOrCssQuery, fun)
    }
  }

}
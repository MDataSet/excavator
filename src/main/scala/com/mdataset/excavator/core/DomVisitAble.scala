package com.mdataset.excavator.core

import org.jsoup.nodes.Element

import scala.collection.JavaConversions._

trait DomVisitAble extends ENodeDef {

  private[excavator] def dom(nodeName: String, cssQuery: String, fun: this.type => Unit): Unit = {
    this.childNodeName = nodeName
    element.select(cssQuery).par.foreach {
      ele =>
        fun(create(this, ele))
    }
  }

  private[excavator] def dom(nodeName: String, cssQuery: Element => Seq[Element], fun: this.type => Unit): Unit = {
    this.childNodeName = nodeName
    cssQuery(element).par.foreach {
      ele =>
        fun(create(this, ele))
    }
  }

}

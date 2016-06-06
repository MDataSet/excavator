package com.mdataset.excavator.core

import scala.collection.JavaConversions._

trait DomVisitAble extends ENodeDef {

  private[excavator] def doms(nodeName: String, cssQuery: String, fun: this.type => Unit): Unit = {
    this.childNodeName = nodeName
    element.select(cssQuery).par.foreach {
      ele =>
        fun(create(this, ele))
    }
  }

}

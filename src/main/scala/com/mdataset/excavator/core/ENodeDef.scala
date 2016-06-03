package com.mdataset.excavator.core

import com.fasterxml.jackson.databind.JsonNode
import com.fasterxml.jackson.databind.node.ObjectNode
import com.typesafe.scalalogging.slf4j.LazyLogging
import org.jsoup.nodes.Element

trait ENodeDef extends LazyLogging {

  var parent: ENodeDef = null
  var currChild: ENodeDef = null
  var data: collection.mutable.Map[String, Any] = collection.mutable.Map()
  protected var element: Element = _
  protected var childNodeName: String = _

  protected def create(parent: ENodeDef, element: Element): this.type = {
    val newNode = getClass.newInstance()
    newNode.parent = parent
    newNode.element = element
    newNode.asInstanceOf[this.type]
  }

  def sleep(millis: Long): this.type = {
    Thread.sleep(millis)
    this
  }

  def process(fun: JsonNode => Unit): this.type = {
    val json = packageFromParent(this)
    fun(json)
    this
  }

  protected def packageFromParent(currObject: this.type , childJson: ObjectNode = null):JsonNode

}

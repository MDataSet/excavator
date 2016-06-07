package com.mdataset.excavator.core

import java.math.BigDecimal

import com.ecfront.common.JsonHelper
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

  def process(fun: JsonNode => Unit, flat: Boolean = true): this.type = {
    val json = if (flat) {
      packageFlatFromParent(this, JsonHelper.createObjectNode())
    } else {
      packageWithLevelFromParent(this, null)
    }
    fun(json)
    this
  }

  protected def packageFlatFromParent(currObject: this.type, json: ObjectNode): JsonNode = {
    packageData(currObject, json)
    if (currObject.parent != null) {
      packageFlatFromParent(currObject.parent.asInstanceOf[this.type], json)
    } else {
      json
    }
  }

  protected def packageWithLevelFromParent(currObject: this.type, childJson: ObjectNode): JsonNode = {
    val currJson = JsonHelper.createObjectNode()
    if (childJson != null) {
      currJson.set(currObject.childNodeName, childJson)
    }
    packageData(currObject, currJson)
    if (currObject.parent != null) {
      packageWithLevelFromParent(currObject.parent.asInstanceOf[this.type], currJson)
    } else {
      currJson
    }
  }

  private def packageData(currObject: this.type, json: ObjectNode): Unit = {
    currObject.data.foreach {
      item =>
        item._2 match {
          case value: String => json.put(item._1, value)
          case value: JsonNode => json.set(item._1, value)
          case value: Seq[String] =>
            val arr = json.putArray(item._1)
            value.foreach(arr.add)
          case value: Int => json.put(item._1, value)
          case value: Long => json.put(item._1, value)
          case value: Float => json.put(item._1, value)
          case value: Double => json.put(item._1, value)
          case value: Boolean => json.put(item._1, value)
          case value: Short => json.put(item._1, value)
          case value: BigDecimal => json.put(item._1, value)
          case _ => logger.warn(s"The type [${item._2.getClass.getName}] not support")
        }
    }
  }

}

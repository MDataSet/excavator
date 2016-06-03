package com.mdataset.excavator.core

import com.ecfront.common.JsonHelper
import com.fasterxml.jackson.databind.JsonNode
import com.fasterxml.jackson.databind.node.ObjectNode

class ENode extends ENodeDef with HtmlVisitAble with DomVisitAble with DataProcessAble {

  override protected def packageFromParent(currObject: ENode.this.type, childJson: ObjectNode): JsonNode = {
    val currJson = JsonHelper.createObjectNode()
    if (childJson != null) {
      currJson.set(currObject.childNodeName, childJson)
    }
    currObject.data.foreach {
      item =>
        item._2 match {
          case value: String => currJson.put(item._1, value)
          case value: Int => currJson.put(item._1, value)
          case value: Long => currJson.put(item._1, value)
          case value: Float => currJson.put(item._1, value)
          case value: Double => currJson.put(item._1, value)
          case value: Boolean => currJson.put(item._1, value)
          case value: Short => currJson.put(item._1, value)
          case value: JsonNode => currJson.set(item._1, value)
          case value: java.math.BigDecimal => currJson.put(item._1, value)
          case _ => logger.warn(s"The type [${item._2.getClass.getName}] not support")
        }
    }
    if (currObject.parent != null) {
      packageFromParent(currObject.parent.asInstanceOf[ENode], currJson)
    } else {
      currJson
    }
  }

}
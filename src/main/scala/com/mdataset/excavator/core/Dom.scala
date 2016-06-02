package com.mdataset.excavator.core

import com.ecfront.common.JsonHelper
import com.fasterxml.jackson.databind.JsonNode
import org.jsoup.nodes.Element
import org.mozilla.javascript._

import scala.collection.JavaConversions._

class Dom extends EObject {

  protected var element: Element = _
  private var jsContext: Context = _
  private var jsScope: ScriptableObject = _

  protected def jsDataInit(): Unit = {
    synchronized {
      if (jsContext == null) {
        synchronized {
          val globalVar = "var window = {};var document = {};\r\n"
          jsContext = Context.enter()
          jsScope = jsContext.initSafeStandardObjects()
          jsContext.evaluateString(jsScope, globalVar, "window", 1, null)
          element.select("script").map {
            item =>
              val js = item.html()
              if (js.nonEmpty) {
                try {
                  jsContext.evaluateString(jsScope, js, "", 1, null)
                } catch {
                  case e: Throwable =>
                }
              }
          }
        }
      }
    }
  }

  def init(parent: EObject, element: Element): this.type = {
    this.parent = parent
    this.element = element
    this
  }

  def doms(nodeName: String, cssQuery: String,fun: Dom => Unit): Unit = {
    this.childNodeName = nodeName
    element.select(cssQuery).par.foreach {
      ele =>
        fun(new Dom().init(this, ele))
    }
  }

  def setText(name: String, cssQuery: String): this.type = {
    nodeData += name -> element.select(cssQuery).text()
    this
  }

  def setText(name: String, cssQueryFun: Element => String): this.type = {
    val text = cssQueryFun(element)
    nodeData += name -> text
    this
  }

  def setJson(name: String, jsName: String): this.type = {
    val value = getValueForScript(name)
    if (value != null) {
      try {
        val json = JsonHelper.toJson(value.asInstanceOf[String])
        nodeData += name -> json
      } catch {
        case e: Throwable =>
          logger.warn("Parse json error.", e)
      }
    }
    this
  }

  def setJson(name: String, jsName: String, jsonFilterFun: JsonNode => JsonNode): this.type = {
    val value = getValueForScript(name)
    if (value != null) {
      try {
        val json = jsonFilterFun(JsonHelper.toJson(value.asInstanceOf[String]))
        nodeData += name -> json
      } catch {
        case e: Throwable =>
          logger.warn("Parse json error.", e)
      }
    }
    this
  }

  private def getValueForScript(name: String): String = {
    jsDataInit()
    val value = jsScope.get(name, jsScope)
    value match {
      case v if v.getClass == classOf[NativeObject] || v.getClass == classOf[NativeArray] =>
        NativeJSON.stringify(jsContext, jsScope, v, null, null).toString
      case v: UniqueTag =>
        logger.warn(s"Script not found variable [$name]")
        null
      case _ =>
        value.toString
    }
  }

}

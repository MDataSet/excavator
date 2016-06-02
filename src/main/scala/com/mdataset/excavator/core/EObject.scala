package com.mdataset.excavator.core

import java.util.Random

import com.ecfront.common.JsonHelper
import com.fasterxml.jackson.databind.JsonNode
import com.fasterxml.jackson.databind.node.ObjectNode
import com.mdataset.excavator.helper.Method.Method
import com.mdataset.excavator.helper._
import com.typesafe.scalalogging.slf4j.LazyLogging
import org.jsoup.Jsoup

import scala.collection.parallel.ParSeq

trait EObject extends LazyLogging {

  var parent: EObject = null
  val nodeData: collection.mutable.Map[String, Any] = collection.mutable.Map()
  var childNodeName: String = ""
  private val rand = new Random(System.currentTimeMillis())

  private val requestHeader = collection.mutable.Map[String, String]()

  def header(key: String, value: String): this.type = {
    requestHeader += key -> value
    this
  }

  private implicit var requestContentType = "text/html; charset=utf-8"

  def contentType(contentType: String): this.type = {
    requestContentType = contentType
    this
  }

  private implicit var requestCharset = Charset.UTF8

  def charset(charset: Charset.Charset): this.type = {
    requestCharset = charset
    this
  }

  private var requestUserAgent = UserAgent.IE10

  def userAgent(userAgent: String): this.type = {
    requestUserAgent = userAgent
    this
  }

  private var requestUserAgents: List[String] = List()

  def userAgents(userAgents: List[String]): this.type = {
    requestUserAgents = userAgents
    this
  }

  private var requestProxy: HttpProxy = null

  def proxy(proxy: HttpProxy): this.type = {
    requestProxy = proxy
    this
  }

  private implicit var requestProxies: List[HttpProxy] = List()

  def proxies(proxies: List[HttpProxy]): this.type = {
    requestProxies = proxies
    this
  }

  def html(nodeName: String, url: String, method: Method = Method.GET, body: AnyRef = null): Dom = {
    this.childNodeName = nodeName
    val formatUrl = EObject.replace(url, nodeData.toMap)
    val userAgent = if (requestUserAgents.nonEmpty) {
      requestUserAgents(rand.nextInt(requestUserAgents.length))
    } else {
      requestUserAgent
    }
    val proxy = if (requestProxies.nonEmpty) {
      requestProxies(rand.nextInt(requestProxies.length))
    } else {
      requestProxy
    }
    val content = HttpHelper.request(method, formatUrl, body, requestHeader.toMap)(requestContentType, requestCharset, userAgent, proxy)
    new Dom().init(this, Jsoup.parse(content))
  }

  def htmls(nodeName: String, urls: => Seq[String], method: Method = Method.GET, body: AnyRef = null): ParSeq[Dom] = {
    urls.par.map {
      html(nodeName, _, method, body)
    }
  }

  def sleep(millis: Long): this.type = {
    Thread.sleep(millis)
    this
  }

  def process(fun: JsonNode => Unit): this.type = {
    val json = EObject.packageFromParent(this)
    fun(json)
    this
  }

}

object EObject extends LazyLogging {

  private def packageFromParent(currObject: EObject, childJson: ObjectNode = null): JsonNode = {
    val currJson = JsonHelper.createObjectNode()
    if (childJson != null) {
      currJson.set(currObject.childNodeName, childJson)
    }
    currObject.nodeData.foreach {
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
      packageFromParent(currObject.parent, currJson)
    } else {
      currJson
    }
  }

  private val matchRegex ="""\{\w+\}""".r

  private def replace(path: String, nodeData: Map[String, Any]): String = {
    var pathR = path
    matchRegex.findAllMatchIn(path).foreach {
      m =>
        val name = m.group(0).substring(1, m.group(0).length - 1)
        if (nodeData.contains(name)) {
          val value = nodeData(name).asInstanceOf[String]
          pathR = pathR.replaceAll(s"""\\{$name\\}""", value)
        } else {
          logger.warn(s"Not found name [$name] in node data.")
        }
    }
    pathR
  }

}
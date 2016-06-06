package com.mdataset.excavator.core

import com.mdataset.excavator.Excavator
import com.mdataset.excavator.helper.Method.Method
import com.mdataset.excavator.helper.{Method, _}
import com.typesafe.scalalogging.slf4j.LazyLogging
import org.jsoup.Jsoup

trait HtmlVisitAble extends ENodeDef {

  private val requestHeader = collection.mutable.Map[String, String]()

  def header(key: String, value: String): this.type = {
    requestHeader += key -> value
    this
  }

  private var requestContentType = "text/html; charset=utf-8"

  def contentType(contentType: String): this.type = {
    requestContentType = contentType
    this
  }

  private var requestCharset = Charset.UTF8

  def charset(charset: Charset.Charset): this.type = {
    requestCharset = charset
    this
  }

  private var requestMethod = Method.GET

  def method(method: Method): this.type = {
    requestMethod = method
    this
  }

  private var requestBody: AnyRef = null

  def body(body: AnyRef): this.type = {
    requestBody = body
    this
  }

  private[excavator] def htmls(nodeName: String, url: String, fun: this.type => Unit): Unit = {
    this.childNodeName = nodeName
    val formatUrl = HtmlVisitAble.replace(url, data.toMap)
    val userAgent = Excavator.pickUserAgent
    val proxy = Excavator.pickProxy
    val content = HttpHelper.request(requestMethod, formatUrl, requestBody, requestHeader.toMap)(requestContentType, requestCharset, userAgent, proxy)
    fun(create(this, Jsoup.parse(content)))
  }

  private[excavator] def htmls(nodeName: String, urls: Seq[String], fun: this.type => Unit): Unit = {
    urls.par.foreach {
      htmls(nodeName, _, fun)
    }
  }

}

object HtmlVisitAble extends LazyLogging {

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

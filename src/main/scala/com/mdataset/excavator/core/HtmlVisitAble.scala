package com.mdataset.excavator.core

import com.mdataset.excavator.Excavator
import com.mdataset.excavator.http.Method.Method
import com.mdataset.excavator.http.{Charset, HttpHelper, Method}
import com.typesafe.scalalogging.slf4j.LazyLogging
import org.jsoup.Jsoup

trait HtmlVisitAble extends ENodeDef {

  private val requestHeader = collection.mutable.Map[String, String]()

  def header(key: String, value: String): this.type = {
    requestHeader += key -> value
    this
  }

  private var requestContentType = "text/html; charset=gb2312"

  def contentType(contentType: String): this.type = {
    requestContentType = contentType
    this
  }

  private var requestCharset = Charset.GB2312

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

  private var specialClientId: String = null

  def clientId(clientId: String): this.type = {
    specialClientId = clientId
    this
  }

  private[excavator] def html(nodeName: String, url: String, fun: this.type => Unit): Unit = {
    this.childNodeName = nodeName
    val formatUrl = HtmlVisitAble.replace(url, data.toMap)
    val userAgent = Excavator.pickUserAgent
    val proxy = Excavator.pickProxy
    val client =
      if (specialClientId != null) {
        HttpHelper.getAndAddClientByName(specialClientId, userAgent, proxy)
      } else {
        HttpHelper.getAndAddClientByHash(userAgent, proxy)
      }
    val content = client.request(requestMethod, formatUrl, requestBody, requestHeader.toMap)(requestContentType, requestCharset)
    fun(create(this, Jsoup.parse(content)))
  }

  private[excavator] def html(nodeName: String, urls: Seq[String], fun: this.type => Unit): Unit = {
    urls.par.foreach {
      html(nodeName, _, fun)
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

package com.mdataset.excavator.core

import com.mdataset.excavator.Excavator
import com.mdataset.excavator.helper.Method._
import com.mdataset.excavator.helper.{Method, _}
import com.typesafe.scalalogging.slf4j.LazyLogging
import org.jsoup.Jsoup

import scala.collection.parallel.ParSeq

trait HtmlVisitAble extends ENodeDef {

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

  def html(nodeName: String, url: String, method: Method = Method.GET, body: AnyRef = null): this.type = {
    this.childNodeName = nodeName
    val formatUrl = HtmlVisitAble.replace(url, data.toMap)
    val userAgent = Excavator.pickUserAgent
    val proxy = Excavator.pickProxy
    val content = HttpHelper.request(method, formatUrl, body, requestHeader.toMap)(requestContentType, requestCharset, userAgent, proxy)
    create(this, Jsoup.parse(content))
  }

  def htmls(nodeName: String, urls: => Seq[String], method: Method = Method.GET, body: AnyRef = null): ParSeq[this.type] = {
    urls.par.map {
      html(nodeName, _, method, body)
    }.asInstanceOf[ParSeq[this.type]]
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

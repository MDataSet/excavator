package com.mdataset.excavator

import java.util.Random

import com.mdataset.excavator.core.ENode
import com.mdataset.excavator.http.{HttpHelper, HttpProcessor, HttpProxy, UserAgent}


object Excavator {

  private val rand = new Random(System.currentTimeMillis())

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

  private[excavator] def pickUserAgent: String = {
    if (requestUserAgents.nonEmpty) {
      requestUserAgents(rand.nextInt(requestUserAgents.length))
    } else {
      requestUserAgent
    }
  }

  private var requestProxy: HttpProxy = _

  def proxy(proxy: HttpProxy): this.type = {
    requestProxy = proxy
    this
  }

  private var requestProxies: List[HttpProxy] = List()

  def proxies(proxies: List[HttpProxy]): this.type = {
    requestProxies = proxies
    this
  }

  private[excavator] def pickProxy: HttpProxy = {
    if (requestProxies.nonEmpty) {
      requestProxies(rand.nextInt(requestProxies.length))
    } else {
      requestProxy
    }
  }

  def start(): ENode = {
    new ENode()
  }

  val getHttpClient: HttpProcessor = {
    HttpHelper.getAndAddClientByHash(pickUserAgent, pickProxy)
  }

}

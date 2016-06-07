package com.mdataset.excavator.http

import com.typesafe.scalalogging.slf4j.LazyLogging

/**
  * HTTP请求辅助类
  */
object HttpHelper extends LazyLogging {

  private val httpClients: collection.mutable.Map[String, HttpProcessor] = collection.mutable.Map[String, HttpProcessor]()
  // 自动添加一个无需代理的client
  httpClients += "" -> HttpProcessor()

  def getClient: HttpProcessor = {
    httpClients("")
  }

  def getClient(name: String): HttpProcessor = {
    httpClients(name)
  }

  def getAndAddClientByName(name: String, userAgent: String = UserAgent.IE11, proxy: HttpProxy = null): HttpProcessor = {
    if (!httpClients.contains(name)) {
      val client = HttpProcessor(userAgent, proxy)
      httpClients += name -> client
    }
    httpClients(name)
  }

  def getAndAddClientByHash(userAgent: String = UserAgent.IE11, proxy: HttpProxy = null): HttpProcessor = {
    val hash = (userAgent + proxy.hashCode()).hashCode + ""
    if (!httpClients.contains(hash)) {
      val client = HttpProcessor(userAgent, proxy)
      httpClients += hash -> client
    }
    httpClients(hash)
  }

}














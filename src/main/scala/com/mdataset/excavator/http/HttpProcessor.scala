package com.mdataset.excavator.http

import java.io.File
import java.net.SocketException

import com.ecfront.common.JsonHelper
import com.mdataset.excavator.http.Method.Method
import com.typesafe.scalalogging.slf4j.LazyLogging
import org.apache.http.auth.{AuthScope, UsernamePasswordCredentials}
import org.apache.http.client.config.RequestConfig
import org.apache.http.client.entity.UrlEncodedFormEntity
import org.apache.http.client.methods.{HttpEntityEnclosingRequestBase, _}
import org.apache.http.cookie.Cookie
import org.apache.http.entity.{FileEntity, StringEntity}
import org.apache.http.impl.client.{BasicCookieStore, BasicCredentialsProvider, CloseableHttpClient, HttpClients}
import org.apache.http.message.BasicNameValuePair
import org.apache.http.util.EntityUtils
import org.apache.http.{HttpHeaders, HttpHost, NameValuePair, NoHttpResponseException}

import scala.collection.JavaConversions._

case class HttpProcessor(userAgent: String = UserAgent.IE11, proxy: HttpProxy = null) extends LazyLogging {

  private val cookieStore = new BasicCookieStore()
  private val httpClient: CloseableHttpClient =
    if (proxy != null && proxy.userName != null) {
      val credProvider = new BasicCredentialsProvider()
      credProvider.setCredentials(
        new AuthScope(proxy.hostName, proxy.port),
        new UsernamePasswordCredentials(proxy.userName, proxy.password))
      HttpClients.custom().setDefaultCookieStore(cookieStore).setDefaultCredentialsProvider(credProvider).build()
    } else {
      HttpClients.custom().setDefaultCookieStore(cookieStore).build()
    }

  private val methodConfig =
    if (proxy != null) {
      val proxyPost = new HttpHost(proxy.hostName, proxy.port)
      RequestConfig.custom().setProxy(proxyPost).build()
    } else {
      RequestConfig.custom().build()
    }

  def getCookies: List[Cookie] = {
    cookieStore.getCookies.toList
  }

  def request(method: Method, url: String, body: AnyRef = null, header: Map[String, String] = Map())
             (implicit contentType: String = "application/json; charset=utf-8",
              charset: Charset.Charset = Charset.UTF8): String = {
    method match {
      case Method.GET => get(url, header)(contentType, charset)
      case Method.POST => post(url, body, header)(contentType, charset)
      case Method.PUT => put(url, body, header)(contentType, charset)
      case Method.DELETE => delete(url, header)(contentType, charset)
    }
  }

  def post(url: String, body: AnyRef, header: Map[String, String] = Map())
          (implicit contentType: String = "application/json; charset=utf-8",
           charset: Charset.Charset = Charset.UTF8): String = {
    execute(new HttpPost(url), body, header, contentType, charset)
  }

  def put(url: String, body: AnyRef, header: Map[String, String] = Map())
         (implicit contentType: String = "application/json; charset=utf-8",
          charset: Charset.Charset = Charset.UTF8): String = {
    execute(new HttpPut(url), body, header, contentType, charset)
  }

  def get(url: String, header: Map[String, String] = Map())
         (implicit contentType: String = "application/json; charset=utf-8",
          charset: Charset.Charset = Charset.UTF8): String = {
    execute(new HttpGet(url), null, header, contentType, charset)
  }

  def delete(url: String, header: Map[String, String] = Map())
            (implicit contentType: String = "application/json; charset=utf-8",
             charset: Charset.Charset = Charset.UTF8): String = {
    execute(new HttpDelete(url), null, header, contentType, charset)
  }

  def upload(url: String, file: File, header: Map[String, String] = Map())
            (implicit contentType: String = "application/json; charset=utf-8",
             charset: Charset.Charset = Charset.UTF8): String = {
    execute(new HttpPost(url), file, header, null, charset)
  }

  private def execute(method: HttpRequestBase, body: AnyRef,
                      header: Map[String, String] = Map(),
                      contentType: String, charset: Charset.Charset, retry: Int = 0): String = {
    logger.debug(s"HTTP [${method.getMethod}] request : ${method.getURI}")
    method.setConfig(methodConfig)
    if (header != null&&header.nonEmpty) {
      header.foreach(h => method.addHeader(h._1, h._2))
    }
    method.addHeader(HttpHeaders.USER_AGENT, userAgent)
    if (contentType != null) {
      method.setHeader(HttpHeaders.CONTENT_TYPE, contentType)
    }
    method.setHeader(HttpHeaders.ACCEPT_ENCODING, "/")
    if (body != null) {
      val entity = body match {
        case b: String => new StringEntity(b, "UTF-8")
        case b: Map[_, _] =>
          val m = new java.util.ArrayList[NameValuePair]()
          b.asInstanceOf[Map[String, Any]].foreach {
            entry =>
              m.add(new BasicNameValuePair(entry._1, entry._2.toString))
          }
          new UrlEncodedFormEntity(m, "UTF-8")
        case b: File =>
          new FileEntity(b)
        case _ => new StringEntity(JsonHelper.toJsonString(body), "UTF-8")
      }
      method.asInstanceOf[HttpEntityEnclosingRequestBase].setEntity(entity)
    }
    try {
      val response = httpClient.execute(method)
      EntityUtils.toString(response.getEntity, charset.toString)
    } catch {
      case e if e.getClass == classOf[SocketException] || e.getClass == classOf[NoHttpResponseException] =>
        // 同络错误重试5次
        if (retry <= 5) {
          Thread.sleep(500)
          logger.warn(s"HTTP [${method.getMethod}] request  ${method.getURI} ERROR. retry [${retry + 1}] .")
          execute(method, body, header, contentType, charset, retry + 1)
        } else {
          logger.warn(s"HTTP [${method.getMethod}] request : ${method.getURI} ERROR.", e)
          throw e
        }
      case e: Exception =>
        logger.warn(s"HTTP [${method.getMethod}] request : ${method.getURI} ERROR.", e)
        throw e
    }
  }


  /**
    * 过滤有注入隐患的HTML标记
    *
    * @param str 原HTML内容
    * @return 安全的HTML内容
    */
  implicit def toSafe(str: String): Object {def safe: String} = new {
    def safe = {
      if (str != null && str.nonEmpty) {
        str.replaceAll("&", "&amp;").replaceAll("\\<", "&lt;").replaceAll("\\>", "&gt;").replaceAll("'", "&apos;").replaceAll("\"", "&quot;")
      } else {
        ""
      }
    }
  }

}

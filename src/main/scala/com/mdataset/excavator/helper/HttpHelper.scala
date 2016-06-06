package com.mdataset.excavator.helper

import java.io.{File, InputStream}
import java.net.SocketException
import java.util.zip.GZIPInputStream

import com.ecfront.common.JsonHelper
import com.mdataset.excavator.helper.Method.Method
import com.typesafe.scalalogging.slf4j.LazyLogging
import org.apache.http._
import org.apache.http.auth.{AuthScope, UsernamePasswordCredentials}
import org.apache.http.client.config.RequestConfig
import org.apache.http.client.entity.UrlEncodedFormEntity
import org.apache.http.client.methods._
import org.apache.http.entity.{FileEntity, HttpEntityWrapper, StringEntity}
import org.apache.http.impl.client.{BasicCredentialsProvider, CloseableHttpClient, HttpClients}
import org.apache.http.message.BasicNameValuePair
import org.apache.http.util.EntityUtils

/**
  * HTTP请求辅助类
  */
object HttpHelper extends LazyLogging {

  val httpClients: collection.mutable.Map[String, CloseableHttpClient] = collection.mutable.Map[String, CloseableHttpClient]()
  // 自动添加一个无需代理的client
  httpClients += "" -> HttpClients.createDefault

  def request(method: Method, url: String, body: AnyRef = null, header: Map[String, String] = Map())
             (implicit contentType: String = "application/json; charset=utf-8",
              charset: Charset.Charset = Charset.UTF8,
              userAgent: String = UserAgent.IE10, proxy: HttpProxy = null): String = {
    method match {
      case Method.GET => get(url, header)(contentType, charset, userAgent, proxy)
      case Method.POST => post(url, body, header)(contentType, charset, userAgent, proxy)
      case Method.PUT => put(url, body, header)(contentType, charset, userAgent, proxy)
      case Method.DELETE => delete(url, header)(contentType, charset, userAgent, proxy)
    }
  }

  def post(url: String, body: AnyRef, header: Map[String, String] = Map())
          (implicit contentType: String = "application/json; charset=utf-8",
           charset: Charset.Charset = Charset.UTF8,
           userAgent: String = UserAgent.IE10, proxy: HttpProxy = null): String = {
    execute(new HttpPost(url), body, header, contentType, charset, userAgent, proxy)
  }

  def put(url: String, body: AnyRef, header: Map[String, String] = Map())
         (implicit contentType: String = "application/json; charset=utf-8",
          charset: Charset.Charset = Charset.UTF8,
          userAgent: String = UserAgent.IE10, proxy: HttpProxy = null): String = {
    execute(new HttpPut(url), body, header, contentType, charset, userAgent, proxy)
  }

  def get(url: String, header: Map[String, String] = Map())
         (implicit contentType: String = "application/json; charset=utf-8",
          charset: Charset.Charset = Charset.UTF8,
          userAgent: String = UserAgent.IE10, proxy: HttpProxy = null): String = {
    execute(new HttpGet(url), null, header, contentType, charset, userAgent, proxy)
  }

  def delete(url: String, header: Map[String, String] = Map())
            (implicit contentType: String = "application/json; charset=utf-8",
             charset: Charset.Charset = Charset.UTF8,
             userAgent: String = UserAgent.IE10, proxy: HttpProxy = null): String = {
    execute(new HttpDelete(url), null, header, contentType, charset, userAgent, proxy)
  }

  def upload(url: String, file: File, header: Map[String, String] = Map())
            (implicit contentType: String = "application/json; charset=utf-8",
             charset: Charset.Charset = Charset.UTF8,
             userAgent: String = UserAgent.IE10, proxy: HttpProxy = null): String = {
    execute(new HttpPost(url), file, header, null, charset, userAgent, proxy)
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

  private def execute(method: HttpRequestBase, body: AnyRef,
                      header: Map[String, String] = Map(),
                      contentType: String, charset: Charset.Charset,
                      userAgent: String, proxy: HttpProxy, retry: Int = 0): String = {
    logger.debug(s"HTTP [${method.getMethod}] request : ${method.getURI}")
    val httpClient =
      if (proxy == null) {
        httpClients("")
      } else {
        val hash =
          if (proxy.userName != null) {
            proxy.hashCode() + ""
          } else {
            ""
          }
        if (!httpClients.contains(hash)) {
          val credsProvider = new BasicCredentialsProvider()
          credsProvider.setCredentials(
            new AuthScope(proxy.hostName, proxy.port),
            new UsernamePasswordCredentials(proxy.userName, proxy.password))
          httpClients += hash -> HttpClients.custom().setDefaultCredentialsProvider(credsProvider).build()
        }
        val proxyPost = new HttpHost(proxy.hostName, proxy.port)
        val config = RequestConfig.custom().setProxy(proxyPost).build()
        method.setConfig(config)
        httpClients(hash)
      }
    if (header != null) {
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
          execute(method, body, header, contentType, charset, userAgent, proxy, retry + 1)
        } else {
          logger.warn(s"HTTP [${method.getMethod}] request : ${method.getURI} ERROR.", e)
          throw e
        }
      case e: Exception =>
        logger.warn(s"HTTP [${method.getMethod}] request : ${method.getURI} ERROR.", e)
        throw e
    }
  }
}

/**
  * 网页编码
  */
object Charset extends Enumeration {
  type Charset = Value
  val UTF8 = Value("utf-8")
  val GB2312 = Value("gb2312")
  val GB18030 = Value("gb18030")
  val gbk = Value("gbk")
}


/**
  * 请求方法
  */
object Method extends Enumeration {
  type Method = Value
  val GET, POST, PUT, DELETE = Value
}

/**
  * 请求方法
  */
object UserAgent {
  val IE6 = "Mozilla/4.0 (compatible; MSIE 6.1; Windows NT 5.1)"
  val IE7 = "Mozilla/4.0 (compatible; MSIE 7.0; Windows NT 6.0)"
  val IE8 = "Mozilla/5.0 (compatible; MSIE 8.0; Windows NT 6.1; Trident/4.0)"
  val IE9 = "Mozilla/5.0 (compatible; MSIE 9.0; Windows NT 6.1; Trident/5.0)"
  val IE10 = "Mozilla/5.0 (compatible; MSIE 10.0; Windows NT 6.1; Trident/6.0)"
  val IE11 = "Mozilla/5.0 (Windows NT 6.1; WOW64; Trident/7.0; AS; rv:11.0) like Gecko"
  val EGDE = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/48.0.2564.82 Safari/537.36 Edge/14.14352"
  val CHROME = "Mozilla/5.0 (Windows NT 10.0; WOW64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/50.0.2661.102 Safari/537.36"
  val FIREFOX = "Mozilla/5.0 (Windows NT 6.1; WOW64; rv:40.0) Gecko/20100101 Firefox/40.1"
  val SAFARI = "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_9_3) AppleWebKit/537.75.14 (KHTML, like Gecko) Version/7.0.3 Safari/7046A194A"
}

/**
  * Gzip压缩装饰器
  *
  * @param entity 返回的http实体
  */
case class GzipDecompressingEntity(entity: HttpEntity) extends HttpEntityWrapper(entity) {

  override def getContent: InputStream = {
    new GZIPInputStream(wrappedEntity.getContent)
  }

  override def getContentLength: Long = {
    -1
  }
}

case class HttpProxy(hostName: String, port: Int, userName: String = null, password: String = null)



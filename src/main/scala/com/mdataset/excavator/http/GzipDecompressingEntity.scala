package com.mdataset.excavator.http

import java.io.InputStream
import java.util.zip.GZIPInputStream

import org.apache.http.HttpEntity
import org.apache.http.entity.HttpEntityWrapper

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

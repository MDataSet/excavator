package com.mdataset.excavator.http

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

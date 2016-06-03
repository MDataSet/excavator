package com.mdataset.excavator

import java.util.concurrent.CountDownLatch

import com.mdataset.excavator.core.ENode
import com.mdataset.excavator.helper.Charset
import com.typesafe.scalalogging.slf4j.LazyLogging
import org.scalatest.{BeforeAndAfter, FunSuite}

class BasicSpec extends FunSuite with BeforeAndAfter with LazyLogging {

  test("AutoHome Test") {

    Excavator.start().contentType("text/html; charset=gb2312").charset(Charset.GB2312)
      .htmls("root", ('A' to 'Z').map("http://www.autohome.com.cn/grade/carhtml/" + _ + ".html"))
      .foreach {
        _.doms("brand", "dl",
          _.setText("brandName", "dt")
            .doms("company", "dd",
              _.setText("companyName", ".h3-tit")
                .doms("series", "li:has(h4)",
                  _.setText("seriesName", "h4")
                    .setText("seriesId", _.attr("id").replace("s", ""))
                    .html("model", "http://car.autohome.com.cn/config/series/{seriesId}.html")
                    .setJson("config", "config", _.get("result").get("paramtypeitems"))
                    .setJson("option", "option", _.get("result").get("configtypeitems"))
                    .process(println)
                )
            )
        )
      }

    new CountDownLatch(1).await()
    /*

    // 理想接口
    Excavator.contentType("text/html; charset=gb2312").charset(Charset.GB2312)
      .htmls("root", ('A' to 'Z').map("http://www.autohome.com.cn/grade/carhtml/" + _ + ".html"))
      .doms("brand", "dl")
      .setText("brandName", "dt")
      .doms("company", "dd")
      .setText("companyName", ".h3-tit")
      .doms("series", "li:has(h4)")
      .setText("seriesName", "h4")
      .setText("seriesId", _.attr("id").replace("s", ""))
      .html("model", "http://car.autohome.com.cn/config/series/{seriesId}.html")
      .setJson("config", "config", _.get("result").get("paramtypeitems"))
      .setJson("option", "option", _.get("result").get("configtypeitems"))
      .process({
        json =>
          println(json)
      })*/

  }

}

package com.mdataset.excavator

import java.util.concurrent.CountDownLatch

import com.mdataset.excavator.helper.{Charset, HttpProxy, UserAgent}
import com.typesafe.scalalogging.slf4j.LazyLogging
import org.scalatest.{BeforeAndAfter, FunSuite}

class BasicSpec extends FunSuite with BeforeAndAfter with LazyLogging {

  test("AutoHome Test") {

    Excavator.userAgents(List(
      UserAgent.CHROME,
      UserAgent.IE11,
      UserAgent.EGDE
    )).proxies(List(
      HttpProxy("192.168.5.4", 7777)
    )).start()
      .contentType("text/html; charset=gb2312").charset(Charset.GB2312)
      .go("root", ('A' to 'Z').map("http://www.autohome.com.cn/grade/carhtml/" + _ + ".html"),
        _.go("brand", "dl",
          _.text("brandName", "dt")
            .go("company", "dd",
              _.text("companyName", ".h3-tit")
                .go("series", "li:has(h4)",
                  _.text("seriesName", "h4")
                    .text("seriesId", _.attr("id").replace("s", ""))
                    .go("model", "http://car.autohome.com.cn/config/series/{seriesId}.html",
                      _.json("config", "config", _.get("result").get("paramtypeitems"))
                        .json("option", "option", _.get("result").get("configtypeitems"))
                        .process(println)
                    )
                )
            )
        )
      )

    new CountDownLatch(1).await()

  }

}

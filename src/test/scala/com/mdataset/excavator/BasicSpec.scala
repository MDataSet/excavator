package com.mdataset.excavator

import com.mdataset.excavator.http.{Charset, HttpProxy, UserAgent}
import com.typesafe.scalalogging.slf4j.LazyLogging
import org.scalatest.{BeforeAndAfter, FunSuite}

class BasicSpec extends FunSuite with BeforeAndAfter with LazyLogging {

  test("Baidu Top Test") {
    import collection.JavaConversions._
    Excavator.start() // 开始抓取
      .go("root", Seq("http://top.baidu.com/")) {
      _.go("tech top", _.select("div .box-cont").filter(_.select(".tab .tab-control li.current").text() == "热门搜索")) {
        _.array("data", _.select(".tab-box .item-list a.list-title").map(_.text()))
          .process(println)
      }
    }
  }

  test("AutoHome Test") {

    Excavator.userAgents(List(
      UserAgent.CHROME,
      UserAgent.IE11,
      UserAgent.EGDE
    )).proxies(List(
      HttpProxy("192.168.5.4", 7777)
    )).start()
      .contentType("text/html; charset=gb2312").charset(Charset.GB2312)
      .go("root", ('A' to 'Z').map("http://www.autohome.com.cn/grade/carhtml/" + _ + ".html"))(
        _.go("brand", "dl")(
          _.text("brandName", "dt")
            .go("company", "dd")(
              _.text("companyName", ".h3-tit")
                .go("series", "li:has(h4)")(
                  _.text("seriesName", "h4")
                    .text("seriesId", _.attr("id").replace("s", ""))
                    .go("model", "http://car.autohome.com.cn/config/series/{seriesId}.html")(
                      _.json("config", "config", _.get("result").get("paramtypeitems"))
                        .json("option", "option", _.get("result").get("configtypeitems"))
                        .process(println)
                    )
                )
            )
        )
      )

  }

  test("xcar test") {
    Excavator.start()
      .go("root", "http://newcar.xcar.com.cn/price/")(
        _.go("brandContainer", "#img_load_box .container tr")(
          _.text("brandName", ".column_tit")
            .go("subBrandContainer", ".column_content")(
              _.text("subBrandName", ".tit")
                .go("modelContainer", "ul")(
                  _.text("modelName", ".item_list")
                    .text("modelUrl", _.select(".item_list a").attr("href"))
                    .go("trimPage", "http://newcar.xcar.com.cn{modelUrl}")(
                      _.go("tirmContainer", ".demio_table_list tr tr")(
                        _.text("trimName", ".list_version")
                          .text("trimPrice", ".guide_price")
                          .process({
                            d =>
                              println(d)
                          }, false))
                    )))))
  }

}

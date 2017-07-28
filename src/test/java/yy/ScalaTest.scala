package yy

import org.junit.{Assert, Test}
import yy.bean.ScalaObj
import yy.json.JSON
import java.lang.Long


/**
  * Created by <yuemenglong@126.com> on 2017/7/28.
  */
class ScalaTest {

  @Test
  def testGetterAndSetter(): Unit = {
    val obj = new ScalaObj
    obj.id = 1
    obj.name = "2"
    val json = JSON.stringify(obj)
    val jo = JSON.parse(json)
    val id = jo.asObj().getLong("id")
    val name = jo.asObj().getStr("name")
    Assert.assertEquals(id, 1L)
    Assert.assertEquals(name, "2")
  }

  @Test
  def testMap(): Unit = {
    val obj = new ScalaObj
    obj.id = 1
    obj.name = "2"
    obj.map += ("name" -> new ScalaObj)
    obj.map("name").name = "3"
    val json = JSON.stringify(obj)
    val jo = JSON.parse(json).asObj()
    val str = jo.asObj().getObj("map").get("name").asObj().getStr("name")
    Assert.assertEquals(str, "3")
  }


  @Test
  def testJs(): Unit = {
    {
      val arr: Array[Long] = Array(1L, 123456789012345L)
      val j1 = JSON.stringify(arr)
      val j2 = JSON.stringifyJs(arr)
      Assert.assertEquals(j1, "[1,123456789012345]")
      Assert.assertEquals(j2, "[1,\"123456789012345\"]")
    }
    {
      val map: Map[String, Long] = Map("a" -> 1L, "b" -> 123456789012345L)
      val j1 = JSON.stringify(map)
      val j2 = JSON.stringifyJs(map)
      Assert.assertEquals(j1, "{\"a\":1,\"b\":123456789012345}")
      Assert.assertEquals(j2, "{\"a\":1,\"b\":\"123456789012345\"}")
    }
  }
}

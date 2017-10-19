package io.github.yuemenglong.json.test

import java.lang.Long

import io.github.yuemenglong.json.JSON
import io.github.yuemenglong.json.parse.JsonObj
import io.github.yuemenglong.json.test.bean.{Obj, ScalaObj}
import org.junit.{Assert, Test}


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
      val arr: Array[Long] = Array(new Long(1L), new Long(123456789012345L))
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

  @Test
  def testEscape(): Unit = {
    val obj = new Obj

    {
      obj.setStringValue("\\name\\")
      val jo = JSON.convert(obj).asObj()
      Assert.assertEquals(jo.getStr("stringValue"), "\\name\\")
      Assert.assertEquals(jo.toString(), "{\"stringValue\":\"\\\\name\\\\\"}")
    }
    {
      obj.setStringValue("\"")
      val jo = JSON.convert(obj).asObj()
      Assert.assertEquals(jo.getStr("stringValue"), "\"")
      Assert.assertEquals(jo.toString(), "{\"stringValue\":\"\\\"\"}")
    }
    {
      obj.setStringValue("1\r\n2\t3")
      val jo = JSON.convert(obj).asObj()
      Assert.assertEquals(jo.getStr("stringValue"), "1\r\n2\t3")
      Assert.assertEquals(jo.toString(), "{\"stringValue\":\"1\\r\\n2\\t3\"}")
    }
  }
}

package io.github.yuemenglong.json.test

import java.lang.Long
import java.util

import io.github.yuemenglong.json.JSON
import io.github.yuemenglong.json.parse.{JsonNode, JsonObj}
import io.github.yuemenglong.json.test.bean.{Obj, ScalaObj, TestEnum}
import org.junit.{Assert, Before, Test}
import io.github.yuemenglong.json.JSON.types._

/**
 * Created by <yuemenglong@126.com> on 2017/7/28.
 */
class ScalaTest {
  @Before
  def before(): Unit = {
    JSON.setStringifyNull(false)
  }

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

  @Test
  def testParseToArray(): Unit = {
    {
      val arr = Array(1, 2, 3)
      val json = JSON.stringify(arr)
      Assert.assertEquals(json, "[1,2,3]")
      val arr2 = JSON.parse(json, classOf[Array[Long]])
      Assert.assertEquals(arr2(0).intValue(), 1)
      Assert.assertEquals(arr2(1).intValue(), 2)
      Assert.assertEquals(arr2(2).intValue(), 3)
      val json2 = JSON.stringify(arr2)
      Assert.assertEquals(json, json2)
    }
    {
      val arr = Array("1", "2", "3")
      val json = JSON.stringify(arr)
      val arr2 = JSON.parse(json, classOf[Array[String]])
      val json2 = JSON.stringify(arr2)
      Assert.assertEquals(json, json2)
    }
    {
      val arr = Array(1, 2, 3)
      val ja = JSON.parse(JSON.stringify(arr))
      val i = ja.asArr().array(0).as(classOf[Integer])
      Assert.assertEquals(i.intValue(), 1)
    }
  }

  @Test
  def testPath(): Unit = {
    val json = """{"aa":[{"bb":1},{"cc":2}]}"""
    val jo = JSON.parse(json)
    Assert.assertEquals(jo.path("aa[0].bb").asInt().intValue(), 1)
    Assert.assertEquals(jo.path("aa[1].bb").asInt(), null)
  }

  @Test
  def testMixObjAndJo(): Unit = {
    val arr = Array[JsonNode](JSON.obj(), JSON.obj())
    val json = JSON.stringify(arr)
    Assert.assertEquals(json, "[{},{}]")
  }

  @Test
  def testStringToBool(): Unit = {
    val json = "{\"booleanValue\":\"true\"}"
    val b = JSON.parse(json, classOf[Obj]).getBooleanValue.booleanValue()
    Assert.assertTrue(b)
  }

  @Test
  def testPretty(): Unit = {
    val jo = JSON.obj()
    jo.setStr("s", "abc")
    jo.setInt("a", 123)
    jo.set("arr", JSON.arr(Array(1, 2.3, "s", false, null, JSON.obj(Map("v" -> 1)))))
    jo.set("obj", JSON.obj(Map("a" -> false)))
    val exp = "{\n  \"s\": \"abc\",\n  \"a\": 123,\n  \"arr\": [1, 2.3, \"s\", false, null, {\n    \"v\": 1\n  }],\n  \"obj\": {\n    \"a\": false\n  }\n}"
    val act = JSON.pretty(jo)
    Assert.assertEquals(act, exp)
  }

  @Test
  def testIgnore(): Unit = {
    {
      val obj = new ScalaObj
      obj.ign = "ign"
      val json = JSON.stringify(obj)
      Assert.assertEquals(json, """{"map":{}}""")
    }

    {
      val json = """{"ign":"ign"}"""
      val obj = JSON.parse(classOf[ScalaObj], json)
      Assert.assertNull(obj.ign)
    }
  }

  @Test
  def testArrayArray(): Unit = {
    val arr: Array[Array[Double]] = Array(Array(1.1, 2.2), Array(3.3, 4.4))
    val json = JSON.stringifyJs(arr)
    val arr2: Array[Array[Double]] = JSON.parse(json, classOf[Array[Array[Double]]])
    arr.zip(arr2).foreach { case (a1, a2) =>
      a1.zip(a2).foreach { case (a, b) =>
        Assert.assertEquals(a, b)
      }
    }
  }

  @Test
  def testMap2(): Unit = {
    val json = """{"ign":"ign"}"""
    val map: Map[String, String] = JSON.parse(json).asObj().map.mapValues(_.asStr())
    Assert.assertEquals(map("ign"), "ign")
  }

  @Test
  def testEnum(): Unit = {
    val obj = new Obj
    obj.setTe(TestEnum.A)
    val json = JSON.stringifyJs(obj)
    Assert.assertEquals(json, """{"te":"A"}""")

    val obj2 = JSON.parse(json, classOf[Obj])
    Assert.assertEquals(obj2.getTe, TestEnum.A)

    val so = new ScalaObj
    so.map = null
    so.te = TestEnum.B
    val json2 = JSON.stringifyJs(so)
    Assert.assertEquals(json2, """{"te":"B"}""")

    val so2 = JSON.parse(json2, classOf[ScalaObj])
    Assert.assertEquals(so2.te, TestEnum.B)
  }
}

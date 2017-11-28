package io.github.yuemenglong.json.parse

import java.lang
import java.lang.reflect.{Field, Method}
import java.text.SimpleDateFormat
import java.util.Date
import java.math.BigDecimal
import java.sql.Timestamp

import io.github.yuemenglong.json.kit.Kit
import io.github.yuemenglong.json.lang.JsonDate

import scala.reflect.ClassTag
import scala.util.matching.Regex


/**
  * Created by Administrator on 2017/7/13.
  */
object Convert {
  val classOfJavaInteger: Class[Integer] = classOf[lang.Integer]
  val classOfJavaLong: Class[lang.Long] = classOf[lang.Long]
  val classOfJavaDouble: Class[lang.Double] = classOf[lang.Double]
  val classOfJavaBoolean: Class[lang.Boolean] = classOf[lang.Boolean]
  val classOfJavaString: Class[lang.String] = classOf[lang.String]

  val classOfScalaInt: Class[Int] = classOf[Int]
  val classOfScalaLong: Class[Long] = classOf[Long]
  val classOfScalaDouble: Class[Double] = classOf[Double]
  val classOfScalaBoolean: Class[Boolean] = classOf[Boolean]
  val classOfScalaString: Class[String] = classOf[String]

  val classOfBigDecimal: Class[BigDecimal] = classOf[BigDecimal]
  val classOfDate: Class[Date] = classOf[Date]
  val classOfTimestamp: Class[Timestamp] = classOf[Timestamp]

  val classOfJsonNode: Class[JsonNode] = classOf[JsonNode]

  val dateFormat: Regex = """(\d{4})-(\d{2})-(\d{2})""".r
  val dateTimeFormat: Regex = """(\d{4})-(\d{2})-(\d{2}) (\d{2}):(\d{2}):(\d{2})""".r

  var constructorMap: Map[Class[_], () => Any] = Map[Class[_], () => Any]()

  implicit def intToObject(x: Int): java.lang.Integer = new java.lang.Integer(x)

  implicit def longToObject(x: Long): java.lang.Long = new java.lang.Long(x)

  implicit def doubleToObject(x: Double): java.lang.Double = new java.lang.Double(x)

  implicit def booleanToObject(x: Boolean): java.lang.Boolean = new java.lang.Boolean(x)

  def setConstructorMap(map: Map[Class[_], () => Any]): Unit = {
    constructorMap = map
  }

  def toNumber(n: JsonValue, clazz: Class[_]): Object = {
    clazz match {
      case `classOfJavaInteger` | `classOfScalaInt` => new lang.Integer(n.toInt)
      case `classOfJavaLong` | `classOfScalaLong` => new lang.Long(n.toLong)
      case `classOfJavaDouble` | `classOfScalaDouble` => new lang.Double(n.toDouble)
    }
  }

  def fromNumber(value: Object): JsonValue = {
    value.getClass match {
      case `classOfJavaInteger` | `classOfScalaInt` => JsonLong(value.asInstanceOf[lang.Integer].longValue())
      case `classOfJavaLong` | `classOfScalaLong` => JsonLong(value.asInstanceOf[lang.Long].longValue())
      case `classOfJavaDouble` | `classOfScalaDouble` => JsonDouble(value.asInstanceOf[lang.Double].doubleValue())
    }
  }

  def toArray(arr: JsonArr, clazz: Class[_]): Array[Object] = {
    val itemClazz = Kit.getArrayType(clazz)
    val ct = ClassTag(itemClazz).asInstanceOf[ClassTag[Object]]
    val array = arr.array.map(fromNodeToValue(_, itemClazz)).toArray(ct)
    Kit.debug(s"ToArray Return: $array")
    array
  }

  def toMap(obj: JsonObj, clazz: Class[_]): Map[String, Object] = {
    obj.map
  }

  def toObject(obj: JsonObj, clazz: Class[_]): Object = {
    def setMethodNameJ(fieldName: String) = s"set${Kit.upperCaseFirst(fieldName)}"

    def setMethodNameS(fieldName: String) = s"${fieldName}_$$eq"

    val fields: Map[String, Field] = Kit.getDeclaredFields(clazz)
      .map(f => (f.getName, f))(collection.breakOut)
    val methods: Map[String, Method] = Kit.getDeclaredMethod(clazz)
      .map(m => (m.getName, m))(collection.breakOut)
    val ret = if (constructorMap.contains(clazz)) {
      constructorMap(clazz)()
    } else {
      clazz.newInstance()
    }
    obj.map.toArray.filter(p => fields.contains(p._1))
      .foreach { case (name, node) =>
        val field = fields(name)
        val value = fromNodeToValue(node, field.getType)
        val method = if (methods.contains(setMethodNameJ(name))) {
          methods(setMethodNameJ(name))
        } else if (methods.contains(setMethodNameS(name))) {
          methods(setMethodNameS(name))
        } else {
          throw new RuntimeException(s"No Setter Found Of $name")
        }
        method.invoke(ret, value.asInstanceOf[Object])
        Kit.debug(s"ToObject SetValue: $node, $name, ${field.getType}, $value")
      }
    Kit.debug(s"ToObject Return: $ret")
    ret.asInstanceOf[Object]
  }

  def fromObject(obj: Object): JsonObj = {
    def getMethodNameJ(fieldName: String) = s"get${Kit.upperCaseFirst(fieldName)}"

    def getMethodNameS(fieldName: String) = fieldName

    val fields: Array[Field] = Kit.getDeclaredFields(obj.getClass)
    val methods: Map[String, Method] = Kit.getDeclaredMethod(obj.getClass)
      .map(m => (m.getName, m))(collection.breakOut)
    val map: Map[String, JsonNode] = fields
      .filter(f => methods.contains(getMethodNameJ(f.getName)) || methods.contains(f.getName))
      .map(f => {
        val name = f.getName
        val value = if (methods.contains(getMethodNameJ(name))) {
          methods(getMethodNameJ(name)).invoke(obj)
        } else if (methods.contains(getMethodNameS(name))) {
          methods(getMethodNameS(name)).invoke(obj)
        } else {
          throw new RuntimeException(s"No Getter On $name")
        }
        val node = fromValueToNode(value, f)
        (name, node)
      })(collection.breakOut)
    JsonObj(map)
  }

  def fromArray(value: Object): JsonNode = {
    val arr = value.asInstanceOf[Array[_]].map(_.asInstanceOf[Object])
    JsonArr(arr.map(fromValueToNode))
  }

  def fromMap(value: Object): JsonNode = {
    val map = value.asInstanceOf[Map[String, Object]]
    JsonObj(map.mapValues(fromValueToNode))
  }

  def fromValueToNode(value: Object, field: Field): JsonNode = {
    if (value == null) {
      return JsonNull()
    }
    //noinspection TypeCheckCanBeMatch
    if (value.isInstanceOf[JsonNode]) {
      return value.asInstanceOf[JsonNode]
    }
    value.getClass match {
      case `classOfJavaInteger` | `classOfJavaLong` | `classOfJavaDouble`
           | `classOfScalaInt` | `classOfScalaLong` | `classOfScalaDouble` => fromNumber(value)
      case `classOfJavaString` | `classOfScalaString` => JsonStr(value.asInstanceOf[lang.String])
      case `classOfJavaBoolean` | `classOfScalaBoolean` => JsonBool(value.asInstanceOf[lang.Boolean])
      case `classOfBigDecimal` => JsonDouble(value.asInstanceOf[BigDecimal].doubleValue())
      case `classOfDate` | `classOfTimestamp` => if (field != null && field.getAnnotation(classOf[JsonDate]) != null) {
        JsonStr(new SimpleDateFormat("yyyy-MM-dd").format(value.asInstanceOf[Date]))
      } else {
        JsonStr(new SimpleDateFormat("yyyy-MM-dd HH:mm:ss").format(value.asInstanceOf[Date]))
      }
      case t => if (value.isInstanceOf[Map[_, _]]) {
        fromMap(value)
      } else if (t.isArray) {
        fromArray(value)
      } else {
        fromObject(value)
      }
    }
  }

  def fromValueToNode(value: Object): JsonNode = {
    fromValueToNode(value, null)
  }

  def fromNodeToValue(node: JsonNode, clazz: Class[_]): Object = {
    if (node.isInstanceOf[JsonNull]) {
      return null
    }
    val ret = (clazz, node) match {
      case (`classOfJavaString` | `classOfScalaString`, v: JsonValue) => v.toStr
      case (`classOfJavaInteger` | `classOfScalaInt`, v: JsonValue) => new lang.Integer(v.toInt)
      case (`classOfJavaLong` | `classOfScalaLong`, v: JsonValue) => new lang.Long(v.toLong)
      case (`classOfJavaDouble` | `classOfScalaDouble`, v: JsonValue) => new lang.Double(v.toDouble)
      case (`classOfJavaBoolean` | `classOfScalaBoolean`, v: JsonValue) => new lang.Boolean(v.toBool)
      case (`classOfJavaBoolean` | `classOfScalaBoolean`, v: JsonBool) => new lang.Boolean(v.value)
      case (`classOfBigDecimal`, v: JsonValue) => new BigDecimal(v.toStr)
      case (`classOfDate` | `classOfTimestamp`, v: JsonValue) => v.toStr match {
        case dateFormat(_*) => new SimpleDateFormat("yyyy-MM-dd").parse(v.toStr)
        case dateTimeFormat(_*) => new SimpleDateFormat("yyyy-MM-dd HH:mm:ss").parse(v.toStr)
        case _ => throw new RuntimeException(s"Invalid Date Format, ${v.toStr}")
      }
      case (_, v: JsonObj) => toObject(v, clazz)
      case (_, v: JsonArr) => toArray(v, clazz)
      case _ => throw new RuntimeException(s"Clazz And Node Type Not Match, ${clazz.getName}, ${node.toString}")
    }
    Kit.debug(s"ToValue Return: $ret")
    ret
  }
}

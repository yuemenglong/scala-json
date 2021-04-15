package io.github.yuemenglong.json.parse

import java.lang
import java.lang.reflect.{Field, Method}
import java.math.BigDecimal
import java.text.SimpleDateFormat
import java.util.Date

import io.github.yuemenglong.json.kit.Kit

import scala.reflect.ClassTag
import scala.util.matching.Regex


/**
 * Created by Administrator on 2017/7/13.
 */
//noinspection LanguageFeature
object Convert {
  val classOfJavaByte: Class[lang.Byte] = classOf[lang.Byte]
  val classOfJavaShort: Class[lang.Short] = classOf[lang.Short]
  val classOfJavaInteger: Class[Integer] = classOf[lang.Integer]
  val classOfJavaLong: Class[lang.Long] = classOf[lang.Long]
  val classOfJavaFloat: Class[lang.Float] = classOf[lang.Float]
  val classOfJavaDouble: Class[lang.Double] = classOf[lang.Double]
  val classOfJavaBoolean: Class[lang.Boolean] = classOf[lang.Boolean]
  val classOfJavaString: Class[lang.String] = classOf[lang.String]

  val classOfScalaByte: Class[Byte] = classOf[Byte]
  val classOfScalaShort: Class[Short] = classOf[Short]
  val classOfScalaInt: Class[Int] = classOf[Int]
  val classOfScalaLong: Class[Long] = classOf[Long]
  val classOfScalaFloat: Class[Float] = classOf[Float]
  val classOfScalaDouble: Class[Double] = classOf[Double]
  val classOfScalaBoolean: Class[Boolean] = classOf[Boolean]
  val classOfScalaString: Class[String] = classOf[String]

  val classOfBigDecimal: Class[BigDecimal] = classOf[BigDecimal]
  val classOfDate: Class[Date] = classOf[Date]
  val classOfSqlDate: Class[java.sql.Date] = classOf[java.sql.Date]
  val classOfTimestamp: Class[java.sql.Timestamp] = classOf[java.sql.Timestamp]

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
      case `classOfJavaByte` | `classOfScalaByte` => new lang.Byte(n.toByte)
      case `classOfJavaShort` | `classOfScalaShort` => new lang.Short(n.toShort)
      case `classOfJavaInteger` | `classOfScalaInt` => new lang.Integer(n.toInt)
      case `classOfJavaLong` | `classOfScalaLong` => new lang.Long(n.toLong)
      case `classOfJavaFloat` | `classOfScalaFloat` => new lang.Float(n.toFloat)
      case `classOfJavaDouble` | `classOfScalaDouble` => new lang.Double(n.toDouble)
      case _ => null
    }
  }

  def fromNumber(value: Object): JsonValue = {
    value.getClass match {
      case `classOfJavaByte` | `classOfScalaByte` => JsonLong(value.asInstanceOf[lang.Byte].longValue())
      case `classOfJavaShort` | `classOfScalaShort` => JsonLong(value.asInstanceOf[lang.Short].longValue())
      case `classOfJavaInteger` | `classOfScalaInt` => JsonLong(value.asInstanceOf[lang.Integer].longValue())
      case `classOfJavaLong` | `classOfScalaLong` => JsonLong(value.asInstanceOf[lang.Long].longValue())
      case `classOfJavaFloat` | `classOfScalaFloat` => JsonDouble(value.asInstanceOf[lang.Float].doubleValue())
      case `classOfJavaDouble` | `classOfScalaDouble` => JsonDouble(value.asInstanceOf[lang.Double].doubleValue())
      case _ => null
    }
  }

  def toArray(arr: JsonArr, clazz: Class[_]): Array[Object] = {
    val itemClazz = Kit.getArrayType(clazz)
    val ct = ClassTag(itemClazz).asInstanceOf[ClassTag[Object]]
    val array = arr.array.map(fromNodeToValue(_, itemClazz)).toArray(ct)
    array
  }

  def toMap(obj: JsonObj, clazz: Class[_]): Map[String, Object] = {
    obj.map
  }

  def toObject(obj: JsonObj, clazz: Class[_]): Object = {
    def setMethodNameJ(fieldName: String) = s"set${Kit.upperCaseFirst(fieldName)}"

    def setMethodNameS(fieldName: String) = s"${fieldName}_$$eq"

    val fields: Map[String, Field] = Kit.getValidFieldMap(clazz)
    val methods: Map[String, Method] = Kit.getValidMethodMap(clazz)
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
      }
    ret.asInstanceOf[Object]
  }

  def fromObject(obj: Object): JsonObj = {
    def getMethodNameJ(fieldName: String) = s"get${Kit.upperCaseFirst(fieldName)}"

    def getMethodNameS(fieldName: String) = fieldName

    val fields: Array[Field] = Kit.getValidFields(obj.getClass)
    val methods: Map[String, Method] = Kit.getValidMethodMap(obj.getClass)
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
    JsonObj(map, fields.map(_.getName))
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
    if (value.getClass.isEnum) {
      return JsonStr(String.valueOf(value))
    }
    val num = fromNumber(value)
    if (num != null) {
      return num
    }
    value.getClass match {
      case `classOfJavaString` | `classOfScalaString` => JsonStr(value.asInstanceOf[lang.String])
      case `classOfJavaBoolean` | `classOfScalaBoolean` => JsonBool(value.asInstanceOf[lang.Boolean])
      case `classOfBigDecimal` => JsonDouble(value.asInstanceOf[BigDecimal].doubleValue())
      case `classOfDate` | `classOfTimestamp` => JsonStr(new SimpleDateFormat("yyyy-MM-dd HH:mm:ss").format(value.asInstanceOf[Date]))
      case `classOfSqlDate` => JsonStr(new SimpleDateFormat("yyyy-MM-dd").format(value.asInstanceOf[Date]))
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
    if (clazz.isEnum) {
      return clazz.getDeclaredMethod("valueOf", classOf[String]).invoke(null, node.asInstanceOf[JsonValue].toStr)
    }
    val num = node match {
      case n: JsonValue => toNumber(n, clazz)
      case _ => null
    }
    if (num != null) {
      return num
    }
    val ret = (clazz, node) match {
      case (`classOfJavaString` | `classOfScalaString`, v: JsonValue) => v.toStr
      case (`classOfJavaBoolean` | `classOfScalaBoolean`, v: JsonValue) => new lang.Boolean(v.toBool)
      case (`classOfJavaBoolean` | `classOfScalaBoolean`, v: JsonBool) => new lang.Boolean(v.value)
      case (`classOfBigDecimal`, v: JsonValue) => new BigDecimal(v.toStr)
      case (`classOfDate` | `classOfSqlDate` | `classOfTimestamp`, v: JsonValue) =>
        val dateValue = v.toStr match {
          case dateFormat(_*) => new SimpleDateFormat("yyyy-MM-dd").parse(v.toStr)
          case dateTimeFormat(_*) => new SimpleDateFormat("yyyy-MM-dd HH:mm:ss").parse(v.toStr)
          case _ => throw new RuntimeException(s"Invalid Date Format, ${v.toStr}")
        }
        clazz match {
          case `classOfDate` => dateValue
          case `classOfSqlDate` => new java.sql.Date(dateValue.getTime)
          case `classOfTimestamp` => new java.sql.Timestamp(dateValue.getTime)
        }
      case (_, v: JsonObj) => toObject(v, clazz)
      case (_, v: JsonArr) => toArray(v, clazz)
      case _ => throw new RuntimeException(s"Clazz And Node Type Not Match, ${clazz.getName}, ${node.toString}")
    }
    ret
  }
}

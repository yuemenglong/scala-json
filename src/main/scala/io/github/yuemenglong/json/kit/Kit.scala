package io.github.yuemenglong.json.kit

import java.lang.reflect.{Field, Method, ParameterizedType}
import java.util.concurrent.ConcurrentHashMap

import io.github.yuemenglong.json.lang.JsonIgnore

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

/**
  * Created by Administrator on 2017/7/5.
  */
object Kit {

  var debugFlag = false

  def upperCaseFirst(str: String): String = {
    if (str == null) {
      return null
    }
    if (str.isEmpty) {
      return str
    }
    str(0).toUpper + str.substring(1)
  }

  def getArrayType(clazz: Class[_]): Class[_] = {
    if (!clazz.isArray) {
      return clazz
    }
    val name = clazz.getName.replaceAll("(^\\[L)|(;$)", "")
    Class.forName(name)
  }

  def isGenericType(clazz: Class[_]): Boolean = {
    clazz.getGenericSuperclass.isInstanceOf[ParameterizedType]
  }

  def getGenericType(clazz: Class[_]): Class[_] = {
    //返回表示此 Class 所表示的实体（类、接口、基本类型或 void）的直接超类的 Type。
    val genericType = clazz.getGenericSuperclass
    if (!genericType.isInstanceOf[ParameterizedType]) return clazz
    //返回表示此类型实际类型参数的 Type 对象的数组。
    val params = genericType.asInstanceOf[ParameterizedType].getActualTypeArguments
    params(0).asInstanceOf[Class[_]]
  }

  private val fieldCache: ConcurrentHashMap[Class[_], Array[Field]] = new ConcurrentHashMap[Class[_], Array[Field]]()
  private val methodCache: ConcurrentHashMap[Class[_], Array[Method]] = new ConcurrentHashMap[Class[_], Array[Method]]()
  private val fieldMapCache: ConcurrentHashMap[Class[_], Map[String, Field]] = new ConcurrentHashMap[Class[_], Map[String, Field]]()
  private val methodMapCache: ConcurrentHashMap[Class[_], Map[String, Method]] = new ConcurrentHashMap[Class[_], Map[String, Method]]()

  def getValidFieldMap(clazz: Class[_]): Map[String, Field] = {
    if (fieldMapCache.contains(clazz)) {
      return fieldMapCache.get(clazz)
    }
    val ret: Map[String, Field] = Kit.getValidFields(clazz)
      .map(f => (f.getName, f))(collection.breakOut)
    fieldMapCache.put(clazz, ret)
    ret
  }

  def getValidMethodMap(clazz: Class[_]): Map[String, Method] = {
    if (methodMapCache.contains(clazz)) {
      return methodMapCache.get(clazz)
    }
    val ret: Map[String, Method] = Kit.getValidMethod(clazz)
      .map(m => (m.getName, m))(collection.breakOut)
    methodMapCache.put(clazz, ret)
    ret
  }

  //  {
  //    val fields: Map[String, Field] = Kit.getDeclaredFields(clazz)
  //      .filter(f => f.getAnnotation(classOf[JsonIgnore]) == null)
  //      .map(f => (f.getName, f))(collection.breakOut)
  //    val methods: Map[String, Method] = Kit.getDeclaredMethod(clazz)
  //      .map(m => (m.getName, m))(collection.breakOut)
  //  }

  def getValidFields(clazz: Class[_]): Array[Field] = {
    if (fieldCache.contains(clazz)) {
      return fieldCache.get(clazz)
    }
    val ab = new ArrayBuffer[Field]()
    clazz.getDeclaredFields.foreach(ab += _)
    var parent = clazz.getSuperclass
    while (parent != null) {
      parent.getDeclaredFields.foreach(ab += _)
      parent = parent.getSuperclass
    }
    val ret = ab.filter(f => f.getAnnotation(classOf[JsonIgnore]) == null).toArray
    fieldCache.put(clazz, ret)
    ret
  }

  def getValidMethod(clazz: Class[_]): Array[Method] = {
    if (methodCache.contains(clazz)) {
      return methodCache.get(clazz)
    }
    val ab = new ArrayBuffer[Method]()
    clazz.getDeclaredMethods.foreach(ab += _)
    var parent = clazz.getSuperclass
    while (parent != null) {
      parent.getDeclaredMethods.foreach(ab += _)
      parent = parent.getSuperclass
    }
    val ret = ab.toArray
    methodCache.put(clazz, ret)
    ret
  }

  def escapeString(str: String): String = {
    """["\\\r\n\t]""".r.replaceAllIn(str, word => {
      word.group(0) match {
        case "\"" => "\\\\\""
        case "\\" => "\\\\\\\\"
        case "\r" => "\\\\r"
        case "\n" => "\\\\n"
        case "\t" => "\\\\t"
      }
    })
  }

  def unescapeString(c: Char): Char = {
    c match {
      case '"' => '"'
      case '\\' => '\\'
      case 'r' => '\r'
      case 'n' => '\n'
      case 't' => '\t'
    }
  }

  def debug(msg: String): Unit = {
    if (debugFlag) {
      println(msg)
    }
  }
}

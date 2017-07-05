package json.kit

import java.lang.reflect.{Field, ParameterizedType}

/**
  * Created by Administrator on 2017/7/5.
  */
object Kit {

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
}

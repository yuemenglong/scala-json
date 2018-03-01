# scala-json
一个scala写的轻量级json库

## 特点
* 高性能，比scala-util里的JSON库快20倍
* 支持对转js的特殊处理，将大的long型字段转为string型，防止js因精度问题丢失数据
* 自动处理循环引用问题(深度优先策略)
* 支持BigDecimal、Date等常用对象

## install
pom文件加入

    <!-- https://mvnrepository.com/artifact/io.github.yuemenglong/scala-json -->
    <dependency>
        <groupId>io.github.yuemenglong</groupId>
        <artifactId>scala-json</artifactId>
        <version>1.0.6</version>
    </dependency>

## demo

    import io.github.yuemenglong.json.JSON
    val json = JSON.stringify(new SomeObject())
    val obj = JSON.parse(json, classOf[SomeObject])

## 常用接口
def stringify(obj: Object): String //将对象转为JSON

def stringifyJs(obj: Object): String //将对象转为JSON，其中较大的long字段转为string类型，避免js因number类型精度问题丢失数据

def parse[T](json: String, clazz: Class[T]): T //将json字符串转为对象

def convert(obj: Object): JsonNode // 将json字符串转为JsonNode对象，可以在JsonNode上对字段进行增删改查操作


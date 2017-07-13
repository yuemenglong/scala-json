package yy;

import org.junit.Assert;
import org.junit.Test;
import yy.bean.Obj;
import yy.json.JSON;
import yy.json.parse.Convert;
import yy.json.parse.JsonNode;
import yy.json.parse.JsonObj;

/**
 * Created by Administrator on 2017/7/12.
 */
public class SimpleTest {
    @Test
    public void test() {
        String json = "{a:null,b:1,c:1.1,d:-1.1234,e:\"string\",f:{fa:1},g:[1,2,3]}";
        JsonNode root = JSON.parse(json);
        String conv = root.toString();
        System.out.println(conv);
    }

    @Test
    public void testConvert() throws IllegalAccessException, InstantiationException {
        Obj obj = new Obj();
        obj.setIntValue(1);
        obj.setLongValue(2L);
        obj.setDoubleValue(3.4);
        obj.setStringValue("str");
        obj.setBooleanValue(true);

        obj.setObjValue(new Obj());
        obj.getObjValue().setIntValue(10);
        obj.getObjValue().setLongValue(20L);

        JsonObj root = Convert.fromObject(obj);
        String s1 = root.toString();
        System.out.println(s1);
        Obj obj2 = (Obj) Convert.toObject(root, Obj.class);
        root = Convert.fromObject(obj2);
        String s2 = root.toString();
        System.out.println(s2);
        Assert.assertEquals(s1, s2);
    }
}

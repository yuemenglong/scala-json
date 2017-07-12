package yy;

import org.junit.Test;
import scala.collection.immutable.Map;
import yy.json.JSON;
import yy.json.parse.JsonNode;
import yy.json.parse.JsonObj;

/**
 * Created by Administrator on 2017/7/12.
 */
public class SimpleTest {
    @Test
    public void test(){
        String json = "{a:null,b:1,c:1.1,d:-1.1234,e:\"string\",f:{fa:1},g:[1,2,3]}";
        JsonNode root = JSON.parse(json);
        String conv = root.toString();
        System.out.println(conv);
    }
}

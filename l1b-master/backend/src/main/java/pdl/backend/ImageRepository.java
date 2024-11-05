package pdl.backend;
import java.io.IOException;
import java.util.Arrays;
import java.util.List;
import java.util.Map;

import org.springframework.beans.factory.InitializingBean;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.jdbc.core.JdbcTemplate;
import org.springframework.stereotype.Repository;

@Repository
public class ImageRepository implements InitializingBean {
    @Autowired
    private JdbcTemplate jdbcTemplate;

    private final String SQl_DROP_TABLE = "DROP TABLE IF EXISTS images";
    private final String SQL_CREATE_TABLE = "CREATE TABLE IF NOT EXISTS images (id bigserial PRIMARY KEY, name VARCHAR(255), histog vector(256), histohs vector(144), historgb vector(1000))";
    private final String SQL_ADD_COLOR_IMAGE = "INSERT INTO images (id, name, histohs, historgb) VALUES (?, ?, ?, ?)"; // +id +name +histoHS +histoRGB
    private final String SQL_ADD_GRAY_IMAGE = "INSERT INTO images (id, name, histog) VALUES (?, ?, ?)"; // +id +name +histoGRAY

    private final String SQL_DELETE_IMAGE = "DELETE FROM images WHERE id = ?"; // +id

    private final String SQL_GET_SIMILAR_1 = "SELECT id, "; // +nomHisto
    private final String SQL_GET_SIMILAR_2 = " <-> (SELECT "; // +nomHisto
    private final String SQL_GET_SIMILAR_3 = " FROM images WHERE id = "; // +id
    private final String SQL_GET_SIMILAR_4 = ") AS distance FROM images WHERE id != "; // +id
    private final String SQL_GET_SIMILAR_5 = " ORDER BY distance LIMIT ";  // +many


    @Override
    public void afterPropertiesSet() throws Exception {
        // Drop table
        jdbcTemplate.execute(SQl_DROP_TABLE);
        // Create table
        this.jdbcTemplate.execute(SQL_CREATE_TABLE);
    }

    public void add(Image img) {
        try {
            if (img.isGray()){
                double[] histoGRAY = Histogram.gray(img);
                jdbcTemplate.update(SQL_ADD_GRAY_IMAGE, img.getId(), img.getName(), histoGRAY);
            }else{
                double[] histoHS = Histogram.coupledHueSat(img);
                double[] histoRGB = Histogram.coupledRGB(img);
                jdbcTemplate.update(SQL_ADD_COLOR_IMAGE, img.getId(), img.getName(), histoHS, histoRGB);
            }
        } catch (IOException e) {
            e.printStackTrace();
        }
    }

    public void delete(long id) {
        jdbcTemplate.update(SQL_DELETE_IMAGE, id);
    }

    public List<Object> getSimilar(long id, String histo, long many){
        String request = SQL_GET_SIMILAR_1 + histo + SQL_GET_SIMILAR_2 + histo + SQL_GET_SIMILAR_3 + id + SQL_GET_SIMILAR_4 + id + SQL_GET_SIMILAR_5 + many;
        List<Map<String, Object>> list = jdbcTemplate.queryForList(request);
        long[] ids = new long[list.size()]; 
        int[] distances = new int[list.size()];
        
        int i=0;
        for(Map<String, Object>result : list){
            ids[i] = (long)result.get("id");
            double tmp = (double)result.get("distance");
            tmp = tmp*100;
            distances[i] = (int)(tmp);
            i++;
        }
        return Arrays.asList(ids, distances);
    }

}

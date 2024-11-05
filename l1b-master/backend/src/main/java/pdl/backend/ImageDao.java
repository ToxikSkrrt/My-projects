package pdl.backend;

import java.awt.image.BufferedImage;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.nio.file.Files;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.Properties;

import javax.imageio.ImageIO;

import org.springframework.beans.factory.InitializingBean;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.MediaType;
import org.springframework.stereotype.Repository;


@Repository
public class ImageDao implements Dao<Image>, InitializingBean {
  @Autowired
  ImageRepository gallery;

  private final Map<Long, Image> images = new HashMap<>();

  public final Map<Long, Image> getMapImages() {
    return this.images;
  }

  public ImageDao() {
    File tmpdir = new File("").getAbsoluteFile();
    File dir = new File(tmpdir.getParent() + "/images");
    File filesList[] = dir.listFiles();

    for (File file : filesList) {
      String fileName = file.getName();

      Optional<String> test = Optional.ofNullable(fileName).filter(f -> f.contains("."))
          .map(f -> f.substring(fileName.lastIndexOf(".") + 1));

      if (!test.equals(Optional.ofNullable("png")) && !test.equals(Optional.ofNullable("jpg")))
        continue;

      try {
        BufferedImage imgtemp = ImageIO.read(file);
        boolean isGray = ImageProcessing.isGrayImage(imgtemp);
        byte[] fileContent = Files.readAllBytes(file.toPath());

        Properties properties = new Properties();
        FileInputStream in = new FileInputStream(dir.toString() + "/metadata.properties");

        properties.load(in);
        in.close();

        String value = properties.getProperty(fileName + ".likes");

        if (value == null) {
          properties.setProperty(fileName + ".likes", "0");

          FileOutputStream out = new FileOutputStream(dir.toString() + "/metadata.properties");

          properties.store(out, null);
          out.close();

          value = "0";
        }

        Image img = new Image(fileName, fileContent,
            test.equals(Optional.ofNullable("png")) ? MediaType.IMAGE_PNG : MediaType.IMAGE_JPEG,
            imgtemp.getWidth() + "*" + imgtemp.getHeight(), isGray, Integer.valueOf(value));
        images.put(img.getId(), img);

      } catch (IOException e) {
        e.printStackTrace();
      }
    }
  }

  @Override
  public void afterPropertiesSet() throws Exception {
    for (Image img : images.values()) {
      gallery.add(img);
    }
  }

  @Override
  public Optional<Image> retrieve(final long id) {
    return Optional.ofNullable(images.get(id));
  }

  @Override
  public List<Image> retrieveAll() {
    return new ArrayList<Image>(images.values());
  }

  @Override
  public void create(final Image img) {
    gallery.add(img);
    images.put(img.getId(), img);

    File tmpdir = new File("").getAbsoluteFile();
    File dir = new File(tmpdir.getParent() + "/images");

    File file = new File(dir, img.getName());
    try {
      file.createNewFile();
    } catch (IOException e) {
      e.printStackTrace();
    }
    try {
      FileOutputStream fos = new FileOutputStream(file);
      fos.write(img.getData());
      fos.close();

      Properties properties = new Properties();
      FileInputStream in = new FileInputStream(dir.toString() + "/metadata.properties");

      properties.load(in);
      in.close();

      properties.setProperty(file.getName() + ".likes", "0");

      FileOutputStream out = new FileOutputStream(dir.toString() + "/metadata.properties");

      properties.store(out, null);
      out.close();

    } catch (IOException e) {
      e.printStackTrace();
    }
  }

  @Override
  public void update(final Image img, final String[] params) {
    img.setName(Objects.requireNonNull(params[0], "Name cannot be null"));

    images.put(img.getId(), img);
  }

  @Override
  public void delete(final Image img) {
    gallery.delete(img.getId());
    images.remove(img.getId());

    File tmpdir = new File("").getAbsoluteFile();
    File file = new File(tmpdir.getParent() + "/images/" + img.getName());

    Properties properties = new Properties();

    try {
      FileInputStream in = new FileInputStream(tmpdir.getParent() + "/images/metadata.properties");

      properties.load(in);
      in.close();

      properties.remove(img.getName() + ".likes");

      FileOutputStream out = new FileOutputStream(tmpdir.getParent() + "/images/metadata.properties");

      properties.store(out, null);
      out.close();

    } catch (IOException e) {
      e.printStackTrace();
    }

    file.delete();
  }

  public void incrLikes(final String name, final Long id) {
    File tmpdir = new File("").getAbsoluteFile().getParentFile();

    Image img = images.get(id);
    img.upVote();
    images.put(id, img);

    Properties properties = new Properties();
    try {
      FileInputStream in = new FileInputStream(tmpdir.toString() + "/images/metadata.properties");

      properties.load(in);
      in.close();

      String value = properties.getProperty(name + ".likes");
      int res = Integer.valueOf(value) + 1;

      properties.setProperty(name + ".likes", String.valueOf(res));

      FileOutputStream out = new FileOutputStream(tmpdir.toString() + "/images/metadata.properties");

      properties.store(out, null);
      out.close();

    } catch (IOException e) {
      e.printStackTrace();
    }
  }

  public void decrLikes(final String name, final Long id) {
    File tmpdir = new File("").getAbsoluteFile().getParentFile();

    Image img = images.get(id);
    img.downVote();
    images.put(id, img);

    Properties properties = new Properties();
    try {
      FileInputStream in = new FileInputStream(tmpdir.toString() + "/images/metadata.properties");

      properties.load(in);
      in.close();

      String value = properties.getProperty(name + ".likes");
      int res = Integer.valueOf(value) - 1;

      properties.setProperty(name + ".likes", String.valueOf(res));

      FileOutputStream out = new FileOutputStream(tmpdir.toString() + "/images/metadata.properties");

      properties.store(out, null);
      out.close();

    } catch (IOException e) {
      e.printStackTrace();
    }
  }
}

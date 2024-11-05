package pdl.backend;

import java.awt.image.BufferedImage;
import java.io.IOException;
import java.util.List;
import java.util.Optional;

import javax.imageio.ImageIO;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.HttpStatusCode;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.ResponseBody;
import org.springframework.web.bind.annotation.RestController;
import org.springframework.web.multipart.MultipartFile;
import org.springframework.web.servlet.mvc.support.RedirectAttributes;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.node.ArrayNode;
import com.fasterxml.jackson.databind.node.ObjectNode;

@RestController
public class ImageController {

  @Autowired
  private ImageRepository gallery;

  @Autowired
  private ObjectMapper mapper;

  private final ImageDao imageDao;

  public ImageController(ImageDao imageDao) {
    this.imageDao = imageDao;
  }

  @SuppressWarnings("null")
  @RequestMapping(value = "/images/{id}", method = RequestMethod.GET, produces = MediaType.IMAGE_JPEG_VALUE)
  public ResponseEntity<?> getImage(@PathVariable("id") long id) {

    Optional<Image> image = imageDao.retrieve(id);

    if (image.isPresent()) {
      return ResponseEntity.ok().contentType(MediaType.IMAGE_JPEG).body(image.get().getData());
    }
    return new ResponseEntity<>("Image id=" + id + " not found.", HttpStatus.NOT_FOUND);
  }

  @RequestMapping(value = "/images/{id}", method = RequestMethod.DELETE)
  public ResponseEntity<?> deleteImage(@PathVariable("id") long id) {

    Optional<Image> image = imageDao.retrieve(id);

    if (image.isPresent()) {
      imageDao.delete(image.get());
      return new ResponseEntity<>("Image id=" + id + " deleted.", HttpStatus.OK);
    }
    return new ResponseEntity<>("Image id=" + id + " not found.", HttpStatus.NOT_FOUND);
  }

  @SuppressWarnings("null")
  @RequestMapping(value = "/images", method = RequestMethod.POST)
  public ResponseEntity<?> addImage(@RequestParam("file") MultipartFile file, RedirectAttributes redirectAttributes) {

    String contentType = file.getContentType();
    if (!(contentType.equals(MediaType.IMAGE_JPEG.toString()) || contentType.equals(MediaType.IMAGE_PNG.toString()))) {
      return new ResponseEntity<>("Only JPEG / PNG file format supported", HttpStatus.UNSUPPORTED_MEDIA_TYPE);
    }
    try {
      BufferedImage imgtemp = ImageIO.read(ImageIO.createImageInputStream(file.getInputStream()));
      boolean isGray = ImageProcessing.isGrayImage(imgtemp);
      imageDao.create(new Image(file.getOriginalFilename(), file.getBytes(),
          contentType.equals(MediaType.IMAGE_JPEG.toString()) ? MediaType.IMAGE_JPEG : MediaType.IMAGE_PNG,
          imgtemp.getWidth() + "*" + imgtemp.getHeight(), isGray, 0));
    } catch (IOException e) {
      return new ResponseEntity<>("Failure to read file", HttpStatus.UNSUPPORTED_MEDIA_TYPE);
    }
    return new ResponseEntity<>("Image uploaded", HttpStatus.CREATED);
  }

  @RequestMapping(value = "/images/similar/{id}", method = RequestMethod.GET, produces = "application/json; charset=UTF-8")
  @ResponseBody
  public ResponseEntity<?> similarImage(@PathVariable("id") long id, @RequestParam("many") String many,
      @RequestParam("histo") String histo) {
  
    if (!this.imageDao.retrieve(id).isPresent()) {
      return new ResponseEntity<>(HttpStatus.NOT_FOUND);
    }

    long NMany = Integer.valueOf(many);
    int NHisto = Integer.parseInt(histo);
    String SHisto;
    if (NHisto == 1)
      SHisto = "histog";
    else if (NHisto == 2)
      SHisto = "histohs";
    else if (NHisto == 3)
      SHisto = "historgb";
    else
      return new ResponseEntity<>(HttpStatus.BAD_REQUEST);

    List<Object> result = gallery.getSimilar(id, SHisto, NMany);

    long[] ids = (long[]) result.get(0);
    int[] distances = (int[]) result.get(1);

    ArrayNode nodes = mapper.createArrayNode();
    for (int i = 0; i < ids.length; i++) {
      Image image = imageDao.retrieve(ids[i]).get();
      ObjectNode objectNode = mapper.createObjectNode();
      objectNode.put("id", ids[i]);
      objectNode.put("name", image.getName());
      objectNode.put("type", image.getType().getSubtype());
      objectNode.put("size", image.getSize());
      objectNode.put("score", distances[i]);
      objectNode.put("likes", image.getLikes());
      nodes.add(objectNode);
    }
    return ResponseEntity.ok().body(nodes);
  }

  @RequestMapping(value = "/images/treatment/{id}", method = RequestMethod.GET, produces = "application/json; charset=UTF-8")
  @ResponseBody
  public ResponseEntity<?> treatmentImage(@PathVariable("id") long id, @RequestParam("filter") String filter,
      @RequestParam("value") String value) {
    if (!this.imageDao.retrieve(id).isPresent()) {
      return new ResponseEntity<>(HttpStatus.NOT_FOUND);
    }

    int NFilter = Integer.parseInt(filter);
    int NValue = Integer.parseInt(value);
    if (NFilter < 1 || NFilter > 8 || NValue < 0)
      return new ResponseEntity<>(HttpStatus.BAD_REQUEST);

    Image image = imageDao.retrieve(id).get();

    try {
      Image outputImage = ImageProcessing.ControllerTreatment(image, NFilter, NValue);

      imageDao.create(outputImage);
      return ResponseEntity.ok().body(outputImage.getId());

    } catch (IOException e) {
      e.printStackTrace();
    }

    return new ResponseEntity<>("Only JPEG / PNG file format supported", HttpStatus.UNSUPPORTED_MEDIA_TYPE);

  }

  @RequestMapping(value = "/images", method = RequestMethod.GET, produces = "application/json")
  @ResponseBody
  public ArrayNode getImageList() {
    List<Image> images = imageDao.retrieveAll();
    ArrayNode nodes = mapper.createArrayNode();
    for (Image image : images) {
      ObjectNode objectNode = mapper.createObjectNode();
      objectNode.put("id", image.getId());
      objectNode.put("name", image.getName());
      objectNode.put("type", image.getType().getSubtype());
      objectNode.put("size", image.getSize());
      objectNode.put("isgray", image.isGray());
      objectNode.put("likes", image.getLikes());
      nodes.add(objectNode);
    }
    return nodes;
  }

  @RequestMapping(value = "/images/upVote/{name}/{id}", method = RequestMethod.GET)
  public void upVote(@PathVariable("name") String name, @PathVariable("id") Long id) {
    imageDao.incrLikes(name, id);
  }

  @RequestMapping(value = "/images/downVote/{name}/{id}", method = RequestMethod.GET)
  public void downVote(@PathVariable("name") String name, @PathVariable("id") Long id) {
    imageDao.decrLikes(name, id);
  }

}

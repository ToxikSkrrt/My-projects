package pdl.backend;

import org.springframework.http.MediaType;

public class Image {
  private static Long count = Long.valueOf(0);
  private Long id;
  private String name;
  private MediaType type;
  private String size;
  private byte[] data;
  private int likes;
  private boolean isGray;

  public Image(final String name, final byte[] data, final MediaType type, final String size, final Boolean isGray,
      final int likes) {
    id = count++;
    this.name = name;
    this.type = type;
    this.size = size;
    this.data = data;
    this.isGray = isGray;
    this.likes = likes;
  }

  public long getId() {
    return id;
  }

  public String getName() {
    return name;
  }

  public void setName(final String name) {
    this.name = name;
  }

  public byte[] getData() {
    return data;
  }

  public MediaType getType() {
    return type;
  }

  public String getSize() {
    return size;
  }

  public void upVote() {
    this.likes++;
  }

  public void downVote() {
    this.likes--;
  }

  public int getLikes() {
    return this.likes;
  }

  public Boolean isGray() {
    return isGray;
  }
}

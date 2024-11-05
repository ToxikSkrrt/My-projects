package pdl.backend;

import static org.junit.jupiter.api.Assertions.fail;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.delete;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.result.MockMvcResultHandlers.print;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

import java.io.File;

import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.content;

import org.junit.jupiter.api.MethodOrderer.OrderAnnotation;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Order;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.TestMethodOrder;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.web.servlet.AutoConfigureMockMvc;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.core.io.ClassPathResource;
import org.springframework.http.MediaType;
import org.springframework.mock.web.MockMultipartFile;
import org.springframework.test.util.ReflectionTestUtils;
import org.springframework.test.web.servlet.MockMvc;
import org.springframework.test.web.servlet.request.MockMvcRequestBuilders;

@SpringBootTest
@AutoConfigureMockMvc
@TestMethodOrder(OrderAnnotation.class)
public class ImageControllerTests {

	@Autowired
	private MockMvc mockMvc; 

	@BeforeAll
	public static void reset() {
		// reset Image class static counter
		ReflectionTestUtils.setField(Image.class, "count", Long.valueOf(0));
	}

	@SuppressWarnings("null")
	@Test
	@Order(1)
	public void getImageListShouldReturnSuccess() throws Exception {
		this.mockMvc.perform(get("/images")).andDo(print()).andExpect(status().isOk())
				.andExpect(content().contentType(MediaType.APPLICATION_JSON));
	}

	@Test
	@Order(2)
	public void getImageShouldReturnNotFound() throws Exception {
		this.mockMvc.perform(get("/images/999")).andDo(print()).andExpect(status().isNotFound());
	}

	@Test
	@Order(3)
	public void getImageShouldReturnSuccess() throws Exception {
		this.mockMvc.perform(get("/images/0")).andExpect(status().isOk());
	}

	@Test
	@Order(4)
	public void deleteImagesShouldReturnMethodNotAllowed() throws Exception {
		this.mockMvc.perform(delete("/images")).andDo(print()).andExpect(status().isMethodNotAllowed());
	}

	@Test
	@Order(5)
	public void deleteImageShouldReturnNotFound() throws Exception {
		this.mockMvc.perform(delete("/images/99999")).andDo(print()).andExpect(status().isNotFound());
	}

	// Problème de suppression définitive de l'image d'ID 0 à chaque execution du
	// test à fix (car on perd une image définitivement à chaque compilation)
	// (mais le test est bien fonctionnel !)

	// @Test
	// @Order(6)
	// public void deleteImageShouldReturnSuccess() throws Exception {
	// this.mockMvc.perform(delete("/images/0")).andDo(print()).andExpect(status().isOk());
	// }

	@Test
	@Order(6)
	public void createImageShouldReturnSuccess() throws Exception {
		final ClassPathResource imgFile = new ClassPathResource("test.jpg");
		MockMultipartFile multipartFile = new MockMultipartFile("file", "test.jpg", "image/jpeg",
				imgFile.getInputStream());
		this.mockMvc.perform(MockMvcRequestBuilders.multipart("/images").file(multipartFile)).andDo(print())
				.andExpect(status().isCreated());
	}

	@Test
	@Order(7)
	public void createImageShouldReturnUnsupportedMediaType() throws Exception {
		MockMultipartFile multipartFile = new MockMultipartFile("file", "test.jpg", "text/plain", "Test".getBytes());
		this.mockMvc.perform(MockMvcRequestBuilders.multipart("/images").file(multipartFile)).andDo(print())
				.andExpect(status().isUnsupportedMediaType());
	}

	@Test
	@Order(8)
	public void getSimilarImageShouldReturnNotFound() throws Exception {
		this.mockMvc.perform(get("/images/similar/99999?histo=3&many=5")).andDo(print()).andExpect(status().isNotFound());
	}

	@Test
	@Order(9)
	public void getSimilarImageShouldReturnBadRequest() throws Exception {
		this.mockMvc.perform(get("/images/similar/0?histo=99&many=5")).andDo(print()).andExpect(status().isBadRequest());
	}

	//OK

	@Test
	@Order(10)
	public void getTraitementImageShouldReturnNotFound() throws Exception {
		this.mockMvc.perform(get("/images/treatment/99999?filter=3&value=20")).andDo(print()).andExpect(status().isNotFound());
	}

	@Test
	@Order(11)
	public void getTraitementImageShouldReturnBadRequest() throws Exception {
		this.mockMvc.perform(get("/images/treatment/0?filter=50&value=20")).andDo(print()).andExpect(status().isBadRequest());
	}
		
	@Test
	@Order(12)
	public void getTraitementImageShouldReturnSuccess() throws Exception {
		this.mockMvc.perform(get("/images/treatment/0?filter=4&value=20")).andDo(print()).andExpect(status().isOk());
	}

	@Test
	@Order(13)
	public boolean doFolderExist() throws Exception {
		File dir = new File(new File("").getParent() + "/images");
		if (!dir.exists()) 
			fail("'images' repository does not exist");
		return true;
	}
}

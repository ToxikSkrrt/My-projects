package pdl.backend;

import java.awt.image.BufferedImage;
import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.util.Properties;

import javax.imageio.ImageIO;

import boofcv.alg.color.ColorHsv;
import boofcv.io.image.ConvertBufferedImage;
import boofcv.struct.image.GrayF32;
import boofcv.struct.image.GrayS16;
import boofcv.struct.image.GrayU8;
import boofcv.struct.image.ImageType;
import boofcv.struct.image.Planar;

// 1 : threshold (Gray)
// 2 : contrast (Gray)
// 3 : equalization (Gray)
// 4 : brightness (Gray & Color)
// 5 : meanFilter (Gray & Color)
// 6 : outline detect with sobel (Gray)
// 7 : colorToGray (Color)
// 8 : colorFilter (Color)

public class ImageProcessing {

	public void setImageProcessProperties(String name) {
		File dir = new File(new File("").getAbsoluteFile().getParent() + "/images");

		try {
			Properties properties = new Properties();
			FileInputStream in = new FileInputStream(dir.toString() + "/metadata.properties");

			properties.load(in);
			in.close();

			properties.setProperty(name + ".likes", "0");

			FileOutputStream out = new FileOutputStream(dir.toString() + "/metadata.properties");

			properties.store(out, null);
			out.close();
		} catch (IOException e) {
			e.printStackTrace();
		}
	}

	public static void threshold(GrayU8 input, int t) {
		for (int y = 0; y < input.height; ++y) {
			for (int x = 0; x < input.width; ++x) {
				int gl = input.get(x, y);
				if (gl < t)
					gl = 0;
				else
					gl = 255;
				input.set(x, y, gl);
			}
		}
	}

	public static void contrast(GrayU8 input) {
		int start = input.get(0, 0);
		int min = start;
		int max = start;
		int[] LUT = new int[256];

		for (int y = 0; y < input.height; ++y) {
			for (int x = 0; x < input.width; ++x) {
				int gl = input.get(x, y);
				if (gl < min)
					min = gl;
				if (gl > max)
					max = gl;
			}
		}
		for (int i = 0; i <= 255; ++i) {
			LUT[i] = (255 * i) / (max - min);
		}
		for (int y = 0; y < input.height; ++y) {
			for (int x = 0; x < input.width; ++x) {
				input.set(x, y, LUT[input.get(x, y)]);
			}
		}
	}

	public static void equalization(GrayU8 input) {
		int[] histogram = new int[256];
		for (int y = 0; y < input.height; ++y) {
			for (int x = 0; x < input.width; ++x) {
				histogram[input.get(x, y)]++;
			}
		}
		for (int i = 1; i <= 255; ++i) {
			histogram[i] = histogram[i - 1] + histogram[i];
		}
		for (int y = 0; y < input.height; ++y) {
			for (int x = 0; x < input.width; ++x) {
				input.set(x, y, ((histogram[input.get(x, y)] * 255) / histogram[255]));
			}
		}
	}

	public static void brightness(Planar<GrayU8> image, int delta) {
		for (int i = 0; i < 3; i++) { // Parcours les 3 niveaux de gris (rgb)
			brightness(image.getBand(i), delta);
		}
	}

	private static void brightness(GrayU8 input, int delta) {
		for (int y = 0; y < input.height; ++y) {
			for (int x = 0; x < input.width; ++x) {
				int gl = input.get(x, y) + delta;
				if (gl > 255)
					gl = 255;
				if (gl < 0)
					gl = 0;
				input.set(x, y, gl);
			}
		}
	}

	public static void meanFilter(Planar<GrayU8> input, Planar<GrayU8> output, int size) {
		for (int i = 0; i < 3; i++) {
			meanFilter(input.getBand(i), output.getBand(i), size);
		}
	}

	private static void meanFilter(GrayU8 input, GrayU8 output, int size) {
		for (int y = 0; y < input.height; ++y) {
			for (int x = 0; x < input.width; ++x) {
				int nb = 0;
				long total = 0;
				int Yobjectif = y - (size / 2);
				int Xobjectif = x - (size / 2);
				for (int ybis = Yobjectif; ybis < Yobjectif + size; ybis++) {
					for (int xbis = Xobjectif; xbis < Xobjectif + size; xbis++) {
						if (xbis >= 0 && xbis < input.width && ybis >= 0 && ybis < input.height) {
							nb++;
							total = total + input.get(xbis, ybis);
						}
					}
				}
				int bl = (int) (total / nb);
				output.set(x, y, bl);
			}
		}
	}

	private static void convolution(GrayU8 input, GrayS16 output, int[][] kernel) {
		int size = kernel.length;
		int half = (size - 1) / 2;
		for (int y = half; y < input.height - half; ++y) {
			for (int x = half; x < input.width - half; ++x) {
				long total = 0;
				for (int xbis = 0; xbis < size; ++xbis)
					for (int ybis = 0; ybis < size; ++ybis)
						total = total + (input.get(x - half + xbis, y - half + ybis) * (kernel[xbis][ybis]));
				output.set(x, y, (int) total);
			}
		}
	}

	private static void gradientImage(GrayU8 input, GrayU8 output, int[][] kernelX, int[][] kernelY) {
		GrayS16 imageX = new GrayS16(input.width, input.height);
		GrayS16 imageY = new GrayS16(input.width, input.height);
		convolution(input, imageX, kernelX);
		convolution(input, imageY, kernelY);
		for (int y = 0; y < input.height; ++y) {
			for (int x = 0; x < input.width; ++x) {
				int gap = imageX.get(x, y) - imageY.get(x, y);
				if (gap < 0)
					gap = gap * (-1);
				output.set(x, y, gap);
			}
		}
	}

	public static void gradientImageSobel(GrayU8 input, GrayU8 output) {
		int[][] kernelX = { { -1, 0, 1 }, { -2, 0, 2 }, { -1, 0, 1 } };
		int[][] kernelY = { { -1, -2, -1 }, { 0, 0, 0 }, { 1, 2, 1 } };
		gradientImage(input, output, kernelX, kernelY);
	}

	public static void colorToGray(Planar<GrayU8> input, GrayU8 output) {
		for (int y = 0; y < input.height; ++y) {
			for (int x = 0; x < input.width; ++x) {
				int r = input.getBand(0).get(x, y);
				int g = input.getBand(1).get(x, y);
				int b = input.getBand(2).get(x, y);
				int gl = (int) (0.3 * r + 0.59 * g + 0.11 * b); // Valeurs griser
				output.set(x, y, gl);
			}
		}
	}

	public static void colorFilter(Planar<GrayF32> input, double tint) {
		if (tint > 359)
			tint = 359;
		if (tint < 0)
			tint = 0;
		tint = Math.toRadians(tint);
		Planar<GrayF32> hsv = input.createSameShape();
		ColorHsv.rgbToHsv(input, hsv);
		for (int y = 0; y < hsv.height; ++y) {
			for (int x = 0; x < hsv.width; ++x) {
				hsv.getBand(0).set(x, y, (float) tint);
			}
		}
		ColorHsv.hsvToRgb(hsv, input);
	}

	public static Image ControllerTreatment(Image img, int filter, int value) throws IOException {
		BufferedImage buffered = ImageIO.read(new ByteArrayInputStream(img.getData()));
		if (buffered == null) {
			throw new IOException("buffered image null");
		}

		if (img.isGray()) {
			GrayU8 U8 = newGU8(buffered);
			if (filter > 0 && filter < 5) {
				if (filter == 1)
					threshold(U8, value);
				else if (filter == 2) {
					contrast(U8);
					value = -1;
				} else if (filter == 3) {
					equalization(U8);
					value = -1;
				} else
					brightness(U8, value);
				BufferedImage bufferedU8 = ConvertBufferedImage.convertTo(U8, null, true);
				byte[] byteArr = ConvertBufferedToByte(bufferedU8, img.getType().getSubtype());
				return new Image(NameImage(img.getName(), img.getType().getSubtype(), filter, value), byteArr,
						img.getType(), img.getSize(), isGrayImage(bufferedU8), 0);
			}
			GrayU8 outputU8 = newGU8(buffered);
			if (filter == 5)
				meanFilter(U8, outputU8, value);
			if (filter == 6) {
				gradientImageSobel(U8, outputU8);
				value = -1;
			}
			BufferedImage bufferedU8 = ConvertBufferedImage.convertTo(outputU8, null, true);
			byte[] byteArr = ConvertBufferedToByte(ConvertBufferedImage.convertTo(outputU8, null, true),
					img.getType().getSubtype());
			return new Image(NameImage(img.getName(), img.getType().getSubtype(), filter, value), byteArr,
					img.getType(), img.getSize(), isGrayImage(bufferedU8), 0);
		} else {
			Planar<GrayU8> U8 = newPU8(buffered);
			if (filter > 3 && filter < 6) {
				if (filter == 4) {
					brightness(U8, value);
					BufferedImage bufferedU8 = ConvertBufferedImage.convertTo(U8, null, true);
					byte[] byteArr = ConvertBufferedToByte(ConvertBufferedImage.convertTo(U8, null, true),
							img.getType().getSubtype());
					return new Image(NameImage(img.getName(), img.getType().getSubtype(), filter, value), byteArr,
							img.getType(), img.getSize(), isGrayImage(bufferedU8), 0);
				}
				Planar<GrayU8> outputU8 = newPU8(buffered);
				meanFilter(U8, outputU8, value);
				BufferedImage bufferedU8 = ConvertBufferedImage.convertTo(outputU8, null, true);
				byte[] byteArr = ConvertBufferedToByte(bufferedU8, img.getType().getSubtype());
				return new Image(NameImage(img.getName(), img.getType().getSubtype(), filter, value), byteArr,
						img.getType(), img.getSize(), isGrayImage(bufferedU8), 0);
			} else {
				if (filter == 7) {
					GrayU8 GU8 = newGU8(buffered);
					colorToGray(U8, GU8);
					value = -1;
					BufferedImage bufferedGU8 = ConvertBufferedImage.convertTo(GU8, null, true);
					byte[] byteArr = ConvertBufferedToByte(bufferedGU8, img.getType().getSubtype());
					return new Image(NameImage(img.getName(), img.getType().getSubtype(), filter, value), byteArr,
							img.getType(), img.getSize(), isGrayImage(bufferedGU8), 0);
				}
				Planar<GrayF32> F32 = newPF32(buffered);
				colorFilter(F32, value);
				BufferedImage bufferedF32 = ConvertBufferedImage.convertTo(F32, null, true);
				byte[] byteArr = ConvertBufferedToByte(bufferedF32, img.getType().getSubtype());
				return new Image(NameImage(img.getName(), img.getType().getSubtype(), filter, value), byteArr,
						img.getType(), img.getSize(), isGrayImage(bufferedF32), 0);
			}
		}
	}

	private static String NameImage(String name, String extension, int filter, int value) {
		String[] filterNames = { "", "threshold", "contrast", "equalization", "brightness", "meanFilter",
				"outlineDetect", "colorToGray", "colorFilter" };
		int dernierPoint = name.lastIndexOf(".");
		String nomSansExtension = name.substring(0, dernierPoint);
		String newName;
		if (value == -1)
			newName = nomSansExtension + "_" + filterNames[filter] + "." + extension;
		else
			newName = nomSansExtension + "_" + filterNames[filter] + "(" + value + ")." + extension;
		return newName;
	}

	private static byte[] ConvertBufferedToByte(BufferedImage bufferedImage, String type) throws IOException {
		ByteArrayOutputStream baos = new ByteArrayOutputStream();
		ImageIO.write(bufferedImage, type, baos);
		byte[] byteArr = baos.toByteArray();
		return byteArr;
	}

	private static GrayU8 newGU8(BufferedImage buffered) {
		GrayU8 U8 = new GrayU8();
		U8.reshape(buffered.getWidth(), buffered.getHeight());
		ConvertBufferedImage.convertFrom(buffered, U8, true);
		return U8;
	}

	private static Planar<GrayU8> newPU8(BufferedImage buffered) {
		Planar<GrayU8> U8 = new Planar<>(GrayU8.class, 1, 1, 3);
		U8.reshape(buffered.getWidth(), buffered.getHeight());
		ConvertBufferedImage.convertFrom(buffered, U8, true);
		return U8;
	}

	private static Planar<GrayF32> newPF32(BufferedImage buffered) {
		Planar<GrayF32> F32 = new Planar<>(GrayF32.class, 1, 1, 3);
		F32.reshape(buffered.getWidth(), buffered.getHeight());
		ConvertBufferedImage.convertFrom(buffered, F32, true);
		return F32;
	}

	private static double meanDifference(GrayU8 img1, GrayU8 img2) {
		double sumDiff = 0.0;
		for (int y = 0; y < img1.height; y++) {
			for (int x = 0; x < img1.width; x++) {
				sumDiff += Math.abs(img1.get(x, y) - img2.get(x, y));
			}
		}
		return sumDiff / (img1.width * img1.height);
	}

	public static boolean isGrayImage(BufferedImage img) {
		Planar<GrayU8> boofImage = ConvertBufferedImage.convertFrom(img, true, ImageType.pl(3, GrayU8.class));

		// Vérifier si toutes les bandes de l'image sont identiques
		boolean isGray = true;
		for (int i = 1; i < boofImage.getNumBands(); i++) {
			double meanDiff = ImageProcessing.meanDifference(boofImage.getBand(0), boofImage.getBand(i));
			if (meanDiff > 0.0) {
				isGray = false;
				break;
			}
		}
		return isGray;
	}
}

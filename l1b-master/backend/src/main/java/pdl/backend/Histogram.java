package pdl.backend;
import java.awt.image.BufferedImage;
import java.io.ByteArrayInputStream;
import java.io.IOException;

import javax.imageio.ImageIO;

import boofcv.alg.color.ColorHsv;
import boofcv.alg.descriptor.UtilFeature;
import boofcv.alg.feature.color.GHistogramFeatureOps;
import boofcv.alg.feature.color.Histogram_F64;
import boofcv.io.image.ConvertBufferedImage;
import boofcv.struct.image.GrayF32;
import boofcv.struct.image.GrayU8;
import boofcv.struct.image.Planar;

public class Histogram {
	public static double[] gray(Image img) throws IOException {
		GrayU8 U8 = new GrayU8();

		BufferedImage buffered = ImageIO.read(new ByteArrayInputStream(img.getData()));

		if (buffered == null) {
			throw new IOException("buffered image null");
		}

		U8.reshape(buffered.getWidth(), buffered.getHeight());
		ConvertBufferedImage.convertFrom(buffered, U8, true);

		double[] histogram = new double[256];

        for (int y = 0; y < U8.height; y++) {
            for (int x = 0; x < U8.width; x++) {
                histogram[U8.get(x, y)]++;
            }
        }

        int totalPixels = U8.width * U8.height;
        for (int i = 0; i < histogram.length; i++) {
            histogram[i] /= totalPixels;
        }

        return histogram;
	}

	public static double[] coupledHueSat(Image img) throws IOException {
		Planar<GrayF32> rgb = new Planar<>(GrayF32.class, 1, 1, 3);
		Planar<GrayF32> hsv = new Planar<>(GrayF32.class, 1, 1, 3);

		BufferedImage buffered = ImageIO.read(new ByteArrayInputStream(img.getData()));

		if (buffered == null) {
			throw new IOException("buffered image null");
		}

		rgb.reshape(buffered.getWidth(), buffered.getHeight());
		hsv.reshape(buffered.getWidth(), buffered.getHeight());

		ConvertBufferedImage.convertFrom(buffered, rgb, true);
		ColorHsv.rgbToHsv(rgb, hsv);

		Planar<GrayF32> hs = hsv.partialSpectrum(0, 1);

		// The number of bins is an important parameter. Try adjusting it
		Histogram_F64 histogram = new Histogram_F64(12, 12);
		histogram.setRange(0, 0, 2.0 * Math.PI); // range of hue is from 0 to 2PI
		histogram.setRange(1, 0, 1.0); // range of saturation is from 0 to 1

		// Compute the histogram
		GHistogramFeatureOps.histogram(hs, histogram);

		UtilFeature.normalizeL2(histogram); // normalize so that image size doesn't matter

		return histogram.data;
	}

	public static double[] coupledRGB(Image img) throws IOException {
		Planar<GrayF32> rgb = new Planar<>(GrayF32.class, 1, 1, 3);

		BufferedImage buffered = ImageIO.read(new ByteArrayInputStream(img.getData()));

		if (buffered == null) {
			throw new IOException("buffered image null");
		}
		rgb.reshape(buffered.getWidth(), buffered.getHeight());
		ConvertBufferedImage.convertFrom(buffered, rgb, true);


		// The number of bins is an important parameter. Try adjusting it
		Histogram_F64 histogram = new Histogram_F64(10, 10, 10);
		histogram.setRange(0, 0, 255);
		histogram.setRange(1, 0, 255);
		histogram.setRange(2, 0, 255);

		if (rgb.getNumBands() == 4){ //RGBA !
			Planar<GrayF32> rgbImage = new Planar<>(GrayF32.class, rgb.width, rgb.height, 3);
			rgbImage.bands[0] = rgb.bands[0]; // Canal rouge
			rgbImage.bands[1] = rgb.bands[1]; // Canal vert
			rgbImage.bands[2] = rgb.bands[2]; // Canal bleu

			GHistogramFeatureOps.histogram(rgbImage, histogram);
		}
		else GHistogramFeatureOps.histogram(rgb, histogram);

		UtilFeature.normalizeL2(histogram); // normalize so that image size doesn't matter

		return histogram.data;
	}
}

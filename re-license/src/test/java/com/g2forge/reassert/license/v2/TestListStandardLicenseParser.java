package com.g2forge.reassert.license.v2;

import java.io.IOException;
import java.io.InputStream;
import java.util.List;
import java.util.stream.Collectors;

import org.junit.Test;
import org.junit.runner.RunWith;
import org.junit.runners.Parameterized;
import org.junit.runners.Parameterized.Parameter;
import org.junit.runners.Parameterized.Parameters;

import com.fasterxml.jackson.databind.ObjectReader;
import com.fasterxml.jackson.dataformat.csv.CsvMapper;
import com.fasterxml.jackson.module.paranamer.ParanamerModule;
import com.g2forge.alexandria.java.core.resource.HResource;
import com.g2forge.alexandria.java.io.RuntimeIOException;
import com.g2forge.alexandria.test.HAssert;
import com.g2forge.reassert.core.api.licenseparser.ILicenseParser;
import com.g2forge.reassert.core.model.contract.license.ILicense;
import com.g2forge.reassert.core.model.contract.license.UnknownLicense;
import com.g2forge.reassert.license.StandardLicense;

import lombok.AccessLevel;
import lombok.Builder;
import lombok.Data;
import lombok.Getter;
import lombok.RequiredArgsConstructor;

@RunWith(Parameterized.class)
public class TestListStandardLicenseParser {
	@Data
	@Builder(toBuilder = true)
	@RequiredArgsConstructor
	public static class TestCase {
		protected final String license;

		protected final String description;
		
		protected final String purpose;
	}

	@Parameters(name = "{1}")
	public static List<Object[]> computeTestParameters() {
		final List<TestCase> licenses;

		{
			final CsvMapper mapper = new CsvMapper();
			mapper.registerModule(new ParanamerModule());
			final ObjectReader reader = mapper.readerFor(TestCase.class).with(mapper.schemaFor(TestCase.class).withHeader().withColumnReordering(true));
			try (final InputStream stream = HResource.getResourceAsStream(TestListStandardLicenseParser.class, "license descriptions.csv", true)) {
				licenses = reader.<TestCase>readValues(stream).readAll();
			} catch (IOException e) {
				throw new RuntimeIOException(e);
			}
		}

		return licenses.stream().map(x -> {
			final ILicense license = (x.getLicense().isEmpty()) ? new UnknownLicense(x.getDescription()) : StandardLicense.valueOfSPDX(x.getLicense());
			return new Object[] { license, x.getDescription(), x.getPurpose() };
		}).collect(Collectors.toList());
	}

	@Getter(lazy = true, value = AccessLevel.PROTECTED)
	private final ILicenseParser parser = StandardLicenseParser.create();

	@Getter(AccessLevel.PROTECTED)
	@Parameter(0)
	public ILicense license;

	@Getter(AccessLevel.PROTECTED)
	@Parameter(1)
	public String text;
	
	@Getter(AccessLevel.PROTECTED)
	@Parameter(2)
	public String purpose;

	@Test
	public void test() {
		final ILicense actual = getParser().parse(getText());
		HAssert.assertEquals(getPurpose(), getLicense(), actual);
	}
}

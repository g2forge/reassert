package com.g2forge.reassert.standard.model.contract.license.parser;

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
import com.g2forge.reassert.core.api.parser.IParser;
import com.g2forge.reassert.core.model.contract.license.ILicenseApplied;
import com.g2forge.reassert.core.model.contract.license.UnknownLicense;
import com.g2forge.reassert.standard.model.contract.license.StandardLicense;

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

		protected final String text;

		protected final String purpose;
	}

	@Parameters(name = "{1}")
	public static List<Object[]> computeTestParameters() {
		final List<TestCase> testCases;

		{
			final CsvMapper mapper = new CsvMapper();
			mapper.registerModule(new ParanamerModule());
			final ObjectReader reader = mapper.readerFor(TestCase.class).with(mapper.schemaFor(TestCase.class).withHeader().withColumnReordering(true));
			try (final InputStream stream = HResource.getResourceAsStream(TestListStandardLicenseParser.class, "license texts.csv", true)) {
				testCases = reader.<TestCase>readValues(stream).readAll();
			} catch (IOException e) {
				throw new RuntimeIOException(e);
			}
		}

		return testCases.stream().map(testCase -> {
			final ILicenseApplied license;
			if (testCase.getLicense().isEmpty()) license = new UnknownLicense(testCase.getText());
			else {
				ILicenseApplied temp = null;
				try {
					temp = StandardLicense.valueOfSPDX(testCase.getLicense());
				} catch (IllegalArgumentException e0) {
					try {
					temp = StandardLicense.valueOf(testCase.getLicense());
					} catch (IllegalArgumentException e1) {
						e1.addSuppressed(e0);
						throw e1;
					}
				}
				license = temp;
			}
			return new Object[] { license, testCase.getText(), testCase.getPurpose() };
		}).collect(Collectors.toList());
	}

	@Getter(lazy = true, value = AccessLevel.PROTECTED)
	private final IParser<ILicenseApplied> parser = StandardLicenseParser.create();

	@Getter(AccessLevel.PROTECTED)
	@Parameter(0)
	public ILicenseApplied license;

	@Getter(AccessLevel.PROTECTED)
	@Parameter(1)
	public String text;

	@Getter(AccessLevel.PROTECTED)
	@Parameter(2)
	public String purpose;

	@Test
	public void test() {
		final ILicenseApplied actual = getParser().parse(getText());
		HAssert.assertEquals(getPurpose(), getLicense(), actual);
	}
}

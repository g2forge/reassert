package com.g2forge.reassert.standard.model.contract.license;

import java.io.IOException;
import java.io.InputStream;
import java.util.Collections;
import java.util.Comparator;
import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import org.junit.Test;
import org.junit.runner.RunWith;
import org.junit.runners.Parameterized;
import org.junit.runners.Parameterized.Parameter;

import com.fasterxml.jackson.databind.ObjectReader;
import com.fasterxml.jackson.dataformat.csv.CsvMapper;
import com.fasterxml.jackson.module.paranamer.ParanamerModule;
import com.g2forge.alexandria.java.core.error.UnreachableCodeError;
import com.g2forge.alexandria.java.core.resource.HResource;
import com.g2forge.alexandria.java.io.RuntimeIOException;
import com.g2forge.alexandria.test.HAssert;
import com.g2forge.alexandria.test.HAssume;
import com.g2forge.reassert.core.api.parser.IParser;
import com.g2forge.reassert.core.model.contract.license.ILicenseApplied;
import com.g2forge.reassert.core.model.contract.license.ILicenseFamily;
import com.g2forge.reassert.core.model.contract.license.ILicenseSpecific;
import com.g2forge.reassert.core.model.contract.license.LicenseVersion;

import lombok.AccessLevel;
import lombok.Getter;

@RunWith(Parameterized.class)
public abstract class ATestStandardLicenseParser {
	public interface ITestCase {
		public ILicenseApplied getLicenseApplied();

		public String getPurpose();

		public String getText();
	}

	public static <TestCase extends ITestCase> List<Object[]> computeTestParameters(Class<TestCase> testCaseType, String file) {
		final List<TestCase> testCases;

		{
			final CsvMapper mapper = new CsvMapper();
			mapper.registerModule(new ParanamerModule());
			final ObjectReader reader = mapper.readerFor(testCaseType).with(mapper.schemaFor(testCaseType).withHeader().withColumnReordering(true));
			try (final InputStream stream = HResource.getResourceAsStream(testCaseType, file, true)) {
				testCases = reader.<TestCase>readValues(stream).readAll();
			} catch (IOException e) {
				throw new RuntimeIOException(e);
			}
		}

		return testCases.stream().map(testCase -> new Object[] { testCase.getLicenseApplied(), testCase.getText(), testCase.getPurpose() }).collect(Collectors.toList());
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

	protected void assumeParseable() {
		HAssume.assumeTrue("Unparseable licenses don't need this test", isParseable());
	}

	@Test
	public void family() {
		assumeParseable();

		final ILicenseFamily license = (ILicenseFamily) getLicense();
		{
			final List<StandardLicenseFamily> families = Stream.of(StandardLicenseFamily.values()).filter(license::isChild).collect(Collectors.toList());
			if (families.size() > 1) {
				for (int i = 0; i < families.size(); i++) {
					for (int j = i + 1; j < families.size(); j++) {
						final StandardLicenseFamily o1 = families.get(i), o2 = families.get(j);
						if (o1.equals(o2) || o1.isChild(o2) || o2.isChild(o1)) continue;
						HAssert.fail(String.format("%1$s and %2$s cannot be compared", o1, o2));
					}
				}
			}
		}

		final ILicenseFamily family = license.getFamily();
		HAssume.assumeNotNull("Licenses without a family are always family consistent, of course", family);
		HAssert.assertTrue(license.isChild(family));
		HAssert.assertNull(family.getSPDXShortID());

		HAssume.assumeTrue("License families have their own naming scheme", license instanceof ILicenseSpecific);
		HAssume.assumeFalse("Licenses in the permissive family are not consistently named", family == StandardLicenseFamily.Permissive);
		HAssert.assertTrue(String.format("Short ID \"%1$s\" did not contain family short ID \"%2$s\"!", license.getShortID(), family.getShortID()), license.getShortID().contains(family.getShortID()));
	}

	protected ReferenceTerms getReference(final ILicenseFamily license) {
		try {
			return ReferenceTerms.valueOfShortID(license.getShortID());
		} catch (IllegalArgumentException exception) {}
		HAssume.assumeTrue("Failed to find reference terms for " + license.getName(), false);
		throw new UnreachableCodeError();
	}

	protected boolean isParseable() {
		return getPurpose().isEmpty();
	}

	@Test
	public void name() {
		assumeParseable();

		final ILicenseFamily license = (ILicenseFamily) getLicense();
		HAssume.assumeTrue("License families have their own naming scheme", license instanceof ILicenseSpecific);
		HAssume.assumeFalse("Creative commons licenses have their own naming scheme", license.getFamily() == StandardLicenseFamily.CC);

		final String expected = license.getShortID().replace('-', ' ') + " license";
		HAssert.assertEquals(expected, license.getName());
	}

	@Test
	public void parse() {
		final ILicenseApplied actual = getParser().parse(getText());
		if (isParseable()) HAssert.assertEquals(getLicense(), actual);
		else HAssert.assertEquals(getPurpose(), getLicense(), actual);
	}

	@Test
	public void terms() {
		assumeParseable();

		final ILicenseFamily license = (ILicenseFamily) getLicense();
		final ReferenceTerms reference = getReference(license);
		HAssert.assertEquals(reference.getTerms(), license.getTerms());
	}

	@Test
	public void version() {
		assumeParseable();

		HAssume.assumeTrue("License families don't have versions", getLicense() instanceof ILicenseSpecific);
		final ILicenseSpecific license = (ILicenseSpecific) getLicense();
		final LicenseVersion version = license.getVersion();
		HAssume.assumeNotNull("Non-versioned licenses are always version consistent, of course", version);

		final String versionString = version.toString();
		if (license.getSPDXShortID() != null) HAssert.assertTrue(String.format("SPDX short ID \"%1$s\" did not contain version number \"%2$s\"!", license.getShortID(), versionString), license.getSPDXShortID().contains(versionString));
		else HAssert.assertTrue(String.format("Short ID \"%1$s\" did not contain version number \"%2$s\"!", license.getShortID(), versionString), license.getShortID().contains(versionString.replace(".0", "").replace(".", "")));
	}
}

package com.g2forge.reassert.standard.model.contract.license.parser;

import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import org.junit.Test;
import org.junit.runner.RunWith;
import org.junit.runners.Parameterized;
import org.junit.runners.Parameterized.Parameter;
import org.junit.runners.Parameterized.Parameters;

import com.g2forge.alexandria.test.HAssert;
import com.g2forge.reassert.core.api.licenseparser.ILicenseParser;
import com.g2forge.reassert.core.model.contract.license.ILicense;
import com.g2forge.reassert.standard.model.contract.license.StandardLicense;

import lombok.AccessLevel;
import lombok.Getter;

@RunWith(Parameterized.class)
public class TestSPDXStandardLicenseParser {
	@Parameters(name = "{0}")
	public static List<Object[]> computeTestParameters() {
		return Stream.of(StandardLicense.values()).filter(license -> license.getSPDXShortID() != null).map(license -> new Object[] { license }).collect(Collectors.toList());
	}

	@Getter(lazy = true, value = AccessLevel.PROTECTED)
	private final ILicenseParser parser = StandardLicenseParser.create();

	@Getter(AccessLevel.PROTECTED)
	@Parameter(0)
	public StandardLicense license;

	@Test
	public void test() {
		final StandardLicense license = getLicense();
		final ILicense actual = getParser().parse(license.getSPDXShortID());
		HAssert.assertEquals(license, actual);
	}
}

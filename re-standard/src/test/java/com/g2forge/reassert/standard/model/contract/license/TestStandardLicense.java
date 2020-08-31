package com.g2forge.reassert.standard.model.contract.license;

import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import org.junit.Test;
import org.junit.runner.RunWith;
import org.junit.runners.Parameterized;
import org.junit.runners.Parameterized.Parameter;
import org.junit.runners.Parameterized.Parameters;

import com.g2forge.alexandria.java.core.error.UnreachableCodeError;
import com.g2forge.alexandria.test.HAssert;
import com.g2forge.alexandria.test.HAssume;

import lombok.AccessLevel;
import lombok.Getter;

@RunWith(Parameterized.class)
public class TestStandardLicense {
	@Parameters(name = "{0}")
	public static List<Object[]> computeTestParameters() {
		return Stream.of(StandardLicense.values()).map(license -> new Object[] { license }).collect(Collectors.toList());
	}

	@Getter(AccessLevel.PROTECTED)
	@Parameter(0)
	public StandardLicense license;

	protected ReferenceTerms getReference(final StandardLicense license) {
		try {
			return ReferenceTerms.valueOfSPDX(license.getSPDXShortID());
		} catch (IllegalArgumentException exception) {}
		try {
			return ReferenceTerms.valueOf(license.name());
		} catch (IllegalArgumentException exception) {}
		HAssume.assumeTrue("Failed to find reference terms for " + license.getName(), false);
		throw new UnreachableCodeError();
	}

	@Test
	public void terms() {
		final StandardLicense license = getLicense();
		final ReferenceTerms reference = getReference(license);
		HAssert.assertEquals(reference.getTerms(), license.getTerms());
	}
}

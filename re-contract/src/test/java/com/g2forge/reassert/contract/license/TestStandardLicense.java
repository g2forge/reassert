package com.g2forge.reassert.contract.license;

import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import org.junit.Test;
import org.junit.runner.RunWith;
import org.junit.runners.Parameterized;
import org.junit.runners.Parameterized.Parameter;
import org.junit.runners.Parameterized.Parameters;

import com.g2forge.alexandria.test.HAssert;

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
		final ReferenceTerms retVal0 = ReferenceTerms.valueOfSPDX(license.getSPDX());
		if (retVal0 != null) return retVal0;
		return ReferenceTerms.valueOf(license.name());
	}

	@Test
	public void terms() {
		final StandardLicense license = getLicense();
		final ReferenceTerms reference = getReference(license);
		HAssert.assertEquals(reference.getTerms(), license.getTerms());
	}
}

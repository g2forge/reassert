package com.g2forge.reassert.contract.usage;

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
public class TestStandardUsage {
	@Parameters(name = "{0}")
	public static List<Object[]> computeTestParameters() {
		return Stream.of(StandardUsage.values()).map(Usage -> new Object[] { Usage }).collect(Collectors.toList());
	}

	@Getter(AccessLevel.PROTECTED)
	@Parameter(0)
	public StandardUsage Usage;

	protected ReferenceTerms getReference(final StandardUsage Usage) {
		return ReferenceTerms.valueOf(Usage.name());
	}

	@Test
	public void terms() {
		final StandardUsage Usage = getUsage();
		final ReferenceTerms reference = getReference(Usage);
		HAssert.assertEquals(reference.getTerms(), Usage.getTerms());
	}
}
